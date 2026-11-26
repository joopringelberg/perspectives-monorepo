-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.
-- END LICENSE

module Test.InheritedPropertyAuthorization where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Data.Foldable (any, for_)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Perspectives.Checking.Authorization (authorizePropertyDelta, roleHasPerspectiveOnPropertyWithVerb)
import Perspectives.Checking.PropertyProvenance (checkPropertyProvenance, findPropertyProvenance, refutedPropertyProvenance)
import Perspectives.ContextAndRole (defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##=))
import Perspectives.Error.Pretty (renderPerspectivesError)
import Perspectives.Data.EncodableMap as EM
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getProperty)
import Perspectives.ModelDependencies as DEP
import Perspectives.Persistence.CausalStore (CausalRoleDoc(..), getCausalRole)
import Perspectives.Persistence.DeltaStore (getDeltasForResource)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.QuarantineStore (QuarantinedTransaction(..), getQuarantinedTransactions)
import Perspectives.Persistence.ResourceVersionStore (getResourceVersion)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.PerspectivesState (getPerspectivesUser)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Representation.QueryFunction (FunctionName(ComposeF, MeF), QueryFunction(BinaryCombinator, Constant, DataTypeGetter, FilterF, RolGetter))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext(..), range)
import Perspectives.Query.UnsafeCompiler (compileFunction, withQueryAuthor)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Class.PersistentType (cacheInDomeinFile, getCalculatedRole, getContext, getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Representation.Class.Role (perspectivesOfRoleType)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser(..), RoleInstance(..), Value(..), perspectivesUser2RoleInstance)
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier(..))
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.RoleAssignment (filledPointsTo, fillerPointsTo)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.HandleTransaction (TransactionOutcome(..), executeTransaction, executeTransactionWithOutcome)
import Perspectives.Types.ObjectGetters (hasPerspectiveOnPropertyWithVerb, hasPerspectiveOnPropertyWithVerbByImplication)
import Test.BindingReplacement as Binding
import Test.VerificationBundles as Verification
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Instance-aware inherited property authorization" do
  test "accepts a concrete multi-hop alternative filler, even outside the permission's state" $
    Binding.withFixture Binding.localPeer [] [] do
      lift installInheritance
      typeOnly <- lift $ unsafePartial $
        hasPerspectiveOnPropertyWithVerb (ENR Binding.userType) Binding.visibleType Binding.visibleProperty SetPropertyValue
      implied <- lift $ unsafePartial $
        hasPerspectiveOnPropertyWithVerbByImplication (ENR Binding.userType) Binding.visibleProperty SetPropertyValue
      result <- permission Verification.localAuthor Binding.visibleId SetPropertyValue
      liftAff do
        assert "The declared union must not imply either individual alternative" $ not typeOnly
        assert "One alternative carrying the property is insufficient for implication" $ not implied
        assert "The concrete binding chain must authorize the inherited write" $ not $ isLeft result

  test "authorizes a property declared on a shared aspect without changing compiler checks" $
    Binding.withFixture Binding.localPeer [] [] do
      lift do
        installInheritance
        let
          addingRole = EnumeratedRoleType $ unwrap Binding.visibleType <> "Addressable"
          aspect = RoleInContext { context: Binding.contextType, role: addingRole }
        EnumeratedRole original <- getEnumeratedRole Binding.visibleType
        cacheInDomeinFile addingRole $ EnumeratedRole original
          { id = addingRole, readableName = addingRole }
        for_ [ Binding.visibleType, Binding.externalType ] \roleType -> do
          EnumeratedRole role <- getEnumeratedRole roleType
          cacheInDomeinFile roleType $ EnumeratedRole role
            { roleAspects = [ aspect ], properties = [] }
        EnumeratedProperty property <- getEnumeratedProperty Binding.visibleProperty
        cacheInDomeinFile Binding.visibleProperty $ EnumeratedProperty property { role = addingRole }
      compilerPermission <- lift $ unsafePartial $
        hasPerspectiveOnPropertyWithVerb (ENR Binding.userType) Binding.visibleType Binding.visibleProperty SetPropertyValue
      implied <- lift $ unsafePartial $
        hasPerspectiveOnPropertyWithVerbByImplication (ENR Binding.userType) Binding.visibleProperty SetPropertyValue
      absentVerb <- lift $ unsafePartial $
        hasPerspectiveOnPropertyWithVerbByImplication (ENR Binding.userType) Binding.visibleProperty RemovePropertyValue
      result <- permission Verification.localAuthor Binding.visibleId SetPropertyValue
      liftAff do
        assert "The compiler must still require the particular target role type" $ not compilerPermission
        assert "Every filler alternative implies the shared declaring aspect" implied
        assert "A shared aspect must not grant an absent property verb" $ not absentVerb
        assert "Incoming authorization must accept the shared-aspect property" $ not $ isLeft result

  test "accepts an author who fills the subject through several intermediate roles" $
    Binding.withFixture Binding.localPeer [] [] do
      lift do
        installInheritance
        -- subject <- employee <- membership <- author (cf. Ambassadeur <- Employees <- Persons <- PerspectivesUsers)
        for_ [ Tuple employee Binding.otherUserType, Tuple membership Binding.bridgeType ] \(Tuple rid rt) ->
          void $ cacheEntity rid $ PerspectRol defaultRolRecord
            { id = rid, pspType = rt, context = Binding.contextId }
        filledPointsTo Binding.userId employee
        fillerPointsTo employee Binding.userId
        filledPointsTo employee membership
        fillerPointsTo membership employee
        filledPointsTo membership Binding.localPeer
        fillerPointsTo Binding.localPeer membership
      result <- permission Verification.localAuthor Binding.visibleId SetPropertyValue
      liftAff $ assert "A multi-hop author chain must authorize the inherited write" $ not $ isLeft result

  test "preserves existing direct property permission" $
    Binding.withFixture Binding.localPeer [] [] do
      result <- permission Verification.localAuthor Binding.visibleId Consult
      liftAff $ assert "Direct type-level permissions must still work" $ not $ isLeft result

  test "rejects a signer who does not fill the subject role" $
    Binding.withFixture Binding.localPeer [] [] do
      lift installInheritance
      result <- permission (PerspectivesUser "remote-old") Binding.visibleId SetPropertyValue
      liftAff $ assert "A role type alone must not supply a concrete actor" $ isLeft result

  test "rejects verbs absent from the perspective" $
    Binding.withFixture Binding.localPeer [] [] do
      lift installInheritance
      result <- permission Verification.localAuthor Binding.visibleId RemovePropertyValue
      liftAff $ assert "Inherited properties do not grant additional verbs" $ isLeft result

  test "rejects another instance with the same storage type" $
    Binding.withFixture Binding.localPeer [] [] do
      lift do
        installInheritance
        PerspectRol original <- getPerspectRol Binding.visibleId
        void $ cacheEntity unrelated $ PerspectRol original { id = unrelated }
      result <- permission Verification.localAuthor unrelated SetPropertyValue
      liftAff $ assert "The selected object's property must resolve to this exact instance" $ isLeft result

  test "rejects an object filtered out of the perspective query" $
    Binding.withFixture Binding.localPeer [] [] do
      lift do
        installInheritance
        changePerspective \(Perspective p) ->
          let
            bool = VDOM PBool Nothing
          in
            Perspective p
              { object = BQD (CDOM $ ST Binding.contextType) (BinaryCombinator ComposeF) p.object
                  ( UQD (range p.object) FilterF
                      (SQD (range p.object) (Constant PBool "false") bool True True)
                      (range p.object)
                      False
                      False
                  )
                  (range p.object)
                  False
                  False
              }
      result <- permission Verification.localAuthor Binding.visibleId SetPropertyValue
      liftAff $ assert "Matching only the perspective's result type is insufficient" $ isLeft result

  test "does not bypass a property shadowed earlier in the actual binding chain" $
    Binding.withFixture Binding.localPeer [] [] do
      lift do
        installInheritance
        EnumeratedRole role <- getEnumeratedRole Binding.hiddenType
        cacheInDomeinFile Binding.hiddenType $ EnumeratedRole role { properties = [ ENP Binding.visibleProperty ] }
      result <- permission Verification.localAuthor Binding.visibleId SetPropertyValue
      liftAff $ assert "Authorization must use write storage resolution, not arbitrary reachability" $ isLeft result

  test "authorizes the replacing property, not the replaced storage property" $
    Binding.withFixture Binding.localPeer [] [] do
      let replacement = EnumeratedPropertyType $ unwrap Binding.visibleProperty <> "Replacement"
      lift do
        installInheritance
        EnumeratedProperty property <- getEnumeratedProperty Binding.visibleProperty
        cacheInDomeinFile replacement $ EnumeratedProperty property { id = replacement, readableName = replacement }
        EnumeratedRole role <- getEnumeratedRole Binding.visibleType
        cacheInDomeinFile Binding.visibleType $ EnumeratedRole role
          { properties = [ ENP replacement ]
          , propertyAliases = Object.singleton (unwrap Binding.visibleProperty) replacement
          }
      replaced <- permission Verification.localAuthor Binding.visibleId SetPropertyValue
      replacing <- lift $ roleHasPerspectiveOnPropertyWithVerb Verification.localAuthor
        (ENR Binding.userType)
        Binding.visibleId
        replacement
        SetPropertyValue
      liftAff do
        assert "The resolved alias destination must be authorized" $ not $ isLeft replacing
        assert "An alias must not authorize writing to the replaced property" $ isLeft replaced

  test "rejects stale inverse links that are not confirmed by the actual binding" $
    Binding.withFixture Binding.localPeer [] [] do
      lift do
        installInheritance
        fillerPointsTo Binding.oldRemotePeer Binding.userId
      result <- permission (PerspectivesUser "remote-old") Binding.visibleId SetPropertyValue
      liftAff $ assert "An inverse index is not proof of the author's binding" $ isLeft result

  test "supports a calculated subject selecting the author's concrete role" $
    Binding.withFixture Binding.localPeer [] [] do
      lift do
        installInheritance
        perspectives <- perspectivesOfRoleType (ENR Binding.userType)
        CalculatedRole role <- getCalculatedRole Binding.calculatedUserType
        cacheInDomeinFile Binding.calculatedUserType $ CalculatedRole role { perspectives = perspectives }
      result <- lift $ roleHasPerspectiveOnPropertyWithVerb Verification.localAuthor
        (CR Binding.calculatedUserType)
        Binding.visibleId
        Binding.visibleProperty
        SetPropertyValue
      liftAff $ assert "Calculated subject membership must be checked in the witnessed context" $ not $ isLeft result

  test "evaluates me for the signed author without leaking that identity to later local queries" $
    Binding.withFixture Binding.localPeer [] [] do
      receiver <- lift getPerspectivesUser
      let
        receiverPerson = RoleInstance "def:#receiving-person"
        authorPerson = RoleInstance "def:#author-person"
        author = PerspectivesUser $ unwrap Binding.localPeer
      lift do
        PerspectRol peer <- getPerspectRol Binding.localPeer
        for_ [ Tuple receiver receiverPerson, Tuple author authorPerson ] \(Tuple identity personId) ->
          void $ cacheEntity (perspectivesUser2RoleInstance identity) $ PerspectRol peer
            { id = perspectivesUser2RoleInstance identity
            , filledRoles = Object.singleton DEP.socialEnvironment $
                Object.singleton DEP.socialEnvironmentPersons [ personId ]
            }
      getter <- lift $ compileFunction $
        SQD (CDOM $ ST Binding.contextType) (DataTypeGetter MeF)
          (RDOM $ ST $ RoleInContext { context: Binding.contextType, role: Binding.bridgeType })
          False
          False
      relative <- lift $ withQueryAuthor author (unwrap Binding.contextId ##= getter)
      local <- lift (unwrap Binding.contextId ##= getter)
      liftAff do
        assert "This fixture must use distinct author and receiver identities" $ author /= receiver
        equal [ unwrap authorPerson ] relative
        equal [ unwrap receiverPerson ] local

  test "a subject that cannot write any property is quarantined, not retried" $
    Verification.withSigningFixture true \key -> do
      lift do
        installInheritance
        changePerspective \(Perspective p) -> Perspective p { propertyVerbs = EM.empty }
      signed <- Verification.signAs Verification.localAuthor
      let
        incoming = Verification.transaction [ signed ] $
          EM.singleton Verification.publicAuthor { key, transportKey: "unused", deltas: [] }
      before <- lift $ getDeltasForResource propertyKey
      outcome <- executeTransactionWithOutcome incoming
      after <- lift $ getDeltasForResource propertyKey
      version <- lift $ getResourceVersion propertyKey
      values <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
      quarantined <- lift getQuarantinedTransactions
      liftAff do
        assert "A model-deterministic lack of authority must quarantine the transaction" $ case outcome of
          Quarantined -> true
          _ -> false
        assert "The quarantine record must attribute the signed author" $
          any (\(QuarantinedTransaction q) -> contains (Pattern ("author " <> unwrap Verification.localAuthor)) q.reason) quarantined
        equal (map (\(DeltaStoreRecord r) -> r._id) before) (map (\(DeltaStoreRecord r) -> r._id) after)
        equal 0 version
        equal [ Value "visible-value" ] values

  test "unsigned peer deltas are rejected" $
    Verification.withSigningFixture true \key -> do
      lift installInheritance
      signed <- Verification.signAs Verification.localAuthor
      let
        unsigned = over SignedDelta (_ { signature = Nothing }) signed
        incoming = Verification.transaction [ unsigned ] $
          EM.singleton Verification.publicAuthor { key, transportKey: "unused", deltas: [] }
      outcome <- executeTransactionWithOutcome incoming
      values <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
      liftAff do
        assert "An unsigned peer delta must be rejected" $ case outcome of
          Rejected reason -> contains (Pattern "Unsigned peer delta") reason
          _ -> false
        equal [ Value "visible-value" ] values

  test "rejected signed updates do not advance history, versions or causality, and can be retried" $
    Verification.withSigningFixture true \key -> do
      lift do
        installInheritance
        -- The subject can write some property, so the failure may be due to missing evidence.
        changePerspective \(Perspective p) -> Perspective p
          { propertyVerbs = EM.singleton (ContextState $ StateIdentifier $ unwrap Binding.contextType <> "$Inactive")
              [ PropertyVerbs (PSet [ ENP unrelatedProperty ]) (PSet [ SetPropertyValue ]) ]
          }
      signed <- Verification.signAs Verification.localAuthor
      let
        incoming = Verification.transaction [ signed ] $
          EM.singleton Verification.publicAuthor { key, transportKey: "unused", deltas: [] }
      before <- lift $ getDeltasForResource propertyKey
      CausalRoleDoc causalBefore <- lift $ getCausalRole (unwrap Binding.visibleId)
      rejected <- try $ executeTransaction incoming
      after <- lift $ getDeltasForResource propertyKey
      version <- lift $ getResourceVersion propertyKey
      values <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
      CausalRoleDoc causalAfter <- lift $ getCausalRole (unwrap Binding.visibleId)
      liftAff do
        assert "Unauthorized property execution must fail, rather than warn and succeed" $ isLeft rejected
        case rejected of
          Left err -> do
            assert "The incoming error must report the type-level failure"
              $ contains (Pattern "Type-level property authorization failed:")
              $ show err
            assert "The incoming error must also report the instance witness failure"
              $ contains (Pattern "Instance property authorization failed for author")
              $ show err
            assert "The incoming error must include the author-chain evidence"
              $ contains (Pattern "roles (transitively) filled by the author:")
              $ show err
          Right _ -> assert "Expected the incoming transaction to fail" false
        equal (map (\(DeltaStoreRecord r) -> r._id) before) (map (\(DeltaStoreRecord r) -> r._id) after)
        equal 0 version
        equal [ Value "visible-value" ] values
        equal causalBefore.clock causalAfter.clock
      lift $ changePerspective grantProperty
      executeTransaction incoming
      retried <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
      retriedVersion <- lift $ getResourceVersion propertyKey
      liftAff do
        equal [ Value "updated" ] retried
        equal 1 retriedVersion

  test "reports both authorization failures and absent author evidence together" $
    Binding.withFixture Binding.localPeer [] [] do
      lift installInheritance
      result <- permission (PerspectivesUser "unknown-author") Binding.visibleId SetPropertyValue
      case result of
        Right _ -> liftAff $ assert "An unknown author must not be authorized" false
        Left err -> do
          message <- lift $ renderPerspectivesError err
          liftAff do
            assert "The type-level failure must retain the original property error" $
              contains (Pattern "Type-level property authorization failed: (UnauthorizedForProperty)") message
            assert "The instance failure must identify the signed author" $
              contains (Pattern "Instance property authorization failed for author unknown-author") message
            assert "Missing author identity evidence must be explicit" $
              contains (Pattern "Author identity found: []") message
            assert "Missing author-role evidence must be explicit" $
              contains (Pattern "roles (transitively) filled by the author: []") message

  test "a sender's property provenance is accepted by the receiver" $
    Binding.withFixture Binding.localPeer [] [] do
      lift installInheritance
      mprov <- lift $ findPropertyProvenance Verification.localAuthor (ENR Binding.userType) Binding.visibleId Binding.visibleProperty [ SetPropertyValue ]
      case mprov of
        Nothing -> liftAff $ assert "The sender must find a witness for an authorized inherited write" false
        Just prov -> do
          checked <- lift $ checkPropertyProvenance Verification.localAuthor (ENR Binding.userType) Binding.visibleId Binding.visibleProperty SetPropertyValue prov
          authorized <- lift $ authorizePropertyDelta Verification.localAuthor (ENR Binding.userType) Binding.visibleId Binding.visibleProperty (Just prov) SetPropertyValue
          refuted <- lift $ refutedPropertyProvenance (ENR Binding.userType) prov
          liftAff do
            equal Binding.contextId prov.evaluationContext
            -- The perspective's object is Hidden, which reaches the storage instance through its filler chain.
            equal Binding.hiddenId prov.objectInstance
            assert "The witness must be confirmed" $ checked == Right unit
            assert "A confirmed witness authorizes the delta" $ not $ isLeft authorized
            assert "A genuine evaluation context is not refuted" $ not refuted

  test "an unconfirmed provenance falls back to the instance search; missing evidence stays an error" $
    Binding.withFixture Binding.localPeer [] [] do
      lift installInheritance
      mprov <- lift $ findPropertyProvenance Verification.localAuthor (ENR Binding.userType) Binding.visibleId Binding.visibleProperty [ SetPropertyValue ]
      case mprov of
        Nothing -> liftAff $ assert "The sender must find a witness for an authorized inherited write" false
        Just prov -> do
          let stale = prov { subjectInstance = RoleInstance "def:#not-yet-received" }
          checked <- lift $ checkPropertyProvenance Verification.localAuthor (ENR Binding.userType) Binding.visibleId Binding.visibleProperty SetPropertyValue stale
          fallback <- lift $ authorizePropertyDelta Verification.localAuthor (ENR Binding.userType) Binding.visibleId Binding.visibleProperty (Just stale) SetPropertyValue
          stranger <- lift $ authorizePropertyDelta (PerspectivesUser "remote-old") (ENR Binding.userType) Binding.visibleId Binding.visibleProperty (Just prov) SetPropertyValue
          liftAff do
            assert "An unknown subject instance cannot confirm the witness" $ isLeft checked
            assert "The receiver's own instance search may still authorize the delta" $ not $ isLeft fallback
            assert "Without evidence for the signer the delta is not authorized (it stays pending)" $ isLeft stranger

  test "a provenance whose evaluation context type cannot hold the subject is refuted" $
    Binding.withFixture Binding.localPeer [] [] do
      lift installInheritance
      mprov <- lift $ findPropertyProvenance Verification.localAuthor (ENR Binding.userType) Binding.visibleId Binding.visibleProperty [ SetPropertyValue ]
      case mprov of
        Nothing -> liftAff $ assert "The sender must find a witness for an authorized inherited write" false
        Just prov -> do
          let unrelatedContext = ContextType $ Binding.model <> "$Unrelated"
          refuted <- lift do
            Context original <- getContext Binding.contextType
            cacheInDomeinFile unrelatedContext $ Context original { id = unrelatedContext, readableName = unrelatedContext, contextAspects = [] }
            refutedPropertyProvenance (ENR Binding.userType) prov { evaluationContextType = unrelatedContext }
          liftAff $ assert "A foreign evaluation context type is a model-deterministic refutation" refuted

permission :: PerspectivesUser -> RoleInstance -> PropertyVerb -> MonadPerspectivesTransaction (Either PerspectivesError Boolean)
permission author storage verb = lift $
  roleHasPerspectiveOnPropertyWithVerb author (ENR Binding.userType) storage Binding.visibleProperty verb

propertyKey :: String
propertyKey = unwrap Binding.visibleId <> "#" <> unwrap Binding.visibleProperty

person :: RoleInstance
person = RoleInstance "def:#inherited-person"

unrelated :: RoleInstance
unrelated = RoleInstance "def:#unrelated-person"

employee :: RoleInstance
employee = RoleInstance "def:#employee"

membership :: RoleInstance
membership = RoleInstance "def:#membership"

installInheritance :: MonadPerspectives Unit
installInheritance = do
  let
    inContext role = RoleInContext { context: Binding.contextType, role }
  EnumeratedRole participant <- getEnumeratedRole Binding.hiddenType
  cacheInDomeinFile Binding.hiddenType $ EnumeratedRole participant
    { binding = Just $ UET $ inContext Binding.bridgeType }
  EnumeratedRole persons <- getEnumeratedRole Binding.bridgeType
  cacheInDomeinFile Binding.bridgeType $ EnumeratedRole persons
    { binding = Just $ SUM [ UET $ inContext Binding.visibleType, UET $ inContext Binding.externalType ] }
  void $ cacheEntity person $ PerspectRol defaultRolRecord
    { id = person, pspType = Binding.bridgeType, context = Binding.contextId, binding = Just Binding.visibleId }
  filledPointsTo Binding.hiddenId person
  filledPointsTo Binding.userId Binding.localPeer
  fillerPointsTo Binding.localPeer Binding.userId
  PerspectContext context <- getPerspectContext Binding.contextId
  void $ cacheEntity Binding.contextId $ PerspectContext context
    { rolInContext = Object.insert (unwrap Binding.bridgeType) [ person ] context.rolInContext }
  changePerspective \(Perspective p) -> grantProperty $ Perspective p
    { object = SQD (CDOM $ ST Binding.contextType) (RolGetter $ ENR Binding.hiddenType)
        (RDOM $ UET $ inContext Binding.hiddenType)
        False
        False
    , roleTypes = [ ENR Binding.hiddenType ]
    }

unrelatedProperty :: EnumeratedPropertyType
unrelatedProperty = EnumeratedPropertyType $ unwrap Binding.visibleType <> "$Unrelated"

grantProperty :: Perspective -> Perspective
grantProperty (Perspective p) = Perspective p
  { propertyVerbs = EM.singleton (ContextState $ StateIdentifier $ unwrap Binding.contextType <> "$Inactive")
      [ PropertyVerbs (PSet [ ENP Binding.visibleProperty ]) (PSet [ SetPropertyValue ]) ]
  }

changePerspective :: (Perspective -> Perspective) -> MonadPerspectives Unit
changePerspective update = do
  EnumeratedRole role <- getEnumeratedRole Binding.userType
  let selected = filter (\(Perspective p) -> p.id == unwrap Binding.visibleType) role.perspectives
  cacheInDomeinFile Binding.userType $ EnumeratedRole role { perspectives = update <$> selected }
