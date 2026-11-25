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
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Perspectives.Checking.Authorization (roleHasPerspectiveOnPropertyWithVerb)
import Perspectives.ContextAndRole (defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##=))
import Perspectives.Data.EncodableMap as EM
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getProperty)
import Perspectives.ModelDependencies as DEP
import Perspectives.Persistence.CausalStore (CausalRoleDoc(..), getCausalRole)
import Perspectives.Persistence.DeltaStore (getDeltasForResource)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.ResourceVersionStore (getResourceVersion)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.PerspectivesState (getPerspectivesUser)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Representation.QueryFunction (FunctionName(ComposeF, MeF), QueryFunction(BinaryCombinator, Constant, DataTypeGetter, FilterF, RolGetter))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext(..), range)
import Perspectives.Query.UnsafeCompiler (compileFunction, withQueryAuthor)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Class.PersistentType (cacheInDomeinFile, getCalculatedRole, getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Representation.Class.Role (perspectivesOfRoleType)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser(..), RoleInstance(..), Value(..), perspectivesUser2RoleInstance)
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), PropertyType(..), RoleType(..), StateIdentifier(..))
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.RoleAssignment (filledPointsTo, fillerPointsTo)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Types.ObjectGetters (hasPerspectiveOnPropertyWithVerb)
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
      result <- permission Verification.localAuthor Binding.visibleId SetPropertyValue
      liftAff do
        assert "The declared union must not imply either individual alternative" $ not typeOnly
        assert "The concrete binding chain must authorize the inherited write" $ not $ isLeft result

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

  test "rejected signed updates do not advance history, versions or causality, and can be retried" $
    Verification.withSigningFixture true \key -> do
      lift do
        installInheritance
        changePerspective \(Perspective p) -> Perspective p { propertyVerbs = EM.empty }
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
