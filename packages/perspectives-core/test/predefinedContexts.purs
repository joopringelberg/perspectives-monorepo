-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.PredefinedContexts where

import Prelude

import Control.Monad.AvarMonadAsk as AA
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, length)
import Data.Foldable (foldl, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (over, unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Foreign.Object as Object
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap as EM
import Perspectives.Deltas (transactieForEachUser)
import Perspectives.DomeinCache (storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Extern.Couchdb (createPredefinedContexts, theWorld)
import Perspectives.Identifiers (typeUri2ModelUri)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.ModelDependencies as DEP
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Persistence.DeltaStore (getDeltasForResource, storeDeltaFromSignedDelta)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.ResourceVersionStore (setResourceVersion)
import Perspectives.Persistent (entityExists, getDomeinFile, getPerspectRol, tryGetPerspectRol)
import Perspectives.Representation.Class.PersistentType (cacheInDomeinFile, getEnumeratedRole)
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Context (ContextKind(..), defaultContext)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), defaultEnumeratedRole)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), PropertyType(..), RoleKind(..), RoleType(..), StateIdentifier(..), externalRoleType)
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..), TransactionDestination(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), currentTransactionForPeer, withoutPredefinedContextScaffolding)
import Perspectives.Sync.VersionedDelta (isPredefinedContextScaffolding)
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)
import Simple.JSON (class WriteForeign, writeJSON)
import Test.BindingReplacement as Binding
import Test.VerificationBundles as Verification
import Perspectives.Authenticate (signDelta)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Predefined context scaffolding" do
  for_ [ "TheWorld", "TheSocialEnvironment" ] \id ->
    for_ [ "", "def:#", "loc:entities#", "rem:https://example.org/entities/#" ] \prefix ->
      test ("omits only creation scaffolding for " <> prefix <> id) do
        let
          historical = scaffolding (PerspectivesUser "historical") (prefix <> id)
          contents = [ Binding.roleDelta Binding.userId, Binding.visiblePropertyDelta ]
          original = peerTransaction $ historical <> contents
          TransactionForPeer result = withoutPredefinedContextScaffolding original
        equal contents result.deltas
        equal Nothing $ EM.lookup (PerspectivesUser "historical") result.publicKeys
        assert "The actual content author's key must remain"
          (not $ isNothing $ EM.lookup (PerspectivesUser "def:#local") result.publicKeys)

  test "preserves ordinary and published context creation" do
    for_ [ "ordinary", "pub:https://example.org/public/#TheWorld", "pub:https://example.org/public/#TheSocialEnvironment" ] \id -> do
      let deltas = scaffolding (PerspectivesUser "historical") id
      equal deltas (unwrap $ withoutPredefinedContextScaffolding $ peerTransaction deltas).deltas

  test "recognizes ordered unmarked scaffolding for both predefined contexts" do
    for_ [ "TheWorld", "TheSocialEnvironment" ] \id -> do
      let
        subject = ENR Binding.userType
        contextType = Binding.contextType
        roleType = externalRoleType contextType
        roleInstance = RoleInstance $ id <> "$External"
        payloads =
          [ writeJSON { subject, resourceKey: id, resourceVersion: 0, id: ContextInstance id, contextType, deltaType: "ConstructEmptyContext" }
          , writeJSON { subject, resourceKey: unwrap roleInstance, resourceVersion: 0, id: ContextInstance id, contextType, roleType, roleInstance, deltaType: "ConstructExternalRole" }
          , writeJSON { subject, resourceKey: unwrap roleInstance, resourceVersion: 0, contextInstance: ContextInstance id, contextType, roleType, roleInstance, deltaType: "AddExternalRole" }
          ]
      for_ payloads \payload ->
        assert "Ordered unmarked scaffolding must be omitted too" $ isPredefinedContextScaffolding payload

  test "preserves memberships, bindings, and external properties of predefined contexts" do
    let
      deltas =
        [ Binding.signed
            { deltaFormatVersion: 2
            , subject: "model://test.example#User@1.0"
            , subjectKind: "ENR"
            , resourceKey: "def:#member"
            , resourceVersion: 0
            , id: ContextInstance "TheWorld"
            , contextType: "model://test.example#World@1.0"
            , roleInstance: RoleInstance "member"
            , roleType: "model://test.example#World$Member@1.0"
            , deltaType: "ConstructEmptyRole"
            }
        , Binding.signed
            { deltaFormatVersion: 2
            , subject: "model://test.example#User@1.0"
            , subjectKind: "ENR"
            , resourceKey: "def:#member"
            , resourceVersion: 1
            , contextInstance: ContextInstance "TheWorld"
            , contextType: "model://test.example#World@1.0"
            , roleInstance: RoleInstance "member"
            , roleType: "model://test.example#World$Member@1.0"
            , deltaType: "AddRoleInstancesToContext"
            }
        , Binding.replacementDelta Binding.oldRemotePeer
        , Binding.signed
            { deltaFormatVersion: 2
            , subject: "model://test.example#User@1.0"
            , subjectKind: "ENR"
            , resourceKey: "def:#TheSocialEnvironment$External#model://test.example#External$MyIdentity"
            , resourceVersion: 1
            , id: RoleInstance "TheSocialEnvironment$External"
            , roleType: "model://test.example#External@1.0"
            , property: "model://test.example#External$MyIdentity@1.0"
            , values: [ Value "identity-file" ]
            , deltaType: "SetProperty"
            }
        ]
    equal deltas (unwrap $ withoutPredefinedContextScaffolding $ peerTransaction deltas).deltas

  test "peer customization omits scaffolding without removing public publication support" $
    Binding.withFixture Binding.oldRemotePeer [] [] do
      lift $ AA.modify \s -> s { perspectivesUser = PerspectivesUser "def:#sender" }
      let
        scaffold = scaffolding (PerspectivesUser "historical") "TheWorld"
        deltas = scaffold <> [ Binding.visiblePropertyDelta ]
        peer = PerspectivesUser "def:#remote-old"
        destination = Peer (unschemePerspectivesUser peer) peer
        public = PublicDestination $ RoleInstance "public-proxy"
      transaction <- AA.get
      let
        input = over Transaction
          ( \t -> t
              { deltas = map (\delta -> DeltaInTransaction { users: [ Binding.userId, Binding.otherLocalId ], delta }) deltas
              , userRoleBottoms = Map.fromFoldable [ Tuple Binding.userId destination, Tuple Binding.otherLocalId public ]
              , publicKeys = (unwrap $ peerTransaction deltas).publicKeys
              }
          )
          transaction
      customized <- lift $ transactieForEachUser input
      liftAff do
        equal (Just [ Binding.visiblePropertyDelta ]) (_.deltas <<< unwrap <$> Map.lookup destination customized)
        equal (Just deltas) (_.deltas <<< unwrap <$> Map.lookup public customized)
        equal (Just $ PerspectivesUser "sender") (_.author <<< unwrap <$> Map.lookup destination customized)
        equal (Just $ PerspectivesUser "sender") (_.author <<< unwrap <$> Map.lookup public customized)
      let
        onlyScaffolding = over Transaction
          (\t -> t { deltas = map (\delta -> DeltaInTransaction { users: [ Binding.userId ], delta }) scaffold })
          input
      empty <- lift $ transactieForEachUser onlyScaffolding
      liftAff $ assert "Scaffolding alone must not produce a peer transaction" (Map.isEmpty empty)

  test "forwarding preserves delta authors and author exclusion without making them envelope senders" $
    Binding.withFixture Binding.oldRemotePeer [] [] do
      lift $ AA.modify \s -> s { perspectivesUser = PerspectivesUser "def:#sender" }
      let
        firstAuthor = PerspectivesUser "first"
        secondAuthor = PerspectivesUser "second"
        first = over SignedDelta (\d -> d { author = firstAuthor }) Binding.visiblePropertyDelta
        second = over SignedDelta (\d -> d { author = secondAuthor }) Binding.visiblePropertyDelta
        recipient = PerspectivesUser "recipient"
        destination = Peer (unschemePerspectivesUser recipient) recipient
        firstDestination = Peer (unschemePerspectivesUser firstAuthor) firstAuthor
        info = { key: "unused", transportKey: "unused", deltas: [] }
      transaction <- AA.get
      let
        input = over Transaction
          ( \t -> t
              { deltas = map
                  (\delta -> DeltaInTransaction { users: [ Binding.userId, Binding.otherLocalId ], delta })
                  [ first, second ]
              , userRoleBottoms = Map.fromFoldable
                  [ Tuple Binding.userId destination, Tuple Binding.otherLocalId firstDestination ]
              , publicKeys = EM.fromFoldable [ Tuple firstAuthor info, Tuple secondAuthor info ]
              }
          )
          transaction
      customized <- lift $ transactieForEachUser input
      liftAff do
        equal (Just [ first, second ]) (_.deltas <<< unwrap <$> Map.lookup destination customized)
        equal (Just [ second ]) (_.deltas <<< unwrap <$> Map.lookup firstDestination customized)
        for_ [ destination, firstDestination ] \peer -> case Map.lookup peer customized of
          Nothing -> assert "Forwarded content must reach the intended peer" false
          Just (TransactionForPeer output) -> do
            equal (PerspectivesUser "sender") output.author
            equal Nothing $ EM.lookup (PerspectivesUser "sender") output.publicKeys
        equal (Just $ EM.singleton secondAuthor info) (_.publicKeys <<< unwrap <$> Map.lookup firstDestination customized)

  test "incoming scaffolding neither accumulates nor imports incidental identities" $
    -- Peer deltas must be signed: sign the property delta with the known key of local.
    Verification.withSigningFixture true \key -> do
      EnumeratedRole subject <- lift $ getEnumeratedRole Binding.userType
      let
        grant (Perspective p) = Perspective p
          { propertyVerbs = EM.singleton (ContextState $ StateIdentifier $ unwrap Binding.contextType)
              [ PropertyVerbs (PSet [ ENP Binding.visibleProperty ]) (PSet [ Consult, SetPropertyValue ]) ]
          }
        local = scaffolding (PerspectivesUser "def:#local") "TheWorld"
        incoming = scaffolding (PerspectivesUser "historical") "TheWorld"
        unsignedProperty = Binding.signed
          { resourceKey: unwrap Binding.visibleId <> "#" <> unwrap Binding.visibleProperty
          , resourceVersion: 2
          , deltaType: "SetProperty"
          , id: RoleInstance "visible"
          , roleType: Binding.visibleType
          , property: Binding.visibleProperty
          , values: [ Value "updated" ]
          , subject: ENR Binding.userType
          }
      propertyDelta <- over SignedDelta (\d -> d { author = PerspectivesUser "local" }) <$> signDelta (unwrap unsignedProperty).encryptedDelta
      lift do
        cacheInDomeinFile Binding.userType $ EnumeratedRole subject { perspectives = map grant subject.perspectives }
        for_ local storeDeltaFromSignedDelta
        setResourceVersion (unwrap Binding.visibleId <> "#" <> unwrap Binding.visibleProperty) 1
      let
        input = over TransactionForPeer
          (\t -> t { publicKeys = EM.insert (PerspectivesUser "def:#local") { key, transportKey: "unused", deltas: [] } t.publicKeys })
          (peerTransaction $ incoming <> [ propertyDelta ])
      executeTransaction input
      PerspectRol role <- lift $ getPerspectRol Binding.visibleId
      identities <- lift $ tryGetPerspectRol (RoleInstance "def:#historical")
      history <- lift $ getDeltasForResource "TheWorld"
      liftAff do
        equal (Just [ Value "updated" ]) $ Object.lookup (unwrap Binding.visibleProperty) role.properties
        assert "An unused verification identity must not be imported" (isNothing identities)
        equal [ PerspectivesUser "def:#local" ] $ map (\(DeltaStoreRecord r) -> r.author) history

  test "bootstraps both containers locally before importing an identity and retains restoration deltas" $
    Binding.withFixture Binding.oldRemotePeer [] [] do
      lift installPredefinedContextTypes
      first@(Tuple world social) <- createPredefinedContexts
      second <- createPredefinedContexts
      liftAff do
        equal (Tuple (ContextInstance "def:#TheWorld") (ContextInstance "def:#TheSocialEnvironment")) first
        equal first second
      for_ [ world, social ] \context -> do
        exists <- lift $ entityExists context
        externalExists <- lift $ entityExists $ RoleInstance $ unwrap context <> "$External"
        history <- lift $ getDeltasForResource (unwrap context)
        externalHistory <- lift $ getDeltasForResource (unwrap context <> "$External")
        let
          scaffold = filter
            (\(DeltaStoreRecord { signedDelta: SignedDelta { encryptedDelta } }) -> isPredefinedContextScaffolding encryptedDelta)
            (history <> externalHistory)
        liftAff do
          assert "The container and its external role must exist without peer input" (exists && externalExists)
          equal 3 (length scaffold)

scaffolding :: PerspectivesUser -> String -> Array SignedDelta
scaffolding author id =
  let
    sign :: forall a. WriteForeign a => a -> SignedDelta
    sign payload = SignedDelta { author, encryptedDelta: writeJSON payload, signature: Nothing }
    subject = "model://test.example#Initializer@1.0"
    contextType = "model://test.example#World@1.0"
    roleType = "model://test.example#World$External@1.0"
  in
    [ sign
        { deltaFormatVersion: 2
        , subject
        , subjectKind: "CR"
        , resourceKey: id
        , resourceVersion: 0
        , id: ContextInstance id
        , contextType
        , deltaType: "ConstructEmptyContext"
        }
    , sign
        { deltaFormatVersion: 2
        , subject
        , subjectKind: "CR"
        , resourceKey: id <> "$External"
        , resourceVersion: 0
        , id: ContextInstance id
        , contextType
        , roleType
        , roleInstance: RoleInstance $ id <> "$External"
        , deltaType: "ConstructExternalRole"
        }
    , sign
        { deltaFormatVersion: 2
        , subject
        , subjectKind: "CR"
        , resourceKey: id <> "$External"
        , resourceVersion: 1
        , contextInstance: ContextInstance id
        , contextType
        , roleType
        , roleInstance: RoleInstance $ id <> "$External"
        , deltaType: "AddExternalRole"
        }
    ]

peerTransaction :: Array SignedDelta -> TransactionForPeer
peerTransaction deltas = currentTransactionForPeer
  { author: PerspectivesUser "def:#local"
  , perspectivesSystem: ContextInstance "def:#test-system"
  , timeStamp: SerializableDateTime bottom
  , deltas
  , publicKeys: EM.fromFoldable
      [ Tuple (PerspectivesUser "historical") { key: "unused", transportKey: "unused", deltas: [] }
      , Tuple (PerspectivesUser "def:#local") { key: "unused", transportKey: "unused", deltas: [] }
      ]
  }

installPredefinedContextTypes :: MonadPerspectives Unit
installPredefinedContextTypes = case typeUri2ModelUri DEP.perspectivesUsers of
  Nothing -> liftAff $ assert "System types must have a model URI" false
  Just model -> do
    DomeinFile d <- getDomeinFile (ModelUri model)
    let
      origin = ArcPosition { line: 1, column: 1 }
      contexts = [ ContextType theWorld, ContextType DEP.socialEnvironment ]
      contextTypes = foldl
        (\result ct -> Object.insert (unwrap ct) (defaultContext (unwrap ct) "Predefined" Domain Nothing origin Nothing) result)
        d.contexts
        contexts
      roleTypes = foldl
        ( \result ct ->
            let
              rt = externalRoleType ct
            in
              Object.insert (unwrap rt) (defaultEnumeratedRole (unwrap rt) "External" ExternalRole (unwrap ct) origin) result
        )
        d.enumeratedRoles
        contexts
    void $ storeDomeinFileInCache (ModelUri model) $ DomeinFile d { contexts = contextTypes, enumeratedRoles = roleTypes }
