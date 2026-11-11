-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Test.BindingReplacement where

import Prelude

import Control.Monad.AvarMonadAsk as AA
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (elem, null)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.AVar (new)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.Cuid2 (cuid2)
import Perspectives.Data.EncodableMap as EM
import Perspectives.Deltas (transactieForEachUser)
import Perspectives.DomeinCache (storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFileRecord)
import Perspectives.Identifiers (typeUri2ModelUri)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.Me (isMe)
import Perspectives.ModelDependencies (perspectivesUsers, perspectivesUsersPublicKey, perspectivesUsersTransportPublicKey)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Persistence.DeltaStore (storeDeltaFromSignedDelta)
import Perspectives.Persistent (getPerspectContext)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Query.QueryTypes (RoleInContext(..)) as QT
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Context (Context(..), ContextKind(..), defaultContext)
import Perspectives.Representation.EnumeratedProperty (defaultEnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), defaultEnumeratedRole)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleKind(..), RoleType(..), StateIdentifier(..))
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.RoleAssignment (lookForAlternativeMe)
import Perspectives.RunPerspectives (runPerspectivesWithoutCouchdb)
import Perspectives.SaveUserData (FillBindingMode(..), changeRoleBinding, setBindingWithMode)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..))
import Perspectives.SetupUser (setupInvertedQueryDatabase)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..), TransactionDestination(..), createTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)
import Simple.JSON (class WriteForeign, writeJSON)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "User-role filler replacement" do
  for_ [ localPeer, oldRemotePeer ] \oldFiller ->
    test ("initializes the new remote peer when replacing " <> unwrap oldFiller) $
      withFixture oldFiller [] [] do
        replaceAndApply oldFiller
        local <- lift $ isMe userId
        PerspectContext context <- lift $ getPerspectContext contextId
        liftAff $ equal false local
        liftAff $ equal Nothing context.me
        transaction <- AA.get
        destinations <- lift $ transactieForEachUser transaction
        let recipient = PerspectivesUser "def:#remote-new"
        case Map.lookup (Peer (unschemePerspectivesUser recipient) recipient) destinations of
          Nothing -> liftAff $ assert "The new peer must receive a transaction" false
          Just (TransactionForPeer { deltas }) -> liftAff do
            for_ [ contextDelta, roleDelta userId, roleDelta visibleId, visiblePropertyDelta, replacementDelta oldFiller ] \delta ->
              assert ("Missing initialization delta: " <> show delta) (elem delta deltas)
            assert "A role outside the new user's perspectives must not be sent"
              (not $ elem (roleDelta hiddenId) deltas)
        let previousRecipient = PerspectivesUser "def:#remote-old"
        case Map.lookup (Peer (unschemePerspectivesUser previousRecipient) previousRecipient) destinations of
          Nothing -> pure unit
          Just (TransactionForPeer { deltas }) ->
            liftAff $ assert "Initialization must not be addressed through the old filler"
              (not $ elem visiblePropertyDelta deltas)

  test "retains another genuinely local instance of the same role type" $
    withFixture localPeer [ remoteAlternativeId, localAlternativeId ] [] do
      replaceAndApply localPeer
      PerspectContext context <- lift $ getPerspectContext contextId
      liftAff $ equal (Just localAlternativeId) context.me
      local <- lift $ isMe userId
      liftAff $ equal false local

  test "falls back to a genuinely local role of another type" $
    withFixture localPeer [] [ otherLocalId ] do
      replaceAndApply localPeer
      PerspectContext context <- lift $ getPerspectContext contextId
      liftAff $ equal (Just otherLocalId) context.me
      local <- lift $ isMe userId
      liftAff $ equal false local

  test "does not initialize the local peer for a remote-to-local replacement" $
    withFixture oldRemotePeer [] [] do
      changeRoleBinding userId (Just localPeer)
      local <- lift $ isMe userId
      Transaction { deltas } <- AA.get
      liftAff $ equal true local
      liftAff $ assert "No remote initialization should be generated" (null deltas)

  test "removing a binding clears local identity without initializing a peer" $
    withFixture localPeer [] [] do
      changeRoleBinding userId Nothing
      local <- lift $ isMe userId
      PerspectContext context <- lift $ getPerspectContext contextId
      Transaction { deltas } <- AA.get
      liftAff $ equal false local
      liftAff $ equal Nothing context.me
      liftAff $ assert "Unbinding must not initialize a peer" (null deltas)

  test "a role scheduled for removal is not selected as its own alternative" $
    withFixture localPeer [] [] do
      AA.modify $ over Transaction \tr -> tr { untouchableRoles = [ userId ] }
      changeRoleBinding userId (Just newRemotePeer)
      PerspectContext context <- lift $ getPerspectContext contextId
      Transaction { deltas } <- AA.get
      liftAff $ equal Nothing context.me
      liftAff $ assert "A removed role must not initialize a peer" (null deltas)

  test "alternative selection does not promote an unrelated remote instance" $
    withFixture oldRemotePeer [ remoteAlternativeId ] [] do
      lookForAlternativeMe userId contextId
      PerspectContext context <- lift $ getPerspectContext contextId
      local <- lift $ isMe remoteAlternativeId
      liftAff $ equal Nothing context.me
      liftAff $ equal false local

  test "replacing a filler with itself remains a no-op" $
    withFixture localPeer [] [] do
      void $ setBindingWithMode FillWithProvidedType userId localPeer Nothing
      Transaction { deltas, scheduledAssignments } <- AA.get
      local <- lift $ isMe userId
      liftAff $ equal true local
      liftAff $ assert "A no-op must not schedule a binding change" (null scheduledAssignments)
      liftAff $ assert "A no-op must not initialize a peer" (null deltas)

replaceAndApply :: RoleInstance -> MonadPerspectivesTransaction Unit
replaceAndApply oldFiller = do
  void $ setBindingWithMode FillWithProvidedType userId newRemotePeer (Just $ replacementDelta oldFiller)
  Transaction { scheduledAssignments } <- AA.get
  case scheduledAssignments of
    [ RoleUnbinding filled (Just filler) _ ] -> do
      liftAff $ equal userId filled
      liftAff $ equal newRemotePeer filler
      changeRoleBinding filled (Just filler)
    _ -> liftAff $ assert "Replacement must schedule exactly one binding change" false

withFixture
  :: RoleInstance
  -> Array RoleInstance
  -> Array RoleInstance
  -> MonadPerspectivesTransaction Unit
  -> Aff Unit
withFixture oldFiller sameTypeAlternatives otherTypeAlternatives action = do
  name <- liftEffect $ cuid2 "bindingReplacement"
  runPerspectivesWithoutCouchdb name do
    setupInvertedQueryDatabase
    installModel
    void $ cacheEntity contextId $ PerspectContext defaultContextRecord
      { id = contextId
      , pspType = contextType
      , buitenRol = externalId
      , me = if oldFiller == localPeer then Just userId else Nothing
      , preferredUserRoleType = Just $ ENR userType
      , rolInContext = Object.fromFoldable
          [ Tuple (unwrap userType) ([ userId ] <> sameTypeAlternatives)
          , Tuple (unwrap otherUserType) otherTypeAlternatives
          , Tuple (unwrap visibleType) [ visibleId ]
          , Tuple (unwrap hiddenType) [ hiddenId ]
          ]
      }
    for_ [ localPeer, oldRemotePeer, newRemotePeer ] \rid ->
      void $ cacheEntity rid $ PerspectRol defaultRolRecord
        { id = rid
        , context = contextId
        , pspType = EnumeratedRoleType perspectivesUsers
        , isMe = rid == localPeer
        , properties = Object.fromFoldable
            [ Tuple perspectivesUsersPublicKey [ Value "test-public-key" ]
            , Tuple perspectivesUsersTransportPublicKey [ Value "test-transport-key" ]
            ]
        }
    for_ [ userId, remoteAlternativeId, localAlternativeId, otherLocalId, visibleId, hiddenId, externalId ] \rid ->
      void $ cacheEntity rid $ PerspectRol defaultRolRecord
        { id = rid
        , context = contextId
        , pspType =
            if rid == visibleId then visibleType
            else if rid == hiddenId then hiddenType
            else if rid == externalId then externalType
            else if rid == otherLocalId then otherUserType
            else userType
        , binding =
            if rid == userId then Just oldFiller
            else if rid == localAlternativeId || rid == otherLocalId then Just localPeer
            else if rid == remoteAlternativeId then Just oldRemotePeer
            else Nothing
        , isMe = rid == userId && oldFiller == localPeer
        , properties = if rid == visibleId then Object.singleton (unwrap visibleProperty) [ Value "visible-value" ] else Object.empty
        }
    for_
      ( [ contextDelta, externalLinkDelta, visiblePropertyDelta ] <> map roleDelta
          [ userId, visibleId, hiddenId, externalId, newRemotePeer, oldRemotePeer, localPeer, remoteAlternativeId, localAlternativeId, otherLocalId ]
      )
      storeDeltaFromSignedDelta
    transaction <- liftAff $ createTransaction (ENR userType) true >>= new
    runReaderT action transaction

installModel :: MonadPerspectives Unit
installModel = do
  let
    origin = ArcPosition { line: 1, column: 1 }
    role rt kind = defaultEnumeratedRole (unwrap rt) (unwrap rt) kind (unwrap contextType) origin
    modelContext = defaultContext (unwrap contextType) "Context" Domain Nothing origin Nothing
    perspective rt = Perspective
      { id: unwrap rt
      , object: SQD (CDOM $ ST contextType) (RolGetter $ ENR rt)
          (RDOM $ ST $ QT.RoleInContext { context: contextType, role: rt })
          False
          False
      , displayName: unwrap rt
      , roleTypes: [ ENR rt ]
      , isEnumerated: true
      , roleVerbs: EM.empty
      , propertyVerbs:
          if rt == visibleType then EM.singleton (ContextState $ StateIdentifier $ unwrap contextType)
            [ PropertyVerbs (PSet [ ENP visibleProperty ]) (PSet [ Consult ]) ]
          else EM.empty
      , actions: EM.empty
      , selfOnly: rt == userType
      , authorOnly: false
      , isSelfPerspective: rt == userType
      , automaticStates: []
      , perspectiveStartPosition: Nothing
      }
  void $ storeDomeinFileInCache (ModelUri model) $ DomeinFile defaultDomeinFileRecord
    { id = ModelUri model
    , namespace = ModelUri model
    , contexts = Object.singleton (unwrap contextType) $ over Context
        (\c -> c { gebruikerRol = [ ENR userType, ENR otherUserType ] })
        modelContext
    , enumeratedRoles = Object.fromFoldable
        [ Tuple (unwrap userType) $ over EnumeratedRole
            (\r -> r { perspectives = [ perspective userType, perspective visibleType ] })
            (role userType UserRole)
        , Tuple (unwrap otherUserType) (role otherUserType UserRole)
        , Tuple (unwrap visibleType) $ over EnumeratedRole
            (\r -> r { properties = [ ENP visibleProperty ] })
            (role visibleType RoleInContext)
        , Tuple (unwrap hiddenType) (role hiddenType RoleInContext)
        , Tuple (unwrap externalType) (role externalType ExternalRole)
        ]
    , enumeratedProperties = Object.singleton (unwrap visibleProperty) $
        defaultEnumeratedProperty (unwrap visibleProperty) "Value" (unwrap visibleType) PString origin
    }
  case typeUri2ModelUri perspectivesUsers of
    Nothing -> liftAff $ assert "The peer role must have a model URI" false
    Just systemModel -> do
      let
        peerRole = over EnumeratedRole
          (\r -> r { properties = map (\p -> ENP $ EnumeratedPropertyType p) [ perspectivesUsersPublicKey, perspectivesUsersTransportPublicKey ] })
          (defaultEnumeratedRole perspectivesUsers "Peer" UserRole (unwrap contextType) origin)
      void $ storeDomeinFileInCache (ModelUri systemModel) $ DomeinFile defaultDomeinFileRecord
        { id = ModelUri systemModel
        , namespace = ModelUri systemModel
        , enumeratedRoles = Object.singleton perspectivesUsers peerRole
        , enumeratedProperties = Object.fromFoldable $ map
            (\p -> Tuple p $ defaultEnumeratedProperty p p perspectivesUsers PString origin)
            [ perspectivesUsersPublicKey, perspectivesUsersTransportPublicKey ]
        }

signed :: forall a. WriteForeign a => a -> SignedDelta
signed payload = SignedDelta
  { author: PerspectivesUser "def:#local"
  , encryptedDelta: writeJSON payload
  , signature: Nothing
  }

contextDelta :: SignedDelta
contextDelta = signed
  { resourceKey: unwrap contextId
  , resourceVersion: 1
  , deltaType: "ConstructEmptyContext"
  , id: contextId
  , contextType
  , subject: ENR userType
  }

externalLinkDelta :: SignedDelta
externalLinkDelta = signed
  { resourceKey: unwrap externalId
  , resourceVersion: 2
  , deltaType: "AddExternalRole"
  , contextInstance: contextId
  , contextType
  , roleInstance: externalId
  , roleType: externalType
  , subject: ENR userType
  }

roleDelta :: RoleInstance -> SignedDelta
roleDelta rid = signed
  { resourceKey: unwrap rid
  , resourceVersion: 1
  , deltaType: "ConstructEmptyRole"
  , roleInstance: rid
  , id: contextId
  , contextType
  , roleType:
      if rid == visibleId then visibleType
      else if rid == hiddenId then hiddenType
      else if rid == externalId then externalType
      else if elem rid [ localPeer, oldRemotePeer, newRemotePeer ] then EnumeratedRoleType perspectivesUsers
      else if rid == otherLocalId then otherUserType
      else userType
  , subject: ENR userType
  }

replacementDelta :: RoleInstance -> SignedDelta
replacementDelta oldFiller = signed
  { resourceKey: unwrap userId <> "#binding"
  , resourceVersion: 2
  , deltaType: "ReplaceBinding"
  , filled: userId
  , filledType: userType
  , filler: Just newRemotePeer
  , fillerType: Just $ EnumeratedRoleType perspectivesUsers
  , oldFiller: Just oldFiller
  , oldFillerType: Just $ EnumeratedRoleType perspectivesUsers
  , subject: ENR userType
  }

visiblePropertyDelta :: SignedDelta
visiblePropertyDelta = signed
  { resourceKey: unwrap visibleId <> "#" <> unwrap visibleProperty
  , resourceVersion: 1
  , deltaType: "SetProperty"
  , id: visibleId
  , roleType: visibleType
  , property: visibleProperty
  , values: [ Value "visible-value" ]
  , subject: ENR userType
  }

model :: String
model = "model://test.example#BindingReplacement"

contextType :: ContextType
contextType = ContextType $ model <> "$Context"

userType :: EnumeratedRoleType
userType = EnumeratedRoleType $ unwrap contextType <> "$User"

otherUserType :: EnumeratedRoleType
otherUserType = EnumeratedRoleType $ unwrap contextType <> "$OtherUser"

visibleType :: EnumeratedRoleType
visibleType = EnumeratedRoleType $ unwrap contextType <> "$Visible"

visibleProperty :: EnumeratedPropertyType
visibleProperty = EnumeratedPropertyType $ unwrap visibleType <> "$Value"

hiddenType :: EnumeratedRoleType
hiddenType = EnumeratedRoleType $ unwrap contextType <> "$Hidden"

externalType :: EnumeratedRoleType
externalType = EnumeratedRoleType $ unwrap contextType <> "$External"

contextId :: ContextInstance
contextId = ContextInstance "def:#replacement-context"

userId :: RoleInstance
userId = RoleInstance "def:#replacement-user"

visibleId :: RoleInstance
visibleId = RoleInstance "def:#visible"

hiddenId :: RoleInstance
hiddenId = RoleInstance "def:#hidden"

externalId :: RoleInstance
externalId = RoleInstance "def:#replacement-context$External"

remoteAlternativeId :: RoleInstance
remoteAlternativeId = RoleInstance "def:#remote-alternative"

localAlternativeId :: RoleInstance
localAlternativeId = RoleInstance "def:#local-alternative"

otherLocalId :: RoleInstance
otherLocalId = RoleInstance "def:#other-local"

localPeer :: RoleInstance
localPeer = RoleInstance "def:#local"

oldRemotePeer :: RoleInstance
oldRemotePeer = RoleInstance "def:#remote-old"

newRemotePeer :: RoleInstance
newRemotePeer = RoleInstance "def:#remote-new"
