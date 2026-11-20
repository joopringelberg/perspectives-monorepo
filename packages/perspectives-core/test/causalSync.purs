-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Test.CausalSync where

import Prelude

import Control.Monad.AvarMonadAsk as AA
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (any, find, head, length, null)
import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.AVar (new)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Data.String (Pattern(..), contains)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.StateCache (cacheCompiledContextState, clearModelStates)
import Perspectives.Assignment.Update (setProperty, withAuthoringRole)
import Perspectives.Authenticate (signDelta)
import Perspectives.ContextAndRole (defaultContextRecord)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##=))
import Perspectives.Cuid2 (cuid2)
import Perspectives.Data.EncodableMap as EM
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Instances.ObjectGetters (getProperty)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.ModelDependencies as DEP
import Perspectives.Names (getMySystem)
import Perspectives.InvertedQuery.Storable (saveInvertedQueries)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo')
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Persistence.API (addDocument_)
import Perspectives.Persistence.CausalStore (CausalInstallationDoc(..), CausalRoleDoc(..), causalDatabaseName, ensureCausalEpoch, getCausalRole, reserveCausalStamp)
import Perspectives.Persistence.DeltaStore (getDeltasForResource, pruneDeltasThrough, storeDeltaFromSignedDelta)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.QuarantineStore (QuarantinedTransaction(..), getQuarantinedTransactions, quarantineDatabaseName)
import Perspectives.Persistence.ResourceVersionStore (getResourceVersion, setResourceVersion)
import Perspectives.Persistent (getPerspectContext, getPerspectRol, saveEntiteit, tryGetPerspectContext, tryGetPerspectRol)
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Class.PersistentType (cacheInDomeinFile, getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), defaultEnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..), StateSpec(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleKind(..), RoleType(..), StateIdentifier(..))
import Perspectives.Representation.Verbs (RoleVerb(..), RoleVerbList(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.RunMonadPerspectivesTransaction (exitContext, phase2)
import Perspectives.RunPerspectives (runPerspectivesWithoutCouchdb)
import Perspectives.SaveUserData (removeContextInstance, scheduleContextRemoval, scheduleRoleRemoval, synchronise)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.StrippedDelta (stripResourceSchemes)
import Perspectives.Sync.Causal (CausalStamp, Clock, currentCausalEpoch, readCausalStamp, validateCausalDelta)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.HandleTransaction (executeTransaction, executeUniverseRoleDelta)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..), createTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), currentTransactionForPeer)
import Perspectives.Sync.VersionedDelta (DeltaEnvelope(..), parseIncomingDelta, signVersionedDelta)
import Perspectives.TypesForDeltas (RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Simple.JSON (class WriteForeign, writeJSON)
import Test.BindingReplacement as Binding
import Test.PropertyHistory as History
import Test.Persistence.Recovery (recoveringDatabase)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Test.VerificationBundles (withSigningFixture)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Causal role deletion and coordinated cutover" do
  for_ [ RemoveRoleInstance, RetractRoleInstance ] \operation ->
    test ("receiving " <> show operation <> " does not reserve a local deletion version or causal dot") $
      withSigningFixture true \key -> do
        prepare key
        grantDeletionPermissions
        let target = if operation == RetractRoleInstance then Binding.userId else Binding.visibleId
        beforeVersion <- lift $ getResourceVersion (unwrap target)
        CausalRoleDoc beforeClock <- lift $ getCausalRole (unwrap target)
        signed <- signPayload "remote-old" $ stripResourceSchemes $ removal operation target Binding.contextId
        AA.modify \t -> Transaction (unwrap t) { isExecutingIncomingDeltas = true }
        executeUniverseRoleDelta
          (removal operation target Binding.contextId)
          signed
        afterVersion <- lift $ getResourceVersion (unwrap target)
        CausalRoleDoc afterClock <- lift $ getCausalRole (unwrap target)
        Transaction tr <- AA.get
        liftAff do
          equal beforeVersion afterVersion
          equal beforeClock.clock afterClock.clock
          assert "Received removal must still schedule physical deletion" $
            any
              ( case _ of
                  RoleRemoval rid -> rid == target
                  _ -> false
              )
              tr.scheduledAssignments
          assert "Received removal must still schedule state exits" $ any (_ == target) tr.rolesToExit
          assert "Received removal must not originate deltas" $ null tr.deltas

  for_ [ RemoveExternalRoleInstance, RemoveUnboundExternalRoleInstance ] \operation ->
    test ("receiving " <> show operation <> " preserves local exit effects without echoing the deletion") $
      withSigningFixture true \key -> do
        prepare key
        grantDeletionPermissions
        installExitPerspectiveQueries
        lift do
          PerspectRol remote <- getPerspectRol Binding.oldRemotePeer
          void $ cacheEntity Binding.oldRemotePeer $ PerspectRol remote { isMe = false }
          PerspectContext context <- getPerspectContext Binding.contextId
          void $ cacheEntity Binding.contextId $ PerspectContext context
            { states = [ StateIdentifier $ unwrap Binding.contextType ] }
          for_ [ Binding.userId, Binding.visibleId ] \rid -> do
            PerspectRol role <- getPerspectRol rid
            void $ cacheEntity rid $ PerspectRol role { states = [ StateIdentifier $ unwrap role.pspType ] }
        removed <- Binding.addFilledContext "received-removal" Binding.userType Binding.localPeer true
        reaction <- Binding.addFilledContext "exit-reaction" Binding.userType Binding.oldRemotePeer false
        let
          external = RoleInstance $ unwrap removed.context <> "$External"
          root = StateIdentifier $ unwrap Binding.contextType
          onExit _ = withAuthoringRole (ENR Binding.userType) do
            setProperty [ Binding.visibleId ] Binding.visibleProperty Nothing [ Value "exit reaction" ]
            scheduleContextRemoval (Just $ ENR Binding.userType) [] reaction.context
            void $ scheduleRoleRemoval synchronise Binding.visibleId
          isExitProperty signed = case parseIncomingDelta (unwrap signed).encryptedDelta of
            Right (RolePropertyEnvelope (RolePropertyDelta d)) ->
              d.property == Binding.visibleProperty && d.values == [ Value "exit reaction" ]
                && d.subject == ENR Binding.userType
            _ -> false
          isReactionDeletion signed = case parseIncomingDelta (unwrap signed).encryptedDelta of
            Right (UniverseRoleEnvelope (UniverseRoleDelta d)) ->
              takeGuid (unwrap d.id) == takeGuid (unwrap reaction.context)
                && d.deltaType == RemoveExternalRoleInstance
                && d.subject == ENR Binding.userType
            _ -> false
          isExitRoleDeletion signed = case parseIncomingDelta (unwrap signed).encryptedDelta of
            Right (UniverseRoleEnvelope (UniverseRoleDelta d)) ->
              takeGuid (unwrap d.roleInstance) == takeGuid (unwrap Binding.visibleId)
                && d.deltaType == RemoveRoleInstance
                && d.subject == ENR Binding.userType
            _ -> false
        void $ pure $ cacheCompiledContextState root
          { query: \_ -> pure (Value "true")
          , objectGetter: Nothing
          , automaticOnEntry: Map.empty
          , automaticOnExit: Map.singleton (ENR Binding.userType) onExit
          , notifyOnEntry: Map.empty
          , notifyOnExit: Map.empty
          , perspectivesOnEntry: Map.empty
          }
        result <- try do
          lift do
            PerspectContext context <- getPerspectContext removed.context
            void $ cacheEntity removed.context $ PerspectContext context { states = [ root ] }
          beforeVersion <- lift $ getResourceVersion (unwrap external)
          CausalRoleDoc beforeClock <- lift $ getCausalRole (unwrap external)
          signed <- signPayload "remote-old" $ stripResourceSchemes $ removal operation external removed.context
          AA.modify \t -> Transaction (unwrap t) { isExecutingIncomingDeltas = true }
          executeUniverseRoleDelta (removal operation external removed.context) signed
          Transaction incoming <- AA.get
          assignment <-
            case
              find
                ( case _ of
                    ContextRemoval cid _ _ -> cid == removed.context
                    _ -> false
                )
                incoming.scheduledAssignments
              of
              Nothing -> throwError $ error "Received context removal was not scheduled"
              Just scheduled -> pure scheduled
          sharing <- liftAff $ createTransaction (ENR $ EnumeratedRoleType DEP.sysUser) true >>= new
          -- Exercise the same fresh sharing boundary used by runSharing, without sending to a broker.
          Transaction effects <- lift $ runReaderT
            (unsafePartial (exitContext Set.empty assignment) *> AA.get)
            sharing
          afterVersion <- lift $ getResourceVersion (unwrap external)
          CausalRoleDoc afterClock <- lift $ getCausalRole (unwrap external)
          history <- lift $ getDeltasForResource History.propertyKey
          reactionHistory <- lift $ getDeltasForResource (unwrap reaction.context <> "$External")
          liftAff do
            equal beforeVersion afterVersion
            equal beforeClock.clock afterClock.clock
            assert "Exit property changes must retain their local authorization subject" $
              any (\(DeltaStoreRecord r) -> isExitProperty r.signedDelta) history
            assert "Exit-action deletion of another context must be generated for peers" $
              any (\(DeltaStoreRecord r) -> isReactionDeletion r.signedDelta) reactionHistory
            assert "Exit property changes must remain in the sharing transaction" $
              any (\(DeltaInTransaction d) -> isExitProperty d.delta) effects.deltas
            assert "Exit-action context deletion must remain in the sharing transaction" $
              any (\(DeltaInTransaction d) -> isReactionDeletion d.delta) effects.deltas
            assert "Exit-action role deletion must remain in the sharing transaction" $
              any (\(DeltaInTransaction d) -> isExitRoleDeletion d.delta) effects.deltas
          exited <- lift $ getPerspectContext removed.context
          liftAff $ assert "Received context must exit its active state" $ null (unwrap exited).states
          lift $ removeContextInstance removed.context Nothing
          absent <- isNothing <$> lift (tryGetPerspectContext removed.context)
          liftAff $ assert "Received context must still be physically removable" absent
        void $ pure $ clearModelStates (ModelUri Binding.model)
        case result of
          Left err -> throwError err
          Right _ -> pure unit

  for_ [ "local", "remote-old" ] \writer ->
    test ("deletes a role after observing the earlier property write by " <> writer) $
      withSigningFixture true \key -> do
        prepare key
        modification <- propertyChange writer 2 $ stamp writer 1 []
        receive [ modification ]
        value <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
        liftAff $ equal [ Value "changed" ] value
        deletion <- roleDeletion $ stamp "local" 2 [ dot writer 1 ]
        receive [ deletion ]
        absent <- isNothing <$> lift (tryGetPerspectRol Binding.visibleId)
        liftAff $ assert "Historical writes must not prevent physical role deletion" absent

  test "a genuinely concurrent write suppresses deletion" $
    withSigningFixture true \key -> do
      prepare key
      modification <- propertyChange "remote-old" 2 $ stamp "remote-old" 1 []
      deletion <- roleDeletion $ stamp "local" 2 []
      receive [ modification ]
      receive [ deletion ]
      present <- isJust <$> lift (tryGetPerspectRol Binding.visibleId)
      value <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
      liftAff do
        assert "A concurrent edit must retain the role" present
        equal [ Value "changed" ] value

  test "a concurrent write arriving after deletion restores the role and its property" $
    withSigningFixture true \key -> do
      prepare key
      deletion <- roleDeletion $ stamp "local" 2 []
      receive [ deletion ]
      absent <- isNothing <$> lift (tryGetPerspectRol Binding.visibleId)
      liftAff $ assert "The role must actually have been deleted first" absent
      modification <- propertyChange "remote-old" 2 $ stamp "remote-old" 1 []
      receive [ modification ]
      present <- isJust <$> lift (tryGetPerspectRol Binding.visibleId)
      value <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
      liftAff do
        assert "A concurrent edit must restore the deleted role" present
        equal [ Value "changed" ] value

  test "an observed delayed write neither resurrects a role nor waits for obsolete version gaps" $
    withSigningFixture true \key -> do
      prepare key
      deletion <- roleDeletion $ stamp "local" 2 [ dot "local" 1 ]
      receive [ deletion ]
      modification <- propertyChange "local" 5 $ stamp "local" 1 []
      receive [ modification ]
      absent <- isNothing <$> lift (tryGetPerspectRol Binding.visibleId)
      version <- lift $ getResourceVersion History.propertyKey
      records <- lift $ getDeltasForResource History.propertyKey
      liftAff do
        assert "An observed old write must not resurrect the role" absent
        equal 5 version
        assert "Covered history must remain available for later concurrent restoration"
          $ isJust
          $ find (\(DeltaStoreRecord r) -> r.resourceVersion == 5 && r.applied && r.disposition == Just "CoveredByDeletion") records

  test "an unauthorised deletion cannot establish a causal tombstone" $
    withSigningFixture true \key -> do
      prepare key
      EnumeratedRole metadata <- lift $ getEnumeratedRole Binding.userType
      let revoke (Perspective p) = Perspective p { roleVerbs = EM.empty }
      lift $ cacheInDomeinFile Binding.userType $ EnumeratedRole metadata { perspectives = map revoke metadata.perspectives }
      deletion <- roleDeletion $ stamp "local" 2 []
      result <- try $ receive [ deletion ]
      CausalRoleDoc state <- lift $ getCausalRole (unwrap Binding.visibleId)
      present <- isJust <$> lift (tryGetPerspectRol Binding.visibleId)
      liftAff do
        assert "Rejected deletion must report failure" $ isLeft result
        equal [] state.deletions
        assert "Rejected deletion must leave the role intact" present

  test "reserves persistent installation-specific counters across role storage aliases" $
    Binding.withFixture Binding.localPeer [] [] do
      installation <- lift ensureCausalEpoch
      first <- lift $ reserveCausalStamp (unwrap Binding.visibleId)
      second <- lift $ reserveCausalStamp "visible"
      CausalRoleDoc stored <- lift $ getCausalRole "visible"
      sameInstallation <- lift ensureCausalEpoch
      liftAff do
        equal installation sameInstallation
        equal first.dot.actor second.dot.actor
        equal 2 second.dot.counter
        equal [ first.dot ] second.context
        equal [ second.dot ] stored.clock
        assert "The role clock must be durably stored" $ isJust stored._rev

  test "signs property and binding changes with one causal clock for their role" $
    withSigningFixture true \_ -> do
      property <- signVersionedDelta $ RolePropertyDelta
        { resourceKey: History.propertyKey
        , resourceVersion: 2
        , deltaType: SetProperty
        , id: RoleInstance "visible"
        , roleType: Binding.visibleType
        , property: Binding.visibleProperty
        , values: [ Value "changed" ]
        , subject: ENR Binding.userType
        }
      binding <- signVersionedDelta $ RoleBindingDelta
        { resourceKey: unwrap Binding.visibleId <> "#binding"
        , resourceVersion: 1
        , deltaType: SetFirstBinding
        , filled: RoleInstance "visible"
        , filledType: Binding.visibleType
        , filler: Just Binding.localPeer
        , fillerType: Just $ EnumeratedRoleType DEP.perspectivesUsers
        , oldFiller: Nothing
        , oldFillerType: Nothing
        , subject: ENR Binding.userType
        }
      liftAff do
        equal (Right unit) $ validateCausalDelta property
        equal (Right unit) $ validateCausalDelta binding
        case readCausalStamp (unwrap property).encryptedDelta, readCausalStamp (unwrap binding).encryptedDelta of
          Right (Just first), Right (Just second) -> do
            equal first.dot.actor second.dot.actor
            equal (first.dot.counter + 1) second.dot.counter
            equal [ first.dot ] second.context
          _, _ -> assert "Every newly signed role mutation needs causal metadata" false

  test "recovers causal counters after restarting and separates installations of the same author" do
    name <- liftEffect $ cuid2 "causalPersistence"
    first <- runPerspectivesWithoutCouchdb name $ reserveCausalStamp "visible"
    second <- runPerspectivesWithoutCouchdb name $ reserveCausalStamp "visible"
    other <- runPerspectivesWithoutCouchdb (name <> "other") do
      AA.modify \s -> s { perspectivesUser = PerspectivesUser name }
      reserveCausalStamp "visible"
    equal first.dot.actor second.dot.actor
    equal (first.dot.counter + 1) second.dot.counter
    equal [ first.dot ] second.context
    assert "Independent installations of one author need different actors" $ other.dot.actor /= first.dot.actor

  test "validates actor ownership, counters and required stamps before mutation" $
    withSigningFixture true \key -> do
      prepare key
      valid <- propertyChange "local" 2 $ stamp "local" 1 []
      wrongAuthor <- propertyChange "local" 2 $ stamp "remote-old" 1 []
      zero <- propertyChange "local" 2 $ stamp "local" 0 []
      selfCovered <- propertyChange "local" 2 $ stamp "local" 1 [ dot "local" 1 ]
      let
        missing = Binding.signed
          { deltaFormatVersion: 4
          , id: "visible"
          , property: unwrap Binding.visibleProperty
          }
      liftAff do
        equal (Right unit) $ validateCausalDelta valid
        for_ [ wrongAuthor, zero, selfCovered, missing ] \invalid ->
          assert "Invalid causal metadata must be rejected" $ isLeft $ validateCausalDelta invalid
      receive [ wrongAuthor ]
      value <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
      liftAff $ equal [ Value "visible-value" ] value

  test "missing and unsupported transaction epochs are durably quarantined without side effects" $
    withSigningFixture true \key -> do
      prepare key
      modification <- propertyChange "local" 2 $ stamp "local" 1 []
      incoming <- packet [ modification ]
      for_ [ Nothing, Just "unsupported-epoch" ] \epoch -> do
        let legacy = TransactionForPeer (unwrap incoming) { causalEpoch = epoch }
        executeTransaction legacy
        executeTransaction legacy
      quarantined <- lift getQuarantinedTransactions
      version <- lift $ getResourceVersion History.propertyKey
      value <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
      liftAff do
        equal 2 (length quarantined)
        equal 1 version
        equal [ Value "visible-value" ] value
        for_ quarantined \(QuarantinedTransaction doc) -> do
          assert "Quarantine must be persistent" $ isJust doc._rev
          equal [ modification ] (unwrap doc.transaction).deltas

  test "a conflicting persisted epoch is an explicit failure, not a successful receive" $
    Binding.withFixture Binding.localPeer [] [] do
      db <- lift causalDatabaseName
      void $ lift $ addDocument_ db
        (CausalInstallationDoc { _id: "installation", _rev: Nothing, epoch: "unsupported", installation: "test-installation" })
        "installation"
      incoming <- packet []
      result <- try $ executeTransaction incoming
      liftAff $ assert "Cutover persistence failures must propagate to the transport" $ isLeft result

  test "quarantine write failures propagate instead of allowing transport acknowledgement" $
    Binding.withFixture Binding.localPeer [] [] do
      void $ lift ensureCausalEpoch
      stub <- liftEffect recoveringDatabase
      db <- lift quarantineDatabaseName
      lift $ AA.modify \s -> s { databases = Object.insert db stub.database s.databases }
      incoming <- packet []
      result <- try $ executeTransaction $ TransactionForPeer (unwrap incoming) { causalEpoch = Nothing }
      liftAff $ case result of
        Left failure -> assert "Expected the actual write failure" $ contains (Pattern "Simulated write failure") (message failure)
        Right _ -> assert "Failed quarantine persistence must not look successful" false

dot :: String -> Int -> { actor :: String, counter :: Int }
dot author counter = { actor: writeJSON [ author, "causal-test-installation" ], counter }

stamp :: String -> Int -> Clock -> CausalStamp
stamp author counter context = { epoch: currentCausalEpoch, dot: dot author counter, context }

signPayload :: forall a. WriteForeign a => String -> a -> MonadPerspectivesTransaction SignedDelta
signPayload author payload = do
  SignedDelta signed <- signDelta $ writeJSON payload
  pure $ SignedDelta signed { author = PerspectivesUser author }

propertyChange :: String -> Int -> CausalStamp -> MonadPerspectivesTransaction SignedDelta
propertyChange author version causal = signPayload author
  { deltaFormatVersion: 4
  , resourceKey: History.propertyKey
  , resourceVersion: version
  , deltaType: "SetProperty"
  , id: "visible"
  , roleType: unwrap Binding.visibleType
  , property: unwrap Binding.visibleProperty
  , values: [ Value "changed" ]
  , subject: unwrap Binding.otherUserType
  , subjectKind: "ENR"
  , causal
  }

roleDeletion :: CausalStamp -> MonadPerspectivesTransaction SignedDelta
roleDeletion causal = signPayload "local"
  { deltaFormatVersion: 4
  , resourceKey: unwrap Binding.visibleId
  , resourceVersion: 2
  , deltaType: "RemoveRoleInstance"
  , id: takeGuid $ unwrap Binding.contextId
  , contextType: unwrap Binding.contextType
  , roleType: unwrap Binding.visibleType
  , roleInstance: "visible"
  , subject: unwrap Binding.userType
  , subjectKind: "ENR"
  , causal
  }

packet :: Array SignedDelta -> MonadPerspectivesTransaction TransactionForPeer
packet deltas = do
  Transaction tr <- AA.get
  keys <- for deltas \(SignedDelta { author }) -> do
    values <- lift (Binding.localPeer ##= getProperty (EnumeratedPropertyType DEP.perspectivesUsersPublicKey))
    case head values of
      Just (Value key) -> pure $ Tuple author { key, transportKey: "unused", deltas: [] }
      Nothing -> throwError $ error "Missing signing key in causal test fixture"
  pure $ currentTransactionForPeer
    { author: PerspectivesUser "local"
    , perspectivesSystem: Binding.contextId
    , timeStamp: tr.timeStamp
    , publicKeys: EM.fromFoldable keys
    , deltas
    }

receive :: Array SignedDelta -> MonadPerspectivesTransaction Unit
receive deltas = do
  incoming <- packet deltas
  executeTransaction incoming
  phase2 false (ENR Binding.userType) unit

prepare :: String -> MonadPerspectivesTransaction Unit
prepare key = do
  History.installAuthor (PerspectivesUser "remote-old") key
  History.grantOriginalWriter
  EnumeratedRole metadata <- lift $ getEnumeratedRole Binding.userType
  let
    grant (Perspective p) = Perspective p
      { roleVerbs = EM.singleton (ContextState $ StateIdentifier $ unwrap Binding.contextType) (Including [ Create, Remove ])
      }
  lift $ cacheInDomeinFile Binding.userType $ EnumeratedRole metadata { perspectives = map grant metadata.perspectives }
  mySystem <- lift getMySystem
  void $ lift $ cacheEntity (ContextInstance mySystem) $ PerspectContext defaultContextRecord
    { id = ContextInstance mySystem
    , pspType = Binding.contextType
    }
  for_ [ DEP.itemsOnClipboard, DEP.recentContexts, DEP.pinnedContexts, DEP.notifications ] \role ->
    lift $ cacheInDomeinFile (EnumeratedRoleType role) $
      defaultEnumeratedRole role role RoleInContext (unwrap Binding.contextType) (ArcPosition { line: 1, column: 1 })
  creation <- signPayload "local"
    { resourceKey: unwrap Binding.visibleId
    , resourceVersion: 1
    , deltaType: "ConstructEmptyRole"
    , id: ContextInstance $ takeGuid $ unwrap Binding.contextId
    , contextType: Binding.contextType
    , roleType: Binding.visibleType
    , roleInstance: RoleInstance "visible"
    , subject: ENR Binding.userType
    }
  initial <- signPayload "local"
    { resourceKey: History.propertyKey
    , resourceVersion: 1
    , deltaType: "SetProperty"
    , id: RoleInstance "visible"
    , roleType: Binding.visibleType
    , property: Binding.visibleProperty
    , values: [ Value "visible-value" ]
    , subject: ENR Binding.userType
    }
  for_ [ { key: unwrap Binding.visibleId, signed: creation }, { key: History.propertyKey, signed: initial } ] \baseline -> do
    lift $ storeDeltaFromSignedDelta baseline.signed
    records <- lift $ getDeltasForResource baseline.key
    for_ (find (\(DeltaStoreRecord r) -> r.signedDelta == baseline.signed) records) \(DeltaStoreRecord r) ->
      lift $ pruneDeltasThrough baseline.key 1 r._id
    lift $ setResourceVersion baseline.key 1
  lift $ void $ saveEntiteit Binding.contextId
  lift $ void $ saveEntiteit Binding.visibleId

grantDeletionPermissions :: MonadPerspectivesTransaction Unit
grantDeletionPermissions = do
  EnumeratedRole metadata <- lift $ getEnumeratedRole Binding.userType
  let
    grant (Perspective p) = Perspective p
      { roleVerbs = EM.singleton (ContextState $ StateIdentifier $ unwrap Binding.contextType)
          (Including [ Create, Remove, RemoveContext, RemoveContextLocally ])
      }
  lift $ cacheInDomeinFile Binding.userType $
    EnumeratedRole metadata { perspectives = map grant metadata.perspectives }

removal :: UniverseRoleDeltaType -> RoleInstance -> ContextInstance -> UniverseRoleDelta
removal deltaType roleInstance id = UniverseRoleDelta
  { id
  , contextType: Binding.contextType
  , roleType:
      if deltaType == RemoveRoleInstance then Binding.visibleType
      else if deltaType == RetractRoleInstance then Binding.userType
      else Binding.externalType
  , roleInstance
  , authorizedRole: Just $ ENR Binding.userType
  , deltaType
  , subject: ENR Binding.userType
  , resourceKey: unwrap roleInstance
  , resourceVersion: 2
  }

installExitPerspectiveQueries :: MonadPerspectivesTransaction Unit
installExitPerspectiveQueries = do
  let
    source = "domain " <> Binding.model <> "\n"
      <> "  case Context\n"
      <> "    user User\n"
      <> "      perspective on Visible\n"
      <> "        only (Remove)\n"
      <> "        props (Value) verbs (Consult, SetPropertyValue)\n"
      <> "    thing Visible\n"
      <> "      property Value (String)\n"
  parsed <- liftAff $ runIndentParser source ARC.domain
  root <- case parsed of
    Left err -> throwError $ error $ show err
    Right value -> pure value
  Tuple result state <- liftAff $ runPhaseTwo' (traverseDomain root)
  model <- case result of
    Left err -> throwError $ error $ show err
    Right (DomeinFile value) -> pure value
  compiled <- liftAff $ runP $ phaseThree model state.postponedStateQualifiedParts state.screens
  case compiled of
    Left err -> throwError $ error $ show err
    Right (Tuple _ queries) -> lift $ saveInvertedQueries queries
