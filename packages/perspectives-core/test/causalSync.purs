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
import Data.Array (any, elem, find, head, length, mapMaybe, null)
import Data.List.NonEmpty as NonEmptyList
import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Decacheable (decache)
import Effect (Effect)
import Effect.Aff.AVar (new)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Data.String (Pattern(..), contains)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.StateCache (cacheCompiledContextState, clearModelStates)
import Perspectives.Assignment.SerialiseAsDeltas (serialiseDependencies)
import Perspectives.Assignment.Update (setProperty, withAuthoringRole)
import Perspectives.Authenticate (signDelta)
import Perspectives.CollectAffectedContexts (compileBoth, createDeltasFromAssumption, handleNewCalculatedUsersForBinding)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (InformedAssumption(..), MonadPerspectivesTransaction, (##=))
import Perspectives.Cuid2 (cuid2)
import Perspectives.Data.EncodableMap as EM
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Deltas (sortTransactionDeltas)
import Perspectives.Instances.ObjectGetters (getProperty)
import Perspectives.Identifiers (deconstructBuitenRol)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.ModelDependencies as DEP
import Perspectives.Names (getMySystem)
import Perspectives.InvertedQuery.Storable (saveInvertedQueries)
import Perspectives.InvertedQuery (InvertedQuery(..))
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo')
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Persistence.API (addDocument_, deleteDatabase, documentsInRange, getDocument, withDatabase)
import Perspectives.Persistence.CausalStore (CausalInstallationDoc(..), CausalRoleDoc(..), causalDatabaseName, ensureCausalEpoch, getCausalRole, recordCausalDelta, reserveCausalStamp)
import Perspectives.Persistence.DeltaStore (getDeltasForResource, pruneDeltasThrough, storeDeltaFromSignedDelta)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.PendingTransactionStore (pendingTransactionDatabaseName)
import Perspectives.Persistence.QuarantineStore (QuarantinedTransaction(..), getQuarantinedTransactions, quarantineDatabaseName)
import Perspectives.Persistence.ResourceVersionStore (getResourceVersion, setResourceVersion)
import Perspectives.Persistent (forceSaveContext, forceSaveRole, getPerspectContext, getPerspectRol, saveEntiteit, tryGetPerspectContext, tryGetPerspectRol)
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Query.QueryTypes (RoleInContext(..)) as QT
import Perspectives.Representation.Class.PersistentType (cacheInDomeinFile, getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), defaultEnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..), StateSpec(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleKind(..), RoleType(..), StateIdentifier(..))
import Perspectives.Representation.Verbs (RoleVerb(..), RoleVerbList(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.RoleAssignment (filledPointsTo)
import Perspectives.RunMonadPerspectivesTransaction (exitContext, phase2)
import Perspectives.RunPerspectives (runPerspectivesWithoutCouchdb)
import Perspectives.SaveUserData (removeContextInstance, scheduleContextRemoval, scheduleRoleRemoval, setFirstBinding, synchronise)
import Perspectives.ScheduledAssignment (ScheduledAssignment(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.StrippedDelta (stripResourceSchemes)
import Perspectives.Sync.Causal (CausalStamp, Clock, currentCausalEpoch, readCausalStamp, validateCausalDelta)
import Perspectives.Sync.ConstructionBaseline (sameResource, selectRoleBaseline)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.HandleTransaction (Delta(..), executeDeltas, executeTransaction, executeUniverseRoleDelta)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..), createTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), currentTransactionForPeer)
import Perspectives.Sync.VersionedDelta (DeltaEnvelope(..), parseIncomingDelta, signVersionedDelta)
import Perspectives.TypesForDeltas (ContextDelta(..), RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), UniverseContextDelta(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
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
  test "construction serialisation excludes obsolete lifecycle removals and memberships" $
    withSigningFixture true \key -> do
      prepare key
      lift $ filledPointsTo Binding.userId Binding.newRemotePeer
      let
        external = RoleInstance $ unwrap Binding.contextId <> "$External"
      context <- signPayload "local"
        { deltaFormatVersion: 4
        , resourceKey: takeGuid $ unwrap Binding.contextId
        , resourceVersion: 0
        , deltaType: "ConstructEmptyContext"
        , id: takeGuid $ unwrap Binding.contextId
        , contextType: unwrap Binding.contextType
        , subject: unwrap Binding.userType
        , subjectKind: "ENR"
        }
      oldExternalCreation <- constructionFor external true "local" (stamp "local" 1 [])
      oldExternalMembership <- membershipFor external true "local" 1 (stamp "local" 2 [ dot "local" 1 ])
      oldExternalRemoval <- signPayload "local"
        { deltaFormatVersion: 4
        , resourceKey: takeGuid $ unwrap external
        , resourceVersion: 2
        , deltaType: "RemoveExternalRoleInstance"
        , id: takeGuid $ unwrap Binding.contextId
        , contextType: unwrap Binding.contextType
        , roleType: unwrap Binding.externalType
        , roleInstance: takeGuid $ unwrap external
        , subject: unwrap Binding.userType
        , subjectKind: "ENR"
        , causal: stamp "local" 3 [ dot "local" 2 ]
        }
      externalCreation <- constructionFor external true "local" (stamp "local" 4 [ dot "local" 3 ])
      externalMembership <- membershipFor external true "local" 38 (stamp "local" 5 [ dot "local" 4 ])
      original <- constructionFor Binding.visibleId false "local" (stamp "local" 1 [])
      oldMembership <- membershipFor Binding.visibleId false "local" 1 (stamp "local" 2 [ dot "local" 1 ])
      deletion <- roleDeletion (stamp "local" 3 [ dot "local" 2 ])
      recreated <- constructionFor Binding.visibleId false "local" (stamp "local" 4 [ dot "local" 3 ])
      currentMembership <- membershipFor Binding.visibleId false "local" 38 (stamp "local" 5 [ dot "local" 4 ])
      lift do
        void $ cacheEntity external $ PerspectRol defaultRolRecord
          { id = external, context = Binding.contextId, pspType = Binding.externalType }
        for_ [ context, oldExternalCreation, oldExternalMembership, oldExternalRemoval, externalCreation, externalMembership, original, oldMembership, deletion, recreated, currentMembership ]
          storeDeltaFromSignedDelta
      serialiseDependencies [ Binding.newRemotePeer ] (NonEmptyList.singleton $ R Binding.visibleId)
      Transaction serialized <- AA.get
      let exported = map (\(DeltaInTransaction d) -> d.delta) $ sortTransactionDeltas serialized.deltas
      liftAff do
        assert "Current construction must be exported" $ any (_ == recreated) exported
        assert "Current membership must be exported" $ any (_ == currentMembership) exported
        assert "An old construction must not be exported" $ not $ any (_ == original) exported
        assert "An old membership must not be exported" $ not $ any (_ == oldMembership) exported
        assert "A lifecycle removal must not be exported" $ not $ any (_ == deletion) exported
        assert "An obsolete context removal must not be exported" $ not $ any (_ == oldExternalRemoval) exported
      retained <- lift $ getDeltasForResource (unwrap Binding.visibleId)
      liftAff $ assert "Obsolete deletion evidence must remain stored" $
        any (\(DeltaStoreRecord r) -> r.signedDelta == deletion) retained
      for_
        [ Tuple "perspective assumptions" $
            createDeltasFromAssumption [ Binding.newRemotePeer ] (RoleAssumption Binding.contextId Binding.visibleType)
        , Tuple "first binding" do
            EnumeratedRole hidden <- lift $ getEnumeratedRole Binding.hiddenType
            lift $ cacheInDomeinFile Binding.hiddenType $ EnumeratedRole hidden
              { binding = Just $ ST $ QT.RoleInContext { context: Binding.contextType, role: Binding.visibleType } }
            void $ installPerspectiveQueries $ "domain " <> Binding.model <> "\n"
              <> "  case Context\n"
              <> "    user User\n"
              <> "      perspective on Hidden >> binding\n"
              <> "        props (Value) verbs (Consult)\n"
              <> "    thing Hidden filledBy Visible\n"
              <> "    thing Visible\n"
              <> "      property Value (String)\n"
            void $ setFirstBinding Binding.hiddenId Binding.visibleId Nothing
        , Tuple "calculated-user initialisation" do
            queries <- installPerspectiveQueries $ "domain " <> Binding.model <> "\n"
              <> "  case Context\n"
              <> "    user User\n"
              <> "    user CalculatedUser = Hidden >> binding >> context >> User\n"
              <> "      perspective on Visible\n"
              <> "        props (Value) verbs (Consult)\n"
              <> "    thing Hidden filledBy Visible\n"
              <> "    thing Visible\n"
              <> "      property Value (String)\n"
            calculated <- for queries \(InvertedQuery query) ->
              if isJust query.calculatedUserRoleType then do
                compiled <- lift $ compileBoth $ InvertedQuery query
                handleNewCalculatedUsersForBinding Binding.visibleId Binding.hiddenId compiled
                pure true
              else pure false
            liftAff $ assert "The model must supply calculated-user detection queries" $ elem true calculated
        ]
        \(Tuple path disclose) -> do
          AA.modify \(Transaction transaction) -> Transaction transaction { deltas = [], deltaMembership = Set.empty }
          disclose
          Transaction disclosed <- AA.get
          let selected = map (\(DeltaInTransaction d) -> d.delta) disclosed.deltas
          liftAff do
            let
              expected = [ context, externalCreation, externalMembership ] <>
                if path == "calculated-user initialisation" then [] else [ recreated, currentMembership ]
            for_ expected \current ->
              assert (path <> " must disclose " <> show current) $ elem current selected
            for_ [ oldExternalCreation, oldExternalMembership, oldExternalRemoval, original, oldMembership, deletion ] \obsolete ->
              assert (path <> " must exclude obsolete lifecycle evidence") $ not $ elem obsolete selected
      Transaction bound <- AA.get
      let
        database = "https://construction.invalid/cw_construction_baseline/"
        publicContext = ContextInstance $ "pub:" <> database <> "#replacement-context"
        publicExternal = RoleInstance $ unwrap publicContext <> "$External"
        publication = mapMaybe
          ( \signed -> case parseIncomingDelta (unwrap signed).encryptedDelta of
              Right (UniverseRoleEnvelope (UniverseRoleDelta role)) | sameResource (unwrap role.roleInstance) (unwrap external) ->
                Just $ URD signed (UniverseRoleDelta $ role { id = publicContext, roleInstance = publicExternal })
              Right (UniverseContextEnvelope (UniverseContextDelta creation)) ->
                Just $ UCD signed (UniverseContextDelta $ creation { id = publicContext })
              Right (ContextEnvelope (ContextDelta membership)) | sameResource (unwrap membership.roleInstance) (unwrap external) ->
                Just $ CDD signed (ContextDelta $ membership { contextInstance = publicContext, roleInstance = publicExternal })
              _ -> Nothing
          )
          (map (\(DeltaInTransaction d) -> d.delta) $ sortTransactionDeltas bound.deltas)
      lift $ withDatabase "cw_construction_baseline" \db ->
        AA.modify \state -> state { databases = Object.insert database db state.databases }
      executeDeltas publication
      Transaction published <- AA.get
      liftAff $ assert "Publication must not schedule deletion of the new public representation"
        $ not
        $ any
            ( case _ of
                ContextRemoval contextId _ _ -> contextId == publicContext
                RoleRemoval roleId -> roleId == publicExternal
                _ -> false
            )
            published.scheduledAssignments
      lift do
        forceSaveContext publicContext
        forceSaveRole publicExternal
      PerspectContext publishedContext <- lift $ getDocument database "replacement-context"
      PerspectRol publishedExternal <- lift $ getDocument database "replacement-context$External"
      liftAff do
        equal publicContext publishedContext.id
        equal publicExternal publishedExternal.id
      lift $ deleteDatabase "cw_construction_baseline"
      lift do
        decache Binding.contextId
        decache external
        setResourceVersion (unwrap external) 37
        recordCausalDelta oldExternalRemoval
      receive [ externalMembership, context, externalCreation ]
      restoredContext <- lift $ tryGetPerspectContext Binding.contextId
      restoredExternal <- lift $ tryGetPerspectRol external
      restoredVersion <- lift $ getResourceVersion (unwrap external)
      liftAff do
        assert "An identical stored context template must reconstruct an absent context" $ isJust restoredContext
        assert "Its external role must survive the retained old removal" $ isJust restoredExternal
        equal 38 restoredVersion

  test "concurrent constructions are not arbitrarily selected as a baseline" $
    withSigningFixture true \_ -> do
      local <- constructionFor Binding.visibleId false "local" (stamp "local" 1 [])
      remote <- constructionFor Binding.visibleId false "remote-old" (stamp "remote-old" 1 [])
      liftAff $ assert "Concurrent construction requires explicit reconciliation"
        $ isLeft
        $ selectRoleBaseline Binding.visibleId Binding.visibleType Binding.contextId [ local, remote ]

  test "a fresh peer accepts a signed causal construction baseline above version one" $
    withSigningFixture true \key -> do
      prepare key
      let fresh = RoleInstance "def:#fresh-baseline"
      creation <- constructionFor fresh false "local" (stamp "local" 4 [ dot "local" 3 ])
      membership <- membershipFor fresh false "local" 38 (stamp "local" 5 [ dot "local" 4 ])
      receive [ membership, creation ]
      present <- isJust <$> lift (tryGetPerspectRol fresh)
      version <- lift $ getResourceVersion (unwrap fresh)
      pending <- lift $ pendingTransactionDatabaseName >>= \db -> documentsInRange db "" "\xFFFF"
      liftAff do
        assert "The baseline must construct the fresh role" present
        equal 38 version
        equal 0 (length pending.rows)

  test "a deleted peer accepts a newer version-zero construction without losing its counters" $
    withSigningFixture true \key -> do
      prepare key
      deletion <- roleDeletion (stamp "local" 2 [ dot "local" 1 ])
      receive [ deletion ]
      creation <- constructionFor Binding.visibleId false "local" (stamp "local" 3 [ dot "local" 2 ])
      membership <- membershipFor Binding.visibleId false "local" 38 (stamp "local" 4 [ dot "local" 3 ])
      receive [ creation, membership ]
      present <- isJust <$> lift (tryGetPerspectRol Binding.visibleId)
      version <- lift $ getResourceVersion (unwrap Binding.visibleId)
      liftAff do
        assert "A causally newer baseline must recreate the deleted role" present
        equal 38 version
      receive [ creation, membership ]
      afterReplay <- lift $ getResourceVersion (unwrap Binding.visibleId)
      liftAff $ equal 38 afterReplay

  test "a construction baseline does not bypass unrelated property gaps" $
    withSigningFixture true \key -> do
      prepare key
      creation <- constructionFor Binding.visibleId false "local" (stamp "local" 3 [ dot "local" 2 ])
      membership <- membershipFor Binding.visibleId false "local" 38 (stamp "local" 4 [ dot "local" 3 ])
      modification <- propertyChange "local" 5 (stamp "local" 5 [ dot "local" 4 ])
      incoming <- packet [ creation, membership, modification ]
      executeTransaction incoming
      pending <- lift $ pendingTransactionDatabaseName >>= \db -> documentsInRange db "" "\xFFFF"
      roleVersion <- lift $ getResourceVersion (unwrap Binding.visibleId)
      propertyVersion <- lift $ getResourceVersion History.propertyKey
      liftAff do
        equal 1 (length pending.rows)
        equal 1 roleVersion
        equal 1 propertyVersion

  test "membership alone cannot establish a high-version construction baseline" $
    withSigningFixture true \key -> do
      prepare key
      let fresh = RoleInstance "def:#membership-without-construction"
      membership <- membershipFor fresh false "local" 38 (stamp "local" 5 [ dot "local" 4 ])
      receive [ membership ]
      pending <- lift $ pendingTransactionDatabaseName >>= \db -> documentsInRange db "" "\xFFFF"
      role <- lift $ tryGetPerspectRol fresh
      version <- lift $ getResourceVersion (unwrap fresh)
      liftAff do
        equal 1 (length pending.rows)
        assert "An incomplete baseline must not create a role" $ isNothing role
        equal 0 version

  test "a missing context constructor cannot bootstrap an external role" $
    withSigningFixture true \key -> do
      prepare key
      let external = RoleInstance "def:#missing-baseline-context$External"
      creation <- constructionFor external true "local" (stamp "local" 4 [ dot "local" 3 ])
      membership <- membershipFor external true "local" 38 (stamp "local" 5 [ dot "local" 4 ])
      receive [ creation, membership ]
      pending <- lift $ pendingTransactionDatabaseName >>= \db -> documentsInRange db "" "\xFFFF"
      role <- lift $ tryGetPerspectRol external
      version <- lift $ getResourceVersion (unwrap external)
      liftAff do
        equal 1 (length pending.rows)
        assert "A context-less baseline must not create its external role" $ isNothing role
        equal 0 version

  test "received membership does not require an outgoing construction baseline" $
    withSigningFixture true \key -> do
      prepare key
      let fresh = RoleInstance "def:#received-membership"
      lift $ void $ cacheEntity fresh $ PerspectRol defaultRolRecord
        { id = fresh, context = Binding.contextId, pspType = Binding.visibleType }
      membership <- membershipFor fresh false "local" 1 (stamp "local" 2 [ dot "local" 1 ])
      receive [ membership ]
      PerspectContext context <- lift $ getPerspectContext Binding.contextId
      version <- lift $ getResourceVersion (unwrap fresh)
      Transaction transaction <- AA.get
      liftAff do
        assert "The received membership must still update the context" $
          case Object.lookup (unwrap Binding.visibleType) context.rolInContext of
            Just roles -> elem fresh roles
            Nothing -> false
        equal 1 version
        assert "Receiving must not echo construction deltas" $ null transaction.deltas

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

constructionFor :: RoleInstance -> Boolean -> String -> CausalStamp -> MonadPerspectivesTransaction SignedDelta
constructionFor role external author causal = signPayload author
  { deltaFormatVersion: 4
  , resourceKey: takeGuid $ unwrap role
  , resourceVersion: 0
  , deltaType: if external then "ConstructExternalRole" else "ConstructEmptyRole"
  , id: takeGuid $ if external then deconstructBuitenRol $ unwrap role else unwrap Binding.contextId
  , contextType: unwrap Binding.contextType
  , roleType: unwrap $ if external then Binding.externalType else Binding.visibleType
  , roleInstance: takeGuid $ unwrap role
  , authorizedRole: if external then Just $ unwrap Binding.userType else Nothing
  , authorizedRoleKind: if external then Just "ENR" else Nothing
  , subject: unwrap Binding.userType
  , subjectKind: "ENR"
  , causal
  }

membershipFor :: RoleInstance -> Boolean -> String -> Int -> CausalStamp -> MonadPerspectivesTransaction SignedDelta
membershipFor role external author version causal = signPayload author
  { deltaFormatVersion: 4
  , resourceKey: takeGuid $ unwrap role
  , resourceVersion: version
  , deltaType: if external then "AddExternalRole" else "AddRoleInstancesToContext"
  , contextInstance: takeGuid $ if external then deconstructBuitenRol $ unwrap role else unwrap Binding.contextId
  , contextType: unwrap Binding.contextType
  , roleType: unwrap $ if external then Binding.externalType else Binding.visibleType
  , roleInstance: takeGuid $ unwrap role
  , destinationContext: Nothing :: Maybe String
  , destinationContextType: Nothing :: Maybe String
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
      { roleVerbs = EM.singleton (ContextState $ StateIdentifier $ unwrap Binding.contextType) (Including [ Create, CreateAndFill, Remove ])
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
    , resourceVersion: 0
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
  lift $ storeDeltaFromSignedDelta $ Binding.membershipDelta Binding.visibleId
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
  void $ installPerspectiveQueries source

installPerspectiveQueries :: String -> MonadPerspectivesTransaction (Array InvertedQuery)
installPerspectiveQueries source = do
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
    Right (Tuple _ queries) -> do
      lift $ saveInvertedQueries queries
      pure $ map _.query queries
