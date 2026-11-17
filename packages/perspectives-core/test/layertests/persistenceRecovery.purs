-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.Persistence.Recovery where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Parallel (parallel, sequential)
import Data.Array (elem, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Decacheable (decache)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, try)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (fromEffectFnAff)
import Effect.Class (liftEffect)
import Foreign.Object (insert)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (ResourceToBeStored(..))
import Perspectives.Couchdb.Revision (changeRevision, rev)
import Perspectives.Data.EncodableMap as EM
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Persistence.API (addDocument, addDocument_, addDocumentImpl, deleteDatabase, getDocumentWithConflictsImpl, withDatabase, withForce)
import Perspectives.Persistence.PendingTransactionStore (PendingTransactionRecord(..), getAllPendingTransactions, getPendingTransactionsForResource, pendingTransactionDatabaseName, removePendingTransaction, storePendingTransaction)
import Perspectives.Persistence.RunEffectAff (runEffectFnAff3)
import Perspectives.Persistence.Types (PouchdbDatabase, runMonadPouchdb)
import Perspectives.Persistent (entitiesDatabaseName, forceSaveRole, getPerspectRol, removeEntiteit, saveEntiteit_, saveMarkedResources)
import Perspectives.Representation.Class.Cacheable (cacheEntity, tryReadEntiteitFromCache)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..), StateIdentifier(..))
import Perspectives.RunMonadPerspectivesTransaction (computeStateEvaluations)
import Perspectives.RunPerspectives (runPerspectivesWithoutCouchdb)
import Perspectives.ScheduledAssignment (StateEvaluation(..))
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.InvertedQueryResult (InvertedQueryResult(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Simple.JSON (read, write)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

foreign import recoveringDatabase :: Effect { database :: PouchdbDatabase, allowWrites :: Effect Unit }

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Persistence recovery" do
  test "retrieves, updates and removes pending transactions with their current revisions" do
    runMonadPouchdb "test" "test" (PerspectivesUser "test") "pending-transactions-regression" Nothing do
      let
        transaction = TransactionForPeer
          { author: PerspectivesUser "def:#author"
          , perspectivesSystem: ContextInstance "def:#system"
          , timeStamp: SerializableDateTime bottom
          , deltas: []
          , publicKeys: EM.empty
          }
        gap = { author: PerspectivesUser "def:#author", resourceKey: "def:#role", expectedVersion: 1 }
      database <- pendingTransactionDatabaseName
      empty <- getAllPendingTransactions
      liftAff $ equal 0 (length empty)
      void $ addDocument_ database { _id: "unrelated", value: "not a transaction" } "unrelated"
      storePendingTransaction transaction [ gap ]
      first <- getAllPendingTransactions
      case first of
        [ PendingTransactionRecord before ] -> do
          liftAff do
            assert "allDocs must include the document revision" (isJust before._rev)
            equal transaction before.transaction
            equal [ gap ] before.missingDeltas
          storePendingTransaction transaction [ gap { expectedVersion = 2 } ]
          updated <- getAllPendingTransactions
          case updated of
            [ PendingTransactionRecord after ] -> do
              liftAff do
                equal before._id after._id
                assert "Reblocking must update the existing document" (after._rev /= before._rev)
                equal [ gap { expectedVersion = 2 } ] after.missingDeltas
              removePendingTransaction after._id after._rev
              remaining <- getAllPendingTransactions
              removed <- getPendingTransactionsForResource after._id
              liftAff do
                equal 0 (length remaining)
                assert "Removed pending documents must not be returned" (isNothing removed)
            _ -> liftAff $ assert "Reblocking must retain exactly one pending record" false
        _ -> liftAff $ assert "Only pending transaction documents should be returned" false
      deleteDatabase database

  test "updates stale revisions without creating conflict branches" do
    runMonadPouchdb "test" "test" (PerspectivesUser "test") "test" Nothing do
      let database = "persistence_conflict_regression"
      first <- addDocument database role "manifest"
      second <- addDocument database (changeRevision first role) "manifest"
      third <- addDocument database (changeRevision first role) "manifest"
      liftAff $ assert "A stale write should advance the winning revision" (third /= second)
      withDatabase database \db -> do
        raw <- liftAff $ fromEffectFnAff $ runEffectFnAff3 getDocumentWithConflictsImpl db "manifest" true
        case read raw of
          Left e -> liftAff $ assert (show e) false
          Right (doc :: { _conflicts :: Maybe (Array String) }) ->
            liftAff $ equal [] (fromMaybe [] doc._conflicts)
      deleteDatabase database

  test "removes existing losing branches while updating the winning revision" do
    runMonadPouchdb "test" "test" (PerspectivesUser "test") "test" Nothing do
      let database = "persistence_existing_conflict_regression"
      first <- addDocument database role "manifest"
      void $ addDocument database (changeRevision first role) "manifest"
      withDatabase database \db -> do
        void $ liftAff $ fromEffectFnAff $ runEffectFnAff3 addDocumentImpl db (write $ changeRevision first role) withForce
        before <- liftAff $ fromEffectFnAff $ runEffectFnAff3 getDocumentWithConflictsImpl db "manifest" true
        case read before of
          Left e -> liftAff $ assert (show e) false
          Right (doc :: { _conflicts :: Maybe (Array String) }) ->
            liftAff $ assert "The fixture should contain a losing branch" (fromMaybe [] doc._conflicts /= [])
      void $ addDocument database (changeRevision first role) "manifest"
      withDatabase database \db -> do
        after <- liftAff $ fromEffectFnAff $ runEffectFnAff3 getDocumentWithConflictsImpl db "manifest" true
        case read after of
          Left e -> liftAff $ assert (show e) false
          Right (doc :: { _conflicts :: Maybe (Array String) }) ->
            liftAff $ equal [] (fromMaybe [] doc._conflicts)
      deleteDatabase database

  test "clears a stale revision when the document has not been persisted" do
    runMonadPouchdb "test" "test" (PerspectivesUser "test") "test" Nothing do
      let database = "persistence_missing_revision_regression"
      revision <- addDocument database (changeRevision (Just "9-stale") role) "manifest"
      liftAff $ assert "The document should be stored" (revision /= Nothing)
      deleteDatabase database

  test "retains failed saves and persists them on the next successful pass" do
    runPerspectivesWithoutCouchdb "persistence-recovery" do
      stub <- liftEffect recoveringDatabase
      database <- entitiesDatabaseName
      modify \s -> s { databases = insert database stub.database s.databases }
      void $ saveEntiteit_ roleId role
      saveMarkedResources
      pending <- gets _.entitiesToBeStored
      liftAff $ assert "A failed save must remain queued" (elem (Rle roleId) pending)
      forced <- try $ forceSaveRole roleId
      liftAff $ assert "A forced save must propagate its failure" case forced of
        Left _ -> true
        Right _ -> false
      liftEffect stub.allowWrites
      saveMarkedResources
      pendingAfterRetry <- gets _.entitiesToBeStored
      liftAff $ assert "A successful retry should clear the queue" (not $ elem (Rle roleId) pendingAfterRetry)
      saved <- getPerspectRol roleId
      liftAff $ equal (Just "1-saved") (rev saved)

  test "evicts previously persisted public contexts when their remote document is missing" do
    runPerspectivesWithoutCouchdb "publication-cache-recovery" do
      withDatabase "publication_cache_regression" \db ->
        modify \s -> s { databases = insert publicDatabase db s.databases }
      void $ cacheEntity publicContextId (changeRevision (Just "1-before-cleanup") publicContext)
      decache publicContextId
      cached <- tryReadEntiteitFromCache publicContextId
      liftAff $ assert "An old public context must not suppress its recreation" (isNothing cached)
      deleteDatabase "publication_cache_regression"

  test "preserves pending public contexts even when their remote document is missing" do
    runPerspectivesWithoutCouchdb "publication-pending-recovery" do
      withDatabase "publication_pending_regression" \db ->
        modify \s -> s { databases = insert publicDatabase db s.databases }
      void $ saveEntiteit_ publicContextId (changeRevision (Just "1-before-cleanup") publicContext)
      decache publicContextId
      cached <- tryReadEntiteitFromCache publicContextId
      pending <- gets _.entitiesToBeStored
      liftAff $ assert "Pending changes must remain cached" (isJust cached)
      liftAff $ assert "Pending changes must remain queued" (elem (Ctxt publicContextId) pending)
      deleteDatabase "publication_pending_regression"

  test "preserves new unpersisted public contexts awaiting their creation deltas" do
    runPerspectivesWithoutCouchdb "publication-new-recovery" do
      withDatabase "publication_new_regression" \db ->
        modify \s -> s { databases = insert publicDatabase db s.databases }
      void $ cacheEntity publicContextId publicContext
      decache publicContextId
      cached <- tryReadEntiteitFromCache publicContextId
      liftAff $ assert "A new unpersisted context must remain cached" (isJust cached)
      deleteDatabase "publication_new_regression"

  test "state evaluation excludes physically deleted subjects without invoking repair" do
    result <- sequential $ parallel (delay (Milliseconds 2000.0) *> pure Nothing) <|> parallel
      ( Just <$> runPerspectivesWithoutCouchdb "deleted-state-subjects" do
          let
            stateId = StateIdentifier "model://test#State"
            survivingContextId = ContextInstance "def:#surviving-context"
            deletedContextId = ContextInstance "def:#deleted-context"
            survivingRoleId = RoleInstance "def:#surviving-role"
            deletedRoleId = RoleInstance "def:#deleted-role"
            contextRecord = defaultContextRecord
              { states = [ stateId ]
              , preferredUserRoleType = Just (ENR $ EnumeratedRoleType "model://test#User")
              }
            roleRecord = defaultRolRecord { states = [ stateId ], context = survivingContextId }
          void $ saveEntiteit_ survivingContextId (PerspectContext contextRecord { _id = "surviving-context", id = survivingContextId })
          void $ saveEntiteit_ deletedContextId (PerspectContext contextRecord { _id = "deleted-context", id = deletedContextId })
          void $ saveEntiteit_ survivingRoleId (PerspectRol roleRecord { _id = "surviving-role", id = survivingRoleId })
          void $ saveEntiteit_ deletedRoleId (PerspectRol roleRecord { _id = "deleted-role", id = deletedRoleId })
          saveMarkedResources
          void $ removeEntiteit deletedRoleId
          void $ removeEntiteit deletedContextId
          contextEvaluations <- computeStateEvaluations (ContextStateQuery stateId [ deletedContextId, survivingContextId ])
          roleEvaluations <- computeStateEvaluations (RoleStateQuery stateId [ deletedRoleId, survivingRoleId ])
          liftAff do
            equal [ ContextStateEvaluation stateId survivingContextId ] contextEvaluations
            equal [ RoleStateEvaluation stateId survivingRoleId ] roleEvaluations
          database <- entitiesDatabaseName
          deleteDatabase database
      )
    assert "Deleted subjects must not block waiting for the absent integrity fixer" (isJust result)

  where
  publicDatabase = "https://publication.invalid/cw_publication_regression/"
  publicContextId = ContextInstance "pub:https://publication.invalid/cw_publication_regression/#publication@2.0"
  publicContext = PerspectContext defaultContextRecord { _id = "publication@2.0", id = publicContextId }
  roleId = RoleInstance "def:#manifest"
  role = PerspectRol defaultRolRecord { _id = "manifest", id = roleId }
