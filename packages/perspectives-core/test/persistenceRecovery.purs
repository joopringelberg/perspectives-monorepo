-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.Persistence.Recovery where

import Prelude

import Control.Monad.AvarMonadAsk (gets, modify)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Decacheable (decache)
import Effect (Effect)
import Effect.Aff (try)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (fromEffectFnAff)
import Effect.Class (liftEffect)
import Foreign.Object (insert)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (ResourceToBeStored(..))
import Perspectives.Couchdb.Revision (changeRevision, rev)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Persistence.API (addDocument, addDocumentImpl, deleteDatabase, getDocumentWithConflictsImpl, withDatabase, withForce)
import Perspectives.Persistence.RunEffectAff (runEffectFnAff3)
import Perspectives.Persistence.Types (PouchdbDatabase, runMonadPouchdb)
import Perspectives.Persistent (entitiesDatabaseName, forceSaveRole, getPerspectRol, saveEntiteit_, saveMarkedResources)
import Perspectives.Representation.Class.Cacheable (cacheEntity, tryReadEntiteitFromCache)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..))
import Perspectives.RunPerspectives (runPerspectivesWithoutCouchdb)
import Simple.JSON (read, write)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

foreign import recoveringDatabase :: Effect { database :: PouchdbDatabase, allowWrites :: Effect Unit }

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Persistence recovery" do
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

  where
  publicDatabase = "https://publication.invalid/cw_publication_regression/"
  publicContextId = ContextInstance "pub:https://publication.invalid/cw_publication_regression/#publication@2.0"
  publicContext = PerspectContext defaultContextRecord { _id = "publication@2.0", id = publicContextId }
  roleId = RoleInstance "def:#manifest"
  role = PerspectRol defaultRolRecord { _id = "manifest", id = roleId }
