-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.ModelPatches (main, theSuite) where

import Prelude

import Control.Monad.Reader (ask)
import Data.Either (isLeft)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (attempt)
import Effect.Aff.Class (liftAff)
import Foreign.Object (singleton)
import Main.RecompileBasicModels (UninterpretedDomeinFile(..))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Perspectives.Assignment.StateCache (CompiledContextState, CompiledRoleState, cacheCompiledContextState, cacheCompiledRoleState, clearModelStates, retrieveCompiledContextState, retrieveCompiledRoleState)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DataUpgrade.PatchModels (patchModels)
import Perspectives.DataUpgrade.RecompileLocalModels (recompileLocalModel)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Persistence.API (addDocument_, deleteDatabase, documentsInDatabase, getDocument_, includeDocs)
import Perspectives.PerspectivesState (modelsDatabaseName, setModelUri)
import Perspectives.Persistent (getDomeinFile, invertedQueryDatabaseName)
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.TypeIdentifiers (StateIdentifier(..))
import Perspectives.RunPerspectives (runPerspectivesWithoutCouchdb)
import Perspectives.SetupUser (setupInvertedQueryDatabase)
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), fromLocalModels, loadStableMapping)
import Test.Layer3Scaffold (applyModelPatches)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Synchronisation model patches" do
  test "recompiles replacement files and preserves stable ids on subsequent patches" do
    runPerspectivesWithoutCouchdb "model-patch-success" do
      setupInvertedQueryDatabase
      db <- modelsDatabaseName
      storeOriginal db
      setModelUri (ModelUri readableUri) (ModelUri stableUri)
      stateAVar <- ask
      let
        pdr = { stateAVar, name: "patch-test", shutdown: pure unit }
        patches = [ { modelUriReadable: readableUri, sourcePath: "test/patches/ScaffoldPatchTest.arc" } ]
      liftAff $ applyModelPatches pdr patches
      DomeinFile compiled <- getDomeinFile (ModelUri stableUri)
      liftAff $ equal (ModelUri readableUri) compiled.namespace
      firstMapping <- loadStableMapping (ModelUri stableUri) fromLocalModels
      liftAff $ assert "Recompilation must persist the stable-id mapping" (isJust firstMapping)
      let cachedStateIds = StateIdentifier <$> [ stableUri <> "$ContextState", stableUri <> "$RoleState" ]
      for_ cachedStateIds \stateId -> do
        let
          contextState :: CompiledContextState
          contextState =
            { query: \_ -> pure (Value "true")
            , objectGetter: Nothing
            , automaticOnEntry: Map.empty
            , automaticOnExit: Map.empty
            , notifyOnEntry: Map.empty
            , notifyOnExit: Map.empty
            , perspectivesOnEntry: Map.empty
            }

          roleState :: CompiledRoleState
          roleState =
            { query: \_ -> pure (Value "true")
            , objectGetter: Nothing
            , automaticOnEntry: Map.empty
            , automaticOnExit: Map.empty
            , notifyOnEntry: Map.empty
            , notifyOnExit: Map.empty
            , perspectivesOnEntry: Map.empty
            }
        void $ pure $ cacheCompiledContextState stateId contextState
        void $ pure $ cacheCompiledRoleState stateId roleState
        liftAff $ assert ("Seeded compiled context state " <> unwrap stateId)
          (isJust $ retrieveCompiledContextState stateId)
        liftAff $ assert ("Seeded compiled role state " <> unwrap stateId)
          (isJust $ retrieveCompiledRoleState stateId)
      liftAff $ applyModelPatches pdr patches
      for_ cachedStateIds \stateId -> do
        liftAff $ assert ("Recompilation must invalidate compiled context state " <> unwrap stateId)
          (isNothing $ retrieveCompiledContextState stateId)
        liftAff $ assert ("Recompilation must invalidate compiled role state " <> unwrap stateId)
          (isNothing $ retrieveCompiledRoleState stateId)
      void $ pure $ clearModelStates (ModelUri stableUri)
      secondMapping <- loadStableMapping (ModelUri stableUri) fromLocalModels
      liftAff do
        equal (_.contextCuids <$> firstMapping) (_.contextCuids <$> secondMapping)
        equal (_.roleCuids <$> firstMapping) (_.roleCuids <$> secondMapping)
        equal (_.propertyCuids <$> firstMapping) (_.propertyCuids <$> secondMapping)
        equal (_.stateCuids <$> firstMapping) (_.stateCuids <$> secondMapping)
      deleteDatabase db
      invertedQueryDatabaseName >>= deleteDatabase

  test "applies patches in order and stops at the first compilation failure" do
    runPerspectivesWithoutCouchdb "model-patch-order" do
      setupInvertedQueryDatabase
      db <- modelsDatabaseName
      storeOriginal db
      setModelUri (ModelUri readableUri) (ModelUri stableUri)
      stateAVar <- ask
      let
        validPatch = { modelUriReadable: readableUri, sourcePath: "test/patches/ScaffoldPatchTest.arc" }
        invalidPatch = { modelUriReadable: readableUri, sourcePath: "package.json" }
      outcome <- liftAff $ attempt $ applyModelPatches
        { stateAVar, name: "patch-test", shutdown: pure unit }
        [ validPatch, invalidPatch, validPatch ]
      liftAff $ assert "Compilation errors must abort subsequent patches" (isLeft outcome)
      UninterpretedDomeinFile stored <- getDocument_ db documentId
      invalidSource <- liftAff $ readTextFile UTF8 "package.json"
      liftAff $ equal invalidSource stored.arc
      deleteDatabase db
      invertedQueryDatabaseName >>= deleteDatabase

  test "updates the original stable document, not a readable-name document" do
    runPerspectivesWithoutCouchdb "model-patch-document" do
      db <- modelsDatabaseName
      storeOriginal db
      patchModels (singleton readableUri "replacement source")
      patched :: UninterpretedDomeinFile <- getDocument_ db documentId
      let UninterpretedDomeinFile record = patched
      liftAff $ equal "replacement source" record.arc
      liftAff $ equal readableUri record.namespace
      liftAff $ equal (ModelUri stableUri) record.id
      { rows } <- documentsInDatabase db includeDocs
      liftAff $ equal [ documentId ] (map _.id rows)
      deleteDatabase db

  test "an empty patch list leaves installed models unchanged" do
    runPerspectivesWithoutCouchdb "model-patch-empty" do
      db <- modelsDatabaseName
      storeOriginal db
      stateAVar <- ask
      liftAff $ applyModelPatches { stateAVar, name: "patch-test", shutdown: pure unit } []
      UninterpretedDomeinFile unchanged <- getDocument_ db documentId
      liftAff $ equal "original source" unchanged.arc
      deleteDatabase db

  test "missing source files abort patch setup" do
    runPerspectivesWithoutCouchdb "model-patch-missing-file" do
      stateAVar <- ask
      outcome <- liftAff $ attempt $ applyModelPatches
        { stateAVar, name: "patch-test", shutdown: pure unit }
        [ { modelUriReadable: readableUri, sourcePath: "test/patches/nonexistent-model-patch.arc" } ]
      liftAff $ assert "File errors must propagate" (isLeft outcome)

  test "missing installed models abort patch setup" do
    runPerspectivesWithoutCouchdb "model-patch-missing-model" do
      stateAVar <- ask
      outcome <- liftAff $ attempt $ applyModelPatches
        { stateAVar, name: "patch-test", shutdown: pure unit }
        [ { modelUriReadable: readableUri, sourcePath: "package.json" } ]
      liftAff $ assert "A missing model must fail setup" (isLeft outcome)
      db <- modelsDatabaseName
      deleteDatabase db

  test "invalid replacement source reports recompilation failure" do
    runPerspectivesWithoutCouchdb "model-patch-invalid-source" do
      db <- modelsDatabaseName
      storeOriginal db
      setModelUri (ModelUri readableUri) (ModelUri stableUri)
      success <- recompileLocalModel (ModelUri readableUri)
      liftAff $ assert "Invalid ARC must not be reported as successfully compiled" (not success)
      deleteDatabase db

readableUri :: String
readableUri = "model://patch.test#PatchTest"

stableUri :: String
stableUri = "model://patch.test#stablepatch"

documentId :: String
documentId = "patch_test-stablepatch.json"

original :: UninterpretedDomeinFile
original = UninterpretedDomeinFile
  { _id: documentId
  , _rev: ""
  , id: ModelUri stableUri
  , namespace: readableUri
  , referredModels: []
  , arc: "original source"
  , _attachments: Nothing
  }

storeOriginal :: String -> MonadPerspectives Unit
storeOriginal db = case original of
  UninterpretedDomeinFile record -> void $ addDocument_ db (record { _rev = Nothing :: Maybe String }) documentId
