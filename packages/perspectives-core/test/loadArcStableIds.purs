-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.LoadArcStableIds where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (sort)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (bracket, error)
import Effect.Aff.Class (liftAff)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.InvertedQuery.Storable (StoredQueries, getPropertyQueries)
import Perspectives.InvertedQueryKey (RunTimeInvertedQueryKey(..))
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Persistence.API (Keys(..), getViewWithDocs)
import Perspectives.Persistent (invertedQueryDatabaseName)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runMonadPerspectivesTransaction')
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), StateUri(..), fromLocalModels, idUriForState, loadStableMapping)
import Perspectives.Sidecar.ToStable (toStable)
import Perspectives.TypePersistence.LoadArc (loadCompileAndStoreArcFile_)
import Simple.JSON (writeJSON)
import Test.PDRInstance (noBus, startPDRInstanceFromSnapshot, testPouchdbUser)
import Test.PDRInstance.Types (runInPDR)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

-- Run with: pnpm run test:stableIds
-- Restores the snapshot in memory; does not run model tests or write a snapshot.
main :: Effect Unit
main = runTest $ suite "Compile-and-store stable IDs" do
  test "Recompilation reuses installed and explicitly supplied mappings" do
    source <- readTextFile UTF8 "src/model/repositoryTools@1.0.arc"
    bracket
      (startPDRInstanceFromSnapshot (testPouchdbUser "alice") defaultRuntimeOptions Nothing noBus "test/pdr-snapshot/universe/aliceAfterReboot")
      (\pdr -> pdr.shutdown)
      \pdr -> runInPDR pdr do
        let
          modelId = ModelUri "model://joopringelberg.nl#ncr77pkxia"
          stateName = StateUri "model://joopringelberg.nl#RepositoryTools$ReadyToInstall"
        installedMapping <- loadStableMapping modelId fromLocalModels >>= case _ of
          Nothing -> throwError $ error "RepositoryTools stable-id mapping is required in the regression snapshot."
          Just mapping -> pure mapping
        stateId <- case idUriForState installedMapping stateName of
          Nothing -> throwError $ error "RepositoryTools ReadyToInstall state is required in the regression snapshot."
          Just id -> pure id
        for_ [ Nothing, Just installedMapping ] \explicitMapping -> do
          result <- runMonadPerspectivesTransaction' doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser)
            ( loadCompileAndStoreArcFile_
                (ModelUri "model://joopringelberg.nl#ncr77pkxia@1.0")
                source
                true
                "ncr77pkxia"
                "model://joopringelberg.nl#RepositoryTools@1.0"
                Nothing
                explicitMapping
            )
          case result of
            Left errs -> throwError $ error ("RepositoryTools recompilation failed: " <> show errs)
            Right (Tuple (DomeinFile dfr) (Tuple _ mapping)) -> liftAff do
              assert "ReadyToInstall retains its installed stable ID" (idUriForState mapping stateName == Just stateId)
              assert "The existing active state still has a definition" (isJust $ Object.lookup stateId dfr.states)

  test "Recompilation replaces queries that reference a state moved under a new parent" do
    source <- readTextFile UTF8 "src/model/AMQPtestModel@1.0.arc"
    bracket
      (startPDRInstanceFromSnapshot (testPouchdbUser "alice") defaultRuntimeOptions Nothing noBus "test/pdr-snapshot/universe/alice")
      (\pdr -> pdr.shutdown)
      \pdr -> runInPDR pdr do
        let
          modelId = ModelUri "model://joopringelberg.nl#xyyehk9bpc"
          stateName = StateUri "model://joopringelberg.nl#AMQPtestModel$Test_Leader_Terminates_Contract$External$TestSucceeded"
        installedMapping <- loadStableMapping modelId fromLocalModels >>= case _ of
          Nothing -> throwError $ error "AMQPtestModel stable-id mapping is required in the regression snapshot."
          Just mapping -> pure mapping
        oldStateId <- case idUriForState installedMapping stateName of
          Nothing -> throwError $ error "The old TestSucceeded state is required in the regression snapshot."
          Just id -> pure id
        role <- toStable (EnumeratedRoleType "model://joopringelberg.nl#AMQPtestModel$Test_Leader_Terminates_Contract$External")
        property <- toStable (EnumeratedPropertyType "model://joopringelberg.nl#AMQPtestModel$Test_Leader_Terminates_Contract$External$ContractWasPresent")
        let key = RTPropertyKey { role, property }
        cachedQueries <- getPropertyQueries pure [ key ]
        liftAff $ assert "The snapshot queries reference the old state" (contains (Pattern oldStateId) (writeJSON cachedQueries))
        result <- runMonadPerspectivesTransaction' doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser)
          ( loadCompileAndStoreArcFile_
              (ModelUri "model://joopringelberg.nl#xyyehk9bpc@1.0")
              source
              true
              "xyyehk9bpc"
              "model://joopringelberg.nl#AMQPtestModel@1.0"
              Nothing
              Nothing
          )
        case result of
          Left errs -> throwError $ error ("AMQPtestModel recompilation failed: " <> show errs)
          Right (Tuple (DomeinFile dfr) (Tuple queries _)) -> do
            db <- invertedQueryDatabaseName
            storedQueries :: StoredQueries <- getViewWithDocs db "defaultViews/modelView" (Key $ unwrap modelId)
            refreshedQueries <- getPropertyQueries pure [ key ]
            liftAff do
              assert "The moved state no longer has its old definition" (not $ isJust $ Object.lookup oldStateId dfr.states)
              assert "Only the new compilation's queries remain" (sort (writeJSON <$> storedQueries) == sort (writeJSON <$> queries))
              assert "The new queries do not reference the old state" (not $ contains (Pattern oldStateId) (writeJSON storedQueries))
              assert "The query cache no longer returns the old state" (not $ contains (Pattern oldStateId) (writeJSON refreshedQueries))
