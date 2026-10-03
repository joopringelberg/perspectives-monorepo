-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.LoadArcStableIds where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (bracket, error)
import Effect.Aff.Class (liftAff)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ModelDependencies (sysUser)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runMonadPerspectivesTransaction')
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), StateUri(..), fromLocalModels, idUriForState, loadStableMapping)
import Perspectives.TypePersistence.LoadArc (loadCompileAndStoreArcFile_)
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
