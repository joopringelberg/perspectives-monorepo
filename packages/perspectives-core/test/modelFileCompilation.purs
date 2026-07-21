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

-- | Test for compiling one or more ARC model files that reside on disk.
-- |
-- | Derives from Test.ModelCompilationRegression but reads ARC source from
-- | the local file system instead of fetching compiled DomeinFiles from a
-- | network CouchDB database.  The test is specifically intended to be used
-- | by Copilot when the parser/compiler system is changed.
-- |
-- | Multiple file paths can be listed in `modelFilePaths`.  Each file is
-- | compiled independently inside a single PDR instance; no topological
-- | sorting is required because all files are expected to be unrelated
-- | (e.g. different versions of the same model, or several candidate
-- | rewrites produced by Copilot).
-- |
-- | Any parse or compile errors are reported clearly and the test fails if
-- | any are present.
-- |
-- | Run with:
-- |   pnpm run test:modelfiles

module Test.ModelFileCompilation where

import Prelude

import Data.Array (null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Perspectives.Parsing.Messages (MultiplePerspectivesErrors)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.TypePersistence.LoadArc.FS (loadAndCompileArcFile_)
import Test.PDRInstance (noBus, runInPDR, testPouchdbUser, withPDRCached)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

type CompilationResult = { filePath :: String, errors :: MultiplePerspectivesErrors }

main :: Effect Unit
main = launchAff_ do
  let user = testPouchdbUser "modelfiletest"
  testResults <- withPDRCached user defaultRuntimeOptions Nothing noBus snapshotDirectory \pdr -> runInPDR pdr do
    for modelFilePaths \filePath -> do
      r <- loadAndCompileArcFile_ filePath
      case r of
        Left errors -> pure { filePath, errors }
        Right _ -> pure { filePath, errors: [] }
  liftEffect $ runTest do
    suite "Model file compilation tests" do
      for_ testResults \{ filePath, errors } ->
        if null errors then
          test (filePath <> " compiled correctly") do
            assert ("The model file '" <> filePath <> "' should compile without errors") true
        else
          test (filePath <> " failed to compile") do
            assert ("The model file '" <> filePath <> "' should compile without errors, but got: " <> show errors) false

-- | The ARC model files to compile.
-- | Each entry is an absolute path or a path relative to the process working
-- | directory (i.e. the package root when run via `pnpm run test:modelfiles`).
-- |
-- | Copilot: populate this array with the files you want to test, e.g.:
-- |   [ "test/mymodel_v1.arc"
-- |   , "test/mymodel_v2.arc"
-- |   ]
modelFilePaths :: Array String
modelFilePaths =
  []

-- | Directory where the PDR snapshot is cached between test runs.
-- | Delete this directory to force a full PDR rebuild on the next run.
snapshotDirectory :: String
snapshotDirectory = "test/pdr-snapshot/modelfiletest"
