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

-- | Regression test for model compilation.
-- |
-- | Fetches all models from the perspectives.domains repository database,
-- | starts a full PDR instance (in-memory PouchDB, setupUser), then compiles
-- | each model in topological dependency order using the production compilation
-- | path (`loadAndCompileArcFileWithSidecar_`).  Any parse or compile errors
-- | are reported clearly and the test fails if any are present.
-- |
-- | Each successfully compiled model is saved to the PDR's local in-memory
-- | PouchDB so that models compiled later can resolve cross-model type
-- | references from models compiled earlier.
-- |
-- | Uses `testOnly` because it requires network access to
-- |   https://perspectives.domains/models_perspectives_domains
-- | Run manually with:
-- |   pnpm run test:layer3

module Test.ModelCompilationRegression where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, foldM, length, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff (error)
import Effect.Class.Console (log)
import Main.RecompileBasicModels (UninterpretedDomeinFile(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectivesTransaction)
import Perspectives.DomeinCache (storeDomeinFileInCouchdb)
import Perspectives.ExecuteInTopologicalOrder (sortTopologicallyEither)
import Perspectives.Identifiers (domeinFileVersion, modelUri2LocalName)
import Perspectives.InvertedQuery.Storable (saveInvertedQueries)
import Perspectives.Logging (traceTest)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Persistence.API (documentsInDatabase, includeDocs)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runMonadPerspectivesTransaction')
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), fromLocalModels, loadStableMapping)
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFileWithSidecar_)
import Simple.JSON (read)
import Test.PDRInstance (noBus, runInPDR, testPouchdbUser, withPDR)
import Test.Unit (TestSuite, suite, testOnly)
import Test.Unit.Assert (assert)

-- | The CouchDB database that contains all compiled model documents
-- | for the perspectives.domains namespace.
modelsDb :: String
modelsDb = "https://perspectives.domains/models_perspectives_domains"

theSuite :: TestSuite
theSuite = suite "Perspectives.ModelCompilationRegression" do

  testOnly "All models from perspectives.domains compile without errors" do
    let user = testPouchdbUser "regressiontest"
    withPDR user defaultRuntimeOptions Nothing noBus \pdr -> do
      -- Fetch every document from the models database and parse as
      -- UninterpretedDomeinFile.  Design documents and other non-model
      -- documents are silently skipped.
      sorted <- runInPDR pdr do
        { rows: allDocs } <- documentsInDatabase modelsDb includeDocs
        let
          models :: Array UninterpretedDomeinFile
          models = catMaybes
            ( map
                ( \{ doc } -> case read <$> doc of
                    Just (Right (df :: UninterpretedDomeinFile)) -> Just df
                    _ -> Nothing
                )
                allDocs
            )
        -- Topologically sort the models so that each model is compiled only
        -- after all of its declared dependencies have been compiled.
        case sortTopologicallyEither
               (\(UninterpretedDomeinFile { id }) -> unwrap id)
               (\(UninterpretedDomeinFile { referredModels }) -> referredModels)
               models of
          Left sortErrors -> throwError (error ("Topological sort failed: " <> show sortErrors))
          Right s -> pure s
      -- Compile each model inside a single PDR transaction so that the
      -- MonadPerspectives state (cache + in-memory PouchDB) is shared
      -- across all compilations.
      errors <- runInPDR pdr $
        runMonadPerspectivesTransaction' doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser) $
          foldM (\errs model -> (\newErrs -> errs <> newErrs) <$> compileAndStoreModel model) [] sorted
      -- Print every error and fail if any were found.
      for_ errors log
      assert
        ( "Model compilation produced "
            <> show (length errors)
            <> " error(s) — see log above for details."
        )
        (null errors)

-- | Compile a single model through the production compilation path
-- | (`loadAndCompileArcFileWithSidecar_`), then persist the result to the
-- | PDR's local in-memory PouchDB so that later compilations can resolve
-- | cross-model type references.
-- |
-- | Returns an empty array on success, or one or more human-readable error
-- | strings on failure.
compileAndStoreModel :: UninterpretedDomeinFile -> MonadPerspectivesTransaction (Array String)
compileAndStoreModel (UninterpretedDomeinFile { _id, id, namespace, arc }) = do
  traceTest ("Compiling model " <> namespace)
  let mversion = domeinFileVersion _id
  -- Load the sidecar mapping from the local models database if available.
  -- For models already installed by setupUser this preserves stable CUIDs;
  -- for new models (not yet installed locally) this is Nothing and a fresh
  -- mapping is created.
  mlocalMapping <- lift $ loadStableMapping (ModelUri $ unwrap id) fromLocalModels
  result <- loadAndCompileArcFileWithSidecar_
    (ModelUri $ unwrap id)
    arc
    true  -- save in cache after compilation
    mlocalMapping
    (unsafePartial modelUri2LocalName (unwrap id))
    (namespace <> fromMaybe "" ((<>) "@" <$> mversion))
    mversion
  case result of
    Left errs -> pure (map (\e -> namespace <> ": " <> show e) errs)
    Right (Tuple df (Tuple invertedQueries _)) -> do
      -- Persist the compiled model to the local in-memory PouchDB so that
      -- subsequent models can find it via getinstalledModelCuids.
      lift $ void $ storeDomeinFileInCouchdb df
      lift $ saveInvertedQueries invertedQueries
      pure []
