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

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, difference, elemIndex, filter, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (error, launchAff_)
import Effect.Class (liftEffect)
import Main.RecompileBasicModels (UninterpretedDomeinFile(..), getVersionedDomeinFileName)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectivesTransaction)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ExecuteInTopologicalOrder (executeInTopologicalOrder) as TOP
import Perspectives.Identifiers (domeinFileVersion, modelUri2LocalName)
import Perspectives.Logging (debugUpgrade, errorUpgrade)
import Perspectives.ModelDependencies (modelManifest, sysUser)
import Perspectives.Parsing.Messages (MultiplePerspectivesErrors, PerspectivesError(..))
import Perspectives.Persistence.API (Keys(..), documentsInDatabase, includeDocs)
import Perspectives.Persistent.FromViews (getSafeViewOnDatabase)
import Perspectives.PerspectivesState (defaultRuntimeOptions, setModelUri)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runMonadPerspectivesTransaction')
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), fromRepository, loadStableMapping)
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFileWithSidecar_)
import Simple.JSON (read)
import Test.PDRInstance (noBus, runInPDR, testPouchdbUser, withPDR)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  let user = testPouchdbUser "regressiontest"
  testResults <- withPDR user defaultRuntimeOptions Nothing noBus \pdr -> runInPDR pdr do
    manifests :: Array RoleInstance <- getSafeViewOnDatabase manifestsDb "defaultViews/roleView" (Key modelManifest)
    runMonadPerspectivesTransaction' doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser) do
      versionsToCompile <- traverse getVersionedDomeinFileName manifests >>= pure <<< catMaybes
      { rows: allModels } <- lift $ documentsInDatabase modelsDb includeDocs
      uninterpretedDomeinFiles <- for (filter (isJust <<< (flip elemIndex versionsToCompile) <<< _.id) allModels) \({ id, doc }) -> case read <$> doc of
        Just (Left errs) -> (lift $ errorUpgrade ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs)) *> pure Nothing
        Nothing -> (lift $ errorUpgrade ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
        Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
      results :: Either MultiplePerspectivesErrors (Array CompilationResult) <- runExceptT $ executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) recompileModelAtUrl
      case results of
        Left errs -> do
          (lift $ errorUpgrade ("Model compilation regression test failed with errors: " <> show errs))
          throwError $ error "Model compilation regression test failed."
        Right compilationResults -> pure compilationResults
  liftEffect $ runTest do
    suite "Model compilation tests" do
      for_ testResults \{ namespace, version, errors } ->
        if null errors then
          test (namespace <> "@" <> version <> " was compiled correctly") do
            assert ("The model with namespace '" <> namespace <> "@" <> version <> "' should compile without errors") (null errors)
        else
          test ("The model with namespace '" <> namespace <> "@" <> version <> "' failed with error") do
            assert ("The model with namespace '" <> namespace <> "@" <> version <> "' should compile without errors, but got error: " <> show errors) false

-- | The CouchDB database that contains all compiled model documents
-- | for the perspectives.domains namespace.
modelsDb :: String
modelsDb = "https://perspectives.domains/models_perspectives_domains"

manifestsDb :: String
manifestsDb = "https://perspectives.domains/cw_perspectives_domains"

type CompilationResult = { namespace :: String, version :: String, errors :: MultiplePerspectivesErrors }

recompileModelAtUrl :: UninterpretedDomeinFile -> ExceptT MultiplePerspectivesErrors MonadPerspectivesTransaction CompilationResult
recompileModelAtUrl model@(UninterpretedDomeinFile { id, namespace, _id, _rev, arc, _attachments }) =
  do
    lift $ lift $ debugUpgrade ("Recompiling " <> namespace)
    case domeinFileVersion $ _id of
      Nothing -> pure $ { namespace, version: "", errors: [ Custom ("recompileModelAtUrl: no version found in model id '" <> unwrap id <> "'.") ] }
      Just version -> do
        -- Load sidecar from repository DB and compile with it. We need the version to take the right sidecar.
        mRepoMapping <- lift $ lift $ loadStableMapping (ModelUri $ unwrap id <> "@" <> version) fromRepository
        -- We have to provide the CUID that has been chosen for the model. This is stored in ModelManifest$External$ModelCuid.
        r <- lift $ loadAndCompileArcFileWithSidecar_ (ModelUri $ unwrap id) arc false mRepoMapping (unsafePartial modelUri2LocalName (unwrap id)) (namespace <> "@" <> version) (Just version)
        case r of
          Left m -> pure $ { namespace, version, errors: m }
          Right (Tuple (DomeinFile df) _) -> do
            -- This will not prevent all forward references to models, but it may help.
            lift $ lift $ setModelUri df.namespace df.id
            pure $ { namespace, version, errors: [] }

--------------------------------------------------------------------------------------------
-- TOPOLOGICAL SORTING
-- Even though we can sort UninterpretedDomeinFile-s using an instance of Ord where we judge
-- two models to be equal if they don't use each other, the resulting order does not respect
-- the basic requirement that a model should be to the right of its used models.
-- The models form a directed graph.
--------------------------------------------------------------------------------------------
type ToSort = Array UninterpretedDomeinFile
type SortedLabels = Array String
type Skipped = Array UninterpretedDomeinFile

executeInTopologicalOrder
  :: forall m
   . MonadThrow MultiplePerspectivesErrors m
  => ToSort
  -> (UninterpretedDomeinFile -> m CompilationResult)
  -> m (Array CompilationResult)
executeInTopologicalOrder toSort action =
  TOP.executeInTopologicalOrder
    (\(UninterpretedDomeinFile { id }) -> unwrap id)
    (\(UninterpretedDomeinFile { id, referredModels }) -> difference referredModels [ unwrap id ])
    toSort
    action

