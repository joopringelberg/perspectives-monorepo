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
-- | Fetches all models from the perspectives.domains repository databases,
-- | compiles each one (ARC parse → phase 2 → phase 3) in topological
-- | dependency order, and reports any parse or compile errors clearly.
-- |
-- | The test caches each successfully compiled model (using its readable
-- | identifier as the cache key) so that models compiled later can resolve
-- | cross-model type references from models compiled earlier.
-- |
-- | Uses `testOnly` because it requires network access to
-- |   https://perspectives.domains/models_perspectives_domains
-- | Run manually with:
-- |   pnpm run test:layer2

module Test.ModelCompilationRegression where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (catMaybes, foldM, length, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Foreign.Object (empty)
import Main.RecompileBasicModels (UninterpretedDomeinFile(..))
import Parsing (ParseError(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFileRecord)
import Perspectives.ExecuteInTopologicalOrder (sortTopologicallyEither)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo_', toStableDomeinFile, toStableModelUri)
import Perspectives.Persistence.API (documentsInDatabase, includeDocs)
import Perspectives.RunPerspectives (runPerspectivesWithoutCouchdb)
import Simple.JSON (read)
import Test.Unit (TestF, suite, testOnly)
import Test.Unit.Assert (assert)

-- | The CouchDB database that contains all compiled model documents
-- | for the perspectives.domains namespace.
modelsDb :: String
modelsDb = "https://perspectives.domains/models_perspectives_domains"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.ModelCompilationRegression" do

  testOnly "All models from perspectives.domains compile without errors" do
    errors <- runPerspectivesWithoutCouchdb "regressiontest" do
      -- Register external (JavaScript) functions so that computed values that
      -- reference them can be compiled in phase 3.
      addAllExternalFunctions
      -- Fetch every document from the models database.
      { rows: allDocs } <- documentsInDatabase modelsDb includeDocs
      -- Attempt to decode each document as an UninterpretedDomeinFile.
      -- Documents that cannot be decoded (design documents, etc.) are silently skipped.
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
        Left sortErrors ->
          pure [ "Topological sort failed: " <> show sortErrors ]
        Right sorted ->
          foldM (\errs model -> append errs <$> compileAndCacheModel model) [] sorted
    -- Print every error and fail if any were found.
    for_ errors log
    assert
      ( "Model compilation produced "
          <> show (length errors)
          <> " error(s) — see log above for details."
      )
      (null errors)

-- | Compile a single model through ARC phases 1–3, then store the result in
-- | the in-memory domain cache under its readable identifier.  Caching with
-- | the readable key ensures that subsequent compilations can resolve
-- | cross-model type references by the human-readable names used in ARC source.
-- |
-- | Returns an empty array on success, or one or more human-readable error
-- | strings on failure.
compileAndCacheModel :: UninterpretedDomeinFile -> MonadPerspectives (Array String)
compileAndCacheModel (UninterpretedDomeinFile { namespace, arc }) = do
  -- ── Phase 1: parse ARC source ──────────────────────────────────────────────
  (r :: Either ParseError ContextE) <- liftAff $ runIndentParser arc ARC.domain
  case r of
    Left parseErr ->
      pure [ namespace <> ": parse error — " <> show parseErr ]
    Right ctxt -> do
      -- ── Phase 2: name resolution ──────────────────────────────────────────
      Tuple result state <-
        liftAff $ runPhaseTwo_' (traverseDomain ctxt) defaultDomeinFileRecord empty empty Nil
      case result of
        Left errs ->
          pure (map (\e -> namespace <> ": phase-2 error — " <> show e) errs)
        Right (DomeinFile dr'@{ id }) -> do
          let dr'' = dr' { referredModels = state.referredModels }
          -- ── Phase 3: inverted-query / expression compilation ──────────────
          x' <- phaseThree dr'' state.postponedStateQualifiedParts state.screens
          case x' of
            Left errs ->
              pure (map (\e -> namespace <> ": phase-3 error — " <> show e) errs)
            Right (Tuple correctedDFR _) -> do
              -- Cache the readable DomeinFile so that later compilations can
              -- resolve references to types declared in this model.
              void $ storeDomeinFileInCache
                (toStableModelUri id)
                (toStableDomeinFile (DomeinFile correctedDFR { arc = arc }))
              pure []
