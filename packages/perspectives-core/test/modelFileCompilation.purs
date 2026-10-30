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
-- | Every model is parsed from its listed ARC file and compiled through the
-- | normal phase-one-to-three pipeline. In-corpus imports are compiled first;
-- | imports outside the corpus and any import cycle must already be available
-- | in the cached PDR snapshot.
-- |
-- | Any parse or compile errors are reported clearly and the test fails if
-- | any are present.
-- |
-- | Run with:
-- |   pnpm run test:compileModelsFromFile

module Test.ModelFileCompilation where

import Prelude

import Data.Array (filter, find, fromFoldable, null)
import Data.Either (Either(..))
import Data.Foldable (all, any, for_)
import Data.List (mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Parsing (ParseError)
import Perspectives.Cuid2 (cuid2)
import Perspectives.Identifiers (modelUriVersion, unversionedModelUri)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE(..), ContextPart(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Messages (MultiplePerspectivesErrors)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runMonadPerspectivesTransaction')
import Perspectives.Sidecar.StableIdMapping (ModelUri(..))
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFile_, parseError2PerspectivesError)
import Test.PDRInstance (noBus, testPouchdbUser, withPDRCached)
import Test.PDRInstance.Types (runInPDR)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

type ModelSource = { modelUri :: String, filePath :: String }

type PreparedModelSource =
  { modelUri :: String
  , filePath :: String
  , source :: String
  , parsed :: Either ParseError ContextE
  , dependencies :: Array String
  }

type CompilationResult =
  { modelUri :: String
  , filePath :: String
  , parsedModelUri :: Maybe String
  , errors :: MultiplePerspectivesErrors
  }

main :: Effect Unit
main = launchAff_ do
  testResults <- getCompilationResults
  liftEffect $ runTest $ modelFileCompilationSuite testResults

getCompilationResults :: Aff (Array CompilationResult)
getCompilationResults = do
  let user = testPouchdbUser "alice"
  preparedSources <- for modelSources prepareModelSource
  withPDRCached user defaultRuntimeOptions Nothing noBus snapshotDirectory \pdr -> do
    for (dependencyFirstOrder preparedSources) \candidate ->
      case candidate.parsed of
        Left e -> pure
          { modelUri: candidate.modelUri
          , filePath: candidate.filePath
          , parsedModelUri: Nothing
          , errors: [ parseError2PerspectivesError e ]
          }
        Right (ContextE { id: parsedModelUri }) ->
          if parsedModelUri /= candidate.modelUri then
            pure
              { modelUri: candidate.modelUri
              , filePath: candidate.filePath
              , parsedModelUri: Just parsedModelUri
              , errors: []
              }
          else do
            modelCuid <- liftEffect $ cuid2 parsedModelUri
            r <- runInPDR pdr $ do
              runMonadPerspectivesTransaction' doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser)
                -- The source always enters the phase-one-to-three compiler. Caching
                -- its result only makes it available to later corpus dependencies.
                (loadAndCompileArcFile_ (ModelUri parsedModelUri) candidate.source true modelCuid parsedModelUri Nothing)
            case r of
              Left errors -> pure
                { modelUri: candidate.modelUri
                , filePath: candidate.filePath
                , parsedModelUri: Just parsedModelUri
                , errors
                }
              Right _ -> pure
                { modelUri: candidate.modelUri
                , filePath: candidate.filePath
                , parsedModelUri: Just parsedModelUri
                , errors: []
                }

modelFileCompilationSuite :: Array CompilationResult -> TestSuite
modelFileCompilationSuite testResults =
  suite "Model file compilation tests" do
    for_ testResults \{ modelUri, filePath, parsedModelUri, errors } ->
      test (modelUri <> " (" <> filePath <> ") compiles from ARC source") do
        assert
          ( "Expected domain URI "
              <> modelUri
              <> ", parsed "
              <> show parsedModelUri
              <> "; compilation errors: "
              <> show errors
          )
          (parsedModelUri == Just modelUri && null errors)

prepareModelSource :: ModelSource -> Aff PreparedModelSource
prepareModelSource { modelUri, filePath } = do
  source <- readTextFile UTF8 filePath
  parsed <- runIndentParser source domain
  let
    dependencies = case parsed of
      Left _ -> []
      Right (ContextE { contextParts }) ->
        fromFoldable $ mapMaybe
          ( case _ of
              PREFIX _ dependency -> Just dependency
              _ -> Nothing
          )
          contextParts
  pure { modelUri, filePath, source, parsed, dependencies }

dependencyFirstOrder :: Array PreparedModelSource -> Array PreparedModelSource
dependencyFirstOrder sources = go [] sources
  where
  go ordered pending
    | null pending = ordered
    | otherwise =
        let
          ready = filter (dependenciesAvailable ordered) pending
        in
          if null ready then ordered <> pending
          else
            go
              (ordered <> ready)
              (filter (\source -> not $ any (\readySource -> readySource.modelUri == source.modelUri) ready) pending)

  dependenciesAvailable ordered source =
    all (dependencyAvailable ordered source) source.dependencies

  dependencyAvailable ordered source dependency = case find (matchesDependency dependency <<< _.modelUri) sources of
    Nothing -> true
    Just dependencySource ->
      dependencySource.modelUri == source.modelUri
        || any (\compiledSource -> compiledSource.modelUri == dependencySource.modelUri) ordered

  matchesDependency dependency sourceUri = case modelUriVersion dependency of
    Just _ -> sourceUri == dependency
    Nothing -> unversionedModelUri sourceUri == dependency

-- System and RepositoryRegistry are installed by setupUser; RepositoryRegistry
-- also appears in extraModels, so each source URI is listed once.
modelSources :: Array ModelSource
modelSources =
  [ { modelUri: "model://perspectives.domains#System@7.0", filePath: "src/model/perspectivesSysteem@7.0.arc" }
  , { modelUri: "model://perspectives.domains#RabbitMQ@2.0", filePath: "src/model/rabbitMQ@2.0.arc" }
  , { modelUri: "model://perspectives.domains#BrokerServices@7.0", filePath: "src/model/BrokerServices@7.0.arc" }
  , { modelUri: "model://perspectives.domains#HyperContext@1.0", filePath: "src/model/hypertext@1.0.arc" }
  , { modelUri: "model://perspectives.domains#Introduction@1.0", filePath: "src/model/introduction@1.0.arc" }
  , { modelUri: "model://perspectives.domains#HelpProject@3.0", filePath: "src/model/helpProject@3.0.arc" }
  , { modelUri: "model://perspectives.domains#Disconnect@1.1", filePath: "src/model/disconnect@1.1.arc" }
  , { modelUri: "model://perspectives.domains#RepositoryRegistry@1.0", filePath: "src/model/repositoryRegistry@1.0.arc" }
  , { modelUri: "model://perspectives.domains#SharedFileServices@4.0", filePath: "src/model/sharedFileStorage@4.0.arc" }
  , { modelUri: "model://joopringelberg.nl#SynchronisationTestModel@2.0", filePath: "src/model/testSynchronisation@2.0.arc" }
  , { modelUri: "model://joopringelberg.nl#TwoPDRDestructiveTests@1.0", filePath: "src/model/twoPDRDestructiveTests@1.0.arc" }
  , { modelUri: "model://joopringelberg.nl#StateTestModel@1.0", filePath: "src/model/testState@1.0.arc" }
  , { modelUri: "model://joopringelberg.nl#SinglePDRDestructiveTests@2.0", filePath: "src/model/singlePDRDestructiveTests@2.0.arc" }
  , { modelUri: "model://joopringelberg.nl#TransactionExecutionTests@1.0", filePath: "src/model/transactionExecutionTests@1.0.arc" }
  , { modelUri: "model://joopringelberg.nl#AMQPtestModel@1.0", filePath: "src/model/AMQPtestModel@1.0.arc" }
  , { modelUri: "model://joopringelberg.nl#AMQPtestSetup@1.0", filePath: "src/model/AMQPtestSetup@1.0.arc" }
  ]

-- | Directory where the PDR snapshot is cached between test runs.
-- | Generate it with `pnpm run create:universeSnapshot` where the model
-- | repositories and their certificate authority are available.
-- | Delete this directory to force a full PDR rebuild on the next run.
snapshotDirectory :: String
snapshotDirectory = "test/pdr-snapshot/universe/alice"
