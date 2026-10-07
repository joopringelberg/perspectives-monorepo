module Test.ModelDependencies where

import Prelude

import Data.Time.Duration (Milliseconds(..))

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..))
import Test.SinglePDRScaffold (LogConfiguration, ModelTest, SinglePDRModelConfiguration, SinglePDRResults, TestModelLoadMethod(..), emptyLogConfiguration, getSinglePDRResults)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

-- Run with:
-- pnpm run test:modelDependencies

main :: Effect Unit
main = launchAff_ do
  results <- getSinglePDRResults rebootUniverseCompileTestModelConfiguration
  liftEffect $ runTest do
    modelDependenciesSuite results

modelDependenciesSuite :: SinglePDRResults -> TestSuite
modelDependenciesSuite results =
  suite "Model dependencies tests" do
    for_ results \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed") do
          assert ("Test '" <> testName <> "' should succeed") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Test should succeed, but got error: " <> show err) false

modelDependenciesConfiguration :: SinglePDRModelConfiguration
modelDependenciesConfiguration =
  { suiteName: "Module dependencies tests"
  , snapshotDirectory: rebootUniverseSnapshotDirectory
  , outputSnapshotDirectory: Nothing
  , testModel: modelDependenciesTestModel
  , testModelLoadMethods: [ LoadModelFromRepository { modelUri: modelDependenciesTestModel } ]
  , indexedTestContext: modelDependenciesIndexedTestContext
  , testAppManager: modelDependenciesTestAppManager
  , testsType: modelDependenciesTestsType
  , testSucceededProperty: repositoryToolsTestSucceededProperty
  , testNameProperty: repositoryToolsTestNameProperty
  , testTimeLimit: Milliseconds 180000.0
  , setupLogConfiguration: --emptyLogConfiguration
      { pdr:
          [ { topic: TEST, logLevel: Trace }
          -- , { topic: RESOURCE, logLevel: Trace }
          -- , { topic: STATE, logLevel: Trace }
          , { topic: INSTALL, logLevel: Trace }
          ]
      }
  , tests: rebootUniverseTests
  }

rebootUniverseCompileTestModelConfiguration :: SinglePDRModelConfiguration
rebootUniverseCompileTestModelConfiguration =
  modelDependenciesConfiguration
    { suiteName = "Reboot universe tests (compile)"
    , testModelLoadMethods =
        [ CompileModelFromSource
            { modelUri: repositoryToolsTestModel
            , sourcePath: "src/model/repositoryTools@1.0.arc"
            , modelUriReadable: "model://joopringelberg.nl#RepositoryTools@1.0"
            , basedOnVersion: Nothing
            }
        , CompileModelFromSource
            { modelUri: modelDependenciesTestModel
            , sourcePath: "src/model/modelDependenciesTest@1.0.arc"
            , modelUriReadable: "model://joopringelberg.nl#TestModelDependencies@1.0"
            , basedOnVersion: Nothing
            }
        ]
    , outputSnapshotDirectory = Just "test/pdr-snapshot/universe/aliceAfterReboot"
    }

modelDependenciesTestModel :: String
-- modelDependenciesTestModel = "model://joopringelberg.nl#TestModelDependencies@1.0"
modelDependenciesTestModel = "model://joopringelberg.nl#khmgwn5c2w@1.0"

repositoryToolsTestModel :: String
-- repositoryToolsTestModel = "model://joopringelberg.nl#RepositoryTools@1.0"
repositoryToolsTestModel = "model://joopringelberg.nl#ncr77pkxia@1.0"

modelDependenciesIndexedTestContext :: String
modelDependenciesIndexedTestContext = "model://joopringelberg.nl#TestModelDependencies$TestModelDependenciesApp"

modelDependenciesTestAppManager :: String
modelDependenciesTestAppManager = "model://joopringelberg.nl#TestModelDependencies$TestApp$Manager"

modelDependenciesTestsType :: String
modelDependenciesTestsType = "model://joopringelberg.nl#TestModelDependencies$TestApp$Tests"

repositoryToolsTestSucceededProperty :: String
repositoryToolsTestSucceededProperty = "model://joopringelberg.nl#RepositoryTools$Test$External$TestSucceeded"

repositoryToolsTestNameProperty :: String
repositoryToolsTestNameProperty = "model://joopringelberg.nl#RepositoryTools$Test$External$TestName"

rebootUniverseSnapshotDirectory :: String
rebootUniverseSnapshotDirectory = "test/pdr-snapshot/universe/alice"

-- Outcomment all tests to just re-create the snapshot without trying to create databases.
rebootUniverseTests :: Array ModelTest
rebootUniverseTests =
  [ { testContextTypeName: "model://joopringelberg.nl#RepositoryTools$Cleanup", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RepositoryTools$ManageCouchdb", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RepositoryTools$CreatePerspectivesDomainsRepository", logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: "model://joopringelberg.nl#TestModelDependencies$LowerDependency", logConfiguration: debugConfiguration }
  -- , { testContextTypeName: "model://joopringelberg.nl#TestModelDependencies$HigherDependency", logConfiguration: debugConfiguration }
  ]

debugConfiguration :: LogConfiguration
debugConfiguration =
  { pdr:
      [
        -- { topic: TEST, logLevel: Trace }
        { topic: RESOURCE, logLevel: Trace }
      , { topic: STATE, logLevel: Trace }
      , { topic: INSTALL, logLevel: Trace }
      , { topic: MODEL, logLevel: Debug }
      ]
  }
