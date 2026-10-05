module Test.RebootUniverse where

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
-- pnpm run test:rebootUniverse

main :: Effect Unit
main = launchAff_ do
  results <- getSinglePDRResults rebootUniverseCompileTestModelConfiguration
  liftEffect $ runTest do
    rebootUniverseSuite results

rebootUniverseSuite :: SinglePDRResults -> TestSuite
rebootUniverseSuite results =
  suite "Reboot universe tests" do
    for_ results \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed") do
          assert ("Test '" <> testName <> "' should succeed") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Test should succeed, but got error: " <> show err) false

rebootUniverseConfiguration :: SinglePDRModelConfiguration
rebootUniverseConfiguration =
  { suiteName: "Reboot universe tests"
  , snapshotDirectory: rebootUniverseSnapshotDirectory
  , outputSnapshotDirectory: Nothing
  , testModel: rebootUniverseTestModel
  , testModelLoadMethods: [ LoadModelFromRepository { modelUri: rebootUniverseTestModel } ]
  , indexedTestContext: rebootUniverseIndexedTestContext
  , testAppManager: rebootUniverseTestAppManager
  , testsType: rebootUniverseTestsType
  , testSucceededProperty: rebootUniverseTestSucceededProperty
  , testNameProperty: rebootUniverseTestNameProperty
  , testTimeLimit: Milliseconds 1800000.0 -- 30 minutes: Big Bang runs all sub-tests in sequence
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
  rebootUniverseConfiguration
    { suiteName = "Reboot universe tests (compile)"
    , testModelLoadMethods =
        [ CompileModelFromSource
            { modelUri: repositoryToolsTestModel
            , sourcePath: "src/model/repositoryTools@1.0.arc"
            , modelUriReadable: "model://joopringelberg.nl#RepositoryTools@1.0"
            , basedOnVersion: Nothing
            }
        , CompileModelFromSource
            { modelUri: rebootUniverseTestModel
            , sourcePath: "src/model/rebootUniverse@2.0.arc"
            , modelUriReadable: "model://joopringelberg.nl#RebootUniverse@2.0"
            , basedOnVersion: Nothing
            }
        ]
    , outputSnapshotDirectory = Just "test/pdr-snapshot/universe/aliceAfterReboot"
    }

rebootUniverseTestModel :: String
-- rebootUniverseTestModel = "model://joopringelberg.nl#RebootUniverse@2.0"
rebootUniverseTestModel = "model://joopringelberg.nl#p80ohyse8t@2.0"

repositoryToolsTestModel :: String
-- repositoryToolsTestModel = "model://joopringelberg.nl#RepositoryTools@1.0"
repositoryToolsTestModel = "model://joopringelberg.nl#ncr77pkxia@1.0"

rebootUniverseIndexedTestContext :: String
rebootUniverseIndexedTestContext = "model://joopringelberg.nl#RebootUniverse$RebootUniverseApp"

rebootUniverseTestAppManager :: String
rebootUniverseTestAppManager = "model://joopringelberg.nl#RebootUniverse$TestApp$Manager"

rebootUniverseTestsType :: String
rebootUniverseTestsType = "model://joopringelberg.nl#RebootUniverse$TestApp$Tests"

rebootUniverseTestSucceededProperty :: String
rebootUniverseTestSucceededProperty = "model://joopringelberg.nl#RepositoryTools$Test$External$TestSucceeded"

rebootUniverseTestNameProperty :: String
rebootUniverseTestNameProperty = "model://joopringelberg.nl#RepositoryTools$Test$External$TestName"

rebootUniverseSnapshotDirectory :: String
rebootUniverseSnapshotDirectory = "test/pdr-snapshot/universe/alice"

-- Outcomment all tests to just re-create the snapshot without trying to create databases.
rebootUniverseTests :: Array ModelTest
rebootUniverseTests =
  [
      { testContextTypeName: "model://joopringelberg.nl#RepositoryTools$Cleanup", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RepositoryTools$ManageCouchdb", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RepositoryTools$CreateBigBangsDatabase", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RepositoryTools$CreatePerspectivesDomainsRepository", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RepositoryTools$CreateJoopringelbergNlRepository", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Couchdb", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Serialise", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Sensor", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Utilities", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_System", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_BodiesWithAccounts", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Parsing", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_HelpLib", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Files", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_CouchdbManagement", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_BrokerServices", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_RabbitMQ", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_HyperContext", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Introduction", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_HelpProject", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Disconnect", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_RepositoryRegistry", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_SharedFileServices", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_RepositoryTools", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_RebootUniverse", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_SynchronisationTestModel", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_TwoPDRDestructiveTests", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_StateTestModel", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_SinglePDRDestructiveTests", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_TransactionExecutionTests", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_AMQPtestModel", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_AMQPtestSetup", logConfiguration: emptyLogConfiguration }
    -- The next model is not yet finished. We'll continue after the reboot.
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_TestModelDependencies", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$ManageBrokerService", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$Add_public_pages", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$CreateRepositoryRegistryPublicPage", logConfiguration: emptyLogConfiguration }
    -- , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$SignUpToBrokerService", logConfiguration: emptyLogConfiguration }
    -- , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$ExecuteBigBang", logConfiguration: emptyLogConfiguration }
  ]

debugConfiguration :: LogConfiguration
debugConfiguration =
  { pdr:
      [
        -- { topic: TEST, logLevel: Trace }
        --   { topic: RESOURCE, logLevel: Trace }
        -- , { topic: STATE, logLevel: Trace }
        { topic: INSTALL, logLevel: Trace }
      -- , { topic: MODEL, logLevel: Debug }
      -- , { topic: ACTION, logLevel: Trace }
      ]
  }
