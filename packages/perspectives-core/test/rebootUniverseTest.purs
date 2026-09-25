module Test.RebootUniverse where

import Prelude

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
  , testModelLoadMethod: LoadModelFromRepository
  , indexedTestContext: rebootUniverseIndexedTestContext
  , testAppManager: rebootUniverseTestAppManager
  , testsType: rebootUniverseTestsType
  , testSucceededProperty: rebootUniverseTestSucceededProperty
  , testNameProperty: rebootUniverseTestNameProperty
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
    , testModelLoadMethod =
        CompileModelFromSource
          { sourcePath: "src/model/rebootUniverse@1.0.arc"
          , modelUriReadable: "model://joopringelberg.nl#RebootUniverse@1.0"
          , basedOnVersion: Nothing
          }
    , outputSnapshotDirectory = Just "test/pdr-snapshot/universe/aliceAfterReboot"
    }

rebootUniverseTestModel :: String
-- rebootUniverseTestModel = "model://joopringelberg.nl#RebootUniverse@1.0"
rebootUniverseTestModel = "model://joopringelberg.nl#eqcwpoi6u6@1.0"

rebootUniverseIndexedTestContext :: String
rebootUniverseIndexedTestContext = "model://joopringelberg.nl#RebootUniverse$RebootUniverseApp"

rebootUniverseTestAppManager :: String
rebootUniverseTestAppManager = "model://joopringelberg.nl#RebootUniverse$TestApp$Manager"

rebootUniverseTestsType :: String
rebootUniverseTestsType = "model://joopringelberg.nl#RebootUniverse$TestApp$Tests"

rebootUniverseTestSucceededProperty :: String
rebootUniverseTestSucceededProperty = "model://joopringelberg.nl#RebootUniverse$Test$External$TestSucceeded"

rebootUniverseTestNameProperty :: String
rebootUniverseTestNameProperty = "model://joopringelberg.nl#RebootUniverse$Test$External$TestName"

rebootUniverseSnapshotDirectory :: String
rebootUniverseSnapshotDirectory = "test/pdr-snapshot/universe/alice"

-- Outcomment all tests to just re-create the snapshot without trying to create databases.
rebootUniverseTests :: Array ModelTest
rebootUniverseTests =
  [ { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$Cleanup", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$ManageCouchdb", logConfiguration: emptyLogConfiguration }
  -- The following tests, once run, are not necessary to run ManageBrokerService and Add_public_pages.
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$CreatePerspectivesDomainsRepository", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Couchdb", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Serialise", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Sensor", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Utilities", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_System", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_BodiesWithAccounts", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Parsing", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_HelpLib", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Files", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_CouchdbManagement", logConfiguration: debugConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_BrokerServices", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_RabbitMQ", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_HyperContext", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Introduction", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_HelpProject", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_Disconnect", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_RepositoryRegistry", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$AddModel_SharedFileServices", logConfiguration: emptyLogConfiguration }
  -- The above tests, once run, are not necessary to run ManageBrokerService and Add_public_pages.
  -- , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$ManageBrokerService", logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$Add_public_pages", logConfiguration: emptyLogConfiguration }
  ]

debugConfiguration :: LogConfiguration
debugConfiguration =
  { pdr:
      [
        -- { topic: TEST, logLevel: Trace }
        { topic: RESOURCE, logLevel: Trace }
      , { topic: STATE, logLevel: Trace }
      -- , { topic: INSTALL, logLevel: Trace }
      ]
  }
