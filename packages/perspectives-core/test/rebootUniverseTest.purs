module Test.RebootUniverse where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..))
import Test.SinglePDRScaffold (ModelTest, SinglePDRModelConfiguration, SinglePDRResults, LogConfiguration, TestModelLoadMethod(..), emptyLogConfiguration, getSinglePDRResults)
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
  , testModel: rebootUniverseTestModel
  , testModelLoadMethod: LoadModelFromRepository
  , indexedTestContext: rebootUniverseIndexedTestContext
  , testAppManager: rebootUniverseTestAppManager
  , testsType: rebootUniverseTestsType
  , testSucceededProperty: rebootUniverseTestSucceededProperty
  , testNameProperty: rebootUniverseTestNameProperty
  , setupLogConfiguration:
      { pdr:
        [ { topic: TEST, logLevel: Trace }
        , { topic: RESOURCE, logLevel: Trace }
        , { topic: STATE, logLevel: Trace }
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
rebootUniverseSnapshotDirectory = "test/pdr-snapshot/rebootuniverse/alice"

rebootUniverseTests :: Array ModelTest
rebootUniverseTests = 
  [ 
      { testContextTypeName: "model://joopringelberg.nl#RebootUniverse$ManageCouchdb", logConfiguration: emptyLogConfiguration }
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
