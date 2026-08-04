module Test.TransactionExecutionTests where

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

main :: Effect Unit
main = launchAff_ do
  results <- getSinglePDRResults transactionExecutionTestModelConfiguration
  liftEffect $ runTest do
    transactionExecutionSuite results

transactionExecutionSuite :: SinglePDRResults -> TestSuite
transactionExecutionSuite results =
  suite "Transaction execution tests" do
    for_ results \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed") do
          assert ("Test '" <> testName <> "' should succeed") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Test should succeed, but got error: " <> show err) false

transactionExecutionTestModelConfiguration :: SinglePDRModelConfiguration
transactionExecutionTestModelConfiguration =
  { suiteName: "Transaction execution tests"
  , snapshotDirectory: transactionExecutionSnapshotDirectory
  , testModel: transactionExecutionTestModel
  , testModelLoadMethod: LoadModelFromRepository
  , indexedTestContext: transactionExecutionIndexedTestContext
  , testAppManager: transactionExecutionTestAppManager
  , testsType: transactionExecutionTestsType
  , testSucceededProperty: transactionExecutionTestSucceededProperty
  , testNameProperty: transactionExecutionTestNameProperty
  , setupLogConfiguration:
      { pdr:
          [ { topic: TEST, logLevel: Debug }
          -- , { topic: RESOURCE, logLevel: Trace }
          -- , { topic: STATE, logLevel: Trace }
          ]
      }
  , tests: transactionExecutionTests
  }

transactionExecutionCompileTestModelConfiguration :: SinglePDRModelConfiguration
transactionExecutionCompileTestModelConfiguration =
  transactionExecutionTestModelConfiguration
    { suiteName = "Transaction execution tests (compile)"
    , testModelLoadMethod =
        CompileModelFromSource
          { sourcePath: "src/model/transactionExecutionTests@1.0.arc"
          , modelUriReadable: "model://joopringelberg.nl#TransactionExecutionTests@1.0"
          , basedOnVersion: Nothing
          }
    }

transactionExecutionTestModel :: String
-- transactionExecutionTestModel = "model://joopringelberg.nl#TransactionExecutionTests@1.0"
transactionExecutionTestModel = "model://joopringelberg.nl#eqcwpoi6u6@1.0"

transactionExecutionIndexedTestContext :: String
transactionExecutionIndexedTestContext = "model://joopringelberg.nl#TransactionExecutionTests$TransactionExecutionTestsApp"

transactionExecutionTestAppManager :: String
transactionExecutionTestAppManager = "model://joopringelberg.nl#TransactionExecutionTests$TestApp$Manager"

transactionExecutionTestsType :: String
transactionExecutionTestsType = "model://joopringelberg.nl#TransactionExecutionTests$TestApp$Tests"

transactionExecutionTestSucceededProperty :: String
transactionExecutionTestSucceededProperty = "model://joopringelberg.nl#TransactionExecutionTests$Test$External$TestSucceeded"

transactionExecutionTestNameProperty :: String
transactionExecutionTestNameProperty = "model://joopringelberg.nl#TransactionExecutionTests$Test$External$TestName"

transactionExecutionSnapshotDirectory :: String
transactionExecutionSnapshotDirectory = "test/pdr-snapshot/transaction-execution"

transactionExecutionTests :: Array ModelTest
transactionExecutionTests = 
  [ 
      { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T01", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T02", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T03", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T04", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T05", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T07", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T08", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T08a", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T09", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T10", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T12", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T13", logConfiguration: emptyLogConfiguration }
    , { testContextTypeName: "model://joopringelberg.nl#TransactionExecutionTests$T14", logConfiguration: emptyLogConfiguration }
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
