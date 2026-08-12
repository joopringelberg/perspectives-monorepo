module Test.TransactionExecutionTestsCompile where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.SinglePDRScaffold (getSinglePDRResults)
import Test.TransactionExecutionTests (transactionExecutionCompileTestModelConfiguration, transactionExecutionSuite)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  results <- getSinglePDRResults transactionExecutionCompileTestModelConfiguration
  liftEffect $ runTest do
    transactionExecutionSuite results
