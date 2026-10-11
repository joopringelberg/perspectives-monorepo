-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.PerformanceHarnessTests (main) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), isLeft)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, attempt, error)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, new, read)
import Test.Layer3Scaffold (executeMeasuredTrial)
import Test.PDRInstance (SynchronisationResult)
import Test.PerformanceMeasurements (MeasurementHooks)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest $ suite "Measured scaffold trial isolation" do
  test "successful trials allow the next scenario" do
    checkTrials (pure $ Right { testName: "scenario", testSucceeded: true }) "success" false 2
  test "semantic failure stops before the next scenario" do
    checkTrials (pure $ Right { testName: "scenario", testSucceeded: false }) "semantic-failure" true 1
  test "completion timeout stops before the next scenario" do
    checkTrials (pure $ Left { testName: "scenario", err: error "timeout" }) "completion-timeout" true 1
  test "scenario exception stops before the next scenario" do
    checkTrials (throwError $ error "scenario failed") "exception" true 1

checkTrials :: Aff SynchronisationResult -> String -> Boolean -> Int -> Aff Unit
checkTrials firstAction expectedStatus shouldAbort expectedStarts = do
  starts <- liftEffect $ new 0
  statuses <- liftEffect $ new []
  let
    hooks :: MeasurementHooks
    hooks =
      { beginTrial: \_ -> modify_ (_ + 1) starts
      , beginAction: pure unit
      , endAction: pure unit
      , endCompletion: pure unit
      , endTrial: \status _ -> modify_ (_ <> [ status ]) statuses
      }
    nextAction = pure $ Right { testName: "next", testSucceeded: true }
  outcome <- attempt $ traverse_ (executeMeasuredTrial hooks "scenario") [ firstAction, nextAction ]
  actualStarts <- liftEffect $ read starts
  actualStatuses <- liftEffect $ read statuses
  equal expectedStarts actualStarts
  assert "failed scenarios must abort the measured traversal" (isLeft outcome == shouldAbort)
  equal (if shouldAbort then [ expectedStatus ] else [ expectedStatus, "success" ]) actualStatuses
