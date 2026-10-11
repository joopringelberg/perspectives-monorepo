-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.Performance (main) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Test.DestructiveSynchronisationTests (synchronisationTestModelConfiguration)
import Test.Layer3Scaffold (getMeasuredSynchronisationResults, performanceSnapshotsAvailable)
import Test.PerformanceMeasurements (amqpMode, configureScenarios, measurementHooks, publishFailure, publishResults)

main :: Effect Unit
main = launchAff_ do
  liftEffect $ configureScenarios (map _.testContextTypeName synchronisationTestModelConfiguration.tests)
  available <- performanceSnapshotsAvailable synchronisationTestModelConfiguration
  if not available then liftEffect $ publishFailure "missing-snapshots"
  else do
    overAMQP <- liftEffect amqpMode
    result <- attempt $ getMeasuredSynchronisationResults measurementHooks overAMQP synchronisationTestModelConfiguration
    liftEffect $ publishResults case result of
      Left _ -> false
      Right _ -> true
