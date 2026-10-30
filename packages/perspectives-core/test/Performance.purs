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
  overAMQP <- liftEffect amqpMode
  let
    configuration =
      if overAMQP then synchronisationTestModelConfiguration
        { snapshotDirAlice = "test/pdr-snapshot/rebootedUniverse/alice"
        , snapshotDirBob = "test/pdr-snapshot/universe/bob"
        }
      else synchronisationTestModelConfiguration
  liftEffect $ configureScenarios (map _.testContextTypeName configuration.tests)
  available <- performanceSnapshotsAvailable configuration
  if not available then liftEffect $ publishFailure "missing-snapshots"
  else do
    result <- attempt $ getMeasuredSynchronisationResults measurementHooks overAMQP configuration
    liftEffect $ publishResults case result of
      Left _ -> false
      Right _ -> true
