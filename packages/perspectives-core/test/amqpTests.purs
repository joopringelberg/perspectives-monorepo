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
-- Full text of this license can be found in the LICENSE directory in the
-- projects root.

-- END LICENSE

module Test.AMQPTests
  ( main
  , getSynchronisationResults
  , synchronisationSuite
  , synchronisationTestModelConfiguration
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, new, read, write)
import Effect.Unsafe (unsafePerformEffect)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..))
import Test.Layer3Scaffold (LogConfiguration, ModelTest, SynchronisationModelConfiguration, SynchronisationResults)
import Test.Layer3Scaffold (getSynchronisationResultsOverAMQP) as Layer3Scaffold
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  results <- getSynchronisationResults
  liftEffect $ runTest do
    synchronisationSuite results

cachedSynchronisationResults :: Ref (Maybe SynchronisationResults)
cachedSynchronisationResults = unsafePerformEffect $ new Nothing

getSynchronisationResults :: Aff SynchronisationResults
getSynchronisationResults = do
  cached <- liftEffect $ read cachedSynchronisationResults
  case cached of
    Just results -> pure results
    Nothing -> do
      results <- Layer3Scaffold.getSynchronisationResultsOverAMQP cachedSynchronisationResults synchronisationTestModelConfiguration
      liftEffect $ write (Just results) cachedSynchronisationResults
      pure results

synchronisationSuite :: SynchronisationResults -> TestSuite
synchronisationSuite results =
  suite "AMQP synchronisation tests" do
    for_ results \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed in Bob's PDR") do
          assert ("Bob should see that test '" <> testName <> "' succeeded") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Bob should see that the test succeeded, but got error: " <> show err) false

synchronisationTestModelConfiguration :: SynchronisationModelConfiguration
synchronisationTestModelConfiguration =
  { suiteName: "AMQP synchronisation tests"
  , snapshotDirAlice: "test/pdr-snapshot/layer3-clean/alice"
  , snapshotDirBob: "test/pdr-snapshot/layer3-clean/bob"
  , testModel
  , indexedTestContext
  , testAppManager
  , testAppFollowerType
  , testsType
  , testSucceededProperty
  , testNameProperty
  -- suite level log configuration, which is used for all tests in this suite, unless a test has its own log configuration.
  , setupLogConfiguration:
      { pdrA:
          [ { topic: TEST, logLevel: Trace }
          , { topic: INSTALL, logLevel: Trace }
          , { topic: RESOURCE, logLevel: Trace }
          , { topic: STATE, logLevel: Trace }
          , { topic: BROKER, logLevel: Trace }
          ]
      , pdrB:
          [ { topic: TEST, logLevel: Trace }
          , { topic: BROKER, logLevel: Trace }
          ]
      }
  , tests: allTests
  }

-------------------------------------------------------------------------------
---- NECESSARY READABLE TYPE NAMES IN model://joopringelberg.nl#AMQPtestModel
-------------------------------------------------------------------------------
testModel :: String
testModel = "model://joopringelberg.nl#xyyehk9bpc@1.0"

-- testModel = "model://joopringelberg.nl#AMQPtestModel@1.0"

indexedTestContext :: String
indexedTestContext = "model://joopringelberg.nl#AMQPtestModel$AMQPtestSyncApp"

testAppManager :: String
testAppManager = "model://joopringelberg.nl#AMQPtestModel$TestApp$Manager"

testAppFollowerType :: String
testAppFollowerType = "model://joopringelberg.nl#AMQPtestModel$TestApp$Follower"

testsType :: String
testsType = "model://joopringelberg.nl#AMQPtestModel$TestApp$Tests"

testSucceededProperty :: String
testSucceededProperty = "model://joopringelberg.nl#AMQPtestModel$Test$External$TestSucceeded"

testNameProperty :: String
testNameProperty = "model://joopringelberg.nl#AMQPtestModel$Test$External$TestName"

test_Leader_Terminates_Contract :: String
test_Leader_Terminates_Contract = "model://joopringelberg.nl#AMQPtestModel$Test_Leader_Terminates_Contract"

-------------------------------------------------------------------------------
---- THE TESTS
---- One entry for each test in model://joopringelberg.nl#AMQPtestModel
-------------------------------------------------------------------------------

allTests :: Array ModelTest
allTests =
  [
    -- { testContextTypeName: test_SetProperty, logConfiguration: Nothing }
    { testContextTypeName: test_Leader_Terminates_Contract, logConfiguration: Just debugConfiguration }
  ]

debugConfiguration :: LogConfiguration
debugConfiguration =
  { pdrA:
      [ { topic: TEST, logLevel: Trace }
      , { topic: RESOURCE, logLevel: Trace }
      , { topic: STATE, logLevel: Trace }
      , { topic: SYNC, logLevel: Trace }
      , { topic: PERSISTENCE, logLevel: Trace }
      -- , { topic: INSTALL, logLevel: Trace }
      ]
  , pdrB:
      [ { topic: TEST, logLevel: Trace }
      , { topic: RESOURCE, logLevel: Trace }
      , { topic: STATE, logLevel: Trace }
      , { topic: SYNC, logLevel: Trace }
      , { topic: PERSISTENCE, logLevel: Trace }
      -- , { topic: BROKER, logLevel: Trace }
      -- , { topic: INSTALL, logLevel: Trace }
      ]
  }

test_SetProperty :: String
test_SetProperty = "model://joopringelberg.nl#AMQPtestModel$Test_SetProperty"