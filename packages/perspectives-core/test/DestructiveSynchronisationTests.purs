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

module Test.DestructiveSynchronisationTests
  ( getSynchronisationResults
  , main
  , synchronisationSuite
  , synchronisationTestModelConfiguration
  )
  where

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
import Test.Layer3Scaffold (ModelTest, SynchronisationModelConfiguration, SynchronisationResults, emptyLogConfiguration, LogConfiguration)
import Test.Layer3Scaffold (getSynchronisationResults) as Layer3Scaffold
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
      results <- Layer3Scaffold.getSynchronisationResults cachedSynchronisationResults synchronisationTestModelConfiguration
      liftEffect $ write (Just results) cachedSynchronisationResults
      pure results

synchronisationSuite :: SynchronisationResults -> TestSuite
synchronisationSuite results =
  suite "Destructive synchronisation tests" do
    for_ results \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed in Bob's PDR") do
          assert ("Bob should see that test '" <> testName <> "' succeeded") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Bob should see that the test succeeded, but got error: " <> show err) false

synchronisationTestModelConfiguration :: SynchronisationModelConfiguration
synchronisationTestModelConfiguration =
  { suiteName: "Destructive synchronisation tests"
  , snapshotDirAlice: "test/pdr-snapshot/layer3-clean/alice"
  , snapshotDirBob: "test/pdr-snapshot/layer3-clean/bob"
  , testModel
  , indexedTestContext
  , testAppManager
  , testAppFollowerType
  , testsType
  , testSucceededProperty
  , testNameProperty
  , setupLogConfiguration:
      { pdrA:
          [ { topic: TEST, logLevel: Trace }
          ]
      , pdrB:
          [ { topic: TEST, logLevel: Trace }
          ]
      }
  , tests: destructiveTests
  }

-------------------------------------------------------------------------------
---- NECESSARY READABLE TYPE NAMES IN model://joopringelberg.nl#TwoPDRDestructiveTests
-------------------------------------------------------------------------------
testModel :: String
testModel = "model://joopringelberg.nl#jdqywnil7h@1.0"
-- testModel = "model://joopringelberg.nl#TwoPDRDestructiveTests@1.0"

indexedTestContext :: String
indexedTestContext = "model://joopringelberg.nl#TwoPDRDestructiveTests$TwoPDRDestructiveTestsApp"

testAppManager :: String
testAppManager = "model://joopringelberg.nl#TwoPDRDestructiveTests$TestApp$Manager"

testAppFollowerType :: String
testAppFollowerType = "model://joopringelberg.nl#TwoPDRDestructiveTests$TestApp$Follower"

testsType :: String
testsType = "model://joopringelberg.nl#TwoPDRDestructiveTests$TestApp$Tests"

testSucceededProperty :: String
testSucceededProperty = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test$External$TestSucceeded"

testNameProperty :: String
testNameProperty = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test$External$TestName"

-------------------------------------------------------------------------------
---- THE TESTS
---- One entry for each test in model://joopringelberg.nl#TwoPDRDestructiveTests
-------------------------------------------------------------------------------

destructiveTests :: Array ModelTest
destructiveTests = 
  [ 
    { testContextTypeName: test_RemoveRole, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RemoveOneRoleInstance, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_DeleteTwoRoles, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_DeleteProperty, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RemoveProperty, logConfiguration: debugConfiguration }
  , { testContextTypeName: test_RemoveOnePropertyValue, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RemoveFiller, logConfiguration: debugConfiguration }
  -- , { testContextTypeName: test_RemoveRoleFiller, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_RemoveRoleFiller_SpecificRoleTypes, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_UnBindRoleFiller_SpecificRoleTypes, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_RemoveContextWithoutRoles, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_RemoveContextWithUnfilledRole, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_RemoveContextWithFilledRole, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_RemoveContextWithFilledRoleThatShouldRemain, logConfiguration: emptyLogConfiguration }
  ]

debugConfiguration :: LogConfiguration
debugConfiguration =       
  { pdrA:
    [
    { topic: TEST, logLevel: Trace }
    , { topic: RESOURCE, logLevel: Trace }
    , { topic: STATE, logLevel: Trace }
    , { topic: INSTALL, logLevel: Trace }
    ]
  , pdrB:
    [ { topic: TEST, logLevel: Trace }
    , { topic: RESOURCE, logLevel: Trace }
    , { topic: STATE, logLevel: Trace }
    , { topic: SYNC, logLevel: Trace }
    , { topic: BROKER, logLevel: Trace }
    ]
  }


test_RemoveRole :: String
test_RemoveRole = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveRole"

test_DeleteProperty :: String
test_DeleteProperty = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_DeleteProperty"

test_RemoveProperty :: String
test_RemoveProperty = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveProperty"

test_RemoveOnePropertyValue :: String
test_RemoveOnePropertyValue = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveOnePropertyValue"

test_DeleteTwoRoles :: String
test_DeleteTwoRoles = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_DeleteTwoRoles"

test_RemoveOneRoleInstance :: String
test_RemoveOneRoleInstance = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveOneRoleInstance"

test_RemoveFiller :: String
test_RemoveFiller = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveFiller"

test_RemoveRoleFiller :: String
test_RemoveRoleFiller = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveRoleFiller"

test_RemoveContextWithoutRoles :: String
test_RemoveContextWithoutRoles = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveContextWithoutRoles"

test_RemoveRoleFiller_SpecificRoleTypes :: String
test_RemoveRoleFiller_SpecificRoleTypes = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveRoleFiller_SpecificRoleTypes"

test_UnBindRoleFiller_SpecificRoleTypes :: String
test_UnBindRoleFiller_SpecificRoleTypes = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_UnBindRoleFiller_SpecificRoleTypes"

test_RemoveContextWithUnfilledRole :: String
test_RemoveContextWithUnfilledRole = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveContextWithUnfilledRole"

test_RemoveContextWithFilledRole :: String
test_RemoveContextWithFilledRole = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveContextWithFilledRole"

test_RemoveContextWithFilledRoleThatShouldRemain :: String
test_RemoveContextWithFilledRoleThatShouldRemain = "model://joopringelberg.nl#TwoPDRDestructiveTests$Test_RemoveContextWithFilledRoleThatShouldRemain"