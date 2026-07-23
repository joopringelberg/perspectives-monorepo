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

module Test.ConstructiveSynchronisationTest
  ( getSynchronisationResults
  , synchronisationSuite
  , synchronisationTestModelConfiguration
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, new, read, write)
import Effect.Unsafe (unsafePerformEffect)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..))
import Test.Layer3Scaffold (ModelTest, SynchronisationModelConfiguration, SynchronisationResults, emptyLogConfiguration)
import Test.Layer3Scaffold (getSynchronisationResults) as Layer3Scaffold
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)

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
  suite "Constructive synchronisation tests" do
    for_ results \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed in Bob's PDR") do
          assert ("Bob should see that test '" <> testName <> "' succeeded") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Bob should see that the test succeeded, but got error: " <> show err) false

synchronisationTestModelConfiguration :: SynchronisationModelConfiguration
synchronisationTestModelConfiguration =
  { suiteName: "Constructive synchronisation tests"
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
  , tests: allTests
  }

-------------------------------------------------------------------------------
---- NECESSARY READABLE TYPE NAMES IN model://joopringelberg.nl#SynchronisationTestModel
-------------------------------------------------------------------------------
testModel :: String
testModel = "model://joopringelberg.nl#hj1bh3wydo@2.0"
-- testModel = "model://joopringelberg.nl#SynchronisationTestModel@2.0"

indexedTestContext :: String
indexedTestContext = "model://joopringelberg.nl#SynchronisationTestModel$TestSyncApp"

testAppManager :: String
testAppManager = "model://joopringelberg.nl#SynchronisationTestModel$TestApp$Manager"

testAppFollowerType :: String
testAppFollowerType = "model://joopringelberg.nl#SynchronisationTestModel$TestApp$Follower"

testsType :: String
testsType = "model://joopringelberg.nl#SynchronisationTestModel$TestApp$Tests"

testSucceededProperty :: String
testSucceededProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test$External$TestSucceeded"

testNameProperty :: String
testNameProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test$External$TestName"

-------------------------------------------------------------------------------
---- THE TESTS
---- One entry for each test in model://joopringelberg.nl#SynchronisationTestModel
-------------------------------------------------------------------------------

allTests :: Array ModelTest
allTests =
  [ { testContextTypeName: test_CreateRole, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_SetProperty, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_SetProperty_on_Filler, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Binding_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_SetProperty_in_CalculatedProperty, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Binding_in_CalculatedProperty, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Binder_in_CalculatedProperty, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Binding_in_CalculatedRole, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_SetProperty_in_CalculatedProperty_BindingStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_SetProperty_in_CalculatedProperty_BinderStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_CreateRole_in_CalculatedRole_ContextStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_CreateRole_in_CalculatedRole_RoleStep, logConfiguration: emptyLogConfiguration }
  ]

test_CreateRole :: String
test_CreateRole = "model://joopringelberg.nl#SynchronisationTestModel$Test_CreateRole"

test_SetProperty :: String
test_SetProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test_SetProperty"

test_Binding_Step :: String
test_Binding_Step = "model://joopringelberg.nl#SynchronisationTestModel$Test_Binding_Step"

test_SetProperty_in_CalculatedProperty :: String
test_SetProperty_in_CalculatedProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test_SetProperty_in_CalculatedProperty"

test_Binding_in_CalculatedProperty :: String
test_Binding_in_CalculatedProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test_Binding_in_CalculatedProperty"

test_Binder_in_CalculatedProperty :: String
test_Binder_in_CalculatedProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test_Binder_in_CalculatedProperty"

test_SetProperty_on_Filler :: String
test_SetProperty_on_Filler = "model://joopringelberg.nl#SynchronisationTestModel$Test_SetProperty_on_Filler"

test_Binding_in_CalculatedRole :: String
test_Binding_in_CalculatedRole = "model://joopringelberg.nl#SynchronisationTestModel$Test_Binding_in_CalculatedRole"

test_SetProperty_in_CalculatedProperty_BindingStep :: String
test_SetProperty_in_CalculatedProperty_BindingStep = "model://joopringelberg.nl#SynchronisationTestModel$Test_SetProperty_in_CalculatedProperty_BindingStep"

test_SetProperty_in_CalculatedProperty_BinderStep :: String
test_SetProperty_in_CalculatedProperty_BinderStep = "model://joopringelberg.nl#SynchronisationTestModel$Test_SetProperty_in_CalculatedProperty_BinderStep"

test_CreateRole_in_CalculatedRole_ContextStep :: String
test_CreateRole_in_CalculatedRole_ContextStep = "model://joopringelberg.nl#SynchronisationTestModel$Test_CreateRole_in_CalculatedRole_ContextStep"

test_CreateRole_in_CalculatedRole_RoleStep :: String
test_CreateRole_in_CalculatedRole_RoleStep = "model://joopringelberg.nl#SynchronisationTestModel$Test_CreateRole_in_CalculatedRole_RoleStep"