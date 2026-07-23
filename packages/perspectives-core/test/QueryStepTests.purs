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

module Test.QueryStepTests
  ( main
  , queryStepSuite
  , queryStepTestModelConfiguration
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..))
import Test.SinglePDRScaffold (ModelTest, SinglePDRModelConfiguration, SinglePDRResults, emptyLogConfiguration, getSinglePDRResults)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  results <- getSinglePDRResults queryStepTestModelConfiguration
  liftEffect $ runTest do
    queryStepSuite results

queryStepSuite :: SinglePDRResults -> TestSuite
queryStepSuite results =
  suite "Query step tests" do
    for_ results \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed") do
          assert ("Test '" <> testName <> "' should succeed") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Test should succeed, but got error: " <> show err) false

queryStepTestModelConfiguration :: SinglePDRModelConfiguration
queryStepTestModelConfiguration =
  { suiteName: "Query step tests"
  , snapshotDirectory: snapshotDirectory
  , testModel
  , indexedTestContext
  , testAppManager
  , testsType
  , testSucceededProperty
  , testNameProperty
  , setupLogConfiguration:
      { pdr:
          [ { topic: TEST, logLevel: Debug }
          ]
      }
  , tests: allTests
  }

testModel :: String
-- testModel = "model://joopringelberg.nl#StateTestModel$1.0"
testModel = "model://joopringelberg.nl#u01vncbjik@1.0"

indexedTestContext :: String
indexedTestContext = "model://joopringelberg.nl#StateTestModel$TestStateApp"

testAppManager :: String
testAppManager = "model://joopringelberg.nl#StateTestModel$TestApp$Manager"

testsType :: String
testsType = "model://joopringelberg.nl#StateTestModel$TestApp$Tests"

testSucceededProperty :: String
testSucceededProperty = "model://joopringelberg.nl#StateTestModel$Test$External$TestSucceeded"

testNameProperty :: String
testNameProperty = "model://joopringelberg.nl#StateTestModel$Test$External$TestName"

snapshotDirectory :: String
snapshotDirectory = "test/pdr-snapshot/layer4"

allTests :: Array ModelTest
allTests =
  [ { testContextTypeName: test_ContextState_RoleStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_ContextStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_PropertyStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_ExternStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_BindingStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_IdentityStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_ModelNameStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_MeStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_TypeOfRoleStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_RoleTypesStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_SpecialisesRoleTypeStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_NotStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_AvailableStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_ComparisonStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_FilledByBinaryStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_FillsBinaryStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_CountSequenceStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleType_TranslateStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_FilledStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_IndexedNameStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleType_RoleTypeIndividualStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_ContextType_ContextTypeIndividualStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_IsInStateStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_RegExStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_RoleState_VariableStep, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_FilledBy_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Fills_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Duration_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_ContextIndividual_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_FillFrom_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Union_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Intersection_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Or_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_And_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Minimum_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Maximum_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_Sum_Step, logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: test_First_Step, logConfiguration: emptyLogConfiguration }
  ]

test_ContextState_RoleStep :: String
test_ContextState_RoleStep = "model://joopringelberg.nl#StateTestModel$Test_ContextState_RoleStep"

test_RoleState_ContextStep :: String
test_RoleState_ContextStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_ContextStep"

test_RoleState_PropertyStep :: String
test_RoleState_PropertyStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_PropertyStep"

test_RoleState_ExternStep :: String
test_RoleState_ExternStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_ExternStep"

test_RoleState_BindingStep :: String
test_RoleState_BindingStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_BindingStep"

test_RoleState_IdentityStep :: String
test_RoleState_IdentityStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_IdentityStep"

test_RoleState_ModelNameStep :: String
test_RoleState_ModelNameStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_ModelNameStep"

test_RoleState_MeStep :: String
test_RoleState_MeStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_MeStep"

test_RoleState_TypeOfRoleStep :: String
test_RoleState_TypeOfRoleStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_TypeOfRoleStep"

test_RoleState_RoleTypesStep :: String
test_RoleState_RoleTypesStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_RoleTypesStep"

test_RoleState_SpecialisesRoleTypeStep :: String
test_RoleState_SpecialisesRoleTypeStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_SpecialisesRoleTypeStep"

test_RoleState_NotStep :: String
test_RoleState_NotStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_NotStep"

test_RoleState_AvailableStep :: String
test_RoleState_AvailableStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_AvailableStep"

test_RoleState_ComparisonStep :: String
test_RoleState_ComparisonStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_ComparisonStep"

test_RoleState_FilledByBinaryStep :: String
test_RoleState_FilledByBinaryStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_FilledByBinaryStep"

test_RoleState_FillsBinaryStep :: String
test_RoleState_FillsBinaryStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_FillsBinaryStep"

test_RoleState_CountSequenceStep :: String
test_RoleState_CountSequenceStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_CountSequenceStep"

test_RoleType_TranslateStep :: String
test_RoleType_TranslateStep = "model://joopringelberg.nl#StateTestModel$Test_RoleType_TranslateStep"

test_RoleState_FilledStep :: String
test_RoleState_FilledStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_FilledStep"

test_RoleState_IndexedNameStep :: String
test_RoleState_IndexedNameStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_IndexedNameStep"

test_RoleType_RoleTypeIndividualStep :: String
test_RoleType_RoleTypeIndividualStep = "model://joopringelberg.nl#StateTestModel$Test_RoleType_RoleTypeIndividualStep"

test_ContextType_ContextTypeIndividualStep :: String
test_ContextType_ContextTypeIndividualStep = "model://joopringelberg.nl#StateTestModel$Test_ContextType_ContextTypeIndividualStep"

test_RoleState_IsInStateStep :: String
test_RoleState_IsInStateStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_IsInStateStep"

test_RoleState_RegExStep :: String
test_RoleState_RegExStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_RegExStep"

test_RoleState_VariableStep :: String
test_RoleState_VariableStep = "model://joopringelberg.nl#StateTestModel$Test_RoleState_VariableStep"

test_FilledBy_Step :: String
test_FilledBy_Step = "model://joopringelberg.nl#StateTestModel$Test_FilledBy_Step"

test_Fills_Step :: String
test_Fills_Step = "model://joopringelberg.nl#StateTestModel$Test_Fills_Step"

test_Duration_Step :: String
test_Duration_Step = "model://joopringelberg.nl#StateTestModel$Test_Duration_Step"

test_ContextIndividual_Step :: String
test_ContextIndividual_Step = "model://joopringelberg.nl#StateTestModel$Test_ContextIndividual_Step"

test_FillFrom_Step :: String
test_FillFrom_Step = "model://joopringelberg.nl#StateTestModel$Test_FillFrom_Step"

test_Union_Step :: String
test_Union_Step = "model://joopringelberg.nl#StateTestModel$Test_Union_Step"

test_Intersection_Step :: String
test_Intersection_Step = "model://joopringelberg.nl#StateTestModel$Test_Intersection_Step"

test_Or_Step :: String
test_Or_Step = "model://joopringelberg.nl#StateTestModel$Test_Or_Step"

test_And_Step :: String
test_And_Step = "model://joopringelberg.nl#StateTestModel$Test_And_Step"

test_Minimum_Step :: String
test_Minimum_Step = "model://joopringelberg.nl#StateTestModel$Test_Minimum_Step"

test_Maximum_Step :: String
test_Maximum_Step = "model://joopringelberg.nl#StateTestModel$Test_Maximum_Step"

test_Sum_Step :: String
test_Sum_Step = "model://joopringelberg.nl#StateTestModel$Test_Sum_Step"

test_First_Step :: String
test_First_Step = "model://joopringelberg.nl#StateTestModel$Test_First_Step"