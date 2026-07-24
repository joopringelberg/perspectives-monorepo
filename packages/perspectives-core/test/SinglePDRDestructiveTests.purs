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

module Test.SinglePDRDestructiveTests
  ( main
  , singlePDRDestructiveSuite
  , singlePDRDestructiveTestModelConfiguration
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..))
import Test.SinglePDRScaffold (ModelTest, SinglePDRModelConfiguration, SinglePDRResults, getSinglePDRResults, emptyLogConfiguration)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  results <- getSinglePDRResults singlePDRDestructiveTestModelConfiguration
  liftEffect $ runTest do
    singlePDRDestructiveSuite results

singlePDRDestructiveSuite :: SinglePDRResults -> TestSuite
singlePDRDestructiveSuite results =
  suite "Single-PDR destructive tests" do
    for_ results \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed") do
          assert ("Test '" <> testName <> "' should succeed") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Test should succeed, but got error: " <> show err) false

singlePDRDestructiveTestModelConfiguration :: SinglePDRModelConfiguration
singlePDRDestructiveTestModelConfiguration =
  { suiteName: "Single-PDR destructive tests"
  , snapshotDirectory: destructiveSnapshotDirectory
  , testModel: destructiveTestModel
  , indexedTestContext: destructiveIndexedTestContext
  , testAppManager: destructiveTestAppManager
  , testsType: destructiveTestsType
  , testSucceededProperty: destructiveTestSucceededProperty
  , testNameProperty: destructiveTestNameProperty
  , setupLogConfiguration:
      { pdr:
          [ { topic: TEST, logLevel: Debug }
          , { topic: RESOURCE, logLevel: Trace }
          , { topic: STATE, logLevel: Trace }
          ]
      }
  , tests: destructiveTests
  }

destructiveTestModel :: String
destructiveTestModel = "model://joopringelberg.nl#avsm5dpqjk@2.0"

destructiveIndexedTestContext :: String
destructiveIndexedTestContext = "model://joopringelberg.nl#SinglePDRDestructiveTests$SinglePDRDestructiveTestApp"

destructiveTestAppManager :: String
destructiveTestAppManager = "model://joopringelberg.nl#SinglePDRDestructiveTests$TestApp$Manager"

destructiveTestsType :: String
destructiveTestsType = "model://joopringelberg.nl#SinglePDRDestructiveTests$TestApp$Tests"

destructiveTestSucceededProperty :: String
destructiveTestSucceededProperty = "model://joopringelberg.nl#SinglePDRDestructiveTests$Test$External$TestSucceeded"

destructiveTestNameProperty :: String
destructiveTestNameProperty = "model://joopringelberg.nl#SinglePDRDestructiveTests$Test$External$TestName"

destructiveSnapshotDirectory :: String
destructiveSnapshotDirectory = "test/pdr-snapshot/destructive"

destructiveTests :: Array ModelTest
destructiveTests = 
  [ 
  --   { testContextTypeName: test_RemoveRole, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_DeleteProperty, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_RemoveProperty, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_RemoveOnePropertyValue, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_DeleteTwoRoles, logConfiguration: emptyLogConfiguration }
  -- , { testContextTypeName: test_RemoveOneRoleInstance, logConfiguration: emptyLogConfiguration }
   { testContextTypeName: test_RemoveRoleFiller, logConfiguration: emptyLogConfiguration }
  ]

test_RemoveRole :: String
test_RemoveRole = "model://joopringelberg.nl#SinglePDRDestructiveTests$Test_RemoveRole"

test_DeleteProperty :: String
test_DeleteProperty = "model://joopringelberg.nl#SinglePDRDestructiveTests$Test_DeleteProperty"

test_RemoveProperty :: String
test_RemoveProperty = "model://joopringelberg.nl#SinglePDRDestructiveTests$Test_RemoveProperty"

test_RemoveOnePropertyValue :: String
test_RemoveOnePropertyValue = "model://joopringelberg.nl#SinglePDRDestructiveTests$Test_RemoveOnePropertyValue"

test_DeleteTwoRoles :: String
test_DeleteTwoRoles = "model://joopringelberg.nl#SinglePDRDestructiveTests$Test_DeleteTwoRoles"

test_RemoveOneRoleInstance :: String
test_RemoveOneRoleInstance = "model://joopringelberg.nl#SinglePDRDestructiveTests$Test_RemoveOneRoleInstance"

test_RemoveRoleFiller :: String
test_RemoveRoleFiller = "model://joopringelberg.nl#SinglePDRDestructiveTests$Test_RemoveRoleFiller"