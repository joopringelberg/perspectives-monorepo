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

module Test.RunAction where

import Prelude

import Data.Time.Duration (Milliseconds(..))

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..))
import Test.SinglePDRScaffold (ModelTest, SinglePDRModelConfiguration, SinglePDRResults, TestModelLoadMethod(..), LogConfiguration, emptyLogConfiguration, getSinglePDRResults)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

-- Run with:
-- pnpm run test:runAction

main :: Effect Unit
main = launchAff_ do
  results <- getSinglePDRResults runActionCompileTestModelConfiguration
  liftEffect $ runTest do
    runActionSuite results

runActionSuite :: SinglePDRResults -> TestSuite
runActionSuite results =
  suite "Run action tests" do
    for_ results \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed") do
          assert ("Test '" <> testName <> "' should succeed") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Test should succeed, but got error: " <> show err) false

runActionConfiguration :: SinglePDRModelConfiguration
runActionConfiguration =
  { suiteName: "Run action tests"
  , snapshotDirectory: runActionSnapshotDirectory
  , outputSnapshotDirectory: Nothing
  , testModel: runActionTestModel
  , testModelLoadMethods: [ LoadModelFromRepository { modelUri: runActionTestModel } ]
  , indexedTestContext: "model://joopringelberg.nl#TestRunAction$TestRunAction"
  , testAppManager: "model://joopringelberg.nl#TestRunAction$TestApp$Manager"
  , testsType: "model://joopringelberg.nl#TestRunAction$TestApp$Tests"
  , testSucceededProperty: "model://joopringelberg.nl#TestRunAction$Test$External$TestSucceeded"
  , testNameProperty: "model://joopringelberg.nl#TestRunAction$Test$External$TestName"
  , testTimeLimit: Milliseconds 180000.0
  , setupLogConfiguration:
      { pdr:
          [ { topic: TEST, logLevel: Trace }
          , { topic: INSTALL, logLevel: Trace }
          ]
      }
  , tests: runActionTests
  }

runActionCompileTestModelConfiguration :: SinglePDRModelConfiguration
runActionCompileTestModelConfiguration =
  runActionConfiguration
    { suiteName = "Run action tests (compile)"
    , testModelLoadMethods =
        [ CompileModelFromSource
            { modelUri: runActionTestModel
            , sourcePath: "src/model/testRunAction@1.0.arc"
            , modelUriReadable: "model://joopringelberg.nl#TestRunAction@1.0"
            , basedOnVersion: Nothing
            }
        ]
    }

runActionTestModel :: String
-- Fresh model CUID for model://joopringelberg.nl#TestRunAction@1.0.
runActionTestModel = "model://joopringelberg.nl#bintlf0se3@1.0"

runActionSnapshotDirectory :: String
runActionSnapshotDirectory = "test/pdr-snapshot/universe/alice"

runActionTests :: Array ModelTest
runActionTests =
  [ { testContextTypeName: "model://joopringelberg.nl#TestRunAction$TwoContextActionsForSameUser", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#TestRunAction$TwoRoleActionsForSameUser", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#TestRunAction$RoleActionForMultipleObjects", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#TestRunAction$TwoContextActionsForDifferentUsers", logConfiguration: emptyLogConfiguration }
  , { testContextTypeName: "model://joopringelberg.nl#TestRunAction$TwoContextActionsWithOnceSettledClausesForSameUser", logConfiguration: emptyLogConfiguration }
  ]

debugConfiguration :: LogConfiguration
debugConfiguration =
  { pdr:
      [ { topic: TEST, logLevel: Trace }
      , { topic: RESOURCE, logLevel: Trace }
      , { topic: STATE, logLevel: Trace }
      , { topic: INSTALL, logLevel: Trace }
      , { topic: MODEL, logLevel: Debug }
      ]
  }
