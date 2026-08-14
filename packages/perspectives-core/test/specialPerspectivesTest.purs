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

-- | Regression test suite for special perspectives:
-- |   * selfOnly properties
-- |   * authorOnly properties
-- |   * selfOnly perspectives
-- |   * authorOnly perspectives
-- |
-- | Uses a three-PDR setup:
-- |   pdrA = Alice  (Manager / Leader)
-- |   pdrB = Bob    (Follower 1 – primary, positive assertions)
-- |   pdrC = Charlie (Follower 2 – secondary, negative assertions)
-- |
-- | Each test verifies two things:
-- |   1. Positive: Bob's PDR has observed the expected effect (TestSucceeded = true).
-- |   2. Negative: Charlie's PDR has NOT observed the restricted data.

module Test.SpecialPerspectivesTest
  ( getSpecialPerspectivesResults
  , specialPerspectivesSuite
  , specialPerspectivesTestModelConfiguration
  ) where

import Prelude

import Data.Array (filter, join, length, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, new, read, write)
import Effect.Unsafe (unsafePerformEffect)
import Perspectives.CoreTypes ((##=))
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..))
import Perspectives.Sidecar.ToStable (toStable)
import Test.Layer3Scaffold (LogConfiguration, SpecialPerspectivesModelConfiguration, SpecialPerspectivesResults, SpecialPerspectivesTestSpec, TestRunner(..), emptyLogConfiguration)
import Test.Layer3Scaffold (getSpecialPerspectivesResults) as Layer3Scaffold
import Test.PDRInstance.Types (PDRInstance, runInPDR)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)

cachedSpecialPerspectivesResults :: Ref (Maybe SpecialPerspectivesResults)
cachedSpecialPerspectivesResults = unsafePerformEffect $ new Nothing

getSpecialPerspectivesResults :: Aff SpecialPerspectivesResults
getSpecialPerspectivesResults = do
  cached <- liftEffect $ read cachedSpecialPerspectivesResults
  case cached of
    Just results -> pure results
    Nothing -> do
      results <- Layer3Scaffold.getSpecialPerspectivesResults cachedSpecialPerspectivesResults specialPerspectivesTestModelConfiguration
      liftEffect $ write (Just results) cachedSpecialPerspectivesResults
      pure results

specialPerspectivesSuite :: SpecialPerspectivesResults -> TestSuite
specialPerspectivesSuite results =
  suite "Special perspectives tests" do
    for_ results \result -> case result of
      Right { testName, testSucceeded, negativeSucceeded } -> do
        test (testName <> " – positive (Bob's PDR)") do
          assert ("Bob should see that test '" <> testName <> "' succeeded") testSucceeded
        test (testName <> " – negative (Charlie's PDR)") do
          assert ("Charlie should NOT observe the restricted data for '" <> testName <> "'") negativeSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Test should succeed, but got error: " <> show err) false

specialPerspectivesTestModelConfiguration :: SpecialPerspectivesModelConfiguration
specialPerspectivesTestModelConfiguration =
  { suiteName: "Special perspectives tests"
  , snapshotDirAlice: "test/pdr-snapshot/special-perspectives/alice"
  , snapshotDirBob: "test/pdr-snapshot/special-perspectives/bob"
  , snapshotDirCharlie: "test/pdr-snapshot/special-perspectives/charlie"
  , testModel
  , indexedTestContext
  , testAppManager
  , testAppFollowerType
  , testsType
  , testSucceededProperty
  , testNameProperty
  , setupLogConfiguration:
      { pdrA: []
      , pdrB: []
      }
  , tests: allTests
  }

-------------------------------------------------------------------------------
---- NECESSARY READABLE TYPE NAMES IN model://joopringelberg.nl#SpecialPerspectivesTestModel
-------------------------------------------------------------------------------
testModel :: String
-- Replace the placeholder below with the actual CUID once the model is compiled
-- and uploaded to the model store.
-- testModel = "model://joopringelberg.nl#SpecialPerspectivesTestModel@1.0"
testModel = "model://joopringelberg.nl#SpecialPerspectivesTestModel@1.0"

indexedTestContext :: String
indexedTestContext = "model://joopringelberg.nl#SpecialPerspectivesTestModel$SpecialPerspectivesTestApp"

testAppManager :: String
testAppManager = "model://joopringelberg.nl#SpecialPerspectivesTestModel$TestApp$Manager"

testAppFollowerType :: String
testAppFollowerType = "model://joopringelberg.nl#SpecialPerspectivesTestModel$TestApp$Follower"

testsType :: String
testsType = "model://joopringelberg.nl#SpecialPerspectivesTestModel$TestApp$Tests"

testSucceededProperty :: String
testSucceededProperty = "model://joopringelberg.nl#SpecialPerspectivesTestModel$Test$External$TestSucceeded"

testNameProperty :: String
testNameProperty = "model://joopringelberg.nl#SpecialPerspectivesTestModel$Test$External$TestName"

-------------------------------------------------------------------------------
---- THE TESTS
-------------------------------------------------------------------------------

allTests :: Array SpecialPerspectivesTestSpec
allTests =
  [ { testContextTypeName: test_SelfOnly_Property
    , logConfiguration: emptyLogConfiguration
    , testRunner: RunByLeader
    , negativeCheck: negativeSelfOnlyProperty test_SelfOnly_Property
    }
  , { testContextTypeName: test_AuthorOnly_Property
    , logConfiguration: emptyLogConfiguration
    , testRunner: RunByFollower1
    , negativeCheck: negativeAuthorOnlyProperty test_AuthorOnly_Property
    }
  , { testContextTypeName: test_SelfOnly_Perspective
    , logConfiguration: emptyLogConfiguration
    , testRunner: RunByLeader
    , negativeCheck: negativeSelfOnlyPerspective test_SelfOnly_Perspective
    }
  , { testContextTypeName: test_AuthorOnly_Perspective
    , logConfiguration: emptyLogConfiguration
    , testRunner: RunByFollower1
    , negativeCheck: negativeAuthorOnlyPerspective test_AuthorOnly_Perspective
    }
  ]

test_SelfOnly_Property :: String
test_SelfOnly_Property = "model://joopringelberg.nl#SpecialPerspectivesTestModel$Test_SelfOnly_Property"

test_AuthorOnly_Property :: String
test_AuthorOnly_Property = "model://joopringelberg.nl#SpecialPerspectivesTestModel$Test_AuthorOnly_Property"

test_SelfOnly_Perspective :: String
test_SelfOnly_Perspective = "model://joopringelberg.nl#SpecialPerspectivesTestModel$Test_SelfOnly_Perspective"

test_AuthorOnly_Perspective :: String
test_AuthorOnly_Perspective = "model://joopringelberg.nl#SpecialPerspectivesTestModel$Test_AuthorOnly_Perspective"

-------------------------------------------------------------------------------
---- NEGATIVE-CHECK FUNCTIONS
---- Each function is called in Charlie's PDR (pdrC) after the positive check
---- passes and must return `true` when Charlie correctly CANNOT observe the
---- restricted data.
-------------------------------------------------------------------------------

-- | selfOnly property P on Follower.
-- | Leader set P = 1 on ALL Follower instances.
-- | Because P is selfonly, each Follower can see only their own P value.
-- | In Charlie's PDR the count of Follower instances that expose a visible P
-- | value must be less than the total number of Follower instances
-- | (i.e. 1 out of 2 – only Charlie's own).
negativeSelfOnlyProperty :: String -> PDRInstance -> ContextInstance -> Aff Boolean
negativeSelfOnlyProperty testContextTypeName pdrC testCtx = do
  let followerTypeStr = testContextTypeName <> "$Follower"
  let pPropertyTypeStr = testContextTypeName <> "$Follower$P"
  followerType <- runInPDR pdrC (toStable (EnumeratedRoleType followerTypeStr))
  pPropertyType <- runInPDR pdrC (toStable (EnumeratedPropertyType pPropertyTypeStr))
  runInPDR pdrC do
    followers <- testCtx ##= getEnumeratedRoleInstances followerType
    psVisible <- traverse
      (\f -> do
        vals <- f ##= getPropertyValues (ENP pPropertyType)
        pure (not (null vals)))
      followers
    let countVisible = length (filter identity psVisible)
    -- With selfonly, Charlie should see P on at most 1 Follower (her own, not Bob's).
    pure (countVisible < length followers)

-- | authorOnly property P on TestRole1.
-- | Bob created TestRole1 and set P.  Because P is authoronly it was never
-- | serialised or sent to Charlie.  In Charlie's PDR no P value must be visible.
negativeAuthorOnlyProperty :: String -> PDRInstance -> ContextInstance -> Aff Boolean
negativeAuthorOnlyProperty testContextTypeName pdrC testCtx = do
  let testRole1TypeStr = testContextTypeName <> "$TestRole1"
  let pPropertyTypeStr = testContextTypeName <> "$TestRole1$P"
  testRole1Type <- runInPDR pdrC (toStable (EnumeratedRoleType testRole1TypeStr))
  pPropertyType <- runInPDR pdrC (toStable (EnumeratedPropertyType pPropertyTypeStr))
  runInPDR pdrC do
    roles <- testCtx ##= getEnumeratedRoleInstances testRole1Type
    if null roles
      then pure true  -- Charlie cannot even see the role (also acceptable)
      else do
        allPs <- traverse
          (\r -> r ##= getPropertyValues (ENP pPropertyType))
          roles
        let anyPVisible = not (null (join allPs))
        pure (not anyPVisible)

-- | selfOnly perspective on Follower (property Q).
-- | Leader set Q = 1 on ALL Followers.  Because the Follower-on-Follower
-- | perspective is selfonly, Charlie's PDR never received Bob's role instance.
-- | The number of Follower instances visible to Charlie must be less than 2.
negativeSelfOnlyPerspective :: String -> PDRInstance -> ContextInstance -> Aff Boolean
negativeSelfOnlyPerspective testContextTypeName pdrC testCtx = do
  let followerTypeStr = testContextTypeName <> "$Follower"
  followerType <- runInPDR pdrC (toStable (EnumeratedRoleType followerTypeStr))
  runInPDR pdrC do
    followers <- testCtx ##= getEnumeratedRoleInstances followerType
    -- Charlie should only have her own Follower instance (1 entry), not Bob's.
    pure (length followers < 2)

-- | authorOnly perspective on PrivateRole (property R).
-- | Bob created PrivateRole.  Because the perspective is authoronly the role was
-- | never serialised or sent to Charlie.  In Charlie's PDR PrivateRole must be absent.
negativeAuthorOnlyPerspective :: String -> PDRInstance -> ContextInstance -> Aff Boolean
negativeAuthorOnlyPerspective testContextTypeName pdrC testCtx = do
  let privateRoleTypeStr = testContextTypeName <> "$PrivateRole"
  privateRoleType <- runInPDR pdrC (toStable (EnumeratedRoleType privateRoleTypeStr))
  runInPDR pdrC do
    roles <- testCtx ##= getEnumeratedRoleInstances privateRoleType
    pure (null roles)
