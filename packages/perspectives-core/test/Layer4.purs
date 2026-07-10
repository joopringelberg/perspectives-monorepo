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

-- | Layer 4 test entry point — single-PDR model-based tests.
-- |
-- | This module is the single-PDR counterpart of Layer3. Where Layer3 requires
-- | two connected PDR instances to test synchronisation, Layer4 runs all tests
-- | within one PDR instance.  The modelled root case TestApp has no Follower
-- | role; it only has a Tests role.
-- |
-- | Test strategy:
-- |   1. Start one PDR instance and load the test model.
-- |   2. For each modelled test case, create a new test context instance and
-- |      fill a Tests role in the root TestApp context with it.
-- |   3. Execute the RunTest context action in the test context.
-- |   4. Poll until the TestSucceeded property on the test's external role has
-- |      been set, then record the result.
-- |
-- | Run with:
-- |
-- |   pnpm run test:layer4

module Test.Layer4 where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for_, traverse)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, new, read, write)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (empty) as OBJ
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.RunAction (runContextAction)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##>))
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (createAndAddRoleInstance, constructContext)
import Perspectives.Logging (ansiRed, infoTest)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.PerspectivesState (defaultRuntimeOptions, disableAllLogging, setTopicLogLevel)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), IndexedContext(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Perspectives.Sidecar.ToStable (toStable)
import Test.PDRInstance (PDRInstance, SynchronisationResult, noBus, pollUntil, pollUntilTestFinishes, runInPDR, testPouchdbUser, withPDR)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  results <- getSinglePDRResults
  liftEffect $ runTest do
    suite "Single-PDR model tests" do
      for_ results \result -> case result of
        Right { testName, testSucceeded } ->
          test (testName <> " should succeed") do
            assert ("Test '" <> testName <> "' should succeed") testSucceeded
        Left { testName, err } ->
          test ("test '" <> testName <> "' failed with error") do
            assert ("Test should succeed, but got error: " <> show err) false

-------------------------------------------------------------------------------
---- COMPUTE AND CACHE ALL RESULTS ONCE
-------------------------------------------------------------------------------
type TopicLogLevelPair =
  { topic :: LogTopic
  , logLevel :: LogLevel
  }

-- | Log-level overrides to apply before running a single test.
type LogConfiguration =
  { pdr :: Array TopicLogLevelPair
  }

emptyLogConfiguration :: LogConfiguration
emptyLogConfiguration =
  { pdr: []
  }

type ModelTest =
  { testContextTypeName :: String
  , logConfiguration :: LogConfiguration
  }

type SinglePDRResults = Array SynchronisationResult

cachedSinglePDRResults :: Ref (Maybe SinglePDRResults)
cachedSinglePDRResults = unsafePerformEffect $ new Nothing

getSinglePDRResults :: Aff SinglePDRResults
getSinglePDRResults = do
  cached <- liftEffect $ read cachedSinglePDRResults
  case cached of
    Just results -> pure results
    Nothing -> do
      results <- withPDR
        (testPouchdbUser "alice")
        defaultRuntimeOptions
        (Just ansiRed)
        noBus
        \pdr -> do
          runInPDR pdr
            ( do
                -- setTopicLogLevel INSTALL Debug
                -- setTopicLogLevel MODEL Debug
                setTopicLogLevel TEST Debug
            -- setTopicLogLevel RESOURCE Trace
            -- setTopicLogLevel STATE Trace
            )

          -- Load the single-PDR test model.
          runInPDR pdr do
            infoTest "Loading test model"
            runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
              $
                addModelToLocalStore_ [ testModel ] (RoleInstance "Ignored")

          -- Wait for the indexed root context to become available after model loading.
          testAppContext <- pollUntil 100 (Milliseconds 100.0)
            "Indexed test context to appear after loading test model"
            ( runInPDR pdr
                do
                  IndexedContext indexedTestContext' <- toStable (IndexedContext indexedTestContext)
                  lookupIndexedContext indexedTestContext'
            )

          -------------------------------------------------------------------------------
          ---- EXECUTE TESTS AND COLLECT RESULTS
          ---- Add one entry to allTests for each test case in the test model.
          -------------------------------------------------------------------------------
          traverse (\t -> executeModelTest pdr testAppContext t.testContextTypeName t.logConfiguration) allTests

      liftEffect $ write (Just results) cachedSinglePDRResults
      pure results

-------------------------------------------------------------------------------
---- RUN A SINGLE TEST AND RETURN THE RESULT
-------------------------------------------------------------------------------
-- | Create a test context instance of the given type, fill a Tests role in the
-- | root TestApp context, execute the RunTest action, and return the result.
-- |
-- | Unlike Layer3's executeModelTest there is only one PDR involved, so:
-- |   * No Follower role is created or awaited.
-- |   * Results are read from the same PDR that runs the test.
executeModelTest :: PDRInstance -> ContextInstance -> String -> LogConfiguration -> Aff SynchronisationResult
executeModelTest pdr testAppContext testContextTypeR logConfiguration = do

  runInPDR pdr do
    for_ logConfiguration.pdr \{ topic, logLevel } -> setTopicLogLevel topic logLevel

  testContextType <- runInPDR pdr
    (toStable (ContextType testContextTypeR))
  testTesterType <- runInPDR pdr
    (toStable (EnumeratedRoleType $ testContextTypeR <> "$Tester"))

  -- Create a new test context instance and fill a Tests role with it.
  theTest <- runInPDR pdr do
    infoTest "Creating test context"
    testAppManager' <- toStable (CalculatedRoleType testAppManager)
    testsType' <- toStable (EnumeratedRoleType testsType)
    runMonadPerspectivesTransaction' shareWithPeers (CR testAppManager')
      do
        result <- runExceptT $ constructContext (Just (ENR testsType'))
          $ ContextSerialization
              { id: Nothing
              , ctype: unwrap testContextType
              , prototype: Nothing
              , rollen: OBJ.empty
              , externeProperties: PropertySerialization OBJ.empty
              }
        case result of
          Left err -> throwError $ error ("Failed to create test context: " <> show err)
          Right testCtx -> do
            void $ createAndAddRoleInstance testsType' (unwrap testAppContext)
              ( RolSerialization
                  { id: Nothing
                  , properties: PropertySerialization OBJ.empty
                  , binding: Just $ buitenRol (unwrap testCtx)
                  }
              )
            pure testCtx

  let testExternalRole = RoleInstance $ buitenRol (unwrap theTest)

  -- Execute the RunTest context action as the Tester role.
  runInPDR pdr
    do
      runMonadPerspectivesTransaction' shareWithPeers (ENR testTesterType)
        ( do
            lift $ infoTest "Executing RunTest action"
            runContextAction (unwrap testTesterType) "RunTest" (unwrap theTest)
        )

  -- Poll until the test has set TestName and TestSucceeded on the external role.
  r <- pollUntilTestFinishes 100 (Milliseconds 100.0)
    "Test to complete with a result"
    ( runInPDR pdr
        do
          testNameProperty' <- toStable (EnumeratedPropertyType testNameProperty)
          mtestName <- testExternalRole ##> getPropertyValues (ENP testNameProperty')
          case mtestName of
            Just (Value testName) -> do
              infoTest ("Test has a name: " <> testName)
              testSucceededProperty' <- toStable (EnumeratedPropertyType testSucceededProperty)
              mtestSucceeded <- testExternalRole ##> getPropertyValues (ENP testSucceededProperty')
              case mtestSucceeded of
                Just (Value testSucceeded) -> pure (Right { testName, testSucceeded: testSucceeded == "true" })
                Nothing -> pure (Left { testName, err: error "TestSucceeded property not found" })
            Nothing -> pure (Left { testName: "unknown testname", err: error "TestName property not found" })
    )

  runInPDR pdr
    ( do
        disableAllLogging
        setTopicLogLevel TEST Debug
    )
  pure r

-------------------------------------------------------------------------------
---- READABLE TYPE NAMES IN THE TEST MODEL
---- Update these constants to match the names used in the test model.
-------------------------------------------------------------------------------
-- | The URI of the single-PDR test model to load. This should be a stable identifier.
testModel :: String
testModel = "model://joopringelberg.nl#u01vncbjik@1.0"

-- | The indexed context name for the root TestApp instance created by the model.
indexedTestContext :: String
indexedTestContext = "model://joopringelberg.nl#StateTestModel$TestStateApp"

-- | The calculated Manager role of the root TestApp.
testAppManager :: String
testAppManager = "model://joopringelberg.nl#StateTestModel$TestApp$Manager"

-- | The Tests role in the root TestApp.
testsType :: String
testsType = "model://joopringelberg.nl#StateTestModel$TestApp$Tests"

-- | The property that records whether a test succeeded ("true"/"false").
testSucceededProperty :: String
testSucceededProperty = "model://joopringelberg.nl#StateTestModel$Test$External$TestSucceeded"

-- | The property that records the human-readable test name.
testNameProperty :: String
testNameProperty = "model://joopringelberg.nl#StateTestModel$Test$External$TestName"

-------------------------------------------------------------------------------
---- THE TESTS
---- Add one entry for each test context type defined in the test model.
-------------------------------------------------------------------------------

allTests :: Array ModelTest
allTests =
  [ { testContextTypeName: test_ContextState_RoleStep, logConfiguration: emptyLogConfiguration }
  ,
    -- { pdr: [ { topic: STATE, logLevel: Trace }
    --         , { topic: RESOURCE, logLevel: Trace }
    --        ]
    -- } },
    { testContextTypeName: test_RoleState_ContextStep, logConfiguration: emptyLogConfiguration }
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
  ]

-- Example test context type name constant:
-- test_Example :: String
-- test_Example = "model://joopringelberg.nl#StateTestModel$Test_Example"

allOn :: Array TopicLogLevelPair
allOn =
  [ { topic: RESOURCE, logLevel: Trace }
  , { topic: DELTA, logLevel: Trace }
  , { topic: STATE, logLevel: Trace }
  , { topic: SYNC, logLevel: Trace }
  , { topic: BROKER, logLevel: Trace }
  , { topic: INSTALL, logLevel: Trace }
  , { topic: TEST, logLevel: Debug }
  , { topic: MODEL, logLevel: Trace }
  , { topic: PERSISTENCE, logLevel: Trace }
  , { topic: QUERY, logLevel: Trace }
  , { topic: AUTH, logLevel: Trace }
  , { topic: UPGRADE, logLevel: Trace }
  , { topic: PARSER, logLevel: Trace }
  , { topic: COMPILER, logLevel: Trace }
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