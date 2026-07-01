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

-- | Layer 3 test entry point — synchronisation tests with a stubbed AMQP transport.
-- |
-- | Prerequisites (before these tests can run):
-- |   1. All Layer 2 prerequisites (in-memory PouchDB via `persistenceAPI.node.js`).
-- |   2. A stub AMQP transport that replaces `@stomp/stompjs` with an in-process
-- |      message bus.  The stub must implement the same interface as the real
-- |      Stomp client and be injectable via the PDR state record.
-- |
-- | Test strategy:
-- |   1. Start two PDR instances (PDR-A and PDR-B) in the same Node.js process,
-- |      each with its own in-memory PouchDB and the stub AMQP transport.
-- |   2. Apply a mutation via PDR-A's API.
-- |   3. Assert that PDR-B receives the expected `TransactionForPeer` and that
-- |      its database reflects the change.
-- |
-- | This covers the full synchronisation path (delta generation, signing,
-- | transmission, application) without requiring a real RabbitMQ broker.
-- |
-- | Run with:
-- |
-- |   pnpm run test:layer3
-- |
-- | Smoke tests for the PDR instance scaffold are included below.
-- | They deliberately use `testOnly` so that they do NOT run automatically in CI:
-- | each test calls `startPDRInstance`, which invokes `setupUser` and downloads
-- | essential Perspectives models from https://perspectives.domains — this requires
-- | network access that is not available in the standard CI environment.
-- | To run these tests manually, pass `--only` to the test runner or select them
-- | from the test menu.

module Test.Layer3 where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array (null)
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
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##=), (##>))
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (createAndAddRoleInstance, constructContext)
import Perspectives.Instances.ObjectGetters (binding, binding_, getEnumeratedRoleInstances)
import Perspectives.Logging (ansiMagenta, ansiRed, infoTest)
import Perspectives.ModelDependencies (sysMe, sysUser)
import Perspectives.Names (lookupIndexedContext, lookupIndexedRole)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (tryGetPerspectContext, tryGetPerspectRol)
import Perspectives.PerspectivesState (defaultRuntimeOptions, setTopicLogLevel, getPerspectivesUser)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, PerspectivesUser, RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), IndexedContext(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Perspectives.Sidecar.ToStable (toStable)
import Test.PDRInstance (PDRInstance, SynchronisationResult, connectPDRs, noBus, pollUntil, pollUntilTestFinishes, runInPDR, testPouchdbUser, withPDR, withTwoPDRs)
import Test.Unit (TestSuite, suite, suiteSkip, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  results <- getSynchronisationResults
  liftEffect $ runTest do
    scaffoldTests
    suite "Synchronisation tests" do
      for_ results \result -> case result of
        Right { testName, testSucceeded } ->
          test (testName <> " should succeed in Bob's PDR") do
            assert ("Bob should see that test '" <> testName <> "' succeeded") testSucceeded
        Left {testName, err} ->
          test ("test '" <> testName <> "' failed with error") do
            assert ("Bob should see that the test succeeded, but got error: " <> show err) false

scaffoldTests :: TestSuite
scaffoldTests = suiteSkip "PDRInstance scaffold" do

    -- | Smoke test: start one PDR instance, verify the system identifier matches
    -- | the value we passed in, then shut down.
    test "start a single PDR instance and read its system identifier" do
      let user = testPouchdbUser "alice"
      withPDR user defaultRuntimeOptions (Just ansiRed) noBus \pdr -> do
        sysId <- runInPDR pdr getSystemIdentifier
        assert "system identifier should equal alice_macbook" (sysId == "alice_macbook")

    -- | Smoke test: start two PDR instances in the same process, verify that
    -- | they have distinct system identifiers.
    test "start two PDR instances with distinct identifiers" do
      withTwoPDRs
        (testPouchdbUser "alice")
        defaultRuntimeOptions
        (Just ansiRed)
        (testPouchdbUser "bob")
        defaultRuntimeOptions
        (Just ansiMagenta)
        \pdrA pdrB -> do
          sysA <- runInPDR pdrA getSystemIdentifier
          sysB <- runInPDR pdrB getSystemIdentifier
          assert "PDR-A system identifier should equal alice_macbook" (sysA == "alice_macbook")
          assert "PDR-B system identifier should equal bob_macbook" (sysB == "bob_macbook")
          assert "PDR-A and PDR-B should have distinct identifiers" (sysA /= sysB)

    test "start two PDR instances and connect them" do
      withTwoPDRs
        (testPouchdbUser "alice")
        defaultRuntimeOptions
        (Just ansiRed)
        (testPouchdbUser "bob")
        defaultRuntimeOptions
        (Just ansiMagenta)
        \pdrA pdrB -> do
          runInPDR pdrA
            ( do
                -- setTopicLogLevel BROKER Debug
                -- setTopicLogLevel INSTALL Trace
                -- setTopicLogLevel SYNC Trace
                setTopicLogLevel TEST Debug
                -- setTopicLogLevel RESOURCE Debug
                setTopicLogLevel DELTA Trace
            )
          runInPDR pdrB
            ( do
                setTopicLogLevel BROKER Debug
                -- setTopicLogLevel INSTALL Trace
                setTopicLogLevel SYNC Trace
                setTopicLogLevel TEST Debug
                setTopicLogLevel RESOURCE Trace
                -- setTopicLogLevel DELTA Warn
            )
          connectPDRs pdrA pdrB
          -- Two Persons instances.
          -- Controleer op PerspectivesUsers in plaats daarvan.
          malice <- runInPDR pdrA do
            muser <- lookupIndexedRole sysMe
            case muser of
              Just user -> binding_ user
              Nothing -> pure Nothing
          mbob <- runInPDR pdrB do
            muser <- lookupIndexedRole sysMe
            case muser of
              Just user -> binding_ user
              Nothing -> pure Nothing
          case malice, mbob of
            Just alice, Just bob -> do
              maliceForBob <- runInPDR pdrB $ tryGetPerspectRol alice
              mbobForAlice <- runInPDR pdrA $ tryGetPerspectRol bob
              case maliceForBob, mbobForAlice of
                Just _, Just _ -> assert "Both PDRs should have each others' Person instance" true
                Just _, Nothing -> assert "Bobs' Person instance should be visible for Alice, too" false
                _, _ -> assert "Both PDRs should have each others' Person instance" false
            _, _ -> assert "Both PDRs should have a `me` instance" false

-------------------------------------------------------------------------------
---- COMPUTE AND CACHE ALL RESULTS ONCE
-------------------------------------------------------------------------------
type SynchronisationResults = Array SynchronisationResult

cachedSynchronisationResults :: Ref (Maybe SynchronisationResults)
cachedSynchronisationResults = unsafePerformEffect $ new Nothing

getSynchronisationResults :: Aff SynchronisationResults
getSynchronisationResults = do
  cached <- liftEffect $ read cachedSynchronisationResults
  case cached of
    Just results -> pure results
    Nothing -> do
      results <- withTwoPDRs
        (testPouchdbUser "alice")
        defaultRuntimeOptions
        (Just ansiRed)
        (testPouchdbUser "bob")
        defaultRuntimeOptions
        (Just ansiMagenta)
        \pdrA pdrB -> do
          connectPDRs pdrA pdrB

          runInPDR pdrA
            ( do
                -- setTopicLogLevel BROKER Debug
                -- setTopicLogLevel INSTALL Debug
                -- setTopicLogLevel SYNC Info
                setTopicLogLevel TEST Debug
                -- setTopicLogLevel RESOURCE Trace
                -- setTopicLogLevel STATE Trace
            )
          runInPDR pdrB
            ( do
                -- setTopicLogLevel BROKER Debug
                -- setTopicLogLevel INSTALL Trace
                -- setTopicLogLevel MODEL Debug
                -- setTopicLogLevel SYNC Trace
                setTopicLogLevel TEST Debug
            -- setTopicLogLevel RESOURCE Trace
            -- setTopicLogLevel DELTA Trace
            )

          -- Get Alice's and Bob's PerspectivesUsers instances for later use.
          alice <- runInPDR pdrA getPerspectivesUser
          bob <- runInPDR pdrB getPerspectivesUser

          -- Alice loads test model in pdrA.
          runInPDR pdrA do
            infoTest "Alice Loads test model in PDRA"
            runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
              $
                addModelToLocalStore_ [ testModel ] (RoleInstance "Ignored")

          testAppContextA <- pollUntil 100 (Milliseconds 100.0)
            "Indexed test context to appear in pdrA after loading test model"
            ( runInPDR pdrA
                do
                  IndexedContext indexedTestContext' <- toStable (IndexedContext indexedTestContext)
                  lookupIndexedContext indexedTestContext'
            )

          -- Alice gives Bob the role Follower in the App.
          runInPDR pdrA do
            infoTest "Alice gives Bob the role Follower in the App in PDRA"
            testAppManager' <- toStable (CalculatedRoleType testAppManager)
            testAppFollowerType' <- toStable (EnumeratedRoleType testAppFollowerType)
            runMonadPerspectivesTransaction' shareWithPeers (CR testAppManager')
              do
                void $ createAndAddRoleInstance
                  testAppFollowerType'
                  (unwrap testAppContextA)
                  ( RolSerialization
                      { id: Nothing
                      , properties: PropertySerialization OBJ.empty
                      , binding: Just (unwrap bob)
                      }
                  )

          void $ pollUntil 100 (Milliseconds 100.0)
            "Alice checks that the Follower role has been constructed in pdrA"
            ( runInPDR pdrA do
                infoTest "Alice checks that the Follower role has been constructed and filled in pdrA"
                testAppFollowerType' <- toStable (EnumeratedRoleType testAppFollowerType)
                roles <- testAppContextA ##= getEnumeratedRoleInstances testAppFollowerType' >=> binding
                if null roles then pure Nothing
                else pure (Just roles)
            )
          let runATest = \testContextTypeName -> executeModelTest pdrA pdrB testAppContextA alice bob testContextTypeName
          -------------------------------------------------------------------------------
          ---- EXECUTE TESTS AND COLLECT RESULTS
          ---- Add a call for each test in model://joopringelberg.nl#SynchronisationTestModel
          -------------------------------------------------------------------------------
          traverse runATest 
            [ test_CreateRole
            , test_SetProperty
            , test_BindRole
            , test_BindRole_toContext
            ]
          

      liftEffect $ write (Just results) cachedSynchronisationResults
      pure results

-------------------------------------------------------------------------------
---- RUN A SINGLE TEST AND RETURN THE RESULT
-------------------------------------------------------------------------------
executeModelTest :: PDRInstance -> PDRInstance -> ContextInstance -> PerspectivesUser -> PerspectivesUser -> String -> Aff SynchronisationResult
executeModelTest pdrA pdrB testAppContextA alice bob testContextTypeR = do

  -- runInPDR pdrA
  --   ( do
  --       setTopicLogLevel RESOURCE Trace
  --       setTopicLogLevel DELTA Trace
  --       setTopicLogLevel STATE Trace
  --       setTopicLogLevel SYNC Trace
  --       setTopicLogLevel BROKER Trace
  --   )
  -- runInPDR pdrB
  --   ( do
  --       setTopicLogLevel RESOURCE Trace
  --       setTopicLogLevel STATE Trace
  --       setTopicLogLevel BROKER Trace
  --       setTopicLogLevel SYNC Trace
  --   )
  
  testContextType <- runInPDR pdrA
    (toStable (ContextType testContextTypeR))
  testFollowerType <- runInPDR pdrA
    (toStable (EnumeratedRoleType $ testContextTypeR <> "$Follower"))
  testLeaderType <- runInPDR pdrA
    (toStable (EnumeratedRoleType $ testContextTypeR <> "$Leader"))

  -- Alice creates a specific test context and fills a new TestApp$Tests role with it.
  -- The "Test" aspect context creates both the Leader and the Follower role, 
  -- and binds Alice to the Leader and Bob to the Follower. 
  theTest <- runInPDR pdrA do
    infoTest "Alice creates a test in PDRA"
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
          Right test -> do
            void $ createAndAddRoleInstance testsType' (unwrap testAppContextA)
              ( RolSerialization
                  { id: Nothing
                  , properties: PropertySerialization OBJ.empty
                  , binding: Just $ buitenRol (unwrap test)
                  }
              )
            pure test

  testExternalRole <- pure $ RoleInstance $ buitenRol (unwrap theTest)

  -- Wait for Bob to have the Follower role in the test in pdrB.
  void $ pollUntil 100 (Milliseconds 100.0)
    "Bob to have the test and the Follower role in it in pdrB"
    ( runInPDR pdrB do
        mtheTest <- tryGetPerspectContext theTest
        case mtheTest of
          Nothing -> pure Nothing
          Just _ -> do
            infoTest "Bob has verified that he has the test context in PDRB"
            -- NOTICE we check for the specialised Follower role, not the Aspect Follower role.
            roles <- theTest ##= getEnumeratedRoleInstances testFollowerType
            if null roles then pure Nothing
            else do
              infoTest "Bob has verified that he has the Follower role in the test in PDRB"
              pure (Just roles)
    )

  -- Alice executes the test, bringing the test role in state Executed.
  runInPDR pdrA
    do 
      runMonadPerspectivesTransaction' shareWithPeers (ENR testLeaderType)
        (do
          lift $ infoTest "Alice executes a test in PDRA"
          runContextAction (unwrap testLeaderType) "RunTest" (unwrap theTest))

  ( pollUntilTestFinishes 100 (Milliseconds 100.0)
      "Bob to have a value for the test to succeed in pdrB"
      ( runInPDR pdrB $
          do
            testNameProperty' <- toStable (EnumeratedPropertyType testNameProperty)
            mtestName <- testExternalRole ##> getPropertyValues (ENP testNameProperty')
            case mtestName of
              Just (Value testName) -> do
                infoTest ("Bob sees that the test has a name: " <> show testName)
                -- Get the value of Enumerated property TestSucceeded of the external role of the test.
                testSucceededProperty' <- toStable (EnumeratedPropertyType testSucceededProperty)
                mtestSucceeded <- testExternalRole ##> getPropertyValues (ENP testSucceededProperty')
                case mtestSucceeded of
                  Just (Value testSucceeded) -> pure (Right { testName, testSucceeded: testSucceeded == "true"})
                  Nothing -> pure (Left { testName, err: error "TestSucceeded property not found" })
              Nothing -> pure (Left { testName: "unknown testname", err: error "TestName property not found" })
      )
    )

-------------------------------------------------------------------------------
---- NECESSARY READABLE TYPE NAMES IN model://joopringelberg.nl#SynchronisationTestModel
-------------------------------------------------------------------------------
testModel :: String
-- testModel = "model://joopringelberg.nl#SynchronisationTestModel@2.0"
testModel = "model://joopringelberg.nl#hj1bh3wydo@2.0"

indexedTestContext :: String
indexedTestContext = "model://joopringelberg.nl#SynchronisationTestModel$TestSyncApp"

testAppManager :: String
testAppManager = "model://joopringelberg.nl#SynchronisationTestModel$TestApp$Manager"

testAppLeaderType :: String
testAppLeaderType = "model://joopringelberg.nl#SynchronisationTestModel$Test$Leader"

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

test_CreateRole :: String
test_CreateRole = "model://joopringelberg.nl#SynchronisationTestModel$Test_CreateRole"

test_SetProperty :: String
test_SetProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test_SetProperty"

test_BindRole :: String
test_BindRole = "model://joopringelberg.nl#SynchronisationTestModel$Test_BindRole_toRole"

test_BindRole_toContext :: String
test_BindRole_toContext = "model://joopringelberg.nl#SynchronisationTestModel$Test_BindRole_toContext"

