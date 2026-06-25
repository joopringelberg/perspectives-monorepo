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
-- |
-- | TODO: Enable sync suites below once the stub AMQP transport is implemented.
-- |       See docsources/nodejs-testing-architecture.md §3 for the design.

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
import Effect (Effect)
import Effect.Aff (Aff, error)
import Foreign.Object (empty) as OBJ
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.RunAction (runContextAction)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##=), (##>>), (##>))
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
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Test.PDRInstance (PDRInstance, connectPDRs, noBus, pollUntil, runInPDR, testPouchdbUser, withPDR, withTwoPDRs)
import Test.Unit (suite, test, testOnly)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "PDRInstance scaffold" do

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
                setTopicLogLevel BROKER Debug
                setTopicLogLevel INSTALL Trace
                setTopicLogLevel SYNC Trace
                setTopicLogLevel TEST Debug
                setTopicLogLevel RESOURCE Debug
                setTopicLogLevel DELTA Warn
            )
          runInPDR pdrB
            ( do
                setTopicLogLevel BROKER Debug
                setTopicLogLevel INSTALL Trace
                setTopicLogLevel SYNC Trace
                setTopicLogLevel TEST Debug
                setTopicLogLevel RESOURCE Debug
                setTopicLogLevel DELTA Warn
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

    testOnly "start two PDR instances and load testmodel" do
      withTwoPDRs
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

          testCtx <- pollUntil 100 (Milliseconds 100.0)
            "Indexed test context to appear in pdrA after loading test model"
            ( runInPDR pdrA
                (lookupIndexedContext indexedTestContext)
            )

          -- Alice gives Bob the role Follower in the App.
          runInPDR pdrA do
            infoTest "Alice gives Bob the role Follower in the App in PDRA"
            runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType testLeaderType)
              $ do
                void $ createAndAddRoleInstance
                  (EnumeratedRoleType testAppFollowerType)
                  (unwrap testCtx)
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
                roles <- testCtx ##= getEnumeratedRoleInstances (EnumeratedRoleType testAppFollowerType) >=> binding
                if null roles then pure Nothing
                else pure (Just roles)
            )

          runInPDR pdrA
            ( do
                setTopicLogLevel RESOURCE Trace
                setTopicLogLevel STATE Trace
                setTopicLogLevel SYNC Trace
                setTopicLogLevel BROKER Trace
            )
          runInPDR pdrB
            ( do
                setTopicLogLevel RESOURCE Trace
                -- setTopicLogLevel STATE Trace
                setTopicLogLevel BROKER Trace
            )
          
          executeModelTest pdrA pdrB testCtx alice bob test1 test1FollowerType

executeModelTest :: PDRInstance -> PDRInstance -> ContextInstance -> PerspectivesUser -> PerspectivesUser -> String -> String -> Aff Unit
executeModelTest pdrA pdrB testCtx alice bob testContextType testFollowerType = do

  -- Hier wordt het specifiek per test.
  -- Alice creates a Test1 context and fills a new TestApp$Tests role with it.
  -- The "Test" aspect context creates both the Leader and the Follower role, 
  -- and binds Alice to the Leader and Bob to the Follower. 
  theTest <- runInPDR pdrA do
    infoTest "Alice creates a test in PDRA"
    runMonadPerspectivesTransaction' shareWithPeers (CR $ CalculatedRoleType testAppManager)
      do
        result <- runExceptT $ constructContext (Just (ENR $ EnumeratedRoleType testsType))
          $ ContextSerialization
            { id: Nothing
            , ctype: testContextType
            , prototype: Nothing
            , rollen: OBJ.empty
            , externeProperties: PropertySerialization OBJ.empty
            }
        case result of
          Left err -> throwError $ error ("Failed to create test context: " <> show err)
          Right test -> do
            void $ createAndAddRoleInstance (EnumeratedRoleType testsType) (unwrap testCtx)
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
            roles <- theTest ##= getEnumeratedRoleInstances (EnumeratedRoleType testFollowerType)
            if null roles then pure Nothing
            else do
              infoTest "Bob has verified that he has the Follower role in the test in PDRB"
              pure (Just roles)
    )

  -- Alice executes the test, bringing the test role in state Executed.
  runInPDR pdrA
    $ runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType testLeaderType)
    $ do
        lift $ infoTest "Alice executes a test in PDRA"
        runContextAction testLeaderType "RunTest" (unwrap theTest)

  testResult <- pollUntil 100 (Milliseconds 100.0)
    "Bob to have a value for the property Name of the external role of the test"
    ( runInPDR pdrB $
        do
          mtestName <- testExternalRole ##> getPropertyValues (ENP $ EnumeratedPropertyType testNameProperty)
          case mtestName of
            Just (Value testName) -> do
              -- Get the value of Calculated property TestSucceeded of the external role of the test.
              testSucceeded <- testExternalRole ##>> getPropertyValues (CP $ CalculatedPropertyType testSucceededProperty)
              pure (Just { testName, testSucceeded })
            Nothing -> pure Nothing

    )

  case testResult of 
    { testName, testSucceeded } -> assert ("Bob should see that test '" <> testName <> "' succeeded") (unwrap testSucceeded == "true")

testModel :: String
-- testModel = "model://joopringelberg.nl#SynchronisationTestModel@2.0"
testModel = "model://joopringelberg.nl#hj1bh3wydo@2.0"

indexedTestContext :: String
-- indexedTestContext = "model://joopringelberg.nl#SynchronisationTestModel$TestSyncApp"
indexedTestContext = "model://joopringelberg.nl#hj1bh3wydo$sxzzlm7ew3$s1nrgv6dbu"

testAppManager :: String
-- testAppManager = "model://joopringelberg.nl#SynchronisationTestModel$TestApp$Manager"
testAppManager = "model://joopringelberg.nl#hj1bh3wydo$sxzzlm7ew3$ftd5ohdwz6"

testLeaderType :: String
-- testLeaderType = "model://joopringelberg.nl#SynchronisationTestModel$Test$Leader"
testLeaderType = "model://joopringelberg.nl#hj1bh3wydo$qncdjftskg$zc16eb7hki"

testAppFollowerType :: String
-- testAppFollowerType = "model://joopringelberg.nl#SynchronisationTestModel$TestApp$Follower"
testAppFollowerType = "model://joopringelberg.nl#hj1bh3wydo$sxzzlm7ew3$onxv7kdh0q"

testsType :: String
-- testsType = "model://joopringelberg.nl#SynchronisationTestModel$TestApp$Tests"
testsType = "model://joopringelberg.nl#hj1bh3wydo$sxzzlm7ew3$qde7w03jms"

testSucceededProperty :: String
-- testSucceededProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test1$External$TestSucceeded"
testSucceededProperty = "model://joopringelberg.nl#hj1bh3wydo$qncdjftskg$External$jvgngnhi6g"

testNameProperty :: String
-- testNameProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test$External$TestName"
testNameProperty = "model://joopringelberg.nl#hj1bh3wydo$bkbe0cg3h2$External$x50lipiazl"

-------------------------------------------------------------------------------
---- THE TESTS
-------------------------------------------------------------------------------

test1 :: String
-- testContextType = "model://joopringelberg.nl#SynchronisationTestModel$Test1"
test1 = "model://joopringelberg.nl#hj1bh3wydo$qncdjftskg"

test1FollowerType :: String
-- test1FollowerType = "model://joopringelberg.nl#SynchronisationTestModel$Test1$Follower"
test1FollowerType = "model://joopringelberg.nl#hj1bh3wydo$qncdjftskg$e21r6td06a"

