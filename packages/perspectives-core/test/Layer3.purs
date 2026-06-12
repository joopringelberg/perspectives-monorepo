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
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for, for_)
import Data.Newtype (unwrap)
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (error)
import Foreign.Object (empty) as OBJ
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Effect.Aff (error)
import Foreign.Object (empty) as OBJ
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.RunAction (runContextAction)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##=), (##>>))
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##=), (##>>))
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (binding, binding_, context, getEnumeratedRoleInstances)
import Perspectives.Logging (ansiMagenta, ansiRed, debugTest)
import Perspectives.ModelDependencies (sysMe, sysUser)
import Perspectives.Names (lookupIndexedContext, lookupIndexedRole)
import Perspectives.Logging (ansiMagenta, ansiRed, debugTest)
import Perspectives.ModelDependencies (sysMe, sysUser)
import Perspectives.Names (lookupIndexedContext, lookupIndexedRole)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (tryGetPerspectRol)
import Perspectives.PerspectivesState (defaultRuntimeOptions, setTopicLogLevel)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Test.PDRInstance (connectPDRs, noBus, runInPDR, testPouchdbUser, waitUntilAllTransactionsComplete, withPDR, withTwoPDRs)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Test.PDRInstance (connectPDRs, noBus, runInPDR, testPouchdbUser, waitUntilAllTransactionsComplete, withPDR, withTwoPDRs)
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
                -- setTopicLogLevel SYNC Trace
            )
          runInPDR pdrB
            ( do
                setTopicLogLevel BROKER Debug
                -- setTopicLogLevel SYNC Trace
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

          runInPDR pdrA
            ( do
                setTopicLogLevel BROKER Debug
                setTopicLogLevel INSTALL Trace
                setTopicLogLevel SYNC Trace
            )
          runInPDR pdrB
            ( do
                setTopicLogLevel BROKER Debug
                setTopicLogLevel INSTALL Trace
                setTopicLogLevel SYNC Trace
            )

          connectPDRs pdrA pdrB

          -- Get Alice's and Bob's Persons instances for later use.
          alice <- runInPDR pdrA do
            muser <- lookupIndexedRole sysMe
            case muser of
              Just user -> binding_ user >>= case _ of 
                  Just alice -> pure alice
                  Nothing -> throwError $ error "Alice should have a `me` instance in her PDR"
              Nothing -> throwError $ error "Alice should have a `me` instance in her PDR"
          bob <- runInPDR pdrB do
            muser <- lookupIndexedRole sysMe
            case muser of
              Just user -> binding_ user >>= case _ of
                  Just bob -> pure bob
                  Nothing -> throwError $ error "Bob should have a `me` instance in his PDR"
              Nothing -> throwError $ error "Bob should have a `me` instance in his PDR"

          -- Alice loads test model in pdrA.
          runInPDR pdrA do
            debugTest "Alice Loads test model in PDRA"
            runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
              $
                addModelToLocalStore_ [testModel] (RoleInstance "Ignored")
          
          waitUntilAllTransactionsComplete pdrA

          -- Retrieve all individual tests.
          testExternalRoles <- runInPDR pdrA do
            debugTest "Retrieve all individual tests in PDRA"
            -- Get the indexed test context instance.
            mTestCtx <- lookupIndexedContext indexedTestContext
            case mTestCtx of
              Nothing -> throwError $ error "Test context instance should be indexed in Alice's PDR"
              Just testCtx -> testCtx ##= getEnumeratedRoleInstances (EnumeratedRoleType testsType) >=> binding
          
          -- Alice gives Bob the role Follower in all tests.
          runInPDR pdrA $
            runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType testLeaderType)
            $ for_ testExternalRoles (\testExternalRole -> do
                lift $ debugTest "Alice gives Bob the Follower role in a test in PDRA"
                theTest <- lift (testExternalRole ##>> context )
                createAndAddRoleInstance
                  (EnumeratedRoleType testFollowerType)
                  (unwrap theTest)
                  ( RolSerialization
                      { id: Nothing
                      , properties: PropertySerialization OBJ.empty
                      , binding: Just (unwrap bob)
                      }
                  ))
          -- Wait for Bob to digest this.
          waitUntilAllTransactionsComplete pdrB

          -- Alice executes all tests, bringing the test role in state Executed.
          runInPDR pdrA $
            runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType testLeaderType)
            $ for_ testExternalRoles (\testExternalRole -> do
                lift $ debugTest "Alice executes a test in PDRA"
                theTest <- lift (testExternalRole ##>> context )
                runContextAction testLeaderType "RunTest" (unwrap theTest)
            )

          -- Wait.
          waitUntilAllTransactionsComplete pdrA
          waitUntilAllTransactionsComplete pdrB

          -- Bob checks that all tests are in state Executed.
          testResults <- runInPDR pdrB $
            for testExternalRoles (\testExternalRole -> do
              debugTest "Bob checks that all tests are in state Executed in PDRB"
              -- Get the value of Calculated property TestSucceeded of the external role of the test.
              testSucceeded <- testExternalRole ##>> getPropertyValues (CP $ CalculatedPropertyType testSucceededProperty)
              testName <- testExternalRole ##>> getPropertyValues (ENP $ EnumeratedPropertyType testNameProperty)
              pure {testName, testSucceeded}
            )
          
          for_ testResults \({testName, testSucceeded}) -> do
            assert ("Bob should see that test '" <> (unwrap testName) <> "' succeeded") (unwrap testSucceeded == "true")

testModel :: String
testModel = "model://joopringelberg.nl#SynchronisationTestModel@1.0"

indexedTestContext :: String
-- indexedTestContext = "model://joopringelberg.nl#SynchronisationTestModel$TestSyncApp"
indexedTestContext = "model://joopringelberg.nl#hj1bh3wydo$sxzzlm7ew3$s1nrgv6dbu"

testLeaderType :: String
-- testLeaderType = "model://joopringelberg.nl#SynchronisationTestModel$Test$Leader"
testLeaderType = "model://joopringelberg.nl#hj1bh3wydo$qncdjftskg$zc16eb7hki"

testFollowerType :: String
-- testFollowerType = "model://joopringelberg.nl#SynchronisationTestModel$Test$Follower"
testFollowerType = "model://joopringelberg.nl#hj1bh3wydo$qncdjftskg$e21r6td06a"

testsType :: String
-- testsType = "model://joopringelberg.nl#SynchronisationTestModel$TestApp$Tests"
testsType = "model://joopringelberg.nl#hj1bh3wydo$sxzzlm7ew3$qde7w03jms"

testSucceededProperty :: String
-- testSucceededProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test$External$TestSucceeded"
testSucceededProperty = "model://joopringelberg.nl#hj1bh3wydo$qncdjftskg$External$jvgngnhi6g"

testNameProperty :: String
-- testNameProperty = "model://joopringelberg.nl#SynchronisationTestModel$Test$External$TestName"
testNameProperty = "model://joopringelberg.nl#hj1bh3wydo$qncdjftskg$External$jvgngnhi6g"