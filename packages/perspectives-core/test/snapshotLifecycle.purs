-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.SnapshotLifecycle (main) where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Data.Either (Either(..), isLeft)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff (attempt, bracket, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, new, read)
import Perspectives.CoreTypes ((##=))
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), IndexedContext(..))
import Perspectives.Sidecar.ToStable (toStable)
import Test.PDRInstance (noBus, startPDRInstanceFromSnapshotWithHook, testPouchdbUser)
import Test.PDRInstance.Types (runInPDR)
import Test.SinglePDRScaffold (saveSuccessfulSnapshot)
import Test.Test.PDRInstance.SubscribePDRtoAMQP (getManagedBrokerService, managedBrokers, brokerServicesApp, requireUnsubscribed)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Foreign.Object as Object

main :: Effect Unit
main = runTest do
  suite "Reusable snapshot publication" do
    test "only a nonempty, entirely successful run publishes a snapshot" do
      writes <- liftEffect $ new 0
      let
        save = liftEffect $ modify_ (_ + 1) writes
        success = Right { testName: "success", testSucceeded: true }
        failure = Right { testName: "failure", testSucceeded: false }
        timeout = Left { testName: "timeout", err: error "timeout" }
      saveSuccessfulSnapshot [] save
      saveSuccessfulSnapshot [ failure ] save
      saveSuccessfulSnapshot [ timeout ] save
      saveSuccessfulSnapshot [ success, failure ] save
      liftEffect (read writes) >>= equal 0
      saveSuccessfulSnapshot [ success, success ] save
      liftEffect (read writes) >>= equal 1

  suite "Rebooted universe prerequisites" do
    test "Alice reuses the saved broker and restores CouchDB credentials without a subscription" do
      bracket
        ( startPDRInstanceFromSnapshotWithHook (testPouchdbUser "alice") defaultRuntimeOptions Nothing noBus
            "test/pdr-snapshot/rebootedUniverse/alice"
            requireUnsubscribed
        )
        _.shutdown
        \pdr -> do
          runInPDR pdr do
            IndexedContext index <- toStable (IndexedContext brokerServicesApp)
            app <- lookupIndexedContext index
            brokerType <- toStable (EnumeratedRoleType managedBrokers)
            before <- case app of
              Just context -> context ##= getEnumeratedRoleInstances brokerType
              Nothing -> pure []
            broker <- getManagedBrokerService
            again <- getManagedBrokerService
            after <- case app of
              Just context -> context ##= getEnumeratedRoleInstances brokerType
              Nothing -> pure []
            credentials <- gets _.couchdbCredentials
            liftAff do
              equal (ContextInstance "pub:https://perspectives.domains/cw_bigbangsdatabase/#BigBangsBrokerService") broker.publicContext
              equal broker.publicContext again.publicContext
              equal before after
              assert "restored Alice has CouchDB server credentials"
                (isJust $ Object.lookup "https://perspectives.domains/" credentials)
            requireUnsubscribed

    test "a seed snapshot cannot silently recreate broker management" do
      bracket
        ( startPDRInstanceFromSnapshotWithHook (testPouchdbUser "alice") defaultRuntimeOptions Nothing noBus
            "test/pdr-snapshot/universe/alice"
            requireUnsubscribed
        )
        _.shutdown
        \pdr -> do
          result <- attempt $ runInPDR pdr getManagedBrokerService
          assert "missing management must fail explicitly" (isLeft result)
