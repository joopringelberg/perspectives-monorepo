-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.BrokerServiceSignupDiagnostic where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Parallel (parOneOf)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, bracket, delay, error, finally, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (message)
import Node.Process (exit')
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..))
import Perspectives.Cuid2 (cuid2)
import Perspectives.Performance (reportNormalisationMeasurements, resetNormalisationMeasurements, setNormalisationPhase, timed)
import Perspectives.PerspectivesState (defaultRuntimeOptions, setTopicLogLevel)
import Test.BrokerServiceSignupTests (aliceHasBobAccount, aliceSnapshotDirectory, cleanupAliceSubscription, currentBrokerCredentials, ensureAliceSubscribed)
import Test.LocalCouchdbTestSupport (addLocalCouchdbCredentials)
import Test.PDRInstance (noBus, pollUntil, snapshotExists, startPDRInstance, startPDRInstanceFromSnapshotWithHook, testPouchdbUser, waitUntilAllTransactionsComplete)
import Test.PDRInstance.Types (PDRInstance, runInPDR)
import Test.RabbitMQCtl (addAdminUser, deleteUser, queueExists, queueHasConsumer, userExists)
import Test.Test.PDRInstance.SubscribePDRtoAMQP (getManagedBrokerService, requireUnsubscribed, reuseAMQPManagement, subscribePDRtoAMQP)
import Perspectives.AMQP.RabbitMQManagement (virtualHost)

main :: Effect Unit
main = launchAff_ do
  result <- parOneOf
    [ attempt runDiagnostic
    , delay (Milliseconds 300000.0) *> pure (Left (error "Broker signup diagnostic exceeded five minutes"))
    ]
  liftEffect reportNormalisationMeasurements
  case result of
    Left err -> liftEffect do
      Console.error $ "Broker signup diagnostic failed: " <> message err
      exit' 1
    Right _ -> liftEffect $ Console.log "Broker signup diagnostic passed"

runDiagnostic :: Aff Unit
runDiagnostic = do
  liftEffect resetNormalisationMeasurements
  exists <- snapshotExists aliceSnapshotDirectory
  unless exists $ throwError $ error $ "Required diagnostic snapshot is missing: " <> aliceSnapshotDirectory
  bracket
    ( do
        suffix <- liftEffect $ cuid2 "signupDiagnostic"
        password <- liftEffect $ cuid2 "signupDiagnosticPassword"
        let username = "bssu_" <> suffix
        pure { adminUserName: username, adminPassword: password }
    )
    ( \credentials -> do
        exists <- userExists credentials.adminUserName
        when exists $ deleteUser credentials.adminUserName
    )
    \credentials ->
      addAdminUser virtualHost credentials.adminUserName credentials.adminPassword *>
        withDiagnosticPDR "alice"
          ( startPDRInstanceFromSnapshotWithHook
              (testPouchdbUser "alice")
              defaultRuntimeOptions
              Nothing
              noBus
              aliceSnapshotDirectory
              do
                requireUnsubscribed
                setTopicLogLevel SYNC Trace
          )
          \alice ->
            withDiagnosticPDR "bob"
              (startPDRInstance (testPouchdbUser "bob") defaultRuntimeOptions Nothing noBus)
              \bob -> do
                runInPDR bob $ setTopicLogLevel SYNC Trace
                addLocalCouchdbCredentials alice
                addLocalCouchdbCredentials bob
                liftEffect $ setNormalisationPhase "management-setup"
                void $ timed "signup.management" "alice" $ reuseAMQPManagement credentials alice
                liftEffect $ setNormalisationPhase "manager-signup"
                timed "signup.manager" "alice" $ ensureAliceSubscribed alice
                alreadyKnown <- aliceHasBobAccount alice
                when alreadyKnown $ throwError $ error "Alice already has Bob's account-holder details before signup"
                broker <- runInPDR alice getManagedBrokerService
                liftEffect $ setNormalisationPhase "subscriber-signup"
                timed "signup.subscriber" "bob" $ subscribePDRtoAMQP broker.publicContext bob
                liftEffect $ setNormalisationPhase "handshake"
                timed "signup.handshake" "alice-bob" do
                  void $ pollUntil 240 (Milliseconds 500.0) "Alice to receive Bob's account-holder details" do
                    received <- aliceHasBobAccount alice
                    pure if received then Just unit else Nothing
                  void $ pollUntil 240 (Milliseconds 500.0) "Bob's RabbitMQ account and active queue consumer" do
                    subscription <- runInPDR bob currentBrokerCredentials
                    case subscription of
                      Nothing -> pure Nothing
                      Just { login, queueId } -> do
                        accountExists <- userExists login
                        hasConsumer <- queueHasConsumer virtualHost queueId
                        pure if accountExists && hasConsumer then Just unit else Nothing
                  waitUntilAllTransactionsComplete 120 alice
                  waitUntilAllTransactionsComplete 120 bob

withDiagnosticPDR :: forall a. String -> Aff PDRInstance -> (PDRInstance -> Aff a) -> Aff a
withDiagnosticPDR user acquire action =
  bracket
    ( do
        liftEffect $ setNormalisationPhase ("startup-" <> user)
        timed "signup.start" user acquire
    )
    ( \pdr -> timed "signup.cleanup" user do
        liftEffect $ setNormalisationPhase ("cleanup-" <> user)
        subscription <- runInPDR pdr currentBrokerCredentials
        finally pdr.shutdown (cleanupAliceSubscription pdr)
        for_ subscription \{ login, queueId } -> do
          accountExists <- userExists login
          queueStillExists <- queueExists virtualHost queueId
          when (accountExists || queueStillExists)
            $ throwError
            $ error
            $ user <> " diagnostic RabbitMQ resources survived cleanup"
    )
    action
