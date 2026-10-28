-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.FreshSubscriptions (main) where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Data.Array (elem, length)
import Data.Either (Either(..), isLeft)
import Data.Foldable (all, for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, bracket, error, throwError)
import Effect.Aff.AVar (tryRead)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (message)
import Effect.Ref (modify_, new, read)
import Perspectives.AMQP.RabbitMQManagement (virtualHost)
import Perspectives.CoreTypes ((##=))
import Perspectives.Cuid2 (cuid2)
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), IndexedContext(..))
import Perspectives.Sidecar.ToStable (toStable)
import Test.PDRInstance (pollUntil, testPouchdbUser, withTwoPDRsCachedNoBus)
import Test.PDRInstance.Types (PDRInstance, runInPDR)
import Test.RabbitMQCtl (addAdminUser, deleteQueue, deleteUser, queueExists, queueHasConsumer, userExists)
import Test.Test.PDRInstance.SubscribePDRtoAMQP (brokerServicesApp, getBrokerSubscriptions, managedBrokers, requireUnsubscribed)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

type Subscription = { login :: String, queueId :: String }

main :: Effect Unit
main = runTest $ suite "Fresh AMQP subscriptions with reused management" do
  test "fresh accounts are removed after two successful runs and a failing callback" do
    username <- liftEffect $ cuid2 "freshSubscriptionAdmin"
    password <- liftEffect $ cuid2 "freshSubscriptionPassword"
    created <- liftEffect $ new []
    bracket
      (addAdminUser virtualHost username password)
      ( \_ -> do
          -- Clean only resources recorded by this test, even when an assertion fails.
          subscriptions <- liftEffect $ read created
          for_ subscriptions \{ login, queueId } -> do
            userExists login >>= \exists -> when exists $ deleteUser login
            queueExists virtualHost queueId >>= \exists -> when exists $ deleteQueue virtualHost queueId
          deleteUser username
      )
      \_ -> do
        let
          run shouldFail = withTwoPDRsCachedNoBus
            { adminUserName: username, adminPassword: password }
            (testPouchdbUser "alice")
            defaultRuntimeOptions
            Nothing
            "test/pdr-snapshot/rebootedUniverse/alice"
            (testPouchdbUser "bob")
            defaultRuntimeOptions
            Nothing
            "test/pdr-snapshot/universe/bob"
            \alice bob -> do
              runInPDR alice do
                IndexedContext index <- toStable (IndexedContext brokerServicesApp)
                app <- lookupIndexedContext index
                brokerType <- toStable (EnumeratedRoleType managedBrokers)
                brokers <- case app of
                  Just context -> context ##= getEnumeratedRoleInstances brokerType
                  Nothing -> pure []
                liftAff $ equal 1 (length brokers)
              aliceSubscription <- activeSubscription alice
              liftEffect $ modify_ (_ <> [ aliceSubscription ]) created
              bobSubscription <- activeSubscription bob
              liftEffect $ modify_ (_ <> [ bobSubscription ]) created
              for_ [ alice, bob ] \pdr -> do
                contracts <- runInPDR pdr getBrokerSubscriptions
                equal 1 (length contracts)
                guardResult <- attempt $ runInPDR pdr requireUnsubscribed
                assert "an active subscription must fail the reusable-snapshot guard" (isLeft guardResult)
              when shouldFail $ throwError $ error "Intentional failure to exercise subscription finalizers"
              pure [ aliceSubscription, bobSubscription ]
        first <- run false
        checkRemoved first
        second <- run false
        checkRemoved second
        assert "each run must generate fresh RabbitMQ users"
          (all (\subscription -> not (elem subscription.login (map _.login first))) second)
        assert "each run must generate fresh RabbitMQ queues"
          (all (\subscription -> not (elem subscription.queueId (map _.queueId first))) second)
        failure <- attempt $ run true
        case failure of
          Left err -> equal "Intentional failure to exercise subscription finalizers" (message err)
          Right _ -> assert "the callback failure must be propagated" false
        liftEffect (read created) >>= checkRemoved

activeSubscription :: PDRInstance -> Aff Subscription
activeSubscription pdr = pollUntil 120 (Milliseconds 500.0) (pdr.name <> " active subscription") do
  subscription <- runInPDR pdr do
    service <- gets _.brokerService >>= liftAff <<< tryRead
    pure $ service <#> \{ login, queueId } -> { login, queueId }
  case subscription of
    Nothing -> pure Nothing
    Just result -> do
      active <- queueHasConsumer virtualHost result.queueId
      pure if active then Just result else Nothing

checkRemoved :: Array Subscription -> Aff Unit
checkRemoved subscriptions =
  for_ subscriptions \{ login, queueId } -> do
    userExists login >>= assert ("RabbitMQ user must be deleted: " <> login) <<< not
    queueExists virtualHost queueId >>= assert ("RabbitMQ queue must be deleted: " <> queueId) <<< not
