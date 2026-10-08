-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Test support for a RabbitMQ node running on the local machine.
-- | Uses `rabbitmqctl`, which authenticates through the Erlang cookie,
-- | so no RabbitMQ admin credentials are needed.
module Test.RabbitMQCtl
  ( purgeQueue
  , purgeOwnQueue
  ) where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.AVar (tryRead)
import Effect.Aff.Class (liftAff)
import Effect.Exception (message)
import Perspectives.AMQP.RabbitMQManagement (virtualHost)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Logging (infoTest, warnTest)

foreign import purgeQueueImpl :: String -> String -> Effect (Promise String)

purgeQueue :: String -> String -> Aff String
purgeQueue vhost queueName = toAffE (purgeQueueImpl vhost queueName)

-- | Purge the queue of the broker service currently set in this PDR (if any).
-- | Messages left in the queue by earlier test runs refer to resources that do not
-- | exist in the PDR restored from a snapshot, so we drop them before subscribing.
purgeOwnQueue :: MonadPerspectives Unit
purgeOwnQueue = do
  bsAVar <- gets _.brokerService
  mbs <- liftAff $ tryRead bsAVar
  case mbs of
    Nothing -> infoTest "purgeOwnQueue: no broker service, nothing to purge."
    Just { queueId } -> do
      r <- liftAff $ attempt $ purgeQueue virtualHost queueId
      case r of
        Left e -> warnTest ("purgeOwnQueue: " <> message e)
        Right out -> infoTest ("purgeOwnQueue: " <> out)
