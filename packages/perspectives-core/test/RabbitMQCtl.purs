-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

-- | Test support for a RabbitMQ node running on the local machine.
-- | Uses `rabbitmqctl`, which authenticates through the Erlang cookie,
-- | so no RabbitMQ admin credentials are needed.
module Test.RabbitMQCtl
  ( purgeQueue
  , purgeOwnQueue
  , userExists
  , queueExists
  , addAdminUser
  , deleteUser
  ) where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Promise (Promise, toAffE)
import Data.Array (any, head)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.AVar (tryRead)
import Effect.Aff.Class (liftAff)
import Effect.Exception (message)
import Perspectives.AMQP.RabbitMQManagement (virtualHost)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Logging (infoTest, warnTest)

foreign import purgeQueueImpl :: String -> String -> Effect (Promise String)
foreign import rabbitmqctlImpl :: Array String -> Effect (Promise String)

rabbitmqctl :: Array String -> Aff String
rabbitmqctl args = toAffE (rabbitmqctlImpl args)

-- | Create a user with the administrator tag and full permissions on the virtual hosts '/' and the given one,
-- | so it can be used for the management API.
addAdminUser :: String -> String -> String -> Aff Unit
addAdminUser vhost name password = do
  void $ rabbitmqctl [ "add_user", name, password ]
  void $ rabbitmqctl [ "set_user_tags", name, "administrator" ]
  for_ [ "/", vhost ] \vh -> rabbitmqctl [ "set_permissions", "-p", vh, name, ".*", ".*", ".*" ]

deleteUser :: String -> Aff Unit
deleteUser name = void $ rabbitmqctl [ "delete_user", name ]

-- | Whether the user exists on the local RabbitMQ node.
userExists :: String -> Aff Boolean
userExists name = do
  out <- rabbitmqctl [ "-q", "--no-table-headers", "list_users" ]
  pure $ any (\line -> head (split (Pattern "\t") line) == Just name) (split (Pattern "\n") out)

-- | Whether the queue exists in the virtual host on the local RabbitMQ node.
queueExists :: String -> String -> Aff Boolean
queueExists vhost queueName = do
  out <- rabbitmqctl [ "-q", "--no-table-headers", "list_queues", "-p", vhost, "name" ]
  pure $ any (_ == queueName) (split (Pattern "\n") out)

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
