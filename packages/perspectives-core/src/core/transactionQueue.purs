-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.TransactionQueue
  ( TransactionQueue
  , TransactionPriority(..)
  , newTransactionQueue
  , withPriority
  ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, bracket, delay)
import Effect.Aff.AVar (empty, take)
import Effect.AVar as AVar
import Effect.Class (liftEffect)

foreign import data TransactionQueue :: Type

data TransactionPriority = UserTransaction | BackgroundTransaction

-- | The argument ensures each PDR gets its own queue.
foreign import newTransactionQueue :: Unit -> TransactionQueue

foreign import enqueue :: TransactionQueue -> Boolean -> Effect Unit -> Effect (Effect Unit)

-- | Queue only outer work: embedded transactions must retain their parent's admission.
-- | Waiting and running are both cancellable; bracket always removes/releases the job.
withPriority :: forall a. TransactionQueue -> TransactionPriority -> Aff a -> Aff a
withPriority queue priority action = do
  ready <- empty
  let
    user = case priority of
      UserTransaction -> true
      BackgroundTransaction -> false
  bracket
    (liftEffect $ enqueue queue user (void $ AVar.tryPut unit ready))
    liftEffect
    \_ -> do
      take ready
      -- Yield to the worker's message loop even when a backlog can run synchronously.
      delay (Milliseconds 0.0)
      action
