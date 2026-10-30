-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

-- | PureScript bindings for the in-process AMQP message-bus stub used in
-- | Layer 3 synchronisation tests.
-- |
-- | The stub replaces the real `@stomp/stompjs` Stomp client with an
-- | in-memory pub/sub bus that routes `TransactionForPeer` messages directly
-- | between two PDR instances running in the same Node.js process.
-- |
-- | Typical test setup:
-- |
-- | ```purescript
-- | bus     <- liftEffect createInProcessBus
-- | let factoryA = makeStompClientFactory bus
-- |     factoryB = makeStompClientFactory bus
-- |
-- | -- Install each factory into the respective PDR's PerspectivesState before
-- | -- calling AMQP.IncomingPost.incomingPost.
-- | runPerspectivesWithState (setStompClientFactory factoryA) stateA
-- | runPerspectivesWithState (setStompClientFactory factoryB) stateB
-- | ```
-- |
-- | See `Test.PDRInstance` for the full two-PDR scaffold.

module Perspectives.AMQP.Stomp.Stub
  ( InProcessBus
  , createInProcessBus
  , makeStompClientFactory
  ) where

import Effect (Effect)
import Perspectives.AMQP.Stomp (StompClient)

-- | An opaque handle to the shared in-process message bus.
-- | Both PDR instances must receive the *same* `InProcessBus` value so that
-- | messages published by one reach the other.
foreign import data InProcessBus :: Type

-- | Create a fresh in-process message bus.
-- | Call this once per test and share the result between the two PDR setup
-- | calls.
foreign import createInProcessBus :: Effect InProcessBus

-- | Given an `InProcessBus`, return a Stomp-client factory
-- | `(String -> Effect StompClient)` suitable for storing in
-- | `PerspectivesState.stompClientFactory`.
-- |
-- | The returned factory ignores the URL argument (there is no real broker)
-- | and instead creates a stub client that routes messages through `bus`.
foreign import makeStompClientFactory
  :: InProcessBus -> (String -> Effect StompClient)
