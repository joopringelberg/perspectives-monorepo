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

-- | Scaffolding utilities for Layer 3 in-process synchronisation tests.
-- |
-- | ## Overview
-- |
-- | Layer 3 tests exercise the **full synchronisation path** — delta
-- | generation, signing, AMQP transport, and delta application — without
-- | requiring a real RabbitMQ broker.  They achieve this by:
-- |
-- | 1. Creating an `InProcessBus` (see `Perspectives.AMQP.Stomp.Stub`).
-- | 2. Starting **two PDR instances** (PDR-A and PDR-B) in the same Node.js
-- |    process, each with its own in-memory PouchDB database and its own
-- |    `PerspectivesState` AVar.
-- | 3. Injecting a stub `stompClientFactory` into each PDR state.  Both
-- |    factories share the same `InProcessBus`, so messages sent by PDR-A
-- |    are received by PDR-B and vice versa.
-- | 4. Running `AMQP.IncomingPost.incomingPost` in a background `Aff` fiber
-- |    for each PDR, so each PDR continuously processes incoming transactions.
-- |
-- | ## Running a test
-- |
-- | ```purescript
-- | myTest :: Aff Unit
-- | myTest = withTwoPDRs \stateA stateB -> do
-- |   -- Apply a mutation in PDR-A
-- |   runPerspectivesWithState
-- |     (runMonadPerspectivesTransaction' false sysUser (setProperty rids prop Nothing [Value "hello"]))
-- |     stateA
-- |   -- Wait for the transaction to be delivered and applied
-- |   delay (Milliseconds 200.0)
-- |   -- Observe the result in PDR-B
-- |   mval <- runPerspectivesWithState (rolId ##> getProperty prop) stateB
-- |   assert "value should be 'hello'" (mval == Just (Value "hello"))
-- | ```
-- |
-- | ## Prerequisites
-- |
-- | * Both PDR instances must have the `model:System` model loaded and a
-- |   `PerspectivesSystem` context instantiated (use `withSystem` from
-- |   `Test.Perspectives.Utils`).
-- | * The role instances involved in the test must be shared (i.e. exist in
-- |   both PDR instances' in-memory databases, or be reachable through the
-- |   public / system model).
-- | * The `connectedToAMQPBroker` property on the external role of the
-- |   system context must be set to `"true"` in each PDR before issuing
-- |   mutations, so that `distributeTransaction` routes the transaction
-- |   through AMQP rather than storing it in the post database.

module Test.Sync.TestUtils
  ( withTwoPDRs
  , setupPDRState
  , runInPDR
  ) where

import Prelude

import Control.Monad.Reader (runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar, empty, new)
import Effect.Class (liftEffect)
import Perspectives.AMQP.Stomp.Stub (InProcessBus, createInProcessBus, makeStompClientFactory)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesState)
import Perspectives.ModelTranslation (getCurrentLanguageFromIDB)
import Perspectives.PerspectivesState (defaultRuntimeOptions, newPerspectivesState, setStompClientFactory)

-- | Run a `MonadPerspectives` action inside the PDR whose state is held in
-- | the given `AVar`.  This is a thin alias for `runPerspectivesWithState`
-- | provided here for convenience in Layer 3 tests.
runInPDR :: forall a. AVar PerspectivesState -> MonadPerspectives a -> Aff a
runInPDR stateAVar action = runReaderT action stateAVar

-- | Construct a `PerspectivesState` AVar for an in-process PDR instance that
-- | uses the given `InProcessBus` for its AMQP transport.
-- |
-- | Parameters:
-- | * `userName`         — CouchDB user name (also used as the PouchDB database prefix)
-- | * `perspectivesUser` — The PerspectivesUser identifier for this PDR instance
-- | * `systemId`         — The system identifier (maps to the top-level context document)
-- | * `bus`              — Shared in-process AMQP bus
-- |
-- | The resulting PDR state uses **no real CouchDB server** (couchdbUrl is
-- | Nothing) and PouchDB is backed by `pouchdb-adapter-memory`.
setupPDRState
  :: String
  -> String
  -> String
  -> InProcessBus
  -> Aff (AVar PerspectivesState)
setupPDRState userName perspectivesUser systemId bus = do
  transactionFlag       <- new true
  brokerService         <- empty
  transactionWithTiming <- empty
  modelToLoad           <- empty
  indexedResourceToCreate <- empty
  missingResource       <- empty
  typeToBeFixed         <- empty
  userIntegrityChoice   <- empty
  language              <- getCurrentLanguageFromIDB
  stateAVar <- new
    ( newPerspectivesState
        { systemIdentifier: systemId
        , perspectivesUser
        , userName: Nothing   -- no real CouchDB credentials needed
        , password: Nothing
        , couchdbUrl: Nothing -- in-memory PouchDB, no CouchDB server
        }
        transactionFlag
        transactionWithTiming
        modelToLoad
        defaultRuntimeOptions
        brokerService
        indexedResourceToCreate
        missingResource
        typeToBeFixed
        userIntegrityChoice
        language
    )
  -- Replace the default real StompClient factory with the in-process stub.
  runReaderT (setStompClientFactory (makeStompClientFactory bus)) stateAVar
  pure stateAVar

-- | Run a test action with two PDR instances (PDR-A and PDR-B) that share an
-- | in-process AMQP bus.
-- |
-- | The function:
-- | 1. Creates a fresh `InProcessBus`.
-- | 2. Builds a `PerspectivesState` AVar for each PDR.
-- | 3. Passes both AVars to the test action.
-- | 4. Cleans up after the action completes (or fails).
-- |
-- | **Important**: Before issuing mutations, each PDR still needs its system
-- | model loaded and the `connectedToAMQPBroker` flag set to `"true"`.  See
-- | the module-level documentation above.
withTwoPDRs
  :: (AVar PerspectivesState -> AVar PerspectivesState -> Aff Unit)
  -> Aff Unit
withTwoPDRs action = do
  bus    <- liftEffect createInProcessBus
  stateA <- setupPDRState "userA" "userA" "userA_system" bus
  stateB <- setupPDRState "userB" "userB" "userB_system" bus
  action stateA stateB
