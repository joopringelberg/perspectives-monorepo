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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-- | Persistent storage for transactions that are blocked because of version gaps,
-- | or whose execution failed with an exception.
-- | When a received transaction contains a delta whose resourceVersion > local + 1,
-- | the entire transaction is blocked (causal dependency via automatic actions).
-- | Blocked transactions are stored here so they survive session restarts; they are
-- | retried by Perspectives.Sync.PendingTransactions.drainPendingTransactions.
-- | See docsources/delta-ordering.md §3.3 and §10 for details.
module Perspectives.Persistence.PendingTransactionStore
  ( PendingTransactionRecord(..)
  , MissingDelta(..)
  , storePendingTransaction
  , recordFailedTransaction
  , removePendingTransaction
  , removePendingTransactionFor
  , removePendingTransactionById
  , getPendingTransactionsForResource
  , getAllPendingTransactions
  , pendingTransactionDatabaseName
  , pendingTransactionDocId
  , blockedStatus
  , failedStatus
  , maxFailedAttempts
  , recordHistoryRequest
  ) where

import Prelude

import Data.Array (catMaybes)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.DateTime.Instant (unInstant)
import Data.Newtype (unwrap)
import Effect.Now (now)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Perspectives.Couchdb.Revision (Revision_)
import Perspectives.Persistence.API (addDocument_, deleteDocument, documentsInRange, tryGetDocument_)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (MonadPouchdb)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser)
import Perspectives.Sync.CanonicalJson (canonicalizeJsonString, computeDeltaId, shortDeltaId)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Simple.JSON (class ReadForeign, class WriteForeign, read, readImpl, writeJSON)

-----------------------------------------------------------
-- TYPES
-----------------------------------------------------------

-- | Describes a missing delta that caused a transaction to be blocked.
-- | A pending transaction may be blocked on multiple missing deltas.
type MissingDelta =
  { author :: PerspectivesUser -- the author of the missing delta
  , resourceKey :: String -- the resource-key of the missing delta
  , expectedVersion :: Int -- the version we expected but didn't receive
  }

-- | A pending transaction stored in PouchDB.
-- | The _id is derived deterministically from the transaction (see pendingTransactionDocId),
-- | so a redelivered transaction maps onto the same record.
-- | status is blockedStatus (retried automatically) or failedStatus (retried only at startup).
-- | missingDeltas is empty for a transaction whose execution failed with an exception;
-- | lastError then holds the reason.
newtype PendingTransactionRecord = PendingTransactionRecord
  { _id :: String
  , _rev :: Maybe String
  , transaction :: TransactionForPeer
  , missingDeltas :: Array MissingDelta
  , status :: String
  , attempts :: Int
  , lastError :: Maybe String
  -- | Bookkeeping for history requests (§10.6), in milliseconds since the epoch:
  -- | when the transaction was first stored, when history was last requested for it,
  -- | and how often.
  , blockedSince :: Maybe Number
  , historyRequestedAt :: Maybe Number
  , historyRequests :: Int
  }

derive newtype instance WriteForeign PendingTransactionRecord
-- | Records written before status, attempts and lastError existed are read as blocked,
-- | without failed attempts, so that the drain still retries them. Records without
-- | history-request bookkeeping are read as never requested.
instance ReadForeign PendingTransactionRecord where
  readImpl f = do
    (r :: { _id :: String, _rev :: Maybe String, transaction :: TransactionForPeer, missingDeltas :: Array MissingDelta, status :: Maybe String, attempts :: Maybe Int, lastError :: Maybe String, blockedSince :: Maybe Number, historyRequestedAt :: Maybe Number, historyRequests :: Maybe Int }) <- readImpl f
    pure $ PendingTransactionRecord
      { _id: r._id
      , _rev: r._rev
      , transaction: r.transaction
      , missingDeltas: r.missingDeltas
      , status: fromMaybe blockedStatus r.status
      , attempts: fromMaybe 0 r.attempts
      , lastError: r.lastError
      , blockedSince: r.blockedSince
      , historyRequestedAt: r.historyRequestedAt
      , historyRequests: fromMaybe 0 r.historyRequests
      }

blockedStatus :: String
blockedStatus = "Blocked"

failedStatus :: String
failedStatus = "Failed"

-- | After this many failed executions a record is no longer retried after every
-- | incoming transaction, only at startup.
maxFailedAttempts :: Int
maxFailedAttempts = 5

-----------------------------------------------------------
-- DATABASE NAME
-----------------------------------------------------------

-- | The name of the PouchDB database for pending transactions.
-- | Convention: {systemIdentifier}_pendingtransactions
pendingTransactionDatabaseName :: forall f. MonadPouchdb f String
pendingTransactionDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_pendingtransactions")

-----------------------------------------------------------
-- OPERATIONS
-----------------------------------------------------------

-- | Store (upsert) a transaction that is blocked by version gaps.
-- | An existing record for the same transaction is updated in place: its attempts and
-- | lastError are kept, its missing deltas replaced and its status reset to blocked.
-- | Storing the same transaction twice (redelivery) is therefore safe.
-- | History-request bookkeeping is kept: a retry that blocks again does not restart the clock.
storePendingTransaction :: forall f. TransactionForPeer -> Array MissingDelta -> MonadPouchdb f Unit
storePendingTransaction txn missingDeltas = do
  time <- currentTime
  upsert txn \existing -> case existing of
    Nothing -> { missingDeltas, status: blockedStatus, attempts: 0, lastError: Nothing, blockedSince: Just time, historyRequestedAt: Nothing, historyRequests: 0 }
    Just (PendingTransactionRecord r) -> (bookkeeping r) { missingDeltas = missingDeltas, status = blockedStatus, blockedSince = Just $ fromMaybe time r.blockedSince }

-- | Store (upsert) a transaction whose execution failed with an exception.
-- | Increments attempts and records the error; after maxFailedAttempts the status becomes failed.
recordFailedTransaction :: forall f. TransactionForPeer -> String -> MonadPouchdb f Unit
recordFailedTransaction txn reason = do
  time <- currentTime
  upsert txn \existing ->
    let
      previous = case existing of
        Nothing -> { missingDeltas: [], status: blockedStatus, attempts: 0, lastError: Nothing, blockedSince: Just time, historyRequestedAt: Nothing, historyRequests: 0 }
        Just (PendingTransactionRecord r) -> bookkeeping r
      attempts = previous.attempts + 1
    in
      previous
        { status = if attempts >= maxFailedAttempts then failedStatus else blockedStatus
        , attempts = attempts
        , lastError = Just reason
        }

-- | Note that history was requested for the pending record with this document id, at `time`.
recordHistoryRequest :: forall f. String -> Number -> MonadPouchdb f Unit
recordHistoryRequest docId time = do
  dbName <- pendingTransactionDatabaseName
  existing <- tryGetDocument_ dbName docId
  case existing of
    Nothing -> pure unit
    Just (PendingTransactionRecord r) -> void $ addDocument_ dbName
      (PendingTransactionRecord r { historyRequestedAt = Just time, historyRequests = r.historyRequests + 1 })
      docId

-- | Remove a pending transaction after it has been executed.
removePendingTransaction :: forall f. String -> Revision_ -> MonadPouchdb f Unit
removePendingTransaction docId rev = do
  dbName <- pendingTransactionDatabaseName
  _ <- deleteDocument dbName docId rev
  pure unit

-- | Remove the pending record of this transaction, if there is one.
removePendingTransactionFor :: forall f. TransactionForPeer -> MonadPouchdb f Unit
removePendingTransactionFor txn = pendingTransactionDocId txn >>= removePendingTransactionById

-- | Remove the pending record with this document ID (using its current revision), if it exists.
removePendingTransactionById :: forall f. String -> MonadPouchdb f Unit
removePendingTransactionById docId = do
  dbName <- pendingTransactionDatabaseName
  existing <- tryGetDocument_ dbName docId
  case existing of
    Nothing -> pure unit
    Just (PendingTransactionRecord { _rev }) -> void $ deleteDocument dbName docId _rev

-- | Get a pending transaction by its document ID.
getPendingTransactionsForResource :: forall f. String -> MonadPouchdb f (Maybe PendingTransactionRecord)
getPendingTransactionsForResource docId = do
  dbName <- pendingTransactionDatabaseName
  tryGetDocument_ dbName docId

-- | Get all pending transactions (with their current _rev), in document-id order.
-- | Documents that cannot be read as a PendingTransactionRecord are skipped.
getAllPendingTransactions :: forall f. MonadPouchdb f (Array PendingTransactionRecord)
getAllPendingTransactions = do
  dbName <- pendingTransactionDatabaseName
  { rows } <- documentsInRange dbName "pending_" "pending_\xFFFF"
  pure $ catMaybes $ map (\row -> row.doc >>= readRecord) rows
  where
  readRecord :: Foreign -> Maybe PendingTransactionRecord
  readRecord = hush <<< read

-----------------------------------------------------------
-- HELPERS
-----------------------------------------------------------

type Bookkeeping =
  { missingDeltas :: Array MissingDelta
  , status :: String
  , attempts :: Int
  , lastError :: Maybe String
  , blockedSince :: Maybe Number
  , historyRequestedAt :: Maybe Number
  , historyRequests :: Int
  }

bookkeeping :: forall r. { missingDeltas :: Array MissingDelta, status :: String, attempts :: Int, lastError :: Maybe String, blockedSince :: Maybe Number, historyRequestedAt :: Maybe Number, historyRequests :: Int | r } -> Bookkeeping
bookkeeping r =
  { missingDeltas: r.missingDeltas
  , status: r.status
  , attempts: r.attempts
  , lastError: r.lastError
  , blockedSince: r.blockedSince
  , historyRequestedAt: r.historyRequestedAt
  , historyRequests: r.historyRequests
  }

currentTime :: forall f. MonadPouchdb f Number
currentTime = liftEffect $ unwrap <<< unInstant <$> now

upsert
  :: forall f
   . TransactionForPeer
  -> (Maybe PendingTransactionRecord -> Bookkeeping)
  -> MonadPouchdb f Unit
upsert txn update = do
  dbName <- pendingTransactionDatabaseName
  docId <- pendingTransactionDocId txn
  existing <- tryGetDocument_ dbName docId
  let
    fields = update existing
    _rev = case existing of
      Nothing -> Nothing
      Just (PendingTransactionRecord r) -> r._rev
  void $ addDocument_ dbName
    ( PendingTransactionRecord
        { _id: docId
        , _rev
        , transaction: txn
        , missingDeltas: fields.missingDeltas
        , status: fields.status
        , attempts: fields.attempts
        , lastError: fields.lastError
        , blockedSince: fields.blockedSince
        , historyRequestedAt: fields.historyRequestedAt
        , historyRequests: fields.historyRequests
        }
    )
    docId

-- | The deterministic document ID of a pending transaction.
-- | Format: pending_<author>_<timestamp>_<shortHash>, where shortHash is a hash of the
-- | canonical JSON of the transaction. Distinct transactions with the same author and
-- | timestamp get distinct records; the same transaction always maps onto the same record.
pendingTransactionDocId :: forall f. TransactionForPeer -> MonadPouchdb f String
pendingTransactionDocId txn = do
  let { author, timeStamp } = unwrap txn
  canonical <- liftEffect $ canonicalizeJsonString (writeJSON txn)
  hash <- liftAff $ computeDeltaId (unwrap author) canonical
  pure $ "pending_" <> unwrap author <> "_" <> show timeStamp <> "_" <> shortDeltaId hash
