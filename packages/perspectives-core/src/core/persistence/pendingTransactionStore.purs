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

-- | Persistent storage for transactions that are blocked because of version gaps.
-- | When a received transaction contains a delta whose resourceVersion > local + 1,
-- | the entire transaction is blocked (causal dependency via automatic actions).
-- | Blocked transactions are stored here so they survive session restarts.
-- | See design/deterministic-delta-ordering.md for details.
module Perspectives.Persistence.PendingTransactionStore
  ( PendingTransactionRecord(..)
  , MissingDelta(..)
  , storePendingTransaction
  , removePendingTransaction
  , getPendingTransactionsForResource
  , getAllPendingTransactions
  , pendingTransactionDatabaseName
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.Persistence.API (addDocument_, tryGetDocument_, deleteDocument)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (MonadPouchdb)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Perspectives.Couchdb.Revision (Revision_)
import Simple.JSON (class ReadForeign, class WriteForeign)

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

-- | A pending (blocked) transaction stored in PouchDB.
-- | The _id is a unique identifier derived from the transaction author + timestamp.
newtype PendingTransactionRecord = PendingTransactionRecord
  { _id :: String
  , _rev :: Maybe String
  , transaction :: TransactionForPeer
  , missingDeltas :: Array MissingDelta
  }

derive newtype instance WriteForeign PendingTransactionRecord
derive newtype instance ReadForeign PendingTransactionRecord

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

-- | Store a blocked transaction.
-- | The document ID is derived from the transaction author and a unique suffix
-- | to allow multiple pending transactions from the same author.
storePendingTransaction :: forall f. TransactionForPeer -> Array MissingDelta -> MonadPouchdb f Unit
storePendingTransaction txn missingDeltas = do
  dbName <- pendingTransactionDatabaseName
  let docId = pendingTransactionDocId txn
  _ <- addDocument_ dbName
    ( PendingTransactionRecord
        { _id: docId
        , _rev: Nothing
        , transaction: txn
        , missingDeltas
        }
    )
    docId
  pure unit

-- | Remove a pending transaction after it has been executed.
removePendingTransaction :: forall f. String -> Revision_ -> MonadPouchdb f Unit
removePendingTransaction docId rev = do
  dbName <- pendingTransactionDatabaseName
  _ <- deleteDocument dbName docId rev
  pure unit

-- | Get a pending transaction by its document ID.
getPendingTransactionsForResource :: forall f. String -> MonadPouchdb f (Maybe PendingTransactionRecord)
getPendingTransactionsForResource docId = do
  dbName <- pendingTransactionDatabaseName
  tryGetDocument_ dbName docId

-- | Get all pending transactions.
-- | TODO: Implement using allDocs or a view for efficiency.
-- | For now, this is a placeholder that should be replaced with proper query logic.
getAllPendingTransactions :: forall f. MonadPouchdb f (Array PendingTransactionRecord)
getAllPendingTransactions = do
  -- TODO: Use allDocs to retrieve all documents from the pending transactions database.
  pure []

-----------------------------------------------------------
-- HELPERS
-----------------------------------------------------------

-- | Create a unique document ID for a pending transaction.
-- | Format: pending_<author>_<timestamp>
pendingTransactionDocId :: TransactionForPeer -> String
pendingTransactionDocId txn =
  let
    { author, timeStamp } = unwrap txn
  in
    "pending_" <> unwrap author <> "_" <> show timeStamp
