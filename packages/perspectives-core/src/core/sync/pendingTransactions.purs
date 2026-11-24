-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

-- | Running incoming peer transactions and retrying the ones that were stored in the
-- | PendingTransactionStore (blocked by version gaps or failed with an exception).
-- | See docsources/delta-ordering.md §10 (unblocking).
-- |
-- | All functions in this module run in MonadPerspectives, *outside* any transaction:
-- | each (re)execution is its own outermost transaction, admitted as background work.
-- | Never call them from within a MonadPerspectivesTransaction or from within
-- | withTransactionPriority: that would deadlock on the transaction flag or the admission queue.
module Perspectives.Sync.PendingTransactions
  ( DrainMode(..)
  , runIncomingTransaction
  , settleIncomingTransaction
  , drainPendingTransactions
  ) where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Error.Class (try)
import Data.Array (filterA, length, null, sortBy)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Ref as Ref
import Perspectives.CoreTypes (MonadPerspectives, PendingDrainGuard(..))
import Perspectives.Logging (traceSync, warnSync)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Persistence.PendingTransactionStore (PendingTransactionRecord(..), blockedStatus, getAllPendingTransactions, getPendingTransactionsForResource, pendingTransactionDocId, recordFailedTransaction, removePendingTransactionById, removePendingTransactionFor)
import Perspectives.Persistence.ResourceVersionStore (getResourceVersion)
import Perspectives.PerspectivesState (withTransactionPriority)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (detectPublicStateChanges, runMonadPerspectivesTransaction')
import Perspectives.Sync.HandleTransaction (TransactionOutcome(..), executeTransactionWithOutcome)
import Perspectives.Sync.HistoryRequest (requestMissingHistory)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Perspectives.TransactionQueue (TransactionPriority(..))

-- | AfterIncoming: retry only blocked records that may have become executable: those whose
-- |   recorded gap keys have advanced, and those that failed with an exception.
-- | AtStartup: retry every record once, including those with failed status, so that records
-- |   stored before a restart are reconsidered. Later passes of the same drain use AfterIncoming.
data DrainMode = AfterIncoming | AtStartup

derive instance Eq DrainMode

-- | Execute an incoming transaction as an outermost, non-sharing background transaction and
-- | maintain the PendingTransactionStore accordingly. Does not drain.
runIncomingTransaction :: TransactionForPeer -> MonadPerspectives Unit
runIncomingTransaction t = do
  outcome <- try $ withTransactionPriority BackgroundTransaction do
    o <- runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser) (executeTransactionWithOutcome t)
    detectPublicStateChanges
    pure o
  void $ settleIncomingTransaction t outcome

-- | Record the result of (re)executing an incoming transaction in the PendingTransactionStore.
-- | * An exception is never lost: the transaction is stored as pending, with the error (I-10).
-- | * A blocked transaction has already been stored by executeTransaction'.
-- | * Otherwise a pending record for this transaction (if any) is removed.
-- | Returns true iff the transaction no longer needs to be retried.
settleIncomingTransaction :: TransactionForPeer -> Either Error TransactionOutcome -> MonadPerspectives Boolean
settleIncomingTransaction t = case _ of
  Left e -> do
    warnSync $ "Incoming transaction failed and is kept for a retry: " <> show e
    recordFailedTransaction t (show e)
    pure false
  Right (Blocked _) -> pure false
  Right (Rejected reason) -> do
    -- A transaction that could not be verified changes nothing. If it was stored before,
    -- count this as a failed attempt; otherwise drop it, as before.
    known <- pendingTransactionDocId t >>= getPendingTransactionsForResource
    case known of
      Just _ -> recordFailedTransaction t reason *> pure false
      Nothing -> pure true
  Right Executed -> removePendingTransactionFor t *> pure true
  Right Quarantined -> removePendingTransactionFor t *> pure true

-- | Retry pending transactions until a pass makes no progress.
-- | Each retry is its own outermost background transaction that goes through the complete
-- | receive pipeline (epoch, verification, baselines, gap check).
-- | If a drain is requested while one is running, the running drain makes another pass.
-- | Finally, history is requested for transactions that have been blocked for too long.
drainPendingTransactions :: DrainMode -> MonadPerspectives Unit
drainPendingTransactions mode = do
  PendingDrainGuard guard <- gets _.pendingTransactionDrain
  { draining } <- liftEffect $ Ref.read guard
  if draining then liftEffect $ Ref.modify_ (_ { dirty = true }) guard
  else do
    liftEffect $ Ref.write { draining: true, dirty: false } guard
    result <- try (pass guard mode)
    -- What is still blocked after the passes may need a history request (§10.6).
    requested <- try requestMissingHistory
    liftEffect $ Ref.write { draining: false, dirty: false } guard
    case result of
      Left e -> warnSync $ "Draining pending transactions failed: " <> show e
      Right _ -> pure unit
    case requested of
      Left e -> warnSync $ "Requesting missing history failed: " <> show e
      Right _ -> pure unit

  where
  pass :: Ref.Ref { draining :: Boolean, dirty :: Boolean } -> DrainMode -> MonadPerspectives Unit
  pass guard passMode = do
    liftEffect $ Ref.modify_ (_ { dirty = false }) guard
    records <- getAllPendingTransactions
    candidates <- filterA (eligible passMode) (sortBy byTime records)
    if null candidates then pure unit
    else do
      traceSync $ "Retrying " <> show (length candidates) <> " pending transaction(s)"
      progress <- for candidates retry
      { dirty } <- liftEffect $ Ref.read guard
      if any identity progress || dirty then pass guard AfterIncoming else pure unit

  byTime :: PendingTransactionRecord -> PendingTransactionRecord -> Ordering
  byTime (PendingTransactionRecord a) (PendingTransactionRecord b) =
    compare (unwrap a.transaction).timeStamp (unwrap b.transaction).timeStamp
      <> compare (unwrap a.transaction).author (unwrap b.transaction).author

  eligible :: DrainMode -> PendingTransactionRecord -> MonadPerspectives Boolean
  eligible AtStartup _ = pure true
  eligible AfterIncoming (PendingTransactionRecord r) =
    if r.status /= blockedStatus then pure false
    -- A failed execution has no recorded gaps; retry it (bounded by maxFailedAttempts).
    else if null r.missingDeltas then pure true
    -- A record is worth a full retry once one of its recorded gaps has been filled.
    else do
      advanced <- for r.missingDeltas \m -> (_ >= m.expectedVersion) <$> getResourceVersion m.resourceKey
      pure $ any identity advanced

  retry :: PendingTransactionRecord -> MonadPerspectives Boolean
  retry (PendingTransactionRecord r) = do
    outcome <- try $ withTransactionPriority BackgroundTransaction do
      o <- runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser) (executeTransactionWithOutcome r.transaction)
      detectPublicStateChanges
      pure o
    done <- settleIncomingTransaction r.transaction outcome
    -- Remove the record we retried by its own identifier as well, so that a drain can never
    -- keep retrying a record that it considers handled.
    when done (removePendingTransactionById r._id)
    pure done
