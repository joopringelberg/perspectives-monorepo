-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Perspectives.Persistence.QuarantineStore where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Aff.Class (liftAff)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Persistence.API (addDocument_, documentsInDatabase, getDocument_, tryGetDocument_)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Sync.CanonicalJson (computeDeltaId)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)

newtype QuarantinedTransaction = QuarantinedTransaction
  { _id :: String
  , _rev :: Maybe String
  , reason :: String
  , transaction :: TransactionForPeer
  }

derive newtype instance ReadForeign QuarantinedTransaction
derive newtype instance WriteForeign QuarantinedTransaction

quarantineDatabaseName :: MonadPerspectives String
quarantineDatabaseName = (_ <> "_quarantinedtransactions") <$> getSystemIdentifier

quarantineTransaction :: String -> TransactionForPeer -> MonadPerspectives String
quarantineTransaction reason transaction = do
  db <- quarantineDatabaseName
  id <- liftAff $ computeDeltaId (unwrap (unwrap transaction).author) (writeJSON transaction)
  existing <- tryGetDocument_ db id
  case existing of
    Just (_ :: QuarantinedTransaction) -> pure unit
    Nothing -> void $ addDocument_ db (QuarantinedTransaction { _id: id, _rev: Nothing, reason, transaction }) id
  pure id

getQuarantinedTransactions :: MonadPerspectives (Array QuarantinedTransaction)
getQuarantinedTransactions = do
  db <- quarantineDatabaseName
  documents <- documentsInDatabase db false
  traverse (\row -> getDocument_ db row.id) documents.rows
