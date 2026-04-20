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

-- | Data upgrade for PDR version 3.2.0.
-- |
-- | DeltaStoreRecord gained a `contextKey` field (type `Maybe String`) in 3.2.0.
-- | For ContextDelta records (deltaType ∈ {AddRoleInstancesToContext, AddExternalRole,
-- | MoveRoleInstancesToAnotherContext}), `contextKey` holds the safe-key form of the
-- | referenced context instance.  This enables `getDeltasForContextKey` to filter by
-- | context without deserialising each `signedDelta`.
-- |
-- | Records created before 3.2.0 have `contextKey = Nothing`.  This migration scans
-- | all DeltaStore documents, and for each ContextDelta record whose `contextKey` is
-- | still `Nothing`, extracts the `contextInstance` from the signed-delta payload and
-- | writes the record back with the correct `contextKey`.
module Perspectives.DataUpgrade.AddContextKeyMigration
  ( addContextKeyToDeltas
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array (length, null, splitAt)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Foreign (Foreign)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Persistence.API (addDocument_, documentsInDatabase, includeDocs)
import Perspectives.Persistence.DeltaStore (deltaStoreDatabaseName, safeKey)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Simple.JSON (read', readJSON')

-----------------------------------------------------------
-- ENTRY POINT
-----------------------------------------------------------

-- | Scan all DeltaStore documents and populate the `contextKey` field for
-- | ContextDelta records that were stored before version 3.2.0.
-- | This is idempotent: records that already have `contextKey` set are skipped.
addContextKeyToDeltas :: Unit -> MonadPerspectives Unit
addContextKeyToDeltas _ = do
  dbName <- deltaStoreDatabaseName
  { rows } <- documentsInDatabase dbName includeDocs
  log ("AddContextKeyMigration: processing " <> show (length rows) <> " delta-store documents")
  void $ tailRecM (processChunk dbName 200) rows

processChunk :: forall r. String -> Int -> Array { doc :: Maybe Foreign | r } -> MonadPerspectives (Step (Array { doc :: Maybe Foreign | r }) Unit)
processChunk dbName chunkSize remaining = do
  let { before: batch, after: rest } = splitAt chunkSize remaining
  for_ batch \{ doc } -> case doc of
    Nothing -> pure unit
    Just foreignDoc -> processDoc dbName foreignDoc
  if null rest then pure (Done unit)
  else do
    -- Yield to Aff between batches to avoid deep synchronous call chains.
    liftAff $ delay (Milliseconds 0.0)
    pure (Loop rest)

-----------------------------------------------------------
-- PER-DOCUMENT PROCESSING
-----------------------------------------------------------

processDoc :: String -> Foreign -> MonadPerspectives Unit
processDoc dbName foreignDoc =
  case runExcept (read' foreignDoc) of
    Left _ -> pure unit -- Not a DeltaStoreRecord (e.g. design doc); skip.
    Right (DeltaStoreRecord r) ->
      -- Only process ContextDelta records that are missing contextKey.
      if isContextDeltaType r.deltaType && r.contextKey == Nothing then do
        let encDelta = (unwrap r.signedDelta).encryptedDelta
        case runExcept (readJSON' encDelta) of
          Right ({ contextInstance } :: { contextInstance :: String }) -> do
            let updated = DeltaStoreRecord r { contextKey = Just (safeKey contextInstance) }
            void $ addDocument_ dbName updated r._id
          Left _ -> pure unit -- Cannot parse; leave contextKey as Nothing.
      else pure unit

-----------------------------------------------------------
-- HELPERS
-----------------------------------------------------------

-- | Returns true when the deltaType string belongs to one of the three ContextDelta
-- | subtypes that carry a `contextInstance` reference.
isContextDeltaType :: String -> Boolean
isContextDeltaType dt =
  dt == "AddRoleInstancesToContext"
    || dt == "AddExternalRole"
    || dt == "MoveRoleInstancesToAnotherContext"
