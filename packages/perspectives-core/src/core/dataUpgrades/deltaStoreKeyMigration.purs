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

-- | Migration from old (URL-unsafe) document IDs in the DeltaStore and
-- | ResourceVersionStore to the new URL-safe format produced by 'safeKey'.
-- |
-- | Old document IDs contained full resource identifiers with scheme prefixes
-- | (def:#, loc:, rem:, model://) and full property type URIs, leading to
-- | percent-encoded forward slashes (%2F) in CouchDB URLs. These are rejected
-- | by many reverse proxies.
-- |
-- | This migration reads every document, checks whether its _id would change
-- | under the new 'safeKey' scheme, and if so creates a replacement document
-- | with the safe _id and deletes the old one.
module Perspectives.DataUpgrade.DeltaStoreKeyMigration
  ( migrateDeltaStoreKeys
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Class.Console (log)
import Foreign (Foreign)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Persistence.API (addDocument_, deleteDocument, documentsInDatabase, includeDocs, tryGetDocument_)
import Perspectives.Persistence.DeltaStore (DeltaStoreRecord(..), deltaStoreDatabaseName, deltaStoreDocId, safeKey)
import Perspectives.Persistence.ResourceVersionStore (resourceVersionDatabaseName)
import Simple.JSON (class ReadForeign, class WriteForeign, read')

-----------------------------------------------------------
-- RESOURCE VERSION DOC (local copy for reading old docs)
-----------------------------------------------------------

-- | Matches the shape of ResourceVersionDoc in ResourceVersionStore.
newtype ResourceVersionDoc = ResourceVersionDoc
  { _id :: String
  , _rev :: Maybe String
  , resourceVersion :: Int
  }

derive newtype instance WriteForeign ResourceVersionDoc
derive newtype instance ReadForeign ResourceVersionDoc

-----------------------------------------------------------
-- ENTRY POINT
-----------------------------------------------------------

-- | Migrate all DeltaStore and ResourceVersionStore documents from old
-- | URL-unsafe document IDs to the new safe format.
-- |
-- | For each document whose _id differs from the safe _id:
-- |   1. Create a new document with the safe _id (preserving all fields).
-- |   2. Delete the old document.
-- |
-- | Documents that already have safe IDs are left untouched.
-- | This is idempotent: running it again is a no-op.
migrateDeltaStoreKeys :: MonadPerspectives Unit
migrateDeltaStoreKeys = do
  migrateDeltaStoreDocs
  migrateResourceVersionDocs

-----------------------------------------------------------
-- DELTA STORE MIGRATION
-----------------------------------------------------------

migrateDeltaStoreDocs :: MonadPerspectives Unit
migrateDeltaStoreDocs = do
  dbName <- deltaStoreDatabaseName
  { rows } <- documentsInDatabase dbName includeDocs
  log ("DeltaStoreKeyMigration: processing " <> show (length rows) <> " delta-store documents")
  for_ rows \{ doc } -> case doc of
    Nothing -> pure unit
    Just foreignDoc -> migrateDeltaDoc dbName foreignDoc

migrateDeltaDoc :: String -> Foreign -> MonadPerspectives Unit
migrateDeltaDoc dbName foreignDoc =
  case runExcept (read' foreignDoc) of
    Right (DeltaStoreRecord r) -> do
      -- Recompute the _id using the current (safe) scheme.
      -- deltaStoreDocId already applies safeKey to resourceKey and takeGuid to author.
      let newId = deltaStoreDocId r.resourceKey r.resourceVersion r.author
      when (newId /= r._id) do
        -- Check whether the target document already exists (partial previous run).
        (existing :: Maybe DeltaStoreRecord) <- tryGetDocument_ dbName newId
        case existing of
          Just _ -> pure unit -- Already migrated.
          Nothing -> do
            let
              newRec = DeltaStoreRecord r
                { _id = newId
                , _rev = Nothing
                , resourceKey = safeKey r.resourceKey
                }
            void $ addDocument_ dbName newRec newId
        -- Delete the old document.
        void $ deleteDocument dbName r._id r._rev
    Left _ -> pure unit -- Not a DeltaStoreRecord (e.g. design doc); skip.

-----------------------------------------------------------
-- RESOURCE VERSION STORE MIGRATION
-----------------------------------------------------------

migrateResourceVersionDocs :: MonadPerspectives Unit
migrateResourceVersionDocs = do
  dbName <- resourceVersionDatabaseName
  { rows } <- documentsInDatabase dbName includeDocs
  log ("DeltaStoreKeyMigration: processing " <> show (length rows) <> " resource-version documents")
  for_ rows \{ doc } -> case doc of
    Nothing -> pure unit
    Just foreignDoc -> migrateVersionDoc dbName foreignDoc

migrateVersionDoc :: String -> Foreign -> MonadPerspectives Unit
migrateVersionDoc dbName foreignDoc =
  case runExcept (read' foreignDoc) of
    Right (ResourceVersionDoc r) -> do
      let newId = safeKey r._id
      when (newId /= r._id) do
        (existing :: Maybe ResourceVersionDoc) <- tryGetDocument_ dbName newId
        case existing of
          Just _ -> pure unit
          Nothing -> do
            let newDoc = ResourceVersionDoc r { _id = newId, _rev = Nothing }
            void $ addDocument_ dbName newDoc newId
        void $ deleteDocument dbName r._id r._rev
    Left _ -> pure unit -- Not a ResourceVersionDoc (e.g. design doc); skip.
