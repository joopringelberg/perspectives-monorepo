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

-- | Migration that merges the separate _resourceversions database into _deltastore.
-- |
-- | Before PDR 3.1.3, resource version numbers were tracked in a dedicated database
-- | ({systemIdentifier}_resourceversions). From 3.1.3 onwards they are stored in
-- | _deltastore alongside the delta documents. Document IDs do not clash: version
-- | documents use the bare safe resource key as their ID, whereas delta documents
-- | use the form <safeKey>_v<resourceVersion>_<author>.
-- |
-- | This migration copies every ResourceVersionDoc from _resourceversions into
-- | _deltastore (skipping any that are already present) so that a clean state is
-- | established before the old database becomes unused.
module Perspectives.DataUpgrade.ResourceVersionsMigration
  ( mergeResourceVersionsIntoDeltaStore
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
import Perspectives.Persistence.API (addDocument_, documentsInDatabase, includeDocs, tryGetDocument_)
import Perspectives.Persistence.DeltaStore (deltaStoreDatabaseName)
import Perspectives.Persistence.ResourceVersionStore (resourceVersionDatabaseName)
import Simple.JSON (class ReadForeign, class WriteForeign, read')

-----------------------------------------------------------
-- RESOURCE VERSION DOC (local copy matching ResourceVersionStore shape)
-----------------------------------------------------------

newtype ResourceVersionDoc = ResourceVersionDoc
  { _id :: String
  , _rev :: Maybe String
  , resourceVersion :: Int
  }

derive newtype instance WriteForeign ResourceVersionDoc
derive newtype instance ReadForeign ResourceVersionDoc

-----------------------------------------------------------
-- MIGRATION
-----------------------------------------------------------

-- | Copy all ResourceVersionDoc documents from the legacy _resourceversions
-- | database into _deltastore.  Documents already present in _deltastore are
-- | left untouched (idempotent).
mergeResourceVersionsIntoDeltaStore :: MonadPerspectives Unit
mergeResourceVersionsIntoDeltaStore = do
  srcDb  <- resourceVersionDatabaseName
  dstDb  <- deltaStoreDatabaseName
  { rows } <- documentsInDatabase srcDb includeDocs
  log ("ResourceVersionsMigration: copying " <> show (length rows) <> " resource-version documents to _deltastore")
  for_ rows \{ doc } -> case doc of
    Nothing -> pure unit
    Just foreignDoc -> migrateDoc dstDb foreignDoc

migrateDoc :: String -> Foreign -> MonadPerspectives Unit
migrateDoc dstDb foreignDoc =
  case runExcept (read' foreignDoc) of
    Right (ResourceVersionDoc r) -> do
      -- Only copy if the document is not already present in _deltastore.
      (existing :: Maybe ResourceVersionDoc) <- tryGetDocument_ dstDb r._id
      case existing of
        Just _  -> pure unit -- Already migrated.
        Nothing ->
          -- Store without the old _rev so PouchDB/CouchDB treats it as a new document.
          void $ addDocument_ dstDb (ResourceVersionDoc r { _rev = Nothing }) r._id
    Left _ -> pure unit -- Not a ResourceVersionDoc (e.g. design doc); skip.
