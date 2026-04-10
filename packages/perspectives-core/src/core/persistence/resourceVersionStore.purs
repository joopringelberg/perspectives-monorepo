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

-- | A persistent store for resource version numbers.
-- | Each resource-key (see deterministic-delta-ordering.md) maps to a version number (Int).
-- | This store is backed by a dedicated PouchDB database so it survives restarts.
module Perspectives.Persistence.ResourceVersionStore
  ( ResourceKey
  , getResourceVersion
  , incrementResourceVersion
  , setResourceVersion
  , initResourceVersion
  , resourceVersionDatabaseName
  ) where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import LRUCache (defaultGetOptions, get, set) as LRU
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Persistence.API (addDocument_, tryGetDocument_)
import Perspectives.Persistence.DeltaStore (safeKey)
import Perspectives.Persistence.State (getSystemIdentifier)
import Simple.JSON (class ReadForeign, class WriteForeign)

-----------------------------------------------------------
-- TYPES
-----------------------------------------------------------

-- | A resource-key uniquely identifies the resource a delta operates on.
-- | See the design document for the granularity:
-- |   - UniverseContextDelta:  ContextInstance (unwrapped)
-- |   - UniverseRoleDelta:     RoleInstance (unwrapped)
-- |   - ContextDelta:          RoleInstance (unwrapped)
-- |   - RoleBindingDelta:      RoleInstance + "#binding"
-- |   - RolePropertyDelta:     RoleInstance + "#" + PropertyType
type ResourceKey = String

-- | PouchDB document for storing a resource version.
-- | The _id field is the resource-key. The _rev field is required for PouchDB updates.
newtype ResourceVersionDoc = ResourceVersionDoc
  { _id :: String
  , _rev :: Maybe String
  , resourceVersion :: Int
  }

derive newtype instance WriteForeign ResourceVersionDoc
derive newtype instance ReadForeign ResourceVersionDoc

-----------------------------------------------------------
-- DATABASE NAME
-----------------------------------------------------------

-- | The name of the PouchDB database for resource versions.
-- | Convention: {systemIdentifier}_resourceversions
resourceVersionDatabaseName :: MonadPerspectives String
resourceVersionDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_resourceversions")

-----------------------------------------------------------
-- CACHE HELPERS
-----------------------------------------------------------

-- | Insert a resource version into the in-memory cache.
cacheInsertVersion :: String -> Int -> MonadPerspectives Unit
cacheInsertVersion key v = do
  cache <- gets _.resourceVersionCache
  void $ liftEffect $ LRU.set key v Nothing cache

-- | Look up a resource version in the in-memory cache.
cacheLookupVersion :: String -> MonadPerspectives (Maybe Int)
cacheLookupVersion key = do
  cache <- gets _.resourceVersionCache
  liftEffect $ LRU.get key LRU.defaultGetOptions cache

-----------------------------------------------------------
-- OPERATIONS
-----------------------------------------------------------

-- | Get the current version number for a resource-key.
-- | Checks the in-memory cache first; falls back to PouchDB on a miss.
-- | Returns 0 if the resource-key is not yet tracked (i.e., a newly created resource).
getResourceVersion :: ResourceKey -> MonadPerspectives Int
getResourceVersion key = do
  let sk = safeKey key
  mCached <- cacheLookupVersion sk
  case mCached of
    Just v -> pure v
    Nothing -> do
      dbName <- resourceVersionDatabaseName
      (mDoc :: Maybe ResourceVersionDoc) <- tryGetDocument_ dbName sk
      case mDoc of
        Nothing -> pure 0
        Just (ResourceVersionDoc { resourceVersion }) -> do
          cacheInsertVersion sk resourceVersion
          pure resourceVersion

-- | Initialise the version for a resource-key to 0.
-- | This is a no-op if the key already exists.
initResourceVersion :: ResourceKey -> MonadPerspectives Unit
initResourceVersion key = do
  dbName <- resourceVersionDatabaseName
  let sk = safeKey key
  (mDoc :: Maybe ResourceVersionDoc) <- tryGetDocument_ dbName sk
  case mDoc of
    Nothing -> do
      _ <- addDocument_ dbName (ResourceVersionDoc { _id: sk, _rev: Nothing, resourceVersion: 0 }) sk
      cacheInsertVersion sk 0
    Just _ -> pure unit

-- | Set the version for a resource-key to a specific value.
-- | Creates the document if it does not exist; updates it otherwise.
-- | Also updates the in-memory cache after a successful database write.
setResourceVersion :: ResourceKey -> Int -> MonadPerspectives Unit
setResourceVersion key version = do
  dbName <- resourceVersionDatabaseName
  let sk = safeKey key
  (mDoc :: Maybe ResourceVersionDoc) <- tryGetDocument_ dbName sk
  case mDoc of
    Nothing -> do
      _ <- addDocument_ dbName (ResourceVersionDoc { _id: sk, _rev: Nothing, resourceVersion: version }) sk
      cacheInsertVersion sk version
    Just (ResourceVersionDoc { _rev }) -> do
      _ <- addDocument_ dbName (ResourceVersionDoc { _id: sk, _rev, resourceVersion: version }) sk
      cacheInsertVersion sk version

-- | Increment the version for a resource-key by 1 and return the new version.
-- | If the key does not exist yet, it is created with version 1.
-- | This is the function to call when constructing a local delta:
-- |   1. Call incrementResourceVersion to get the new version
-- |   2. Stamp the delta with that version
-- | Also updates the in-memory cache with the new version.
incrementResourceVersion :: ResourceKey -> MonadPerspectives Int
incrementResourceVersion key = do
  dbName <- resourceVersionDatabaseName
  let sk = safeKey key
  (mDoc :: Maybe ResourceVersionDoc) <- tryGetDocument_ dbName sk
  case mDoc of
    Nothing -> do
      _ <- addDocument_ dbName (ResourceVersionDoc { _id: sk, _rev: Nothing, resourceVersion: 1 }) sk
      cacheInsertVersion sk 1
      pure 1
    Just (ResourceVersionDoc { _rev, resourceVersion }) -> do
      let newVersion = resourceVersion + 1
      _ <- addDocument_ dbName (ResourceVersionDoc { _id: sk, _rev, resourceVersion: newVersion }) sk
      cacheInsertVersion sk newVersion
      pure newVersion
