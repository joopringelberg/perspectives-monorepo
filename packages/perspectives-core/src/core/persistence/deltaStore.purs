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

-- | A persistent store for all deltas (locally created and received).
-- | Replaces the current practice of embedding deltas in resource representations
-- | (PerspectContext and PerspectRol). Each delta is stored as a separate document
-- | with a deterministic ID: <resourceKey>_<resourceVersion>_<author>.
-- | See design/deterministic-delta-ordering.md for details.
module Perspectives.Persistence.DeltaStore
  ( DeltaStoreRecord(..)
  , storeDelta
  , storeDeltaFromSignedDelta
  , getDelta
  , getDeltasForResource
  , getDeltasForResourceByDeltaType
  , deltaStoreDatabaseName
  , deltaStoreDocId
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Foreign (Foreign)
import Perspectives.Persistence.API (addDocument_, documentsInRange, tryGetDocument_)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (MonadPouchdb)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Simple.JSON (class ReadForeign, class WriteForeign, read', readJSON')

-----------------------------------------------------------
-- TYPES
-----------------------------------------------------------

-- | A record stored in the delta-store PouchDB database.
-- | Document ID: <resourceKey>_<resourceVersion>_<author>
newtype DeltaStoreRecord = DeltaStoreRecord
  { _id :: String
  , _rev :: Maybe String
  , resourceKey :: String
  , resourceVersion :: Int
  , author :: PerspectivesUser
  , signedDelta :: SignedDelta
  , deltaType :: String
  , applied :: Boolean
  }

derive newtype instance WriteForeign DeltaStoreRecord
derive newtype instance ReadForeign DeltaStoreRecord

-----------------------------------------------------------
-- DATABASE NAME
-----------------------------------------------------------

-- | The name of the PouchDB database for the delta-store.
-- | Convention: {systemIdentifier}_deltastore
deltaStoreDatabaseName :: forall f. MonadPouchdb f String
deltaStoreDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_deltastore")

-----------------------------------------------------------
-- DOCUMENT ID
-----------------------------------------------------------

-- | Construct the deterministic document ID for a delta in the store.
-- | Format: <resourceKey>_v<resourceVersion>_<author>
-- | This is unique: in a conflict, two deltas share resourceVersion but have different authors.
deltaStoreDocId :: String -> Int -> PerspectivesUser -> String
deltaStoreDocId resourceKey resourceVersion author =
  resourceKey <> "_v" <> show resourceVersion <> "_" <> unwrap author

-----------------------------------------------------------
-- OPERATIONS
-----------------------------------------------------------

-- | Store a delta in the delta-store.
-- | If a delta with the same document ID already exists, this is a no-op
-- | (deltas are immutable once created).
storeDelta :: forall f. DeltaStoreRecord -> MonadPouchdb f Unit
storeDelta rec@(DeltaStoreRecord { _id }) = do
  dbName <- deltaStoreDatabaseName
  -- Check if the delta already exists to avoid overwriting.
  (existing :: Maybe DeltaStoreRecord) <- tryGetDocument_ dbName _id
  case existing of
    Just _ -> pure unit -- Already stored, nothing to do.
    Nothing -> do
      _ <- addDocument_ dbName rec _id
      pure unit

-- | Retrieve a specific delta from the store by its document ID.
getDelta :: forall f. String -> MonadPouchdb f (Maybe DeltaStoreRecord)
getDelta docId = do
  dbName <- deltaStoreDatabaseName
  tryGetDocument_ dbName docId

-- | Retrieve all deltas for a given resource-key, sorted by resourceVersion.
-- | Uses PouchDB allDocs range query with startkey/endkey to efficiently
-- | find all documents matching the resourceKey prefix.
getDeltasForResource :: forall f. String -> MonadPouchdb f (Array DeltaStoreRecord)
getDeltasForResource resourceKey = do
  dbName <- deltaStoreDatabaseName
  -- Document IDs follow the pattern: <resourceKey>_v<version>_<author>
  -- Use \uffff as the end sentinel to match all suffixes.
  let startkey = resourceKey <> "_v"
  let endkey = resourceKey <> "_v\xFFFF"
  result <- documentsInRange dbName startkey endkey
  pure $ catMaybes $ map decodeDoc result.rows
  where
  decodeDoc :: { id :: String, value :: { rev :: String }, doc :: Maybe Foreign } -> Maybe DeltaStoreRecord
  decodeDoc { doc: Just foreignDoc } = case runExcept $ read' foreignDoc of
    Right (rec :: DeltaStoreRecord) -> Just rec
    Left _ -> Nothing
  decodeDoc _ = Nothing

-- | Retrieve all deltas for a given resource-key, filtered by deltaType.
-- | Useful for finding e.g. only property deltas or only binding deltas.
getDeltasForResourceByDeltaType :: forall f. String -> String -> MonadPouchdb f (Array DeltaStoreRecord)
getDeltasForResourceByDeltaType resourceKey deltaType = do
  allDeltas <- getDeltasForResource resourceKey
  pure $ filter (\(DeltaStoreRecord r) -> r.deltaType == deltaType) allDeltas

-----------------------------------------------------------
-- STORE FROM SIGNED DELTA
-----------------------------------------------------------

-- | Minimal record extracted from a stringified delta JSON to get ordering info and delta type.
type DeltaInfo =
  { resourceKey :: String
  , resourceVersion :: Int
  , deltaType :: String
  }

-- | Extract ordering info and deltaType from the encrypted/stringified delta JSON.
extractDeltaInfo :: String -> Maybe DeltaInfo
extractDeltaInfo stringifiedDelta = case runExcept $ readJSON' stringifiedDelta of
  Right (info :: DeltaInfo) -> Just info
  Left _ -> Nothing

-- | Store a locally-created delta from a SignedDelta.
-- | Extracts resourceKey, resourceVersion and deltaType from the encryptedDelta string.
-- | This is a no-op if the ordering fields cannot be extracted.
storeDeltaFromSignedDelta :: forall f. SignedDelta -> MonadPouchdb f Unit
storeDeltaFromSignedDelta signedDelta@(SignedDelta { author, encryptedDelta }) =
  case extractDeltaInfo encryptedDelta of
    Nothing -> pure unit -- Cannot extract ordering info, skip storage
    Just { resourceKey, resourceVersion, deltaType } -> do
      let docId = deltaStoreDocId resourceKey resourceVersion author
      storeDelta $ DeltaStoreRecord
        { _id: docId
        , _rev: Nothing
        , resourceKey
        , resourceVersion
        , author
        , signedDelta
        , deltaType
        , applied: true -- Locally-created deltas are applied immediately
        }
