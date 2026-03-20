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
  , getDeltasForRoleInstance
  , updateDeltaApplied
  , extractDeltaInfo
  , deltaStoreDatabaseName
  , deltaStoreDocId
  , safeKey
  ) where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Except (runExcept)
import Data.Array (catMaybes, filter, head, last, length) as Arr
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), drop, indexOf, lastIndexOf, length, split, take) as Str
import Effect.Class (liftEffect)
import Foreign (Foreign)
import LRUCache (defaultGetOptions, get, set) as LRU
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Persistence.API (addDocument_, documentsInRange, tryGetDocument_)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser)
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Simple.JSON (class ReadForeign, class WriteForeign, read', readJSON')

-----------------------------------------------------------
-- DATABASE NAME
-----------------------------------------------------------

-- | The name of the PouchDB database for the delta-store.
-- | Convention: {systemIdentifier}_deltastore
deltaStoreDatabaseName :: MonadPerspectives String
deltaStoreDatabaseName = getSystemIdentifier >>= pure <<< (_ <> "_deltastore")

-----------------------------------------------------------
-- URL-SAFE KEY
-----------------------------------------------------------

-- | Convert a (potentially composite) resource key to a URL-safe string
-- | suitable for CouchDB document IDs.
-- |
-- | Resource keys may contain URI schemes (def:, loc:, rem:, model:) and
-- | full model URIs with '://' which break CouchDB access through reverse
-- | proxies that reject or mangle percent-encoded forward slashes (%2F).
-- |
-- | Transformation rules:
-- |   "def:#somecuid"                                       → "somecuid"
-- |   "def:#somecuid#binding"                               → "somecuid#binding"
-- |   "def:#somecuid#model://domain#A$B$C$D"                → "somecuid#A$D"
-- |   "somecuid"                                            → "somecuid" (already safe)
-- |   "somecuid#binding"                                    → "somecuid#binding" (idempotent)
safeKey :: String -> String
safeKey key =
  let
    guidPart = takeGuid key -- strips scheme prefix (def:#, loc:db#, rem:url#)
  in
    case Str.indexOf (Str.Pattern "#") guidPart of
      Nothing -> guidPart -- no suffix, just the CUID
      Just hashIdx ->
        let
          baseCuid = Str.take hashIdx guidPart
          suffix = Str.drop (hashIdx + 1) guidPart
        in
          if suffix == "binding" then guidPart -- already safe
          else baseCuid <> "#" <> shortenPropertyType suffix

-- | Shorten a property type identifier to just model-CUID + type-CUID.
-- | Handles both full URIs and already-shortened forms:
-- |   "model://domain#A$B$C$D"  → "A$D"
-- |   "A$B$C$D"                 → "A$D"   (idempotent)
-- |   "A$D"                     → "A$D"   (idempotent)
-- |   "A"                       → "A"     (single segment)
shortenPropertyType :: String -> String
shortenPropertyType propType =
  let
    -- Strip everything up to and including the last '#' (the model URI part).
    segments = case Str.lastIndexOf (Str.Pattern "#") propType of
      Nothing -> propType
      Just idx -> Str.drop (idx + 1) propType
    parts = Str.split (Str.Pattern "$") segments
    first = fromMaybe segments (Arr.head parts)
    last_ = fromMaybe segments (Arr.last parts)
  in
    if Arr.length parts <= 1 then segments
    else first <> "$" <> last_

-----------------------------------------------------------
-- DOCUMENT ID
-----------------------------------------------------------

-- | Construct the deterministic document ID for a delta in the store.
-- | Format: <safeResourceKey>_v<resourceVersion>_<author>
-- | This is unique: in a conflict, two deltas share resourceVersion but have different authors.
-- | The resourceKey is converted to a URL-safe form via 'safeKey'.
deltaStoreDocId :: String -> Int -> PerspectivesUser -> String
deltaStoreDocId resourceKey resourceVersion author =
  safeKey resourceKey <> "_v" <> show resourceVersion <> "_" <> takeGuid (unwrap author)

-----------------------------------------------------------
-- CACHE HELPERS
-----------------------------------------------------------

-- | Insert a delta record into the in-memory cache.
cacheInsertDelta :: String -> DeltaStoreRecord -> MonadPerspectives Unit
cacheInsertDelta key rec = do
  cache <- gets _.deltaCache
  void $ liftEffect $ LRU.set key rec Nothing cache

-- | Look up a delta record in the in-memory cache.
cacheLookupDelta :: String -> MonadPerspectives (Maybe DeltaStoreRecord)
cacheLookupDelta key = do
  cache <- gets _.deltaCache
  liftEffect $ LRU.get key LRU.defaultGetOptions cache

-- | Populate the cache with a batch of delta records.
populateDeltaCache :: Array DeltaStoreRecord -> MonadPerspectives Unit
populateDeltaCache records = do
  cache <- gets _.deltaCache
  void $ liftEffect $ traverse_ (\rec@(DeltaStoreRecord { _id }) -> LRU.set _id rec Nothing cache) records

-----------------------------------------------------------
-- OPERATIONS
-----------------------------------------------------------

-- | Store a delta in the delta-store.
-- | If a delta with the same document ID already exists, this is a no-op
-- | (deltas are immutable once created). Also stores the delta in the
-- | in-memory cache.
storeDelta :: DeltaStoreRecord -> MonadPerspectives Unit
storeDelta rec@(DeltaStoreRecord { _id }) = do
  dbName <- deltaStoreDatabaseName
  -- Check if the delta already exists to avoid overwriting.
  (existing :: Maybe DeltaStoreRecord) <- tryGetDocument_ dbName _id
  case existing of
    Just existingRec ->
      -- Delta already exists in DB; cache the stored record (which has the correct _rev).
      cacheInsertDelta _id existingRec
    Nothing -> do
      _ <- addDocument_ dbName rec _id
      cacheInsertDelta _id rec

-- | Retrieve a specific delta from the store by its document ID.
-- | Checks the in-memory cache first; falls back to PouchDB on a miss.
getDelta :: String -> MonadPerspectives (Maybe DeltaStoreRecord)
getDelta docId = do
  mCached <- cacheLookupDelta docId
  case mCached of
    Just rec -> pure (Just rec)
    Nothing -> do
      dbName <- deltaStoreDatabaseName
      (mRec :: Maybe DeltaStoreRecord) <- tryGetDocument_ dbName docId
      case mRec of
        Nothing -> pure Nothing
        Just rec -> do
          cacheInsertDelta docId rec
          pure (Just rec)

-- | Retrieve all deltas for a given resource-key, sorted by resourceVersion.
-- | Uses PouchDB allDocs range query with startkey/endkey to efficiently
-- | find all documents matching the resourceKey prefix.
-- | Also populates the in-memory delta cache with the returned records.
getDeltasForResource :: String -> MonadPerspectives (Array DeltaStoreRecord)
getDeltasForResource resourceKey = do
  dbName <- deltaStoreDatabaseName
  -- Convert to URL-safe key for document ID range query.
  let sk = safeKey resourceKey
  let startkey = sk <> "_v"
  let endkey = sk <> "_v\xFFFF"
  result <- documentsInRange dbName startkey endkey
  let records = Arr.catMaybes $ map decodeDoc result.rows
  -- Populate the cache with all returned records.
  populateDeltaCache records
  pure records
  where
  decodeDoc :: { id :: String, value :: { rev :: String }, doc :: Maybe Foreign } -> Maybe DeltaStoreRecord
  decodeDoc { doc: Just foreignDoc } = case runExcept $ read' foreignDoc of
    Right (rec :: DeltaStoreRecord) -> Just rec
    Left _ -> Nothing
  decodeDoc _ = Nothing

-- | Retrieve all deltas for a given resource-key, filtered by deltaType.
-- | Useful for finding e.g. only property deltas or only binding deltas.
getDeltasForResourceByDeltaType :: String -> String -> MonadPerspectives (Array DeltaStoreRecord)
getDeltasForResourceByDeltaType resourceKey deltaType = do
  allDeltas <- getDeltasForResource resourceKey
  pure $ Arr.filter (\(DeltaStoreRecord r) -> r.deltaType == deltaType) allDeltas

-- | Retrieve all deltas for a role instance AND all its sub-resources
-- | (property and binding resource keys of the form roleInstance#subKey).
-- | Uses a range query on the delta-store document IDs, then filters to exclude
-- | false positives where another role instance shares the same ID prefix.
-- | Also populates the in-memory delta cache with the returned records.
getDeltasForRoleInstance :: String -> MonadPerspectives (Array DeltaStoreRecord)
getDeltasForRoleInstance roleInstanceId = do
  dbName <- deltaStoreDatabaseName
  -- Convert to URL-safe key for document ID range query.
  let sk = safeKey roleInstanceId
  -- Document IDs follow: <safeKey>_v<version>_<author>
  -- Role instance deltas:       safeKey_v..._...
  -- Sub-resource deltas: safeKey#..._v..._...
  -- Both start with safeKey, so a range query covers all of them.
  let startkey = sk
  -- Use the highest unicode character as end sentinel to include all possible suffixes.
  let endkey = sk <> "\xFFFF"
  result <- documentsInRange dbName startkey endkey
  let allDeltas = Arr.catMaybes $ map decodeDoc result.rows
  -- Keep only records where resourceKey is the role itself or one of its sub-resources.
  let filtered = Arr.filter (isRoleOrSubResource sk) allDeltas
  -- Populate cache.
  populateDeltaCache filtered
  pure filtered
  where
  decodeDoc :: { id :: String, value :: { rev :: String }, doc :: Maybe Foreign } -> Maybe DeltaStoreRecord
  decodeDoc { doc: Just foreignDoc } = case runExcept $ read' foreignDoc of
    Right (rec :: DeltaStoreRecord) -> Just rec
    Left _ -> Nothing
  decodeDoc _ = Nothing

  isRoleOrSubResource :: String -> DeltaStoreRecord -> Boolean
  isRoleOrSubResource safeRid (DeltaStoreRecord { resourceKey }) =
    let
      safeRk = safeKey resourceKey
      ridLen = Str.length safeRid
    in
      safeRk == safeRid
        ||
          ( Str.length safeRk > ridLen + 1
              && Str.take (ridLen + 1) safeRk == safeRid <> "#"
          )

-- | Update the `applied` flag of an existing delta-store record.
-- | No-op if the record does not exist. Also updates the in-memory cache.
updateDeltaApplied :: String -> Boolean -> MonadPerspectives Unit
updateDeltaApplied docId newApplied = do
  dbName <- deltaStoreDatabaseName
  (existing :: Maybe DeltaStoreRecord) <- tryGetDocument_ dbName docId
  case existing of
    Nothing -> pure unit
    Just (DeltaStoreRecord r) -> do
      let updated = DeltaStoreRecord (r { applied = newApplied })
      void $ addDocument_ dbName updated docId
      cacheInsertDelta docId updated

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
storeDeltaFromSignedDelta :: SignedDelta -> MonadPerspectives Unit
storeDeltaFromSignedDelta signedDelta@(SignedDelta { author, encryptedDelta }) =
  case extractDeltaInfo encryptedDelta of
    Nothing -> pure unit -- Cannot extract ordering info, skip storage
    Just { resourceKey, resourceVersion, deltaType } -> do
      let docId = deltaStoreDocId resourceKey resourceVersion author
      storeDelta $ DeltaStoreRecord
        { _id: docId
        , _rev: Nothing
        , resourceKey: safeKey resourceKey
        , resourceVersion
        , author
        , signedDelta
        , deltaType
        , applied: true -- Locally-created deltas are applied immediately
        }
