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
--
-- END LICENSE

-- | Knowledge means durably queued, not merely assembled or sent. Losing a
-- | knowledge write can cause a duplicate, but must never omit an unqueued delta.
module Perspectives.Persistence.PeerDeliveryStore
  ( PeerDeliveryDoc(..)
  , peerDeliveryDatabaseName
  , peerDeliveryDocId
  , signedDeltaDeliveryId
  , storedDeltaDeliveryId
  , knownPeerDeltaIds
  , peerDeliveryKnowledge
  , unknownToPeer
  , recordQueuedPeerDeltas
  , retainedKnowledge
  , extendContiguousVersion
  , forgetPeerDeliveryKnowledge
  , lowerPeerDeliveryKnowledge
  , VersionCoverage
  ) where

import Prelude

import Data.Array (filter, nub, sort, zipWith)
import Data.Foldable (any, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Persistence.API (addDocument_, deleteDocument, documentsInRange, tryGetDocument_)
import Perspectives.Persistence.DeltaStore (extractDeltaInfo, getDeltasForResource, safeKey)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistence.Types (MonadPouchdb)
import Perspectives.Sync.CanonicalJson (computeDeltaId)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.UnschemedIdentifiers (UnschemedResourceIdentifier)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)

newtype PeerDeliveryDoc = PeerDeliveryDoc
  { _id :: String
  , _rev :: Maybe String
  , knownIds :: Array String
  , maxVersion :: Maybe Int
  -- | The highest version up to which the peer has been queued every retained
  -- | version (or a checkpoint covering it). Unlike maxVersion, this survives
  -- | pruning and therefore tells whether a checkpoint is redundant (I-9).
  , contiguousVersion :: Maybe Int
  }

derive newtype instance ReadForeign PeerDeliveryDoc
derive newtype instance WriteForeign PeerDeliveryDoc

peerDeliveryDatabaseName :: forall f. MonadPouchdb f String
peerDeliveryDatabaseName = (_ <> "_peerdelivery") <$> getSystemIdentifier

-- | JSON's tuple boundaries avoid collisions between destination/resource pairs.
peerDeliveryDocId :: UnschemedResourceIdentifier -> String -> String
peerDeliveryDocId destination resourceKey = writeJSON [ safeKey (unwrap destination), safeKey resourceKey ]

-- | Content identity, rather than a version watermark, preserves conflicts and
-- | historical deltas learned after a newer version has already been queued.
signedDeltaDeliveryId :: SignedDelta -> MonadPerspectives String
signedDeltaDeliveryId (SignedDelta { author, encryptedDelta }) =
  liftAff $ computeDeltaId (unwrap author) encryptedDelta

storedDeltaDeliveryId :: DeltaStoreRecord -> MonadPerspectives String
storedDeltaDeliveryId (DeltaStoreRecord { deltaId, signedDelta }) = case deltaId of
  Just identity -> pure identity
  Nothing -> signedDeltaDeliveryId signedDelta

knownPeerDeltaIds :: UnschemedResourceIdentifier -> String -> MonadPerspectives (Set.Set String)
knownPeerDeltaIds destination resourceKey = _.knownIds <$> peerDeliveryKnowledge destination resourceKey

peerDeliveryKnowledge :: UnschemedResourceIdentifier -> String -> MonadPerspectives { knownIds :: Set.Set String, maxVersion :: Maybe Int, contiguousVersion :: Maybe Int }
peerDeliveryKnowledge destination resourceKey = do
  db <- peerDeliveryDatabaseName
  doc <- tryGetDocument_ db (peerDeliveryDocId destination resourceKey)
  pure case doc of
    Nothing -> { knownIds: Set.empty, maxVersion: Nothing, contiguousVersion: Nothing }
    Just (PeerDeliveryDoc { knownIds, maxVersion, contiguousVersion }) -> { knownIds: Set.fromFoldable knownIds, maxVersion, contiguousVersion }

unknownToPeer :: UnschemedResourceIdentifier -> String -> String -> MonadPerspectives Boolean
unknownToPeer destination resourceKey deltaId =
  not <<< Set.member deltaId <$> knownPeerDeltaIds destination resourceKey

-- | Intersect on every successful delivery so bounded history has bounded
-- | delivery metadata too. Full-history properties naturally keep their IDs.
retainedKnowledge :: Array String -> Array String -> Array String -> Array String
retainedKnowledge retained previous delivered =
  let
    retainedIds = Set.fromFoldable retained
  in
    filter (flip Set.member retainedIds) $ nub $ previous <> delivered

-- | A version of a resource key and whether the peer has been queued a record at it.
type VersionCoverage = { version :: Int, checkpoint :: Boolean, known :: Boolean }

-- | Extend a contiguous watermark. A known checkpoint carries the complete state at
-- | its version, so it lifts the watermark to that version. Above it, the watermark
-- | advances through consecutive retained versions as long as the peer knows a record
-- | at each. Versions that were never retained locally do not exist and are skipped;
-- | version 0 (constructors) never counts. An unknown checkpoint stands for pruned
-- | versions, so it bars the watermark (unless the peer knows a record at its version).
-- | The watermark never decreases.
extendContiguousVersion :: Int -> Array VersionCoverage -> Int
extendContiguousVersion start coverage =
  let
    lifted = foldl (\k c -> if c.checkpoint && c.known then max k c.version else k) start coverage
    versions = sort $ nub $ map _.version $ filter (\c -> c.version > lifted) coverage
    knownAt v = any (\c -> c.known && c.version == v) coverage
  in
    foldl (\{ k, open } v -> if open && knownAt v then { k: v, open } else { k, open: false }) { k: lifted, open: true } versions # _.k

-- | Call ONLY after the original encrypted packet has been persisted in the
-- | outbox. Packet retries must not regenerate or modify either payload.
recordQueuedPeerDeltas :: UnschemedResourceIdentifier -> Array SignedDelta -> MonadPerspectives Unit
recordQueuedPeerDeltas destination deltas = do
  identified <- for deltas \delta@(SignedDelta { encryptedDelta }) -> case extractDeltaInfo encryptedDelta of
    Nothing -> pure Nothing
    Just { resourceKey, resourceVersion, deltaType } -> do
      deltaId <- signedDeltaDeliveryId delta
      pure $ Just $ Tuple resourceKey { deltaId, resourceVersion, checkpoint: deltaType == "PropertyCheckpoint" }
  let
    byResource = foldl
      ( \acc -> case _ of
          Nothing -> acc
          Just (Tuple key entry) -> Map.insertWith (<>) key [ entry ] acc
      )
      Map.empty
      identified
  db <- peerDeliveryDatabaseName
  for_ (Map.toUnfoldable byResource :: Array (Tuple String (Array { deltaId :: String, resourceVersion :: Int, checkpoint :: Boolean }))) \(Tuple resourceKey delivered) -> do
    retained <- getDeltasForResource resourceKey
    retainedIds <- for retained storedDeltaDeliveryId
    let docId = peerDeliveryDocId destination resourceKey
    existing <- tryGetDocument_ db docId
    let
      old = case existing of
        Nothing -> { _rev: Nothing, knownIds: [], maxVersion: Nothing, contiguousVersion: Nothing }
        Just (PeerDeliveryDoc previous) -> { _rev: previous._rev, knownIds: previous.knownIds, maxVersion: previous.maxVersion, contiguousVersion: previous.contiguousVersion }
      knownIds = retainedKnowledge retainedIds old.knownIds (map _.deltaId delivered)
      knownSet = Set.fromFoldable knownIds
      coverage =
        map (\{ resourceVersion, checkpoint } -> { version: resourceVersion, checkpoint, known: true }) delivered
          <> zipRetained retained retainedIds knownSet
      contiguousVersion = Just $ extendContiguousVersion (fromMaybe 0 old.contiguousVersion) coverage
      maxVersion = foldl
        ( \version entry -> Just $ max entry.resourceVersion $ case version of
            Nothing -> entry.resourceVersion
            Just previous -> previous
        )
        old.maxVersion
        delivered
    when (knownIds /= old.knownIds || maxVersion /= old.maxVersion || contiguousVersion /= old.contiguousVersion)
      $ void
      $ addDocument_ db (PeerDeliveryDoc { _id: docId, _rev: old._rev, knownIds, maxVersion, contiguousVersion }) docId

zipRetained :: Array DeltaStoreRecord -> Array String -> Set.Set String -> Array VersionCoverage
zipRetained retained ids known = zipWith
  ( \(DeltaStoreRecord { resourceVersion, deltaType }) deltaId ->
      { version: resourceVersion, checkpoint: deltaType == "PropertyCheckpoint", known: Set.member deltaId known }
  )
  retained
  ids

-- | Forget everything this installation believes it has queued for a peer (I-7).
-- | Used when the peer's signing or transport key changes: whatever it had may be
-- | lost (reinstallation, restore), so later knowledge-based forwarding must start afresh.
forgetPeerDeliveryKnowledge :: UnschemedResourceIdentifier -> MonadPerspectives Unit
forgetPeerDeliveryKnowledge destination = do
  db <- peerDeliveryDatabaseName
  -- Document ids are JSON arrays whose first element is the destination.
  let prefix = "[" <> writeJSON (safeKey (unwrap destination)) <> ","
  { rows } <- documentsInRange db prefix (prefix <> "\xFFFF")
  for_ rows \{ id, value } -> void $ deleteDocument db id (Just value.rev)

-- | The peer reports that it lacks `fromVersion` of this resource key (a history request):
-- | knowledge at or above it is not to be trusted. The watermark drops below it and the
-- | ids of retained records from that version on are forgotten. maxVersion is kept: it
-- | records what this peer has been entitled to see, which bounds later history requests.
lowerPeerDeliveryKnowledge :: UnschemedResourceIdentifier -> String -> Int -> MonadPerspectives Unit
lowerPeerDeliveryKnowledge destination resourceKey fromVersion = do
  db <- peerDeliveryDatabaseName
  let docId = peerDeliveryDocId destination resourceKey
  existing <- tryGetDocument_ db docId
  case existing of
    Nothing -> pure unit
    Just (PeerDeliveryDoc doc) -> do
      retained <- getDeltasForResource resourceKey
      untrusted <- Set.fromFoldable <$> for (filter (\(DeltaStoreRecord r) -> r.resourceVersion >= fromVersion) retained) storedDeltaDeliveryId
      let
        knownIds = filter (not <<< flip Set.member untrusted) doc.knownIds
        contiguousVersion = map (min (fromVersion - 1)) doc.contiguousVersion
      when (knownIds /= doc.knownIds || contiguousVersion /= doc.contiguousVersion)
        $ void
        $ addDocument_ db (PeerDeliveryDoc doc { knownIds = knownIds, contiguousVersion = contiguousVersion }) docId
