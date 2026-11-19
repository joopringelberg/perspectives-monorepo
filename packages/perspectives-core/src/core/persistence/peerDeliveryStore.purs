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
  ) where

import Prelude

import Data.Array (filter, nub)
import Data.Foldable (foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Persistence.API (addDocument_, tryGetDocument_)
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

peerDeliveryKnowledge :: UnschemedResourceIdentifier -> String -> MonadPerspectives { knownIds :: Set.Set String, maxVersion :: Maybe Int }
peerDeliveryKnowledge destination resourceKey = do
  db <- peerDeliveryDatabaseName
  doc <- tryGetDocument_ db (peerDeliveryDocId destination resourceKey)
  pure case doc of
    Nothing -> { knownIds: Set.empty, maxVersion: Nothing }
    Just (PeerDeliveryDoc { knownIds, maxVersion }) -> { knownIds: Set.fromFoldable knownIds, maxVersion }

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

-- | Call ONLY after the original encrypted packet has been persisted in the
-- | outbox. Packet retries must not regenerate or modify either payload.
recordQueuedPeerDeltas :: UnschemedResourceIdentifier -> Array SignedDelta -> MonadPerspectives Unit
recordQueuedPeerDeltas destination deltas = do
  identified <- for deltas \delta@(SignedDelta { encryptedDelta }) -> case extractDeltaInfo encryptedDelta of
    Nothing -> pure Nothing
    Just { resourceKey, resourceVersion } -> do
      deltaId <- signedDeltaDeliveryId delta
      pure $ Just $ Tuple resourceKey { deltaId, resourceVersion }
  let
    byResource = foldl
      ( \acc -> case _ of
          Nothing -> acc
          Just (Tuple key entry) -> Map.insertWith (<>) key [ entry ] acc
      )
      Map.empty
      identified
  db <- peerDeliveryDatabaseName
  for_ (Map.toUnfoldable byResource :: Array (Tuple String (Array { deltaId :: String, resourceVersion :: Int }))) \(Tuple resourceKey delivered) -> do
    retained <- getDeltasForResource resourceKey
    retainedIds <- for retained storedDeltaDeliveryId
    let docId = peerDeliveryDocId destination resourceKey
    existing <- tryGetDocument_ db docId
    let
      old = case existing of
        Nothing -> { _rev: Nothing, knownIds: [], maxVersion: Nothing }
        Just (PeerDeliveryDoc previous) -> { _rev: previous._rev, knownIds: previous.knownIds, maxVersion: previous.maxVersion }
      knownIds = retainedKnowledge retainedIds old.knownIds (map _.deltaId delivered)
      maxVersion = foldl
        ( \version entry -> Just $ max entry.resourceVersion $ case version of
            Nothing -> entry.resourceVersion
            Just previous -> previous
        )
        old.maxVersion
        delivered
    when (knownIds /= old.knownIds || maxVersion /= old.maxVersion)
      $ void
      $ addDocument_ db (PeerDeliveryDoc { _id: docId, _rev: old._rev, knownIds, maxVersion }) docId
