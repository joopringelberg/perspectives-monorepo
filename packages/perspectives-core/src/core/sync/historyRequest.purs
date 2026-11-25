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

-- | Receiver-driven repair (docsources/delta-ordering.md §10.6).
-- | A peer whose transaction stays blocked on a version gap asks the transaction's
-- | author for the missing versions. The author answers with an ordinary transaction
-- | built from its DeltaStore, bounded by what it has shipped to the requester before.
-- |
-- | A history request is not a delta: it is signed like one, but travels in the
-- | encrypted payload of an EncryptedTransactionForPeer instead of a TransactionForPeer,
-- | and never enters the DeltaStore.
-- |
-- | All effectful functions run *outside* any transaction (see Perspectives.Sync.PendingTransactions).
module Perspectives.Sync.HistoryRequest
  ( HistoryRange
  , HistoryRequest
  , HistoryRequestMessage(..)
  , historyRanges
  , historyRequestDue
  , historyRequestGrace
  , historyRequestBackoff
  , selectHistory
  , requestMissingHistory
  , sendHistoryRequest
  , answerHistoryRequest
  , maxRangesPerRequest
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Array (catMaybes, concat, filter, foldl, length, nub, nubByEq, null, sortBy, take)
import Data.DateTime.Instant (toDateTime, unInstant)
import Data.Either (Either(..), hush)
import Data.Foldable (any, maximumBy, minimum)
import Data.Int (pow, toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Perspectives.Authenticate (encryptForRecipients, signDeltaMP, verifyDelta)
import Perspectives.CoreTypes (MonadPerspectives, (##>))
import Perspectives.Data.EncodableMap as ENCMAP
import Perspectives.Deltas (addStoredDeltaForInitialVisibility, sendTransactieToUserUsingAMQP)
import Perspectives.Instances.ObjectGetters (deltaAuthor2ResourceIdentifier, getProperty)
import Perspectives.Logging (traceSync, warnSync)
import Perspectives.ModelDependencies (perspectivesUsersTransportPublicKey, sysUser)
import Perspectives.Names (getMySystem)
import Perspectives.Persistence.DeltaStore (extractDeltaInfo, getDeltasForResource, safeKey)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.PendingTransactionStore (PendingTransactionRecord(..), blockedStatus, getAllPendingTransactions, recordHistoryRequest)
import Perspectives.Persistence.PeerDeliveryStore (lowerPeerDeliveryKnowledge, peerDeliveryKnowledge)
import Perspectives.Persistent (entityExists)
import Perspectives.PerspectivesState (getPerspectivesUser, withTransactionPriority)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), Value(..), perspectivesUser2RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.TransactionForPeer (EncryptedTransactionForPeer(..))
import Perspectives.TransactionQueue (TransactionPriority(..))
import Perspectives.UnschemedIdentifiers (UnschemedResourceIdentifier(..), unschemePerspectivesUser)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

-----------------------------------------------------------
-- TYPES
-----------------------------------------------------------

-- | Versions fromVersion..toVersion (inclusive) of a resource key.
type HistoryRange = { resourceKey :: String, fromVersion :: Int, toVersion :: Int }

-- | The signed content of a history request. requester and addressee are unschemed
-- | PerspectivesUser identifiers. The addressee is part of the signed content, so a
-- | request cannot be redirected to another peer.
type HistoryRequest =
  { requester :: String
  , addressee :: String
  , requests :: Array HistoryRange
  , timeStamp :: Number
  }

-- | The encrypted payload of a history request; the signed delta holds a HistoryRequest.
newtype HistoryRequestMessage = HistoryRequestMessage { historyRequest :: SignedDelta }

derive newtype instance WriteForeign HistoryRequestMessage
derive newtype instance ReadForeign HistoryRequestMessage

-----------------------------------------------------------
-- TIMING
-----------------------------------------------------------

-- | A blocked transaction may merely be waiting for a delayed one: ask only after this
-- | many milliseconds.
historyRequestGrace :: Number
historyRequestGrace = 10.0 * minute

-- | The minimum time between two requests for the same blocked transaction, after the
-- | given number of earlier requests: one hour, doubling, at most a day.
historyRequestBackoff :: Int -> Number
historyRequestBackoff requests = min day (hour * toNumber (pow 2 (max 0 (requests - 1))))

minute :: Number
minute = 60000.0

hour :: Number
hour = 60.0 * minute

day :: Number
day = 24.0 * hour

-- | Whether history should now (milliseconds since the epoch) be requested for this record.
-- | Only blocked records with recorded gaps qualify; a failed execution has nothing to ask.
historyRequestDue :: Number -> PendingTransactionRecord -> Boolean
historyRequestDue time (PendingTransactionRecord r) =
  r.status == blockedStatus && not (null r.missingDeltas) && case r.historyRequestedAt of
    Nothing -> case r.blockedSince of
      Nothing -> false
      Just since -> time - since >= historyRequestGrace
    Just requested -> time - requested >= historyRequestBackoff r.historyRequests

-----------------------------------------------------------
-- RANGES AND SELECTION (pure)
-----------------------------------------------------------

-- | The versions a blocked transaction misses: from the first missing version up to
-- | just below the lowest version of that key the transaction itself carries.
historyRanges :: PendingTransactionRecord -> Array HistoryRange
historyRanges (PendingTransactionRecord { transaction, missingDeltas }) =
  nub $ map rangeFor missingDeltas
  where
  carried :: Array { resourceKey :: String, resourceVersion :: Int, deltaType :: String }
  carried = catMaybes $ map
    ( \(SignedDelta { encryptedDelta }) -> map
        (\{ resourceKey, resourceVersion, deltaType } -> { resourceKey: safeKey resourceKey, resourceVersion, deltaType })
        (extractDeltaInfo encryptedDelta)
    )
    (unwrap transaction).deltas

  rangeFor :: { resourceKey :: String, expectedVersion :: Int | _ } -> HistoryRange
  rangeFor { resourceKey, expectedVersion } =
    let
      key = safeKey resourceKey
      above = map _.resourceVersion $ filter
        (\c -> c.resourceKey == key && c.resourceVersion > expectedVersion)
        carried
    in
      { resourceKey: key
      , fromVersion: expectedVersion
      , toVersion: case minimum above of
          Just lowest -> lowest - 1
          Nothing -> expectedVersion
      }

-- | The retained records that cover versions fromVersion..cap.
-- | * If the record at fromVersion is still retained, the records from there on
-- |   (with the version-0 constructors when the range starts at the beginning).
-- | * Otherwise (it was pruned) the newest checkpoint in the range, plus the records after it.
-- | Records above cap are never selected.
selectHistory :: Int -> Int -> Array DeltaStoreRecord -> Array DeltaStoreRecord
selectHistory fromVersion cap records =
  if cap < fromVersion then []
  else if any (\r -> not (isCheckpoint r) && version r == fromVersion) records then
    filter (\r -> not (isCheckpoint r) && inRange (version r)) records
  else case maximumBy (comparing version) (filter (\r -> isCheckpoint r && version r >= fromVersion && version r <= cap) records) of
    Just c -> [ c ] <> filter (\r -> not (isCheckpoint r) && version r > version c && version r <= cap) records
    Nothing -> filter (\r -> not (isCheckpoint r) && inRange (version r)) records
  where
  inRange v = (v >= fromVersion && v <= cap) || (fromVersion <= 1 && v == 0)
  version (DeltaStoreRecord r) = r.resourceVersion
  isCheckpoint (DeltaStoreRecord r) = r.deltaType == "PropertyCheckpoint"

-- | Bounds the work a single request can cause.
maxRangesPerRequest :: Int
maxRangesPerRequest = 100

-----------------------------------------------------------
-- REQUESTER
-----------------------------------------------------------

-- | Ask the authors of long-blocked transactions for the versions they miss.
-- | Requests for all due records of one author travel in one message.
requestMissingHistory :: MonadPerspectives Unit
requestMissingHistory = do
  time <- currentMillis
  me <- unschemePerspectivesUser <$> getPerspectivesUser
  records <- getAllPendingTransactions
  let
    due = filter (historyRequestDue time) records
    byAuthor = foldl
      ( \acc record@(PendingTransactionRecord r) ->
          let
            author = unschemePerspectivesUser (unwrap r.transaction).author
          in
            -- Our own other installations do not answer: we never ship deltas to ourselves.
            if author == me then acc
            else Map.insertWith (<>) author [ { docId: r._id, ranges: historyRanges record } ] acc
      )
      Map.empty
      due
  for_ (Map.toUnfoldable byAuthor :: Array (Tuple UnschemedResourceIdentifier (Array { docId :: String, ranges :: Array HistoryRange }))) \(Tuple author entries) -> do
    let ranges = take maxRangesPerRequest $ nub $ concat $ map _.ranges entries
    result <- try $ sendHistoryRequest author ranges
    case result of
      Left e -> warnSync $ "Could not request history from " <> show author <> ": " <> show e
      Right sent -> when sent $ for_ entries \{ docId } -> recordHistoryRequest docId time

-- | Sign and send a history request to a peer. Returns false if the peer has no transport key.
sendHistoryRequest :: UnschemedResourceIdentifier -> Array HistoryRange -> MonadPerspectives Boolean
sendHistoryRequest addressee requests = do
  me <- unschemePerspectivesUser <$> getPerspectivesUser
  time <- currentMillis
  instant <- liftEffect now
  let resource = perspectivesUser2RoleInstance $ deltaAuthor2ResourceIdentifier (PerspectivesUser $ unwrap addressee)
  mtransportKey <- resource ##> getProperty (EnumeratedPropertyType perspectivesUsersTransportPublicKey)
  case mtransportKey of
    Just (Value transportKey) -> do
      signed <- signDeltaMP $ writeJSON ({ requester: unwrap me, addressee: unwrap addressee, requests, timeStamp: time } :: HistoryRequest)
      encrypted <- liftAff $ encryptForRecipients (writeJSON $ HistoryRequestMessage { historyRequest: signed }) [ { recipient: unwrap addressee, transportKey } ]
      system <- getMySystem
      traceSync $ "Requesting history from " <> show addressee <> " for " <> show (length requests) <> " range(s)"
      sendTransactieToUserUsingAMQP addressee $ EncryptedTransactionForPeer
        { author: PerspectivesUser $ unwrap me
        , perspectivesSystem: ContextInstance system
        , timeStamp: SerializableDateTime $ toDateTime instant
        , encryptedDeltas:
            { ciphertext: encrypted.ciphertext
            , iv: encrypted.iv
            , wrappedKeys: map (\{ recipient, wrappedKey } -> { recipient: UnschemedResourceIdentifier recipient, wrappedKey }) encrypted.wrappedKeys
            }
        , publicKeys: ENCMAP.empty
        }
      pure true
    _ -> do
      warnSync $ "Cannot request history from " <> show addressee <> ": no transport key"
      pure false

-----------------------------------------------------------
-- SENDER
-----------------------------------------------------------

-- | Answer a history request with an ordinary transaction.
-- | The request must be signed by the requester and addressed to us. Per resource key,
-- | only versions up to the highest version we ever queued for the requester are sent:
-- | a history request never discloses more than ordinary forwarding did. Answering
-- | first lowers our delivery knowledge for the requested range, so that later
-- | knowledge-based forwarding no longer assumes the requester has it.
answerHistoryRequest :: SignedDelta -> MonadPerspectives Unit
answerHistoryRequest signed@(SignedDelta { author, signature }) =
  if isNothing signature then warnSync "Ignoring an unsigned history request"
  else do
    mpayload <- verifyDelta signed
    case mpayload >>= (hush <<< readJSON) of
      Nothing -> warnSync $ "Ignoring a history request that cannot be verified or read, from " <> show author
      Just (request :: HistoryRequest) -> do
        me <- unschemePerspectivesUser <$> getPerspectivesUser
        let requester = UnschemedResourceIdentifier request.requester
        if requester /= unschemePerspectivesUser author || request.addressee /= unwrap me then
          warnSync $ "Ignoring a history request whose requester or addressee does not match, from " <> show author
        else do
          let requesterRole = perspectivesUser2RoleInstance $ deltaAuthor2ResourceIdentifier (PerspectivesUser request.requester)
          known <- entityExists requesterRole
          if not known then warnSync $ "Ignoring a history request from unknown peer " <> show requester
          else do
            selections <- for (take maxRangesPerRequest request.requests) \range -> do
              { maxVersion } <- peerDeliveryKnowledge requester range.resourceKey
              case maxVersion of
                Nothing -> pure []
                Just entitled -> do
                  records <- getDeltasForResource range.resourceKey
                  lowerPeerDeliveryKnowledge requester range.resourceKey range.fromVersion
                  pure $ selectHistory range.fromVersion (min range.toVersion entitled) records
            let records = nubByEq (\(DeltaStoreRecord a) (DeltaStoreRecord b) -> a._id == b._id) $ concat selections
            if null records then traceSync $ "Nothing to answer to the history request of " <> show requester
            else do
              traceSync $ "Answering the history request of " <> show requester <> " with " <> show (length records) <> " delta(s)"
              withTransactionPriority BackgroundTransaction
                $ void
                $ runMonadPerspectivesTransaction' true (ENR $ EnumeratedRoleType sysUser)
                $ for_ (sortBy (comparing \(DeltaStoreRecord r) -> r.resourceVersion) records)
                    (addStoredDeltaForInitialVisibility [ requesterRole ])

currentMillis :: MonadPerspectives Number
currentMillis = liftEffect $ unwrap <<< unInstant <$> now
