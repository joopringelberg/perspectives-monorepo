-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Test.PeerDelivery where

import Prelude

import Control.Monad.AvarMonadAsk as AA
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (elem, filter, length)
import Data.Array.NonEmpty as NA
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Set as Set
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Foreign.Object as Object
import Perspectives.Sync.VersionedDelta (currentUniverseEpoch)
import Perspectives.Assignment.SerialiseAsDeltas (serialiseRoleInstancesAndProperties, serialiseRoleInstancesAndPropertiesForInitialVisibility)
import Perspectives.CoreTypes (MonadPerspectivesTransaction)
import Perspectives.Data.EncodableMap as EM
import Perspectives.Deltas (addPublicKeysToTransaction, addStoredDeltas, savePeerTransactionInOutgoingPost, sortTransactionDeltas)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Persistence.API (tryGetDocument_)
import Perspectives.Persistence.DeltaStore (getDeltasForResource, pruneDeltasThrough, safeKey, storeDeltaFromSignedDelta)
import Perspectives.Persistence.PeerDeliveryStore (PeerDeliveryDoc(..), extendContiguousVersion, forgetPeerDeliveryKnowledge, lowerPeerDeliveryKnowledge, peerDeliveryDatabaseName, peerDeliveryDocId, peerDeliveryKnowledge, retainedKnowledge, signedDeltaDeliveryId, unknownToPeer)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.PendingTransactionStore (PendingTransactionRecord(..), blockedStatus)
import Perspectives.Sync.HistoryRequest (answerHistoryRequest, historyRanges, historyRequestBackoff, historyRequestDue, historyRequestGrace, selectHistory)
import Perspectives.Persistent (getPerspectRol, postDatabaseName)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser(..), Value(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.OutgoingTransaction (OutgoingTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Sync.TransactionForPeer (EncryptedTransactionForPeer(..), TransactionForPeer(..), currentTransactionForPeer, removeUnusedPublicKeys)
import Perspectives.UnschemedIdentifiers (UnschemedResourceIdentifier, unschemePerspectivesUser)
import Simple.JSON (writeJSON)
import Test.BindingReplacement as Binding
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Durable incremental peer delivery" do
  test "assembly does not advance knowledge and can be repeated after failure" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      records <- lift $ getDeltasForResource propertyKey
      addStoredDeltas [ Binding.userId ] records
      first <- transactionDeltas
      unknown <- lift $ signedDeltaDeliveryId Binding.visiblePropertyDelta >>= unknownToPeer destination propertyKey
      liftAff do
        equal [ Binding.visiblePropertyDelta ] first
        assert "Assembling a transaction must not claim delivery" unknown
      clearDeltas
      addStoredDeltas [ Binding.userId ] records
      second <- transactionDeltas
      liftAff $ equal first second

  test "failed outbox persistence leaves the original delta unknown" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      encrypted <- packet
      result <- lift $ try $ savePeerTransactionInOutgoingPost destination "_invalid" encrypted [ Binding.visiblePropertyDelta ]
      unknown <- lift $ signedDeltaDeliveryId Binding.visiblePropertyDelta >>= unknownToPeer destination propertyKey
      liftAff do
        assert "PouchDB must reject a reserved document ID" $ isLeft result
        assert "An unqueued delta must be retried in full" unknown

  test "successful queue persistence records knowledge and preserves the encrypted packet on retry" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      encrypted <- packet
      lift $ savePeerTransactionInOutgoingPost destination "delivery-1" encrypted [ Binding.visiblePropertyDelta ]
      firstDoc <- deliveryDoc
      lift $ savePeerTransactionInOutgoingPost destination "delivery-1" encrypted [ Binding.visiblePropertyDelta ]
      secondDoc <- deliveryDoc
      stored <- lift do
        db <- postDatabaseName
        tryGetDocument_ db "delivery-1"
      unknown <- lift $ signedDeltaDeliveryId Binding.visiblePropertyDelta >>= unknownToPeer destination propertyKey
      liftAff do
        assert "Knowledge may advance after queue persistence" $ not unknown
        equal (writeJSON firstDoc) (writeJSON secondDoc)
        case stored of
          Just (OutgoingTransaction outgoing) -> equal (writeJSON encrypted) (writeJSON outgoing.transaction)
          Nothing -> assert "The original encrypted outbox packet must exist" false
      records <- lift $ getDeltasForResource propertyKey
      addStoredDeltas [ Binding.userId ] records
      deltas <- transactionDeltas
      liftAff $ equal [] deltas

  test "a message-ID collision cannot replace a packet or claim delivery of different deltas" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      encrypted <- packet
      lift $ savePeerTransactionInOutgoingPost destination "collision" encrypted [ Binding.visiblePropertyDelta ]
      let
        original = unwrap encrypted
        changed = EncryptedTransactionForPeer original
          { encryptedDeltas = original.encryptedDeltas { ciphertext = "different-ciphertext" } }
        newDelta = propertyChange 2 "SetProperty" "not-queued" (PerspectivesUser "local")
      lift $ storeDeltaFromSignedDelta newDelta
      result <- lift $ try $ savePeerTransactionInOutgoingPost destination "collision" changed [ newDelta ]
      unknown <- lift $ signedDeltaDeliveryId newDelta >>= unknownToPeer destination propertyKey
      stored <- lift do
        db <- postDatabaseName
        tryGetDocument_ db "collision"
      liftAff do
        assert "Different packets must not share an outbox ID" $ isLeft result
        assert "A rejected replacement cannot advance delivery knowledge" unknown
        case stored of
          Just (OutgoingTransaction outgoing) -> equal (writeJSON encrypted) (writeJSON outgoing.transaction)
          Nothing -> assert "The original queued packet must remain intact" false

  test "established peers receive a new version, author conflict and late older delta, not known history" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      let
        newest = propertyChange 20 "SetProperty" "newest" (PerspectivesUser "local")
        authorConflict = propertyChange 20 "SetProperty" "conflict" (PerspectivesUser "another-author")
        sameAuthorConflict = propertyChange 20 "SetProperty" "another-payload" (PerspectivesUser "local")
        late = propertyChange 2 "AddProperty" "late" (PerspectivesUser "local")
        next = propertyChange 21 "SetProperty" "next" (PerspectivesUser "local")
      lift $ storeDeltaFromSignedDelta newest
      encrypted <- packet
      lift $ savePeerTransactionInOutgoingPost destination "known" encrypted [ Binding.visiblePropertyDelta, newest ]
      lift $ for_ [ authorConflict, sameAuthorConflict, late, next ] storeDeltaFromSignedDelta
      records <- lift $ getDeltasForResource propertyKey
      addStoredDeltas [ Binding.userId ] records
      deltas <- transactionDeltas
      liftAff do
        equal 4 $ length deltas
        for_ [ authorConflict, sameAuthorConflict, late, next ] \delta ->
          assert "Previously unknown content must not be hidden by a numeric watermark" $ elem delta deltas
        assert "Known retained history must not be retransmitted" $ not $ elem newest deltas

  test "knowledge is isolated by destination and resource" $
    Binding.withFixture Binding.newRemotePeer [ Binding.remoteAlternativeId ] [] do
      encrypted <- packet
      lift $ savePeerTransactionInOutgoingPost destination "known" encrypted [ Binding.visiblePropertyDelta ]
      records <- lift $ getDeltasForResource propertyKey
      addStoredDeltas [ Binding.userId, Binding.remoteAlternativeId ] records
      Transaction { deltas } <- AA.get
      otherResourceUnknown <- lift $ signedDeltaDeliveryId Binding.visiblePropertyDelta >>= unknownToPeer destination "another-resource"
      liftAff do
        equal [ [ Binding.remoteAlternativeId ] ] $ map (\(DeltaInTransaction d) -> d.users) deltas
        assert "Knowledge for one resource cannot suppress another resource" otherResourceUnknown

  test "new visibility receives full retained history, even after earlier visibility" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      encrypted <- packet
      lift $ savePeerTransactionInOutgoingPost destination "previous-visibility" encrypted [ Binding.visiblePropertyDelta ]
      serialiseRoleInstancesAndProperties Binding.contextId (NA.singleton Binding.userId)
        visibleObject
        [ ENP Binding.visibleProperty ]
        false
        false
      ordinary <- transactionDeltas
      liftAff $ assert "Ordinary dependency updates must remain incremental" $ not $ elem Binding.visiblePropertyDelta ordinary
      clearDeltas
      serialiseRoleInstancesAndPropertiesForInitialVisibility Binding.contextId (NA.singleton Binding.userId)
        visibleObject
        [ ENP Binding.visibleProperty ]
        false
        false
      initial <- transactionDeltas
      liftAff $ assert "Re-gaining visibility must explicitly send the retained baseline" $ elem Binding.visiblePropertyDelta initial

  test "new destinations receive every retained historical delta" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      let older = propertyChange 2 "AddProperty" "older" (PerspectivesUser "local")
      lift $ storeDeltaFromSignedDelta older
      records <- lift $ getDeltasForResource propertyKey
      addStoredDeltas [ Binding.userId ] records
      deltas <- transactionDeltas
      liftAff do
        equal 2 $ length deltas
        assert "The entire retained history is the initial baseline" $ elem older deltas && elem Binding.visiblePropertyDelta deltas

  test "new visibility includes retained history for an empty/deleted property" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      let deletion = propertyChange 2 "DeleteProperty" "deleted" (PerspectivesUser "local")
      lift do
        storeDeltaFromSignedDelta deletion
        PerspectRol role <- getPerspectRol Binding.visibleId
        void $ cacheEntity Binding.visibleId $ PerspectRol role { properties = Object.empty }
      serialiseRoleInstancesAndPropertiesForInitialVisibility Binding.contextId (NA.singleton Binding.userId)
        visibleObject
        [ ENP Binding.visibleProperty ]
        false
        false
      deltas <- transactionDeltas
      liftAff do
        assert "No current value must not erase the retained historical baseline" $ elem Binding.visiblePropertyDelta deltas
        assert "The retained deletion must accompany its history" $ elem deletion deltas

  test "a moving checkpoint is redundant for established peers, but ordinary unknown historical deltas are not" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      let
        newest = propertyChange 20 "SetProperty" "newest" (PerspectivesUser "local")
        originalWinner = propertyChange 2 "SetProperty" "boundary" (PerspectivesUser "remote-old")
        baseline = checkpointChange originalWinner
        lateConflict = propertyChange 2 "AddProperty" "late-conflict" (PerspectivesUser "local")
      lift $ storeDeltaFromSignedDelta newest
      encrypted <- packet
      lift $ savePeerTransactionInOutgoingPost destination "known-latest" encrypted [ Binding.visiblePropertyDelta, newest ]
      lift $ for_ [ baseline, lateConflict ] storeDeltaFromSignedDelta
      records <- lift $ getDeltasForResource propertyKey
      addStoredDeltas [ Binding.userId ] records
      deltas <- transactionDeltas
      liftAff do
        equal [ lateConflict ] deltas
        assert "A checkpoint older than queued state is redundant" $ not $ elem baseline deltas
      clearDeltas
      serialiseRoleInstancesAndPropertiesForInitialVisibility Binding.contextId (NA.singleton Binding.userId)
        visibleObject
        [ ENP Binding.visibleProperty ]
        false
        false
      initial <- transactionDeltas
      liftAff $ assert "Explicit initialisation overrides the checkpoint watermark" $ elem baseline initial

  test "knowledge drops pruned IDs after the next durable delivery" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      let newer = propertyChange 3 "SetProperty" "newer" (PerspectivesUser "local")
      encrypted <- packet
      lift $ savePeerTransactionInOutgoingPost destination "before-pruning" encrypted [ Binding.visiblePropertyDelta ]
      lift $ storeDeltaFromSignedDelta newer
      lift $ pruneDeltasThrough propertyKey 1 "no-checkpoint"
      lift $ savePeerTransactionInOutgoingPost destination "after-pruning" encrypted [ newer ]
      doc <- deliveryDoc
      expected <- lift $ signedDeltaDeliveryId newer
      liftAff case doc of
        Just (PeerDeliveryDoc { knownIds }) -> equal [ expected ] knownIds
        Nothing -> assert "Delivery knowledge must be persisted" false

  test "retained knowledge has no fixed cap for fullhistory and preserves late IDs" do
    let ids = show <$> (Set.toUnfoldable $ Set.fromFoldable [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ] :: Array Int)
    equal ids $ retainedKnowledge ids ids []
    equal [ "old", "new", "late" ] $ retainedKnowledge [ "old", "new", "late" ] [ "gone", "old", "new" ] [ "late", "late" ]

  test "checkpoint verification keeps both signer and embedded winner evidence" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      transaction <- AA.get
      let
        winner = propertyChange 2 "SetProperty" "winner" (PerspectivesUser "remote-old")
        baseline = checkpointChange winner
      Transaction assembled <- lift $ addPublicKeysToTransaction $
        over Transaction (\t -> t { deltas = [ DeltaInTransaction { users: [ Binding.userId ], delta: baseline } ] }) transaction
      let
        TransactionForPeer kept = removeUnusedPublicKeys $ currentTransactionForPeer
          { author: PerspectivesUser "local"
          , perspectivesSystem: Binding.contextId
          , timeStamp: assembled.timeStamp
          , deltas: [ baseline ]
          , publicKeys: assembled.publicKeys
          }
      liftAff do
        equal 2 $ Map.size $ unwrap kept.publicKeys
        assert "The winner's original verification bundle must survive filtering" $
          EM.lookup (PerspectivesUser "remote-old") kept.publicKeys /= Nothing

  test "property operations replay numerically with the checkpoint, between creation and role deletion" do
    let
      winner = propertyChange 2 "SetProperty" "baseline" (PerspectivesUser "local")
      baseline = checkpointChange winner
      change version operation = propertyChange version operation "v" (PerspectivesUser "local")
      creation = Binding.roleDelta Binding.visibleId
      deletion = change 0 "RemoveRoleInstance"
      expected =
        [ creation
        , baseline
        , change 3 "RemoveProperty"
        , change 4 "AddProperty"
        , change 5 "DeleteProperty"
        , change 10 "SetProperty"
        , deletion
        ]
      unsorted = [ deletion, change 10 "SetProperty", change 5 "DeleteProperty", change 4 "AddProperty", baseline, creation, change 3 "RemoveProperty" ]
      sorted = sortTransactionDeltas $ map (\delta -> DeltaInTransaction { users: [], delta }) unsorted
    equal expected $ map (\(DeltaInTransaction d) -> d.delta) sorted

  test "the contiguous watermark only advances through versions the peer has" do
    let
      plain version known = { version, checkpoint: false, known }
      checkpointAt version known = { version, checkpoint: true, known }
    equal 3 $ extendContiguousVersion 0 [ plain 1 true, plain 2 true, plain 3 true ]
    equal 1 $ extendContiguousVersion 0 [ plain 1 true, plain 2 false, plain 3 true ]
    equal 0 $ extendContiguousVersion 0 [ plain 1 false, plain 20 true ]
    -- Versions never retained locally do not exist and are skipped.
    equal 20 $ extendContiguousVersion 0 [ plain 1 true, plain 20 true ]
    -- Two records at one version (a conflict): one known record suffices.
    equal 2 $ extendContiguousVersion 0 [ plain 1 true, plain 2 false, plain 2 true ]
    -- A known checkpoint carries the complete state at its version.
    equal 5 $ extendContiguousVersion 0 [ checkpointAt 4 true, plain 5 true, plain 7 false ]
    -- An unknown checkpoint stands for pruned versions the peer may lack.
    equal 0 $ extendContiguousVersion 0 [ checkpointAt 4 false, plain 5 true ]
    equal 5 $ extendContiguousVersion 0 [ checkpointAt 4 false, plain 4 true, plain 5 true ]
    -- Constructors (version 0) never count, and the watermark never decreases.
    equal 6 $ extendContiguousVersion 6 [ plain 0 true, plain 1 false ]

  test "a checkpoint is shipped when the peer was only queued a newer version (I-9)" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      let
        newest = propertyChange 20 "SetProperty" "newest" (PerspectivesUser "local")
        originalWinner = propertyChange 2 "SetProperty" "boundary" (PerspectivesUser "remote-old")
        baseline = checkpointChange originalWinner
      lift $ storeDeltaFromSignedDelta newest
      encrypted <- packet
      -- Only the newest version was queued: version 1 was withheld from this peer.
      lift $ savePeerTransactionInOutgoingPost destination "only-newest" encrypted [ newest ]
      lift $ storeDeltaFromSignedDelta baseline
      knowledge <- lift $ peerDeliveryKnowledge destination propertyKey
      records <- lift $ getDeltasForResource propertyKey
      addStoredDeltas [ Binding.userId ] records
      deltas <- transactionDeltas
      liftAff do
        equal (Just 20) knowledge.maxVersion
        equal (Just 0) knowledge.contiguousVersion
        assert "A checkpoint below an unknown version is not redundant" $ elem baseline deltas
        assert "Known retained history must not be retransmitted" $ not $ elem newest deltas

  test "a key change forgets, and a history request lowers, delivery knowledge (I-7)" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      let newest = propertyChange 20 "SetProperty" "newest" (PerspectivesUser "local")
      lift $ storeDeltaFromSignedDelta newest
      encrypted <- packet
      lift $ savePeerTransactionInOutgoingPost destination "both" encrypted [ Binding.visiblePropertyDelta, newest ]
      lift $ lowerPeerDeliveryKnowledge destination propertyKey 20
      lowered <- lift $ peerDeliveryKnowledge destination propertyKey
      visibleId <- lift $ signedDeltaDeliveryId Binding.visiblePropertyDelta
      newestId <- lift $ signedDeltaDeliveryId newest
      liftAff do
        assert "Knowledge below the requested version is kept" $ Set.member visibleId lowered.knownIds
        assert "Knowledge from the requested version on is dropped" $ not $ Set.member newestId lowered.knownIds
        equal (Just 19) lowered.contiguousVersion
        equal (Just 20) lowered.maxVersion
      -- Unsigned history requests are ignored and leave knowledge intact.
      lift $ answerHistoryRequest $ SignedDelta { author: PerspectivesUser "remote-new", signature: Nothing, encryptedDelta: "{}" }
      unchanged <- lift $ peerDeliveryKnowledge destination propertyKey
      liftAff $ assert "An unsigned request changes nothing" $ Set.member visibleId unchanged.knownIds
      lift $ forgetPeerDeliveryKnowledge destination
      forgotten <- lift $ peerDeliveryKnowledge destination propertyKey
      liftAff do
        equal Nothing forgotten.maxVersion
        assert "A forgotten peer knows nothing" $ Set.isEmpty forgotten.knownIds

  test "history requests: missing ranges, back-off and selection" $
    Binding.withFixture Binding.newRemotePeer [] [] do
      Transaction { timeStamp } <- AA.get
      let
        v2 = propertyChange 2 "AddProperty" "two" (PerspectivesUser "local")
        v3 = propertyChange 3 "SetProperty" "three" (PerspectivesUser "local")
        v5 = propertyChange 5 "SetProperty" "five" (PerspectivesUser "local")
        v7 = propertyChange 7 "SetProperty" "seven" (PerspectivesUser "local")
        baseline = checkpointChange v2
        record = PendingTransactionRecord
          { _id: "pending"
          , _rev: Nothing
          , transaction: currentTransactionForPeer
              { author: PerspectivesUser "remote-new"
              , perspectivesSystem: Binding.contextId
              , timeStamp
              , deltas: [ v7, v5 ]
              , publicKeys: EM.empty
              }
          , missingDeltas: [ { author: PerspectivesUser "remote-new", resourceKey: propertyKey, expectedVersion: 2 } ]
          , status: blockedStatus
          , attempts: 1
          , lastError: Nothing
          , blockedSince: Just 0.0
          , historyRequestedAt: Nothing
          , historyRequests: 0
          }
        requested n at = modifyRecord (_ { historyRequestedAt = Just at, historyRequests = n }) record
        hour = 3600000.0
        modifyRecord f (PendingTransactionRecord r) = PendingTransactionRecord (f r)
      liftAff do
        equal [ { resourceKey: safeKey propertyKey, fromVersion: 2, toVersion: 4 } ] $ historyRanges record
        assert "Wait for the grace period" $ not $ historyRequestDue (historyRequestGrace - 1.0) record
        assert "Ask after the grace period" $ historyRequestDue historyRequestGrace record
        assert "Back off for an hour after the first request" $ not $ historyRequestDue (hour - 1.0) (requested 1 0.0)
        assert "Ask again after an hour" $ historyRequestDue hour (requested 1 0.0)
        assert "The back-off doubles" $ not $ historyRequestDue (3.0 * hour) (requested 3 0.0)
        equal (24.0 * hour) $ historyRequestBackoff 10
        assert "A failed record has nothing to ask" $ not $ historyRequestDue hour
          (modifyRecord (_ { missingDeltas = [] }) record)
      lift $ for_ [ v2, v3, v5, baseline ] storeDeltaFromSignedDelta
      records <- lift $ getDeltasForResource propertyKey
      let
        selected from cap rs = map (\(DeltaStoreRecord r) -> r.signedDelta) $ selectHistory from cap rs
        pruned = filter (\(DeltaStoreRecord r) -> r.signedDelta /= v2) records
      liftAff do
        equal 2 $ length $ selected 2 3 records
        assert "Retained versions are sent as they are" $ elem v2 (selected 2 3 records) && elem v3 (selected 2 3 records)
        assert "Nothing above the cap is sent" $ not $ elem v5 (selected 2 3 records)
        equal 3 $ length $ selected 2 5 pruned
        assert "A pruned start is covered by the newest checkpoint" $ elem baseline (selected 2 5 pruned)
        equal [] $ selected 4 3 records

destination :: UnschemedResourceIdentifier
destination = unschemePerspectivesUser $ PerspectivesUser "def:#remote-new"

propertyKey :: String
propertyKey = unwrap Binding.visibleId <> "#" <> unwrap Binding.visibleProperty

propertyChange :: Int -> String -> String -> PerspectivesUser -> SignedDelta
propertyChange resourceVersion deltaType value author = SignedDelta
  { author
  , signature: Just $ "signature-" <> value
  , encryptedDelta: writeJSON
      { resourceKey: propertyKey
      , resourceVersion
      , deltaType
      , deltaFamily: "RolePropertyDelta"
      , deltaFormatVersion: 5
      , universeEpoch: currentUniverseEpoch
      , id: Binding.visibleId
      , property: Binding.visibleProperty
      , values: [ Value value ]
      }
  }

checkpointChange :: SignedDelta -> SignedDelta
checkpointChange winner = SignedDelta
  { author: PerspectivesUser "local"
  , signature: Just "checkpoint-signature"
  , encryptedDelta: writeJSON
      { resourceKey: propertyKey
      , resourceVersion: 2
      , deltaType: "SetProperty"
      , deltaFamily: "RolePropertyDelta"
      , deltaFormatVersion: 3
      , universeEpoch: currentUniverseEpoch
      , id: Binding.visibleId
      , property: Binding.visibleProperty
      , values: [ Value "winner" ]
      , historyCheckpoint: { beforeValues: [] :: Array Value, winner }
      }
  }

visibleObject :: QueryFunctionDescription
visibleObject = SQD (CDOM $ ST Binding.contextType) (RolGetter $ ENR Binding.visibleType)
  (RDOM $ ST $ RoleInContext { context: Binding.contextType, role: Binding.visibleType })
  False
  False

transactionDeltas :: MonadPerspectivesTransaction (Array SignedDelta)
transactionDeltas = do
  Transaction { deltas } <- AA.get
  pure $ map (\(DeltaInTransaction d) -> d.delta) deltas

clearDeltas :: MonadPerspectivesTransaction Unit
clearDeltas = AA.modify $ over Transaction \t -> t { deltas = [], deltaMembership = Set.empty }

deliveryDoc :: MonadPerspectivesTransaction (Maybe PeerDeliveryDoc)
deliveryDoc = lift do
  db <- peerDeliveryDatabaseName
  tryGetDocument_ db (peerDeliveryDocId destination propertyKey)

packet :: MonadPerspectivesTransaction EncryptedTransactionForPeer
packet = do
  Transaction { timeStamp } <- AA.get
  pure $ EncryptedTransactionForPeer
    { author: PerspectivesUser "local"
    , perspectivesSystem: Binding.contextId
    , timeStamp
    , publicKeys: EM.empty
    , encryptedDeltas:
        { ciphertext: "the-original-ciphertext"
        , iv: "the-original-iv"
        , wrappedKeys: [ { recipient: destination, wrappedKey: "the-original-wrapped-key" } ]
        }
    }
