-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Test.PropertyHistory where

import Prelude

import Control.Monad.AvarMonadAsk as AA
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, head, length, range)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Effect.Exception (error)
import Foreign.Object as Object
import Perspectives.Sync.VersionedDelta (currentUniverseEpoch)
import Perspectives.Authenticate (signDelta)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##=))
import Perspectives.Data.EncodableMap as EM
import Perspectives.Deltas (addStoredDeltas, savePeerTransactionInOutgoingPost)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getProperty)
import Perspectives.ModelDependencies as DEP
import Perspectives.Persistence.DeltaStore (deltaStoreDocId, getDelta, getDeltasForResource, getDeltasForRoleInstance, safeKey)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.CausalStore (CausalRoleDoc(..), getCausalRole)
import Perspectives.Persistence.PropertyHistoryStore (storeIncomingDeltaWithHistory, storeLocalDeltaWithHistory)
import Perspectives.Persistence.PendingTransactionStore (getPendingTransactionsForResource, pendingTransactionDocId)
import Perspectives.Persistence.ResourceVersionStore (getResourceVersion)
import Perspectives.PerspectivesState (getPerspectivesUser)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Class.PersistentType (cacheInDomeinFile, getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..))
import Perspectives.Representation.TypeIdentifiers (RoleType(..))
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.Sync.HandleTransaction (executeTransaction, restorePropertyHistories)
import Perspectives.Sync.Causal (covers, currentCausalEpoch)
import Perspectives.Sync.PropertyHistory (checkpoint, isCheckpoint, propertyHistory, readPropertyChange)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (Transaction(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), currentTransactionForPeer)
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)
import Simple.JSON (writeJSON)
import Test.BindingReplacement as Binding
import Test.PeerDelivery as Delivery
import Test.VerificationBundles (withSigningFixture)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Checkpointed property history" do
  test "retains one baseline and ten versions, invalidating resource, role and delta caches" $
    Binding.withFixture Binding.localPeer [] [] do
      before <- lift $ getDeltasForResource propertyKey
      void $ lift $ getDeltasForRoleInstance (unwrap Binding.visibleId)
      for_ (range 2 42) \version ->
        lift $ storeLocalDeltaWithHistory $ change version "SetProperty" [ Value $ show version ]
      after <- lift $ getDeltasForResource propertyKey
      roleHistory <- lift $ getDeltasForRoleInstance (unwrap Binding.visibleId)
      let baselines = filter (\(DeltaStoreRecord r) -> isCheckpoint (unwrap r.signedDelta).encryptedDelta) after
      liftAff do
        equal 11 (length after)
        equal 1 (length baselines)
        equal 11 $ length $ filter (\(DeltaStoreRecord r) -> safeKey r.resourceKey == safeKey propertyKey) roleHistory
        case propertyHistory after of
          Left message -> assert message false
          Right state -> equal [ Value "42" ] state.values
      for_ before \(DeltaStoreRecord r) -> do
        stale <- lift $ getDelta r._id
        liftAff $ case stale of
          Nothing -> pure unit
          Just _ -> assert "Pruned records must not survive in the delta cache" false

  test "fullhistory retains all local and received records" $
    Binding.withFixture Binding.localPeer [] [] do
      EnumeratedProperty metadata <- lift $ getEnumeratedProperty Binding.visibleProperty
      lift $ cacheInDomeinFile Binding.visibleProperty $ EnumeratedProperty metadata { fullhistory = Just true }
      for_ (range 2 24) \version ->
        lift $ storeLocalDeltaWithHistory $ change version "SetProperty" [ Value $ show version ]
      incoming <- recordFor $ change 25 "AddProperty" [ Value "received" ]
      lift $ storeIncomingDeltaWithHistory incoming
      history <- lift $ getDeltasForResource propertyKey
      liftAff do
        equal 25 (length history)
        assert "An opted-in property must not be compacted"
          $ not
          $ anyCheckpoint history

  test "a checkpoint covers compacted causal history but not later retained mutations" $
    Binding.withFixture Binding.localPeer [] [] do
      for_ (range 2 35) \version ->
        lift $ storeLocalDeltaWithHistory $ change version "SetProperty" [ Value $ show version ]
      history <- lift $ getDeltasForResource propertyKey
      CausalRoleDoc state <- lift $ getCausalRole (unwrap Binding.visibleId)
      let actor = writeJSON [ "local", "history-fixture" ]
      liftAff do
        assert "The live role clock must include the latest write" $ covers state.clock { actor, counter: 35 }
        case head $ filter (\(DeltaStoreRecord r) -> isCheckpoint (unwrap r.signedDelta).encryptedDelta) history of
          Nothing -> assert "Expected a compacted causal checkpoint" false
          Just (DeltaStoreRecord r) -> case checkpoint (unwrap r.signedDelta).encryptedDelta of
            Just marker -> case marker.causalContext of
              Just summary -> do
                assert "Compacted writes must remain causally covered" $ covers summary { actor, counter: r.resourceVersion }
                assert "A checkpoint must not claim to cover its later tail" $ not $ covers summary { actor, counter: 35 }
              Nothing -> assert "Causal checkpoints need a causal summary" false
            Nothing -> assert "Invalid checkpoint" false

  test "relative additions and removals survive repeated compaction and property reconstruction" $
    Binding.withFixture Binding.localPeer [] [] do
      for_ (range 2 35) \version ->
        lift $ storeLocalDeltaWithHistory $ change version
          (if version == 35 then "RemoveProperty" else "AddProperty")
          [ Value $ if version == 35 then "visible-value" else show version ]
      history <- lift $ getDeltasForResource propertyKey
      PerspectRol role <- lift $ getPerspectRol Binding.visibleId
      lift $ void $ cacheEntity Binding.visibleId $ PerspectRol role { properties = Object.empty }
      restorePropertyHistories history
      actual <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
      liftAff do
        equal 11 (length history)
        equal (Value <<< show <$> range 2 34) actual

  test "late packets cannot repopulate pruned history" $
    Binding.withFixture Binding.localPeer [] [] do
      for_ (range 2 30) \version ->
        lift $ storeLocalDeltaWithHistory $ change version "SetProperty" [ Value $ show version ]
      delayed <- recordFor $ change 2 "SetProperty" [ Value "delayed" ]
      for_ (range 1 4) \_ -> lift $ storeIncomingDeltaWithHistory delayed
      history <- lift $ getDeltasForResource propertyKey
      liftAff $ equal 11 (length history)

  test "a normal update sends only its new delta after advancing the checkpoint" $
    Binding.withFixture Binding.oldRemotePeer [] [] do
      for_ (range 2 30) \version ->
        lift $ storeLocalDeltaWithHistory $ change version "SetProperty" [ Value $ show version ]
      previous <- lift $ getDeltasForResource propertyKey
      encrypted <- Delivery.packet
      let destination = unschemePerspectivesUser $ PerspectivesUser "remote-old"
      lift $ savePeerTransactionInOutgoingPost destination "history-initialised" encrypted $
        map (\(DeltaStoreRecord r) -> r.signedDelta) previous
      lift $ storeLocalDeltaWithHistory $ change 31 "SetProperty" [ Value "31" ]
      retained <- lift $ getDeltasForResource propertyKey
      addStoredDeltas [ Binding.userId ] retained
      Transaction transaction <- AA.get
      liftAff $ equal 1 (length transaction.deltas)

  test "retains the latest file-publication operation as operational evidence" $
    Binding.withFixture Binding.localPeer [] [] do
      lift $ storeLocalDeltaWithHistory $ change 2 "UploadFile" [ Value "file-metadata" ]
      for_ (range 3 35) \version ->
        lift $ storeLocalDeltaWithHistory $ change version "SetProperty" [ Value $ show version ]
      history <- lift $ getDeltasForResource propertyKey
      liftAff do
        equal 12 (length history)
        equal 1 $ length $ filter (\(DeltaStoreRecord r) -> r.deltaType == "UploadFile") history

  test "a newly visible peer bootstraps from an authenticated read-only discloser's baseline and tail" do
    captured <- liftEffect $ Ref.new Nothing
    withSigningFixture true \key -> do
      author <- lift getPerspectivesUser
      for_ (range 2 30) \version -> do
        signed <- signedChange version
        lift $ storeLocalDeltaWithHistory signed
      records <- lift $ getDeltasForResource propertyKey
      -- Peer deltas must be signed; sign the gap-causing delta with the sender's key.
      let
        unsigned = Binding.signed
          { resourceKey: "other-visible#" <> unwrap Binding.visibleProperty
          , resourceVersion: 5
          , deltaType: "SetProperty"
          , deltaFamily: "RolePropertyDelta"
          , deltaFormatVersion: 5
          , universeEpoch: currentUniverseEpoch
          , subjectKind: "ENR"
          , id: RoleInstance "other-visible"
          , roleType: Binding.visibleType
          , property: Binding.visibleProperty
          , values: [ Value "not-yet-applicable" ]
          , subject: unwrap Binding.otherUserType
          , causal:
              { epoch: currentCausalEpoch
              , dot: { actor: writeJSON [ "local", "gap-test-installation" ], counter: 1 }
              , context: ([] :: Array { actor :: String, counter :: Int })
              }
          }
      SignedDelta signedMissing <- signDelta (unwrap unsigned).encryptedDelta
      let missing = SignedDelta signedMissing { author = (unwrap unsigned).author }
      liftEffect $ Ref.write (Just { key, author, records, missing }) captured
    saved <- liftEffect $ Ref.read captured
    case saved of
      Nothing -> assert "Sender history was not captured" false
      Just snapshot -> withSigningFixture true \_ -> do
        grantOriginalWriter
        installAuthor snapshot.author snapshot.key
        installAuthor (PerspectivesUser "local") snapshot.key
        Transaction tr <- AA.get
        let
          keys = EM.fromFoldable
            [ Tuple snapshot.author { key: snapshot.key, transportKey: "unused", deltas: [] }
            , Tuple (PerspectivesUser "local") { key: snapshot.key, transportKey: "unused", deltas: [] }
            ]
          incoming = currentTransactionForPeer
            { author: snapshot.author
            , perspectivesSystem: Binding.contextId
            , timeStamp: tr.timeStamp
            , publicKeys: keys
            , deltas: map (\(DeltaStoreRecord r) -> r.signedDelta) snapshot.records
            }
          blocked = TransactionForPeer (unwrap incoming) { deltas = (unwrap incoming).deltas <> [ snapshot.missing ] }
        executeTransaction blocked
        blockedVersion <- lift $ getResourceVersion propertyKey
        pending <- lift $ pendingTransactionDocId blocked >>= getPendingTransactionsForResource
        liftAff do
          equal 0 blockedVersion
          case pending of
            Nothing -> assert "An unrelated gap must block the whole checkpoint transaction" false
            Just _ -> pure unit
        executeTransaction incoming
        actual <- lift (Binding.visibleId ##= getProperty Binding.visibleProperty)
        version <- lift $ getResourceVersion propertyKey
        history <- lift $ getDeltasForResource propertyKey
        liftAff do
          equal [ Value "30" ] actual
          equal 30 version
          equal 11 (length history)
        -- Replaying the same packet does not replay state transitions or grow history.
        executeTransaction incoming
        replayed <- lift $ getDeltasForResource propertyKey
        liftAff $ equal 11 (length replayed)

propertyKey :: String
propertyKey = unwrap Binding.visibleId <> "#" <> unwrap Binding.visibleProperty

change :: Int -> String -> Array Value -> SignedDelta
change version operation values = Binding.signed
  { resourceKey: propertyKey
  , deltaFormatVersion: 5
  , universeEpoch: currentUniverseEpoch
  , resourceVersion: version
  , deltaType: operation
  , deltaFamily: "RolePropertyDelta"
  , id: RoleInstance "visible"
  , roleType: Binding.visibleType
  , property: Binding.visibleProperty
  , values
  , subject: unwrap Binding.otherUserType
  , subjectKind: "ENR"
  , causal:
      { epoch: currentCausalEpoch
      , dot: { actor: writeJSON [ "local", "history-fixture" ], counter: version }
      , context: if version > 1 then [ { actor: writeJSON [ "local", "history-fixture" ], counter: version - 1 } ] else []
      }
  }

signedChange :: Int -> MonadPerspectivesTransaction SignedDelta
signedChange version = do
  SignedDelta original <- signDelta (unwrap $ change version "SetProperty" [ Value $ show version ]).encryptedDelta
  pure $ SignedDelta original { author = PerspectivesUser "local" }

recordFor :: SignedDelta -> MonadPerspectivesTransaction DeltaStoreRecord
recordFor signed = do
  case readPropertyChange signed of
    Left message -> throwError $ error message
    Right delta -> pure $ DeltaStoreRecord
      { _id: deltaStoreDocId delta.resourceKey delta.resourceVersion (unwrap signed).author
      , _rev: Nothing
      , resourceKey: safeKey delta.resourceKey
      , resourceVersion: delta.resourceVersion
      , author: (unwrap signed).author
      , signedDelta: signed
      , deltaId: Just $ "received-" <> show delta.resourceVersion
      , deltaType: delta.deltaType
      , applied: true
      , disposition: Just "Applied"
      , contextKey: Nothing
      }

anyCheckpoint :: Array DeltaStoreRecord -> Boolean
anyCheckpoint records = length (filter (\(DeltaStoreRecord r) -> isCheckpoint (unwrap r.signedDelta).encryptedDelta) records) > 0

grantOriginalWriter :: MonadPerspectivesTransaction Unit
grantOriginalWriter = do
  EnumeratedRole reader <- lift $ getEnumeratedRole Binding.userType
  EnumeratedRole writer <- lift $ getEnumeratedRole Binding.otherUserType
  let
    grant (Perspective p) = Perspective p
      { propertyVerbs = map
          ( map
              ( \(PropertyVerbs properties _) ->
                  PropertyVerbs properties (PSet [ Consult, SetPropertyValue ])
              )
          )
          p.propertyVerbs
      }
  lift $ cacheInDomeinFile Binding.otherUserType $ EnumeratedRole writer { perspectives = grant <$> reader.perspectives }

installAuthor :: PerspectivesUser -> String -> MonadPerspectivesTransaction Unit
installAuthor author key = lift do
  PerspectRol local <- getPerspectRol Binding.localPeer
  let id = RoleInstance $ "def:#" <> takeGuid (unwrap author)
  void $ cacheEntity id $ PerspectRol local
    { id = id
    , properties = Object.insert DEP.perspectivesUsersPublicKey [ Value key ] local.properties
    }
