-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

module Perspectives.Deltas where

import Control.Monad.AvarMonadAsk (modify, gets) as AA
import Control.Monad.Error.Class (throwError)
import Control.Monad.State.Trans (StateT, execStateT, get, lift, modify, put)
import Data.Array (catMaybes, concat, elemIndex, filter, filterA, foldl, head, nub, null, snoc, sortBy, union)
import Data.DateTime.Instant (toDateTime)
import Data.Foldable (any)
import Data.Map (Map, empty, filter, filterKeys, insert, insertWith, lookup, mapMaybeWithKey, toUnfoldable, values) as Map
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (over, unwrap)
import Data.Ordering (Ordering)
import Data.Set as Set
import Data.Traversable (for, for_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Now (now)
import Partial.Unsafe (unsafePartial)
import Perspectives.AMQP.Stomp (sendToTopic)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.Authenticate (encryptForRecipients)
import Perspectives.ContextAndRole (rol_property)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##>))
import Perspectives.Data.EncodableMap as ENCMAP
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Me (notIsMe)
import Perspectives.Instances.ObjectGetters (deltaAuthor2ResourceIdentifier, getProperty, perspectivesUsersRole_, roleType_)
import Perspectives.Logging (debugBroker, infoDelta, traceDelta)
import Perspectives.ModelDependencies (connectedToAMQPBroker, userChannel) as DEP
import Perspectives.ModelDependencies (perspectivesUsersCancelled, perspectivesUsersPublicKey, perspectivesUsersTransportPublicKey)
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (Url, addDocument, addDocument_, tryGetDocument_)
import Perspectives.Persistence.DeltaStore (extractDeltaInfo, getDeltasForResource)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.PeerDeliveryStore (peerDeliveryKnowledge, recordQueuedPeerDeltas, storedDeltaDeliveryId)
import Perspectives.Persistence.PropertyHistoryStore (storeLocalDeltaWithHistory)
import Perspectives.Performance (timed)
import Perspectives.Persistent (entityExists, getPerspectRol, postDatabaseName)
import Perspectives.PerspectivesState (getPerspectivesUser, nextTransactionNumber, stompClient)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..), Value(..), perspectivesUser2RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), RoleType(..))
import Perspectives.ResourceIdentifiers (isInPublicScheme, takeGuid)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.OutgoingTransaction (OutgoingTransaction(..))
import Perspectives.Sync.PropertyHistory (checkpoint)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (PublicKeyInfo, Transaction(..), TransactionDestination(..), appendTransactionDelta)
import Perspectives.Sync.TransactionForPeer (EncryptedTransactionForPeer(..), TransactionForPeer(..), addToTransactionForPeer, currentTransactionForPeer, deduplicatePublicKeys, removeUnusedPublicKeys, transactieID, withoutPredefinedContextScaffolding)
import Perspectives.Types.ObjectGetters (isPublicProxy)
import Perspectives.UnschemedIdentifiers (UnschemedResourceIdentifier, unschemePerspectivesUser)
import Prelude (Unit, bind, compare, discard, eq, flip, map, not, pure, show, unit, void, when, ($), (&&), (*>), (<$>), (<<<), (<>), (==), (>=), (>=>), (>>=), (>>>), (||))
import Simple.JSON (writeJSON)

-- | Splits the transaction in versions specific for each peer and public role, but does not send them.
-- | Send the peer versions with `sendPeerTransactions`; select the public role versions with `publicRoleTransactions`.
-- | The caller should publish the public role versions before sending to peers, so that peers can retrieve
-- | the public resources they learn about.
-- IMPLEMENTATION NOTICE: we cannot handle the public role versions here by calling executeTransactionForPublicRole
-- because that would need cyclic model imports.
distributeTransaction :: Transaction -> MonadPerspectives TransactionPerUser
distributeTransaction (Transaction tr@{ changedDomeinFiles }) = do
  for_ changedDomeinFiles (ModelUri >>> saveCachedDomeinFile)
  -- Sort the deltas into the required partial order before distributing to peers.
  let sortedTransaction = Transaction tr { deltas = sortTransactionDeltas tr.deltas }
  addPublicKeysToTransaction sortedTransaction >>= transactieForEachUser

publicRoleTransactions :: TransactionPerUser -> TransactionPerUser
publicRoleTransactions = Map.filterKeys case _ of
  PublicDestination _ -> true
  Peer _ _ -> false

sendPeerTransactions :: TransactionPerUser -> MonadPerspectives Unit
sendPeerTransactions customizedTransacties =
  for_ (Map.values peerGroups) \(Tuple transaction recipients) -> do
    recipientKeys <- catMaybes <$> for recipients recipientTransportKey
    if null recipientKeys then pure unit
    else do
      encryptedPayload <- liftAff $ encryptForRecipients (writeJSON transaction) (map (\(Tuple _ key) -> { recipient: unwrap key.recipient, transportKey: key.transportKey }) recipientKeys)
      let
        encryptedTransaction = EncryptedTransactionForPeer
          { author: (unwrap transaction).author
          , perspectivesSystem: (unwrap transaction).perspectivesSystem
          , timeStamp: (unwrap transaction).timeStamp
          , encryptedDeltas:
              { ciphertext: encryptedPayload.ciphertext
              , iv: encryptedPayload.iv
              , wrappedKeys: map (\(Tuple _ key) -> { recipient: key.recipient, wrappedKey: fromRecipient key.recipient encryptedPayload.wrappedKeys }) recipientKeys
              }
          , publicKeys: (unwrap transaction).publicKeys
          }
      for_ recipientKeys \(Tuple recipient _) ->
        sendTransactieToUserUsingAMQPWithDelivery recipient encryptedTransaction
          (unwrap transaction).deltas
  where
  peerGroups :: Map.Map String (Tuple TransactionForPeer (Array (Tuple UnschemedResourceIdentifier PerspectivesUser)))
  peerGroups =
    foldl collectPeerGroup Map.empty (Map.toUnfoldable customizedTransacties :: Array (Tuple TransactionDestination TransactionForPeer))

  collectPeerGroup :: Map.Map String (Tuple TransactionForPeer (Array (Tuple UnschemedResourceIdentifier PerspectivesUser))) -> Tuple TransactionDestination TransactionForPeer -> Map.Map String (Tuple TransactionForPeer (Array (Tuple UnschemedResourceIdentifier PerspectivesUser)))
  collectPeerGroup groups (Tuple destination transaction) = case destination of
    Peer guid resource ->
      let
        recipient = Tuple guid resource
        transaction' = removeUnusedPublicKeys transaction
        serialised = writeJSON transaction'
      in
        case Map.lookup serialised groups of
          Nothing -> Map.insert serialised (Tuple transaction' [ recipient ]) groups
          Just (Tuple groupedTransaction recipients) -> Map.insert serialised (Tuple groupedTransaction (snoc recipients recipient)) groups
    PublicDestination _ -> groups

  -- | Reads the transport key from the schemed PerspectivesUsers resource that computeUserRoleBottom selected
  -- | (a local `def:` copy if available, otherwise the published `pub:` resource).
  recipientTransportKey :: Tuple UnschemedResourceIdentifier PerspectivesUser -> MonadPerspectives (Maybe (Tuple UnschemedResourceIdentifier RecipientKey))
  recipientTransportKey (Tuple recipient resource) = do
    mtransportKey <- perspectivesUser2RoleInstance resource ##> getProperty (EnumeratedPropertyType perspectivesUsersTransportPublicKey)
    case mtransportKey of
      Just (Value transportKey) -> pure $ Just $ Tuple recipient { recipient, transportKey }
      _ -> pure Nothing

  fromRecipient :: UnschemedResourceIdentifier -> Array { recipient :: String, wrappedKey :: String } -> String
  fromRecipient recipient wrappedKeys =
    unsafePartial $ fromJust $ _.wrappedKey <$> head (filter (\wk -> wk.recipient == unwrap recipient) wrappedKeys)

-- | Send a transaction using the Couchdb Channel.
sendTransactieToUserUsingCouchdb :: Url -> String -> TransactionForPeer -> MonadPerspectives Unit
sendTransactieToUserUsingCouchdb cdbUrl userId t = do
  userType <- roleType_ (RoleInstance userId)
  getChannel <- getDynamicPropertyGetter DEP.userChannel (ST $ userType)
  mchannel <- (RoleInstance userId) ##> getChannel
  case mchannel of
    Nothing -> pure unit
    Just (Value channel) -> do
      transactionNumber <- nextTransactionNumber
      void $ addDocument (cdbUrl <> channel) t (transactieID t <> "_" <> show transactionNumber)

-- | `userId` WILL be either 
-- |   * a model://perspectives.domains#System$PerspectivesSystem$User instance, or
-- |   * an instance of the Visitor role.
sendTransactieToUserUsingAMQP :: UnschemedResourceIdentifier -> EncryptedTransactionForPeer -> MonadPerspectives Unit
sendTransactieToUserUsingAMQP perspectivesUser t =
  sendTransactieToUserUsingAMQPWithDelivery perspectivesUser t []

sendTransactieToUserUsingAMQPWithDelivery :: UnschemedResourceIdentifier -> EncryptedTransactionForPeer -> Array SignedDelta -> MonadPerspectives Unit
sendTransactieToUserUsingAMQPWithDelivery perspectivesUser t@(EncryptedTransactionForPeer { timeStamp }) deliveredDeltas = do
  connected <- connectedToAMQPBroker
  n <- liftEffect $ now
  dt <- pure $ SerializableDateTime (toDateTime n)
  transactionNumber <- nextTransactionNumber
  -- Distinct packets queued in the same millisecond must not replace each other.
  messageId <- pure $ unwrap perspectivesUser <> show dt <> "_" <> show transactionNumber
  let
    persist = savePeerTransactionInOutgoingPost perspectivesUser messageId t deliveredDeltas
  if connected then do
    mstompClient <- stompClient
    case mstompClient of
      Just stompClient -> do
        -- If sending fails, we will still save the transaction in the OutgoingTransactions post database, so that it can be retried later.
        -- The transaction will be removed when incomingPost receives an acknowledgement from the RabbitMQ broker, meaning 
        -- that the broker has accepted and stored the message.
        persist
        -- Just send the message to the topic that is the addressees PerspectivesUser instance.
        -- Each system will listen to a queue that is bound to that topic upon subscription.
        debugBroker $ "Sending transaction for user " <> unwrap perspectivesUser <> " to AMQP broker with transaction timestamp " <> show timeStamp
        liftEffect $ sendToTopic stompClient perspectivesUser messageId (writeJSON t)
      otherwise -> persist
  else persist

  where
  connectedToAMQPBroker :: MonadPerspectives Boolean
  connectedToAMQPBroker = do
    mySystem <- getMySystem
    mConnected <- (RoleInstance $ buitenRol mySystem) ##> getProperty (EnumeratedPropertyType DEP.connectedToAMQPBroker)
    pure $ mConnected == (Just $ Value "true")

saveTransactionInOutgoingPost :: UnschemedResourceIdentifier -> String -> EncryptedTransactionForPeer -> MonadPerspectives Unit
saveTransactionInOutgoingPost userId messageId t@(EncryptedTransactionForPeer { timeStamp }) = timed "outbox.save" messageId do
  debugBroker $ "Saving transaction for user " <> unwrap userId <> " in OutgoingTransactions post database with transaction timestamp " <> show timeStamp
  postDB <- postDatabaseName
  existing <- tryGetDocument_ postDB messageId
  case existing of
    Just (OutgoingTransaction { receiver, transaction }) ->
      if receiver == userId && writeJSON transaction == writeJSON t then pure unit
      else throwError $ error $ "Refusing to replace an outgoing packet: " <> messageId
    Nothing -> do
      revision <- addDocument_ postDB (OutgoingTransaction { _id: messageId, receiver: userId, transaction: t }) messageId
      case revision of
        Just _ -> pure unit
        Nothing -> throwError $ error $ "Outgoing packet persistence returned no revision: " <> messageId

savePeerTransactionInOutgoingPost :: UnschemedResourceIdentifier -> String -> EncryptedTransactionForPeer -> Array SignedDelta -> MonadPerspectives Unit
savePeerTransactionInOutgoingPost userId messageId encrypted deltas = do
  saveTransactionInOutgoingPost userId messageId encrypted
  recordQueuedPeerDeltas userId deltas

-- | An object of TransactionForPeer where the keys are the string value of RoleInstances, invariably identifying user roles.
-- | Must be either an instance of sys:PerspectivesSystem$User, or of a RoleInstance of a type with RoleKind Visitor.
type TransactionPerUser = Map.Map TransactionDestination TransactionForPeer

type RecipientKey =
  { recipient :: UnschemedResourceIdentifier
  , transportKey :: String
  }

-- | The Transaction holds Deltas and each Delta names user instances who should receive that Delta.
-- | This function builds a custom version of the Transaction for each such user.
-- | `users` in DeltaInTransaction will not always be model://perspectives.domains#System$PerspectivesSystem$User instances.
transactieForEachUser :: Transaction -> MonadPerspectives TransactionPerUser
transactieForEachUser t@(Transaction tr@{ timeStamp, deltas, userRoleBottoms, publicKeys }) = do
  sender <- over PerspectivesUser takeGuid <$> getPerspectivesUser
  transactions <- execStateT
    ( void $ for deltas \(DeltaInTransaction { users, delta }) -> do
        system <- lift getMySystem
        -- Lookup the ultimate filler for all users in the delta.
        (sysUsers :: Array TransactionDestination) <- pure $ catMaybes (flip Map.lookup userRoleBottoms <$> users)
        addDeltaToCustomisedTransactie sender delta (nub sysUsers) (ContextInstance system)
    )
    Map.empty
  pure $ Map.mapMaybeWithKey
    ( \destination transaction -> case destination of
        Peer _ _ ->
          let
            filtered = withoutPredefinedContextScaffolding transaction
          in
            if null (unwrap filtered).deltas then Nothing else Just filtered
        PublicDestination _ -> Just transaction
    )
    transactions
  where
  -- `destinations` WILL be either 
  --    * Peer model://perspectives.domains#System$TheWorld$PerspectivesUsers instances, or
  --    * PublicDestination roleInstance, where the latter is an instance of the Visitor role.
  addDeltaToCustomisedTransactie :: PerspectivesUser -> SignedDelta -> (Array TransactionDestination) -> ContextInstance -> StateT TransactionPerUser (MonadPerspectives) Unit
  addDeltaToCustomisedTransactie sender d@(SignedDelta { author }) destinations perspectivesSystem = for_
    destinations
    ( \destination -> case destination of
        peer@(Peer perspectivesUser _) ->
          if not $ eq perspectivesUser (unschemePerspectivesUser author) then do
            trs <- get
            case Map.lookup peer trs of
              -- TODO: verwijder public key info voor deltas die er niet toe doen voor deze user.
              Nothing -> put $ Map.insert peer (currentTransactionForPeer { author: sender, perspectivesSystem, timeStamp, deltas: [ d ], publicKeys }) trs
              Just trans -> put $ Map.insert peer (addToTransactionForPeer d trans) trs
          else pure unit
        publicRole@(PublicDestination _) -> do
          trs <- get
          case Map.lookup publicRole trs of
            Nothing -> put $ Map.insert publicRole (currentTransactionForPeer { author: sender, perspectivesSystem, timeStamp, deltas: [ d ], publicKeys }) trs
            Just trans -> put $ Map.insert publicRole (addToTransactionForPeer d trans) trs
    )

addDomeinFileToTransactie :: ID -> MonadPerspectivesTransaction Unit
addDomeinFileToTransactie dfId = AA.modify
  ( over Transaction \(t@{ changedDomeinFiles }) ->
      t { changedDomeinFiles = union changedDomeinFiles [ dfId ] }
  )

-- | If we have a public role instance, return [Tuple rid [PublicDestination rid]].
-- | If the role chain bottoms out in an instance of TheWorld$PerspectivesUsers, return Tuple rid <<the other PerspectivesSystem$Users>>.
-- | If the role chain bottoms out in another UserRole that is not TheWorld$NonPerspectivesUsers (e.g. Onlookers), return Tuple rid <<that role instance>>.
-- | Onlookers instances represent TCP subscribers (Transaction Collection Points) reached via RabbitMQ.
-- | Otherwise return an empty array.
-- | Also return an empty array if the PerspectivesUser has been cancelled.
-- | Also return an empty array for the fictive serialization user (def:#serializationuser), which is never a real peer.
-- | A Peer destination carries the schemed PerspectivesUsers resource to read peer data from. When the chain bottoms
-- | out in a public (`pub:`) PerspectivesUsers instance, we prefer the local `def:` copy if it exists (it is kept up to
-- | date by the peer's own deltas); otherwise we use the public resource (e.g. a peer known only via a published context).
computeUserRoleBottom :: RoleInstance -> MonadPerspectives (Array (Tuple RoleInstance TransactionDestination))
computeUserRoleBottom rid =
  if unwrap rid == "def:#serializationuser" then pure []
  else ((map ENR <<< roleType_ >=> isPublicProxy) rid) >>=
    if _ then pure [ Tuple rid (PublicDestination rid) ]
    else perspectivesUsersRole_ rid >>= case _ of
      Nothing -> pure []
      Just bottom -> do
        perspectivesUser <- preferLocalPerspectivesUser bottom
        ((perspectivesUser2RoleInstance perspectivesUser) ##> getProperty (EnumeratedPropertyType perspectivesUsersCancelled)) >>= case _ of
          Just (Value "true") -> pure []
          _ -> pure [ Tuple rid (Peer (unschemePerspectivesUser perspectivesUser) perspectivesUser) ]
  where
  preferLocalPerspectivesUser :: PerspectivesUser -> MonadPerspectives PerspectivesUser
  preferLocalPerspectivesUser u@(PerspectivesUser s) =
    if isInPublicScheme s then do
      let local = deltaAuthor2ResourceIdentifier (PerspectivesUser $ unwrap $ unschemePerspectivesUser u)
      entityExists (perspectivesUser2RoleInstance local) >>= if _ then pure local else pure u
    else pure u

-- | Add the delta at the end of the array, unless it is already in the transaction or there are no users (and ignore the own user).
-- | Only include 
-- |    * public roles
-- |    * instances of sys:PerspectivesSystem$User, but not the one that equals the local sys:Me.
addDelta :: DeltaInTransaction -> MonadPerspectivesTransaction Unit
addDelta = addDeltaWithStorage true

-- | Forward only retained history not yet durably queued for each destination.
addStoredDelta :: Array RoleInstance -> DeltaStoreRecord -> MonadPerspectivesTransaction Unit
addStoredDelta users record = addStoredDeltas users [ record ]

-- | Read knowledge once per resource/destination, not once per historical delta.
-- | In particular, forwarding fullhistory must not repeatedly decode an
-- | ever-growing knowledge document for every entry in the same history.
addStoredDeltas :: Array RoleInstance -> Array DeltaStoreRecord -> MonadPerspectivesTransaction Unit
addStoredDeltas users records = do
  destinations <- lift $ for users \user -> Tuple user <$> computeUserRoleBottom user
  let
    byResource = foldl
      (\acc record@(DeltaStoreRecord { resourceKey }) -> Map.insertWith (flip (<>)) resourceKey [ record ] acc)
      Map.empty
      records
  for_ (Map.toUnfoldable byResource :: Array (Tuple String (Array DeltaStoreRecord))) \(Tuple resourceKey history) -> do
    knowledge <- lift $ for destinations \(Tuple user bottoms) -> do
      known <- for bottoms \(Tuple _ destination) -> case destination of
        PublicDestination _ -> pure Nothing
        Peer peer _ -> Just <$> peerDeliveryKnowledge peer resourceKey
      pure $ Tuple user known
    for_ history \record@(DeltaStoreRecord { signedDelta, deltaType, resourceVersion }) -> do
      deltaId <- lift $ storedDeltaDeliveryId record
      let
        unknown = filter
          ( \(Tuple _ known) -> null known || any
              ( case _ of
                  Nothing -> true
                  Just peer ->
                    not (Set.member deltaId peer.knownIds) &&
                      not (deltaType == "PropertyCheckpoint" && maybe false (\version -> version >= resourceVersion) peer.maxVersion)
              )
              known
          )
          knowledge
        users' = map (\(Tuple user _) -> user) unknown
      addDeltaWithStorage false $ DeltaInTransaction { users: users', delta: signedDelta }

-- | State entry must explicitly reinitialise a newly gained perspective, even
-- | when the same peer saw its data before losing visibility.
addStoredDeltaForInitialVisibility :: Array RoleInstance -> DeltaStoreRecord -> MonadPerspectivesTransaction Unit
addStoredDeltaForInitialVisibility users (DeltaStoreRecord { signedDelta }) =
  addDeltaWithStorage false $ DeltaInTransaction { users, delta: signedDelta }

addDeltaWithStorage :: Boolean -> DeltaInTransaction -> MonadPerspectivesTransaction Unit
addDeltaWithStorage shouldStore (DeltaInTransaction deltarecord@{ users, delta }) = do
  -- Store locally-created deltas in the DeltaStore for persistent history.
  -- Skip storage when processing incoming deltas (executeTransaction / executeDeltas for public roles):
  -- in those paths the delta is either already stored (executeTransaction uses executeDeltaWithVersionTracking)
  -- or must not be stored at all (executeDeltas for public roles uses modified author identifiers).
  isExecuting <- AA.gets (\(Transaction tr) -> tr.isExecutingIncomingDeltas)
  let SignedDelta { author, encryptedDelta } = delta
  lift $ traceDelta $ "addDelta: isExecutingIncomingDeltas=" <> show isExecuting
    <> "; recipients="
    <> show users
    <> "; delta="
    <> show (extractDeltaInfo encryptedDelta)
  when (shouldStore && not isExecuting) $ lift $ storeLocalDeltaWithHistory delta
  -- NOTE. Even though we try not to create deltas with roles that represent me, on system installation this can go wrong.
  users' <- lift $ filterA notIsMe users
  if null users' || isExecuting then
    lift $ traceDelta $ "addDelta: not queued; isExecutingIncomingDeltas=" <> show isExecuting
      <> "; recipients after excluding local user="
      <> show users'
  else do
    newUserBottoms <- lift (concat <$> for users' computeUserRoleBottom)
    newDelta <- pure (DeltaInTransaction deltarecord { users = users' })
    lift $ infoDelta $ "Adding a delta to transaction for users " <> show users' <> " with user role bottoms " <> show newUserBottoms
      <> ", author "
      <> show author
      <> ", delta "
      <> show (extractDeltaInfo encryptedDelta)
    AA.modify
      ( over Transaction
          ( \t@{ userRoleBottoms } ->
              t { userRoleBottoms = foldl (\userBottoms' (Tuple role user) -> Map.insert role user userBottoms') userRoleBottoms newUserBottoms }
          ) <<< appendTransactionDelta newDelta
      )

-- | Instrumental for QUERY UPDATES.
addCorrelationIdentifiersToTransactie :: Array CorrelationIdentifier -> MonadPerspectivesTransaction Unit
addCorrelationIdentifiersToTransactie corrIds = AA.modify (over Transaction \t@{ correlationIdentifiers } -> t { correlationIdentifiers = union correlationIdentifiers corrIds })

addCreatedContextToTransaction :: ContextInstance -> MonadPerspectivesTransaction Unit
addCreatedContextToTransaction cid =
  AA.modify
    ( over Transaction \t@{ createdContexts } -> t
        { createdContexts =
            if isJust $ elemIndex cid createdContexts then createdContexts
            else snoc createdContexts cid
        }
    )

addCreatedRoleToTransaction :: RoleInstance -> MonadPerspectivesTransaction Unit
addCreatedRoleToTransaction rid =
  AA.modify
    ( over Transaction \t@{ createdRoles } -> t
        { createdRoles =
            if isJust $ elemIndex rid createdRoles then createdRoles
            else snoc createdRoles rid
        }
    )

addPublicKeysToTransaction :: Transaction -> MonadPerspectives Transaction
addPublicKeysToTransaction (Transaction tr@{ deltas }) = do
  publicKeys :: Map.Map PerspectivesUser PublicKeyInfo <- execStateT (void $ for deltas addPublicKeyInfo) Map.empty
  bundles <- liftEffect $ deduplicatePublicKeys $ ENCMAP.EncodableMap publicKeys
  pure $ Transaction tr { publicKeys = bundles }

  where
  addPublicKeyInfo :: DeltaInTransaction -> StateT (Map.Map PerspectivesUser PublicKeyInfo) MonadPerspectives Unit
  addPublicKeyInfo (DeltaInTransaction { delta }) = do
    addAuthor (unwrap delta).author
    case checkpoint (unwrap delta).encryptedDelta of
      Just { winner } ->
        addAuthor (unwrap winner).author
      _ -> pure unit

  addAuthor :: PerspectivesUser -> StateT (Map.Map PerspectivesUser PublicKeyInfo) MonadPerspectives Unit
  addAuthor author = do
    keys <- get
    case Map.lookup author keys of
      Just _ -> pure unit
      Nothing -> do
        mpkInfo <- lift $ getPkInfo author
        case mpkInfo of
          Nothing -> pure unit
          Just pkInfo -> void $ modify (\keys' -> Map.insert author pkInfo keys')

  -- This is built on the assumption that the argument is the string value of the RoleInstance of type TheWorld$PerspectivesUser
  -- that fills SocialEnvironment$Me
  -- Queries the DeltaStore for the creation deltas and public key property delta of this PerspectivesUser role instance.
  getPkInfo :: PerspectivesUser -> MonadPerspectives (Maybe PublicKeyInfo)
  getPkInfo perspectivesUser = do
    -- The perspectivesUser is taken from the SignedDelta and is schemaless.
    let roleInstanceId = perspectivesUser2RoleInstance $ deltaAuthor2ResourceIdentifier perspectivesUser
    authorRole <- getPerspectRol roleInstanceId
    -- Get creation deltas for this role instance (UniverseRoleDelta + ContextDelta).
    creationDeltas <- getDeltasForResource (unwrap roleInstanceId)
    -- Get property deltas for the public key property.
    pkPropertyDeltas <- getDeltasForResource (unwrap roleInstanceId <> "#" <> unwrap (EnumeratedPropertyType perspectivesUsersPublicKey))
    transportPkPropertyDeltas <- getDeltasForResource (unwrap roleInstanceId <> "#" <> unwrap (EnumeratedPropertyType perspectivesUsersTransportPublicKey))
    let allDeltas = map (\(DeltaStoreRecord { signedDelta }) -> signedDelta) (creationDeltas <> pkPropertyDeltas <> transportPkPropertyDeltas)
    case head $ rol_property authorRole (EnumeratedPropertyType perspectivesUsersPublicKey), head $ rol_property authorRole (EnumeratedPropertyType perspectivesUsersTransportPublicKey) of
      Just (Value key), Just (Value transportKey) -> pure $ Just { key, transportKey, deltas: allDeltas }
      Nothing, _ -> logPerspectivesError (NoPublicKeyForAuthor (unwrap roleInstanceId)) *> pure Nothing
      _, Nothing -> logPerspectivesError (NoPublicKeyForAuthor (unwrap roleInstanceId)) *> pure Nothing

-- | Priority ordering for delta types. Lower number = executed first.
-- | Implements the partial order required for correct peer execution:
-- | - Context creation before role creation
-- | - Role creation before adding roles to a context (UniverseRoleDelta before ContextDelta)
-- | - Role-context linking before binding/property updates
-- | - Role/context construction before removal
-- | Within each delta type:
-- | - UniverseRoleDelta: ConstructEmptyRole/ConstructExternalRole before RemoveRoleInstance/…
-- | - ContextDelta: AddRoleInstancesToContext/AddExternalRole before MoveRoleInstancesToAnotherContext
-- | - RoleBindingDelta: SetFirstBinding before RemoveBinding/ReplaceBinding
-- | - Property mutations and checkpoints share a priority and replay by resource version.
deltaTypeSortPriority :: String -> Int
deltaTypeSortPriority dt
  | dt == "ConstructExternalRole" = 0
  | dt == "ConstructEmptyContext" = 1
  | dt == "ConstructEmptyRole" = 2
  | dt == "AddExternalRole" = 2
  | dt == "AddRoleInstancesToContext" = 3
  | dt == "SetFirstBinding" = 4
  | dt == "AddProperty" = 5
  | dt == "SetProperty" = 5
  | dt == "PropertyCheckpoint" = 5
  | dt == "UploadFile" = 5
  | dt == "MoveRoleInstancesToAnotherContext" = 5
  | dt == "RemoveRoleInstance" = 6
  | dt == "RemoveUnboundExternalRoleInstance" = 6
  | dt == "RemoveExternalRoleInstance" = 6
  | dt == "RetractRoleInstance" = 6
  | dt == "RemoveBinding" = 6
  | dt == "ReplaceBinding" = 6
  | dt == "RemoveProperty" = 5
  | dt == "DeleteProperty" = 5
  | true = 99

-- | Sort the deltas in an array into the correct partial order for execution by peers.
-- | Uses the delta type priority as the primary sort key. Property histories
-- | are grouped by resource key and replayed numerically, not lexicographically
-- | and not grouped by operation (which would incorrectly move removals).
-- | Deltas whose type cannot be determined are placed at the end.
sortTransactionDeltas :: Array DeltaInTransaction -> Array DeltaInTransaction
sortTransactionDeltas = sortBy compareDeltaInTransaction
  where
  compareDeltaInTransaction :: DeltaInTransaction -> DeltaInTransaction -> Ordering
  compareDeltaInTransaction
    (DeltaInTransaction { delta: SignedDelta { encryptedDelta: d1 } })
    (DeltaInTransaction { delta: SignedDelta { encryptedDelta: d2 } }) =
    let
      mInfo1 = extractDeltaInfo d1
      mInfo2 = extractDeltaInfo d2
      priority1 = maybe 99 (\i -> deltaTypeSortPriority i.deltaType) mInfo1
      priority2 = maybe 99 (\i -> deltaTypeSortPriority i.deltaType) mInfo2
      version1 = maybe 0 _.resourceVersion mInfo1
      version2 = maybe 0 _.resourceVersion mInfo2
      key1 = maybe "" _.resourceKey mInfo1
      key2 = maybe "" _.resourceKey mInfo2
    in
      compare priority1 priority2
        <>
          if priority1 == 5 && priority2 == 5 then
            compare key1 key2 <> compare version1 version2
          else compare 0 0
