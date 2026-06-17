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
import Control.Monad.State.Trans (StateT, execStateT, get, lift, modify, put)
import Data.Array (catMaybes, concat, elemIndex, filterA, foldl, head, nub, null, snoc, sortBy, union)
import Data.DateTime.Instant (toDateTime)
import Data.Map (Map, empty, insert, lookup, filter) as Map
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (over, unwrap)
import Data.Ordering (Ordering)
import Data.Traversable (for, for_)
import Data.TraversableWithIndex (forWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Now (now)
import Partial.Unsafe (unsafePartial)
import Perspectives.AMQP.Stomp (sendToTopic)
import Perspectives.ApiTypes (CorrelationIdentifier)
import Perspectives.ContextAndRole (rol_property)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##>))
import Perspectives.Data.EncodableMap as ENCMAP
import Perspectives.DomeinCache (saveCachedDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Me (notIsMe)
import Perspectives.Instances.ObjectGetters (deltaAuthor2ResourceIdentifier, getProperty, perspectivesUsersRole_, roleType_)
import Perspectives.Logging (debugBroker, infoDelta)
import Perspectives.ModelDependencies (connectedToAMQPBroker, userChannel) as DEP
import Perspectives.ModelDependencies (perspectivesUsersCancelled, perspectivesUsersPublicKey)
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (Url, addDocument)
import Perspectives.Persistence.DeltaStore (extractDeltaInfo, getDeltasForResource, storeDeltaFromSignedDelta)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistent (getPerspectRol, postDatabaseName)
import Perspectives.PerspectivesState (nextTransactionNumber, stompClient)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser, RoleInstance(..), Value(..), perspectivesUser2RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), RoleType(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.OutgoingTransaction (OutgoingTransaction(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (PublicKeyInfo, Transaction(..), TransactionDestination(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), addToTransactionForPeer, transactieID)
import Perspectives.Types.ObjectGetters (isPublicProxy)
import Perspectives.UnschemedIdentifiers (UnschemedResourceIdentifier, unschemePerspectivesUser)
import Prelude (Unit, bind, compare, discard, eq, flip, map, not, pure, show, unit, void, when, ($), (*>), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>), (||))
import Simple.JSON (writeJSON)

-- | Splits the transaction in versions specific for each peer and sends them.
-- | If a public roles are involved, will return their TransactionForPeer instances.
-- IMPLEMENTATION NOTICE: we cannot handle these instances here by calling executeTransactionForPublicRole
-- because that would need cyclic model imports.
distributeTransaction :: Transaction -> MonadPerspectives TransactionPerUser
distributeTransaction (Transaction tr@{ changedDomeinFiles }) = do
  for_ changedDomeinFiles (ModelUri >>> saveCachedDomeinFile)
  -- Sort the deltas into the required partial order before distributing to peers.
  let sortedTransaction = Transaction tr { deltas = sortTransactionDeltas tr.deltas }
  -- Send the Transaction to all involved.
  addPublicKeysToTransaction sortedTransaction >>= distributeTransactie'

distributeTransactie' :: Transaction -> MonadPerspectives TransactionPerUser
distributeTransactie' t = do
  (customizedTransacties :: TransactionPerUser) <- transactieForEachUser t
  map (unsafePartial fromJust) <<< Map.filter isJust <$> forWithIndex customizedTransacties sendTransactie

-- | If we have the visitor user, handle it by augmenting resources in the public store with the deltas.
sendTransactie :: TransactionDestination -> TransactionForPeer -> MonadPerspectives (Maybe TransactionForPeer)
-- Remove keys that are not required for the peer, given the deltas.
-- Note that this is a suboptimal approach, as we would only have had to do this once for each user type.
sendTransactie (Peer perspectivesUser) t = sendTransactieToUserUsingAMQP perspectivesUser (removeUnnecessaryKeys t) *> pure Nothing
  where
  removeUnnecessaryKeys :: TransactionForPeer -> TransactionForPeer
  removeUnnecessaryKeys (TransactionForPeer rec@{ deltas, publicKeys }) =
    let
      (occurringUsers :: Array PerspectivesUser) = map (_.author <<< unwrap) deltas
    in
      TransactionForPeer rec { publicKeys = ENCMAP.filterKeys (\k -> isJust $ elemIndex k occurringUsers) publicKeys }
sendTransactie (PublicDestination r) t = pure $ Just t

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
sendTransactieToUserUsingAMQP :: UnschemedResourceIdentifier -> TransactionForPeer -> MonadPerspectives Unit
sendTransactieToUserUsingAMQP perspectivesUser t@(TransactionForPeer {timeStamp}) = do
  connected <- connectedToAMQPBroker
  n <- liftEffect $ now
  dt <- pure $ SerializableDateTime (toDateTime n)
  messageId <- pure $ unwrap perspectivesUser <> show dt
  if connected then do
    mstompClient <- stompClient
    case mstompClient of
      Just stompClient -> do
        saveTransactionInOutgoingPost perspectivesUser messageId t
        -- Just send the message to the topic that is the addressees PerspectivesUser instance.
        -- Each system will listen to a queue that is bound to that topic upon subscription.
        debugBroker $ "Sending transaction for user " <> unwrap perspectivesUser <> " to AMQP broker with transaction timestamp " <> show timeStamp
        liftEffect $ sendToTopic stompClient perspectivesUser messageId (writeJSON t)
      otherwise -> saveTransactionInOutgoingPost perspectivesUser messageId t
  else saveTransactionInOutgoingPost perspectivesUser messageId t

  where
  connectedToAMQPBroker :: MonadPerspectives Boolean
  connectedToAMQPBroker = do
    mySystem <- getMySystem
    mConnected <- (RoleInstance $ buitenRol mySystem) ##> getProperty (EnumeratedPropertyType DEP.connectedToAMQPBroker)
    pure $ mConnected == (Just $ Value "true")

saveTransactionInOutgoingPost :: UnschemedResourceIdentifier -> String -> TransactionForPeer -> MonadPerspectives Unit
saveTransactionInOutgoingPost userId messageId t@(TransactionForPeer {timeStamp}) = do
  debugBroker $ "Saving transaction for user " <> unwrap userId <> " in OutgoingTransactions post database with transaction timestamp " <> show timeStamp
  postDB <- postDatabaseName
  void $ addDocument postDB (OutgoingTransaction { _id: messageId, receiver: userId, transaction: t }) messageId

-- | An object of TransactionForPeer where the keys are the string value of RoleInstances, invariably identifying user roles.
-- | Must be either an instance of sys:PerspectivesSystem$User, or of a RoleInstance of a type with RoleKind Visitor.
type TransactionPerUser = Map.Map TransactionDestination TransactionForPeer

-- | The Transaction holds Deltas and each Delta names user instances who should receive that Delta.
-- | This function builds a custom version of the Transaction for each such user.
-- | `users` in DeltaInTransaction will not always be model://perspectives.domains#System$PerspectivesSystem$User instances.
transactieForEachUser :: Transaction -> MonadPerspectives TransactionPerUser
transactieForEachUser t@(Transaction tr@{ timeStamp, deltas, userRoleBottoms, publicKeys }) = do
  execStateT
    ( void $ for deltas \(DeltaInTransaction { users, delta }) -> do
        system <- lift getMySystem
        -- Lookup the ultimate filler for all users in the delta.
        (sysUsers :: Array TransactionDestination) <- pure $ catMaybes (flip Map.lookup userRoleBottoms <$> users)
        addDeltaToCustomisedTransactie delta (nub sysUsers) (ContextInstance system)
    )
    Map.empty
  where
  -- `destinations` WILL be either 
  --    * Peer model://perspectives.domains#System$TheWorld$PerspectivesUsers instances, or
  --    * PublicDestination roleInstance, where the latter is an instance of the Visitor role.
  addDeltaToCustomisedTransactie :: SignedDelta -> (Array TransactionDestination) -> ContextInstance -> StateT TransactionPerUser (MonadPerspectives) Unit
  addDeltaToCustomisedTransactie d@(SignedDelta { author }) destinations perspectivesSystem = for_
    destinations
    ( \destination -> case destination of
        peer@(Peer perspectivesUser) ->
          if not $ eq perspectivesUser (unschemePerspectivesUser author) then do
            trs <- get
            case Map.lookup peer trs of
              -- TODO: verwijder public key info voor deltas die er niet toe doen voor deze user.
              Nothing -> put $ Map.insert peer (TransactionForPeer { author, perspectivesSystem, timeStamp, deltas: [ d ], publicKeys }) trs
              Just trans -> put $ Map.insert peer (addToTransactionForPeer d trans) trs
          else pure unit
        publicRole@(PublicDestination _) -> do
          trs <- get
          case Map.lookup publicRole trs of
            Nothing -> put $ Map.insert publicRole (TransactionForPeer { author, perspectivesSystem, timeStamp, deltas: [ d ], publicKeys }) trs
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
computeUserRoleBottom :: RoleInstance -> MonadPerspectives (Array (Tuple RoleInstance TransactionDestination))
computeUserRoleBottom rid =
  if unwrap rid == "def:#serializationuser" then pure []
  else ((map ENR <<< roleType_ >=> isPublicProxy) rid) >>=
    if _ then pure [ Tuple rid (PublicDestination rid) ]
    else perspectivesUsersRole_ rid >>= case _ of
      Nothing -> pure []
      Just perspectivesUser -> ((perspectivesUser2RoleInstance perspectivesUser) ##> getProperty (EnumeratedPropertyType perspectivesUsersCancelled)) >>= case _ of
        Just (Value "true") -> pure []
        _ -> pure [ Tuple rid (Peer $ unschemePerspectivesUser perspectivesUser) ]

-- | Add the delta at the end of the array, unless it is already in the transaction or there are no users (and ignore the own user).
-- | Only include 
-- |    * public roles
-- |    * instances of sys:PerspectivesSystem$User, but not the one that equals the local sys:Me.
addDelta :: DeltaInTransaction -> MonadPerspectivesTransaction Unit
addDelta (DeltaInTransaction deltarecord@{ users, delta }) = do
  -- Store locally-created deltas in the DeltaStore for persistent history.
  -- Skip storage when processing incoming deltas (executeTransaction / executeDeltas for public roles):
  -- in those paths the delta is either already stored (executeTransaction uses executeDeltaWithVersionTracking)
  -- or must not be stored at all (executeDeltas for public roles uses modified author identifiers).
  isExecuting <- AA.gets (\(Transaction tr) -> tr.isExecutingIncomingDeltas)
  when (not isExecuting) $ lift $ storeDeltaFromSignedDelta delta
  -- NOTE. Even though we try not to create deltas with roles that represent me, on system installation this can go wrong.
  users' <- (lift $ filterA notIsMe users)
  if null users' || isExecuting then pure unit
  else do
    newUserBottoms <- lift (concat <$> for users' computeUserRoleBottom)
    newDelta <- pure (DeltaInTransaction deltarecord { users = users' })
    lift $ infoDelta $ "Adding a delta to transaction for users " <> show users' <> " with user role bottoms " <> show newUserBottoms
    AA.modify
      ( over Transaction \t@{ deltas, userRoleBottoms } ->
          let
            alreadyPresent = isJust $ elemIndex newDelta deltas
          in
            t
              { deltas =
                  if alreadyPresent then deltas
                  else snoc deltas newDelta
              , userRoleBottoms = foldl (\userBottoms' (Tuple role user) -> Map.insert role user userBottoms') userRoleBottoms newUserBottoms
              }
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
  pure $ Transaction tr { publicKeys = ENCMAP.EncodableMap publicKeys }

  where
  addPublicKeyInfo :: DeltaInTransaction -> StateT (Map.Map PerspectivesUser PublicKeyInfo) MonadPerspectives Unit
  addPublicKeyInfo (DeltaInTransaction { delta }) = case delta of
    SignedDelta { author } -> do
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
    let allDeltas = map (\(DeltaStoreRecord { signedDelta }) -> signedDelta) (creationDeltas <> pkPropertyDeltas)
    case head $ rol_property authorRole (EnumeratedPropertyType perspectivesUsersPublicKey) of
      Nothing -> logPerspectivesError (NoPublicKeyForAuthor (unwrap roleInstanceId)) *> pure Nothing
      Just (Value key) -> pure $ Just { key, deltas: allDeltas }

-- | Priority ordering for delta types. Lower number = executed first.
-- | Implements the partial order required for correct peer execution:
-- | - Context creation before role creation
-- | - Role creation before adding roles to a context (UniverseRoleDelta before ContextDelta)
-- | - Role-context linking before binding/property updates
-- | - Construction/addition before removal/move operations
-- | Within each delta type:
-- | - UniverseRoleDelta: ConstructEmptyRole/ConstructExternalRole before RemoveRoleInstance/…
-- | - ContextDelta: AddRoleInstancesToContext/AddExternalRole before MoveRoleInstancesToAnotherContext
-- | - RoleBindingDelta: SetFirstBinding before RemoveBinding/ReplaceBinding
-- | - RolePropertyDelta: AddProperty/SetProperty/UploadFile before RemoveProperty/DeleteProperty
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
  | dt == "UploadFile" = 5
  | dt == "MoveRoleInstancesToAnotherContext" = 5
  | dt == "RemoveRoleInstance" = 6
  | dt == "RemoveUnboundExternalRoleInstance" = 6
  | dt == "RemoveExternalRoleInstance" = 6
  | dt == "RemoveBinding" = 6
  | dt == "ReplaceBinding" = 6
  | dt == "RemoveProperty" = 7
  | dt == "DeleteProperty" = 7
  | true = 99

-- | Sort the deltas in an array into the correct partial order for execution by peers.
-- | Uses the delta type priority as the primary sort key, followed by resource version
-- | and resource key for a deterministic stable order within the same priority level.
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
        -- <> compare version1 version2
        -- <> compare key1 key2
