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

module Perspectives.Sync.HandleTransaction where

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Except (lift, runExcept, runExceptT)
import Control.Monad.State (StateT, gets, modify, runStateT) as ST
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Array (catMaybes, concat, filter, fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, isNothing)
import Data.MediaType (MediaType(..))
import Data.Newtype (over, unwrap)
import Data.Ordering (Ordering(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (addProperty, addRoleInstanceToContext, deleteProperty, moveRoleInstanceToAnotherContext, removeProperty, setProperty)
import Perspectives.Authenticate (deserializeJWK, tryGetPublicKey, verifyDelta, verifyDelta')
import Perspectives.Checking.Authorization (roleHasPerspectiveOnExternalRoleWithVerbs, roleHasPerspectiveOnPropertyWithVerb, roleHasPerspectiveOnRoleWithVerb)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord, getNextRolIndex)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, removeInternally, (###=), (###>>), (##=), (##>), (##>>))
import Perspectives.Data.EncodableMap as ENCMAP
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addCreatedContextToTransaction, addCreatedRoleToTransaction)
import Perspectives.DependencyTracking.Dependency (findRoleRequests)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (buitenRol, deconstructBuitenRol, typeUri2LocalName_, typeUri2ModelUri_)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.Builders (lookupOrCreateContextInstance, lookupOrCreateRoleInstance, createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (getProperty, roleType)
import Perspectives.Instances.Values (parsePerspectivesFile)
import Perspectives.ModelDependencies (rootContext)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (getAttachment)
import Perspectives.Persistent (addAttachment, entityExists, forceSaveRole, getPerspectRol, saveEntiteit, saveEntiteit_, tryGetPerspectEntiteit)
import Perspectives.PerspectivesState (transactionLevel)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ResourceType(..), RoleType(..), StateIdentifier(..), externalRoleType, roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerb(..)) as Verbs
import Perspectives.ResourceIdentifiers (createPublicIdentifier, isInPublicScheme, resourceIdentifier2DocLocator, resourceIdentifier2WriteDocLocator, takeGuid)
import Perspectives.SaveUserData (removeBinding, removeContextIfUnbound, replaceBinding, scheduleContextRemoval, scheduleRoleRemoval, setFirstBinding, synchronise)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sidecar.ToReadable (toReadable)
import Perspectives.StrippedDelta (addPublicResourceScheme, addResourceSchemes, addSchemeToResourceIdentifier)
import Perspectives.Persistence.DeltaStore (DeltaStoreRecord(..), storeDelta, deltaStoreDocId)
import Perspectives.Persistence.PendingTransactionStore (MissingDelta, storePendingTransaction)
import Perspectives.Persistence.ResourceVersionStore (getResourceVersion, setResourceVersion)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (PublicKeyInfo)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.Types.ObjectGetters (contextAspectsClosure, hasAspect, isPublic, roleAspectsClosure, publicUserRole)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), DeltaRecord, RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (class Eq, class Ord, Unit, bind, compare, discard, flip, map, pure, show, unit, void, ($), (*>), (+), (/=), (<$>), (<<<), (<>), (==), (>), (>>=))
import Simple.JSON (readJSON')

-- TODO. Each of the executing functions must catch errors that arise from unknown types.
-- Inspect the model version of an unknown type and establish whether the resident model is newer or older than
-- the arriving type. If newer, check whether the type is backwards compatible with the arriving older type.
-- If older, retrieve the newer version of the model and check whether the arriving type is backwards compatible with
-- the type in the resident model.
-- If the types are truly incompatible, either ignore the incoming delta (best solution if the resident model is newer)
-- or update to the newer model version (have the end user consent).

executeContextDelta :: ContextDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeContextDelta (ContextDelta { deltaType, contextInstance, contextType, roleType, roleInstance, destinationContext, subject }) signedDelta = do
  padding <- lift transactionLevel
  log (padding <> show deltaType <> " to/from " <> show contextInstance <> " and " <> show roleInstance)
  case deltaType of
    -- The subject must be allowed to change the role: they must have a perspective on it that includes:
    --  * the verb CreateAndFill, in case a context role is created;
    --  * the verb Create, in case another role is created.
    AddRoleInstancesToContext -> (lift $ roleHasPerspectiveOnRoleWithVerb subject roleType [ Verbs.Create, Verbs.CreateAndFill ] Nothing Nothing) >>= case _ of
      Left e -> handleError e
      -- Takes care of PERSISTENCE of both context and role.
      Right _ -> addRoleInstanceToContext contextInstance roleType (Tuple roleInstance (Just signedDelta))
    -- NOTE: the perspective should always include the Delete verb.
    MoveRoleInstancesToAnotherContext -> (lift $ roleHasPerspectiveOnRoleWithVerb subject roleType [ Verbs.Create, Verbs.CreateAndFill ] Nothing Nothing) >>= case _ of
      Left e -> handleError e
      Right _ -> moveRoleInstanceToAnotherContext contextInstance (unsafePartial $ fromJust destinationContext) roleType (Just signedDelta) roleInstance
    -- The contextDelta is stored in the DeltaStore via executeDeltaWithVersionTracking.
    -- No need to store it on the role record.
    AddExternalRole -> pure unit

executeRoleBindingDelta :: RoleBindingDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeRoleBindingDelta (RoleBindingDelta { filled, filler, deltaType, subject }) signedDelta = do
  padding <- lift transactionLevel
  log (padding <> show deltaType <> " of " <> show filled <> " (to) " <> show filler)
  roleType' <- lift (filled ##>> roleType)
  (lift $ roleHasPerspectiveOnRoleWithVerb subject roleType' [ Verbs.Fill, Verbs.CreateAndFill ] Nothing Nothing) >>= case _ of
    Left e -> handleError e
    Right _ -> case deltaType of
      SetFirstBinding -> void $ setFirstBinding filled (unsafePartial $ fromJust filler) (Just signedDelta)
      RemoveBinding -> void $ removeBinding filled
      ReplaceBinding -> void $ replaceBinding filled (unsafePartial $ fromJust filler) (Just signedDelta)

-- TODO. Wat met SetPropertyValue?
executeRolePropertyDelta :: RolePropertyDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeRolePropertyDelta d@(RolePropertyDelta { id, roleType, deltaType, values, property, subject }) signedDelta = do
  padding <- lift transactionLevel
  lift $ toReadable property >>= \readableProperty -> log (padding <> show deltaType <> " for " <> show id <> " and property " <> show readableProperty)
  case deltaType of
    AddProperty -> do
      -- we need not check whether the model is known if we assume valid transactions:
      -- a role instance creation delta must have preceded the property delta.
      (lift $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.AddPropertyValue) >>= case _ of
        -- This is mainly for historical reasons: we used to set properties by first removing and then adding them.
        -- This means that there are, in fact, in the Perspectives Universe property deltas with AddProperty that 
        -- have been made with a perspective of SetPropertyValue.
        Left e -> (lift $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.SetPropertyValue) >>= case _ of
          Left e1 -> handleError e1
          Right _ -> addProperty [ id ] property (flip Tuple (Just signedDelta) <$> values)
        Right _ -> addProperty [ id ] property (flip Tuple (Just signedDelta) <$> values)
    RemoveProperty -> (lift $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.RemovePropertyValue) >>= case _ of
      Left e -> handleError e
      Right _ -> removeProperty [ id ] property (Just signedDelta) values
    DeleteProperty -> (lift $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.DeleteProperty) >>= case _ of
      Left e -> handleError e
      Right _ -> deleteProperty [ id ] property (Just signedDelta)
    SetProperty -> (lift $ roleHasPerspectiveOnPropertyWithVerb subject id property Verbs.SetPropertyValue) >>= case _ of
      Left e -> handleError e
      Right _ -> setProperty [ id ] property (Just signedDelta) values
    UploadFile -> do
      -- Do this only when we're executing the Delta for a public role. 
      -- As this is a RolePropertyDelta, it was created when a file was added to the role for a property with a File range. 
      -- The only 'user' whose resource identifiers are all in the public scheme, is the virtual user for whom we make things public.
      -- This 'user' never changes a resource himself - he is not an active actor.
      -- Consequently, if we find, on executing this Delta, that its id is a public resource, we're acting for the public role.
      -- NOT for the 'real' user who uploaded the file.
      -- So, testing whether the id is in the public scheme is an adequate test to detect this case.
      if isInPublicScheme (unwrap id) then do
        -- Currently, the id is in the public scheme. Transform to the local scheme, so we can retrieve the attachment locally.
        storageSchemes <- lift $ gets _.typeToStorage
        { database, documentName } <- lift $ (addSchemeToResourceIdentifier storageSchemes (RType roleType) (takeGuid (unwrap id)) >>= resourceIdentifier2DocLocator)
        -- Retrieve the attachment from the local version of the role. 
        -- Use the last segment of the property type as the attachments name.
        matt <- lift $ getAttachment database documentName (typeUri2LocalName_ (unwrap property))
        case matt of
          Nothing -> pure unit
          Just att -> do
            -- Add the attachment to the public version of the role.
            { database: pubDatabase } <- lift $ resourceIdentifier2WriteDocLocator (unwrap id)
            prol <- lift $ getPerspectRol id
            mvalue <- lift (id ##> getProperty property)
            case mvalue of
              Nothing -> pure unit
              Just val -> case parsePerspectivesFile (unwrap val) of
                Left e -> pure unit
                Right rec -> do
                  -- First make sure the cached and saved role are the same and that the role is not waiting to be saved
                  lift $ forceSaveRole id
                  success <- lift $ addAttachment id (typeUri2LocalName_ (unwrap property)) att (MediaType rec.mimeType)
                  if success then do
                    -- The revision on the cached version is no longer valid.
                    -- Neither does it have the new attachment info.
                    void $ lift $ removeInternally id
                  else throwError (error ("Could not save file in the database"))

      else pure unit

-- TODO. Wat we nodig hebben, is een secundair kanaal naar de client waarin we
-- fouten en waarschuwingen kunnen sturen.
-- Totdat we dat hebben, zetten we een waarschuwing op de console.
handleError :: PerspectivesError -> MonadPerspectivesTransaction Unit
handleError e = liftEffect $ log (show e)

-----------------------------------------------------------
-- DELTA ORDERING TYPES AND HELPERS
-----------------------------------------------------------

-- | Minimal record to extract just the ordering fields from any delta JSON.
-- | All delta types share these fields via DeltaRecord.
type DeltaOrderingInfo =
  { resourceKey :: String
  , resourceVersion :: Int
  }

-- | Extract ordering info from a stringified delta.
-- | Returns Nothing if the JSON does not contain the required fields.
extractOrderingInfo :: String -> Maybe DeltaOrderingInfo
extractOrderingInfo stringifiedDelta = case runExcept $ readJSON' stringifiedDelta of
  Right (info :: DeltaOrderingInfo) -> Just info
  Left _ -> Nothing

-- | Check whether a transaction has any version gaps.
-- | A gap exists when a delta's resourceVersion is greater than the local version + 1.
-- | Returns the list of missing deltas (gaps) if any.
checkForGaps :: Array { resourceKey :: String, resourceVersion :: Int, author :: PerspectivesUser } -> MonadPerspectivesTransaction (Array MissingDelta)
checkForGaps deltaInfos = do
  gaps <- for deltaInfos \{ resourceKey, resourceVersion, author } -> do
    localVersion <- lift $ getResourceVersion resourceKey
    if resourceVersion > localVersion + 1 then pure $ Just { author, resourceKey, expectedVersion: localVersion + 1 }
    else pure Nothing
  pure $ catMaybes gaps

-- | Retrieves from the repository the model that holds the ContextType, if necessary.
-- | The ConstructExternalRole always precedes the UniverseContextDelta for the context it is the external
-- | role for. Hence we only have to check whether the external role exists.
executeUniverseContextDelta :: UniverseContextDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeUniverseContextDelta (UniverseContextDelta { id, contextType, deltaType, subject }) signedDelta = do
  padding <- lift transactionLevel
  lift $ toReadable contextType >>= \readableContextType -> log (padding <> show deltaType <> " with id " <> show id <> " and with type " <> show readableContextType)
  allTypes <- lift (contextType ###= contextAspectsClosure)
  externalRoleExists <- lift $ entityExists (RoleInstance $ buitenRol $ unwrap id)
  if externalRoleExists then case deltaType of
    ConstructEmptyContext -> void $ runExceptT $ lookupOrCreateContextInstance
      contextType
      (Just $ unwrap id)
      ( lift $ do
          (exists :: Maybe PerspectContext) <- lift $ tryGetPerspectEntiteit id
          -- This is where we make the operation idempotent. Nothing happens if it already exists.
          if isNothing exists then do
            contextInstance <- pure
              ( PerspectContext defaultContextRecord
                  { _id = takeGuid $ unwrap id
                  , id = id
                  , displayName = unwrap id
                  , pspType = contextType
                  , allTypes = allTypes
                  , buitenRol = RoleInstance $ buitenRol $ unwrap id
                  , states = [ StateIdentifier $ unwrap contextType ]
                  }
              )
            lift $ void $ saveEntiteit_ id contextInstance

            -- If the context type has a public role, create an instance of its proxy; but only when the contextId is not a public identifier.
            -- Proxies are not created for public contexts, as they are not needed.
            if isInPublicScheme (unwrap id) then pure unit
            else do
              publicRoles <- lift $ (contextType ###= publicUserRole)
              void $ for (EnumeratedRoleType <<< roletype2string <$> publicRoles)
                \t -> createAndAddRoleInstance
                  t
                  (unwrap id)
                  (RolSerialization { id: Nothing, properties: PropertySerialization empty, binding: Nothing })

            (lift $ findRoleRequests (ContextInstance "def:AnyContext") (externalRoleType contextType)) >>= addCorrelationIdentifiersToTransactie
            addCreatedContextToTransaction id
          else pure unit
          pure id
      )
  else logPerspectivesError $ UnauthorizedForContext "auteur" subject contextType

-- | Retrieves from the repository the model that holds the RoleType, if necessary.
executeUniverseRoleDelta :: UniverseRoleDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeUniverseRoleDelta (UniverseRoleDelta { id, roleType, roleInstance, authorizedRole, deltaType, subject }) s = do
  padding <- lift transactionLevel
  readableRoleType <- lift $ toReadable roleType
  readableSubject <- lift $ toReadable subject
  log (padding <> show deltaType <> " for/from " <> show id <> " with id " <> show roleInstance <> " with type " <> show readableRoleType <> " for user role " <> show readableSubject)
  void $ lift $ retrieveDomeinFile (ModelUri $ unsafePartial typeUri2ModelUri_ $ unwrap roleType)
  case deltaType of
    ConstructEmptyRole -> do
      -- We have to check this case: a user is allowed to create himself.
      -- The role is cached, but not saved. However, when it is added to its context, it will be saved.
      if userCreatesThemselves then constructAnotherRole_
      else do
        -- Check if the author has a perspective on the role to be created that includes
        -- the verb Create.
        (lift $ roleHasPerspectiveOnRoleWithVerb subject roleType [ Verbs.Create, Verbs.CreateAndFill ] Nothing Nothing) >>= case _ of
          Left e -> handleError e
          Right _ -> constructAnotherRole_
    ConstructExternalRole -> do
      -- Notice that merely constructing a role has no consequences for the responsibilities
      -- QUERY UPDATES, RULE TRIGGERING, PERSISTENCE or CURRENTUSER.
      -- If roleType has the aspect model:System$RootContext$External, it can stand alone. These roles can
      -- be created by any user.
      -- PERSISTENCE is handled here.
      lift (roleType ###>> hasAspect (EnumeratedRoleType rootContext)) >>=
        if _ then void $ lookupOrCreateRoleInstance roleType Nothing constructExternalRole
        else (lift $ roleHasPerspectiveOnExternalRoleWithVerbs subject authorizedRole [ Verbs.CreateAndFill ] Nothing Nothing) >>= case _ of
          Left e -> handleError e
          Right _ -> void $ lookupOrCreateRoleInstance roleType Nothing constructExternalRole
    RemoveRoleInstance -> do
      -- We justify the Delete verb with the reasoning that the user is allowed to delete all role instances, he certainly is allowed to delete one.
      -- Similarly, if the user is allowed to remove a contextrole with its filler (requiring RemoveContext), he is allowed to remove the contctrole instance.
      (lift $ roleHasPerspectiveOnRoleWithVerb subject roleType [ Verbs.Remove, Verbs.Delete, Verbs.RemoveContext, Verbs.DeleteContext ] Nothing Nothing) >>= case _ of
        Left e -> handleError e
        Right _ -> void $ scheduleRoleRemoval synchronise roleInstance

    -- TODO Het lijkt niet nuttig om beide cases te behouden.
    RemoveUnboundExternalRoleInstance -> do
      (lift $ roleHasPerspectiveOnExternalRoleWithVerbs subject authorizedRole [ Verbs.DeleteContext, Verbs.RemoveContext ] Nothing Nothing) >>= case _ of
        Left e -> handleError e
        Right _ -> flip removeContextIfUnbound authorizedRole roleInstance
    RemoveExternalRoleInstance -> do
      (lift $ roleHasPerspectiveOnExternalRoleWithVerbs subject authorizedRole [ Verbs.DeleteContext, Verbs.RemoveContext ] Nothing Nothing) >>= case _ of
        Left e -> handleError e
        -- As external roles are always stored the same as their contexts, we can reliably retrieve the context instance id from the role id.
        Right _ -> scheduleContextRemoval authorizedRole [] (ContextInstance $ deconstructBuitenRol $ unwrap roleInstance)
  where
  userCreatesThemselves :: Boolean
  userCreatesThemselves = case subject of
    ENR r -> r == roleType
    _ -> false

  constructAnotherRole_ :: MonadPerspectivesTransaction Unit
  constructAnotherRole_ = do
    -- find the number of roleinstances in the context.
    offset <- (lift (id ##= getRoleInstances (ENR roleType))) >>= pure <<< getNextRolIndex
    void $ lookupOrCreateRoleInstance
      roleType
      (Just $ unwrap roleInstance)
      do
        (exists :: Maybe PerspectRol) <- lift $ tryGetPerspectEntiteit roleInstance
        -- Here we make constructing another role idempotent. Nothing happens if it already exists.
        if isNothing exists then void $ constructEmptyRole_ id offset roleInstance
        else pure unit
        pure roleInstance

  constructEmptyRole_ :: ContextInstance -> Int -> RoleInstance -> MonadPerspectivesTransaction Boolean
  constructEmptyRole_ contextInstance i rolInstanceId = do
    (exists :: Maybe PerspectRol) <- lift $ tryGetPerspectEntiteit rolInstanceId
    if isNothing exists then do
      allTypes <- lift (roleType ###= roleAspectsClosure)
      role <- pure
        ( PerspectRol defaultRolRecord
            { _id = takeGuid $ unwrap rolInstanceId
            , id = rolInstanceId
            , pspType = roleType
            , allTypes = allTypes
            , context = contextInstance
            , occurrence = i
            -- The contextDelta will be added when we handle the corresponding ContextDelta.
            , states = [ StateIdentifier $ unwrap roleType ]
            }
        )
      addCreatedRoleToTransaction rolInstanceId
      void $ lift $ cacheEntity rolInstanceId role
      pure true
    else pure false

  -- PERSISTENCE
  constructExternalRole :: MonadPerspectivesTransaction RoleInstance
  constructExternalRole = do
    padding <- lift transactionLevel
    log (padding <> "ConstructExternalRole in " <> show id)
    -- Here we make constructing an external role idempotent. Nothing happens if it already exists.
    constructEmptyRole_ id 0 roleInstance >>=
      if _ then lift $ void $ saveEntiteit roleInstance
      else pure unit
    pure roleInstance

-- | Execute all Deltas in a run that does not distribute.
-- executeTransaction :: TransactionForPeer -> MonadPerspectives Unit
-- executeTransaction t@(TransactionForPeer{deltas}) = void $ (runMonadPerspectivesTransaction'
--     false
--     (ENR $ EnumeratedRoleType sysUser)
--     (for_ deltas f))

-- TODO #29 Refactor delta decoding for performance
-- | Executes all deltas, which leads to a changed store of resources. 
-- | Does not change anything if some of the information could not be verified.
-- | The verified keys map is passed from verifyTransaction to executeTransaction'
-- | so that keys of authors not yet in the entity store can still be used for
-- | re-verification during delta execution.
executeTransaction :: TransactionForPeer -> MonadPerspectivesTransaction Unit
executeTransaction t = try (verifyTransaction t) >>= case _ of
  Left e -> logPerspectivesError (Custom $ "Could not execute a transaction. Reason: " <> show e)
  Right verifiedKeys -> executeTransaction' verifiedKeys t

  where

  verifyTransaction :: TransactionForPeer -> MonadPerspectivesTransaction (Map.Map PerspectivesUser CryptoKey)
  verifyTransaction (TransactionForPeer { deltas, publicKeys }) = do
    Tuple _ keysMap <- ST.runStateT
      ( do
          -- Verify public key informaton.
          -- Stop or throw if a key cannot be verified!
          forWithIndex_ (unwrap publicKeys) verifyPublicKey
          -- Verify all deltas, using the (now verified) public keys.
          -- Stop or throw if a delta cannot be verified!
          for_ deltas verifyDelta_
      )
      Map.empty
    pure keysMap

  -- Verify the deltas that make up the PerspectivesUser instance and his public key. 
  -- Add this author-key combination to state.
  -- For a author-key combination that is not yet known, will throw up one error on the console 
  -- as the PDR tries to find the author.
  verifyPublicKey :: PerspectivesUser -> PublicKeyInfo -> ST.StateT (Map.Map PerspectivesUser CryptoKey) MonadPerspectivesTransaction Unit
  verifyPublicKey authorOfKey { key, deltas: keyDeltas } = do
    mcryptoKey <- lift $ lift $ tryGetPublicKey authorOfKey
    case mcryptoKey of
      Nothing -> do
        cryptoKey <- liftAff $ deserializeJWK key
        void $ for keyDeltas \d@(SignedDelta { author }) ->
          if author == authorOfKey then (lift $ lift $ verifyDelta' d (Just cryptoKey)) >>=
            case _ of
              Nothing -> throwError (error $ "Cannot verify key of author: " <> show author)
              Just _ -> pure unit
          else do
            mcryptoKeyOfAuthor <- lift $ lift $ tryGetPublicKey author
            case mcryptoKeyOfAuthor of
              Nothing -> throwError (error $ "No public key known for this author: " <> show author)
              Just keyOfAuthor -> (lift $ lift $ verifyDelta' d (Just keyOfAuthor)) >>=
                case _ of
                  Nothing -> throwError (error $ "Cannot verify key of author: " <> show author)
                  Just _ -> void $ ST.modify \s -> Map.insert author keyOfAuthor s
        void $ ST.modify \s -> Map.insert authorOfKey cryptoKey s
      Just cryptoKey -> void $ ST.modify \s -> Map.insert authorOfKey cryptoKey s

  verifyDelta_ :: SignedDelta -> ST.StateT (Map.Map PerspectivesUser CryptoKey) MonadPerspectivesTransaction Unit
  verifyDelta_ d@(SignedDelta { author }) = do
    cryptoKey <- ST.gets (Map.lookup author)
    lift $ lift $ verifyDelta' d cryptoKey >>=
      case _ of
        Nothing -> throwError (error $ "Cannot verify delta allegedly by author: " <> show author)
        Just _ -> pure unit

executeTransaction' :: Map.Map PerspectivesUser CryptoKey -> TransactionForPeer -> MonadPerspectivesTransaction Unit
executeTransaction' verifiedKeys t@(TransactionForPeer { deltas, publicKeys }) = do

  -- Add all public key information (possibly leading to more TheWorld$PerspectivesUsers instances).
  for_ (unwrap publicKeys) \{ deltas: keyDeltas } -> void $ for keyDeltas \s@(SignedDelta { encryptedDelta }) -> executeDelta s (Just encryptedDelta)

  -- STEP 1: Verify and extract ordering info from all deltas.
  -- Use the keys collected during verifyTransaction to avoid re-querying the entity store
  -- for authors whose PerspectivesUsers instance may not yet have been fully persisted.
  verifiedDeltas <- catMaybes <$> for deltas \s@(SignedDelta { author }) -> do
    mStringified <- case Map.lookup author verifiedKeys of
      Just cryptoKey -> lift $ verifyDelta' s (Just cryptoKey)
      Nothing -> lift $ verifyDelta s
    pure $ case mStringified of
      Nothing -> Nothing
      Just stringified -> case extractOrderingInfo stringified of
        Nothing -> Just { signedDelta: s, stringified, resourceKey: "", resourceVersion: 0, author }
        Just { resourceKey, resourceVersion } -> Just { signedDelta: s, stringified, resourceKey, resourceVersion, author }

  -- STEP 2: Check for gaps. If any delta has resourceVersion > localVersion + 1, block the transaction.
  let
    deltaInfos = filter (\d -> d.resourceKey /= "")
      (map (\d -> { resourceKey: d.resourceKey, resourceVersion: d.resourceVersion, author: d.author }) verifiedDeltas)
  gaps <- checkForGaps deltaInfos
  case gaps of
    [] -> do
      -- No gaps: execute all deltas with version tracking.
      void $ for verifiedDeltas \{ signedDelta, stringified, resourceKey, resourceVersion, author } ->
        executeDeltaWithVersionTracking signedDelta stringified resourceKey resourceVersion author
    _ -> do
      -- Gaps detected: block the entire transaction.
      log "Transaction blocked: version gaps detected. Storing as pending."
      lift $ storePendingTransaction t gaps
  where

  executeDeltaWithVersionTracking :: SignedDelta -> String -> String -> Int -> PerspectivesUser -> MonadPerspectivesTransaction Unit
  executeDeltaWithVersionTracking s stringified resourceKey resourceVersion author = do
    -- Execute the delta (same logic as before).
    executeDelta s (Just stringified)
    -- After successful execution, update the resource version store and store delta in delta-store.
    if resourceKey /= "" then do
      -- Update the resource version to the delta's version (which should be localVersion + 1).
      lift $ setResourceVersion resourceKey resourceVersion
      -- Store the delta in the delta-store.
      lift $ storeDelta $ DeltaStoreRecord
        { _id: deltaStoreDocId resourceKey resourceVersion author
        , _rev: Nothing
        , resourceKey
        , resourceVersion
        , author
        , signedDelta: s
        , deltaType: "" -- TODO: extract deltaType string from the parsed delta
        , applied: true
        }
    else pure unit

  executeDelta :: SignedDelta -> Maybe String -> MonadPerspectivesTransaction Unit
  -- For now, we fail silently on deltas that cannot be authenticated.
  executeDelta s Nothing = pure unit
  executeDelta s (Just stringifiedDelta) = do
    padding <- lift transactionLevel
    storageSchemes <- lift $ gets _.typeToStorage
    catchError
      ( case runExcept $ readJSON' stringifiedDelta of
          Right d1 -> lift (addResourceSchemes storageSchemes d1) >>= flip executeRolePropertyDelta s
          Left _ -> case runExcept $ readJSON' stringifiedDelta of
            Right d2 -> lift (addResourceSchemes storageSchemes d2) >>= flip executeRoleBindingDelta s
            Left _ -> case runExcept $ readJSON' stringifiedDelta of
              Right d3 -> lift (addResourceSchemes storageSchemes d3) >>= flip executeContextDelta s
              Left _ -> case runExcept $ readJSON' stringifiedDelta of
                Right d4 -> lift ((addResourceSchemes storageSchemes d4)) >>= flip executeUniverseRoleDelta s
                Left _ -> case runExcept $ readJSON' stringifiedDelta of
                  Right d5 -> lift (addResourceSchemes storageSchemes d5) >>= flip executeUniverseContextDelta s
                  Left _ -> log (padding <> "Failing to parse and execute: " <> stringifiedDelta)
      )
      (\e -> liftEffect $ log (padding <> show e))

-- | All identifiers in deltas in a transaction have been stripped from their storage schemes, except for those with the pub: scheme.
-- | This function adds public resource schemes for the given storageUrl or, when a different publishing point is found for an identifier,
-- | that specific url.
-- | As an INVARIANT, all these (public) resources are de-cached, so we ensure that they are freshly retrieved from the database prior to executing 
-- | a delta on them. This is so we mitigate the risk of a peer having written the resource while we have an older version in cache.
-- | However, if the resource isn't present in the database, we don't delete it from the cache.
-- | All deltas in this transaction have either been sent to this installation and thus have been verified before, or
-- | they are created by this installation so verification is meaningless.
-- | We ignore a delta if its subject is public.
expandDeltas :: TransactionForPeer -> String -> MonadPerspectivesTransaction (Array Delta)
expandDeltas t@(TransactionForPeer { deltas, publicKeys }) storageUrl = do
  contentDeltas :: Array Delta <- catMaybes <$> (for deltas expandDelta)
  keyDeltas :: Array Delta <- concat <$> for (fromFoldable (ENCMAP.values publicKeys)) \{ deltas: kd } -> catMaybes <$> (for kd expandDelta)
  pure $ keyDeltas <> contentDeltas

  where

  expandDelta :: SignedDelta -> MonadPerspectivesTransaction (Maybe Delta)
  expandDelta s@(SignedDelta sr@{ author, encryptedDelta, signature }) = do
    -- Use the storageUrl to add a public scheme to the author.
    s' <- pure $ SignedDelta sr { author = over PerspectivesUser (createPublicIdentifier storageUrl) author }
    padding <- lift transactionLevel
    case runExcept $ readJSON' encryptedDelta of
      Right (d1 :: RolePropertyDelta) -> notWhenPublicSubject (unwrap d1) (lift $ (Just <<< RPD s' <$> addPublicResourceScheme storageUrl d1))
      Left _ -> case runExcept $ readJSON' encryptedDelta of
        Right (d2 :: RoleBindingDelta) -> notWhenPublicSubject (unwrap d2) (lift $ (Just <<< RBD s' <$> addPublicResourceScheme storageUrl d2))
        Left _ -> case runExcept $ readJSON' encryptedDelta of
          Right (d3 :: ContextDelta) -> notWhenPublicSubject (unwrap d3) (lift $ (Just <<< CDD s' <$> addPublicResourceScheme storageUrl d3))
          Left _ -> case runExcept $ readJSON' encryptedDelta of
            Right (d4 :: UniverseRoleDelta) -> notWhenPublicSubject (unwrap d4) (lift $ (Just <<< URD s' <$> addPublicResourceScheme storageUrl d4))
            Left _ -> case runExcept $ readJSON' encryptedDelta of
              Right (d5 :: UniverseContextDelta) -> notWhenPublicSubject (unwrap d5) (lift $ (Just <<< UCD s' <$> addPublicResourceScheme storageUrl d5))
              Left _ -> log (padding <> "Failing to parse and execute: " <> encryptedDelta) *> pure Nothing

    where
    notWhenPublicSubject :: forall f. DeltaRecord f -> MonadPerspectivesTransaction (Maybe Delta) -> MonadPerspectivesTransaction (Maybe Delta)
    notWhenPublicSubject rec a = (lift $ isPublic rec.subject) >>= if _ then pure Nothing else a

executeDeltas :: Array Delta -> MonadPerspectivesTransaction Unit
-- We use `for` rather than `for_` because the latter folds from the right, starting with the last element.
executeDeltas deltas = void $ for deltas case _ of
  UCD s d -> executeUniverseContextDelta d s
  URD s d -> executeUniverseRoleDelta d s
  CDD s d -> executeContextDelta d s
  RBD s d -> executeRoleBindingDelta d s
  RPD s d -> executeRolePropertyDelta d s

-----------------------------------------------------------
-- COLLECTING DELTAS
-----------------------------------------------------------
data Delta
  = UCD SignedDelta UniverseContextDelta
  | URD SignedDelta UniverseRoleDelta
  | CDD SignedDelta ContextDelta
  | RBD SignedDelta RoleBindingDelta
  | RPD SignedDelta RolePropertyDelta

derive instance Generic Delta _
derive instance Eq Delta
instance Ord Delta where
  compare (UCD _ u1) (UCD _ u2) = compare u1 u2
  compare (URD _ u1) (URD _ u2) = compare u1 u2
  compare (CDD _ u1) (CDD _ u2) = compare u1 u2
  compare (RBD _ u1) (RBD _ u2) = compare u1 u2
  compare (RPD _ u1) (RPD _ u2) = compare u1 u2
  compare _ _ = LT
