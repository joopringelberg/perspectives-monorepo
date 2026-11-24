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

import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.Except (lift, runExcept, runExceptT)
import Control.Monad.State (StateT, gets, modify, runStateT) as ST
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Array (any, catMaybes, concat, filter, filterA, foldl, fromFoldable, last, null, nub, sortBy)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing)
import Data.MediaType (MediaType(..))
import Data.Newtype (over, unwrap)
import Data.Ordering (Ordering(..))
import Data.String (Pattern(..), drop, indexOf, length, take) as Str
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
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
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addCreatedContextToTransaction, addCreatedRoleToTransaction, deltaTypeSortPriority)
import Perspectives.DependencyTracking.Dependency (findRoleRequests)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.Error.Pretty (renderPerspectivesError, renderPerspectivesWarning)
import Perspectives.Identifiers (buitenRol, deconstructBuitenRol, typeUri2LocalName_, typeUri2ModelUri_)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.Builders (lookupOrCreateContextInstance, lookupOrCreateRoleInstance, createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (bottom_, getProperty, roleType)
import Perspectives.Instances.Values (parsePerspectivesFile)
import Perspectives.Logging (traceSync, warnSync)
import Perspectives.ModelDependencies (rootContext)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (getAttachment)
import Perspectives.Persistence.DeltaStore (deltaStoreDocIdWithDeltaId, extractDeltaInfo, findDeltaByDeltaId, getDeltasForResource, getDeltasForRoleInstance, safeKey, storeDelta, storeDeltaFromSignedDelta, updateDeltaApplied)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistence.CausalStore (CausalRoleDoc(..), coveredByDeletion, ensureCausalEpoch, getCausalRole, observedByDeletion, recordCausalDelta)
import Perspectives.Persistence.QuarantineStore (quarantineTransaction)
import Perspectives.Persistence.PendingTransactionStore (MissingDelta, storePendingTransaction)
import Perspectives.Persistence.PropertyHistoryStore (storeIncomingDeltaWithHistory)
import Perspectives.Persistence.ResourceVersionStore (getResourceVersion, setResourceVersion)
import Perspectives.Performance (timed)
import Perspectives.Persistent (addAttachment, entityExists, forceSaveRole, getPerspectRol, saveEntiteit, saveEntiteit_, tryGetPerspectContext, tryGetPerspectEntiteit, tryGetPerspectRol)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..), cacheEntity)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ResourceType(..), RoleType(..), StateIdentifier(..), externalRoleType, roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerb(..)) as Verbs
import Perspectives.ResourceIdentifiers (createPublicIdentifier, isInPublicScheme, resourceIdentifier2DocLocator, resourceIdentifier2WriteDocLocator, takeGuid)
import Perspectives.Sync.CanonicalJson (computeDeltaId)
import Perspectives.Sync.Causal (causalRoleKey, currentCausalEpoch, validateCausalDelta)
import Perspectives.Sync.Causal (coversClock, stampClock)
import Perspectives.Sync.ConstructionBaseline (ConstructionBaseline, causallyBefore, inferConstructionBaselines, sameResource, sameSignedContent, selectContextConstruction, selectRoleConstruction)
import Perspectives.SaveUserData (removeBinding, removeReceivedContextIfUnbound, replaceBinding, scheduleReceivedContextRemoval, scheduleReceivedRoleRemoval, setFirstBinding)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.StrippedDelta (addPublicResourceScheme, addResourceSchemes, addSchemeToResourceIdentifier)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.PropertyHistory (checkpoint, conflictPrecedence, isCheckpoint, isPropertyOperation, propertyHistory, validateCheckpoint)
import Perspectives.Sync.Transaction (PublicKeyInfo, Transaction(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), deduplicatePublicKeys, withoutPredefinedContextScaffolding)
import Perspectives.Sync.VersionedDelta (DeltaEnvelope(..), parseIncomingDelta)
import Perspectives.Types.ObjectGetters (contextAspectsClosure, hasAspect, isPublic, roleAspectsClosure, publicUserRole)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), DeltaRecord, RoleBindingDelta(..), RoleBindingDeltaType(..), RolePropertyDelta(..), RolePropertyDeltaType(..), UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Perspectives.UnschemedIdentifiers (UnschemedResourceIdentifier, unschemePerspectivesUser)
import Perspectives.Warning (PerspectivesWarning(..))
import Prelude (class Eq, class Monad, class Ord, class Show, Unit, bind, compare, discard, flip, map, max, negate, not, pure, show, unit, void, when, ($), (*>), (+), (/=), (<), (<$>), (<<<), (<>), (==), (>), (>=), (>>=), (&&), (||), (>=>))
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
  lift $ renderPerspectivesWarning >=> traceSync $ (ExecutingContextDelta (show deltaType) (show contextInstance) (show roleInstance))
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
  lift $ renderPerspectivesWarning >=> traceSync $ (ExecutingRoleBindingDelta (show deltaType) (show filled) (show filler))
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
  let
    authorize = roleHasPerspectiveOnPropertyWithVerb (unwrap signedDelta).author subject id property
    reject e = do
      message <- lift $ renderPerspectivesError e
      throwError $ error message
  lift $ renderPerspectivesWarning >=> traceSync $ (ExecutingRolePropertyDelta deltaType id property)
  if isCheckpoint (unwrap signedDelta).encryptedDelta then do
    -- Checkpoints certify state as disclosed by a peer, not a new authorised write.
    -- Network entry verifies both the disclosure signature and original winner.
    case (unwrap signedDelta).signature of
      Nothing -> throwError (error "A peer property checkpoint must be signed")
      Just _ -> pure unit
    case validateCheckpoint signedDelta of
      Left message -> throwError $ error message
      Right _ -> pure unit
    case checkpoint (unwrap signedDelta).encryptedDelta of
      Just marker -> do
        verified <- lift $ verifyDelta marker.winner
        when (isNothing verified) $ throwError (error "Cannot verify checkpoint winner")
      Nothing -> throwError (error "Missing property checkpoint marker")
    setProperty [ id ] property (Just signedDelta) values
  else case deltaType of
    AddProperty -> do
      -- we need not check whether the model is known if we assume valid transactions:
      -- a role instance creation delta must have preceded the property delta.
      (lift $ authorize Verbs.AddPropertyValue) >>= case _ of
        -- This is mainly for historical reasons: we used to set properties by first removing and then adding them.
        -- This means that there are, in fact, in the Perspectives Universe property deltas with AddProperty that 
        -- have been made with a perspective of SetPropertyValue.
        Left _ -> (lift $ authorize Verbs.SetPropertyValue) >>= case _ of
          Left e1 -> reject e1
          Right _ -> addProperty [ id ] property (flip Tuple (Just signedDelta) <$> values)
        Right _ -> addProperty [ id ] property (flip Tuple (Just signedDelta) <$> values)
    RemoveProperty -> (lift $ authorize Verbs.RemovePropertyValue) >>= case _ of
      Left e -> reject e
      Right _ -> removeProperty [ id ] property (Just signedDelta) values
    DeleteProperty -> (lift $ authorize Verbs.DeleteProperty) >>= case _ of
      Left e -> reject e
      Right _ -> deleteProperty [ id ] property (Just signedDelta)
    SetProperty -> (lift $ authorize Verbs.SetPropertyValue) >>= case _ of
      Left e -> reject e
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
handleError e = lift $ renderPerspectivesError >=> warnSync $ e

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
checkForGaps = checkForGapsWith (lift <<< getResourceVersion)

checkForGapsWith :: forall m. Monad m => (String -> m Int) -> Array { resourceKey :: String, resourceVersion :: Int, author :: PerspectivesUser } -> m (Array MissingDelta)
checkForGapsWith lookupVersion deltaInfos = do
  let
    -- Storage addresses may change on publication; version histories use safeKey identity.
    resourceKeys = nub (map (safeKey <<< _.resourceKey) deltaInfos)
  gaps <- for resourceKeys \resourceKey -> do
    localVersion <- lookupVersion resourceKey
    let
      sortedInfos = sortBy (\d1 d2 -> compare d1.resourceVersion d2.resourceVersion)
        (filter (\d -> safeKey d.resourceKey == resourceKey) deltaInfos)
      result = foldl
        ( \acc d ->
            case acc.gap of
              Just _ -> acc
              Nothing ->
                if d.resourceVersion < acc.expectedVersion then acc
                else if d.resourceVersion == acc.expectedVersion then acc { expectedVersion = acc.expectedVersion + 1 }
                else
                  acc
                    { gap = Just
                        { author: d.author
                        , resourceKey: d.resourceKey
                        , expectedVersion: acc.expectedVersion
                        }
                    }
        )
        { expectedVersion: localVersion + 1, gap: Nothing }
        sortedInfos
    pure result.gap
  pure $ catMaybes gaps

-- | Retrieves from the repository the model that holds the ContextType, if necessary.
-- | The ConstructExternalRole always precedes the UniverseContextDelta for the context it is the external
-- | role for. Hence we only have to check whether the external role exists.
executeUniverseContextDelta :: UniverseContextDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeUniverseContextDelta (UniverseContextDelta { id, contextType, deltaType, subject }) signedDelta = do
  lift $ renderPerspectivesWarning >=> traceSync $ (ExecutingUniverseContextDelta deltaType id contextType)
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
  else lift $ renderPerspectivesError >=> warnSync $ UnauthorizedForContext "auteur" subject contextType

-- | Retrieves from the repository the model that holds the RoleType, if necessary.
executeUniverseRoleDelta :: UniverseRoleDelta -> SignedDelta -> MonadPerspectivesTransaction Unit
executeUniverseRoleDelta delta@(UniverseRoleDelta { id, roleType, roleInstance, authorizedRole, deltaType, subject }) s = do
  lift $ renderPerspectivesWarning >=> traceSync $ (ExecutingUniverseRoleDelta deltaType id roleInstance roleType subject)
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
      withRemovalPermission $ void $ scheduleReceivedRoleRemoval roleInstance

    -- TODO Het lijkt niet nuttig om beide cases te behouden.
    RemoveUnboundExternalRoleInstance -> do
      withRemovalPermission $ flip removeReceivedContextIfUnbound authorizedRole roleInstance
    RemoveExternalRoleInstance -> do
      withRemovalPermission $ scheduleReceivedContextRemoval authorizedRole (ContextInstance $ deconstructBuitenRol $ unwrap roleInstance)
    -- The author removed the context locally and thereby leaves it. A user who may remove a context locally may leave it;
    -- RemoveContext and DeleteContext imply RemoveContextLocally.
    -- She can only retract role instances that she fills herself.
    RetractRoleInstance -> do
      withRemovalPermission $ void $ scheduleReceivedRoleRemoval roleInstance
  where
  withRemovalPermission action = do
    result <- roleDeletionPermission delta s
    case result of
      Left e -> handleError e
      Right _ -> action

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
    lift $ renderPerspectivesWarning >=> traceSync $ (ConstructingExternalRole (show id))
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

roleDeletionPermission :: UniverseRoleDelta -> SignedDelta -> MonadPerspectivesTransaction (Either PerspectivesError Unit)
roleDeletionPermission (UniverseRoleDelta { roleType, roleInstance, authorizedRole, deltaType, subject }) (SignedDelta { author }) =
  case deltaType of
    RemoveRoleInstance ->
      void <$> lift (roleHasPerspectiveOnRoleWithVerb subject roleType [ Verbs.Remove, Verbs.Delete, Verbs.RemoveContext, Verbs.DeleteContext ] Nothing Nothing)
    RemoveUnboundExternalRoleInstance -> externalPermission
    RemoveExternalRoleInstance -> externalPermission
    RetractRoleInstance -> do
      result <- lift $ roleHasPerspectiveOnExternalRoleWithVerbs subject authorizedRole [ Verbs.RemoveContextLocally, Verbs.RemoveContext, Verbs.DeleteContext ] Nothing Nothing
      case result of
        Left e -> pure $ Left e
        Right _ -> do
          filler <- lift $ bottom_ roleInstance
          pure $
            if takeGuid (unwrap filler) == takeGuid (unwrap author) then Right unit
            else Left $ Custom ("A user can only retract role instances that she fills herself. " <> show author <> " tried to retract " <> show roleInstance <> ".")
    _ -> pure $ Left $ Custom "Expected a role deletion"
  where
  externalPermission = void <$> lift (roleHasPerspectiveOnExternalRoleWithVerbs subject authorizedRole [ Verbs.DeleteContext, Verbs.RemoveContext ] Nothing Nothing)

-- TODO #29 Refactor delta decoding for performance
-- | Executes all deltas, which leads to a changed store of resources. 
-- | Does not change anything if some of the information could not be verified.
-- | The verified keys map is passed from verifyTransaction to executeTransaction'
-- | so that keys of authors not yet in the entity store can still be used for
-- | re-verification during delta execution.
executeTransaction :: TransactionForPeer -> MonadPerspectivesTransaction Unit
executeTransaction = void <<< executeTransactionWithOutcome

-- | The result of handling an incoming transaction.
-- | * Executed: all deltas were handled (applied, recognised as duplicates, outdated or conflicting).
-- | * Blocked: version gaps were found; the transaction has been stored in the PendingTransactionStore.
-- | * Quarantined: the transaction is outside the active causal epoch and has been quarantined.
-- | * Rejected: the transaction could not be verified; nothing has been changed.
-- | Exceptions during execution are not an outcome: they propagate (see Perspectives.Sync.PendingTransactions).
data TransactionOutcome = Executed | Blocked (Array MissingDelta) | Quarantined | Rejected String

instance Show TransactionOutcome where
  show Executed = "Executed"
  show (Blocked gaps) = "Blocked " <> show gaps
  show Quarantined = "Quarantined"
  show (Rejected reason) = "Rejected " <> reason

executeTransactionWithOutcome :: TransactionForPeer -> MonadPerspectivesTransaction TransactionOutcome
executeTransactionWithOutcome t = do
  void $ lift ensureCausalEpoch
  if (unwrap t).causalEpoch /= Just currentCausalEpoch then do
    id <- lift $ quarantineTransaction "Transaction is outside the active causal epoch" t
    lift $ warnSync $ "Quarantined transaction " <> id <> ": missing or unsupported causal epoch; manual reconciliation required"
    pure Quarantined
  else executeCurrentTransaction t

executeCurrentTransaction :: TransactionForPeer -> MonadPerspectivesTransaction TransactionOutcome
executeCurrentTransaction t = do
  transactionNumber <- gets \(Transaction tr) -> tr.transactionNumber
  -- Prevent addDelta/insertDelta from storing deltas in the DeltaStore.
  -- executeDeltaWithVersionTracking handles DeltaStore persistence directly;
  -- the update functions called via executeDelta must not also store via addDelta.
  modify (over Transaction \tr -> tr { isExecutingIncomingDeltas = true })
  let filtered = withoutPredefinedContextScaffolding t
  try
    ( timed "incoming.verify" (show transactionNumber) do
        publicKeys <- liftEffect $ deduplicatePublicKeys (unwrap filtered).publicKeys
        let incoming = over TransactionForPeer (\r -> r { publicKeys = publicKeys }) filtered
        verifiedKeys <- verifyTransaction incoming
        pure $ Tuple verifiedKeys incoming
    ) >>= case _ of
    Left e -> do
      lift $ renderPerspectivesError >=> warnSync $ (IncomingTransactionFailed (show e))
      pure $ Rejected (show e)
    Right (Tuple verifiedKeys incoming) -> timed "incoming.execute" (show transactionNumber) (executeTransaction' verifiedKeys t incoming)

verifyTransaction :: TransactionForPeer -> MonadPerspectivesTransaction (Map.Map UnschemedResourceIdentifier CryptoKey)
verifyTransaction (TransactionForPeer { deltas, publicKeys }) = do
  Tuple _ keysMap <- ST.runStateT
    ( do
        -- Verify public key informaton.
        -- Stop or throw if a key cannot be verified!
        forWithIndex_ (unwrap publicKeys) verifyPublicKey
        for_ (unwrap publicKeys) \info -> for_ info.deltas verifyDelta_
        -- Verify all deltas, using the (now verified) public keys.
        -- Stop or throw if a delta cannot be verified!
        for_ deltas verifyDelta_
    )
    Map.empty
  pure keysMap

  where

  -- Verify the deltas that make up the PerspectivesUser instance and his public key. 
  -- Add this author-key combination to state.
  -- For a author-key combination that is not yet known, will throw up one error on the console 
  -- as the PDR tries to find the author.
  verifyPublicKey :: PerspectivesUser -> PublicKeyInfo -> ST.StateT (Map.Map UnschemedResourceIdentifier CryptoKey) MonadPerspectivesTransaction Unit
  verifyPublicKey authorOfKey { key, deltas: keyDeltas } = do
    mcryptoKey <- lift $ lift do
      localKey <- tryGetPublicKey $ over PerspectivesUser takeGuid authorOfKey
      case localKey of
        Just _ -> pure localKey
        Nothing ->
          if isInPublicScheme (unwrap authorOfKey) then tryGetPublicKey authorOfKey
          else pure Nothing
    case mcryptoKey of
      Nothing -> do
        cryptoKey <- liftAff $ deserializeJWK key
        void $ for keyDeltas \d@(SignedDelta { author }) ->
          if unschemePerspectivesUser author == unschemePerspectivesUser authorOfKey then (lift $ lift $ verifyDelta' d (Just cryptoKey)) >>=
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
                  Just _ -> void $ ST.modify \s -> Map.insert (unschemePerspectivesUser author) keyOfAuthor s
        void $ ST.modify \s -> Map.insert (unschemePerspectivesUser authorOfKey) cryptoKey s
      Just cryptoKey -> void $ ST.modify \s -> Map.insert (unschemePerspectivesUser authorOfKey) cryptoKey s

  verifyDelta_ :: SignedDelta -> ST.StateT (Map.Map UnschemedResourceIdentifier CryptoKey) MonadPerspectivesTransaction Unit
  verifyDelta_ d@(SignedDelta { author }) = do
    cryptoKey <- ST.gets (Map.lookup $ unschemePerspectivesUser author)
    (lift $ lift $ verifyDelta' d cryptoKey) >>=
      case _ of
        Nothing -> throwError (error $ "Cannot verify delta allegedly by author: " <> show author)
        Just _ -> do
          case parseIncomingDelta (unwrap d).encryptedDelta of
            Left message -> throwError $ error $ "Invalid incoming delta: " <> message
            Right _ -> pure unit
          case validateCausalDelta d of
            Left message -> throwError $ error message
            Right _ -> pure unit
          case checkpoint (unwrap d).encryptedDelta of
            Nothing -> pure unit
            Just marker -> do
              case (unwrap d).signature of
                Nothing -> throwError (error "A peer property checkpoint must be signed")
                Just _ -> pure unit
              case validateCheckpoint d of
                Left message -> throwError $ error message
                Right _ -> verifyDelta_ marker.winner

validatedConstructionBaselines :: Array SignedDelta -> MonadPerspectivesTransaction (Map.Map String ConstructionBaseline)
validatedConstructionBaselines history = do
  inferred <- case inferConstructionBaselines history of
    Left message -> throwError $ error message
    Right baselines -> pure baselines
  validated <- for inferred \baseline -> case baseline.causal of
    Nothing -> pure Nothing
    Just causal -> do
      let UniverseRoleDelta creation = baseline.role
      storageSchemes <- lift $ gets _.typeToStorage
      UniverseRoleDelta expanded <- lift $ addResourceSchemes storageSchemes baseline.role
      existing <- lift $ tryGetPerspectRol expanded.roleInstance
      localVersion <- lift $ getResourceVersion (unwrap creation.roleInstance)
      CausalRoleDoc known <- lift $ getCausalRole (unwrap creation.roleInstance)
      let
        newerThanDeletions = not $ any (\deletion -> not $ coversClock causal.context (stampClock deletion)) known.deletions
        compatible = case existing of
          Nothing -> newerThanDeletions && (localVersion == 0 || baseline.version > localVersion)
          Just (PerspectRol role) ->
            role.pspType == expanded.roleType && sameResource (unwrap role.context) (unwrap expanded.id)
        complete = baseline.version > 0
      if not (compatible && complete) then pure Nothing
      else do
        permission <-
          if creation.deltaType == ConstructExternalRole then do
            root <- lift $ creation.roleType ###>> hasAspect (EnumeratedRoleType rootContext)
            if root then pure $ Right unit
            else map (map (constUnit)) $ lift $
              roleHasPerspectiveOnExternalRoleWithVerbs creation.subject creation.authorizedRole [ Verbs.CreateAndFill ] Nothing Nothing
          else if creation.subject == ENR creation.roleType then pure $ Right unit
          else map (map (constUnit)) $ lift $
            roleHasPerspectiveOnRoleWithVerb creation.subject creation.roleType [ Verbs.Create, Verbs.CreateAndFill ] Nothing Nothing
        requirePermission permission
        case parseIncomingDelta (unwrap baseline.membership).encryptedDelta of
          Right (ContextEnvelope (ContextDelta membership)) | creation.deltaType == ConstructEmptyRole -> do
            membershipPermission <- lift $
              roleHasPerspectiveOnRoleWithVerb membership.subject membership.roleType [ Verbs.Create, Verbs.CreateAndFill ] Nothing Nothing
            requirePermission (map constUnit membershipPermission)
          _ -> pure unit
        pure $ Just $ Tuple (safeKey $ unwrap creation.roleInstance) baseline
  let candidates = Map.fromFoldable $ catMaybes validated
  complete <- filterA
    ( \(Tuple _ baseline) -> do
        storageSchemes <- lift $ gets _.typeToStorage
        UniverseRoleDelta creation <- lift $ addResourceSchemes storageSchemes baseline.role
        context <- lift $ tryGetPerspectContext creation.id
        case context of
          Just _ -> pure true
          Nothing -> case Map.lookup (safeKey $ buitenRol $ unwrap creation.id) candidates of
            Nothing -> pure false
            Just external -> case selectContextConstruction external history of
              Left message -> throwError $ error message
              Right constructor -> pure $ isJust constructor
    )
    (catMaybes validated)
  pure $ Map.fromFoldable complete
  where
  constUnit _ = unit
  requirePermission = case _ of
    Left failure -> do
      message <- lift $ renderPerspectivesError failure
      throwError $ error message
    Right _ -> pure unit

-- | `original` is the transaction as received; it is what a pending record stores, so that a
-- | blocked and a failed attempt of the same transaction share one pending record.
executeTransaction' :: Map.Map UnschemedResourceIdentifier CryptoKey -> TransactionForPeer -> TransactionForPeer -> MonadPerspectivesTransaction TransactionOutcome
executeTransaction' verifiedKeys original t@(TransactionForPeer { deltas, publicKeys }) = do

  -- Add all public key information (possibly leading to more TheWorld$PerspectivesUsers instances).
  -- Key deltas are executed directly (not through executeDeltaWithVersionTracking),
  -- so persist them explicitly in the DeltaStore first.
  for_ (unwrap publicKeys) \{ deltas: keyDeltas } -> void $ for keyDeltas \s@(SignedDelta { encryptedDelta }) -> do
    lift $ storeDeltaFromSignedDelta s
    executeDelta s (Just encryptedDelta)
    case extractDeltaInfo encryptedDelta of
      Just info | not $ isDeletionDeltaType info.deltaType -> lift $ recordCausalDelta s
      _ -> pure unit

  -- STEP 1: Verify and extract ordering info from all deltas.
  -- Use the keys collected during verifyTransaction to avoid re-querying the entity store
  -- for authors whose PerspectivesUsers instance may not yet have been fully persisted.
  verifiedDeltas <- catMaybes <$> for deltas \s@(SignedDelta { author }) -> do
    mStringified <- case Map.lookup (unschemePerspectivesUser author) verifiedKeys of
      Just cryptoKey -> lift $ verifyDelta' s (Just cryptoKey)
      Nothing -> lift $ verifyDelta s
    pure $ case mStringified of
      Nothing -> Nothing

      Just stringified -> case extractOrderingInfo stringified of
        Just { resourceKey, resourceVersion } -> Just { signedDelta: s, stringified, resourceKey, resourceVersion, author }
        Nothing -> Nothing

  baselines <- validatedConstructionBaselines (map _.signedDelta verifiedDeltas)
  liveDeltas <- filterA (\d -> not <$> coveredWhileAbsent d.signedDelta d.resourceKey) verifiedDeltas

  -- STEP 2: Check for gaps. If any delta has resourceVersion > localVersion + 1, block the transaction.
  let
    deltaInfos = filter (\d -> d.resourceKey /= "")
      (map (\d -> { resourceKey: d.resourceKey, resourceVersion: d.resourceVersion, author: d.author }) liveDeltas)
  let
    checkpointVersions = foldl
      ( \versions d ->
          if isCheckpoint d.stringified then Map.insertWith max (safeKey d.resourceKey) d.resourceVersion versions
          else versions
      )
      Map.empty
      verifiedDeltas
  gaps <- checkForGapsWith
    ( \key -> do
        local <- lift $ getResourceVersion key
        let
          baselineVersion = case Map.lookup (safeKey key) baselines of
            Just baseline -> baseline.version
            Nothing -> 0
        pure $ max baselineVersion $ max local $ case Map.lookup (safeKey key) checkpointVersions of
          Just version -> version
          Nothing -> 0
    )
    deltaInfos
  case gaps of
    [] -> do
      -- No gaps: execute all deltas with version tracking.
      void $ for (sortBy compareForReplay verifiedDeltas) \{ signedDelta, stringified, resourceKey, resourceVersion, author } ->
        executeDeltaWithVersionTracking baselines signedDelta stringified resourceKey resourceVersion author
      pure Executed
    _ -> do
      -- Gaps detected: block the entire transaction.
      lift $ renderPerspectivesWarning >=> traceSync $ TransactionBlockedByVersionGaps
      lift $ traceSync $ "Missing peer delta versions: " <> show gaps
      lift $ storePendingTransaction original gaps
      pure $ Blocked gaps
  where

  compareForReplay left right =
    let
      priority d = case extractDeltaInfo d.stringified of
        Just info -> deltaTypeSortPriority info.deltaType
        Nothing -> 99
      propertyOrder =
        if priority left == 5 && priority right == 5 then
          compare (safeKey left.resourceKey) (safeKey right.resourceKey)
            <> compare (not $ isCheckpoint left.stringified) (not $ isCheckpoint right.stringified)
            <> compare left.resourceVersion right.resourceVersion
        else EQ
    in
      compare (priority left) (priority right) <> propertyOrder

  executeDeltaWithVersionTracking :: Map.Map String ConstructionBaseline -> SignedDelta -> String -> String -> Int -> PerspectivesUser -> MonadPerspectivesTransaction Unit
  executeDeltaWithVersionTracking baselines s stringified resourceKey resourceVersion author = do
    deltaId <- liftAff $ computeDeltaId (unwrap author) stringified
    mDuplicate <- lift $ findDeltaByDeltaId deltaId
    -- Extract the deltaType and contextKey from the stringified delta content.
    let
      mInfo = extractDeltaInfo stringified
      deltaType = case mInfo of
        Just info -> info.deltaType
        Nothing -> ""
      contextKey = case mInfo of
        Just info -> map safeKey info.contextInstance
        Nothing -> Nothing
      persistIncomingDelta applied disposition = do
        when (isPropertyOperation deltaType) $ lift $ recordCausalDelta s
        lift $ storeIncomingDeltaWithHistory $ DeltaStoreRecord
          { _id: deltaStoreDocIdWithDeltaId resourceKey resourceVersion author deltaId
          , _rev: Nothing
          , resourceKey
          , resourceVersion
          , author
          , deltaId: Just deltaId
          , signedDelta: s
          , deltaType
          , applied
          , disposition: Just disposition
          , contextKey
          }
      applyDeletion = do
        storageSchemes <- lift $ gets _.typeToStorage
        case parseIncomingDelta stringified of
          Right (UniverseRoleEnvelope deletion) -> do
            expanded <- lift $ addResourceSchemes storageSchemes deletion
            permitted <- roleDeletionPermission expanded s
            case permitted of
              Left e -> do
                message <- lift $ renderPerspectivesError e
                throwError $ error message
              Right _ -> pure unit
          _ -> throwError $ error "Expected a role deletion payload"
        lift $ recordCausalDelta s
        obsolete <- supersededByConstruction s resourceKey
        suppressed <-
          if obsolete then pure true
          else if deltaType == "RetractRoleInstance" then pure false
          else isDeletionSuppressedByModifyWins s resourceKey
        if suppressed then do
          lift $ renderPerspectivesWarning >=> traceSync $ ModifyWinsOverDeleteSuppressed resourceKey
          persistIncomingDelta false (if obsolete then "SupersededByConstruction" else "ModifyWinsSuppressedDelete")
          pure false
        else do
          executeDelta s (Just stringified)
          persistIncomingDelta true "Applied"
          pure true
      baseline = Map.lookup (safeKey resourceKey) baselines
      baselineRecord = case baseline of
        Just b -> sameSignedContent s b.construction || sameSignedContent s b.membership
        Nothing -> false
      contextBaseline = Map.lookup (safeKey $ buitenRol resourceKey) baselines
      contextBaselineRecord = case contextBaseline of
        Just b -> case selectContextConstruction b [ s ] of
          Right (Just _) -> true
          _ -> false
        Nothing -> false
      applyBaseline b = do
        lift $ recordCausalDelta s
        executeDelta s (Just stringified)
        local <- lift $ getResourceVersion resourceKey
        lift $ setResourceVersion resourceKey (max local b.version)
        persistIncomingDelta true "ConstructionBaseline"
      -- A property delta below the local version may be the winner of a conflict at its
      -- own version that arrived late (I-4), or fill a version this installation never
      -- received. When it would win there, the property is re-derived from its full
      -- version-ordered history, so every installation ends with the same fold of winners.
      reconsiderLateProperty localVersion = do
        existing <- lift $ getDeltasForResource resourceKey
        let
          floor = foldl
            ( \v (DeltaStoreRecord r) ->
                if isCheckpoint (unwrap r.signedDelta).encryptedDelta then max v r.resourceVersion else v
            )
            0
            existing
          sameVersionDeltas = filter (\(DeltaStoreRecord r) -> r.resourceVersion == resourceVersion) existing
        if resourceVersion >= floor && not (existingDeltaWins (SignedDelta (unwrap s) { author = author }) sameVersionDeltas) then do
          lift $ renderPerspectivesWarning >=> traceSync $ (LateWinnerRederivingProperty resourceKey deltaType resourceVersion localVersion)
          for_ sameVersionDeltas \(DeltaStoreRecord old) ->
            when (not $ isCheckpoint (unwrap old.signedDelta).encryptedDelta)
              $ lift
              $ updateDeltaApplied old._id false
          persistIncomingDelta true "LateWinner"
          history <- lift $ getDeltasForResource resourceKey
          restorePropertyFromHistory history
        else do
          lift $ renderPerspectivesWarning >=> traceSync $ (SkippingOutdatedDelta resourceKey deltaType resourceVersion localVersion)
          persistIncomingDelta false "Outdated"
    case baseline, baselineRecord, contextBaselineRecord, mDuplicate of
      Just b, true, _, _ -> applyBaseline b
      _, _, true, _ -> do
        executeDelta s (Just stringified)
        persistIncomingDelta true "ConstructionBaseline"
      _, _, _, Just _ -> pure unit
      _, _, _, Nothing | resourceKey /= "" -> do
        when (not (isDeletionDeltaType deltaType) && not (isPropertyOperation deltaType)) $ lift $ recordCausalDelta s
        localVersion <- lift $ getResourceVersion resourceKey
        historical <- coveredWhileAbsent s resourceKey
        if historical then do
          -- Keep evidence available for a later concurrent modification's restoration.
          lift $ setResourceVersion resourceKey (max localVersion resourceVersion)
          persistIncomingDelta true "CoveredByDeletion"
        else if isCheckpoint stringified then do
          -- Never overwrite a peer's equally new or newer state with a baseline.
          when (resourceVersion > localVersion) do
            executeDelta s (Just stringified)
            lift $ setResourceVersion resourceKey resourceVersion
            persistIncomingDelta true "Checkpoint"
        else if resourceVersion < localVersion then
          if isPropertyOperation deltaType then reconsiderLateProperty localVersion
          else do
            -- Outdated delta: version is behind local version. Store but don't execute.
            lift $ renderPerspectivesWarning >=> traceSync $ (SkippingOutdatedDelta resourceKey deltaType resourceVersion localVersion)
            persistIncomingDelta false "Outdated"
        else if resourceVersion == localVersion then do
          -- Check for existing deltas at this version to distinguish a fresh creation from a genuine conflict.
          existingDeltas <- lift $ getDeltasForResource resourceKey
          let sameVersionDeltas = filter (\(DeltaStoreRecord r) -> r.resourceVersion == resourceVersion) existingDeltas
          if resourceVersion == 0 && null sameVersionDeltas then do
            -- Fresh resource creation: version 0 with no prior deltas for this key.
            -- getResourceVersion returns 0 both for untracked resources and for resources
            -- at version 0, so when there are no existing deltas this is the first mutation,
            -- not a write-write conflict. Execute directly.
            executeDelta s (Just stringified)
            persistIncomingDelta true "Applied"
          else do
            -- Genuine version conflict: two deltas claim the same version from different authors.
            -- Resolve deterministically by lexicographic comparison of author IDs:
            -- the author with the highest ID wins on all installations.
            -- Check whether any already-stored delta at this version has an author >= the incoming author.
            let incomingAuthorWins = not (existingDeltaWins (SignedDelta (unwrap s) { author = author }) sameVersionDeltas)
            if incomingAuthorWins then do
              -- Incoming author wins: execute the delta (overwriting the current value).
              lift $ renderPerspectivesWarning >=> traceSync $ (VersionConflictIncomingWins resourceKey deltaType resourceVersion (show author))
              applied <-
                if isDeletionDeltaType deltaType then applyDeletion
                else do
                  executeDelta s (Just stringified)
                  persistIncomingDelta true "Applied"
                  pure true
              when applied do
                for_ sameVersionDeltas \(DeltaStoreRecord old) ->
                  when (not $ isCheckpoint (unwrap old.signedDelta).encryptedDelta)
                    $ lift
                    $ updateDeltaApplied old._id false
                when (isPropertyOperation deltaType) do
                  history <- lift $ getDeltasForResource resourceKey
                  restorePropertyFromHistory history
            else do
              -- Existing author wins: store but don't execute.
              lift $ renderPerspectivesWarning >=> traceSync $ (VersionConflictIncomingLoses resourceKey deltaType resourceVersion (show author))
              persistIncomingDelta false "ConflictLoser"
        else do
          -- resourceVersion > localVersion: normal next expected version.
          -- (Gaps have already been ruled out by checkForGaps.)
          -- Apply modify-wins-over-delete logic before executing.
          -- A retraction is not suppressed: leaving a context wins over concurrent modifications.
          if isDeletionDeltaType deltaType then do
            -- Incoming is a role deletion. Check for concurrent local modifications on sub-resources.
            void applyDeletion
            lift $ setResourceVersion resourceKey resourceVersion
          else if isSubResourceKey resourceKey then do
            -- Incoming is a sub-resource modification (property or binding).
            -- Check whether the role instance was deleted and needs restoration.
            -- Extract the role instance ID from the sub-resource key.
            -- Resource keys have the form: roleInstanceId <> "#" <> subResourceId,
            -- and roleInstanceId itself contains one "#" (the scheme separator, e.g. "def:#id").
            -- So we must find the SECOND "#" to locate the role/sub-resource boundary.
            let roleInstanceId = extractRoleInstanceId resourceKey
            mRole <- lift $ tryGetPerspectRol (RoleInstance roleInstanceId)
            case mRole of
              Nothing -> do
                -- Role does not exist. Use the resource version as a proxy for deletion:
                -- if localVersion > 0, the role existed at some point and was likely deleted.
                -- This is more robust than relying on the deletion delta being in the DeltaStore,
                -- because the creation deltas may have arrived in the same transaction as the
                -- modification and been stored with applied=false (outdated path), preventing
                -- a pure delta-store based check from working.
                localRoleVersion <- lift $ getResourceVersion roleInstanceId
                if localRoleVersion > 0 then do
                  -- Modify wins over delete: restore the role from the delta-store.
                  lift $ renderPerspectivesWarning >=> traceSync $ (ModifyWinsOverDeleteRestoring roleInstanceId)
                  restoreRoleFromDeltaStore roleInstanceId
                else pure unit
                -- Execute the modification (role should now exist if restored).
                executeDelta s (Just stringified)
                lift $ setResourceVersion resourceKey resourceVersion
                persistIncomingDelta true "Applied"
              Just _ -> do
                -- Role exists: execute normally.
                executeDelta s (Just stringified)
                lift $ setResourceVersion resourceKey resourceVersion
                persistIncomingDelta true "Applied"
          else do
            -- Role-level delta that is not a deletion (e.g. ConstructEmptyRole, AddRoleInstancesToContext).
            executeDelta s (Just stringified)
            lift $ setResourceVersion resourceKey resourceVersion
            persistIncomingDelta true "Applied"
      _, _, _, Nothing -> pure unit

  -- | Returns true if a deltaType string represents a role-instance deletion.
  isDeletionDeltaType :: String -> Boolean
  isDeletionDeltaType dt =
    dt
      == "RemoveRoleInstance"
      || dt
        == "RemoveExternalRoleInstance"
      || dt
        == "RemoveUnboundExternalRoleInstance"
      || dt
        == "RetractRoleInstance"

  -- | Returns true if an incoming deletion delta for `roleInstanceId` (a role instance ID)
  -- | should be suppressed because there are locally-applied modification deltas on
  -- | sub-resources (properties or bindings) of that role — modify-wins-over-delete.
  isDeletionSuppressedByModifyWins :: SignedDelta -> String -> MonadPerspectivesTransaction Boolean
  isDeletionSuppressedByModifyWins deletion roleInstanceId = do
    allRoleDeltas <- lift $ getDeltasForRoleInstance roleInstanceId
    concurrent <- filterA (\(DeltaStoreRecord r) -> not <$> lift (observedByDeletion deletion r.signedDelta))
      $ filter (\d@(DeltaStoreRecord r) -> r.applied && isSubResourceDeltaOf roleInstanceId d) allRoleDeltas
    pure $ not $ null concurrent

  supersededByConstruction :: SignedDelta -> String -> MonadPerspectivesTransaction Boolean
  supersededByConstruction deletion roleKey = case parseIncomingDelta (unwrap deletion).encryptedDelta of
    Right (UniverseRoleEnvelope delta) -> do
      storageSchemes <- lift $ gets _.typeToStorage
      UniverseRoleDelta expanded <- lift $ addResourceSchemes storageSchemes delta
      existing <- lift $ tryGetPerspectRol expanded.roleInstance
      case existing of
        Nothing -> pure false
        Just (PerspectRol role) -> do
          records <- lift $ getDeltasForResource roleKey
          case selectRoleConstruction expanded.roleInstance role.pspType role.context (map (\(DeltaStoreRecord r) -> r.signedDelta) records) of
            Left message -> throwError $ error message
            Right Nothing -> pure false
            Right (Just construction) -> case causallyBefore deletion construction of
              Left message -> throwError $ error message
              Right before -> pure before
    _ -> pure false

  coveredWhileAbsent signed key =
    if not $ isSubResourceKey key then pure false
    else do
      role <- lift $ tryGetPerspectRol (RoleInstance $ extractRoleInstanceId key)
      if isNothing role then lift $ coveredByDeletion signed else pure false

  -- | Restore a deleted role by re-applying its creation and property/binding deltas
  -- | from the delta-store, then marking the deletion delta(s) as not applied.
  -- | Role-level deltas (ConstructEmptyRole, AddRoleInstancesToContext) are applied
  -- | before sub-resource deltas (properties, bindings) to preserve correct order.
  -- |
  -- | Role-level deltas are included regardless of their `applied` flag because:
  -- |   1. Role creation (ConstructEmptyRole, AddRoleInstancesToContext) is idempotent.
  -- |   2. When the creation deltas arrive in the SAME transaction as the modification
  -- |      that triggers restoration, they are stored with applied=false (they were
  -- |      outdated relative to the local deletion version). Filtering by applied=true
  -- |      would exclude them and leave the role unrestored.
  -- |
  -- | Sub-resource deltas (properties, bindings) are included only when applied=true
  -- | to avoid re-applying modifications that have already been superseded.
  restoreRoleFromDeltaStore :: String -> MonadPerspectivesTransaction Unit
  restoreRoleFromDeltaStore roleInstanceId = do
    allDeltas <- lift $ getDeltasForRoleInstance roleInstanceId
    let compareByVersion (DeltaStoreRecord r1) (DeltaStoreRecord r2) = compare r1.resourceVersion r2.resourceVersion
    -- Mark all applied deletion deltas as no longer applied.
    let deletionDeltas = filter (\(DeltaStoreRecord r) -> r.applied && isDeletionDeltaType r.deltaType) allDeltas
    for_ deletionDeltas \(DeltaStoreRecord { _id }) ->
      lift $ updateDeltaApplied _id false
    -- Role-level deltas (resourceKey == roleInstanceId): include ALL non-deletion ones
    -- regardless of applied status (role creation is idempotent).
    let
      roleLevelDeltas = sortBy compareByVersion $
        filter (\d@(DeltaStoreRecord r) -> not (isDeletionDeltaType r.deltaType) && not (isSubResourceDeltaOf roleInstanceId d)) allDeltas
    -- Sub-resource deltas (resourceKey starts with roleInstanceId <> "#"): only applied=true
    -- to avoid re-applying outdated or suppressed property/binding modifications.
    let
      subResourceDeltas = sortBy compareByVersion $
        filter (\d@(DeltaStoreRecord r) -> r.applied && not (isDeletionDeltaType r.deltaType) && isSubResourceDeltaOf roleInstanceId d) allDeltas
    for_ roleLevelDeltas \(DeltaStoreRecord { signedDelta: sd }) ->
      executeDelta sd (Just (unwrap sd).encryptedDelta)
    for_ (filter (\(DeltaStoreRecord r) -> not $ isPropertyOperation r.deltaType) subResourceDeltas) \(DeltaStoreRecord { signedDelta: sd }) ->
      executeDelta sd (Just (unwrap sd).encryptedDelta)
    restorePropertyHistories subResourceDeltas

  -- | Returns true if any DeltaStoreRecord in the array takes precedence over (or equals)
  -- | the incoming delta according to `conflictPrecedence` (author first, signed content second).
  -- | A checkpoint competes with the original winner it certifies.
  existingDeltaWins :: SignedDelta -> Array DeltaStoreRecord -> Boolean
  existingDeltaWins incoming records = any
    ( \(DeltaStoreRecord r) ->
        let
          winner = case checkpoint (unwrap r.signedDelta).encryptedDelta of
            Just marker -> marker.winner
            Nothing -> SignedDelta (unwrap r.signedDelta) { author = r.author }
        in
          conflictPrecedence winner incoming /= LT
    )
    records

  -- | Check whether a resource key is a sub-resource key (i.e. it has a second "#").
  -- | Resource IDs have the form "scheme:#identifier" (one "#" from the scheme separator).
  -- | Sub-resource keys extend that with another "#": "scheme:#identifier#subKey".
  -- | We detect this by searching for a "#" after the first one.
  isSubResourceKey :: String -> Boolean
  isSubResourceKey key = case Str.indexOf (Str.Pattern "#") key of
    Nothing -> false
    Just p1 -> case Str.indexOf (Str.Pattern "#") (Str.drop (p1 + 1) key) of
      Nothing -> false
      Just _ -> true

  -- | Check whether a DeltaStoreRecord is a sub-resource delta of the given role instance.
  -- | A sub-resource delta has a resourceKey that starts with roleInstanceId <> "#".
  -- | This avoids the incorrect Str.contains "#" check: roleInstanceId itself contains "#"
  -- | (e.g. "def:#id"), so every delta resourceKey in a role's delta set also contains "#".
  isSubResourceDeltaOf :: String -> DeltaStoreRecord -> Boolean
  isSubResourceDeltaOf roleInstanceId (DeltaStoreRecord { resourceKey }) =
    let
      roleKey = safeKey roleInstanceId
      key = safeKey resourceKey
    in
      Str.length key > Str.length roleKey
        && Str.take (Str.length roleKey + 1) key == roleKey <> "#"

  -- | Extract the role instance ID from a sub-resource key.
  -- | A sub-resource key has the form: roleInstanceId <> "#" <> subResourceId,
  -- | where roleInstanceId itself contains one "#" (the scheme separator, e.g. "def:#id").
  -- | We find the SECOND "#" to locate the role/sub-resource boundary.
  -- | If there is no second "#" the key is itself a role instance ID; it is returned as-is.
  extractRoleInstanceId :: String -> String
  extractRoleInstanceId key = case Str.indexOf (Str.Pattern "#") key of
    Nothing -> key
    Just p1 -> case Str.indexOf (Str.Pattern "#") (Str.drop (p1 + 1) key) of
      Nothing -> key -- Only one "#": this IS a role instance key, not a sub-resource key.
      Just p2 -> Str.take (p1 + 1 + p2) key

  executeDelta :: SignedDelta -> Maybe String -> MonadPerspectivesTransaction Unit
  -- For now, we fail silently on deltas that cannot be authenticated.
  executeDelta s Nothing = pure unit
  executeDelta s (Just stringifiedDelta) = do
    storageSchemes <- lift $ gets _.typeToStorage
    let
      execute = case parseIncomingDelta stringifiedDelta of
        Right (RolePropertyEnvelope d1) -> lift (addResourceSchemes storageSchemes d1) >>= flip executeRolePropertyDelta s
        Right (RoleBindingEnvelope d2) -> lift (addResourceSchemes storageSchemes d2) >>= flip executeRoleBindingDelta s
        Right (ContextEnvelope d3) -> lift (addResourceSchemes storageSchemes d3) >>= flip executeContextDelta s
        Right (UniverseRoleEnvelope d4) -> lift (addResourceSchemes storageSchemes d4) >>= flip executeUniverseRoleDelta s
        Right (UniverseContextEnvelope d5) -> lift (addResourceSchemes storageSchemes d5) >>= flip executeUniverseContextDelta s
        Left _ -> throwError (error "Cannot execute an unparseable incoming delta")
    if isCheckpoint stringifiedDelta then execute
    else catchError execute \e -> do
      lift $ renderPerspectivesError >=> warnSync $ (DeltaExecutionError (show e))
      case parseIncomingDelta stringifiedDelta of
        Right (RolePropertyEnvelope _) -> throwError e
        _ -> pure unit

-- Reconstruct each property once, instead of treating retained history as a new
-- sequence of writes. Baseline-boundary conflicts use the original author's ID.
restorePropertyHistories :: Array DeltaStoreRecord -> MonadPerspectivesTransaction Unit
restorePropertyHistories records = do
  let
    groups = foldl
      (\acc record@(DeltaStoreRecord r) -> Map.insertWith (<>) (safeKey r.resourceKey) [ record ] acc)
      Map.empty
      (filter (\(DeltaStoreRecord r) -> isPropertyOperation r.deltaType) records)
  for_ groups restorePropertyFromHistory

restorePropertyFromHistory :: Array DeltaStoreRecord -> MonadPerspectivesTransaction Unit
restorePropertyFromHistory records = case propertyHistory records of
  Left message -> throwError $ error message
  Right state -> case last state.changes of
    Nothing -> pure unit
    Just signed -> case parseIncomingDelta (unwrap signed).encryptedDelta of
      Right (RolePropertyEnvelope propertyDelta) -> do
        storageSchemes <- lift $ gets _.typeToStorage
        RolePropertyDelta change <- lift $ addResourceSchemes storageSchemes propertyDelta
        setProperty [ change.id ] change.property (Just signed) state.values
      _ -> throwError (error "Property history contains a non-property delta")

-- | All identifiers in deltas in a transaction have been stripped from their storage schemes, except for those with the pub: scheme.
-- | This function adds public resource schemes for the given storageUrl or, when a different publishing point is found for an identifier,
-- | that specific url.
-- | As an INVARIANT, all these (public) resources are de-cached, so we ensure that they are freshly retrieved from the database prior to executing 
-- | a delta on them. This is so we mitigate the risk of a peer having written the resource while we have an older version in cache.
-- | Pending saves and new, unpersisted instances remain cached. Previously persisted
-- | instances are evicted even if their database was deleted, so creation deltas can reconstruct them.
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
    case parseIncomingDelta encryptedDelta of
      Right (RolePropertyEnvelope d1) -> notWhenPublicSubject (unwrap d1) (lift $ (Just <<< RPD s' <$> addPublicResourceScheme storageUrl d1))
      Right (RoleBindingEnvelope d2) -> notWhenPublicSubject (unwrap d2) (lift $ (Just <<< RBD s' <$> addPublicResourceScheme storageUrl d2))
      Right (ContextEnvelope d3) -> notWhenPublicSubject (unwrap d3) (lift $ (Just <<< CDD s' <$> addPublicResourceScheme storageUrl d3))
      Right (UniverseRoleEnvelope d4) -> notWhenPublicSubject (unwrap d4) (lift $ (Just <<< URD s' <$> addPublicResourceScheme storageUrl d4))
      Right (UniverseContextEnvelope d5) -> notWhenPublicSubject (unwrap d5) (lift $ (Just <<< UCD s' <$> addPublicResourceScheme storageUrl d5))
      Left _ -> (lift $ renderPerspectivesError >=> warnSync $ (UnparseableIncomingDelta encryptedDelta)) *> pure Nothing

    where
    notWhenPublicSubject :: forall f. DeltaRecord f -> MonadPerspectivesTransaction (Maybe Delta) -> MonadPerspectivesTransaction (Maybe Delta)
    notWhenPublicSubject rec a = (lift $ isPublic rec.subject) >>= if _ then pure Nothing else a

executeDeltas :: Array Delta -> MonadPerspectivesTransaction Unit
-- We use `for` rather than `for_` because the latter folds from the right, starting with the last element.
executeDeltas deltas = do
  -- Prevent addDelta/insertDelta from storing deltas in the DeltaStore.
  -- These deltas are applied on behalf of a public role; they must not be stored
  -- (the originating user's installation already stored them under the original author key).
  modify (over Transaction \tr -> tr { isExecutingIncomingDeltas = true })
  -- File publication reads the final property metadata, after baseline/tail replay.
  let ordered = filter (not <<< isFileUpload) deltas <> filter isFileUpload deltas
  void $ for ordered case _ of
    UCD s d -> executeUniverseContextDelta d s
    URD s d -> executeUniverseRoleDelta d s
    CDD s d -> executeContextDelta d s
    RBD s d -> executeRoleBindingDelta d s
    RPD s d -> executeRolePropertyDelta d s
  where
  isFileUpload (RPD _ (RolePropertyDelta { deltaType: UploadFile })) = true
  isFileUpload _ = false

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
