module Perspectives.Sidecar.ToReadable where

import Prelude

import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Data.EncodableMap as EM
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (typeUri2ModelUri_)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getDomeinFile)
import Perspectives.PerspectivesState (getModelUnderCompilation)
import Perspectives.Query.QueryTypes (Domain(..), RoleInContext(..))
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType)
import Perspectives.Representation.Class.PersistentType (getCalculatedProperty, getCalculatedRole, getContext, getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType, CalculatedRoleType, ContextType, EnumeratedPropertyType, IndexedContext(..), PropertyType(..), RoleType(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sidecar.StableIdMapping (StableIdMapping, fromLocalModels, loadStableMapping)

-- Also see module Perspectives.HumanReadableType, where we convert from Stable to Readable based on the displayName property of the type.

type SideCarMap = Map String StableIdMapping

type WithSideCars = StateT SideCarMap MonadPerspectives

runWithSideCars :: forall a. WithSideCars a -> MonadPerspectives a
runWithSideCars action = evalStateT action empty

runWithPreloadedSideCars :: forall a. Map String StableIdMapping -> WithSideCars a -> MonadPerspectives a
runWithPreloadedSideCars preloaded action = evalStateT action preloaded

class StableToReadable a where
  toReadable :: a -> MonadPerspectives a

instance StableToReadable IndexedContext where
  toReadable ic@(IndexedContext stableId) = do
    getDomeinFile (ModelUri $ unsafePartial typeUri2ModelUri_ stableId) >>= \(DomeinFile { toReadableContextIndividuals }) -> case EM.lookup ic toReadableContextIndividuals of
      Just readableId -> pure readableId
      Nothing -> do
        logPerspectivesError (Custom $ "Failed to convert IndexedContext from stable to readable: no mapping found for stable id " <> stableId)
        pure (IndexedContext stableId)

instance StableToReadable ContextType where
  toReadable ct = getContext ct >>= \(Context { readableName }) -> pure readableName

instance StableToReadable EnumeratedRoleType where
  toReadable er = getEnumeratedRole er >>= \(EnumeratedRole { readableName }) -> pure readableName

instance StableToReadable CalculatedRoleType where
  toReadable er = getCalculatedRole er >>= \(CalculatedRole { readableName }) -> pure readableName

instance StableToReadable RoleType where
  toReadable (ENR r) = ENR <$> toReadable r
  toReadable (CR r) = CR <$> toReadable r

-- Convert a PropertyType that carries a stable identifier into its readable FQN variant
instance StableToReadable PropertyType where
  toReadable (ENP p) = ENP <$> toReadable p
  toReadable (CP p) = CP <$> toReadable p

instance StableToReadable EnumeratedPropertyType where
  toReadable er = getEnumeratedProperty er >>= \(EnumeratedProperty { readableName }) -> pure readableName

instance StableToReadable CalculatedPropertyType where
  toReadable er = getCalculatedProperty er >>= \(CalculatedProperty { readableName }) -> pure readableName

instance StableToReadable RoleInContext where
  toReadable (RoleInContext { role, context }) = do
    role' <- toReadable role
    context' <- toReadable context
    pure $ RoleInContext { role: role', context: context' }

instance StableToReadable (ADT RoleInContext) where
  toReadable adtRoleInContext = traverse toReadable adtRoleInContext

instance StableToReadable (ADT ContextType) where
  toReadable adtContextType = traverse toReadable adtContextType

instance StableToReadable Domain where
  toReadable (RDOM adt) = RDOM <$> toReadable adt
  toReadable (CDOM adt) = CDOM <$> toReadable adt
  toReadable (VDOM r mproptype) = do
    mproptype' <- case mproptype of
      Just proptype -> Just <$> toReadable proptype
      Nothing -> pure Nothing
    pure $ VDOM r mproptype'
  toReadable x = pure x

ensureSideCar :: String -> StateT SideCarMap MonadPerspectives (Maybe StableIdMapping)
ensureSideCar stableId = do
  let modelUri = unsafePartial typeUri2ModelUri_ stableId
  mmodelUnderCompilation <- lift $ getModelUnderCompilation
  case mmodelUnderCompilation of
    Just (ModelUri modelUnderCompilation) ->
      if modelUri == modelUnderCompilation then
        -- Do not try to load the sidecar for the model under compilation itself. 
        -- On first compilation, it will not be available yet.
        -- Neiter do we need it: all identifiers from the model under compilation are still readable.
        pure Nothing
      else lookupSideCar modelUri
    Nothing -> lookupSideCar modelUri
  where
  lookupSideCar :: String -> StateT SideCarMap MonadPerspectives (Maybe StableIdMapping)
  lookupSideCar modelUri = do
    sidecars <- get
    case lookup modelUri sidecars of
      Just mapping -> pure $ Just mapping
      Nothing -> do
        mmapping <- lift $ loadStableMapping (ModelUri modelUri) fromLocalModels
        case mmapping of
          Just mapping -> do
            modify_ (insert modelUri mapping)
            pure $ Just mapping
          Nothing -> do
            logPerspectivesError (Custom $ "Failed to load stable mapping for sidecar with id: " <> modelUri)
            pure Nothing
