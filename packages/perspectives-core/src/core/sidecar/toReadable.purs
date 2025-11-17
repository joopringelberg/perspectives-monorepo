module Perspectives.Sidecar.ToReadable where

import Prelude

import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array (find)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), swap)
import Foreign.Object as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (typeUri2LocalName, typeUri2ModelUri_)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.PerspectivesState (getModelUnderCompilation)
import Perspectives.Query.QueryTypes (Domain(..), RoleInContext(..))
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), PropertyType(..), RoleType(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sidecar.StableIdMapping (StableIdMapping, fromLocalModels, loadStableMapping)

-- Also see module Perspectives.HumanReadableType, where we convert from Stable to Readable based on the displayName property of the type.

type SideCarMap = Map String StableIdMapping

type WithSideCars = StateT SideCarMap MonadPerspectives

newtype IndexedContext = IndexedContext String

runWithSideCars :: forall a. WithSideCars a -> MonadPerspectives a
runWithSideCars action = evalStateT action empty

runWithPreloadedSideCars :: forall a. Map String StableIdMapping -> WithSideCars a -> MonadPerspectives a
runWithPreloadedSideCars preloaded action = evalStateT action preloaded

class StableToReadable a where
  toReadable :: a -> WithSideCars a

instance StableToReadable IndexedContext where
  toReadable (IndexedContext stableId) = do
    mmapping <- ensureSideCar stableId
    case mmapping of
      Just { contextIndividuals } -> do
        -- Is our stable id one of the values in this mapping?
        case OBJ.lookup stableId (OBJ.fromFoldable (swap <$> ((OBJ.toUnfoldable contextIndividuals) :: Array (Tuple String String)))) of
          Just readableId -> pure (IndexedContext readableId)
          Nothing -> pure (IndexedContext stableId)
      Nothing -> pure (IndexedContext stableId)

instance StableToReadable ContextType where
  toReadable (ContextType stableContextId) = do
    mmapping <- ensureSideCar stableContextId
    case mmapping of
      Just { contextCuids } -> do
        let
          cuid = case typeUri2LocalName stableContextId of
            Just x -> x
            Nothing -> stableContextId
          pairs = (OBJ.toUnfoldable contextCuids :: Array (Tuple String String))
        case find (\(Tuple _ c) -> c == cuid) pairs of
          Just (Tuple fqn _) -> pure $ ContextType fqn
          Nothing -> pure $ ContextType stableContextId
      Nothing -> pure $ ContextType stableContextId

instance StableToReadable EnumeratedRoleType where
  toReadable (EnumeratedRoleType stableRoleId) = do
    mmapping <- ensureSideCar stableRoleId
    case mmapping of
      Just { roleCuids } -> do
        let
          cuid = case typeUri2LocalName stableRoleId of
            Just x -> x
            Nothing -> stableRoleId
          pairs = (OBJ.toUnfoldable roleCuids :: Array (Tuple String String))
        case find (\(Tuple _ c) -> c == cuid) pairs of
          Just (Tuple fqn _) -> pure $ EnumeratedRoleType fqn
          Nothing -> pure $ EnumeratedRoleType stableRoleId
      Nothing -> pure $ EnumeratedRoleType stableRoleId

instance StableToReadable CalculatedRoleType where
  toReadable (CalculatedRoleType stableRoleId) = do
    mmapping <- ensureSideCar stableRoleId
    case mmapping of
      Just { roleCuids } -> do
        let
          cuid = case typeUri2LocalName stableRoleId of
            Just x -> x
            Nothing -> stableRoleId
          pairs = (OBJ.toUnfoldable roleCuids :: Array (Tuple String String))
        case find (\(Tuple _ c) -> c == cuid) pairs of
          Just (Tuple fqn _) -> pure $ CalculatedRoleType fqn
          Nothing -> pure $ CalculatedRoleType stableRoleId
      Nothing -> pure $ CalculatedRoleType stableRoleId

instance StableToReadable RoleType where
  toReadable (ENR r) = ENR <$> toReadable r
  toReadable (CR r) = CR <$> toReadable r

-- Convert a PropertyType that carries a stable identifier into its readable FQN variant
instance StableToReadable PropertyType where
  toReadable (ENP p) = ENP <$> toReadable p
  toReadable (CP p) = CP <$> toReadable p

instance StableToReadable EnumeratedPropertyType where
  toReadable (EnumeratedPropertyType stablePropId) = do
    mmapping <- ensureSideCar stablePropId
    case mmapping of
      Just { propertyCuids } -> do
        let
          cuid = case typeUri2LocalName stablePropId of
            Just x -> x
            Nothing -> stablePropId
          pairs = (OBJ.toUnfoldable propertyCuids :: Array (Tuple String String))
        case find (\(Tuple _ c) -> c == cuid) pairs of
          Just (Tuple fqn _) -> pure (EnumeratedPropertyType fqn)
          Nothing -> pure (EnumeratedPropertyType stablePropId)
      Nothing -> pure (EnumeratedPropertyType stablePropId)

instance StableToReadable CalculatedPropertyType where
  toReadable (CalculatedPropertyType stablePropId) = do
    mmapping <- ensureSideCar stablePropId
    case mmapping of
      Just { propertyCuids } -> do
        let
          cuid = case typeUri2LocalName stablePropId of
            Just x -> x
            Nothing -> stablePropId
          pairs = (OBJ.toUnfoldable propertyCuids :: Array (Tuple String String))
        case find (\(Tuple _ c) -> c == cuid) pairs of
          Just (Tuple fqn _) -> pure (CalculatedPropertyType fqn)
          Nothing -> pure (CalculatedPropertyType stablePropId)
      Nothing -> pure (CalculatedPropertyType stablePropId)

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
