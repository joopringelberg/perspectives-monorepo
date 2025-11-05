module Perspectives.Sidecar.ToReadable where

import Prelude

import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, last)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..), swap)
import Foreign.Object as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (typeUri2ModelUri_)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), EnumeratedPropertyType(..), CalculatedPropertyType(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sidecar.StableIdMapping (StableIdMapping, fromLocalModels, loadStableMapping)

-- Also see module Perspectives.HumanReadableType, where we convert from Stable to Readable based on the displayName property of the type.

type SideCarMap = Map String StableIdMapping

newtype IndexedContext = IndexedContext String

runWithSideCars :: forall a. StateT SideCarMap MonadPerspectives a -> MonadPerspectives a
runWithSideCars action = evalStateT action empty

runWithPreloadedSideCars :: forall a. Map String StableIdMapping -> StateT SideCarMap MonadPerspectives a -> MonadPerspectives a
runWithPreloadedSideCars preloaded action = evalStateT action preloaded

class StableToReadable a where
  toReadable :: a -> StateT SideCarMap MonadPerspectives a

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

-- Convert a PropertyType that carries a stable identifier into its readable FQN variant
instance StableToReadable PropertyType where
  toReadable (ENP (EnumeratedPropertyType stablePropId)) = do
    mmapping <- ensureSideCar stablePropId
    case mmapping of
      Just { propertyCuids } -> do
        let
          cuid = case last (split (Pattern "$") stablePropId) of
            Just x -> x
            Nothing -> stablePropId
          pairs = (OBJ.toUnfoldable propertyCuids :: Array (Tuple String String))
        case find (\(Tuple _ c) -> c == cuid) pairs of
          Just (Tuple fqn _) -> pure $ ENP (EnumeratedPropertyType fqn)
          Nothing -> pure $ ENP (EnumeratedPropertyType stablePropId)
      Nothing -> pure $ ENP (EnumeratedPropertyType stablePropId)
  toReadable (CP (CalculatedPropertyType stablePropId)) = do
    mmapping <- ensureSideCar stablePropId
    case mmapping of
      Just { propertyCuids } -> do
        let
          cuid = case last (split (Pattern "$") stablePropId) of
            Just x -> x
            Nothing -> stablePropId
          pairs = (OBJ.toUnfoldable propertyCuids :: Array (Tuple String String))
        case find (\(Tuple _ c) -> c == cuid) pairs of
          Just (Tuple fqn _) -> pure $ CP (CalculatedPropertyType fqn)
          Nothing -> pure $ CP (CalculatedPropertyType stablePropId)
      Nothing -> pure $ CP (CalculatedPropertyType stablePropId)

ensureSideCar :: String -> StateT SideCarMap MonadPerspectives (Maybe StableIdMapping)
ensureSideCar stableId = do
  sidecars <- get
  let modelUri = unsafePartial typeUri2ModelUri_ stableId
  case lookup modelUri sidecars of
    Just mapping -> pure $ Just mapping
    Nothing -> do
      mmapping <- lift $ loadStableMapping (ModelUri modelUri) fromLocalModels
      case mmapping of
        Just mapping -> do
          modify_ (insert stableId mapping)
          pure $ Just mapping
        Nothing -> do
          logPerspectivesError (Custom $ "Failed to load stable mapping for sidecar with id: " <> modelUri)
          pure Nothing