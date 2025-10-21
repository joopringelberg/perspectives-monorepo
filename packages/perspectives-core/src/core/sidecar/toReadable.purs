module Perspectives.Sidecar.ToReadable where

import Prelude

import Control.Monad.Except.Trans (throwError)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, swap)
import Effect.Aff (error)
import Foreign.Object (Object, fromFoldable, insert, lookup, toUnfoldable, empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Identifiers (typeUri2ModelUri_)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sidecar.StableIdMapping (StableIdMapping, fromLocalModels, loadStableMapping)

type SideCarMap = Object StableIdMapping

newtype IndexedContext = IndexedContext String

runWithSideCars :: forall a. StateT SideCarMap MonadPerspectives a -> MonadPerspectives a
runWithSideCars action = evalStateT action empty

class StableToReadable a where
  toReadable :: a -> StateT SideCarMap MonadPerspectives a

instance StableToReadable IndexedContext where
  toReadable (IndexedContext stableId) = do
    { contextIndividuals } <- ensureSideCar stableId
    -- Is our stable id one of the values in this mapping?
    case lookup stableId (fromFoldable (swap <$> ((toUnfoldable contextIndividuals) :: Array (Tuple String String)))) of
      Just readableId -> pure (IndexedContext readableId)
      Nothing -> pure (IndexedContext stableId)

ensureSideCar :: String -> StateT SideCarMap MonadPerspectives StableIdMapping
ensureSideCar stableId = do
  sidecars <- get
  case lookup stableId sidecars of
    Just mapping -> pure mapping
    Nothing -> do
      mmapping <- lift $ loadStableMapping (ModelUri $ unsafePartial typeUri2ModelUri_ stableId) fromLocalModels
      case mmapping of
        Just mapping -> do
          modify_ (insert stableId mapping)
          pure mapping
        Nothing -> throwError (error "Failed to load stable mapping")