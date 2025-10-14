module Perspectives.HumanReadableType where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Class.PersistentType (getPerspectType)
import Perspectives.Representation.Context (Context)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole)
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), StateIdentifier(..), ViewType(..))
import Perspectives.Representation.View (View)

class Identifiable e i <= HumanReadableType e i | i -> e where
  lookupDisplayName :: i -> MonadPerspectives String
  swapDisplayName :: i -> MonadPerspectives i

instance HumanReadableType EnumeratedRole EnumeratedRoleType where
  lookupDisplayName er = getPerspectType er >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = catchError (lookupDisplayName x >>= pure <<< EnumeratedRoleType) \_ -> pure x

instance HumanReadableType EnumeratedProperty EnumeratedPropertyType where
  lookupDisplayName ep = getPerspectType ep >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = catchError (lookupDisplayName x >>= pure <<< EnumeratedPropertyType) \_ -> pure x

instance HumanReadableType CalculatedRole CalculatedRoleType where
  lookupDisplayName cr = getPerspectType cr >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = catchError (lookupDisplayName x >>= pure <<< CalculatedRoleType) \_ -> pure x

instance HumanReadableType CalculatedProperty CalculatedPropertyType where
  lookupDisplayName cp = getPerspectType cp >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = catchError (lookupDisplayName x >>= pure <<< CalculatedPropertyType) \_ -> pure x

instance HumanReadableType Context ContextType where
  lookupDisplayName ct = getPerspectType ct >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = catchError (lookupDisplayName x >>= pure <<< ContextType) \_ -> pure x

instance HumanReadableType View ViewType where
  lookupDisplayName vt = getPerspectType vt >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = catchError (lookupDisplayName x >>= pure <<< ViewType) \_ -> pure x

instance HumanReadableType State StateIdentifier where
  lookupDisplayName st = getPerspectType st >>= \(State r) -> pure
    ( case r.displayName of
        Just dn -> dn
        Nothing -> unwrap $ r.id
    )
  swapDisplayName x = catchError (lookupDisplayName x >>= pure <<< StateIdentifier) \_ -> pure x
