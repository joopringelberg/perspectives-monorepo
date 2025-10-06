module Perspectives.HumanReadableType where

import Prelude

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
  swapDisplayName x = lookupDisplayName x >>= pure <<< EnumeratedRoleType

instance HumanReadableType EnumeratedProperty EnumeratedPropertyType where
  lookupDisplayName ep = getPerspectType ep >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = lookupDisplayName x >>= pure <<< EnumeratedPropertyType

instance HumanReadableType CalculatedRole CalculatedRoleType where
  lookupDisplayName cr = getPerspectType cr >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = lookupDisplayName x >>= pure <<< CalculatedRoleType

instance HumanReadableType CalculatedProperty CalculatedPropertyType where
  lookupDisplayName cp = getPerspectType cp >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = lookupDisplayName x >>= pure <<< CalculatedPropertyType

instance HumanReadableType Context ContextType where
  lookupDisplayName ct = getPerspectType ct >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = lookupDisplayName x >>= pure <<< ContextType

instance HumanReadableType View ViewType where
  lookupDisplayName vt = getPerspectType vt >>= pure <<< _.displayName <<< unwrap
  swapDisplayName x = lookupDisplayName x >>= pure <<< ViewType

instance HumanReadableType State StateIdentifier where
  lookupDisplayName st = getPerspectType st >>= \(State r) -> pure
    ( case r.displayName of
        Just dn -> dn
        Nothing -> unwrap $ r.id
    )
  swapDisplayName x = lookupDisplayName x >>= pure <<< StateIdentifier

