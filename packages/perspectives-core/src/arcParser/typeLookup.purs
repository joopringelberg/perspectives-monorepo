module Perspectives.Parsing.Arc.PhaseThree.TypeLookup where

import Control.Monad.Trans.Class (lift)
import Control.Plus (map)
import Data.Array (filter, filterA)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Identifiers (areLastSegmentsOf)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree)
import Perspectives.Query.QueryTypes (roleInContext2Role)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.PersistentType (getCalculatedRole, readable2stable)
import Perspectives.Representation.Class.Role (allProperties, allRoles, roleADT)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, PropertyType, RoleType(..), propertytype2string, roletype2string)
import Perspectives.Sidecar.ToReadable (toReadable)
import Prelude (pure, ($), (<<<), (==), (>>=), (>>>), bind, (>=>), (<$>))

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND A PROPERTYTYPE WORKING FROM STRINGS OR ADT'S
------- NOTE: these functions work on Readable types only, in the sense that the string arguments should be Readable.
----------------------------------------------------------------------------------------

-- | Look for an unqualified Property on a given RoleType
-- | (recursing on aspects and on the binding of Enumerated Roles), using a criterium.
lookForUnqualifiedPropertyType_ :: String -> (RoleType -> PhaseThree (Array PropertyType))
lookForUnqualifiedPropertyType_ s (ENR i) = lookForProperty (propertytype2string >>> areLastSegmentsOf s) (ST i)
lookForUnqualifiedPropertyType_ s (CR i) = do
  rle <- (lift $ lift $ getCalculatedRole i)
  adt <- lift $ lift $ roleADT rle
  lookForProperty (propertytype2string >>> areLastSegmentsOf s) (map roleInContext2Role adt)

-- | Look for a Property on a given ADT, using a qualified name (recursing on aspects
-- | and on the binding of Enumerated Roles).
-- | Note: use this function to check that the property is actually defined.
lookForPropertyType :: String -> (ADT EnumeratedRoleType -> PhaseThree (Array PropertyType))
lookForPropertyType s = lookForProperty (propertytype2string >>> ((==) s))

-- | Look for a Property on a given ADT (recursing on aspects and on the binding of
-- | Enumerated Roles) using the postfix of a name.
lookForUnqualifiedPropertyType :: String -> (ADT EnumeratedRoleType -> PhaseThree (Array PropertyType))
lookForUnqualifiedPropertyType s = lookForProperty (propertytype2string >>> areLastSegmentsOf s)

-- | Look for a Property on a given ADT (recursing on aspects and on the binding of
-- | Enumerated Roles), using a criterium.
lookForProperty :: (PropertyType -> Boolean) -> ADT EnumeratedRoleType -> PhaseThree (Array PropertyType)
lookForProperty criterium adt = (lift $ lift $ allProperties adt) >>= ensurePropertiesAreReadable >>= pure <<< filter criterium

ensurePropertiesAreReadable :: Array PropertyType -> PhaseThree (Array PropertyType)
ensurePropertiesAreReadable props = lift $ lift (traverse toReadable props)

lookForUnqualifiedRoleTypeOfADT :: String -> ADT ContextType -> PhaseThree (Array RoleType)
lookForUnqualifiedRoleTypeOfADT s adt = do
  roles <- lift $ lift $ allRoles adt
  lift $ lift $ filterA (toReadable >=> roletype2string >>> areLastSegmentsOf s >>> pure) roles

readableRoletype2stable :: RoleType -> MonadPerspectives RoleType
readableRoletype2stable (CR calculatedType) = CR <$> (readable2stable calculatedType)
readableRoletype2stable (ENR enumeratedType) = ENR <$> (readable2stable enumeratedType)