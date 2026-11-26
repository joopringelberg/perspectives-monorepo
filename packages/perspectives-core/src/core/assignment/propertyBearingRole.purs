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

-- | Resolving the role instance that stores a property. Shared by the update functions
-- | (Perspectives.Assignment.Update) and property authorization (Perspectives.Checking.Authorization).
module Perspectives.Assignment.PropertyBearingRole
  ( RoleProp(..)
  , getPropertyBearingRoleInstance
  , roleProp_property
  , roleProp_roleinstance
  ) where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Array (elemIndex)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Foreign.Object (empty, lookup)
import Perspectives.CoreTypes (MonadPerspectives, (##>>))
import Perspectives.Instances.ObjectGetters (binding_, roleType)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, PropertyType(..))
import Perspectives.Types.ObjectGetters (propertyAliases)

-- | Get the property bearing role individual in the chain.
-- | If the property is defined on role instance's type (either directly or by Aspect), return it; otherwise
-- | recursively apply this function to its binding.
-- | Takes (aspect) property replacements into consideration.
getPropertyBearingRoleInstance :: EnumeratedPropertyType -> RoleInstance -> MonadPerspectives (Maybe RoleProp)
getPropertyBearingRoleInstance prop roleId = do
  tp <- roleId ##>> roleType
  -- NOTE. UP TILL version v0.20.0 we have a very specific problem with the external roles of the specialisations
  -- of model:System$Model. These roles are fetched before the models themselves are fetched; indeed, we don't mean to 
  -- get the models until the end user explicitly asks for it.
  -- This situation will go away in the next version.
  aliases <- catchError (propertyAliases tp)
    \_ -> pure empty
  case lookup (unwrap prop) aliases of
    Just destination -> pure $ Just $ RoleProp roleId destination
    Nothing -> do
      allProps <- allLocallyRepresentedProperties (ST tp)
      if isJust $ elemIndex (ENP prop) allProps then pure $ Just $ RoleProp roleId prop
      else binding_ roleId >>= maybe (pure Nothing) (getPropertyBearingRoleInstance prop)

data RoleProp = RoleProp RoleInstance EnumeratedPropertyType

roleProp_roleinstance :: RoleProp -> RoleInstance
roleProp_roleinstance (RoleProp ri _) = ri

roleProp_property :: RoleProp -> EnumeratedPropertyType
roleProp_property (RoleProp _ pt) = pt
