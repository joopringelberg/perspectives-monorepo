-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.Instances.Clipboard where

import Prelude

import Data.Maybe (Maybe)
import Perspectives.CoreTypes (MonadPerspectives, (##>))
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (binding)
import Perspectives.ModelDependencies (itemsOnClipboard, selectedClipboardItem)
import Perspectives.Names (getMySystem)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), RoleType(..))

getSelectedRoleFromClipboard :: MonadPerspectives (Maybe RoleInstance)
getSelectedRoleFromClipboard = do
  -- Get the SelectedClipboardItem.
  -- Then get its ClipboardData and return that.
  mysystem <- getMySystem
  (ContextInstance mysystem) ##> ((getRoleInstances (CR $ CalculatedRoleType selectedClipboardItem)) >=> binding) 

-- | Given a role instance that might have been saved on the clipboard, 
-- | returns the item on the clipboard that holds it.
findItemOnClipboardWithRole :: RoleInstance -> MonadPerspectives (Maybe RoleInstance)
findItemOnClipboardWithRole rid = do
  mysystem <- getMySystem
  ((ContextInstance mysystem) ##> filter
    (getRoleInstances (ENR $ EnumeratedRoleType itemsOnClipboard))
    (binding >=> \r -> pure (r == rid)))

-- | returns all items on the clipboard (not the roles that have been saved on the clipboard: that are the fillers).
allItemsOnClipboard :: MonadPerspectives (Maybe RoleInstance)
allItemsOnClipboard = do
  mysystem <- getMySystem
  (ContextInstance mysystem) ##> (getRoleInstances (ENR $ EnumeratedRoleType itemsOnClipboard))