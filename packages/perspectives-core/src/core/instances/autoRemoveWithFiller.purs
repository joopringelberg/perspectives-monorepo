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

module Perspectives.Instances.AutoRemoveWithFiller where

import Prelude

import Data.Array (catMaybes)
import Perspectives.CoreTypes (MonadPerspectives, (##>), (##=))
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (binding)
import Perspectives.ModelDependencies (itemsOnClipboard, notifications, pinnedContexts, recentContexts)
import Perspectives.Names (getMySystem)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))

-- | Given a role instance that will be removed, return the eventual item on the clipboard, 
-- | the recent context, the pinned context and the notifications that it fills.
emptyRolesToRemove :: RoleInstance -> MonadPerspectives (Array RoleInstance)
emptyRolesToRemove rid = do
  mysystem <- getMySystem
  mclipboardItem <- ((ContextInstance mysystem) ##> filter
    (getRoleInstances (ENR $ EnumeratedRoleType itemsOnClipboard))
    (binding >=> \r -> pure (r == rid)))
  mrecentContext <- ((ContextInstance mysystem) ##> filter
    (getRoleInstances (ENR $ EnumeratedRoleType recentContexts))
    (binding >=> \r -> pure (r == rid)))
  mpinnedContext <- ((ContextInstance mysystem) ##> filter
    (getRoleInstances (ENR $ EnumeratedRoleType pinnedContexts))
    (binding >=> \r -> pure (r == rid)))
  notifications <- ((ContextInstance mysystem) ##= filter
    (getRoleInstances (ENR $ EnumeratedRoleType notifications))
    (binding >=> \r -> pure (r == rid)))
  pure $ notifications <> catMaybes [mclipboardItem, mrecentContext, mpinnedContext]
