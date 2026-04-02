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

-- | This module handles missing resources (roles or contexts for which the local
-- | entities database has no document) by automatically restoring them from the
-- | DeltaStore and notifying the end user via a piggybacked warning message.
-- |
-- | The warning carries the external role ID of the restored context and the
-- | context's display name so that the frontend can render a hyperlink that
-- | dispatches an `OpenContext` event, allowing the user to navigate directly to
-- | the restored context.
-- |
-- | See `packages/perspectives-core/docsources/pdr-messaging.md` for a full
-- | description of the warning piggybacking mechanism.

-- END LICENSE
module Perspectives.ReferentialIntegrity
  ( fixReferences
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.ContextAndRole (context_buitenRol, context_displayName)
import Perspectives.CoreTypes (MonadPerspectives, ResourceToBeStored(..))
import Perspectives.Identifiers (buitenRol)
import Perspectives.InstanceRepresentation (PerspectContext)
import Perspectives.Instances.ObjectGetters (role2contextFromDatabase_)
import Perspectives.Persistent (getPerspectContext, saveMarkedResources)
import Perspectives.PerspectivesState (addWarning)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.RestoreResource (restoreResource)

-- | Handle a missing resource by restoring it from the DeltaStore and notifying
-- | the end user via a piggybacked warning message.
-- |
-- | The warning includes the external role ID and display name of the restored
-- | context so that the frontend can offer a navigation hyperlink.
-- |
-- | Domain-file resources are silently ignored.
fixReferences :: ResourceToBeStored -> MonadPerspectives Boolean
fixReferences resource@(Rle roleId) = do
  -- Restore the resource so that it is available in the database.
  restoreResource resource
  -- Find the context of this role to determine the external role for navigation.
  contextIds <- role2contextFromDatabase_ roleId
  { displayName, extRole } <- case head contextIds of
    Nothing -> pure { displayName: "", extRole: "" }
    Just contextId -> do
      mCtxt <- try $ getPerspectContext contextId
      pure $ case mCtxt of
        Left _ -> { displayName: "", extRole: buitenRol (unwrap contextId) }
        Right ctxt -> { displayName: context_displayName ctxt, extRole: unwrap (context_buitenRol ctxt) }
  -- Notify the user that the resource has been restored, via the piggybacked warning mechanism.
  addWarning
    { message: "Some data had been damaged or lost. The missing data have been restored. Click the hyperlink to open the relevant context:"
    , error: ""
    , externalRoleId: extRole
    , contextName: displayName
    }
  -- Persist the restored resource.
  saveMarkedResources
  pure true
fixReferences resource@(Ctxt contextId) = do
  -- Restore the resource so that it is available in the database.
  restoreResource resource
  -- Get the external role and display name of the restored context for navigation.
  mCtxt <- try $ getPerspectContext contextId
  let { displayName, extRole } = case mCtxt of
        Left _ -> { displayName: "", extRole: buitenRol (unwrap contextId) }
        Right ctxt -> { displayName: context_displayName ctxt, extRole: unwrap (context_buitenRol ctxt) }
  -- Notify the user that the context has been restored, via the piggybacked warning mechanism.
  addWarning
    { message: "Some data had been damaged or lost. The missing data have been restored. Click the hyperlink to open the relevant context:"
    , error: ""
    , externalRoleId: extRole
    , contextName: displayName
    }
  -- Persist the restored context.
  saveMarkedResources
  pure true
fixReferences (Dfile _) = pure false

