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
import Control.Monad.Writer (execWriterT, lift, tell)
import Data.Array (concat, delete, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (drop, length) as Str
import Data.Traversable (for, for_)
import Effect.Class.Console (log)
import Foreign.Object (mapWithKey)
import Perspectives.Assignment.Update (cacheAndSave)
import Perspectives.ContextAndRole (changeContext_me, context_buitenRol, context_displayName, context_me, removeRol_gevuldeRollen, rol_binding, rol_gevuldeRollen, setRol_gevuldeRollen)
import Perspectives.ContextStateCompiler (evaluateContextState)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, ResourceToBeStored(..))
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError')
import Perspectives.Identifiers (buitenRol)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol)
import Perspectives.Instances.Clipboard (findItemOnClipboardWithRole)
import Perspectives.Instances.ObjectGetters (Filler_(..), context2roleFromDatabase_, contextType_, filled2fillerFromDatabase_, filler2filledFromDatabase_, role2contextFromDatabase_, roleType_)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Persistent (getPerspectContext, getPerspectRol, removeEntiteit, saveMarkedResources)
import Perspectives.PerspectivesState (addWarning, transactionLevel)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType(..), RoleType(..))
import Perspectives.RestoreResource (restoreResource)
import Perspectives.RoleAssignment (filledNoLongerPointsTo) as RA
import Perspectives.RoleStateCompiler (evaluateRoleState)
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runEmbeddedIfNecessary)
import Perspectives.SaveUserData (scheduleRoleRemoval)
import Perspectives.Types.ObjectGetters (contextGroundState, roleGroundState)

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
  -- The message field serves as a stable identifier; the frontend uses i18n keys for the
  -- user-facing text when externalRoleId is non-empty (see www.tsx restorationPanel_message).
  addWarning
    { message: "RestoredMissingResource"
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
  let
    { displayName, extRole } = case mCtxt of
      Left _ -> { displayName: "", extRole: buitenRol (unwrap contextId) }
      Right ctxt -> { displayName: context_displayName ctxt, extRole: unwrap (context_buitenRol ctxt) }
  -- Notify the user that the context has been restored, via the piggybacked warning mechanism.
  -- The message field serves as a stable identifier; the frontend uses i18n keys for the
  -- user-facing text when externalRoleId is non-empty (see www.tsx restorationPanel_message).
  addWarning
    { message: "RestoredMissingResource"
    , error: ""
    , externalRoleId: extRole
    , contextName: displayName
    }
  -- Persist the restored context.
  saveMarkedResources
  pure true
fixReferences (Dfile _) = pure false

----------------------------------------------------------------------------
---- I've kept this mechanism. We might want to use it later.
----------------------------------------------------------------------------
-- | Apply this function when a reference to a context has been found that cannot be retrieved.
-- | We want all references to this context to be removed.
-- | Only roles refer to contexts.
-- | All these roles must be removed, as a role must have a context!
-- | Notice that we do not synchronize the changes.
fixContextReferences :: ContextInstance -> MonadPerspectives Unit
fixContextReferences cid@(ContextInstance c) = do
  padding <- transactionLevel
  log (padding <> "fixContextReferences: " <> show cid)
  -- As we look for dangling references in the database only, we should be sure that
  -- there is no difference between cache and database.
  saveMarkedResources
  -- These are roles that refer to the missing context as their context. They must be removed.
  referringRoles <- context2roleFromDatabase_ cid
  rolesToEvaluate <- concat <$> for referringRoles \roleId -> (try $ (getPerspectRol roleId)) >>= handlePerspectRolError' "fixContextReferences" []
    \role -> do
      -- The role that possibly refers to it as one that it fills, loses that reference.
      affectedRoles <- execWriterT do
        case rol_binding role of
          Nothing -> pure unit
          Just filler -> do
            lift (filler `fillerNoLongerPointsTo_` roleId)
            tell [ filler ]
        -- And any roles filled by roleId must no longer refer to it as their filler (binding).
        for_ (rol_gevuldeRollen role) \roleMap ->
          for_ roleMap
            ( \filledRoles' ->
                for_ filledRoles' \filled -> do
                  lift (filled `RA.filledNoLongerPointsTo` roleId)
                  tell [ filled ]
            ) 
      -- PERSISTENCE (finally remove the role instance from cache and database).
      void $ removeEntiteit roleId
      pure affectedRoles
  -- Now recompute the states of these roles.
  runEmbeddedIfNecessary doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser)
    (for_ rolesToEvaluate reEvaluateRoleStates)
  saveMarkedResources

reEvaluateRoleStates :: RoleInstance -> MonadPerspectivesTransaction Unit
reEvaluateRoleStates rid = (lift $ roleType_ rid) >>= evaluateRoleState rid <<< roleGroundState

reEvaluateContextStates :: ContextInstance -> MonadPerspectivesTransaction Unit
reEvaluateContextStates cid = (lift $ contextType_ cid) >>= evaluateContextState cid <<< contextGroundState

-- | Apply this function when a reference to a role has been found that cannot be retrieved.
-- | All references from other roles and from its context must be removed.
fixRoleReferences :: RoleInstance -> MonadPerspectives Unit
fixRoleReferences roleId@(RoleInstance r) = do
  padding <- transactionLevel
  log (padding <> "fixRoleReferences: " <> show roleId)

  -- As we look for dangling references in the database only, we should be sure that
  -- there is no difference between cache and database.
  saveMarkedResources
  ctxts <- role2contextFromDatabase_ roleId
  -- If the role type is unlinked, we will find no contexts.
  for_ ctxts removeRoleInstanceFromContext
  -- Retrieve all roles that still refer to roleId as their filler.
  filledRoles <- filler2filledFromDatabase_ (Filler_ roleId)
  -- Remove this role from all other roles that point to it as their filler.
  for_ filledRoles (flip RA.filledNoLongerPointsTo roleId)
  --- Retrieve the filler that still refers to roleId.
  fillerRoles <- filled2fillerFromDatabase_ roleId
  for_ fillerRoles
    \({ filler, filledContextType, filledRoleType }) -> (fillerNoLongerPointsTo filler roleId filledContextType filledRoleType)
  -- Now recompute the states of these roles.
  runEmbeddedIfNecessary doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser)
    ( do
        -- Now check the clipboard. It may hold a reference to the role that can no longer be found.
        (lift $ findItemOnClipboardWithRole roleId) >>= case _ of
          Nothing -> pure unit
          Just item -> void $ scheduleRoleRemoval doNotShareWithPeers item
        for_ (filledRoles <> (_.filler <$> fillerRoles)) reEvaluateRoleStates
        for_ ctxts reEvaluateContextStates
    )
  saveMarkedResources

  where
  removeRoleInstanceFromContext :: ContextInstance -> MonadPerspectives Unit
  removeRoleInstanceFromContext contextId = do
    (try $ getPerspectContext contextId) >>=
      handlePerspectContextError "removeRoleInstance"
        \(pe :: PerspectContext) -> do
          -- Modify the context: remove the role instances from those recorded with the role type.
          changedContext <- pure $ removeFromContext pe
          -- CURRENTUSER.
          case context_me pe of
            Just m | m == roleId -> do
              cacheAndSave contextId (changeContext_me changedContext Nothing)
            _ -> cacheAndSave contextId changedContext

  removeFromContext :: PerspectContext -> PerspectContext
  removeFromContext (PerspectContext ct@{ rolInContext }) = PerspectContext ct
    { rolInContext =
        mapWithKey (\rtype roles -> delete roleId roles) rolInContext
    }

-- This version only changes the administration in the filler; the full version in Perspectives.RoleAssignment changes both sides.
fillerNoLongerPointsTo :: RoleInstance -> RoleInstance -> ContextType -> EnumeratedRoleType -> MonadPerspectives Unit
fillerNoLongerPointsTo fillerId filledId filledContextType filledRoleType = (try $ getPerspectRol fillerId) >>=
  handlePerspectRolError' "fillerNoLongerPointsTo" unit
    \(filler :: PerspectRol) -> do
      filler' <- pure $ (removeRol_gevuldeRollen filler filledContextType filledRoleType filledId)
      cacheAndSave fillerId filler'

fillerNoLongerPointsTo_ :: RoleInstance -> RoleInstance -> MonadPerspectives Unit
fillerNoLongerPointsTo_ fillerId filledId = (try $ getPerspectRol fillerId) >>=
  handlePerspectRolError' "fillerNoLongerPointsTo_" unit
    \(filler :: PerspectRol) -> do
      -- As the context of the filled role has been deleted, we cannot establish its type.
      -- Instead of indexing, we have to filter the entire structure in search of fillerId.
      cacheAndSave
        fillerId
        ( setRol_gevuldeRollen
            filler
            ( mapWithKey
                ( \ctype typeGroup -> mapWithKey
                    (\rtype filleds -> delete filledId filleds)
                    typeGroup
                )
                (rol_gevuldeRollen filler)
            )
        )

