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

-- | This module provides two complementary strategies for handling a missing resource
-- | (a role or context for which the local entities database has no document):
-- |
-- | 1. **Restore**: Re-applies the creation and modification deltas stored in the DeltaStore
-- |    to reconstitute the missing document. See `Perspectives.RestoreResource`.
-- |
-- | 2. **Clean up**: Removes all dangling references to the missing resource. This was the
-- |    previous default behaviour.
-- |
-- | `fixReferences` first restores the missing resource (so its readable name and type are
-- | available), then asks the end user via a blocking `setPDRStatus` message whether to keep
-- | the restored resource or permanently remove all dangling references.  The frontend puts
-- | its choice (true = restore, false = remove) into the `userIntegrityChoice` AVar stored in
-- | `PerspectivesState`; `fixReferences` blocks until that AVar is filled.

-- END LICENSE
module Perspectives.ReferentialIntegrity
  ( fixReferences
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Writer (execWriterT, lift, tell)
import Data.Array (concat, delete)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (drop, length) as Str
import Data.Traversable (for, for_)
import Effect.Aff.AVar (take) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Foreign.Object (mapWithKey)
import Perspectives.Assignment.Update (cacheAndSave)
import Perspectives.ContextAndRole (changeContext_me, context_me, removeRol_gevuldeRollen, rol_binding, rol_gevuldeRollen, setRol_gevuldeRollen)
import Perspectives.ContextStateCompiler (evaluateContextState)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, ResourceToBeStored(..))
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError')
import Perspectives.HumanReadableType (translateType)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol)
import Perspectives.Instances.Clipboard (findItemOnClipboardWithRole)
import Perspectives.Instances.ObjectGetters (Filler_(..), context2roleFromDatabase_, contextType_, filled2fillerFromDatabase_, filler2filledFromDatabase_, role2contextFromDatabase_, roleType_)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Persistent (getPerspectContext, getPerspectRol, removeEntiteit, saveMarkedResources)
import Perspectives.PerspectivesState (getUserIntegrityChoiceAVar, getPDRStatusSetter, transactionLevel)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType(..), RoleType(..))
import Perspectives.RestoreResource (restoreResource)
import Perspectives.RoleAssignment (filledNoLongerPointsTo) as RA
import Perspectives.RoleStateCompiler (evaluateRoleState)
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runEmbeddedIfNecessary)
import Perspectives.SaveUserData (scheduleRoleRemoval)
import Perspectives.Types.ObjectGetters (contextGroundState, roleGroundState)
import Simple.JSON (writeJSON)

-- | Handle a missing resource by first restoring it from the DeltaStore and then
-- | asking the end user whether to keep the restored resource or permanently remove
-- | all dangling references to it.
-- |
-- | The user is presented with a blocking dialog via the `setPDRStatus`
-- | "requestUserIntegrityChoice" channel.  The frontend must:
-- |   1. Show the dialog with the provided message and the two option labels.
-- |   2. Call the PDR's `putUserIntegrityChoice(true/false)` API to unblock this fiber.
-- |
-- | Domain-file resources are silently ignored.
fixReferences :: ResourceToBeStored -> MonadPerspectives Unit
fixReferences resource@(Rle roleId) = do
  -- Restore the resource so that its type is queryable.
  restoreResource resource
  -- Determine a human-readable description for the dialog.
  mTypeName <- try $ roleType_ roleId >>= translateType
  let
    typeName = either (const "onbekende rol") identity mTypeName
    instanceDisplay = trailingInstanceId (unwrap roleId)
    message = buildIntegrityChoiceMessage "rol" instanceDisplay typeName
  -- Ask the user; this call blocks until the frontend puts a value into the AVar.
  choice <- requestIntegrityChoice message
  if choice
    then
      -- User chose "Herstel": the resource is already restored; persist it.
      saveMarkedResources
    else do
      -- User chose "Verwijder definitief": undo the restoration and clean up.
      void $ removeEntiteit roleId
      fixRoleReferences roleId
fixReferences resource@(Ctxt contextId) = do
  -- Restore the resource so that its type is queryable.
  restoreResource resource
  -- Determine a human-readable description for the dialog.
  mTypeName <- try $ contextType_ contextId >>= translateType
  let
    typeName = either (const "onbekende context") identity mTypeName
    instanceDisplay = trailingInstanceId (unwrap contextId)
    message = buildIntegrityChoiceMessage "context" instanceDisplay typeName
  -- Ask the user; this call blocks until the frontend puts a value into the AVar.
  choice <- requestIntegrityChoice message
  if choice
    then
      -- User chose "Herstel": the resource is already restored; persist it.
      saveMarkedResources
    else do
      -- User chose "Verwijder definitief": undo the restoration and clean up.
      void $ removeEntiteit contextId
      fixContextReferences contextId
fixReferences (Dfile _) = pure unit

-- | Send a "requestUserIntegrityChoice" status message to all connected frontend clients
-- | and block this fiber until one of them puts a Boolean into `userIntegrityChoice`.
-- | Returns `true` (restore) or `false` (remove permanently).
requestIntegrityChoice :: String -> MonadPerspectives Boolean
requestIntegrityChoice message = do
  setPDRStatus <- getPDRStatusSetter
  _ <- pure $ setPDRStatus "requestUserIntegrityChoice" message
  choiceAVar <- getUserIntegrityChoiceAVar
  liftAff $ AVar.take choiceAVar

-- | The hardcoded Dutch dialog text mirrors the spec agreed with the product owner.
-- | If multi-language support is needed in the future, this string should be moved
-- | to the translation system (see Perspectives.ModelTranslation).
buildIntegrityChoiceMessage :: String -> String -> String -> String
buildIntegrityChoiceMessage resourceKind instanceDisplay typeName =
  writeJSON
    { resourceKind
    , message:
        "De "
          <> resourceKind
          <> " "
          <> instanceDisplay
          <> ", een "
          <> typeName
          <> " is niet langer beschikbaar, maar er wordt nog wel naar verwezen."
          <> " Dat kan vanuit een andere rol of context zijn, maar ook vanuit het klembord of de vastgeprikte contexten."
          <> " Wil je deze "
          <> resourceKind
          <> " definitief verwijderen of juist herstellen?"
    , restoreOption: "Herstel"
    , removeOption: "Verwijder definitief"
    }

-- | Returns the trailing 30 characters of a string, or the full string if
-- | shorter than 30 characters.  Used to produce a short instance identifier
-- | for user-facing messages without exposing the full (potentially long) ID.
trailingInstanceId :: String -> String
trailingInstanceId s =
  let l = Str.length s
  in if l > 30 then Str.drop (l - 30) s else s

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

