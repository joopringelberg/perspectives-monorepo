-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

-- | Restore a missing resource from the DeltaStore.
-- | When a role or context identifier is found for which no resource can be
-- | located in the local entities database, this module re-applies all creation
-- | and modification deltas that are stored in the DeltaStore to reconstitute
-- | the missing document.
module Perspectives.RestoreResource
  ( restoreResource
  ) where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (lift, runExcept)
import Data.Array (filter, sortBy)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (length, take) as Str
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, ResourceToBeStored(..), removeInternally)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Persistence.DeltaStore (DeltaStoreRecord(..), getDeltasForResource, getDeltasForRoleInstance, safeKey, updateDeltaApplied)
import Perspectives.PerspectivesState (transactionLevel)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runEmbeddedIfNecessary)
import Perspectives.StrippedDelta (addResourceSchemes)
import Perspectives.Sync.HandleTransaction (executeContextDelta, executeRoleBindingDelta, executeRolePropertyDelta, executeUniverseContextDelta, executeUniverseRoleDelta)
import Perspectives.Sync.LegacyDeltas (toContextDelta, toRoleBindingDelta, toRolePropertyDelta, toUniverseContextDelta, toUniverseRoleDelta)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Simple.JSON (readJSON')

-- | Restore a missing resource from the DeltaStore.
-- | For a missing role, re-applies all creation and modification deltas.
-- | For a missing context, re-applies the context-creation delta.
-- | Domain files (Dfile) are not handled here.
-- | The entity is removed from the LRU cache before replay so that delta
-- | execution creates a fresh entity without a stale _rev.  A stale _rev
-- | causes PouchDB to fail with 404 "missing" when the document was deleted
-- | outside of PouchDB's normal API (e.g., manual IndexedDB removal).
restoreResource :: ResourceToBeStored -> MonadPerspectives Unit
restoreResource (Rle roleInstance) = do
  void $ removeInternally roleInstance
  runEmbeddedIfNecessary doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser) $
    restoreRoleFromDeltaStore (unwrap roleInstance)
restoreResource (Ctxt contextInstance) = do
  void $ removeInternally contextInstance
  runEmbeddedIfNecessary doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser) $
    restoreContextFromDeltaStore (unwrap contextInstance)
restoreResource (Dfile _) = pure unit

-- | Restore a deleted or missing role by re-applying its creation and
-- | property/binding deltas from the DeltaStore, then marking any deletion
-- | deltas as no longer applied.
-- | Role-level deltas (creation etc.) are applied before sub-resource deltas
-- | (properties, bindings) to preserve the correct construction order.
restoreRoleFromDeltaStore :: String -> MonadPerspectivesTransaction Unit
restoreRoleFromDeltaStore roleInstanceId = do
  allDeltas <- lift $ getDeltasForRoleInstance roleInstanceId
  let compareByVersion (DeltaStoreRecord r1) (DeltaStoreRecord r2) = compare r1.resourceVersion r2.resourceVersion
  -- Mark all applied deletion deltas as no longer applied.
  let deletionDeltas = filter (\(DeltaStoreRecord r) -> r.applied && isDeletionDeltaType r.deltaType) allDeltas
  for_ deletionDeltas \(DeltaStoreRecord { _id }) ->
    lift $ updateDeltaApplied _id false
  -- Role-level deltas (creation etc.): include ALL non-deletion ones regardless of applied status
  -- (role creation is idempotent).
  let
    roleLevelDeltas = sortBy compareByVersion $
      filter (\d@(DeltaStoreRecord r) -> not (isDeletionDeltaType r.deltaType) && not (isSubResourceDeltaOf roleInstanceId d)) allDeltas
  -- Sub-resource deltas (properties, bindings): only applied=true to avoid
  -- re-applying outdated or superseded modifications.
  let
    subResourceDeltas = sortBy compareByVersion $
      filter (\d@(DeltaStoreRecord r) -> r.applied && not (isDeletionDeltaType r.deltaType) && isSubResourceDeltaOf roleInstanceId d) allDeltas
  for_ roleLevelDeltas \(DeltaStoreRecord { signedDelta: sd }) ->
    applyDelta sd (Just (unwrap sd).encryptedDelta)
  for_ subResourceDeltas \(DeltaStoreRecord { signedDelta: sd }) ->
    applyDelta sd (Just (unwrap sd).encryptedDelta)

-- | Restore a deleted or missing context by re-applying its creation deltas
-- | from the DeltaStore.
restoreContextFromDeltaStore :: String -> MonadPerspectivesTransaction Unit
restoreContextFromDeltaStore contextId = do
  allDeltas <- lift $ getDeltasForResource contextId
  let compareByVersion (DeltaStoreRecord r1) (DeltaStoreRecord r2) = compare r1.resourceVersion r2.resourceVersion
  -- Apply all deltas in version order (there are no context-deletion delta types).
  let creationDeltas = sortBy compareByVersion allDeltas
  for_ creationDeltas \(DeltaStoreRecord { signedDelta: sd }) ->
    applyDelta sd (Just (unwrap sd).encryptedDelta)

-- | Returns true if a deltaType string represents a role-instance deletion.
isDeletionDeltaType :: String -> Boolean
isDeletionDeltaType dt =
  dt == "RemoveRoleInstance"
    || dt == "RemoveExternalRoleInstance"
    || dt == "RemoveUnboundExternalRoleInstance"

-- | Returns true if a DeltaStoreRecord is a sub-resource delta of the given
-- | role instance (i.e. its resourceKey starts with safeKey(roleInstanceId) <> "#").
-- | DeltaStoreRecord.resourceKey is stored as a safe key; the roleInstanceId is
-- | compared after conversion via safeKey to strip any scheme prefix.
isSubResourceDeltaOf :: String -> DeltaStoreRecord -> Boolean
isSubResourceDeltaOf roleInstanceId (DeltaStoreRecord { resourceKey }) =
  let
    safeRid = safeKey roleInstanceId
  in
    Str.length resourceKey > Str.length safeRid + 1
      && Str.take (Str.length safeRid + 1) resourceKey == safeRid <> "#"

-- | Apply a signed delta by dispatching to the appropriate execute function.
-- | Tries to parse the encrypted delta as each known delta type in turn.
-- | Silently fails if the delta cannot be parsed.
applyDelta :: SignedDelta -> Maybe String -> MonadPerspectivesTransaction Unit
applyDelta _ Nothing = pure unit
applyDelta s (Just stringifiedDelta) = do
  padding <- lift transactionLevel
  storageSchemes <- lift $ gets _.typeToStorage
  catchError
    ( case runExcept $ readJSON' stringifiedDelta of
        Right d1 -> lift (addResourceSchemes storageSchemes d1) >>= flip executeRolePropertyDelta s
        Left _ -> case runExcept $ readJSON' stringifiedDelta of
          Right d2 -> lift (addResourceSchemes storageSchemes d2) >>= flip executeRoleBindingDelta s
          Left _ -> case runExcept $ readJSON' stringifiedDelta of
            Right d3 -> lift (addResourceSchemes storageSchemes d3) >>= flip executeContextDelta s
            Left _ -> case runExcept $ readJSON' stringifiedDelta of
              Right d4 -> lift (addResourceSchemes storageSchemes d4) >>= flip executeUniverseRoleDelta s
              Left _ -> case runExcept $ readJSON' stringifiedDelta of
                Right d5 -> lift (addResourceSchemes storageSchemes d5) >>= flip executeUniverseContextDelta s
                -- Fallback: try legacy delta formats.
                Left _ -> case runExcept $ readJSON' stringifiedDelta of
                  Right ld1 -> lift (addResourceSchemes storageSchemes (toRolePropertyDelta ld1)) >>= flip executeRolePropertyDelta s
                  Left _ -> case runExcept $ readJSON' stringifiedDelta of
                    Right ld2 -> lift (addResourceSchemes storageSchemes (toRoleBindingDelta ld2)) >>= flip executeRoleBindingDelta s
                    Left _ -> case runExcept $ readJSON' stringifiedDelta of
                      Right ld3 -> lift (addResourceSchemes storageSchemes (toContextDelta ld3)) >>= flip executeContextDelta s
                      Left _ -> case runExcept $ readJSON' stringifiedDelta of
                        Right ld4 -> lift (addResourceSchemes storageSchemes (toUniverseRoleDelta ld4)) >>= flip executeUniverseRoleDelta s
                        Left _ -> case runExcept $ readJSON' stringifiedDelta of
                          Right ld5 -> lift (addResourceSchemes storageSchemes (toUniverseContextDelta ld5)) >>= flip executeUniverseContextDelta s
                          Left _ -> log (padding <> "Failed to parse delta for resource restoration: " <> stringifiedDelta)
    )
    (\e -> liftEffect $ log (padding <> show e))
