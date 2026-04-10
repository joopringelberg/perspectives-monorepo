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

module Perspectives.Warning where

import Prelude

import Data.Foldable (intercalate)
import Data.Newtype (unwrap)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, RoleType, StateIdentifier, roletype2string)

data PerspectivesWarning
  = ModelLacksModelId String
  | ModelLacksUrl String
  | PropertySynchronizationIncomplete EnumeratedPropertyType RoleType (Array RoleType)
  | RoleSynchronizationIncomplete EnumeratedRoleType RoleType (Array RoleType)
  | RoleBindingSynchronizationIncomplete EnumeratedRoleType RoleType (Array RoleType)
  | NotificationError StateIdentifier
  | AutomaticActionError StateIdentifier
  | ExternalFunctionError String
  | NoTranslations String
  | NonCriticalError
  -- Delta execution informational messages
  | ExecutingContextDelta String String String
  | ExecutingRoleBindingDelta String String String
  | ExecutingRolePropertyDelta String String String
  | ExecutingUniverseContextDelta String String String
  | ExecutingUniverseRoleDelta String String String String String
  | ConstructingExternalRole String
  -- Transaction version-management messages
  | TransactionBlockedByVersionGaps
  | SkippingOutdatedDelta String Int Int
  | VersionConflictIncomingWins String Int String
  | VersionConflictIncomingLoses String Int String
  | ModifyWinsOverDeleteSuppressed String
  | ModifyWinsOverDeleteRestoring String
  -- User interaction
  | MissingResource String String String

instance showPerspectivesWarning :: Show PerspectivesWarning where
  show (ModelLacksModelId dfid) = "(ModelLacksModelId) The model '" <> dfid <> "' lacks a value for the property ModelIdentification on its Model instance."
  show (ModelLacksUrl dfid) = "(ModelLacksUrl) The model '" <> dfid <> "' lacks a value for the property Url on its Model instance."
  show (PropertySynchronizationIncomplete prop source destinations) = "(PropertySynchronizationIncomplete) Modifications to property:\n\t'" <> (unwrap prop) <> "'\n by:\n\t'" <> roletype2string source <> "'\n cannot be sent to:\n\t* " <> intercalate "\n\t* " (map roletype2string destinations) <> "."
  show (RoleSynchronizationIncomplete role source destinations) = "(RoleSynchronizationIncomplete) New (and removed) instances of role type:\n\t'" <> (unwrap role) <> "'\n by:\n\t'" <> roletype2string source <> "'\n cannot be sent to:\n\t* " <> intercalate "\n\t* " (map roletype2string destinations) <> "."
  show (RoleBindingSynchronizationIncomplete role source destinations) = "(RoleBindingSynchronizationIncomplete) Filling (and emptying) instances of role type:\n\t'" <> (unwrap role) <> "'\n by:\n\t'" <> roletype2string source <> "'\n cannot be communicated with:\n\t* " <> intercalate "\n\t* " (map roletype2string destinations) <> "."
  show (NotificationError stateId) = "(NotificationError) Error on notifying in state " <> unwrap stateId
  show (AutomaticActionError stateId) = "(AutomaticActionError) Error on executing automatic action in state " <> unwrap stateId
  show (ExternalFunctionError fname) = "(ExternalFunctionError) External library function '" <> fname <> "' resulted in an error."
  show (NoTranslations domain) = "(NoTranslations) No translations found for domain '" <> domain <> "'."
  show (NonCriticalError) = "(NonCriticalError) An error occurred, but it was caught and handled, so the system can continue running. Check the error message for details."
  show (ExecutingContextDelta deltaType contextInstance roleInstance) = "(ExecutingContextDelta) " <> deltaType <> " to/from " <> contextInstance <> " and " <> roleInstance
  show (ExecutingRoleBindingDelta deltaType filled filler) = "(ExecutingRoleBindingDelta) " <> deltaType <> " of " <> filled <> " (to) " <> filler
  show (ExecutingRolePropertyDelta deltaType roleId property) = "(ExecutingRolePropertyDelta) " <> deltaType <> " for " <> roleId <> " and property " <> property
  show (ExecutingUniverseContextDelta deltaType contextId contextType) = "(ExecutingUniverseContextDelta) " <> deltaType <> " with id " <> contextId <> " and with type " <> contextType
  show (ExecutingUniverseRoleDelta deltaType contextId roleInstance roleType subject) = "(ExecutingUniverseRoleDelta) " <> deltaType <> " for/from " <> contextId <> " with id " <> roleInstance <> " with type " <> roleType <> " for user role " <> subject
  show (ConstructingExternalRole contextId) = "(ConstructingExternalRole) ConstructExternalRole in " <> contextId
  show TransactionBlockedByVersionGaps = "(TransactionBlockedByVersionGaps) Transaction blocked: version gaps detected. Storing as pending."
  show (SkippingOutdatedDelta resourceKey resourceVersion localVersion) = "(SkippingOutdatedDelta) Skipping outdated delta for " <> resourceKey <> " (version " <> show resourceVersion <> " < local " <> show localVersion <> ")"
  show (VersionConflictIncomingWins resourceKey resourceVersion author) = "(VersionConflictIncomingWins) Version conflict for " <> resourceKey <> " at version " <> show resourceVersion <> ": incoming author " <> author <> " wins."
  show (VersionConflictIncomingLoses resourceKey resourceVersion author) = "(VersionConflictIncomingLoses) Version conflict for " <> resourceKey <> " at version " <> show resourceVersion <> ": incoming author " <> author <> " loses."
  show (ModifyWinsOverDeleteSuppressed resourceKey) = "(ModifyWinsOverDeleteSuppressed) Modify-wins-over-delete: suppressing deletion of " <> resourceKey <> " because concurrent sub-resource modifications exist."
  show (ModifyWinsOverDeleteRestoring roleInstanceId) = "(ModifyWinsOverDeleteRestoring) Modify-wins-over-delete: restoring role " <> roleInstanceId <> " to apply incoming modification."
  show (MissingResource resourceKind instanceDisplay typeName) =
    "De " <> resourceKind <> " " <> instanceDisplay
      <> ", een "
      <> typeName
      <> " is niet langer beschikbaar, maar er wordt nog wel naar verwezen."
      <> " Dat kan vanuit een andere rol of context zijn, maar ook vanuit het klembord of de vastgeprikte contexten."
      <> " Wil je deze "
      <> resourceKind
      <> " definitief verwijderen of juist herstellen?"