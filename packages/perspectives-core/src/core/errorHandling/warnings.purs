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
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, RoleType, StateIdentifier, roletype2string)
import Perspectives.TypesForDeltas (RolePropertyDeltaType, UniverseContextDeltaType, UniverseRoleDeltaType)

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
  | ExecutingRolePropertyDelta RolePropertyDeltaType RoleInstance EnumeratedPropertyType
  | ExecutingUniverseContextDelta UniverseContextDeltaType ContextInstance ContextType
  | ExecutingUniverseRoleDelta UniverseRoleDeltaType ContextInstance RoleInstance EnumeratedRoleType RoleType
  | ConstructingExternalRole String
  -- Transaction version-management messages
  | TransactionBlockedByVersionGaps
  | SkippingOutdatedDelta String String Int Int
  | VersionConflictIncomingWins String String Int String
  | VersionConflictIncomingLoses String String Int String
  | ModifyWinsOverDeleteSuppressed String
  | ModifyWinsOverDeleteRestoring String
  -- User interaction
  | MissingResource String String String
  | ConstructContext ContextType (Maybe RoleType) ContextInstance
  -- Resource creation
  | FilledRoleInstance RoleInstance EnumeratedRoleType RoleInstance EnumeratedRoleType
  -- State transitions
  | ContextStateNotValid ContextInstance StateIdentifier
  | AlreadyInContextState ContextInstance StateIdentifier
  | RoleStateNotValid RoleInstance StateIdentifier
  | AlreadyInRoleState RoleInstance StateIdentifier
  | NoContextToBindIn EnumeratedRoleType ContextInstance
  | NoBindings EnumeratedRoleType ContextInstance
  | NoBinding ContextInstance
  | NoBinder ContextInstance
  | NoRoleTypesToCreate EnumeratedRoleType ContextInstance RoleType
  | CannotConstructMinimalSelfPerspective ContextType RoleType
  | ConstructedMinimalSelfPerspective ContextType RoleType

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
  show (ExecutingRolePropertyDelta deltaType roleId property) = "(ExecutingRolePropertyDelta) " <> show deltaType <> " for " <> show roleId <> " and property " <> show property
  show (ExecutingUniverseContextDelta deltaType contextId contextType) = "(ExecutingUniverseContextDelta) " <> show deltaType <> " with id " <> show contextId <> " and with type " <> show contextType
  show (ExecutingUniverseRoleDelta deltaType contextId roleInstance roleType subject) = "(ExecutingUniverseRoleDelta) " <> show deltaType <> " for/from " <> show contextId <> " with id " <> show roleInstance <> " with type " <> show roleType <> " for user role " <> show subject
  show (ConstructingExternalRole contextId) = "(ConstructingExternalRole) ConstructExternalRole in " <> contextId
  show TransactionBlockedByVersionGaps = "(TransactionBlockedByVersionGaps) Transaction blocked: version gaps detected. Storing as pending."
  show (SkippingOutdatedDelta resourceKey deltaType resourceVersion localVersion) = "(SkippingOutdatedDelta) Skipping outdated delta for " <> resourceKey <> " (" <> deltaType <> ") (incoming version " <> show resourceVersion <> " < local " <> show localVersion <> ")"
  show (VersionConflictIncomingWins resourceKey deltaType resourceVersion author) = "(VersionConflictIncomingWins) Version conflict for " <> resourceKey <> " (" <> deltaType <> ") at version " <> show resourceVersion <> ": incoming author " <> author <> " wins."
  show (VersionConflictIncomingLoses resourceKey deltaType resourceVersion author) = "(VersionConflictIncomingLoses) Version conflict for " <> resourceKey <> " (" <> deltaType <> ") at version " <> show resourceVersion <> ": incoming author " <> author <> " loses."
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
  show (ConstructContext contextType mAuthorizedRole contextInstance) =
    "(ConstructContext) Constructing context of type " <> show contextType
      <> " with id "
      <> show contextInstance
      <> case mAuthorizedRole of
        Just authorizedRole -> " in authorized role " <> show authorizedRole
        Nothing -> ""
  show (FilledRoleInstance filled filledType filler fillerType) =
    "(FilledRoleInstance) Filled role instance " <> show filled <> " of type " <> " " <> show filledType <> " by filler " <> show filler <> " of type " <> " " <> show fillerType
  show (ContextStateNotValid contextInstance stateId) =
    "(ContextStateNotValid) Context " <> show contextInstance <> " is not in state " <> show stateId
  show (AlreadyInContextState contextInstance stateId) =
    "(AlreadyInContextState) Context " <> show contextInstance <> " is already in state " <> show stateId
  show (RoleStateNotValid roleInstance stateId) =
    "(RoleStateNotValid) Role " <> show roleInstance <> " is not in state " <> show stateId
  show (AlreadyInRoleState roleInstance stateId) =
    "(AlreadyInRoleState) Role " <> show roleInstance <> " is already in state " <> show stateId
  show (NoContextToBindIn qualifiedRoleIdentifier contextId) =
    "(NoContextToBindIn) No context to create and fill a role " <> show qualifiedRoleIdentifier <> " in context " <> show contextId
  show (NoBindings qualifiedRoleIdentifier contextId) =
    "(NoBindings) No role instances found to create and fill role of type " <> show qualifiedRoleIdentifier <> " in context " <> show contextId
  show (NoBinding contextId) =
    "(NoBinding) No filler found in context " <> show contextId
  show (NoBinder contextId) =
    "(NoBinder) No role instance found to fill in context " <> show contextId
  show (NoRoleTypesToCreate qualifiedRoleIdentifier ctxt roleType) =
    "(NoRoleTypesToCreate) User " <> show roleType <> " has no perspective on role " <> show qualifiedRoleIdentifier <> " or its specialisations in context " <> show ctxt
  show (CannotConstructMinimalSelfPerspective contextType roleType) =
    "(CannotConstructMinimalSelfPerspective) Cannot construct a minimal self-perspective for context type " <> show contextType <> " and role type " <> show roleType
  show (ConstructedMinimalSelfPerspective contextType roleType) =
    "(ConstructedMinimalSelfPerspective) Constructed a minimal self-perspective for context type " <> show contextType <> " and role type " <> show roleType
