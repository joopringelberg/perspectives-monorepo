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

module Perspectives.Assignment.RunAction where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CompileAssignment (compileAssignment)
import Perspectives.CompileRoleAssignment (compileAssignmentFromRole)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##>))
import Perspectives.Identifiers (isTypeUri)
import Perspectives.Instances.Me (getMeInRoleAndContext)
import Perspectives.PerspectivesState (addBinding, pushFrame, restoreFrame)
import Perspectives.Representation.Action (Action(..)) as ACTION
import Perspectives.Instances.ObjectGetters (roleType_)
import Perspectives.Representation.Class.Role (getRoleType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.TypeIdentifiers (RoleType(..))
import Perspectives.Types.ObjectGetters (findPerspective, findPerspectiveForObject, getAction, getContextAction, getContextActionFromUnqualifiedName)
import Partial.Unsafe (unsafePartial)

-- | Execute a context action on behalf of a user role type in a context instance.
-- | Parameters:
-- |   user       – String identifying the user role type
-- |   actionName – String identifying the action (qualified or unqualified)
-- |   context    – String identifying the context instance
-- | Throws an error when the action or the user role instance cannot be found.
runContextAction :: String -> String -> String -> MonadPerspectivesTransaction Unit
runContextAction user actionName context = do
  userRoleType <- lift $ getRoleType user
  maction <- lift $
    if isTypeUri actionName then getContextAction actionName userRoleType
    else getContextActionFromUnqualifiedName actionName userRoleType
  muserRoleInstance <- lift ((ContextInstance context) ##> getMeInRoleAndContext userRoleType)
  case muserRoleInstance, maction of
    Just userInstance, Just (ACTION.Action { qfd: action }) -> do
      oldFrame <- lift $ pushFrame
      lift $ addBinding "currentcontext" [ context ]
      lift $ addBinding "currentactor" [ unwrap userInstance ]
      updater <- lift $ compileAssignment action
      updater (ContextInstance context)
      lift $ restoreFrame oldFrame
    _, _ -> throwError $ error
      $ "cannot identify Action with role type '" <> show userRoleType
          <> "' and action name '"
          <> actionName
          <> "'."

-- | Execute a perspective action on behalf of an authoring role in a context instance.
-- | Parameters:
-- |   authoringRole – RoleType of the user role executing the action
-- |   perspectiveId – String identifying the perspective
-- |   actionName    – String identifying the action within the perspective
-- |   context       – String identifying the context instance (= 'object' in Perspectives.Api)
-- |   object        – String identifying the role instance that is the perspective object (= 'predicate' in Perspectives.Api)
-- | Throws an error when the action or the authoring role instance cannot be found.
runAction :: RoleType -> String -> String -> String -> String -> MonadPerspectivesTransaction Unit
runAction authoringRole perspectiveId actionName context object = do
  maction <- lift $ (map $ getAction actionName) <$> (findPerspective authoringRole (\(Perspective { id }) -> pure $ id == perspectiveId))
  mauthoringRoleInstance <- lift ((ContextInstance context) ##> getMeInRoleAndContext authoringRole)
  case mauthoringRoleInstance, maction of
    Just author, Just (Just (ACTION.Action { qfd: action })) -> do
      oldFrame <- lift $ pushFrame
      lift $ addBinding "currentcontext" [ context ]
      lift $ addBinding "currentactor" [ unwrap author ]
      updater <- lift $ compileAssignmentFromRole action
      updater (RoleInstance object)
      lift $ restoreFrame oldFrame
    _, _ -> throwError $ error
      $ "cannot identify Action with role type '" <> show authoringRole
          <> "', perspectiveId '"
          <> perspectiveId
          <> "' and action name '"
          <> actionName
          <> "'."

-- | Execute a perspective action on behalf of an authoring role in a context instance,
-- | finding the perspective by matching the object role type instead of by perspective id.
-- | Parameters:
-- |   authoringRole – RoleType of the user role executing the action
-- |   actionName    – String identifying the action within the perspective
-- |   context       – String identifying the context instance (= 'object' in Perspectives.Api)
-- |   object        – String identifying the role instance that is the perspective object (= 'predicate' in Perspectives.Api)
-- | Throws an error when the perspective, action, or authoring role instance cannot be found.
-- | PARTIAL: can only be used after perspective objects have been compiled in PhaseThree.
runActionForObject :: RoleType -> String -> String -> String -> MonadPerspectivesTransaction Unit
runActionForObject authoringRole actionName context object = do
  objectRoleType <- lift $ roleType_ (RoleInstance object)
  mperspective <- lift $ unsafePartial $ findPerspectiveForObject authoringRole (ENR objectRoleType)
  maction <- pure $ mperspective >>= getAction actionName
  mauthoringRoleInstance <- lift ((ContextInstance context) ##> getMeInRoleAndContext authoringRole)
  case mauthoringRoleInstance, maction of
    Just author, Just (ACTION.Action { qfd: action }) -> do
      oldFrame <- lift $ pushFrame
      lift $ addBinding "currentcontext" [ context ]
      lift $ addBinding "currentactor" [ unwrap author ]
      updater <- lift $ compileAssignmentFromRole action
      updater (RoleInstance object)
      lift $ restoreFrame oldFrame
    _, _ -> throwError $ error
      $ "cannot identify Action with role type '" <> show authoringRole
          <> "' and action name '"
          <> actionName
          <> "' for object '"
          <> object
          <> "'."
