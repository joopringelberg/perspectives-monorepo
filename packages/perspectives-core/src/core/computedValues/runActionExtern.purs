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

-- | Runtime implementations of the ARC `runContextAction` and `runRoleAction` statements.
-- | Registered as hidden effect functions (looked up dynamically by name, see
-- | Perspectives.Query.StatementCompiler) so that the low-level statement compiler, which
-- | constructs calls to them, does not need to statically depend on the transaction
-- | embedding machinery (avoiding a module cycle through Perspectives.CompileAssignment).

module Perspectives.Extern.RunAction where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Perspectives.Assignment.RunAction (runActionForObject, runContextAction)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##>))
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Instances.Me (getMeInRoleAndContext)
import Perspectives.Representation.Class.Role (getRoleType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.RunMonadPerspectivesTransaction (runEmbeddedIfNecessaryAwaitingSettlement, shareWithPeers)
import Unsafe.Coerce (unsafeCoerce)

-- | Runs the named context action, for the local user's instance of the given (qualified)
-- | user role type, as a depth-first embedded transaction that is fully settled - including
-- | every `once settled` stage it triggers - before this function returns. Does nothing if
-- | no local instance of the user role exists in the context ("filled by me" is the
-- | authorization boundary for this statement).
runContextActionEffect :: Array String -> Array String -> ContextInstance -> MonadPerspectivesTransaction Unit
runContextActionEffect actionNameArr userRoleTypeArr contextId = case head actionNameArr, head userRoleTypeArr of
  Just actionName, Just userRoleTypeString -> do
    userRoleType <- lift $ getRoleType userRoleTypeString
    muserRoleInstance <- lift (contextId ##> getMeInRoleAndContext userRoleType)
    case muserRoleInstance of
      Nothing -> pure unit
      Just _ -> do
        _ <- lift $ runEmbeddedIfNecessaryAwaitingSettlement shareWithPeers userRoleType
          (runContextAction userRoleTypeString actionName (unwrap contextId))
        pure unit
  _, _ -> pure unit

-- | Runs the named perspective action (identified by its object role instance), for the
-- | local user's instance of the given (qualified) user role type, as a depth-first
-- | embedded transaction that is fully settled before this function returns. Does nothing
-- | if no local instance of the user role exists in the context.
runRoleActionEffect :: Array String -> Array String -> Array String -> ContextInstance -> MonadPerspectivesTransaction Unit
runRoleActionEffect actionNameArr userRoleTypeArr objectArr contextId = case head actionNameArr, head userRoleTypeArr, head objectArr of
  Just actionName, Just userRoleTypeString, Just objectId -> do
    userRoleType <- lift $ getRoleType userRoleTypeString
    muserRoleInstance <- lift (contextId ##> getMeInRoleAndContext userRoleType)
    case muserRoleInstance of
      Nothing -> pure unit
      Just _ -> do
        _ <- lift $ runEmbeddedIfNecessaryAwaitingSettlement shareWithPeers userRoleType
          (runActionForObject userRoleType actionName (unwrap contextId) objectId)
        pure unit
  _, _, _ -> pure unit

externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "RunContextActionEffect" { func: unsafeCoerce runContextActionEffect, nArgs: 2, isFunctional: True, isEffect: true }
  , Tuple "RunRoleActionEffect" { func: unsafeCoerce runRoleActionEffect, nArgs: 3, isFunctional: True, isEffect: true }
  ]
