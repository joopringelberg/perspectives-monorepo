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
import Data.Traversable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Perspectives.Assignment.RunAction (runActionForObject, runContextAction)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (##>))
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Instances.Me (getMeInRoleAndContext)
import Perspectives.Representation.Class.Role (getRoleType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.RunMonadPerspectivesTransaction (runEmbeddedIfNecessaryAwaitingSettlement, shareWithPeers)
import Unsafe.Coerce (unsafeCoerce)

-- | Runs the named context action in each context, for the local user's instance of the
-- | given (qualified) user role type. Each context action runs as a depth-first embedded
-- | transaction and is fully settled before the next context is processed. Contexts without
-- | a local instance of the user role ("filled by me") are skipped.
runContextActionEffect :: Array String -> Array String -> Array String -> ContextInstance -> MonadPerspectivesTransaction Unit
runContextActionEffect actionNameArr userRoleTypeArr contexts _ = case head actionNameArr, head userRoleTypeArr of
  Just actionName, Just userRoleTypeString -> do
    userRoleType <- lift $ getRoleType userRoleTypeString
    traverse_ (runForContext actionName userRoleTypeString userRoleType) contexts
  _, _ -> pure unit
  where
  runForContext actionName userRoleTypeString userRoleType context = do
    let contextId = ContextInstance context
    muserRoleInstance <- lift (contextId ##> getMeInRoleAndContext userRoleType)
    case muserRoleInstance of
      Nothing -> pure unit
      Just _ -> do
        _ <- lift $ runEmbeddedIfNecessaryAwaitingSettlement shareWithPeers userRoleType
          (runContextAction userRoleTypeString actionName context)
        pure unit

-- | Runs the named perspective action (identified by each object role instance), for the
-- | local user's instance of the given (qualified) user role type in the context.
-- | Each object is handled as a depth-first embedded transaction that is fully settled
-- | before the next object is processed.
runRoleActionEffect :: Array String -> Array String -> Array String -> ContextInstance -> MonadPerspectivesTransaction Unit
runRoleActionEffect actionNameArr userRoleTypeArr objectArr contextId = case head actionNameArr, head userRoleTypeArr of
  Just actionName, Just userRoleTypeString -> do
    userRoleType <- lift $ getRoleType userRoleTypeString
    muserRoleInstance <- lift (contextId ##> getMeInRoleAndContext userRoleType)
    case muserRoleInstance of
      Nothing -> pure unit
      Just _ -> do
        traverse_
          ( \objectId -> do
              _ <- lift $ runEmbeddedIfNecessaryAwaitingSettlement shareWithPeers userRoleType
                (runActionForObject userRoleType actionName (unwrap contextId) objectId)
              pure unit
          )
          objectArr
        pure unit
  _, _ -> pure unit

externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "RunContextActionEffect" { func: unsafeCoerce runContextActionEffect, nArgs: 3, isFunctional: True, isEffect: true }
  , Tuple "RunRoleActionEffect" { func: unsafeCoerce runRoleActionEffect, nArgs: 3, isFunctional: True, isEffect: true }
  ]
