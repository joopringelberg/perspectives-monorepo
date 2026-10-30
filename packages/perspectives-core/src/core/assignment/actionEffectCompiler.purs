-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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
module Perspectives.CompileActionEffect where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter, uncons)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Effect.Exception (error)
import Perspectives.CompileTimeFacets (scheduleSettledTransaction)
import Perspectives.CoreTypes (MP, Updater)
import Perspectives.PerspectivesState (pushFrame, restoreFrame)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Query.UnsafeCompiler (typeTimeOnly)
import Perspectives.Representation.Action (ActionEffect(..))
import Perspectives.Representation.TypeIdentifiers (RoleType, StateIdentifier)

compileActionEffectWith
  :: forall a
   . (QueryFunctionDescription -> MP (Updater a))
  -> ActionEffect
  -> RoleType
  -> Maybe StateIdentifier
  -> MP (Updater a)
compileActionEffectWith compileStage (ActionEffect { bindings, stages, capturedBindingNames }) authoringRole mstateId = do
  bindingUpdaters <- traverse compileStage (filter (not <<< typeTimeOnly) bindings)
  stageUpdaters <- traverse compileStage stages
  case uncons stageUpdaters of
    Nothing -> throwError (error "Cannot compile an action effect without stages.")
    Just { head, tail } -> pure \resource ->
      if bindings == [] then runStageAndSchedule head tail resource
      else do
        oldFrame <- lift pushFrame
        catchError
          ( do
              traverse_ (_ $ resource) bindingUpdaters
              runStageAndSchedule head tail resource
              lift $ restoreFrame oldFrame
          )
          \e -> do
            lift $ restoreFrame oldFrame
            throwError e
  where
  runStageAndSchedule head tail resource = do
    head resource
    case tail of
      [] -> pure unit
      _ -> scheduleSettledTransaction (runStages tail) authoringRole mstateId capturedBindingNames resource

  runStages :: Array (Updater a) -> Updater a
  runStages updaters resource = case uncons updaters of
    Nothing -> pure unit
    Just { head, tail } -> runStageAndSchedule head tail resource