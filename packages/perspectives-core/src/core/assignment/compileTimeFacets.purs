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
module Perspectives.CompileTimeFacets where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (mapMaybe, nub)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.AVar (AVar)
import Effect.Aff (Fiber)
import Effect.Aff.AVar (put)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (CapturedBindings, MP, MonadPerspectivesTransaction, PendingSettledStack, RepeatingTransaction(..), Updater, appendPendingSettled)
import Perspectives.PerspectivesState (addBinding, lookupVariableBinding, pushFrame, restoreFrame)
import Perspectives.Repetition (Repeater(..))
import Perspectives.Representation.Action (StartMoment(..), TimeFacets)
import Perspectives.Representation.TypeIdentifiers (RoleType, StateIdentifier)
import Unsafe.Coerce (unsafeCoerce)

captureBindings :: Array String -> MP CapturedBindings
captureBindings names = mapMaybe identity <$> traverse capture names
  where
  capture name = do
    mvalues <- lookupVariableBinding name
    pure $ Tuple name <$> mvalues

-- | The variables bound by the state- and action machinery before an effect runs.
-- | Variable bindings are private to a fiber, so effects that run later (in another fiber) must
-- | capture these from the fiber that schedules them.
contextVariableNames :: Array String
contextVariableNames = [ "currentcontext", "currentactor", "notifieduser" ]

-- | Run the transaction in a new frame holding the captured bindings.
withCapturedBindings :: forall a. CapturedBindings -> MonadPerspectivesTransaction a -> MonadPerspectivesTransaction a
withCapturedBindings [] transaction = transaction
withCapturedBindings capturedBindings transaction = do
  oldFrame <- lift pushFrame
  for_ capturedBindings \(Tuple name values) -> lift $ addBinding name values
  catchError
    ( do
        r <- transaction
        lift $ restoreFrame oldFrame
        pure r
    )
    \e -> do
      lift $ restoreFrame oldFrame
      throwError e

-- | Instead of dispatching to `transactionWithTiming` right away, appends to the top frame of the
-- | current `PendingSettledStack`. It is only handed over to `transactionWithTiming` once the enclosing
-- | `runMonadPerspectivesTransaction'` call has itself fully finished (see runMonadPerspectivesTransaction.purs).
scheduleSettledTransaction :: forall a. Updater a -> RoleType -> Maybe StateIdentifier -> Array String -> Updater a
scheduleSettledTransaction transaction authoringRole stateId capturedBindingNames a = do
  (stack :: PendingSettledStack) <- lift (gets _.pendingSettledTransactions :: MP PendingSettledStack)
  capturedBindings <- lift $ captureBindings (nub (contextVariableNames <> capturedBindingNames))
  liftEffect $ appendPendingSettled stack
    ( SettledTransaction
        { transaction: transaction a
        , instanceId: unsafeUnwrapResource a
        , stateId
        , authoringRole
        , capturedBindings
        }
    )

addTimeFacets :: forall a f. Partial => Updater a -> TimeFacets f -> RoleType -> StateIdentifier -> MP (Updater a)
addTimeFacets updater { startMoment, endMoment, repeats } authoringRole stateId = do
  pure $ repeat repeats updater
  where
  repeat :: Repeater -> Updater a -> Updater a
  repeat Never u = case startMoment of
    Immediately -> u
    After s -> \a -> do
      (av :: AVar RepeatingTransaction) <- lift (gets _.transactionWithTiming :: MP (AVar RepeatingTransaction))
      capturedBindings <- lift $ captureBindings contextVariableNames
      liftAff $ put
        ( PostponedTransaction
            { transaction: withCapturedBindings capturedBindings (u a)
            , instanceId: unsafeUnwrapResource a
            , stateId
            , authoringRole
            , startMoment: s
            }
        )
        av
    OnceSettled -> scheduleSettledTransaction u authoringRole (Just stateId) []
  repeat (Forever duration) u = \a -> do
    (av :: AVar RepeatingTransaction) <- lift (gets _.transactionWithTiming :: MP (AVar RepeatingTransaction))
    capturedBindings <- lift $ captureBindings contextVariableNames
    liftAff $ put
      ( TransactionWithTiming
          { transaction: withCapturedBindings capturedBindings (u a)
          , interval: duration
          , instanceId: unsafeUnwrapResource a
          , stateId
          , authoringRole
          , startMoment
          , endMoment
          }
      )
      av
  repeat (RepeatFor nrOfTimes duration) u = \a -> do
    (av :: AVar RepeatingTransaction) <- lift (gets _.transactionWithTiming :: MP (AVar RepeatingTransaction))
    capturedBindings <- lift $ captureBindings contextVariableNames
    liftAff $ put
      ( RepeatNtimes
          { transaction: withCapturedBindings capturedBindings (u a)
          , interval: duration
          , nrOfTimes
          , instanceId: unsafeUnwrapResource a
          , stateId
          , authoringRole
          , startMoment
          , endMoment
          }
      )
      av

  returnFiber :: Fiber Unit -> Unit
  returnFiber f = unit

unsafeUnwrapResource :: forall a. a -> String
unsafeUnwrapResource = unsafeCoerce
