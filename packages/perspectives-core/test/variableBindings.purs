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
-- Full text of this license can be found in the LICENSE directory in the
-- projects root.

-- END LICENSE

-- | Variable bindings must be private to a fiber: concurrently running fibers that share the
-- | PerspectivesState must not see, overwrite or restore each other's bindings.
module Test.VariableBindings where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (delay, forkAff, joinFiber)
import Effect.Aff.Class (liftAff)
import Perspectives.PerspectivesState (addBinding, lookupVariableBinding, pushFrame, restoreFrame, withFrame)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (equal)

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.PerspectivesState variable bindings" do

  test "a frame restored in one fiber does not remove a binding made in another fiber" do
    Tuple a b <- runP do
      state <- ask
      liftAff do
        fa <- forkAff $ runPerspectivesWithState
          ( do
              liftAff $ delay (Milliseconds 5.0)
              addBinding "x" [ "a" ]
              liftAff $ delay (Milliseconds 30.0)
              lookupVariableBinding "x"
          )
          state
        fb <- forkAff $ runPerspectivesWithState
          ( do
              old <- pushFrame
              liftAff $ delay (Milliseconds 20.0)
              restoreFrame old
              lookupVariableBinding "x"
          )
          state
        Tuple <$> joinFiber fa <*> joinFiber fb
    equal (Just [ "a" ]) a
    equal Nothing b

  test "a binding made in a frame in one fiber is not visible in another fiber" do
    Tuple a b <- runP do
      state <- ask
      liftAff do
        fa <- forkAff $ runPerspectivesWithState
          ( withFrame do
              addBinding "y" [ "a" ]
              liftAff $ delay (Milliseconds 20.0)
              lookupVariableBinding "y"
          )
          state
        fb <- forkAff $ runPerspectivesWithState
          ( do
              liftAff $ delay (Milliseconds 10.0)
              lookupVariableBinding "y"
          )
          state
        Tuple <$> joinFiber fa <*> joinFiber fb
    equal (Just [ "a" ]) a
    equal Nothing b

  test "bindings survive within a fiber across frames" do
    r <- runP do
      addBinding "z" [ "outer" ]
      inner <- withFrame do
        addBinding "z" [ "inner" ]
        lookupVariableBinding "z"
      outer <- lookupVariableBinding "z"
      pure (Tuple inner outer)
    equal (Tuple (Just [ "inner" ]) (Just [ "outer" ])) r
