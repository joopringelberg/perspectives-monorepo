-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Test.Logging where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Reader (ask)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, finally)
import Effect.Aff.AVar (AVar)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Main (disableLogTopic, globalStateRef, setLogLevelForTopic)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), PerspectivesState)
import Perspectives.Logging (logActive)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "JavaScript log configuration" do
  test "setting previously unrecognized topics enables informational logging" $
    withGlobalState \state -> for_ [ DELTA, TEST, RESOURCE, STARTUP, ACTION ] \topic -> do
      liftEffect $ setLogLevelForTopic (show topic) "TRACE"
      waitForLevel state topic Trace
      active <- runPerspectivesWithState (logActive topic Info) state
      assert ("INFO logging must be enabled for " <> show topic) active

  test "disabling previously unrecognized topics suppresses logging" $
    withGlobalState \state -> for_ [ DELTA, TEST, RESOURCE, STARTUP, ACTION ] \topic -> do
      liftEffect $ disableLogTopic (show topic)
      waitForLevel state topic Silent
      active <- runPerspectivesWithState (logActive topic Error) state
      assert ("Logging must be disabled for " <> show topic) (not active)

withGlobalState :: (AVar PerspectivesState -> Aff Unit) -> Aff Unit
withGlobalState action = runP do
  state <- ask
  liftAff do
    previous <- liftEffect $ Ref.read globalStateRef
    finally (liftEffect $ Ref.write previous globalStateRef) do
      liftEffect $ Ref.write (Just state) globalStateRef
      action state

-- The JavaScript helpers dispatch configuration changes asynchronously.
waitForLevel :: AVar PerspectivesState -> LogTopic -> LogLevel -> Aff Unit
waitForLevel state topic expected = poll 100
  where
  poll remaining = do
    config <- runPerspectivesWithState (gets _.logConfig) state
    let actual = Map.lookup topic config.topicLevels
    if actual == Just expected || remaining == 0 then equal (Just expected) actual
    else do
      delay $ Milliseconds 1.0
      poll (remaining - 1)
