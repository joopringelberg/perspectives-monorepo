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

-- | Structured logging for the PDR.
-- |
-- | Use `pdrLog` as the single entry point:
-- |
-- | ```purescript
-- | pdrLog "SYNC" Info "Transaction sent"
-- | ```
-- |
-- | Or use the pre-bound convenience functions such as `infoSync`, `warnBroker`, etc.
-- |
-- | Messages are only emitted when the message level is ≥ the configured
-- | threshold for the topic (falling back to `defaultLevel` when the topic has
-- | no specific override).  The configuration lives in `PerspectivesState` and
-- | can be changed at runtime without restarting.

module Perspectives.Logging
  ( ansiBlack
  , ansiBlue
  , ansiCyan
  , ansiGreen
  , ansiMagenta
  , ansiRed
  , ansiReset
  , ansiWhite
  , ansiYellow
  , debugBroker
  , debugCompiler
  , debugDelta
  , debugInstall
  , debugModel
  , debugPersistence
  , debugQuery
  , debugResource
  , debugState
  , debugSync
  , debugTest
  , debugUpgrade
  , errorBroker
  , errorCompiler
  , errorDelta
  , errorInstall
  , errorModel
  , errorOther
  , errorParser
  , errorPersistence
  , errorResource
  , errorSync
  , errorTest
  , errorUpgrade
  , infoBroker
  , infoDelta
  , infoInstall
  , infoModel
  , infoResource
  , infoSync
  , infoTest
  , infoUpgrade
  , logActive
  , logWhen
  , noColor
  , pdrLog
  , traceBroker
  , traceDelta
  , traceInstall
  , traceModel
  , tracePersistence
  , traceQuery
  , traceResource
  , traceState
  , traceSync
  , traceTest
  , warnAuth
  , warnBroker
  , warnDelta
  , warnModel
  , warnOther
  , warnPersistence
  , warnResource
  , warnState
  , warnSync
  , warnTest
  ) where

import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (liftEffect)
import Effect.Class.Console (log) as Console
import Perspectives.CoreTypes (class MonadPerspectivesWithState, LogLevel(..), LogTopic(..), PerspectivesExtraState, getPS)
import Prelude (Unit, bind, show, when, ($), (>=), (<>), pure)

-- | Emit a log message for the given topic at the given level.
-- | The message is only written to the console when `level >= threshold`,
-- | where `threshold` is the per-topic override from `logConfig.topicLevels`,
-- | or `logConfig.defaultLevel` when no override is present.
pdrLog :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => LogTopic -> LogLevel -> String -> m Unit
pdrLog topic level message = do
  { defaultLevel, topicLevels } <- getPS _.logConfig
  logColor <- getPS _.logColor
  let threshold = fromMaybe defaultLevel (Map.lookup topic topicLevels)
  when (level >= threshold) do
    let prefix = "[" <> show level <> "] [" <> show topic <> "] "
    let completeMessage = prefix <> message
    let
      styledMessage = case logColor of
        Just color -> color <> completeMessage <> ansiReset
        Nothing -> completeMessage
    liftEffect $ Console.log styledMessage

-- | Check whether logging is active for the given topic and level.
logActive :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => LogTopic -> LogLevel -> m Boolean
logActive topic level = do
  { defaultLevel, topicLevels } <- getPS _.logConfig
  let threshold = fromMaybe defaultLevel (Map.lookup topic topicLevels)
  pure (level >= threshold)

logWhen :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => LogLevel -> LogTopic -> m String -> m Unit
logWhen level topic message = do
  active <- logActive topic level
  when active do
    msg <- message
    pdrLog topic level msg

-- | ANSI reset code.
ansiReset :: String
ansiReset = "\x1b[0m"

-- | ANSI foreground color codes.
ansiBlack :: String
ansiBlack = "\x1b[30m"

ansiRed :: String
ansiRed = "\x1b[31m"

ansiGreen :: String
ansiGreen = "\x1b[32m"

ansiYellow :: String
ansiYellow = "\x1b[33m"

ansiBlue :: String
ansiBlue = "\x1b[34m"

ansiMagenta :: String
ansiMagenta = "\x1b[35m"

ansiCyan :: String
ansiCyan = "\x1b[36m"

ansiWhite :: String
ansiWhite = "\x1b[37m"

noColor :: Maybe String
noColor = Nothing

-----------------------------------------------------------
-- SYNC
-----------------------------------------------------------
traceSync :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
traceSync = pdrLog SYNC Trace

debugSync :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugSync = pdrLog SYNC Debug

infoSync :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
infoSync = pdrLog SYNC Info

warnSync :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
warnSync = pdrLog SYNC Warn

errorSync :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorSync = pdrLog SYNC Error

-----------------------------------------------------------
-- BROKER
-----------------------------------------------------------
traceBroker :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
traceBroker = pdrLog BROKER Trace

debugBroker :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugBroker = pdrLog BROKER Debug

infoBroker :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
infoBroker = pdrLog BROKER Info

warnBroker :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
warnBroker = pdrLog BROKER Warn

errorBroker :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorBroker = pdrLog BROKER Error

-----------------------------------------------------------
-- QUERY
-----------------------------------------------------------
traceQuery :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
traceQuery = pdrLog QUERY Trace

debugQuery :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugQuery = pdrLog QUERY Debug

-----------------------------------------------------------
-- PERSISTENCE
-----------------------------------------------------------
tracePersistence :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
tracePersistence = pdrLog PERSISTENCE Trace

debugPersistence :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugPersistence = pdrLog PERSISTENCE Debug

errorPersistence :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorPersistence = pdrLog PERSISTENCE Error

-----------------------------------------------------------
-- STATE
-----------------------------------------------------------
traceState :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
traceState = pdrLog STATE Trace

debugState :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugState = pdrLog STATE Debug

-----------------------------------------------------------
-- AUTH
-----------------------------------------------------------
warnAuth :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
warnAuth = pdrLog AUTH Warn

-----------------------------------------------------------
-- MODEL
-----------------------------------------------------------
traceModel :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
traceModel = pdrLog MODEL Trace

debugModel :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugModel = pdrLog MODEL Debug

warnModel :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
warnModel = pdrLog MODEL Warn

infoModel :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
infoModel = pdrLog MODEL Info

errorModel :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorModel = pdrLog MODEL Error

-----------------------------------------------------------
-- UPGRADE
-----------------------------------------------------------
debugUpgrade :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugUpgrade = pdrLog UPGRADE Debug

infoUpgrade :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
infoUpgrade = pdrLog UPGRADE Info

errorUpgrade :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorUpgrade = pdrLog UPGRADE Error

-----------------------------------------------------------
-- COMPILER
-----------------------------------------------------------
debugCompiler :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugCompiler = pdrLog COMPILER Debug

errorCompiler :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorCompiler = pdrLog COMPILER Error

-----------------------------------------------------------
-- INSTALL
-----------------------------------------------------------
errorInstall :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorInstall = pdrLog INSTALL Error

debugInstall :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugInstall = pdrLog INSTALL Debug

infoInstall :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
infoInstall = pdrLog INSTALL Info

traceInstall :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
traceInstall = pdrLog INSTALL Trace

-----------------------------------------------------------
-- OTHER
-----------------------------------------------------------
warnOther :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
warnOther = pdrLog OTHER Warn

errorOther :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorOther = pdrLog OTHER Error

-----------------------------------------------------------
-- PARSER (additional levels)
-----------------------------------------------------------
errorParser :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorParser = pdrLog PARSER Error

-----------------------------------------------------------
-- PERSISTENCE (additional levels)
-----------------------------------------------------------
warnPersistence :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
warnPersistence = pdrLog PERSISTENCE Warn

-----------------------------------------------------------
-- STATE (additional levels)
-----------------------------------------------------------
warnState :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
warnState = pdrLog STATE Warn

-----------------------------------------------------------
-- DELTA
-----------------------------------------------------------
traceDelta :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
traceDelta = pdrLog DELTA Trace

debugDelta :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugDelta = pdrLog DELTA Debug

infoDelta :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
infoDelta = pdrLog DELTA Info

warnDelta :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
warnDelta = pdrLog DELTA Warn

errorDelta :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorDelta = pdrLog DELTA Error

-----------------------------------------------------------
-- TEST
-----------------------------------------------------------
traceTest :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
traceTest = pdrLog TEST Trace

debugTest :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugTest = pdrLog TEST Debug

infoTest :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
infoTest = pdrLog TEST Info

warnTest :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
warnTest = pdrLog TEST Warn

errorTest :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorTest = pdrLog TEST Error

-----------------------------------------------------------
-- RESOURCE-RELATED WARNINGS
-----------------------------------------------------------
traceResource :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
traceResource = pdrLog RESOURCE Trace

debugResource :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
debugResource = pdrLog RESOURCE Debug

infoResource :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
infoResource = pdrLog RESOURCE Info

warnResource :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
warnResource = pdrLog RESOURCE Warn

errorResource :: forall m. MonadPerspectivesWithState PerspectivesExtraState m => String -> m Unit
errorResource = pdrLog RESOURCE Error