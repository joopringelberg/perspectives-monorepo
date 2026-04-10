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
  ( debugBroker
  , debugCompiler
  , debugInstall
  , debugModel
  , debugPersistence
  , debugQuery
  , debugState
  , debugSync
  , debugUpgrade
  , errorBroker
  , errorCompiler
  , errorInstall
  , errorModel
  , errorOther
  , errorParser
  , errorPersistence
  , errorSync
  , errorUpgrade
  , infoBroker
  , infoInstall
  , infoModel
  , infoSync
  , infoUpgrade
  , pdrLog
  , traceBroker
  , tracePersistence
  , traceQuery
  , traceState
  , traceSync
  , warnAuth
  , warnBroker
  , warnModel
  , warnOther
  , warnPersistence
  , warnState
  , warnSync
  ) where

import Control.Monad.AvarMonadAsk (gets)
import Data.Map (lookup) as Map
import Data.Maybe (fromMaybe)
import Effect.Class (liftEffect)
import Effect.Class.Console (log) as Console
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), MonadPerspectives)
import Prelude (Unit, bind, show, when, ($), (>=), (<>))

-- | Emit a log message for the given topic at the given level.
-- | The message is only written to the console when `level >= threshold`,
-- | where `threshold` is the per-topic override from `logConfig.topicLevels`,
-- | or `logConfig.defaultLevel` when no override is present.
pdrLog :: LogTopic -> LogLevel -> String -> MonadPerspectives Unit
pdrLog topic level message = do
  { defaultLevel, topicLevels } <- gets _.logConfig
  let threshold = fromMaybe defaultLevel (Map.lookup topic topicLevels)
  when (level >= threshold) do
    let prefix = "[" <> show level <> "] [" <> show topic <> "] "
    liftEffect $ Console.log (prefix <> message)

-----------------------------------------------------------
-- SYNC
-----------------------------------------------------------
traceSync :: String -> MonadPerspectives Unit
traceSync = pdrLog SYNC Trace

debugSync :: String -> MonadPerspectives Unit
debugSync = pdrLog SYNC Debug

infoSync :: String -> MonadPerspectives Unit
infoSync = pdrLog SYNC Info

warnSync :: String -> MonadPerspectives Unit
warnSync = pdrLog SYNC Warn

errorSync :: String -> MonadPerspectives Unit
errorSync = pdrLog SYNC Error

-----------------------------------------------------------
-- BROKER
-----------------------------------------------------------
traceBroker :: String -> MonadPerspectives Unit
traceBroker = pdrLog BROKER Trace

debugBroker :: String -> MonadPerspectives Unit
debugBroker = pdrLog BROKER Debug

infoBroker :: String -> MonadPerspectives Unit
infoBroker = pdrLog BROKER Info

warnBroker :: String -> MonadPerspectives Unit
warnBroker = pdrLog BROKER Warn

errorBroker :: String -> MonadPerspectives Unit
errorBroker = pdrLog BROKER Error

-----------------------------------------------------------
-- QUERY
-----------------------------------------------------------
traceQuery :: String -> MonadPerspectives Unit
traceQuery = pdrLog QUERY Trace

debugQuery :: String -> MonadPerspectives Unit
debugQuery = pdrLog QUERY Debug

-----------------------------------------------------------
-- PERSISTENCE
-----------------------------------------------------------
tracePersistence :: String -> MonadPerspectives Unit
tracePersistence = pdrLog PERSISTENCE Trace

debugPersistence :: String -> MonadPerspectives Unit
debugPersistence = pdrLog PERSISTENCE Debug

errorPersistence :: String -> MonadPerspectives Unit
errorPersistence = pdrLog PERSISTENCE Error

-----------------------------------------------------------
-- STATE
-----------------------------------------------------------
traceState :: String -> MonadPerspectives Unit
traceState = pdrLog STATE Trace

debugState :: String -> MonadPerspectives Unit
debugState = pdrLog STATE Debug

-----------------------------------------------------------
-- AUTH
-----------------------------------------------------------
warnAuth :: String -> MonadPerspectives Unit
warnAuth = pdrLog AUTH Warn

-----------------------------------------------------------
-- MODEL
-----------------------------------------------------------
debugModel :: String -> MonadPerspectives Unit
debugModel = pdrLog MODEL Debug

warnModel :: String -> MonadPerspectives Unit
warnModel = pdrLog MODEL Warn

infoModel :: String -> MonadPerspectives Unit
infoModel = pdrLog MODEL Info

errorModel :: String -> MonadPerspectives Unit
errorModel = pdrLog MODEL Error

-----------------------------------------------------------
-- UPGRADE
-----------------------------------------------------------
debugUpgrade :: String -> MonadPerspectives Unit
debugUpgrade = pdrLog UPGRADE Debug

infoUpgrade :: String -> MonadPerspectives Unit
infoUpgrade = pdrLog UPGRADE Info

errorUpgrade :: String -> MonadPerspectives Unit
errorUpgrade = pdrLog UPGRADE Error

-----------------------------------------------------------
-- COMPILER
-----------------------------------------------------------
debugCompiler :: String -> MonadPerspectives Unit
debugCompiler = pdrLog COMPILER Debug

errorCompiler :: String -> MonadPerspectives Unit
errorCompiler = pdrLog COMPILER Error

-----------------------------------------------------------
-- INSTALL
-----------------------------------------------------------
errorInstall :: String -> MonadPerspectives Unit
errorInstall = pdrLog INSTALL Error

debugInstall :: String -> MonadPerspectives Unit
debugInstall = pdrLog INSTALL Debug

infoInstall :: String -> MonadPerspectives Unit
infoInstall = pdrLog INSTALL Info

-----------------------------------------------------------
-- OTHER
-----------------------------------------------------------
warnOther :: String -> MonadPerspectives Unit
warnOther = pdrLog OTHER Warn

errorOther :: String -> MonadPerspectives Unit
errorOther = pdrLog OTHER Error

-----------------------------------------------------------
-- PARSER (additional levels)
-----------------------------------------------------------
errorParser :: String -> MonadPerspectives Unit
errorParser = pdrLog PARSER Error

-----------------------------------------------------------
-- PERSISTENCE (additional levels)
-----------------------------------------------------------
warnPersistence :: String -> MonadPerspectives Unit
warnPersistence = pdrLog PERSISTENCE Warn

-----------------------------------------------------------
-- STATE (additional levels)
-----------------------------------------------------------
warnState :: String -> MonadPerspectives Unit
warnState = pdrLog STATE Warn