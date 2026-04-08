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

module Perspectives.ErrorLogging where

import Prelude

import Control.Monad.AvarMonadAsk (modify)
import Data.Array (cons)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), MonadPerspectives)
import Perspectives.Logging (pdrLog)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Warning (PerspectivesWarning)

logPerspectivesError :: forall m. MonadEffect m => PerspectivesError -> m Unit
logPerspectivesError = liftEffect <<< log <<< show

-- HAS A SINGLE DEPENDENCY IN DOMEINCACHE THAT WE CANNOT REPLACE DUE TO CIRCULARITY ISSUES.
warnModeller :: PerspectivesWarning -> MonadPerspectives Unit
warnModeller warning = do
  modify \(s@{ warnings }) -> s { warnings = cons ({ message: show warning, error: "" }) warnings }
  pdrLog MODEL Warn (show warning)
