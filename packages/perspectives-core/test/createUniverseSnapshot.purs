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

-- | Create a fresh PDR snapshot from scratch.
-- |
-- | This module is not a test suite.  It starts a brand new PDR instance
-- | (so `setupUser` runs and the base models are installed), then installs the
-- | models listed in `extraModels` and finally writes a snapshot to
-- | `snapshotDirectory`.  That snapshot can subsequently be used by
-- | `withPDRCached` to start PDR instances without paying the model
-- | installation cost.
-- |
-- | Run with:
-- | pnpm run create:universeSnapshot

module Test.CreateUniverseSnapshot where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (bracket, launchAff_)
import Effect.Class.Console (log)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..))
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Logging (ansiRed, infoTest)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.PerspectivesState (defaultRuntimeOptions, setTopicLogLevel)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Test.PDRInstance (noBus, snapshotPDR, startPDRInstance, testPouchdbUser, waitUntilAllTransactionsComplete)
import Test.PDRInstance.Types (runInPDR)

-----------------------------------------------------------
-- CONFIGURATION
-----------------------------------------------------------

-- | The directory the snapshot is written to.
snapshotDirectory :: String
snapshotDirectory = "test/pdr-snapshot/universe/alice"

-- | The name of the user the snapshot is created for.  Must match the name
-- | used by whatever test restores this snapshot.
userName :: String
userName = "alice"

-- | Models installed on top of the models that `setupUser` already installs.
-- | Edit this list to change the contents of the snapshot.
extraModels :: Array String
extraModels =
  [ "model://perspectives.domains#CouchdbManagement"
  , "model://perspectives.domains#BodiesWithAccounts"
  ]

-- | Log topics enabled while the snapshot is being created.
logConfiguration :: Array { topic :: LogTopic, logLevel :: LogLevel }
logConfiguration =
  [ { topic: INSTALL, logLevel: Trace }
  ]

-----------------------------------------------------------
-- MAIN
-----------------------------------------------------------

main :: Effect Unit
main = launchAff_ do
  bracket
    (startPDRInstance (testPouchdbUser userName) defaultRuntimeOptions (Just ansiRed) noBus)
    _.shutdown
    \pdr -> do
      runInPDR pdr $ for_ logConfiguration \{ topic, logLevel } -> setTopicLogLevel topic logLevel

      for_ extraModels \modelUri -> do
        runInPDR pdr do
          infoTest ("Installing model " <> modelUri)
          runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
            (addModelToLocalStore_ [ modelUri ] (RoleInstance "Ignored"))
        -- Models may trigger state-entry bots; let those settle before the next model.
        waitUntilAllTransactionsComplete 60 pdr

      snapshotPDR (testPouchdbUser userName).systemIdentifier (testPouchdbUser userName).perspectivesUser snapshotDirectory
      log ("Snapshot written to " <> snapshotDirectory)
