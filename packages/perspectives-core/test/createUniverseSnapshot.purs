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

import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (bracket, error, launchAff_)
import Effect.Class.Console (log)
import Foreign.Object (Object, fromFoldable, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), MonadPerspectives, (##=), (##>))
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Identifiers (modelUri2LocalName, modelUri2SchemeAndAuthority, modelUriVersion, unversionedModelUri)
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.Logging (ansiRed, infoTest)
import Perspectives.ModelDependencies (sysUser, manifestsRole, localModelNameProperty, modelCuidProperty)
import Perspectives.Persistent (saveMarkedResources)
import Perspectives.PerspectivesState (defaultRuntimeOptions, setTopicLogLevel)
import Perspectives.Query.UnsafeCompiler (getPropertyFromTelescope)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Test.PDRInstance (noBus, snapshotPDR, startPDRInstance, testPouchdbUser)
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
  [ "model://perspectives.domains#RabbitMQ@2.0"
  , "model://perspectives.domains#BrokerServices@6.1"
  , "model://perspectives.domains#HyperContext@1.0"
  , "model://perspectives.domains#Introduction@1.0"
  , "model://perspectives.domains#HelpProject@3.0"
  , "model://perspectives.domains#Disconnect@1.1"
  , "model://perspectives.domains#RepositoryRegistry@1.0"
  , "model://perspectives.domains#SharedFileServices@4.0"
  ]

repository :: String
repository = "pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains"

-- | Log topics enabled while the snapshot is being created.
logConfiguration :: Array { topic :: LogTopic, logLevel :: LogLevel }
logConfiguration =
  [ { topic: INSTALL, logLevel: Trace }
  , { topic: TEST, logLevel: Info }
  , { topic: RESOURCE, logLevel: Trace }
  , { topic: STATE, logLevel: Trace }
  ]

-----------------------------------------------------------
-- MAPPING READABLE MODEL NAMES TO STABLE MODEL URIS
-----------------------------------------------------------

-- | Query the repository for all its ModelManifests and return a mapping from
-- | the Readable local model name (e.g. "BrokerServices") to the CUID chosen
-- | for it.
getLocalModelNameToCuid :: ContextInstance -> MonadPerspectives (Object String)
getLocalModelNameToCuid repo = do
  manifests <- repo ##= getEnumeratedRoleInstances (EnumeratedRoleType manifestsRole)
  entries <- for manifests \manifest -> do
    mname <- manifest ##> getPropertyFromTelescope (EnumeratedPropertyType localModelNameProperty)
    mcuid <- manifest ##> getPropertyFromTelescope (EnumeratedPropertyType modelCuidProperty)
    pure case mname, mcuid of
      Just (Value name), Just (Value cuid) -> Just (Tuple name cuid)
      _, _ -> Nothing
  pure $ fromFoldable (catMaybes entries)

-- | Replace the Readable local name in a model URI by its CUID, preserving the
-- | version part (if any).
readableModelUri2StableModelUri :: Object String -> String -> Maybe String
readableModelUri2StableModelUri cuids modelUri = unsafePartial do
  let unversioned = unversionedModelUri modelUri
  cuid <- lookup (modelUri2LocalName unversioned) cuids
  pure $ modelUri2SchemeAndAuthority unversioned
    <> "#"
    <> cuid
    <> maybe "" (append "@") (modelUriVersion modelUri)

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

      cuids <- runInPDR pdr $ getLocalModelNameToCuid (ContextInstance repository)

      stableModelUris <- traverse
        ( \modelUri -> case readableModelUri2StableModelUri cuids modelUri of
            Just stableUri -> pure stableUri
            Nothing -> throwError $ error ("No ModelManifest found in the repository for " <> modelUri)
        )
        extraModels

      for_ stableModelUris \modelUri -> do
        runInPDR pdr do
          infoTest ("Installing model " <> modelUri)
          runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
            (addModelToLocalStore_ [ modelUri ] (RoleInstance "Ignored"))

      runInPDR pdr saveMarkedResources

      snapshotPDR (testPouchdbUser userName).systemIdentifier (testPouchdbUser userName).perspectivesUser snapshotDirectory
      log ("Snapshot written to " <> snapshotDirectory)