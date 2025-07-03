-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

-- | A 'data-upgrade' is a procedure that is carried out on stored data of an installation, in order to ensure
-- | that they can be handled by a new version of the PDR.

module Perspectives.DataUpgrade where


import Prelude

import Control.Monad.Except (runExceptT)
import Data.Array (catMaybes, elemIndex, union)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Traversable (for)
import Data.Version (Version, parseVersion)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Foreign (unsafeToForeign)
import IDBKeyVal (idbGet, idbSet)
import Main.RecompileBasicModels (UninterpretedDomeinFile, executeInTopologicalOrder, recompileModel)
import Perspectives.Assignment.Update (cacheAndSave, setProperty)
import Perspectives.CoreTypes (MonadPerspectives, (##=))
import Perspectives.DataUpgrade.RecompileLocalModels (recompileLocalModels)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (updateModel_)
import Perspectives.Extern.Utilities (pdrVersion)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (buitenRol)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (binding, getProperty)
import Perspectives.ModelDependencies (isSystemModel, rootName, settings, startContexts, sysUser)
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (createDatabase, databaseInfo, documentsInDatabase, includeDocs)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (entitiesDatabaseName, getDomeinFile, getPerspectRol, saveEntiteit_, saveMarkedResources)
import Perspectives.PerspectivesState (modelsDatabaseName, pushMessage, removeMessage)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..), EnumeratedRoleType(..), RoleType(..), EnumeratedPropertyType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.SetupCouchdb (setContext2RoleView, setFilled2FillerView, setFiller2FilledView, setRole2ContextView)
import Perspectives.SetupUser (setupInvertedQueryDatabase)
import Simple.JSON (read)
import Unsafe.Coerce (unsafeCoerce)

type PDRVersion = String

type Upgrade = Unit -> MonadPerspectives Unit

runDataUpgrades :: MonadPerspectives Unit 
runDataUpgrades = do
  -- Get the current PDR version
  mcurrentVersion <- liftAff $ idbGet "CurrentPDRVersion"
  (installedVersion :: String) <- case mcurrentVersion of
    Just installedVersion -> pure (unsafeCoerce installedVersion)
    Nothing -> do 
      -- This mechanism was introduced during development of version 0.25.
      -- Installations existing prior to 0.25 will be brought to heel with these instructions.
      liftAff $ idbSet "CurrentPDRVersion" (unsafeToForeign "0.24")
      pure "0.24"

  ----------------------------------------------------------------------------------------
  -- NOTE ON PATCHING.
  -- A 'patch' actually is a complete model source text, with some modifications.
  -- As the PDR that executes upgrades is always the most recent version, it expects the latest syntax.
  -- Comment out any older patches!
  -- NOTE ON UPDATING MODELS
  -- Similar reasoning holds for updating models. The latest (recommended) version is fetched and installed
  -- locally. This obviously has to happen only once for a model.
  ----------------------------------------------------------------------------------------
  runUpgrade installedVersion "0.24.1" addFixingUpdates 
  runUpgrade installedVersion "0.24.2" indexedQueries 
  -- runUpgrade installedVersion "0.25.0" updateModels0250
  -- runUpgrade installedVersion "0.25.2" 
  --   (do 
  --     patchModels PDR2501.replacements
  --     void recompileLocalModels)
  -- runUpgrade installedVersion "0.25.3" 
  --   (do 
  --     patchModels PDR2503.replacements
  --     void recompileLocalModels)
  -- runUpgrade installedVersion "0.25.4" updateModels0254
  -- runUpgrade installedVersion "0.25.5" 
  --   (do 
  --     patchModels PDR2505.replacements
  --     void recompileLocalModels)
  -- runUpgrade installedVersion "0.25.6" 
  --   (void recompileLocalModels)
  runUpgrade installedVersion "0.26.0"
    \_ -> do 
      updateModels0260 unit
      void recompileLocalModels
  runUpgrade installedVersion "0.26.3"
    (\_ -> void recompileLocalModels)
  runUpgrade installedVersion "0.26.5"
    (\_ -> void recompileLocalModels)
  -- As 0.26.7 upgrade performs the same actions as 0.26.6 (and more), it is no longer necessary to perform updateModels0266.
  -- runUpgrade installedVersion "0.26.6"
  --   updateModels0266
  runUpgrade installedVersion "0.26.7"
    updateModels0267
  runUpgrade installedVersion "0.26.9"
    (\_ -> do
      updateModels0269
      addIsSystemModel
      addSettingsType
      -- Add IsSystemModel to various models.
      )
  runUpgrade installedVersion "0.26.10"
    (\_ -> do
      -- Add IsSystemModel to various models.
      addIsSystemModel
      -- Add SettingsType to the system model.
      addSettingsType)
  
  runUpgrade installedVersion "0.26.11"
    (\_ -> do 
      -- We reload the libraries. They now have translations. 
      -- This prevents the non-critical error messages on startup.
      updateModels02611
      )
  
  runUpgrade installedVersion "3.0.6"
    (\_ -> do
      runMonadPerspectivesTransaction'
        false
        (ENR $ EnumeratedRoleType sysUser)
        do
          updateModel_ ["model://perspectives.domains#System@3.0"] ["false"] (RoleInstance "")
      addSettingsType
      )
  
  runUpgrade installedVersion "3.0.7"
    (\_ -> void recompileLocalModels)
  
  runUpgrade installedVersion "3.0.8"
    (\_ -> do
      -- Add IsSystemModel to various models.
      runMonadPerspectivesTransaction'
        false
        (ENR $ EnumeratedRoleType sysUser)
        do
          updateModel_ ["model://perspectives.domains#System@3.0"] ["false"] (RoleInstance "")
          updateModel_ ["model://perspectives.domains#CouchdbManagement@8.0"] ["false"] (RoleInstance "")
    )


  -- Add new upgrades above this line and provide the pdr version number in which they were introduced.
  ----------------------------------------------------------------------------------------
  ------- SET CURRENT VERSION
  ----------------------------------------------------------------------------------------
  if installedVersion `isLowerVersion` pdrVersion
    then liftAff $ idbSet "CurrentPDRVersion" (unsafeToForeign pdrVersion) 
    else pure unit

----------------------------------------------------------------------------------------
---- RUN UPGRADE
----------------------------------------------------------------------------------------
-- | Runs the upgrade iff
-- |    * the currently installed PDR  version is lower than the upgradeVersion argument
-- |    * AND
-- |    * the upgradeVersion argument is lower than or equal to the package version (pdrVersion: the version in package.json)
runUpgrade :: PDRVersion -> PDRVersion -> Upgrade -> MonadPerspectives Unit
runUpgrade installedVersion upgradeVersion upgrade = if isLowerVersion installedVersion upgradeVersion && 
     (isLowerVersion upgradeVersion pdrVersion || upgradeVersion == pdrVersion)
  -- Run the upgrade
  then do 
    log ("Running upgrade to version " <> upgradeVersion)
    pushMessage ("Upgrading to PDR version " <> upgradeVersion)
    upgrade unit
    removeMessage ("Upgrading to PDR version " <> upgradeVersion)
  else pure unit

----------------------------------------------------------------------------------------
---- VERSION COMPARING
----------------------------------------------------------------------------------------
-- Parse version strings into the Version type
toVersion :: String -> Maybe Version
toVersion str = case parseVersion str of
  Right v -> Just v
  Left _ -> Nothing

-- Compare versions (returns true if v1 < v2)
isLowerVersion :: String -> String -> Boolean
isLowerVersion v1 v2 = 
  case toVersion v1, toVersion v2 of
    Just v1', Just v2' -> v1' < v2'
    _, _ -> false  -- Handle parsing errors however you prefer

----------------------------------------------------------------------------------------
---- SPECIFIC UPGRADES
----------------------------------------------------------------------------------------

addFixingUpdates :: Unit -> MonadPerspectives Unit
addFixingUpdates _ = do
  db <- entitiesDatabaseName
  setFiller2FilledView db
  setFilled2FillerView db
  setContext2RoleView db
  setRole2ContextView db

indexedQueries :: Unit -> MonadPerspectives Unit
indexedQueries _ = do
  addAllExternalFunctions
  sysId <- getSystemIdentifier
  -- Create the indexedQueries database
  createDatabase $ sysId <> "_invertedqueries"
  void $ databaseInfo $ sysId <> "_invertedqueries"
  -- set all the views
  setupInvertedQueryDatabase
  -- Fix the source of model:System
  (DomeinFile dfr) <- getDomeinFile (DomeinFileId "model://perspectives.domains#System")
  void $ saveEntiteit_ (DomeinFileId "model://perspectives.domains#System")
    (DomeinFile dfr {arc = replace (Pattern "    state InitMe = not exists Me") (Replacement "\n    external \n      aspect sys:RootContext$External\n    state InitMe = not exists Me") dfr.arc})
  -- recompile local models.
  modelsDb <- modelsDatabaseName
  {rows:allModels} <- documentsInDatabase modelsDb includeDocs
  -- As doc is still uninterpreted, we can only rely on the rows.id member of the PouchdbAllDocs record. These, however, are DomeinFileIdentifiers.
  -- We do not have a useful test on the form of such identifiers.
  uninterpretedDomeinFiles <- for allModels \({id, doc}) -> case read <$> doc of
    Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
    Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
    Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
  r <- runMonadPerspectivesTransaction'
    false
    (ENR $ EnumeratedRoleType sysUser)
    (runExceptT (executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) recompileModel))
  case r of
    Left errors -> logPerspectivesError (Custom ("recompileLocalModels: " <> show errors))
    Right success -> saveMarkedResources

updateModels0250 :: Upgrade
updateModels0250 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ ["model://perspectives.domains#System@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#BodiesWithAccounts@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#CouchdbManagement@2.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#BrokerServices@3.0"] ["false"] (RoleInstance "")

updateModels0254 :: Upgrade
updateModels0254 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ ["model://perspectives.domains#System@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#CouchdbManagement@2.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#BrokerServices@3.0"] ["false"] (RoleInstance "")

updateModels0260 :: Upgrade
updateModels0260 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ ["model://perspectives.domains#System@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#BodiesWithAccounts@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#CouchdbManagement@3.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#BrokerServices@3.0"] ["false"] (RoleInstance "")

updateModels0266 :: Upgrade
updateModels0266 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ ["model://perspectives.domains#System@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#CouchdbManagement@6.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#BrokerServices@3.0"] ["false"] (RoleInstance "")

updateModels0267 :: Upgrade
updateModels0267 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do 
    updateModel_ ["model://perspectives.domains#System@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#BodiesWithAccounts@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#CouchdbManagement@6.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#BrokerServices@3.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#Disconnect@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#HyperContext@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#Introduction@2.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#SharedFileServices@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#SimpleChat@1.0"] ["false"] (RoleInstance "")

----------------------------------------------------------------------------------------
---- VERSION 0.26.8
----------------------------------------------------------------------------------------
updateModels0269:: MonadPerspectives Unit
updateModels0269 = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ ["model://perspectives.domains#System@3.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#BodiesWithAccounts@2.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#CouchdbManagement@8.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#BrokerServices@4.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#HyperContext@2.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#SharedFileServices@2.0"] ["false"] (RoleInstance "")


addIsSystemModel :: MonadPerspectives Unit
addIsSystemModel = do
  systemId <- getMySystem
  targets <- (ContextInstance systemId) ##= filter 
    ((getRoleInstances (ENR $ EnumeratedRoleType startContexts)) >=> binding)
    ((getProperty (EnumeratedPropertyType rootName)) >=> \(Value name) -> pure $ isJust $ elemIndex name ["My System", "Broker Services App", "Couchdb Management App", "Hypertext types", "Shared File Services App"])
  runMonadPerspectivesTransaction'
    false
    (ENR $ EnumeratedRoleType sysUser) 
    (for_ targets \roleInstance -> setProperty [roleInstance] (EnumeratedPropertyType isSystemModel) Nothing [Value "true"])


addSettingsType :: MonadPerspectives Unit
addSettingsType = do
  systemId <- getMySystem
  PerspectRol rec@{allTypes} <- getPerspectRol (RoleInstance $ buitenRol systemId)
  cacheAndSave (RoleInstance $ buitenRol systemId) (PerspectRol rec {allTypes = [(EnumeratedRoleType settings)] `union` allTypes })
  saveMarkedResources

updateModels02611:: MonadPerspectives Unit
updateModels02611 = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ ["model://perspectives.domains#Couchdb@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#Serialise@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#Sensor@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#Utilities@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#Parsing@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#Files@1.0"] ["false"] (RoleInstance "")
    updateModel_ ["model://perspectives.domains#RabbitMQ@1.0"] ["false"] (RoleInstance "")
