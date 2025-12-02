-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

module Perspectives.DataUpgrade
  ( Entity(..)
  , PDRVersion
  , Upgrade
  , addFixingUpdates
  , addIsSystemModel
  , addSettingsType
  , getAllSideCars
  , indexedQueries
  , normalizeIndexedNames
  , normalizeTypes
  , runDataUpgrades
  , runUpgrade
  , save
  , updateModels0250
  , updateModels0254
  , updateModels0260
  , updateModels02611
  , updateModels0266
  , updateModels0267
  , updateModels0269
  ) where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Writer (runWriterT)
import Data.Array (catMaybes, elemIndex, foldM, head, length, union)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), Replacement(..), replace)
import Data.Traversable (for, traverse)
import Data.Tuple (fst)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Foreign (unsafeToForeign)
import Foreign.Object (empty, lookup)
import IDBKeyVal (idbGet, idbSet)
import Main.RecompileBasicModels (UninterpretedDomeinFile, executeInTopologicalOrder, recompileModel)
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (cacheAndSave, setProperty)
import Perspectives.ContextAndRole (rol_property)
import Perspectives.CoreTypes (MonadPerspectives, removeInternally, (##=))
import Perspectives.DataUpgrade.RecompileLocalModels (recompileLocalModels)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DomeinCache (storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (roleInstancesFromCouchdb, updateModel', updateModel_)
import Perspectives.Extern.Utilities (isLowerVersion, pdrVersion)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (buitenRol, splitTypeUri, unversionedModelUri)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.Combinators (filter)
import Perspectives.Instances.ObjectGetters (binding, getProperty)
import Perspectives.ModelDependencies (indexedContext, indexedContextName, indexedRole, indexedRoleName, isSystemModel, rootName, settings, startContexts, sysUser, systemModelName, theSystem)
import Perspectives.ModelDependencies.ReadableStableMappings (modelStableToReadable)
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Arc.PhaseTwoDefs (toReadableDomeinFile, toStableDomeinFile)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (databaseInfo, documentsInDatabase, includeDocs, resetViewIndex)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (entitiesDatabaseName, getDomeinFile, getPerspectRol, saveEntiteit_, saveMarkedResources, tryGetPerspectEntiteit, tryGetPerspectRol)
import Perspectives.PerspectivesState (modelsDatabaseName, pushMessage, removeMessage)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..), EnumeratedPropertyType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.SetupCouchdb (setContext2RoleView, setFilled2FillerView, setFiller2FilledView, setRole2ContextView)
import Perspectives.SetupUser (setupInvertedQueryDatabase)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Readable, Stable)
import Perspectives.Sidecar.NormalizeTypeNames (fqn2tid, normalize, normalizeTypeNames)
import Perspectives.Sidecar.StableIdMapping (StableIdMapping, fromRepository, loadStableMapping, lookupContextIndividualId, lookupRoleIndividualId)
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
    ( \_ -> do
        updateModels0269 unit
        addIsSystemModel unit
        addSettingsType unit
    -- Add IsSystemModel to various models.
    )
  runUpgrade installedVersion "0.26.10"
    ( \_ -> do
        -- Add IsSystemModel to various models.
        addIsSystemModel unit
        -- Add SettingsType to the system model.
        addSettingsType unit
    )

  runUpgrade installedVersion "0.26.11"
    ( \_ -> do
        -- We reload the libraries. They now have translations. 
        -- This prevents the non-critical error messages on startup.
        updateModels02611 unit
    )

  runUpgrade installedVersion "3.0.6"
    ( \_ -> do
        runMonadPerspectivesTransaction'
          false
          (ENR $ EnumeratedRoleType sysUser)
          do
            updateModel_ [ "model://perspectives.domains#System@3.0" ] [ "false" ] (RoleInstance "")
        addSettingsType unit
    )

  runUpgrade installedVersion "3.0.7"
    (\_ -> void recompileLocalModels)

  runUpgrade installedVersion "3.0.8"
    ( \_ -> do
        -- Add IsSystemModel to various models.
        runMonadPerspectivesTransaction'
          false
          (ENR $ EnumeratedRoleType sysUser)
          do
            updateModel_ [ "model://perspectives.domains#System@3.0" ] [ "false" ] (RoleInstance "")
            updateModel_ [ "model://perspectives.domains#CouchdbManagement@8.0" ] [ "false" ] (RoleInstance "")
    )

  runUpgrade installedVersion "3.0.9"
    ( \_ -> do
        -- Add IsSystemModel to various models.
        runMonadPerspectivesTransaction'
          false
          (ENR $ EnumeratedRoleType sysUser)
          do
            updateModel_ [ "model://perspectives.domains#System@3.0" ] [ "false" ] (RoleInstance "")
            void $ createAndAddRoleInstance
              (EnumeratedRoleType $ systemModelName <> "$SystemDataUpgrade")
              "def:#perspectives_domains-System_modelRootContext"
              (RolSerialization { id: Nothing, properties: PropertySerialization empty, binding: Nothing })
            system <- lift getMySystem
            void $ createAndAddRoleInstance
              (EnumeratedRoleType $ theSystem <> "$SystemDataUpgrade")
              system
              (RolSerialization { id: Nothing, properties: PropertySerialization empty, binding: Nothing })
    )

  runUpgrade installedVersion "3.0.11"
    ( \_ -> do
        -- Add IsSystemModel to various models.
        runMonadPerspectivesTransaction'
          false
          (ENR $ EnumeratedRoleType sysUser)
          do
            updateModel_ [ "model://perspectives.domains#System@3.0" ] [ "false" ] (RoleInstance "")
    )

  runUpgrade installedVersion "3.0.39"
    ( \_ -> do
        normalizeTypes unit
        normalizeIndexedNames unit
    )

  runUpgrade installedVersion "3.0.48" normalizeLocalDomeinFiles

  runUpgrade installedVersion "3.0.58"
    ( \_ -> void recompileLocalModels
    )

  -- Add new upgrades above this line and provide the pdr version number in which they were introduced.

  ----------------------------------------------------------------------------------------
  ------- SET CURRENT VERSION
  ----------------------------------------------------------------------------------------
  if installedVersion `isLowerVersion` pdrVersion then liftAff $ idbSet "CurrentPDRVersion" (unsafeToForeign pdrVersion)
  else pure unit

----------------------------------------------------------------------------------------
---- RUN UPGRADE
----------------------------------------------------------------------------------------
-- | Runs the upgrade iff
-- |    * the currently installed PDR  version is lower than the upgradeVersion argument
-- |    * AND
-- |    * the upgradeVersion argument is lower than or equal to the package version (pdrVersion: the version in package.json)
runUpgrade :: PDRVersion -> PDRVersion -> Upgrade -> MonadPerspectives Unit
runUpgrade installedVersion upgradeVersion upgrade =
  if
    isLowerVersion installedVersion upgradeVersion &&
      (isLowerVersion upgradeVersion pdrVersion || upgradeVersion == pdrVersion)
  -- Run the upgrade
  then do
    log ("Running upgrade to version " <> upgradeVersion)
    pushMessage ("Upgrading to PDR version " <> upgradeVersion)
    upgrade unit
    removeMessage ("Upgrading to PDR version " <> upgradeVersion)
  else pure unit

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
  void $ databaseInfo $ sysId <> "_invertedqueries"
  -- set all the views
  setupInvertedQueryDatabase
  -- Fix the source of model:System
  (DomeinFile dfr) <- getDomeinFile (ModelUri "model://perspectives.domains#System")
  void $ saveEntiteit_ (ModelUri "model://perspectives.domains#System")
    (DomeinFile dfr { arc = replace (Pattern "    state InitMe = not exists Me") (Replacement "\n    external \n      aspect sys:RootContext$External\n    state InitMe = not exists Me") dfr.arc })
  -- recompile local models.
  modelsDb <- modelsDatabaseName
  { rows: allModels } <- documentsInDatabase modelsDb includeDocs
  -- As doc is still uninterpreted, we can only rely on the rows.id member of the PouchdbAllDocs record. These, however, are DomeinFileIdentifiers.
  -- We do not have a useful test on the form of such identifiers.
  uninterpretedDomeinFiles <- for allModels \({ id, doc }) -> case read <$> doc of
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
    updateModel_ [ "model://perspectives.domains#System@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#BodiesWithAccounts@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#CouchdbManagement@2.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#BrokerServices@3.0" ] [ "false" ] (RoleInstance "")

updateModels0254 :: Upgrade
updateModels0254 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ [ "model://perspectives.domains#System@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#CouchdbManagement@2.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#BrokerServices@3.0" ] [ "false" ] (RoleInstance "")

updateModels0260 :: Upgrade
updateModels0260 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ [ "model://perspectives.domains#System@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#BodiesWithAccounts@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#CouchdbManagement@3.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#BrokerServices@3.0" ] [ "false" ] (RoleInstance "")

updateModels0266 :: Upgrade
updateModels0266 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ [ "model://perspectives.domains#System@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#CouchdbManagement@6.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#BrokerServices@3.0" ] [ "false" ] (RoleInstance "")

updateModels0267 :: Upgrade
updateModels0267 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ [ "model://perspectives.domains#System@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#BodiesWithAccounts@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#CouchdbManagement@6.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#BrokerServices@3.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#Disconnect@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#HyperContext@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#Introduction@2.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#SharedFileServices@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#SimpleChat@1.0" ] [ "false" ] (RoleInstance "")

----------------------------------------------------------------------------------------
---- VERSION 0.26.8
----------------------------------------------------------------------------------------
updateModels0269 :: Upgrade
updateModels0269 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ [ "model://perspectives.domains#System@3.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#BodiesWithAccounts@2.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#CouchdbManagement@8.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#BrokerServices@4.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#HyperContext@2.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#SharedFileServices@2.0" ] [ "false" ] (RoleInstance "")

addIsSystemModel :: Upgrade
addIsSystemModel _ = do
  systemId <- getMySystem
  targets <- (ContextInstance systemId) ##= filter
    ((getRoleInstances (ENR $ EnumeratedRoleType startContexts)) >=> binding)
    ((getProperty (EnumeratedPropertyType rootName)) >=> \(Value name) -> pure $ isJust $ elemIndex name [ "My System", "Broker Services App", "Couchdb Management App", "Hypertext types", "Shared File Services App" ])
  runMonadPerspectivesTransaction'
    false
    (ENR $ EnumeratedRoleType sysUser)
    (for_ targets \roleInstance -> setProperty [ roleInstance ] (EnumeratedPropertyType isSystemModel) Nothing [ Value "true" ])

addSettingsType :: Upgrade
addSettingsType _ = do
  systemId <- getMySystem
  PerspectRol rec@{ allTypes } <- getPerspectRol (RoleInstance $ buitenRol systemId)
  cacheAndSave (RoleInstance $ buitenRol systemId) (PerspectRol rec { allTypes = [ (EnumeratedRoleType settings) ] `union` allTypes })
  saveMarkedResources

updateModels02611 :: Upgrade
updateModels02611 _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    updateModel_ [ "model://perspectives.domains#Couchdb@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#Serialise@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#Sensor@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#Utilities@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#Parsing@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#Files@1.0" ] [ "false" ] (RoleInstance "")
    updateModel_ [ "model://perspectives.domains#RabbitMQ@1.0" ] [ "false" ] (RoleInstance "")

data Entity = Ctxt PerspectContext | Rle PerspectRol | DoNotSaveRole RoleInstance | DoNotSaveContext ContextInstance | Unknown

normalizeTypes :: Upgrade
normalizeTypes _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    let
      (referredModels :: (Array (ModelUri Stable))) =
        [ ModelUri "model://perspectives.domains#tiodn6tcyc@5.0"
        , ModelUri "model://perspectives.domains#xyfxpg3lzq@11.0"
        , ModelUri "model://perspectives.domains#bxxptg50jp@4.0"
        , ModelUri "model://perspectives.domains#xjrfkxrzyt@3.0"
        , ModelUri "model://perspectives.domains#zjuzxbqpgc@5.0"
        , ModelUri "model://perspectives.domains#hkfgpmwt93@3.0"
        , ModelUri "model://perspectives.domains#l75w588kuk@2.0"
        , ModelUri "model://perspectives.domains#nip6odtx4r@3.0"
        , ModelUri "model://perspectives.domains#dcm0arlqnz@2.0"
        , ModelUri "model://perspectives.domains#s2gyoyohau@2.0"
        , ModelUri "model://perspectives.domains#salp36dvb9@2.0"
        , ModelUri "model://perspectives.domains#piln392sut@2.0"
        , ModelUri "model://perspectives.domains#m203lt2idk@2.0"
        ]
    -- no dependencies to install (false); don't add to the installation (false)
    for_ referredModels \modelUri -> updateModel' modelUri false false

    -- Normalize all contexts and roles. In this process, there is no reliance on the current type names.
    entitiesDb <- lift $ entitiesDatabaseName
    { rows: allEntities } <- lift $ documentsInDatabase entitiesDb includeDocs

    sidecars :: Map.Map (ModelUri Readable) StableIdMapping <- foldM
      ( \scs modelUri@(ModelUri domeinFileName) -> do
          mmapping <- lift $ loadStableMapping modelUri fromRepository
          case mmapping of
            Nothing -> pure scs
            Just submapping -> pure $ Map.insert (toReadable $ ModelUri $ unversionedModelUri domeinFileName) submapping scs
      )
      Map.empty
      referredModels

    -- run in WithSideCars.
    (entities :: Array Entity) <- pure $ unwrap $ runReaderT
      ( for
          allEntities
          ( \{ doc } ->
              unsafePartial case read <$> doc of
                Just (Right r@(PerspectRol rol)) -> do
                  r' <- normalize r
                  if r' == r then pure $ DoNotSaveRole (identifier r)
                  else pure $ Rle r'
                Just _ -> case read <$> doc of
                  Just (Right c@(PerspectContext ctxt)) -> do
                    c' <- normalize c
                    if c' == c then pure $ DoNotSaveContext (identifier c)
                    else pure $ Ctxt c'
                  _ -> pure $ Unknown
          )
      )
      { sidecars, perspMap: empty }
    lift $ for_ entities save
    lift $ saveMarkedResources

-- | puts the modified entity in cache when it needs to be saved, or removes it from cache when marked as DoNotSave.
save :: Entity -> MonadPerspectives Unit
save (Ctxt ctxt) = void $ saveEntiteit_ (identifier ctxt) ctxt
save (Rle rol) = void $ saveEntiteit_ (identifier rol) rol
save (DoNotSaveRole rle) = void $ removeInternally rle
save (DoNotSaveContext ctxt) = void $ removeInternally ctxt
save Unknown = pure unit

toReadable :: ModelUri Stable -> ModelUri Readable
toReadable (ModelUri uri) = ModelUri $ unsafePartial fromJust $ lookup uri modelStableToReadable

getAllSideCars :: MonadPerspectives (Map.Map (ModelUri Readable) StableIdMapping)
getAllSideCars = do
  let
    (referredModels :: (Array (ModelUri Stable))) =
      [ ModelUri "model://perspectives.domains#tiodn6tcyc@5.0"
      , ModelUri "model://perspectives.domains#xyfxpg3lzq@11.0"
      , ModelUri "model://perspectives.domains#bxxptg50jp@4.0"
      , ModelUri "model://perspectives.domains#xjrfkxrzyt@3.0"
      , ModelUri "model://perspectives.domains#zjuzxbqpgc@5.0"
      , ModelUri "model://perspectives.domains#hkfgpmwt93@3.0"
      , ModelUri "model://perspectives.domains#l75w588kuk@2.0"
      , ModelUri "model://perspectives.domains#nip6odtx4r@3.0"
      , ModelUri "model://perspectives.domains#dcm0arlqnz@2.0"
      , ModelUri "model://perspectives.domains#s2gyoyohau@2.0"
      , ModelUri "model://perspectives.domains#salp36dvb9@2.0"
      , ModelUri "model://perspectives.domains#piln392sut@2.0"
      , ModelUri "model://perspectives.domains#m203lt2idk@2.0"
      ]

  foldM
    ( \scs modelUri@(ModelUri domeinFileName) -> do
        mmapping <- loadStableMapping modelUri fromRepository
        case mmapping of
          Nothing -> pure scs
          Just submapping -> pure $ Map.insert (toReadable (ModelUri $ unversionedModelUri domeinFileName)) submapping scs
    )
    Map.empty
    referredModels

normalizeIndexedNames :: Upgrade
normalizeIndexedNames _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  do
    sidecars <- lift getAllSideCars
    log ("Loaded sidecars for " <> show (Map.size sidecars) <> " models.")

    -- Repair the allTypes of all PerspectRol instances.
    entitiesDb <- lift $ entitiesDatabaseName
    { rows: allEntities } <- lift $ documentsInDatabase entitiesDb includeDocs

    -- run in WithSideCars. 
    (entities :: Array Entity) <- pure $ unwrap $ runReaderT
      ( for
          allEntities
          ( \{ doc } ->
              unsafePartial case read <$> doc of
                Just (Right r@(PerspectRol rol)) -> do
                  allTypes' <- traverse fqn2tid rol.allTypes
                  if allTypes' == rol.allTypes then pure $ DoNotSaveRole (identifier r)
                  else pure $ Rle $ PerspectRol rol { allTypes = allTypes' }
                Just _ -> case read <$> doc of
                  Just (Right c@(PerspectContext ctxt)) -> do
                    allTypes' <- traverse fqn2tid ctxt.allTypes
                    if allTypes' == ctxt.allTypes then pure $ DoNotSaveContext (identifier c)
                    else pure $ Ctxt $ PerspectContext ctxt { allTypes = allTypes' }
                  _ -> pure $ Unknown
          )
      )
      { sidecars, perspMap: empty }
    lift $ for_ entities save

    log ("Starting normalization of indexed names")
    -- Reset the view index for IndexedRoles and IndexedContexts.
    (lift entitiesDatabaseName) >>= \db -> lift $ void $ resetViewIndex db "defaultViews/roleView"
    contextInstances <- lift (fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb [ indexedContext ] (ContextInstance ""))))
    log ("Loaded " <> show (length contextInstances) <> " indexed context instances.")

    -- Remember that the property values are still in 'readable' form. Hence we need to index the sidecars by Readable ModelUris.
    void $ for contextInstances \rid -> do
      mrol <- lift $ tryGetPerspectRol rid
      case mrol of
        Nothing -> pure unit
        Just rol -> case head $ rol_property rol (EnumeratedPropertyType indexedContextName) of
          Nothing -> pure unit
          Just (Value readableName) -> do
            stableName <- pure $ do
              case splitTypeUri readableName of
                -- Fall back to readableName when no stable mapping is found.
                Nothing -> readableName
                Just { modelUri } -> case Map.lookup (ModelUri modelUri) sidecars of
                  Nothing -> readableName
                  Just sim -> case lookupContextIndividualId sim readableName of
                    Nothing -> readableName
                    Just stableId -> stableId
            -- Set the property on the IndexedContexts role instance.
            setProperty [ rid ] (EnumeratedPropertyType indexedContextName) Nothing [ Value stableName ]

    (roleInstances :: Array RoleInstance) <- lift (fst <$> runWriterT (runArrayT (roleInstancesFromCouchdb [ indexedRole ] (ContextInstance ""))))
    log ("Loaded " <> show (length roleInstances) <> " indexed role instances.")
    void $ for roleInstances \rid -> do
      mrol <- lift $ tryGetPerspectRol rid
      case mrol of
        Nothing -> pure unit
        Just rol -> case head $ rol_property rol (EnumeratedPropertyType indexedRoleName) of
          Nothing -> pure unit
          Just (Value readableName) -> do
            stableName <- pure $ do
              case splitTypeUri readableName of
                -- Fall back to readableName when no stable mapping is found.
                Nothing -> readableName
                Just { modelUri } -> case Map.lookup (ModelUri modelUri) sidecars of
                  Nothing -> readableName
                  Just sim -> case lookupRoleIndividualId sim readableName of
                    Nothing -> readableName
                    Just stableId -> stableId
            -- Set the property on the IndexedRoles role instance.
            setProperty [ rid ] (EnumeratedPropertyType indexedRoleName) Nothing [ Value stableName ]
    lift $ saveMarkedResources

normalizeLocalDomeinFiles :: Upgrade
normalizeLocalDomeinFiles _ = runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  ( do
      let
        (referredModels :: (Array (ModelUri Stable))) =
          [ ModelUri "model://perspectives.domains#tiodn6tcyc"
          , ModelUri "model://perspectives.domains#xyfxpg3lzq"
          , ModelUri "model://perspectives.domains#bxxptg50jp"
          , ModelUri "model://perspectives.domains#xjrfkxrzyt"
          , ModelUri "model://perspectives.domains#zjuzxbqpgc"
          , ModelUri "model://perspectives.domains#hkfgpmwt93"
          , ModelUri "model://perspectives.domains#l75w588kuk"
          , ModelUri "model://perspectives.domains#nip6odtx4r"
          , ModelUri "model://perspectives.domains#dcm0arlqnz"
          , ModelUri "model://perspectives.domains#s2gyoyohau"
          , ModelUri "model://perspectives.domains#salp36dvb9"
          , ModelUri "model://perspectives.domains#piln392sut"
          , ModelUri "model://perspectives.domains#m203lt2idk"
          ]
      sidecars <- lift getAllSideCars
      -- no dependencies to install (false); don't add to the installation (false)
      models :: Array (DomeinFile Stable) <- lift (catMaybes <$> (for referredModels tryGetPerspectEntiteit))
      (updatedModels :: Array (DomeinFile Readable)) <- pure $ unwrap $ runReaderT
        (for models \model -> normalizeTypeNames (toReadableDomeinFile model))
        { sidecars, perspMap: empty }
      lift $ for_ updatedModels \model -> do
        log ("Saving normalized local model: " <> show (identifier model))
        let stableModel = toStableDomeinFile model
        storeDomeinFileInCouchdbPreservingAttachments stableModel
  )
