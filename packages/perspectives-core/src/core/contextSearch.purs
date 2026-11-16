-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2026 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
-- END LICENSE

module Perspectives.ContextSearch where

import Prelude

import Control.Monad.Writer (runWriterT)
import Data.Array (catMaybes, filter, nub)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (fst)
import Effect.Aff (catchError)
import Foreign.Object (Object, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, (##=), (##>))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Extern.Couchdb (contextInstancesFromCouchdb)
import Perspectives.HumanReadableType (translateType)
import Perspectives.Identifiers (isModelUri, modelUri2LocalName, typeUri2ModelUri_, unversionedModelUri)
import Perspectives.Instances.ObjectGetters (externalRole, roleType_)
import Perspectives.ModelDependencies (apps)
import Perspectives.Names (getMySystem)
import Perspectives.Persistent (getDomeinFile, tryGetPerspectContext, tryGetPerspectRol)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedRoleType, RoleType(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sidecar.ToReadable (toReadable)
import Perspectives.TypePersistence.PerspectiveSerialisation (getReadableName)

type ContextSearchOption = { id :: String, label :: String }

-- A missing manifest or stale instance must not hide the other search results.
optional :: forall a. MonadPerspectives (Maybe a) -> MonadPerspectives (Maybe a)
optional action = catchError action (\_ -> pure Nothing)

getContextSearchModels :: MonadPerspectives (Array ContextSearchOption)
getContextSearchModels = do
  system <- getMySystem
  appRoles <- ContextInstance system ##= getRoleInstances (CR $ CalculatedRoleType apps)
  appTypes <- for appRoles roleType_
  for (modelIdsFromAppTypes appTypes) \id -> do
    readable <- toReadable (ContextType id)
    translated <- translateType (ContextType id)
    pure { id, label: modelSearchLabel id (unwrap readable) translated }

-- Apps already filters StartContexts and follows their bindings. Each returned
-- external role's type identifies its app's model, without consulting manifests.
modelIdsFromAppTypes :: Array EnumeratedRoleType -> Array String
modelIdsFromAppTypes = nub <<< map (unversionedModelUri <<< unsafePartial typeUri2ModelUri_ <<< unwrap)

-- Domain roots are not entries in model translation tables, and unlike nested
-- types their readable names have no '$'-separated local name.
modelSearchLabel :: String -> String -> String -> String
modelSearchLabel modelId readableId translated =
  if translated /= modelId && translated /= readableId then translated
  else if isModelUri readableId then unsafePartial $ modelUri2LocalName (unversionedModelUri readableId)
  else readableId

topLevelContextTypes :: ContextType -> Object Context -> Array ContextType
topLevelContextTypes domain contexts =
  map (\(Context c) -> c.id) $ filter (\(Context c) -> c.context == Just domain) $ values contexts

getContextSearchTypes :: String -> MonadPerspectives (Array ContextSearchOption)
getContextSearchTypes modelId = catchError
  ( do
      -- getDomeinFile reads the locally installed compiled model.
      DomeinFile df <- getDomeinFile (ModelUri $ unversionedModelUri modelId)
      options <- for (topLevelContextTypes (ContextType $ unwrap df.id) df.contexts) \contextType -> optional do
        label <- translateType contextType
        pure $ Just { id: unwrap contextType, label }
      pure $ catMaybes options
  )
  (\_ -> pure [])

getContextSearchInstances :: ContextType -> MonadPerspectives (Array ContextSearchOption)
getContextSearchInstances contextType = do
  instances <- RoleInstance "" ##= contextInstancesFromCouchdb [ unwrap contextType ]
  options <- for instances \instanceId -> optional do
    available <- tryGetPerspectContext instanceId
    case available of
      Nothing -> pure Nothing
      Just _ -> do
        role <- instanceId ##> externalRole
        case role of
          Nothing -> pure Nothing
          Just external -> do
            availableRole <- tryGetPerspectRol external
            case availableRole of
              Nothing -> pure Nothing
              Just _ -> do
                roleType <- roleType_ external
                label <- fst <$> (runWriterT $ getReadableName roleType external)
                pure $ Just { id: unwrap instanceId, label }
  pure $ catMaybes options
