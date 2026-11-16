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
import Data.Array (any, catMaybes, filter, nubByEq)
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
import Perspectives.Identifiers (isModelUri, modelUri2LocalName, typeUri2LocalName, unversionedModelUri)
import Perspectives.Instances.ObjectGetters (binding, context, externalRole, getAllFilledRoles, roleType_)
import Perspectives.ModelDependencies (modelToRemove, modelsInUse)
import Perspectives.Names (getMySystem)
import Perspectives.Persistent (getDomeinFile, tryGetPerspectContext, tryGetPerspectRol)
import Perspectives.Query.UnsafeCompiler (getPropertyValues, getRoleInstances)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), propertytype2string)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sidecar.ToReadable (toReadable)
import Perspectives.TypePersistence.PerspectiveSerialisation (getReadableName)
import Perspectives.Utilities (findM)

type ContextSearchOption = { id :: String, label :: String }

-- A missing manifest or stale instance must not hide the other search results.
optional :: forall a. MonadPerspectives (Maybe a) -> MonadPerspectives (Maybe a)
optional action = catchError action (\_ -> pure Nothing)

getContextSearchModels :: MonadPerspectives (Array ContextSearchOption)
getContextSearchModels = do
  system <- getMySystem
  installed <- ContextInstance system ##= getRoleInstances (ENR $ EnumeratedRoleType modelsInUse)
  options <- for installed \installedRole -> optional do
    modelUri <- installedRole ##> getPropertyValues (ENP $ EnumeratedPropertyType modelToRemove)
    versionManifest <- installedRole ##> binding
    case modelUri, versionManifest of
      Just (Value uri), Just versionExternal -> do
        available <- tryGetPerspectRol versionExternal
        case available of
          Nothing -> pure Nothing
          Just _ -> do
            -- ModelsInUse binds a version manifest; IsLibrary belongs to its
            -- parent model manifest, reached through the Versions binder.
            parents <- versionExternal ##= (getAllFilledRoles >=> context >=> externalRole)
            flags <- for parents \parent -> optional do
              availableParent <- tryGetPerspectRol parent
              case availableParent of
                Nothing -> pure Nothing
                Just _ -> propertyByLocalName "IsLibrary" parent
            if any (_ == Just (Value "false")) flags then do
              let id = unversionedModelUri uri
              readable <- toReadable (ContextType id)
              translated <- translateType (ContextType id)
              pure $ Just { id, label: modelSearchLabel id (unwrap readable) translated }
            else pure Nothing
      _, _ -> pure Nothing
  pure $ nubByEq (\a b -> a.id == b.id) $ catMaybes options

-- Domain roots are not entries in model translation tables, and unlike nested
-- types their readable names have no '$'-separated local name.
modelSearchLabel :: String -> String -> String -> String
modelSearchLabel modelId readableId translated =
  if translated /= modelId && translated /= readableId then translated
  else if isModelUri readableId then unsafePartial $ modelUri2LocalName (unversionedModelUri readableId)
  else readableId

-- Compare readable property identifiers, not the CUIDs in compiled models.
propertyByLocalName :: String -> RoleInstance -> MonadPerspectives (Maybe Value)
propertyByLocalName name role = do
  roleType <- roleType_ role
  properties <- allLocallyRepresentedProperties (ST roleType)
  property <- findM matches properties
  case property of
    Nothing -> pure Nothing
    Just p -> role ##> getPropertyValues p
  where
  matches :: PropertyType -> MonadPerspectives Boolean
  matches p = do
    readable <- toReadable p
    pure $ typeUri2LocalName (propertytype2string readable) == Just name

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
