-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

-- | Compilation and runtime access for model-provided help conversations.
-- |
-- | Modellers edit YAML files in CouchdbManagement. This module turns the
-- | complete source collection into one stable-ID-indexed JSON attachment and
-- | later resolves a conversation from the locally installed attachment.
module Perspectives.Conversation
  ( clearConversationCache
  , generateConversations
  , getHelpConversation
  ) where

import Prelude

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn5, runFn5)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (traverse)
import Effect.Aff (error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn4, runEffectFn4)
import Foreign (Foreign, unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##>))
import Perspectives.Extern.Files (getPFileTextValue)
import Perspectives.Identifiers (modelUri2ModelUrl, typeUri2ModelUri_, unversionedModelUri)
import Perspectives.Logging (warnModel)
import Perspectives.ModelDependencies (conversationSourceDocumentName, conversationSourceYaml)
import Perspectives.Persistence.API (addAttachment, fromBlob, getAttachment, retrieveDocumentVersion, toFile)
import Perspectives.PerspectivesState (conversationCacheDelete, conversationCacheInsert, conversationCacheLookup)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), PropertyType(..), RoleType, roletype2string)
import Perspectives.ResourceIdentifiers (resourceIdentifier2DocLocator)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Stable)
import Perspectives.Sidecar.StableIdMapping (StableIdMapping, fromRepository, loadStableMapping)

foreign import compileConversationSourcesImpl
  :: EffectFn4 (Array String) (Array String) StableIdMapping String String

foreign import parseConversationStoreImpl :: String -> Foreign

foreign import resolveConversationImpl
  :: Fn5 Foreign String String String String (Nullable String)

-- | Compile all YAML source files and replace conversations.json atomically.
-- | Source role identifiers are passed instead of parallel property arrays. We
-- | read each name/file pair together so a missing value cannot misalign files.
generateConversations
  :: Array String
  -> Array String
  -> RoleInstance
  -> MonadPerspectivesTransaction Unit
generateConversations sourceRoleIds modelUris _ = case head modelUris of
  Nothing -> throwError $ error "GenerateConversations requires a versioned model URI."
  Just versionedModelUri -> do
    sourceDocuments <- traverse readSource sourceRoleIds
    let documentNames = map _.documentName sourceDocuments
    let sources = map _.yaml sourceDocuments
    mapping <- lift $ loadStableMapping (ModelUri versionedModelUri :: ModelUri Stable) fromRepository >>= case _ of
      Nothing -> throwError $ error "GenerateConversations could not load stableIdMapping.json."
      Just stableMapping -> pure stableMapping

    -- The FFI performs structural validation while it still has the complete
    -- YAML object graph, then emits the compact runtime representation.
    compiled <- liftEffect $ try $ runEffectFn4
      compileConversationSourcesImpl
      documentNames
      sources
      mapping
      versionedModelUri
    json <- case compiled of
      Left compileError -> throwError compileError
      Right result -> pure result

    let { repositoryUrl, documentName } = unsafePartial modelUri2ModelUrl versionedModelUri
    attachment <- liftEffect $ toFile "conversations.json" "application/json" (unsafeToForeign json)
    revision <- lift $ retrieveDocumentVersion repositoryUrl documentName
    void $ lift $ addAttachment
      repositoryUrl
      documentName
      revision
      "conversations.json"
      attachment
      (MediaType "application/json")
  where
  readSource :: String -> MonadPerspectivesTransaction { documentName :: String, yaml :: String }
  readSource sourceRoleId = do
    mDocumentName <- lift ((RoleInstance sourceRoleId) ##> getPropertyValues (ENP $ EnumeratedPropertyType conversationSourceDocumentName))
    mFileValue <- lift ((RoleInstance sourceRoleId) ##> getPropertyValues (ENP $ EnumeratedPropertyType conversationSourceYaml))
    documentName <- case mDocumentName of
      Nothing -> throwError $ error $ "Conversation source role '" <> sourceRoleId <> "' has no document name."
      Just (Value name) -> pure name
    fileValue <- case mFileValue of
      Nothing -> throwError $ error $ "Conversation source '" <> documentName <> "' has no YAML file."
      Just (Value value) -> pure value
    contents <- lift $ getPFileTextValue fileValue
    case contents of
      Nothing -> throwError $ error $ "Cannot read conversation source '" <> documentName <> "'."
      Just source -> pure { documentName, yaml: source }

-- | Resolve a conversation for a context and audience. An empty target role
-- | denotes context-level help; otherwise the lookup is perspective-level.
getHelpConversation
  :: String
  -> RoleType
  -> Maybe RoleType
  -> Maybe String
  -> MonadPerspectives (Maybe String)
getHelpConversation contextType audienceRoleType targetRoleType perspectiveId = do
  store <- loadConversationStore contextType
  pure $ toMaybe $ runFn5 resolveConversationImpl
    store
    contextType
    (roletype2string audienceRoleType)
    ( case targetRoleType of
        Nothing -> ""
        Just target -> roletype2string target
    )
    ( case perspectiveId of
        Nothing -> ""
        Just identifier -> identifier
    )

-- | Load and decode conversations.json once per model. The cached Foreign value
-- | is a validated JavaScript object, so lookups do not repeatedly parse JSON.
loadConversationStore :: String -> MonadPerspectives Foreign
loadConversationStore contextType = do
  let modelUri = unsafePartial typeUri2ModelUri_ contextType
  let emptyStore = parseConversationStoreImpl "{\"schema\":\"perspectives-help/v1\",\"bindings\":{},\"conversations\":{}}"
  conversationCacheLookup modelUri >>= case _ of
    Just store -> pure store
    Nothing -> do
      { database, documentName } <- resourceIdentifier2DocLocator $ unversionedModelUri modelUri
      getAttachment database documentName "conversations.json" >>= case _ of
        Nothing -> do
          conversationCacheInsert modelUri emptyStore
          pure emptyStore
        Just blob -> do
          json <- liftAff $ fromBlob blob
          parsed <- liftEffect $ try $ pure $ parseConversationStoreImpl json
          case parsed of
            Left parseError -> do
              warnModel $ "Cannot parse conversations.json for '" <> modelUri <> "': " <> show parseError
              conversationCacheInsert modelUri emptyStore
              pure emptyStore
            Right store -> do
              conversationCacheInsert modelUri store
              pure store

-- | Remove cached help data when a model version is replaced or removed.
clearConversationCache :: String -> MonadPerspectives Unit
clearConversationCache modelUri = conversationCacheDelete modelUri
