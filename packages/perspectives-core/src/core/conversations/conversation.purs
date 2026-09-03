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
  ( ConversationSource
  , ConversationSourceLocation
  , augmentConversationSource
  , cacheConversationArtifact
  , clearConversationCache
  , compileConversationSources
  , generateConversations
  , getHelpConversation
  , initializeConversationSource
  , readConversationSources
  , resolveConversationSource
  , resolveConversationLabel
  , storeConversationArtifactInRepository
  , storeConversationArtifactLocally
  ) where

import Prelude

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, head)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, Fn5, runFn3, runFn5)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn2, EffectFn3, EffectFn4, EffectFn6, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn6)
import Foreign (Foreign, unsafeToForeign)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction, (##>))
import Perspectives.Extern.Files (getPFileTextValue)
import Perspectives.Identifiers (modelUri2ModelUrl, typeUri2ModelUri_, unversionedModelUri)
import Perspectives.Logging (warnModel)
import Perspectives.ModelDependencies (conversationSourceDocumentName, conversationSourceYaml)
import Perspectives.Persistence.API (addAttachment, fromBlob, getAttachment, retrieveDocumentVersion, toFile)
import Perspectives.PerspectivesState (addWarning, conversationCacheDelete, conversationCacheInsert, conversationCacheLookup)
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

foreign import resolveConversationSourceImpl
  :: EffectFn6 String (Array String) String String String String (Nullable { conversationId :: String, documentName :: String })

foreign import resolveConversationLabelImpl
  :: Fn3 Foreign String String (Nullable String)

foreign import augmentConversationSourceImpl :: EffectFn3 String String String String

foreign import initializeConversationSourceImpl
  :: EffectFn2 String ConversationContextDescriptor String

type ConversationContextDescriptor =
  { contextType :: String
  , displayName :: String
  , audienceRoles :: Array String
  , perspectives :: Array ConversationPerspectiveDescriptor
  }

type ConversationPerspectiveDescriptor =
  { id :: String
  , signature :: String
  , audienceRole :: String
  , targetRoles :: Array String
  , targetDisplayName :: String
  }

type ConversationSource =
  { documentName :: String
  , roleInstance :: RoleInstance
  , yaml :: String
  }

type ConversationSourceLocation =
  { conversationId :: String
  , source :: ConversationSource
  }

initializeConversationSource :: String -> ConversationContextDescriptor -> Effect String
initializeConversationSource = runEffectFn2 initializeConversationSourceImpl

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
    sourceDocuments <- lift $ readConversationSources sourceRoleIds
    compiled <- lift $ try $ compileConversationSources sourceDocuments versionedModelUri
    case compiled of
      Left compileError -> lift $ addWarning
        { message: "Error in conversation YAML."
        , error: show compileError
        , externalRoleId: ""
        , contextName: ""
        }
      Right json -> lift $ storeConversationArtifactInRepository versionedModelUri json

readConversationSources :: Array String -> MonadPerspectives (Array ConversationSource)
readConversationSources = traverse readSource
  where
  readSource sourceRoleId = do
    mDocumentName <- (RoleInstance sourceRoleId) ##> getPropertyValues (ENP $ EnumeratedPropertyType conversationSourceDocumentName)
    mFileValue <- (RoleInstance sourceRoleId) ##> getPropertyValues (ENP $ EnumeratedPropertyType conversationSourceYaml)
    documentName <- case mDocumentName of
      Nothing -> throwError $ error $ "Conversation source role '" <> sourceRoleId <> "' has no document name."
      Just (Value name) -> pure name
    fileValue <- case mFileValue of
      Nothing -> throwError $ error $ "Conversation source '" <> documentName <> "' has no YAML file."
      Just (Value value) -> pure value
    contents <- getPFileTextValue fileValue
    case contents of
      Nothing -> throwError $ error $ "Cannot read conversation source '" <> documentName <> "'."
      Just source -> pure { documentName, roleInstance: RoleInstance sourceRoleId, yaml: source }

compileConversationSources :: Array ConversationSource -> String -> MonadPerspectives String
compileConversationSources sourceDocuments versionedModelUri = do
  mapping <- loadStableMapping (ModelUri versionedModelUri :: ModelUri Stable) fromRepository >>= case _ of
    Nothing -> throwError $ error "Could not load stableIdMapping.json for conversation compilation."
    Just stableMapping -> pure stableMapping
  liftEffect $ runEffectFn4
    compileConversationSourcesImpl
    (map _.documentName sourceDocuments)
    (map _.yaml sourceDocuments)
    mapping
    versionedModelUri

resolveConversationSource
  :: Array ConversationSource
  -> String
  -> String
  -> String
  -> Maybe String
  -> Maybe String
  -> MonadPerspectives (Maybe ConversationSourceLocation)
resolveConversationSource sourceDocuments versionedModelUri contextType audienceRoleType targetRoleType perspectiveId = do
  compiled <- compileConversationSources sourceDocuments versionedModelUri
  location <- liftEffect $ runEffectFn6
    resolveConversationSourceImpl
    compiled
    (map _.documentName sourceDocuments)
    contextType
    audienceRoleType
    (maybeString targetRoleType)
    (maybeString perspectiveId)
  pure do
    parsed <- toMaybe location
    source <- find (\candidate -> candidate.documentName == parsed.documentName) sourceDocuments
    pure { conversationId: parsed.conversationId, source }
  where
  maybeString = case _ of
    Nothing -> ""
    Just value -> value

augmentConversationSource :: ConversationSourceLocation -> String -> Effect ConversationSource
augmentConversationSource { conversationId, source } conversationYaml = do
  yaml <- runEffectFn3 augmentConversationSourceImpl source.yaml conversationId conversationYaml
  pure $ source { yaml = yaml }

storeConversationArtifactLocally :: String -> String -> MonadPerspectives Unit
storeConversationArtifactLocally versionedModelUri json = do
  { database, documentName } <- resourceIdentifier2DocLocator $ unversionedModelUri versionedModelUri
  storeConversationAttachment database documentName json

storeConversationArtifactInRepository :: String -> String -> MonadPerspectives Unit
storeConversationArtifactInRepository versionedModelUri json = do
  let { repositoryUrl, documentName } = unsafePartial modelUri2ModelUrl versionedModelUri
  storeConversationAttachment repositoryUrl documentName json

storeConversationAttachment :: String -> String -> String -> MonadPerspectives Unit
storeConversationAttachment database documentName json = do
  attachment <- liftEffect $ toFile "conversations.json" "application/json" (unsafeToForeign json)
  revision <- retrieveDocumentVersion database documentName
  void $ addAttachment database documentName revision "conversations.json" attachment (MediaType "application/json")

cacheConversationArtifact :: String -> String -> MonadPerspectives Unit
cacheConversationArtifact versionedModelUri json =
  conversationCacheInsert (unversionedModelUri versionedModelUri) (parseConversationStoreImpl json)

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

resolveConversationLabel :: String -> String -> MonadPerspectives (Maybe String)
resolveConversationLabel contextType perspectiveId = do
  store <- loadConversationStore contextType
  pure $ toMaybe $ runFn3 resolveConversationLabelImpl store contextType perspectiveId

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