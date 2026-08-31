-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Extern.Help where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError, try)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, concat, elem, head, null, nub)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (error)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign (unsafeToForeign)
import Foreign.Object as Object
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (saveFile)
import Perspectives.ContextAndRole (rol_context)
import Perspectives.Conversation (ConversationSource, augmentConversationSource, cacheConversationArtifact, compileConversationSources, generateConversations, initializeConversationSource, readConversationSources, resolveConversationSource, storeConversationArtifactInRepository, storeConversationArtifactLocally)
import Perspectives.Conversations.Parser (parseConversation)
import Perspectives.Conversations.Renderer (conversationBodyToYaml, renderConversationFromContextYaml)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MonadPerspectivesTransaction, mkLibEffect1, mkLibEffect2, mkLibEffect5, mkLibFunc4, mkLibFunc5, (##=), (##>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Boundaries (handleExternalFunctionError, handleExternalStatementError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Extern.Parsing (withRepositoryModel)
import Perspectives.Identifiers (modelUriVersion)
import Perspectives.InstanceRepresentation (PerspectContext(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (allRoleBinders, binding, context, externalRole, getEnumeratedRoleInstances)
import Perspectives.ModelDependencies (conversationSourceContextType, conversationSourceDocumentKind, conversationSourceDocumentName, conversationSourceYaml, conversationSources, versionedModelURI)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.Class.Role (displayNameOfRoleType, perspectivesOfRoleType)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType, roletype2string)
import Perspectives.Sidecar.HashQFD (qfdSignature)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Stable)
import Perspectives.Sidecar.ToReadable (toReadable)

type InitialContextDescriptor =
  { contextType :: String
  , displayName :: String
  , audienceRoles :: Array String
  , perspectives :: Array InitialPerspectiveDescriptor
  }

type InitialPerspectiveDescriptor =
  { id :: String
  , signature :: String
  , audienceRole :: String
  , targetRoles :: Array String
  , targetDisplayName :: String
  }

initializeConversations
  :: Array String
  -> RoleInstance
  -> MonadPerspectivesTransaction Unit
initializeConversations modelUris manifestExternal =
  try initialize >>= handleExternalStatementError "model://perspectives.domains#HelpLib$InitializeConversations"
  where
  initialize :: MonadPerspectivesTransaction Unit
  initialize = do
    versionedModelUri <- require "a versioned stable model URI" modelUris
    version <- case modelUriVersion versionedModelUri of
      Nothing -> throwError $ error "InitializeConversations requires a versioned stable model URI."
      Just value -> pure value
    { namespace, descriptors } <- lift $ withRepositoryModel (ModelUri versionedModelUri :: ModelUri Stable)
      \(DomeinFile { namespace, contexts }) -> do
        descriptors <- traverse contextDescriptor (Object.values contexts)
        pure { namespace, descriptors }
    manifest <- lift $ getPerspectRol manifestExternal
    let manifestContext = rol_context manifest
    sourceRoles <- lift $ (manifestContext ##= getEnumeratedRoleInstances (EnumeratedRoleType conversationSources))
    existingContextTypes <- lift $ catMaybes <$> traverse sourceContextType sourceRoles
    let readableVersionedModelUri = unwrap namespace <> "@" <> version
    createdRoles <- catMaybes <$> traverse
      (createMissingSource manifestContext readableVersionedModelUri existingContextTypes)
      descriptors
    let allSourceRoles = sourceRoles <> createdRoles
    sourceDocuments <- lift $ readConversationSources $ map unwrap allSourceRoles
    artifact <- lift $ compileConversationSources sourceDocuments versionedModelUri
    lift $ storeConversationArtifactInRepository versionedModelUri artifact
    lift $ storeConversationArtifactLocally versionedModelUri artifact
    lift $ cacheConversationArtifact versionedModelUri artifact

  sourceContextType :: RoleInstance -> MonadPerspectives (Maybe String)
  sourceContextType sourceRole = (sourceRole ##> getPropertyValues (ENP $ EnumeratedPropertyType conversationSourceContextType)) >>= case _ of
    Nothing -> pure Nothing
    Just (Value contextType) -> pure $ Just contextType

  contextDescriptor :: Context -> MonadPerspectives InitialContextDescriptor
  contextDescriptor (Context { readableName, displayName, gebruikerRol }) = do
    readableAudienceRoles <- traverse toReadable gebruikerRol
    perspectiveGroups <- traverse perspectiveDescriptors gebruikerRol
    pure
      { contextType: unwrap readableName
      , displayName
      , audienceRoles: map roletype2string readableAudienceRoles
      , perspectives: concat perspectiveGroups
      }

  perspectiveDescriptors :: RoleType -> MonadPerspectives (Array InitialPerspectiveDescriptor)
  perspectiveDescriptors audienceRole = do
    readableAudienceRole <- toReadable audienceRole
    perspectives <- perspectivesOfRoleType audienceRole
    catMaybes <$> traverse (perspectiveDescriptor readableAudienceRole) perspectives

  perspectiveDescriptor :: RoleType -> Perspective -> MonadPerspectives (Maybe InitialPerspectiveDescriptor)
  perspectiveDescriptor readableAudienceRole (Perspective { id, object, roleTypes }) = case head roleTypes of
    Nothing -> pure Nothing
    Just targetRole -> do
      readableTargets <- traverse toReadable roleTypes
      targetDisplayName <- displayNameOfRoleType targetRole
      pure $ Just
        { id
        , signature: qfdSignature object
        , audienceRole: roletype2string readableAudienceRole
        , targetRoles: map roletype2string readableTargets
        , targetDisplayName
        }

  createMissingSource
    :: ContextInstance
    -> String
    -> Array String
    -> InitialContextDescriptor
    -> MonadPerspectivesTransaction (Maybe RoleInstance)
  createMissingSource manifestContext readableVersionedModelUri existingContextTypes descriptor
    | descriptor.contextType `elem` existingContextTypes = pure Nothing
    | otherwise = do
        yaml <- liftEffect $ initializeConversationSource readableVersionedModelUri descriptor
        let contextType = descriptor.contextType
        let
          properties = PropertySerialization $ Object.fromFoldable
            [ Tuple conversationSourceDocumentName [ contextType ]
            , Tuple conversationSourceDocumentKind [ "Context" ]
            , Tuple conversationSourceContextType [ contextType ]
            ]
        created <- createAndAddRoleInstance
          (EnumeratedRoleType conversationSources)
          (unwrap manifestContext)
          (RolSerialization { id: Nothing, properties, binding: Nothing })
        sourceRole <- case created of
          Nothing -> throwError $ error $ "Could not create conversation source for '" <> contextType <> "'."
          Just role -> pure role
        void $ saveFile sourceRole (EnumeratedPropertyType conversationSourceYaml) (unsafeToForeign yaml) "text/yaml" (Just contextType)
        pure $ Just sourceRole

toConversationText
  :: Array String
  -> Array String
  -> Array String
  -> Array String
  -> (RoleInstance ~~> Value)
toConversationText contextTypes audienceRoleTypes targetRoleTypes perspectiveIds branchExternal =
  try
    ( ArrayT do
        contextType <- require "a stable context type" contextTypes
        audienceRoleType <- require "a stable audience role type" audienceRoleTypes
        project <- lift $ getAuthoringProject branchExternal
        location <- lift $ resolveConversationSource
          project.sources
          project.versionedModelUri
          contextType
          audienceRoleType
          (head targetRoleTypes)
          (head perspectiveIds)
        case location of
          Nothing -> pure []
          Just { conversationId, source } -> do
            rendered <- liftEffect $ renderConversationFromContextYaml source.yaml conversationId
            case rendered of
              Left renderError -> throwError $ error $ show renderError
              Right text -> pure [ Value text ]
    ) >>= handleExternalFunctionError "model://perspectives.domains#HelpLib$ToConversationText"

toConversationYaml
  :: Array String
  -> Array String
  -> Array String
  -> Array String
  -> Array String
  -> (RoleInstance ~~> Value)
toConversationYaml _ _ _ _ texts _ =
  try
    ( ArrayT case head texts of
        Nothing -> throwError $ error "ToConversationYaml requires conversation text."
        Just text -> case parseConversation text of
          Left parseError -> throwError $ error $ show parseError
          Right body -> pure [ Value $ conversationBodyToYaml body ]
    ) >>= handleExternalFunctionError "model://perspectives.domains#HelpLib$ToConversationYaml"

mergeConversationYamlLocally
  :: Array String
  -> Array String
  -> Array String
  -> Array String
  -> Array String
  -> RoleInstance
  -> MonadPerspectivesTransaction Unit
mergeConversationYamlLocally contextTypes audienceRoleTypes targetRoleTypes perspectiveIds conversationYamls branchExternal =
  try (mergeConversationYaml_ false contextTypes audienceRoleTypes targetRoleTypes perspectiveIds conversationYamls branchExternal)
    >>= handleExternalStatementError "model://perspectives.domains#HelpLib$MergeConversationYamlLocally"

mergeConversationYaml
  :: Array String
  -> Array String
  -> Array String
  -> Array String
  -> Array String
  -> RoleInstance
  -> MonadPerspectivesTransaction Unit
mergeConversationYaml contextTypes audienceRoleTypes targetRoleTypes perspectiveIds conversationYamls branchExternal =
  try (mergeConversationYaml_ true contextTypes audienceRoleTypes targetRoleTypes perspectiveIds conversationYamls branchExternal)
    >>= handleExternalStatementError "model://perspectives.domains#HelpLib$MergeConversationYaml"

mergeConversationYaml_
  :: Boolean
  -> Array String
  -> Array String
  -> Array String
  -> Array String
  -> Array String
  -> RoleInstance
  -> MonadPerspectivesTransaction Unit
mergeConversationYaml_ persist contextTypes audienceRoleTypes targetRoleTypes perspectiveIds conversationYamls branchExternal = do
  contextType <- require "a stable context type" contextTypes
  audienceRoleType <- require "a stable audience role type" audienceRoleTypes
  conversationYaml <- require "conversation YAML" conversationYamls
  project <- lift $ getAuthoringProject branchExternal
  location <- lift $ resolveConversationSource
    project.sources
    project.versionedModelUri
    contextType
    audienceRoleType
    (head targetRoleTypes)
    (head perspectiveIds)
  resolved <- case location of
    Nothing -> throwError $ error "No conversation source matches the requested help location."
    Just value -> pure value
  augmented <- liftEffect $ augmentConversationSource resolved conversationYaml
  let augmentedSources = map (replaceSource augmented) project.sources
  artifact <- lift $ compileConversationSources augmentedSources project.versionedModelUri
  when persist do
    void $ saveFile augmented.roleInstance (EnumeratedPropertyType conversationSourceYaml) (unsafeToForeign augmented.yaml) "text/yaml" Nothing
    lift $ storeConversationArtifactInRepository project.versionedModelUri artifact
  lift $ storeConversationArtifactLocally project.versionedModelUri artifact
  lift $ cacheConversationArtifact project.versionedModelUri artifact
  where
  replaceSource replacement source
    | replacement.roleInstance == source.roleInstance = replacement
    | otherwise = source

type AuthoringProject =
  { sources :: Array ConversationSource
  , versionedModelUri :: String
  }

getAuthoringProject :: RoleInstance -> MonadPerspectives AuthoringProject
getAuthoringProject branchExternal = do
  branchRoles <- branchExternal ##= allRoleBinders
  helpProjectContexts <- concat <$> traverse (\role -> role ##= context) branchRoles
  helpProjectRoles <- concat <$> traverse rolesInContext helpProjectContexts
  boundContexts <- concat <$> traverse (\role -> role ##= (binding >=> context)) helpProjectRoles
  candidates <- catMaybes <$> traverse projectInContext (nub boundContexts)
  case candidates of
    [ project ] -> pure project
    [] -> throwError $ error "The ConversationBranch is not connected to a VersionedModelManifest with ConversationSources."
    _ -> throwError $ error "The ConversationBranch resolves to more than one VersionedModelManifest."
  where
  rolesInContext contextInstance = do
    PerspectContext { rolInContext } <- getPerspectContext contextInstance
    pure $ concat $ Object.values rolInContext

  projectInContext contextInstance = do
    sourceRoles <- contextInstance ##= getConversationSources
    if null sourceRoles then pure Nothing
    else do
      modelUri <- contextInstance ##> (externalRole >=> getPropertyValues (ENP $ EnumeratedPropertyType versionedModelURI))
      case modelUri of
        Nothing -> pure Nothing
        Just (Value uri) -> do
          sources <- readConversationSources $ map unwrap sourceRoles
          pure $ Just { sources, versionedModelUri: uri }

  getConversationSources = getEnumeratedRoleInstances (EnumeratedRoleType conversationSources)

externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ mkLibEffect1 "model://perspectives.domains#HelpLib$InitializeConversations" True initializeConversations
  , mkLibEffect2 "model://perspectives.domains#HelpLib$GenerateConversations" True generateConversations
  , mkLibFunc4 "model://perspectives.domains#HelpLib$ToConversationText" True toConversationText
  , mkLibFunc5 "model://perspectives.domains#HelpLib$ToConversationYaml" True toConversationYaml
  , mkLibEffect5 "model://perspectives.domains#HelpLib$MergeConversationYamlLocally" True mergeConversationYamlLocally
  , mkLibEffect5 "model://perspectives.domains#HelpLib$MergeConversationYaml" True mergeConversationYaml
  ]

require :: forall m. MonadThrow Error m => String -> Array String -> m String
require description values = case head values of
  Nothing -> throwError $ error $ "Help function requires " <> description <> "."
  Just value -> pure value