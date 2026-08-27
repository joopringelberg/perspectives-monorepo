-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Extern.Help where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError, try)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, concat, head, null, nub)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Effect.Aff (error)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Foreign (unsafeToForeign)
import Foreign.Object as Object
import Perspectives.Assignment.Update (saveFile)
import Perspectives.Conversation (ConversationSource, augmentConversationSource, cacheConversationArtifact, compileConversationSources, generateConversations, readConversationSources, resolveConversationSource, storeConversationArtifactInRepository, storeConversationArtifactLocally)
import Perspectives.Conversations.Parser (parseConversation)
import Perspectives.Conversations.Renderer (conversationBodyToYaml, renderConversationFromContextYaml)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MonadPerspectivesTransaction, mkLibEffect2, mkLibEffect5, mkLibFunc4, mkLibFunc5, (##=), (##>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Error.Boundaries (handleExternalFunctionError, handleExternalStatementError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.InstanceRepresentation (PerspectContext(..))
import Perspectives.Instances.ObjectGetters (allRoleBinders, binding, context, externalRole, getEnumeratedRoleInstances)
import Perspectives.ModelDependencies (conversationSourceYaml, conversationSources, versionedModelURI)
import Perspectives.Persistent (getPerspectContext)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance, Value(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..))

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
    void $ saveFile augmented.roleInstance (EnumeratedPropertyType conversationSourceYaml) (unsafeToForeign augmented.yaml) "text/yaml"
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
  [ mkLibEffect2 "model://perspectives.domains#HelpLib$GenerateConversations" True generateConversations
  , mkLibFunc4 "model://perspectives.domains#HelpLib$ToConversationText" True toConversationText
  , mkLibFunc5 "model://perspectives.domains#HelpLib$ToConversationYaml" True toConversationYaml
  , mkLibEffect5 "model://perspectives.domains#HelpLib$MergeConversationYamlLocally" True mergeConversationYamlLocally
  , mkLibEffect5 "model://perspectives.domains#HelpLib$MergeConversationYaml" True mergeConversationYaml
  ]

require :: forall m. MonadThrow Error m => String -> Array String -> m String
require description values = case head values of
  Nothing -> throwError $ error $ "Help function requires " <> description <> "."
  Just value -> pure value