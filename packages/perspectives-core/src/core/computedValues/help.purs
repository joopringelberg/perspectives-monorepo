-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Extern.Help where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError, try)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, concat, elem, filter, head, null, nub)
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
import Partial.Unsafe (unsafePartial)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.Update (saveFile, setProperty)
import Perspectives.ContextAndRole (rol_context)
import Perspectives.Conversation (ConversationSource, augmentConversationSource, cacheConversationArtifact, compileConversationSources, generateConversations, initializeConversationSource, readConversationSources, resolveConversationLabel, resolveConversationSource, storeConversationArtifactInRepository, storeConversationArtifactLocally)
import Perspectives.Conversations.Parser (parseConversation)
import Perspectives.Conversations.Renderer (conversationBodyToYaml, renderConversationFromContextYaml)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MonadPerspectivesTransaction, mkLibEffect1, mkLibEffect2, mkLibEffect5, mkLibFunc4, mkLibFunc5, (##=), (##>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Boundaries (handleExternalFunctionError, handleExternalStatementError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Extern.Parsing (withRepositoryModel)
import Perspectives.Identifiers (modelUriVersion, typeUri2ModelUri_, unversionedModelUri)
import Perspectives.InstanceRepresentation (PerspectContext(..))
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (allRoleBinders, binding, context, externalRole, getEnumeratedRoleInstances, getMyType)
import Perspectives.ModelDependencies (conversationSourceContextType, conversationSourceDocumentKind, conversationSourceDocumentName, conversationSourceYaml, conversationSources, modelURIReadable, modelsInUse, versionedModelURI)
import Perspectives.Names (getMySystem)
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Query.UnsafeCompiler (getPropertyValues, getRoleInstances)
import Perspectives.Representation.Class.Role (displayNameOfRoleType, perspectivesOfRoleType)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType, roletype2string)
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Perspectives.Sidecar.HashQFD (qfdSignature)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Stable)
import Perspectives.Sidecar.StableIdMapping (PropertyUri(..), RoleUri(..), fromLocalModels, idUriForProperty, idUriForRole, loadStableMapping)
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
        let nonRootContexts = filter (\(Context { readableName }) -> unwrap readableName /= unwrap namespace) (Object.values contexts)
        descriptors <- traverse contextDescriptor nonRootContexts
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

type ConversationEditorBranch =
  { branchExternal :: String
  , branchContext :: String
  , authoringRoleType :: String
  , conversationText :: String
  , conversationTextPropertyType :: String
  , contextTypePropertyType :: String
  , conversationIdentifierPropertyType :: String
  }

type HelpProjectTypeIds =
  { conversationBranchesRoleType :: EnumeratedRoleType
  , conversationTextPropertyType :: EnumeratedPropertyType
  , contextTypePropertyType :: EnumeratedPropertyType
  , conversationIdentifierPropertyType :: EnumeratedPropertyType
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

getConversationBranch :: String -> String -> MonadPerspectives (Maybe ConversationEditorBranch)
getConversationBranch contextType perspectiveId = do
  mconversationLabel <- resolveConversationLabel contextType perspectiveId
  case mconversationLabel of
    Nothing -> pure Nothing
    Just conversationLabel -> do
      mtypeIds <- getHelpProjectTypeIds
      case mtypeIds of
        Nothing -> pure Nothing
        Just typeIds -> do
          manifestExternals <- getTargetModelManifestExternals contextType
          candidates <- concat <$> traverse (candidateHelpProjectContexts typeIds.conversationBranchesRoleType) manifestExternals
          resolveCandidate candidates typeIds conversationLabel
  where
  resolveCandidate [] _ _ = pure Nothing
  resolveCandidate ({ helpProjectContext, modelManifestExternal, myRoleType } : rest) typeIds conversationLabel = do
    existing <- findExistingBranch helpProjectContext typeIds contextType conversationLabel
    case existing of
      Just { branchExternal, branchContext } -> do
        text <- ensureConversationText modelManifestExternal contextType conversationLabel myRoleType branchExternal typeIds.conversationTextPropertyType
        pure $ Just
          { branchExternal: unwrap branchExternal
          , branchContext: unwrap branchContext
          , authoringRoleType: roletype2string myRoleType
          , conversationText: text
          , conversationTextPropertyType: unwrap typeIds.conversationTextPropertyType
          , contextTypePropertyType: unwrap typeIds.contextTypePropertyType
          , conversationIdentifierPropertyType: unwrap typeIds.conversationIdentifierPropertyType
          }
      Nothing -> do
        created <- createBranch helpProjectContext modelManifestExternal myRoleType typeIds contextType conversationLabel
        case created of
          Nothing -> resolveCandidate rest typeIds conversationLabel
          Just { branchExternal, branchContext, conversationText } ->
            pure $ Just
              { branchExternal: unwrap branchExternal
              , branchContext: unwrap branchContext
              , authoringRoleType: roletype2string myRoleType
              , conversationText
              , conversationTextPropertyType: unwrap typeIds.conversationTextPropertyType
              , contextTypePropertyType: unwrap typeIds.contextTypePropertyType
              , conversationIdentifierPropertyType: unwrap typeIds.conversationIdentifierPropertyType
              }

  candidateHelpProjectContexts conversationBranchesRoleType manifestExternal = do
    binderRoles <- manifestExternal ##= allRoleBinders
    contexts <- concat <$> traverse (\role -> role ##= context) binderRoles
    catMaybes <$> traverse (toCandidate manifestExternal) (nub contexts)
    where
    toCandidate modelManifestExternal helpProjectContext = do
      mmyRoleType <- helpProjectContext ##> getMyType
      case mmyRoleType of
        Nothing -> pure Nothing
        Just myRoleType -> do
          _ <- helpProjectContext ##= getRoleInstances (ENR conversationBranchesRoleType)
          pure $ Just { helpProjectContext, modelManifestExternal, myRoleType }

findExistingBranch
  :: ContextInstance
  -> HelpProjectTypeIds
  -> String
  -> String
  -> MonadPerspectives (Maybe { branchExternal :: RoleInstance, branchContext :: ContextInstance })
findExistingBranch helpProjectContext typeIds contextType conversationLabel = do
  branches <- helpProjectContext ##= getRoleInstances (ENR typeIds.conversationBranchesRoleType)
  found <- catMaybes <$> traverse branchIfMatching branches
  pure $ head found
  where
  branchIfMatching branch = do
    mboundContext <- branch ##> (binding >=> context)
    case mboundContext of
      Nothing -> pure Nothing
      Just branchContext -> do
        mbranchExternal <- branchContext ##> externalRole
        case mbranchExternal of
          Nothing -> pure Nothing
          Just branchExternal -> do
            mcontextType <- branchExternal ##> getPropertyValues (ENP typeIds.contextTypePropertyType)
            mconversationLabel <- branchExternal ##> getPropertyValues (ENP typeIds.conversationIdentifierPropertyType)
            pure case mcontextType, mconversationLabel of
              Just (Value existingContextType), Just (Value existingConversationLabel)
                | existingContextType == contextType && existingConversationLabel == conversationLabel -> Just { branchExternal, branchContext }
              _, _ -> Nothing

createBranch
  :: ContextInstance
  -> RoleInstance
  -> RoleType
  -> HelpProjectTypeIds
  -> String
  -> String
  -> MonadPerspectives (Maybe { branchExternal :: RoleInstance, branchContext :: ContextInstance, conversationText :: String })
createBranch helpProjectContext modelManifestExternal myRoleType typeIds contextType conversationLabel = do
  created <- try $ runMonadPerspectivesTransaction (roletype2string myRoleType) do
    createAndAddRoleInstance
      typeIds.conversationBranchesRoleType
      (unwrap helpProjectContext)
      (RolSerialization { id: Nothing, properties: PropertySerialization Object.empty, binding: Nothing })
  case created of
    Left _ -> pure Nothing
    Right Nothing -> pure Nothing
    Right (Just branchRole) -> do
      mboundContext <- branchRole ##> (binding >=> context)
      case mboundContext of
        Nothing -> pure Nothing
        Just branchContext -> do
          mbranchExternal <- branchContext ##> externalRole
          case mbranchExternal of
            Nothing -> pure Nothing
            Just branchExternal -> do
              mtext <- try $ renderConversationText modelManifestExternal contextType conversationLabel
              case mtext of
                Left _ -> pure Nothing
                Right text -> do
                  void $ runMonadPerspectivesTransaction (roletype2string myRoleType) do
                    setProperty [ branchExternal ] typeIds.contextTypePropertyType Nothing [ Value contextType ]
                    setProperty [ branchExternal ] typeIds.conversationIdentifierPropertyType Nothing [ Value conversationLabel ]
                    setProperty [ branchExternal ] typeIds.conversationTextPropertyType Nothing [ Value text ]
                  pure $ Just { branchExternal, branchContext, conversationText: text }

ensureConversationText :: RoleInstance -> String -> String -> RoleType -> RoleInstance -> EnumeratedPropertyType -> MonadPerspectives String
ensureConversationText modelManifestExternal contextType conversationLabel myRoleType branchExternal conversationTextPropertyType = do
  mtext <- branchExternal ##> getPropertyValues (ENP conversationTextPropertyType)
  case mtext of
    Just (Value text) | text /= "" -> pure text
    _ -> do
      text <- renderConversationText modelManifestExternal contextType conversationLabel
      void $ runMonadPerspectivesTransaction (roletype2string myRoleType) $
        setProperty [ branchExternal ] conversationTextPropertyType Nothing [ Value text ]
      pure text

renderConversationText :: RoleInstance -> String -> String -> MonadPerspectives String
renderConversationText modelManifestExternal contextType conversationLabel = do
  modelManifestContext <- modelManifestExternal ##>> context
  sourceRoles <- modelManifestContext ##= getEnumeratedRoleInstances (EnumeratedRoleType conversationSources)
  sourceDocuments <- readConversationSources $ map unwrap sourceRoles
  matchingSources <- catMaybes <$> traverse (sourceForContextType contextType) sourceDocuments
  located <- catMaybes <$> traverse sourceWithConversation matchingSources
  case head located of
    Nothing -> throwError $ error $ "No conversation source found for context type '" <> contextType <> "' and conversation '" <> conversationLabel <> "'."
    Just source -> do
      rendered <- liftEffect $ renderConversationFromContextYaml source.yaml conversationLabel
      case rendered of
        Left renderError -> throwError $ error $ show renderError
        Right text -> pure text
  where
  sourceForContextType targetContextType source = do
    msourceContextType <- source.roleInstance ##> getPropertyValues (ENP $ EnumeratedPropertyType conversationSourceContextType)
    pure case msourceContextType of
      Just (Value sourceContextType) | sourceContextType == targetContextType -> Just source
      _ -> Nothing

  sourceWithConversation source = do
    rendered <- liftEffect $ renderConversationFromContextYaml source.yaml conversationLabel
    pure case rendered of
      Left _ -> Nothing
      Right _ -> Just source

mergeConversationBranchLocally
  :: EnumeratedPropertyType
  -> EnumeratedPropertyType
  -> EnumeratedPropertyType
  -> RoleInstance
  -> MonadPerspectivesTransaction Unit
mergeConversationBranchLocally contextTypePropertyType conversationIdentifierPropertyType conversationTextPropertyType branchExternal = do
  mcontextType <- lift $ branchExternal ##> getPropertyValues (ENP contextTypePropertyType)
  contextType <- case mcontextType of
    Just (Value value) -> pure value
    Nothing -> throwError $ error "ConversationBranch has no ContextType."
  mconversationLabel <- lift $ branchExternal ##> getPropertyValues (ENP conversationIdentifierPropertyType)
  conversationLabel <- case mconversationLabel of
    Just (Value value) -> pure value
    Nothing -> throwError $ error "ConversationBranch has no ConversationIdentifier."
  mconversationText <- lift $ branchExternal ##> getPropertyValues (ENP conversationTextPropertyType)
  conversationText <- case mconversationText of
    Just (Value value) -> pure value
    Nothing -> throwError $ error "ConversationBranch has no ConversationText."
  conversationYaml <- case parseConversation conversationText of
    Left parseError -> throwError $ error $ show parseError
    Right body -> pure $ conversationBodyToYaml body
  project <- lift $ getAuthoringProject branchExternal
  matchingSources <- lift $ catMaybes <$> traverse (sourceForContextType contextType) project.sources
  located <- lift $ catMaybes <$> traverse sourceWithConversation matchingSources
  resolved <- case head located of
    Nothing -> throwError $ error $ "No conversation source found for context type '" <> contextType <> "' and conversation '" <> conversationLabel <> "'."
    Just source -> pure { conversationId: conversationLabel, source }
  augmented <- liftEffect $ augmentConversationSource resolved conversationYaml
  let augmentedSources = map (replaceSource augmented) project.sources
  artifact <- lift $ compileConversationSources augmentedSources project.versionedModelUri
  lift $ storeConversationArtifactLocally project.versionedModelUri artifact
  lift $ cacheConversationArtifact project.versionedModelUri artifact
  where
  replaceSource replacement source
    | replacement.roleInstance == source.roleInstance = replacement
    | otherwise = source

  sourceForContextType targetContextType source = do
    msourceContextType <- lift $ source.roleInstance ##> getPropertyValues (ENP $ EnumeratedPropertyType conversationSourceContextType)
    pure case msourceContextType of
      Just (Value sourceContextType) | sourceContextType == targetContextType -> Just source
      _ -> Nothing

  sourceWithConversation source = do
    rendered <- liftEffect $ renderConversationFromContextYaml source.yaml conversationLabel
    pure case rendered of
      Left _ -> Nothing
      Right _ -> Just source

getTargetModelManifestExternals :: String -> MonadPerspectives (Array RoleInstance)
getTargetModelManifestExternals contextType = do
  system <- getMySystem
  modelRoles <- (ContextInstance system) ##= getRoleInstances (ENR $ EnumeratedRoleType modelsInUse)
  let modelUri = unsafePartial typeUri2ModelUri_ contextType
  manifestExternals <- catMaybes <$> traverse (matchingManifest modelUri) modelRoles
  pure $ nub manifestExternals
  where
  matchingManifest modelUri modelRole = do
    mmanifestExternal <- modelRole ##> binding
    case mmanifestExternal of
      Nothing -> pure Nothing
      Just manifestExternal -> do
        mversionedModelUri <- manifestExternal ##> getPropertyValues (CP $ CalculatedPropertyType versionedModelURI)
        pure case mversionedModelUri of
          Just (Value versionedModelUri) | unversionedModelUri versionedModelUri == modelUri -> Just manifestExternal
          _ -> Nothing

getHelpProjectTypeIds :: MonadPerspectives (Maybe HelpProjectTypeIds)
getHelpProjectTypeIds = do
  mhelpProjectModel <- getVersionedStableModelUri "model://joopringelberg.nl#HelpProject"
  case mhelpProjectModel of
    Nothing -> pure Nothing
    Just helpProjectVersionedStableModelUri -> do
      mmapping <- loadStableMapping (ModelUri helpProjectVersionedStableModelUri :: ModelUri Stable) fromLocalModels
      pure do
        mapping <- mmapping
        conversationBranchesRoleType <- EnumeratedRoleType <$> idUriForRole mapping (RoleUri "model://joopringelberg.nl#HelpProject$HelpProject$ConversationBranches")
        conversationTextPropertyType <- EnumeratedPropertyType <$> idUriForProperty mapping (PropertyUri "model://joopringelberg.nl#HelpProject$HelpProject$ConversationBranch$ConversationText")
        contextTypePropertyType <- EnumeratedPropertyType <$> idUriForProperty mapping (PropertyUri "model://joopringelberg.nl#HelpProject$HelpProject$ConversationBranch$ContextType")
        conversationIdentifierPropertyType <- EnumeratedPropertyType <$> idUriForProperty mapping (PropertyUri "model://joopringelberg.nl#HelpProject$HelpProject$ConversationBranch$ConversationIdentifier")
        pure
          { conversationBranchesRoleType
          , conversationTextPropertyType
          , contextTypePropertyType
          , conversationIdentifierPropertyType
          }

getVersionedStableModelUri :: String -> MonadPerspectives (Maybe String)
getVersionedStableModelUri readableModelUri = do
  system <- getMySystem
  modelRoles <- (ContextInstance system) ##= getRoleInstances (ENR $ EnumeratedRoleType modelsInUse)
  candidates <- catMaybes <$> traverse matchingModel modelRoles
  pure $ head candidates
  where
  matchingModel modelRole = do
    mmanifestExternal <- modelRole ##> binding
    case mmanifestExternal of
      Nothing -> pure Nothing
      Just manifestExternal -> do
        mreadableModelUri <- manifestExternal ##> getPropertyValues (CP $ CalculatedPropertyType modelURIReadable)
        mversionedModelUri <- manifestExternal ##> getPropertyValues (CP $ CalculatedPropertyType versionedModelURI)
        pure case mreadableModelUri, mversionedModelUri of
          Just (Value readableUri), Just (Value versionedUri) | readableUri == readableModelUri -> Just versionedUri
          _, _ -> Nothing

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