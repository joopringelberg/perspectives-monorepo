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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-- | This module defines External Core functions for model://perspectives.domains#Couchdb.

module Perspectives.Extern.Parsing where

import Prelude

import Control.Monad.Error.Class (catchError, throwError, try)
import Control.Monad.State (StateT, execStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, head, intercalate)
import Data.Either (Either(..))
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect.Aff (error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object, empty, insert)
import Main.RecompileBasicModels (recompileModelsAtUrl)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, MonadPerspectivesTransaction, mkLibEffect1, mkLibEffect2, mkLibFunc1, mkLibFunc2, (##>))
import Perspectives.Couchdb (DeleteCouchdbDocument(..), DocWithAttachmentInfo(..))
import Perspectives.Couchdb.Revision (Revision_, changeRevision)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (AttachmentFiles)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Boundaries (handleExternalFunctionError, handleExternalStatementError)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (retrieveModelFromLocalStore, updateModel)
import Perspectives.Extern.Files (getPFileTextValue)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (DomeinFileName, ModelUri, isModelUri, modelUri2ModelUrl, unversionedModelUri)
import Perspectives.InvertedQuery.Storable (StoredQueries)
import Perspectives.ModelDependencies (sysUser, versionedModelManifestModelCuid) as MD
import Perspectives.ModelTranslation (ModelTranslation, augmentModelTranslation, generateFirstTranslation, parseTranslation, writeTranslationYaml, generateTranslationTable) as MT
import Perspectives.ModelTranslation (ModelTranslation, emptyTranslationTable)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (addAttachment, addDocument, deleteDocument, fromBlob, getAttachment, getDocument, retrieveDocumentVersion, toFile, tryGetDocument_)
import Perspectives.PerspectivesState (addWarning, getWarnings, resetWarnings, setWarnings)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance, Value(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runEmbeddedTransaction)
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), StableIdMapping, loadStableMapping) as Sidecar
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFile_, loadAndCompileArcFileWithSidecar_)
import Simple.JSON (readJSON, readJSON_, writeJSON)
import Unsafe.Coerce (unsafeCoerce)

-- | Read the .arc file, parse it and try to compile it. Does neither cache nor store.
-- | However, will load, cache and store dependencies of the model.
-- | The DomeinFileName should be unversioned.
-- | Warnings are returned as a result value; they are not sent to the client.
parseAndCompileArc :: Array DomeinFileName -> Array ArcSource -> (RoleInstance ~~> Value)
parseAndCompileArc domeinFileName_ arcSource_ versionedModelManifest =
  try
    ( case head domeinFileName_, head arcSource_ of
        Nothing, _ -> pure $ Value "No model name given!"
        _, Nothing -> pure $ Value "No arc source given!"
        Just domeinFileName, Just arcSource -> catchError
          do
            previousWarnings <- lift $ lift $ getWarnings
            lift $ lift $ resetWarnings
            Value modelCuid <- getPropertyValues (CP $ CalculatedPropertyType MD.versionedModelManifestModelCuid) versionedModelManifest
            r <- lift $ lift $ runEmbeddedTransaction true (ENR $ EnumeratedRoleType MD.sysUser)
              (loadAndCompileArcFile_ (Sidecar.ModelUri domeinFileName) arcSource false modelCuid)
            case r of
              Left errs -> ArrayT $ pure (Value <<< show <$> errs)
              -- Als er meldingen zijn, geef die dan terug.
              Right _ -> do
                warnings <- lift $ lift $ getWarnings
                lift $ lift $ setWarnings previousWarnings
                pure $ Value $ intercalate "\n" (cons "OK" warnings)
          \e -> ArrayT $ pure [ Value (show e) ]
    )
    >>= handleExternalFunctionError "model://perspectives.domains#Parsing$ParseAndCompileArc"

-- | Parse and compile the Arc file, saves in cache. Warnings are sent to the client.
applyImmediately :: Array DomeinFileName -> Array ArcSource -> RoleInstance -> MonadPerspectivesTransaction Unit
applyImmediately domeinFileName_ arcSource_ versionedModelManifest =
  try
    ( case head domeinFileName_, head arcSource_ of
        Nothing, _ -> lift $ addWarning "Parsing$applyImmediately: no model name given!"
        _, Nothing -> lift $ addWarning "Parsing$applyImmediately: no arc source given!"
        Just domeinFileName, Just arcSource -> catchError
          do
            mmodelCuid <- lift (versionedModelManifest ##> getPropertyValues (CP $ CalculatedPropertyType MD.versionedModelManifestModelCuid))
            case mmodelCuid of
              Nothing -> lift $ addWarning "Parsing$applyImmediately: no model CUID given!"
              Just (Value modelCuid) -> do
                r <- lift $ runEmbeddedTransaction true (ENR $ EnumeratedRoleType MD.sysUser)
                  (loadAndCompileArcFile_ (Sidecar.ModelUri domeinFileName) arcSource true modelCuid)
                case r of
                  Left errs -> lift $ addWarning ("Error in Parsing$applyImmediately: " <> show errs)
                  Right _ -> pure unit
          \e -> lift $ addWarning ("Error in Parsing$applyImmediately: " <> show e)
    )
    >>= handleExternalStatementError "model://perspectives.domains#Parsing$ApplyImmediately"

type ArcSource = String
type CrlSource = String
type Url = String

-- | Parse and compile the Arc file. Upload to the repository. Does neither cache, nor stores it in the local collection of DomeinFiles.
-- | If the file is not valid, nothing happens.
-- | The DomeinFileName should be versioned (e.g. model://perspectives.domains#System@1.0).
uploadToRepository
  :: Array DomeinFileName
  -> Array ArcSource
  -> RoleInstance
  -> MonadPerspectivesTransaction Unit
uploadToRepository domeinFileName_ arcSource_ versionedModelManifest =
  try
    ( case head domeinFileName_, head arcSource_ of
        Just domeinFileName, Just arcSource -> do
          -- Retrieve existing sidecar (if any) from repository. domeinFilename should be Stable.
          mMapping <- lift $ Sidecar.loadStableMapping (Sidecar.ModelUri domeinFileName)
          let split = unsafePartial modelUri2ModelUrl domeinFileName
          mmodelCuid <- lift (versionedModelManifest ##> getPropertyValues (CP $ CalculatedPropertyType MD.versionedModelManifestModelCuid))
          case mmodelCuid of
            Nothing -> lift $ addWarning "Parsing$applyImmediately: no model CUID given!"
            Just (Value modelCuid) -> do
              r <- loadAndCompileArcFileWithSidecar_ (Sidecar.ModelUri $ unversionedModelUri domeinFileName) arcSource false mMapping modelCuid
              case r of
                Left m -> logPerspectivesError $ Custom ("uploadToRepository: " <> show m)
                -- Here we will have a tuple of the DomeinFile and an instance of StoredQueries plus the updated mapping.
                Right (Tuple df@(DomeinFile { id, namespace }) (Tuple invertedQueries mapping')) -> do
                  lift $ void $ uploadToRepository_ split df invertedQueries mapping'
        _, _ -> logPerspectivesError $ Custom ("uploadToRepository lacks arguments")
    )
    >>= handleExternalStatementError "model://perspectives.domains#Parsing$UploadToRepository"

type URL = String

-- | As uploadToRepository, but provide the DomeinFile as argument.
-- | Adds an empty TranslationTable if the DomeinFile did not yet exist in the Repository.
-- TODO: retrieve the mapping sidecar from the repository in this function, save its updated value to the repository here too.
uploadToRepository_ :: { repositoryUrl :: String, documentName :: String } -> DomeinFile -> StoredQueries -> Sidecar.StableIdMapping -> MonadPerspectives Unit
uploadToRepository_ splitName (DomeinFile df) invertedQueries mapping = do
  -- Get the attachment info
  (mremoteDf :: Maybe DocWithAttachmentInfo) <- tryGetDocument_ splitName.repositoryUrl splitName.documentName
  attachments <- case mremoteDf of
    Nothing -> defaultAttachments empty
    Just (DocWithAttachmentInfo { _attachments }) -> case _attachments of
      Nothing -> defaultAttachments empty
      Just atts -> do
        mappingFile <- liftEffect $ toFile "stableIdMapping.json" "application/json" (unsafeToForeign $ writeJSON mapping)
        queryFile <- liftEffect $ toFile "storedQueries.json" "application/json" (unsafeToForeign $ writeJSON invertedQueries)
        attachments' <- traverseWithIndex
          (\attName { content_type } -> Tuple (MediaType content_type) <$> getAttachment splitName.repositoryUrl splitName.documentName attName)
          atts
        -- Now add or overwrite the inverted queries and the stable mapping, assuming translations are present.
        pure $ insert
          "storedQueries.json"
          (Tuple (MediaType "application/json") (Just $ unsafeCoerce queryFile))
          ( insert
              "stableIdMapping.json"
              (Tuple (MediaType "application/json") (Just $ unsafeCoerce mappingFile))
              attachments'
          )
  -- Get the revision (if any) from the remote database, so we can overwrite.
  (mVersion :: Maybe String) <- retrieveDocumentVersion splitName.repositoryUrl splitName.documentName
  -- The _id of df will be a versionless identifier. If we don't set it to the versioned name, the document
  -- will be stored under the versionless name.
  (newRev :: Revision_) <- addDocument splitName.repositoryUrl (changeRevision mVersion (DomeinFile df { _id = splitName.documentName })) splitName.documentName
  void $ execStateT (go splitName.repositoryUrl splitName.documentName attachments) newRev

  where
  -- Default attachments are built from the mapping and inverted queries that are passed in as arguments, 
  -- and an empty translation table.
  defaultAttachments :: AttachmentFiles -> MonadPerspectives AttachmentFiles
  defaultAttachments attachments = do
    -- Add an empty translations file.
    translationsFile <- liftEffect $ unsafeCoerce toFile "translationtable.json" "application/json" (unsafeToForeign $ writeJSON emptyTranslationTable)
    -- Add a skeleton stableIdMapping sidecar.
    mappingFile <- liftEffect $ toFile "stableIdMapping.json" "application/json" (unsafeToForeign $ writeJSON mapping)
    queryFile <- liftEffect $ toFile "storedQueries.json" "application/json" (unsafeToForeign $ writeJSON invertedQueries)
    pure $ insert
      "stableIdMapping.json"
      (Tuple (MediaType "application/json") (Just $ unsafeCoerce mappingFile))
      ( insert
          "translationtable.json"
          (Tuple (MediaType "application/json") (Just $ unsafeCoerce translationsFile))
          ( insert
              "storedQueries.json"
              ( Tuple
                  (MediaType "application/json")
                  (Just $ unsafeCoerce queryFile)
              )
              attachments
          )
      )

  -- As each attachment that we add will bump the document version, we have to catch it and use it on the
  -- next attachment.
  go :: URL -> String -> Object (Tuple MediaType (Maybe Foreign)) -> StateT Revision_ MonadPerspectives Unit
  go documentUrl documentName attachments = do
    forWithIndex_
      attachments
      ( \attName (Tuple mimetype mattachment) -> do
          case mattachment of
            Nothing -> pure unit
            Just attachment -> do
              newRev <- get
              DeleteCouchdbDocument { rev } <- lift $ addAttachment documentUrl documentName newRev attName attachment mimetype
              put rev
      )
    -- Lastly, add the StoredQueries
    (newRev :: Revision_) <- get
    theFile <- liftEffect $ toFile "storedQueries.json" "application/json" (unsafeToForeign $ writeJSON invertedQueries)
    lift $ void $ addAttachment documentUrl documentName newRev "storedQueries.json" theFile (MediaType "application/json")

removeFromRepository
  :: Array ModelUri
  -> RoleInstance
  -> MonadPerspectivesTransaction Unit
removeFromRepository modelUris _ =
  try
    ( case head modelUris of
        Just modelUri ->
          if isModelUri modelUri then void $ lift $ removeFromRepository_ (unsafePartial modelUri2ModelUrl modelUri)
          else logPerspectivesError $ DomeinFileErrorBoundary "RemoveFromRepository" ("This modelURI is not well-formed: " <> modelUri)
        _ -> logPerspectivesError $ Custom ("removeFromRepository lacks the ModelURI argument.")
    )
    >>= handleExternalStatementError "model://perspectives.domains#Parsing$RemoveFromRepository"

  where
  removeFromRepository_ :: { repositoryUrl :: String, documentName :: String } -> MonadPerspectives Boolean
  removeFromRepository_ splitName = deleteDocument splitName.repositoryUrl splitName.documentName Nothing

-- | Parse and compile all models found at the URL, e.g. https://perspectives.domains/models_perspectives_domains
compileRepositoryModels
  :: Array Url
  -> Array Url
  -> RoleInstance
  -> MonadPerspectivesTransaction Unit
compileRepositoryModels modelsurl_ manifestsurl_ _ =
  try
    ( case head modelsurl_, head manifestsurl_ of
        Just modelsurl, Just manifestsurl -> recompileModelsAtUrl modelsurl manifestsurl
        _, _ -> logPerspectivesError $ Custom ("compileRepositoryModels lacks arguments")
    )
    >>= handleExternalStatementError "model://perspectives.domains#Parsing$CompileRepositoryModels"

-------------------------------------------------------------------------------
---- PARSE AND STORE LOCALLY ONLY
------------------------------------------------------------------------------- 
-- | Parse and compile the Arc file. Store in the local model database. Does not cache.
-- | If the file is not valid, nothing happens.
-- | The DomeinFileName should be versioned (e.g. model://perspectives.domains#System@1.0).
-- | Attachments are taken from the local model database, 
-- | where the StoredQueries that result from model compilation overwrite those from the local store. 
-- | The translation is that of the Repository, though.
storeModelLocally_
  :: Array DomeinFileName
  -> Array ArcSource
  -> RoleInstance
  -> MonadPerspectivesTransaction Unit
storeModelLocally_ domeinFileName_ arcSource_ versionedModelManifest =
  try
    ( case head domeinFileName_, head arcSource_ of
        Just domeinFileName, Just arcSource -> do
          -- Load mapping from local models DB (if present)
          mLocalMapping <- lift $ Sidecar.loadStableMapping (Sidecar.ModelUri domeinFileName)
          mmodelCuid <- lift (versionedModelManifest ##> getPropertyValues (CP $ CalculatedPropertyType MD.versionedModelManifestModelCuid))
          case mmodelCuid of
            Nothing -> lift $ addWarning "Parsing$applyImmediately: no model CUID given!"
            Just (Value modelCuid) -> do
              r <- loadAndCompileArcFileWithSidecar_ (Sidecar.ModelUri $ unversionedModelUri domeinFileName) arcSource false mLocalMapping modelCuid
              case r of
                Left m -> logPerspectivesError $ Custom ("storeModelLocally: " <> show m)
                -- Here we will have a tuple of the DomeinFile and an instance of StoredQueries.
                Right (Tuple (DomeinFile dfr@{ id, namespace }) (Tuple invertedQueries mapping')) -> do
                  (Tuple _ attachments) <- retrieveModelFromLocalStore id
                  theFile <- liftEffect $ toFile "storedQueries.json" "application/json" (unsafeToForeign $ writeJSON invertedQueries)
                  mappingFile <- liftEffect $ toFile "stableIdMapping.json" "application/json" (unsafeToForeign $ writeJSON mapping')
                  attachments' <- pure $ insert
                    "storedQueries.json"
                    (Tuple (MediaType "application/json") (Just $ unsafeCoerce theFile))
                    ( insert
                        "stableIdMapping.json"
                        (Tuple (MediaType "application/json") (Just $ unsafeCoerce mappingFile))
                        attachments
                    )
                  -- Also saves the inverted queries in the inverted query database.
                  updateModel false {-with dependencies-}  true {-install for first time if necessary-}  id (Tuple dfr attachments') invertedQueries

        _, _ -> logPerspectivesError $ Custom ("StoreModelLocally lacks arguments")
    )
    >>= handleExternalStatementError "model://perspectives.domains#Parsing$StoreModelLocally"

-------------------------------------------------------------------------------
---- MODEL TRANSLATION
-------------------------------------------------------------------------------
-- | From the DomeinFile indicated by the namespace, generate ModelTranslation and serialise it to a PString.
-- | The DomeinFileName should be versioned (e.g. model://perspectives.domains#System@1.0).
generateFirstTranslation :: Array DomeinFileName -> (RoleInstance ~~> Value)
generateFirstTranslation modelURI_ _ = case head modelURI_ of
  Nothing -> handleExternalFunctionError "model://perspectives.domains#Parsing$GenerateFirstTranslation"
    (Left (error "Model URI should be provided."))
  Just dfName -> do
    case (unsafePartial modelUri2ModelUrl dfName) of
      { repositoryUrl, documentName } -> do
        x <- try $ lift $ lift $ getDocument repositoryUrl documentName
        -- x <- try $ lift $ lift $ getDomeinFile (DomeinFileId dfName)
        case x of
          Left e -> handleExternalFunctionError "model://perspectives.domains#Parsing$GenerateFirstTranslation"
            (Left e)
          Right df -> pure $ Value $ writeJSON $ MT.generateFirstTranslation df

-- | From a serialised ModelTranslation, generate YAML (a PString)
getTranslationYaml :: Array String -> (RoleInstance ~~> Value)
getTranslationYaml modelTranslation_ _ = case head modelTranslation_ of
  Nothing -> handleExternalFunctionError "model://perspectives.domains#Parsing$GetTranslationYaml"
    (Left (error "A ModelTranslation should be provided."))
  Just modelTranslation -> case readJSON modelTranslation of
    Left e -> handleExternalFunctionError "model://perspectives.domains#Parsing$GetTranslationYaml"
      (Left $ error (show e))
    Right (translation :: MT.ModelTranslation) -> pure $ Value $ MT.writeTranslationYaml translation

-- | From a YAML string, generate a (serialised) ModelTranslation.
parseYamlTranslation :: Array String -> (RoleInstance ~~> Value)
parseYamlTranslation pfile_ _ = ArrayT case head pfile_ of
  Nothing -> pure []
  Just pfile -> do
    mYaml <- lift $ getPFileTextValue pfile
    case mYaml of
      Nothing -> pure $ []
      Just yaml -> do
        translation' <- liftEffect $ MT.parseTranslation yaml
        case translation' of
          Left e -> throwError e
          Right translation -> pure $ [ Value $ writeJSON translation ]

-- | From a PString that holds a ModelTranslation, generate the table and upload to the repository.
-- | The DomeinFileName should be versioned (e.g. model://perspectives.domains#System@1.0).
generateTranslationTable :: Array String -> Array String -> RoleInstance -> MonadPerspectivesTransaction Unit
generateTranslationTable translation_ domeinFileName_ _ = case head translation_, head domeinFileName_ of
  Nothing, _ -> handleExternalStatementError "model://perspectives.domains#Parsing$GenerateTranslationTable"
    (Left (error "A String holding a ModelTranslation should be provided."))
  _, Nothing -> handleExternalStatementError "model://perspectives.domains#Parsing$GenerateTranslationTable"
    (Left (error "A Versioned model name should be provided. (e.g. model://perspectives.domains#System@1.0)"))
  Just modelTranslationString, Just domeinFileName -> case readJSON modelTranslationString of
    -- Fail silently
    Left e -> log ("generateTranslationTable: " <> show e)
    -- Generate the table and upload as attachment to the repository DomeinFile.
    Right (modelTranslation :: ModelTranslation) -> case (unsafePartial modelUri2ModelUrl domeinFileName) of
      { repositoryUrl, documentName } -> do
        table <- pure (MT.generateTranslationTable modelTranslation)
        theFile <- liftEffect $ toFile "translationtable.json" "application/json" (unsafeToForeign $ writeJSON table)
        mRev <- lift $ retrieveDocumentVersion repositoryUrl documentName
        -- Ignore the new revision. We do not have a local representation of the remote DomeinFile.
        void $ lift $ addAttachment
          repositoryUrl
          documentName
          mRev
          "translationtable.json"
          theFile
          (MediaType "text/json")

-- | Fill a ModelTranslation freshly generated from a DomeinFile, with translations taken from a TranslationTable.
-- | NOTE: the TranslationTable must be available on the versioned model in the repository. It is not a property value.
-- | The ModelTranslation must be passed in as a string.
-- | The result is the (serialised) ModelTranslation.
-- | The original ModelTranslation is returned when there is no TranslationTable or when it cannot be parsed correctly.
augmentModelTranslation :: Array String -> Array String -> (RoleInstance ~~> Value)
augmentModelTranslation translation_ domeinFileName_ _ = case head translation_, head domeinFileName_ of
  Nothing, _ -> handleExternalFunctionError "model://perspectives.domains#Parsing$AugmentModelTranslation"
    (Left (error "A String holding a ModelTranslation should be provided."))
  _, Nothing -> handleExternalFunctionError "model://perspectives.domains#Parsing$AugmentModelTranslation"
    (Left (error "A Versioned model name should be provided. (e.g. model://perspectives.domains#System@1.0)"))
  Just modelTranslationString, Just domeinFileName -> case readJSON_ modelTranslationString of
    -- Fail silently
    Nothing -> handleExternalFunctionError "model://perspectives.domains#Parsing$AugmentModelTranslation"
      (Left (error "The model translation string could not be parsed."))
    -- Retrieve the Existing Translation from the repository and apply it to the ModelTranslation.
    Just (modelTranslation :: ModelTranslation) -> case (unsafePartial modelUri2ModelUrl domeinFileName) of
      { repositoryUrl, documentName } -> do
        mTranslationTableBlob <- lift $ lift $ getAttachment repositoryUrl documentName "translationtable.json"
        case mTranslationTableBlob of
          Nothing -> pure $ Value modelTranslationString
          Just translationTableBlob -> do
            translationTableString <- liftAff $ fromBlob translationTableBlob
            case readJSON translationTableString of
              Left e -> pure $ Value modelTranslationString
              Right translationTable -> pure $ Value $ writeJSON (MT.augmentModelTranslation translationTable modelTranslation)

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ mkLibFunc2 "model://perspectives.domains#Parsing$ParseAndCompileArc" True parseAndCompileArc
  , mkLibEffect2 "model://perspectives.domains#Parsing$ApplyImmediately" True applyImmediately
  , mkLibEffect2 "model://perspectives.domains#Parsing$UploadToRepository" True uploadToRepository
  , mkLibEffect2 "model://perspectives.domains#Parsing$StoreModelLocally" True storeModelLocally_
  , mkLibEffect1 "model://perspectives.domains#Parsing$RemoveFromRepository" True removeFromRepository
  , mkLibEffect2 "model://perspectives.domains#Parsing$CompileRepositoryModels" True compileRepositoryModels
  , mkLibEffect2 "model://perspectives.domains#Parsing$CompileRepositoryModels" True compileRepositoryModels
  , mkLibFunc1 "model://perspectives.domains#Parsing$GenerateFirstTranslation" True generateFirstTranslation
  , mkLibFunc1 "model://perspectives.domains#Parsing$GetTranslationYaml" True getTranslationYaml
  , mkLibFunc1 "model://perspectives.domains#Parsing$ParseYamlTranslation" True parseYamlTranslation
  , mkLibEffect2 "model://perspectives.domains#Parsing$GenerateTranslationTable" True generateTranslationTable
  , mkLibFunc2 "model://perspectives.domains#Parsing$AugmentModelTranslation" True augmentModelTranslation
  ]
