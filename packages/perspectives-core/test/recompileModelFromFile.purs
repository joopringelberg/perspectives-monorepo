-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Tools.RecompileModelFromFile where

import Prelude

import Affjax.RequestBody as RequestBody
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AJ
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (drop)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, error, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception (message)
import Foreign (Foreign)
import Foreign.Object (Object, empty, insert)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (argv, lookupEnv, setExitCode)
import Perspectives.Couchdb (AttachmentInfo)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Pretty (renderMultiplePerspectivesErrors)
import Perspectives.Identifiers (modelUri2LocalName, modelUri2ModelUrl, modelUriVersion, unversionedModelUri, url2Authority)
import Perspectives.InvertedQuery.Storable (StoredQueries)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.ModelTranslation (emptyTranslationTable)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Persistence.Authentication (addCredentials)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runMonadPerspectivesTransaction')
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), Readable, Stable, StableIdMapping, fromRepository, loadStableMapping)
import Perspectives.TypePersistence.LoadArc (loadAndCompileArcFile_, normalizeTrailingWhitespace)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (readJSON, write, writeJSON)
import Test.PDRInstance (noBus, testPouchdbUser, withPDRCached)
import Test.PDRInstance.Types (runInPDR)

type Target =
  { stableModelUri :: String
  , readableModelUri :: String
  , repositoryUrl :: String
  , documentName :: String
  }

type RepositoryModel =
  { _rev :: String
  , _attachments :: Maybe AttachmentInfo
  , id :: ModelUri Stable
  , namespace :: ModelUri Readable
  }

foreign import utf8Base64 :: String -> String

main :: Effect Unit
main = launchAff_ do
  result <- attempt do
    args <- liftEffect $ drop 2 <$> argv
    case args of
      [ stableModelUri, filePath ] -> recompileModelFromFile stableModelUri filePath
      _ -> throwError $ error "Usage: pnpm run recompile:model <unversioned-stable-ModelUri> <local.arc>"
  case result of
    Left e -> liftEffect do
      Console.error $ message e
      setExitCode 1
    Right _ -> pure unit

resolveTarget :: String -> String -> Aff Target
resolveTarget stableModelUri source = do
  unless (test (unsafeRegex "^model://(?:[A-Za-z0-9-]+\\.)+[A-Za-z0-9-]+#[A-Za-z0-9_-]+$" noFlags) stableModelUri)
    $ throwError
    $ error "Provide an unversioned stable ModelUri, e.g. model://perspectives.domains#tiodn6tcyc"
  parsed <- runIndentParser (normalizeTrailingWhitespace source) domain
  readableModelUri <- case parsed of
    Left e -> throwError $ error ("Cannot parse ARC source: " <> show e)
    Right (ContextE { id }) -> pure id
  version <- case modelUriVersion readableModelUri of
    Nothing -> throwError $ error "The ARC domain declaration must include a version."
    Just v -> pure v
  let
    versionedStableModelUri = stableModelUri <> "@" <> version
    split = unsafePartial modelUri2ModelUrl versionedStableModelUri
    readableSplit = unsafePartial modelUri2ModelUrl readableModelUri
  unless (split.repositoryUrl == readableSplit.repositoryUrl)
    $ throwError
    $ error "The stable ModelUri and ARC domain declaration must identify the same repository."
  pure
    { stableModelUri: versionedStableModelUri
    , readableModelUri
    , repositoryUrl: split.repositoryUrl
    , documentName: split.documentName
    }

-- A direct HTTP probe avoids withDatabase, which would create a missing repository.
request :: Maybe { username :: String, password :: String } -> Method -> String -> Maybe String -> Aff (AJ.Response String)
request credentials method url body = do
  result <- AJ.request $ AJ.defaultRequest
    { method = Left method
    , url = url
    , responseFormat = ResponseFormat.string
    , headers = [ RequestHeader "Content-Type" "application/json" ] <> case credentials of
        Nothing -> []
        Just { username, password } -> [ RequestHeader "Authorization" ("Basic " <> utf8Base64 (username <> ":" <> password)) ]
    , content = RequestBody.string <$> body
    }
  case result of
    Left e -> throwError $ error ("Request to " <> url <> " failed: " <> AJ.printError e)
    Right response -> pure response

requireStatus :: Int -> String -> AJ.Response String -> Aff Unit
requireStatus expected url response =
  unless (response.status == StatusCode expected)
    $ throwError
    $ error ("Request to " <> url <> " returned " <> show response.status <> ": " <> response.body)

probeRepository :: Maybe { username :: String, password :: String } -> String -> Aff Unit
probeRepository credentials url = request credentials GET url Nothing >>= requireStatus 200 url

storeDocument :: Maybe { username :: String, password :: String } -> String -> Foreign -> Aff Unit
storeDocument credentials url document =
  request credentials PUT url (Just $ writeJSON document) >>= requireStatus 201 url

compilationAttachments :: Maybe AttachmentInfo -> StoredQueries -> StableIdMapping -> Foreign -> Object Foreign
compilationAttachments existing queries mapping dependencies =
  insert "storedQueries.json" (attachment $ writeJSON queries)
    $ insert "stableIdMapping.json" (attachment $ writeJSON mapping)
    $ insert "modelDependencies.json" (attachment $ writeJSON dependencies)
    $
      case existing of
        Just attachments -> write <$> attachments
        Nothing -> insert "translationtable.json" (attachment $ writeJSON emptyTranslationTable) empty
  where
  attachment text = write { content_type: "application/json", data: utf8Base64 text }

prepareDocument :: Target -> Maybe RepositoryModel -> DomeinFile Stable -> StoredQueries -> StableIdMapping -> Foreign
prepareDocument target existing (DomeinFile df) queries mapping = write $ df
  { _id = target.documentName
  , _rev = _._rev <$> existing
  , _attachments = compilationAttachments (existing >>= _._attachments) queries mapping (write $ fromMaybe [] df.modelDependencies)
  }

recompileModelFromFile :: String -> String -> Aff Unit
recompileModelFromFile stableModelUri filePath = do
  -- Read before starting a PDR or accessing any repository.
  source <- readTextFile UTF8 filePath
  target <- resolveTarget stableModelUri source
  username <- liftEffect $ lookupEnv "PDR_REPOSITORY_USERNAME"
  password <- liftEffect $ lookupEnv "PDR_REPOSITORY_PASSWORD"
  credentials <- case username, password of
    Nothing, Nothing -> pure Nothing
    Just u, Just p -> pure $ Just { username: u, password: p }
    _, _ -> throwError $ error "Set both PDR_REPOSITORY_USERNAME and PDR_REPOSITORY_PASSWORD, or neither."
  probeRepository credentials target.repositoryUrl
  let documentUrl = target.repositoryUrl <> "/" <> target.documentName
  documentResponse <- request credentials GET documentUrl Nothing
  existing <- case documentResponse.status of
    StatusCode 404 -> pure Nothing
    StatusCode 200 -> case readJSON documentResponse.body of
      Left e -> throwError $ error ("Cannot decode existing model: " <> show e)
      Right (model :: RepositoryModel) -> do
        unless (unwrap model.id == stableModelUri && unwrap model.namespace == unversionedModelUri target.readableModelUri)
          $ throwError
          $ error "Existing repository document does not match the supplied stable ModelUri and ARC model name."
        pure $ Just model
    _ -> requireStatus 200 documentUrl documentResponse *> pure Nothing
  withPDRCached (testPouchdbUser "alice") defaultRuntimeOptions Nothing noBus "test/pdr-snapshot/universe/alice" \pdr -> do
    compiled <- runInPDR pdr do
      case credentials, url2Authority target.repositoryUrl of
        Just { username: u, password: p }, Just authority -> addCredentials authority u p
        _, _ -> pure unit
      -- Never silently replace the IDs of an existing release whose mapping is absent or invalid.
      case existing of
        Nothing -> pure unit
        Just _ -> do
          mapping <- loadStableMapping (ModelUri target.stableModelUri) fromRepository
          case mapping of
            Nothing -> throwError $ error "Existing release has no valid stableIdMapping.json; refusing to regenerate stable IDs."
            Just _ -> pure unit
      runMonadPerspectivesTransaction' doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser) do
        result <- loadAndCompileArcFile_ (ModelUri target.stableModelUri) source false
          (unsafePartial modelUri2LocalName stableModelUri)
          target.readableModelUri
          Nothing
        case result of
          Left errors -> do
            rendered <- lift $ renderMultiplePerspectivesErrors errors
            throwError $ error rendered
          Right products -> pure products
    let
      Tuple df (Tuple queries mapping) = compiled
      document = prepareDocument target existing df queries mapping
    -- One revision-checked PUT persists the model and sidecars atomically, retaining other attachments as stubs.
    storeDocument credentials documentUrl document
    liftEffect $ Console.log ("Recompiled " <> target.readableModelUri <> " into " <> documentUrl)
