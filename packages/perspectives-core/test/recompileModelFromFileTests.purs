-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.RecompileModelFromFile where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Array (length)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Aff (Aff, attempt, bracket)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign.Object (Object, fromFoldable, lookup, keys)
import Data.Tuple (Tuple(..))
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFileRecord)
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), emptyStableIdMapping)
import Simple.JSON (readJSON, write, writeJSON)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Tools.RecompileModelFromFile (prepareDocument, probeRepository, resolveTarget, storeDocument, utf8Base64)

type UploadedDocument = { _id :: String, _rev :: Maybe String, _attachments :: Object Foreign }

type RepositoryMock =
  { url :: String
  , requests :: Effect (Array { method :: String, body :: String, authorization :: String })
  , close :: Effect (Promise Unit)
  }

foreign import startRepositoryMock :: Int -> Effect (Promise RepositoryMock)

withRepositoryMock :: forall a. Int -> (RepositoryMock -> Aff a) -> Aff a
withRepositoryMock status = bracket (toAffE $ startRepositoryMock status) (\mock -> toAffE mock.close)

main :: Effect Unit
main = runTest $ suite "Local model repository recompilation" do
  test "Derives repository and document from stable URI, version from ARC" do
    target <- resolveTarget stableUri (source "@2.7")
    equal "https://perspectives.domains/models_perspectives_domains" target.repositoryUrl
    equal "perspectives_domains-abc123@2.7.json" target.documentName
    equal (stableUri <> "@2.7") target.stableModelUri
    equal "model://perspectives.domains#Example@2.7" target.readableModelUri
  test "Accepts source with trailing blank lines" do
    target <- resolveTarget stableUri (source "@2.7" <> "\n\n")
    equal "perspectives_domains-abc123@2.7.json" target.documentName
  test "Requires a versioned ARC domain declaration" do
    result <- attempt $ resolveTarget stableUri (source "")
    assert "Unversioned source must fail" $ case result of
      Left _ -> true
      Right _ -> false
  test "Rejects a versioned CLI ModelUri" do
    result <- attempt $ resolveTarget (stableUri <> "@1.0") (source "@2.7")
    assert "The source must be the only source of the version" $ case result of
      Left _ -> true
      Right _ -> false
  test "Rejects invalid CLI URIs and malformed source" do
    invalidUri <- attempt $ resolveTarget "model://localhost#abc123" (source "@2.7")
    invalidSource <- attempt $ resolveTarget stableUri "not an ARC model\n"
    assert "Invalid URI must fail" $ case invalidUri of
      Left _ -> true
      Right _ -> false
    assert "Invalid source must fail" $ case invalidSource of
      Left _ -> true
      Right _ -> false
  test "Rejects different repositories" do
    result <- attempt $ resolveTarget "model://example.org#abc123" (source "@2.7")
    assert "Mismatched authorities must fail" $ case result of
      Left _ -> true
      Right _ -> false
  test "Preserves revision and unrelated attachment stubs, replaces compiler sidecars" do
    target <- resolveTarget stableUri (source "@2.7")
    let
      stub =
        { content_type: "application/json"
        , digest: "md5-existing"
        , length: 42
        , revpos: 3
        , stub: true
        }
      existing =
        { _rev: "3-original"
        , id: ModelUri stableUri
        , namespace: ModelUri "model://perspectives.domains#Example"
        , _attachments: Just $ fromFoldable
            [ Tuple "translationtable.json" stub
            , Tuple "custom.json" stub
            , Tuple "storedQueries.json" stub
            ]
        }
      df = DomeinFile $ defaultDomeinFileRecord
        { id = ModelUri stableUri
        , namespace = ModelUri "model://perspectives.domains#Example"
        , arc = source "@2.7"
        , modelDependencies = Just []
        }
      document = prepareDocument target (Just existing) df [] emptyStableIdMapping
    case readJSON (writeJSON document) of
      Left errors -> assert (show errors) false
      Right (decoded :: UploadedDocument) -> do
        equal target.documentName decoded._id
        equal (Just "3-original") decoded._rev
        equal (writeJSON $ Just $ write stub) (writeJSON $ lookup "translationtable.json" decoded._attachments)
        equal (writeJSON $ Just $ write stub) (writeJSON $ lookup "custom.json" decoded._attachments)
        equal 5 $ length $ keys decoded._attachments
        equal
          (writeJSON $ Just $ write { content_type: "application/json", data: utf8Base64 "[]" })
          (writeJSON $ lookup "storedQueries.json" decoded._attachments)
        equal
          (writeJSON $ Just $ write { content_type: "application/json", data: utf8Base64 "[]" })
          (writeJSON $ lookup "modelDependencies.json" decoded._attachments)
        assert "Stable-ID sidecar must be included" $ isJust $ lookup "stableIdMapping.json" decoded._attachments
  test "New documents have no revision and receive translations plus all sidecars" do
    target <- resolveTarget stableUri (source "@2.7")
    let document = prepareDocument target Nothing (DomeinFile defaultDomeinFileRecord) [] emptyStableIdMapping
    case readJSON (writeJSON document) of
      Left errors -> assert (show errors) false
      Right (decoded :: UploadedDocument) -> do
        equal target.documentName decoded._id
        equal (Nothing :: Maybe String) decoded._rev
        equal 4 $ length $ keys decoded._attachments
        assert "Default translation table must be included" $ isJust $ lookup "translationtable.json" decoded._attachments
  test "Attachment encoding preserves UTF-8" do
    equal "w6k=" $ utf8Base64 "\x00e9"
  test "Missing repository is rejected by a read-only probe" do
    withRepositoryMock 404 \mock -> do
      result <- attempt $ probeRepository Nothing mock.url
      assert "Missing repository must fail" $ case result of
        Left _ -> true
        Right _ -> false
      calls <- liftEffect mock.requests
      equal [ "GET" ] $ _.method <$> calls
  test "Repository authentication failure is not treated as missing" do
    withRepositoryMock 401 \mock -> do
      result <- attempt $ probeRepository Nothing mock.url
      assert "Unauthorized access must fail" $ case result of
        Left _ -> true
        Right _ -> false
      calls <- liftEffect mock.requests
      equal [ "GET" ] $ _.method <$> calls
  test "Writes the model and sidecars with one authenticated PUT" do
    target <- resolveTarget stableUri (source "@2.7")
    withRepositoryMock 201 \mock -> do
      let document = prepareDocument target Nothing (DomeinFile defaultDomeinFileRecord) [] emptyStableIdMapping
      storeDocument (Just { username: "tester", password: "test-password" }) mock.url document
      calls <- liftEffect mock.requests
      equal [ "PUT" ] $ _.method <$> calls
      equal [ "Basic " <> utf8Base64 "tester:test-password" ] $ _.authorization <$> calls
      case calls of
        [ call ] -> case readJSON call.body of
          Left errors -> assert (show errors) false
          Right (decoded :: UploadedDocument) -> do
            equal target.documentName decoded._id
            equal 4 $ length $ keys decoded._attachments
        _ -> assert "Exactly one request must be sent" false
  test "Revision conflicts fail without retrying or reporting success" do
    withRepositoryMock 409 \mock -> do
      result <- attempt $ storeDocument Nothing mock.url (write { _id: "existing", _rev: "1-stale" })
      assert "Conflict must fail" $ case result of
        Left _ -> true
        Right _ -> false
      calls <- liftEffect mock.requests
      equal [ "PUT" ] $ _.method <$> calls
  where
  stableUri = "model://perspectives.domains#abc123"
  source version = "domain model://perspectives.domains#Example" <> version <> "\n"
