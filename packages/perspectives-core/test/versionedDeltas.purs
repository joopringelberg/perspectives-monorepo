module Test.VersionedDeltas where

import Prelude

import Data.Array (any)
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign.Object (insert, values)
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFileRecord, stampDomeinFileTypeVersion)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Persistence.DeltaStore (deltaStoreDocIdWithDeltaId)
import Perspectives.Representation.Context (Context(..), ContextKind(..), defaultContext)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.Sync.CanonicalJson (canonicalizeJsonString, computeDeltaId)
import Perspectives.Sync.HandleTransaction (checkForGapsWith)
import Perspectives.Sync.VersionedDelta (DeltaEnvelope(..), parseDeltaFormatVersion, parseIncomingDelta)
import Perspectives.TypesForDeltas (RolePropertyDelta(..), RolePropertyDeltaType(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)
import Control.Monad.Free (Free)
import Simple.JSON (writeJSON)

main :: Effect Unit
main = runTest theSuite

theSuite :: Free TestF Unit
theSuite = suite "Versioned delta phase 0/1" do
  test "canonical JSON sorts object keys" do
    canonical <- liftEffect $ canonicalizeJsonString "{\"b\":1,\"a\":2}"
    Assert.assert "canonical JSON should sort keys" (canonical == "{\"a\":2,\"b\":1}")

  test "delta id is deterministic for exact bytes" do
    deltaId1 <- liftAff $ computeDeltaId "def:#author" "{\"a\":1}"
    deltaId2 <- liftAff $ computeDeltaId "def:#author" "{\"a\":1}"
    deltaId3 <- liftAff $ computeDeltaId "def:#author" "{\"a\":2}"
    Assert.assert "same author and payload should yield the same delta id" (deltaId1 == deltaId2)
    Assert.assert "different payload bytes should yield a different delta id" (deltaId1 /= deltaId3)

  test "delta format parser distinguishes legacy and v2" do
    Assert.assert "missing version field should be treated as legacy format 1" (parseDeltaFormatVersion "{\"a\":1}" == 1)
    Assert.assert "explicit version field should be treated as format 2" (parseDeltaFormatVersion "{\"deltaFormatVersion\":2}" == 2)

  test "v2 role-property deltas strip revision suffixes on parse" do
    let
      payload = writeJSON
        { deltaFormatVersion: 2
        , subject: "model://example.org#Sales$User@3.2"
        , subjectKind: "ENR"
        , resourceKey: "def:#role#model://example.org#Sales$Order$Status"
        , resourceVersion: 4
        , id: RoleInstance "def:#role"
        , roleType: "model://example.org#Sales$Order@3.2"
        , property: "model://example.org#Sales$Order$Status@3.2"
        , values: [ Value "approved" ]
        , deltaType: SetProperty
        }
    case parseIncomingDelta payload of
      Right (RolePropertyEnvelope (RolePropertyDelta delta)) -> do
        Assert.assert "subject should parse as an enumerated role" (delta.subject == ENR (EnumeratedRoleType "model://example.org#Sales$User"))
        Assert.assert "role type should drop the version suffix" (delta.roleType == EnumeratedRoleType "model://example.org#Sales$Order")
        Assert.assert "property type should drop the version suffix" (delta.property == EnumeratedPropertyType "model://example.org#Sales$Order$Status")
      _ -> Assert.assert "format-2 role-property payload should parse" false

  test "delta store ids retain a short delta suffix" do
    let docId = deltaStoreDocIdWithDeltaId "def:#role" 4 (PerspectivesUser "def:#author") "0123456789abcdef9876543210"
    Assert.assert "doc ids should end in the short delta id suffix" (docId == "role_v4_author|0123456789abcdef")

  suite "version gaps across storage addresses" do
    let
      localKey = "def:#repository#binding"
      publicKey = "pub:https://example.org/cw_repositories/#repository#binding"
      author = PerspectivesUser "alice"
      delta resourceKey resourceVersion = { resourceKey, resourceVersion, author }
      gaps version infos = unwrap $ checkForGapsWith
        (\key -> Identity $ if key == "repository#binding" then version else 0)
        infos

    test "combines local and public history, duplicates, and out-of-order versions" do
      Assert.equal [] $ gaps 0
        [ delta publicKey 3
        , delta localKey 1
        , delta publicKey 2
        , delta localKey 1
        ]

    test "looks up the existing version using the canonical resource key" do
      Assert.equal [] $ gaps 1 [ delta publicKey 2, delta localKey 3 ]

    test "preserves the original address and author of a genuinely missing version" do
      Assert.equal [ { author, resourceKey: publicKey, expectedVersion: 2 } ] $
        gaps 0 [ delta localKey 1, delta publicKey 3 ]

    test "still detects missing initial history" do
      Assert.equal [ { author, resourceKey: publicKey, expectedVersion: 1 } ] $
        gaps 0 [ delta publicKey 2, delta publicKey 3 ]

    test "does not combine different resources or binding and property histories" do
      let
        propertyKey = "def:#repository#model://example.org#Model$Role$Name"
        otherKey = "pub:https://example.org/cw_repositories/#other#binding"
      Assert.equal
        [ { author, resourceKey: propertyKey, expectedVersion: 1 }
        , { author, resourceKey: otherKey, expectedVersion: 1 }
        ]
        $ gaps 0 [ delta localKey 1, delta propertyKey 2, delta otherKey 2 ]

    test "ignores legacy and already applied versions" do
      Assert.equal [] $ gaps 2 [ delta publicKey (-1), delta localKey 1, delta publicKey 3 ]

  test "compiled types are stamped with the model version" do
    let contextId = "model://example.org#Sales"
    let base = defaultContext contextId "Sales" Domain Nothing (ArcPosition { line: 1, column: 1 }) Nothing
    let domeinFile = DomeinFile $ defaultDomeinFileRecord { contexts = insert contextId base defaultDomeinFileRecord.contexts }
    let (DomeinFile { contexts }) = stampDomeinFileTypeVersion "3.0" domeinFile
    let stamped = any (\(Context ctx) -> ctx.typeVersion == Just "3.0" && ctx.id == ContextType contextId) (values contexts)
    Assert.assert "stamping a compiled domein file should attach the model version to its contexts" stamped
