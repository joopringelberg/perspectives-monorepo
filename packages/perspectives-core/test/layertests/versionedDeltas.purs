module Test.VersionedDeltas where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (any, sort)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Foreign.Object (insert, values)
import Foreign.Object as Object
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFileRecord, stampDomeinFileTypeVersion)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Persistence.DeltaStore (deltaStoreDocIdWithDeltaId, safeKey)
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
import Simple.JSON (readJSON', write, writeJSON)

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

  test "delta format parser distinguishes unmarked and v2" do
    Assert.assert "missing format marker should be treated as format 1" (parseDeltaFormatVersion "{\"a\":1}" == 1)
    Assert.assert "explicit version field should be treated as format 2" (parseDeltaFormatVersion "{\"deltaFormatVersion\":2}" == 2)

  suite "formats before 5 are retired" do
    let
      subject = ENR (EnumeratedRoleType "model://example.org#Sales$User")
      contextType = "model://example.org#Sales"
      roleType = "model://example.org#Sales$Order"
      property = "model://example.org#Sales$Order$Status"
      fixtures =
        [ { name: "context creation"
          , family: "UniverseContextDelta"
          , key: "context"
          , payload: writeJSON { subject, id: "context", contextType, deltaType: "ConstructEmptyContext" }
          }
        , { name: "role creation"
          , family: "UniverseRoleDelta"
          , key: "role"
          , payload: writeJSON
              { subject
              , id: "context"
              , contextType
              , roleType
              , authorizedRole: Nothing :: Maybe RoleType
              , roleInstances: [ "role" ]
              , deltaType: "ConstructEmptyRole"
              }
          }
        , { name: "context membership"
          , family: "ContextDelta"
          , key: "role"
          , payload: writeJSON
              { subject
              , contextInstance: "context"
              , contextType
              , roleType
              , roleInstance: "role"
              , destinationContext: Nothing :: Maybe String
              , destinationContextType: Nothing :: Maybe String
              , deltaType: "AddRoleInstancesToContext"
              }
          }
        , { name: "binding"
          , family: "RoleBindingDelta"
          , key: "role#binding"
          , payload: writeJSON
              { subject
              , filled: "role"
              , filledType: roleType
              , filler: Just "filler"
              , fillerType: Just roleType
              , oldFiller: Nothing :: Maybe String
              , oldFillerType: Nothing :: Maybe String
              , deltaType: "SetFirstBinding"
              }
          }
        , { name: "property"
          , family: "RolePropertyDelta"
          , key: "role#" <> property
          , payload: writeJSON
              { subject, id: "role", roleType, property, values: [ "approved" ], deltaType: "SetProperty" }
          }
        ]
    for_ fixtures \{ name, key, family, payload } ->
      test ("rejects pre-ordering, unmarked and format-4 " <> name <> " but reads format 5") do
        case parseIncomingDelta payload of
          Left _ -> pure unit
          Right _ -> Assert.assert "Pre-ordering payloads must not be converted to version zero" false
        case runExcept $ readJSON' payload of
          Left errors -> Assert.assert (show errors) false
          Right (fields :: Object.Object Foreign) -> do
            let
              singular =
                if Object.member "roleInstances" fields then
                  Object.insert "roleInstance" (write "role") $ Object.delete "roleInstances" fields
                else fields
              ordered = Object.insert "subject" (write "model://example.org#Sales$User")
                $ Object.insert "subjectKind" (write "ENR")
                $ Object.insert "resourceKey" (write key)
                $ Object.insert "resourceVersion" (write 1) singular
              format4 = Object.insert "deltaFormatVersion" (write 4) ordered
              format5 = Object.insert "deltaFamily" (write family) $ Object.insert "deltaFormatVersion" (write 5) ordered
              withoutFamily = Object.insert "deltaFormatVersion" (write 5) ordered
            for_ [ ordered, format4, withoutFamily ] \retired -> case parseIncomingDelta (writeJSON retired) of
              Left _ -> pure unit
              Right _ -> Assert.assert "Unmarked, format-4 and family-less payloads must be rejected" false
            case parseIncomingDelta (writeJSON format5) of
              Right _ -> pure unit
              Left message -> Assert.assert ("Format-5 payload must be readable: " <> message) false

  test "format-5 role-property deltas strip revision suffixes on parse" do
    let
      payload = writeJSON
        { deltaFormatVersion: 5
        , deltaFamily: "RolePropertyDelta"
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
      _ -> Assert.assert "format-5 role-property payload should parse" false

  test "format-5 role-property deltas carry provenance with unversioned types" do
    let
      payload = writeJSON
        { deltaFormatVersion: 5
        , deltaFamily: "RolePropertyDelta"
        , subject: "model://example.org#Sales$User@3.2"
        , subjectKind: "ENR"
        , resourceKey: "def:#role#model://example.org#Sales$Order$Status"
        , resourceVersion: 4
        , id: RoleInstance "def:#role"
        , roleType: "model://example.org#Sales$Order@3.2"
        , property: "model://example.org#Sales$Order$Status@3.2"
        , values: [ Value "approved" ]
        , deltaType: SetProperty
        , provenance:
            { subjectInstance: RoleInstance "def:#user"
            , subjectInstanceType: "model://example.org#Sales$User@3.2"
            , evaluationContext: "def:#sale"
            , evaluationContextType: "model://example.org#Sales@3.2"
            , objectInstance: RoleInstance "def:#role"
            , objectInstanceType: "model://example.org#Sales$Order@3.2"
            }
        }
    case parseIncomingDelta payload of
      Right (RolePropertyEnvelope (RolePropertyDelta { provenance: Just prov })) -> do
        Assert.equal (RoleInstance "def:#user") prov.subjectInstance
        Assert.equal (EnumeratedRoleType "model://example.org#Sales$User") prov.subjectInstanceType
        Assert.equal "model://example.org#Sales" (unwrap prov.evaluationContextType)
        Assert.equal (EnumeratedRoleType "model://example.org#Sales$Order") prov.objectInstanceType
      _ -> Assert.assert "format-5 provenance should parse" false

  test "delta store ids retain a short delta suffix" do
    let docId = deltaStoreDocIdWithDeltaId "def:#role" 4 (PerspectivesUser "def:#author") "0123456789abcdef9876543210"
    Assert.assert "doc ids should end in the short delta id suffix" (docId == "role_v0000000004_author|0123456789abcdef")

  test "delta store ids sort in numeric version order" do
    let
      author = PerspectivesUser "def:#author"
      ids = map (\v -> deltaStoreDocIdWithDeltaId "def:#role" v author "0123456789abcdef") [ 2, 9, 10, 100 ]
    Assert.equal ids (sort ids)

  suite "resource key normalisation" do
    for_ [ "", "def:#", "loc:cw_roles#", "rem:https://example.org/cw_roles/#", "pub:https://example.org/cw_roles/#", "pub:cw_roles#" ] \prefix ->
      for_
        [ { resource: "role", expected: "role" }
        , { resource: "context$External", expected: "context$External" }
        , { resource: "role#binding", expected: "role#binding" }
        , { resource: "role#model://example.org#Model$Context$Role$Name", expected: "role#Model$Name" }
        , { resource: "role#Model$Context$Role$Name", expected: "role#Model$Name" }
        , { resource: "role#Model$Name", expected: "role#Model$Name" }
        , { resource: "role#Name", expected: "role#Name" }
        ]
        \{ resource, expected } ->
          test ("normalises " <> prefix <> resource <> " idempotently") do
            let canonical = safeKey (prefix <> resource)
            Assert.equal expected canonical
            Assert.equal expected (safeKey canonical)

    test "preserves an empty key" do
      Assert.equal "" (safeKey "")

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

    test "accepts a property removal when the version lookup normalises the key again" do
      let
        propertyKey = "def:#repository#model://example.org#Model$Role$Name"
        lookupVersion key = Identity $ if safeKey key == "repository#Model$Name" then 1 else 0
      Assert.equal [] $ unwrap $ checkForGapsWith lookupVersion [ delta propertyKey 2 ]

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

    test "ignores already applied versions" do
      Assert.equal [] $ gaps 2 [ delta publicKey 0, delta localKey 1, delta publicKey 3 ]

  test "compiled types are stamped with the model version" do
    let contextId = "model://example.org#Sales"
    let base = defaultContext contextId "Sales" Domain Nothing (ArcPosition { line: 1, column: 1 }) Nothing
    let domeinFile = DomeinFile $ defaultDomeinFileRecord { contexts = insert contextId base defaultDomeinFileRecord.contexts }
    let (DomeinFile { contexts }) = stampDomeinFileTypeVersion "3.0" domeinFile
    let stamped = any (\(Context ctx) -> ctx.typeVersion == Just "3.0" && ctx.id == ContextType contextId) (values contexts)
    Assert.assert "stamping a compiled domein file should attach the model version to its contexts" stamped
