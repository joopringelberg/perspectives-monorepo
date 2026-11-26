-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.VerificationBundles where

import Prelude

import Control.Monad.AvarMonadAsk as AA
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Crypto.Subtle.Constants.EC (ecdsa, p384)
import Crypto.Subtle.Key.Generate (ec, generateKeyPair)
import Crypto.Subtle.Key.Types (CryptoKeyPair(..), exportKey, jwk, sign, verify)
import Data.Either (Either(..), isLeft)
import Data.Array (elem)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (over, unwrap)
import Data.String (Pattern(..), Replacement(..), contains)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Foreign.Object as Object
import Perspectives.Authenticate (signDelta)
import Perspectives.CoreTypes (MonadPerspectivesTransaction)
import Perspectives.Data.EncodableMap as EM
import Perspectives.Deltas (addPublicKeysToTransaction)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.ModelDependencies as DEP
import Perspectives.Persistence.ResourceVersionStore (setResourceVersion)
import Perspectives.Persistence.DeltaStore (storeDeltaFromSignedDelta)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Class.PersistentType (cacheInDomeinFile, getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier(..))
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.Sync.Causal (currentCausalEpoch)
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.HandleTransaction (TransactionOutcome(..), executeTransaction, executeTransactionWithOutcome, verifyTransaction)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.StrippedDelta (stripResourceSchemes)
import Perspectives.Sync.VersionedDelta (currentMinReaderVersion, currentUniverseEpoch, signVersionedDelta)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Perspectives.Sync.Transaction (PublicKeyInfo, Transaction(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), currentTransactionForPeer, deduplicatePublicKeys, removeUnusedPublicKeys)
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)
import Simple.JSON (readJSON, unsafeStringify, writeJSON)
import Test.BindingReplacement as Binding
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Verification bundle identities" do
  test "combines aliases while preserving public lookup, distinct identities and signed evidence" do
    let
      first = Binding.visiblePropertyDelta
      second = Binding.roleDelta Binding.localPeer
      info = bundle { deltas = [ first ] }
      otherInfo = bundle { deltas = [ first, second ] }
    keys <- liftEffect $ deduplicatePublicKeys $ EM.fromFoldable
      [ Tuple localAuthor info
      , Tuple publicAuthor otherInfo
      , Tuple (PerspectivesUser "def:#local") info
      , Tuple (PerspectivesUser "other") bundle
      ]
    equal 2 $ Map.size $ unwrap keys
    equal (Just otherInfo) $ EM.lookup publicAuthor keys
    equal (Just bundle) $ EM.lookup (PerspectivesUser "other") keys
    let
      TransactionForPeer retained = removeUnusedPublicKeys $ transaction
        [ over SignedDelta (\d -> d { author = localAuthor }) first ]
        keys
    equal 1 $ Map.size $ unwrap retained.publicKeys
    equal (Just otherInfo) $ EM.lookup publicAuthor retained.publicKeys

  test "compares key material independently of JSON formatting" do
    keys <- liftEffect $ deduplicatePublicKeys $ EM.fromFoldable
      [ Tuple localAuthor bundle
      , Tuple publicAuthor bundle { key = "{ \"x\": \"same\", \"kty\": \"EC\" }" }
      ]
    equal 1 $ Map.size $ unwrap keys

  for_
    [ Tuple "signing" $ bundle { key = "{\"kty\":\"EC\",\"x\":\"different\"}" }
    , Tuple "transport" $ bundle { transportKey = "{\"kty\":\"RSA\",\"n\":\"different\"}" }
    ]
    \(Tuple kind conflicting) ->
      test ("rejects conflicting " <> kind <> " keys for aliases of one identity") do
        result <- liftEffect $ Exception.try $ deduplicatePublicKeys $ EM.fromFoldable
          [ Tuple localAuthor bundle, Tuple publicAuthor conflicting ]
        assert "Conflicting aliases must not silently select a key" $ isLeft result

  test "collects one outgoing bundle for mixed public and ordinary delta authors" $
    withSigningFixture true \_ -> do
      lift do
        PerspectRol role <- getPerspectRol Binding.localPeer
        void $ cacheEntity (RoleInstance $ unwrap publicAuthor) $
          PerspectRol role { id = RoleInstance $ unwrap publicAuthor }
      let
        role = UniverseRoleDelta
          { id: Binding.contextId
          , contextType: Binding.contextType
          , roleType: EnumeratedRoleType DEP.perspectivesUsers
          , roleInstance: Binding.localPeer
          , authorizedRole: Nothing
          , deltaType: RemoveRoleInstance
          , subject: ENR Binding.userType
          , resourceKey: unwrap Binding.localPeer
          , resourceVersion: 2
          }
      removed <- signVersionedDelta $ stripResourceSchemes role
      recreated <- signVersionedDelta $ stripResourceSchemes $
        UniverseRoleDelta (unwrap role) { deltaType = ConstructEmptyRole, resourceVersion = 0 }
      membership <- signVersionedDelta $ stripResourceSchemes $ ContextDelta
        { contextInstance: Binding.contextId
        , contextType: Binding.contextType
        , roleType: EnumeratedRoleType DEP.perspectivesUsers
        , roleInstance: Binding.localPeer
        , destinationContext: Nothing
        , destinationContextType: Nothing
        , deltaType: AddRoleInstancesToContext
        , subject: ENR Binding.userType
        , resourceKey: unwrap Binding.localPeer
        , resourceVersion: 38
        }
      lift $ for_ [ removed, recreated, membership ] storeDeltaFromSignedDelta
      input <- AA.get
      let
        deltas = map
          ( \author -> DeltaInTransaction
              { users: [ Binding.userId ]
              , delta: over SignedDelta (\d -> d { author = author }) Binding.visiblePropertyDelta
              }
          )
          [ localAuthor, publicAuthor ]
      Transaction output <- lift $ addPublicKeysToTransaction $
        over Transaction (\t -> t { deltas = deltas }) input
      liftAff do
        equal deltas output.deltas
        equal 1 $ Map.size $ unwrap output.publicKeys
        assert "The public locator must remain available" $ isJust $ EM.lookup publicAuthor output.publicKeys
        case EM.lookup publicAuthor output.publicKeys of
          Nothing -> assert "The author needs a verification bundle" false
          Just info -> do
            assert "The bundle must include the current constructor" $ elem recreated info.deltas
            assert "The bundle must include the current membership" $ elem membership info.deltas
            assert "The bundle must exclude historical removal" $ not $ elem removed info.deltas
            assert "The bundle must exclude the obsolete constructor" $ not $ elem (Binding.roleDelta Binding.localPeer) info.deltas

  for_ [ false, true ] \known ->
    for_ [ localAuthor, publicAuthor ] \bundleAuthor ->
      test ("verifies mixed signed authors using one bundle at " <> show bundleAuthor <> ", known=" <> show known) $
        withSigningFixture known \key -> do
          ordinary <- signAs localAuthor
          published <- signAs publicAuthor
          keys <- verifyTransaction $ transaction [ ordinary, published ] $
            EM.singleton bundleAuthor { key, transportKey: "unused", deltas: [ ordinary ] }
          liftAff do
            equal 1 $ Map.size keys
            assert "Both aliases must resolve to the verified identity" $
              Map.member (unschemePerspectivesUser localAuthor) keys
            assert "This test must exercise actual signatures" $
              isJust (unwrap ordinary).signature && isJust (unwrap published).signature

  test "uses the public locator when only the published identity key is known" $
    withSigningFixture true \key -> do
      lift do
        PerspectRol role <- getPerspectRol Binding.localPeer
        void $ cacheEntity Binding.localPeer $
          PerspectRol role { properties = Object.delete DEP.perspectivesUsersPublicKey role.properties }
      signed <- signAs localAuthor
      keys <- verifyTransaction $ transaction [ signed ] $
        EM.singleton publicAuthor { key, transportKey: "unused", deltas: [] }
      liftAff $ equal 1 $ Map.size keys

  test "does not replace a known local identity key with a public alias" $
    withSigningFixture true \key -> do
      CryptoKeyPair other <- liftAff $ generateKeyPair (ec ecdsa p384) true [ sign, verify ]
      localKey <- liftAff $ unsafeStringify <$> exportKey jwk other.publicKey
      lift do
        PerspectRol role <- getPerspectRol Binding.localPeer
        void $ cacheEntity Binding.localPeer $
          PerspectRol role { properties = Object.insert DEP.perspectivesUsersPublicKey [ Value localKey ] role.properties }
      signed <- signAs publicAuthor
      result <- try $ verifyTransaction $ transaction [ signed ] $
        EM.singleton publicAuthor { key, transportKey: "unused", deltas: [] }
      liftAff $ assert "A public alias must not displace the locally known key" $ isLeft result

  test "still rejects a tampered signed delta using a public author alias" $
    withSigningFixture true \key -> do
      signed <- signAs publicAuthor
      let tampered = over SignedDelta (\d -> d { encryptedDelta = "{}" }) signed
      result <- try $ verifyTransaction $ transaction [ tampered ] $
        EM.singleton localAuthor { key, transportKey: "unused", deltas: [] }
      liftAff $ assert "Alias lookup must not bypass signature verification" $ isLeft result

  test "executes a signed ordinary-author update with only a public-alias bundle" $
    withSigningFixture true \key -> do
      EnumeratedRole subject <- lift $ getEnumeratedRole Binding.userType
      let
        grant (Perspective p) = Perspective p
          { propertyVerbs = EM.singleton (ContextState $ StateIdentifier $ unwrap Binding.contextType)
              [ PropertyVerbs (PSet [ ENP Binding.visibleProperty ]) (PSet [ Consult, SetPropertyValue ]) ]
          }
      lift do
        cacheInDomeinFile Binding.userType $ EnumeratedRole subject { perspectives = map grant subject.perspectives }
        setResourceVersion (unwrap Binding.visibleId <> "#" <> unwrap Binding.visibleProperty) 0
      signed <- signAs localAuthor
      executeTransaction $ transaction [ signed ] $
        EM.singleton publicAuthor { key, transportKey: "unused", deltas: [] }
      PerspectRol role <- lift $ getPerspectRol Binding.visibleId
      liftAff $ equal (Just [ Value "updated" ]) $ Object.lookup (unwrap Binding.visibleProperty) role.properties

  test "every signed delta carries the universe, the minimal reader version and its transaction slot" $
    withSigningFixture true \_ -> do
      let
        membership = ContextDelta
          { contextInstance: Binding.contextId
          , contextType: Binding.contextType
          , roleType: EnumeratedRoleType DEP.perspectivesUsers
          , roleInstance: Binding.localPeer
          , destinationContext: Nothing
          , destinationContextType: Nothing
          , deltaType: AddRoleInstancesToContext
          , subject: ENR Binding.userType
          , resourceKey: unwrap Binding.localPeer
          , resourceVersion: 1
          }
      first <- signVersionedDelta $ stripResourceSchemes membership
      second <- signVersionedDelta $ stripResourceSchemes membership
      liftAff case readJSON (unwrap first).encryptedDelta, readJSON (unwrap second).encryptedDelta of
        Right (a :: SlotFields), Right (b :: SlotFields) -> do
          equal currentUniverseEpoch a.universeEpoch
          equal currentMinReaderVersion a.minReaderVersion
          equal a.transactionId b.transactionId
          equal 0 a.transactionPosition
          equal 1 b.transactionPosition
        _, _ -> assert "Signed deltas must carry the transaction-level fields" false

  test "rejects a delta signed into another universe, or without a universe" $
    for_ [ replace "\"universeEpoch\":\"" "\"universeEpoch\":\"other-", replace ("\"universeEpoch\":\"" <> currentUniverseEpoch <> "\"") "\"other\":0" ] \rewrite ->
      withSigningFixture true \key -> do
        grantVisibleProperty
        signed <- signAsWith rewrite localAuthor
        outcome <- executeTransactionWithOutcome $ transaction [ signed ] $
          EM.singleton publicAuthor { key, transportKey: "unused", deltas: [] }
        PerspectRol role <- lift $ getPerspectRol Binding.visibleId
        liftAff do
          assert "A delta outside the current universe must be rejected" case outcome of
            Rejected reason -> contains (Pattern "outside the current Perspectives Universe") reason
            _ -> false
          equal (Just [ Value "visible-value" ]) $ Object.lookup (unwrap Binding.visibleProperty) role.properties

  test "keeps a transaction pending when a delta requires a newer reader or has an unknown family" $
    for_
      [ replace "\"deltaFormatVersion\":5" "\"deltaFormatVersion\":6,\"minReaderVersion\":6"
      , replace "\"deltaFormatVersion\":5" "\"deltaFormatVersion\":6"
      , replace "\"deltaFamily\":\"RolePropertyDelta\"" "\"deltaFamily\":\"FutureDelta\""
      ]
      \rewrite -> withSigningFixture true \key -> do
        grantVisibleProperty
        signed <- signAsWith rewrite localAuthor
        result <- try $ executeTransactionWithOutcome $ transaction [ signed ] $
          EM.singleton publicAuthor { key, transportKey: "unused", deltas: [] }
        PerspectRol role <- lift $ getPerspectRol Binding.visibleId
        liftAff do
          assert "A delta for a newer reader must throw, so that the transaction stays pending" $ isLeft result
          equal (Just [ Value "visible-value" ]) $ Object.lookup (unwrap Binding.visibleProperty) role.properties

  test "applies a newer format that older readers may read, ignoring its unknown fields" $
    withSigningFixture true \key -> do
      grantVisibleProperty
      signed <- signAsWith (replace "\"deltaFormatVersion\":5" "\"deltaFormatVersion\":6,\"minReaderVersion\":5,\"futureField\":{\"x\":1}") localAuthor
      outcome <- executeTransactionWithOutcome $ transaction [ signed ] $
        EM.singleton publicAuthor { key, transportKey: "unused", deltas: [] }
      PerspectRol role <- lift $ getPerspectRol Binding.visibleId
      liftAff do
        assert ("A readable newer format must execute, got " <> show outcome) case outcome of
          Executed -> true
          _ -> false
        equal (Just [ Value "updated" ]) $ Object.lookup (unwrap Binding.visibleProperty) role.properties

type SlotFields = { universeEpoch :: String, minReaderVersion :: Int, transactionId :: String, transactionPosition :: Int }

replace :: String -> String -> String -> String
replace pattern replacement = String.replace (Pattern pattern) (Replacement replacement)

grantVisibleProperty :: MonadPerspectivesTransaction Unit
grantVisibleProperty = do
  EnumeratedRole subject <- lift $ getEnumeratedRole Binding.userType
  let
    grant (Perspective p) = Perspective p
      { propertyVerbs = EM.singleton (ContextState $ StateIdentifier $ unwrap Binding.contextType)
          [ PropertyVerbs (PSet [ ENP Binding.visibleProperty ]) (PSet [ Consult, SetPropertyValue ]) ]
      }
  lift do
    cacheInDomeinFile Binding.userType $ EnumeratedRole subject { perspectives = map grant subject.perspectives }
    setResourceVersion (unwrap Binding.visibleId <> "#" <> unwrap Binding.visibleProperty) 0

localAuthor :: PerspectivesUser
localAuthor = PerspectivesUser "local"

publicAuthor :: PerspectivesUser
publicAuthor = PerspectivesUser "pub:https://example.org/public/#local"

bundle :: PublicKeyInfo
bundle =
  { key: "{\"kty\":\"EC\",\"x\":\"same\"}"
  , transportKey: "{\"kty\":\"RSA\",\"n\":\"same\"}"
  , deltas: []
  }

transaction :: Array SignedDelta -> EM.EncodableMap PerspectivesUser PublicKeyInfo -> TransactionForPeer
transaction deltas publicKeys = currentTransactionForPeer
  { author: localAuthor
  , perspectivesSystem: ContextInstance "def:#test-system"
  , timeStamp: SerializableDateTime bottom
  , deltas
  , publicKeys
  }

signAs :: PerspectivesUser -> MonadPerspectivesTransaction SignedDelta
signAs = signAsWith identity

-- | Sign the standard property delta after rewriting its JSON text (to construct wire-format variants).
signAsWith :: (String -> String) -> PerspectivesUser -> MonadPerspectivesTransaction SignedDelta
signAsWith rewrite author =
  over SignedDelta (\d -> d { author = author }) <$>
    signDelta
      ( rewrite $ writeJSON
          { resourceKey: unwrap Binding.visibleId <> "#" <> unwrap Binding.visibleProperty
          , resourceVersion: 1
          , deltaType: "SetProperty"
          , deltaFamily: "RolePropertyDelta"
          , deltaFormatVersion: 5
          , universeEpoch: currentUniverseEpoch
          , subjectKind: "ENR"
          , id: RoleInstance "visible"
          , roleType: Binding.visibleType
          , property: Binding.visibleProperty
          , values: [ Value "updated" ]
          , subject: unwrap Binding.userType
          , causal:
              { epoch: currentCausalEpoch
              , dot: { actor: writeJSON [ unwrap (unschemePerspectivesUser author), "verification-test-installation" ], counter: 1 }
              , context: ([] :: Array { actor :: String, counter :: Int })
              }
          }
      )

withSigningFixture :: Boolean -> (String -> MonadPerspectivesTransaction Unit) -> Aff Unit
withSigningFixture known action =
  Binding.withFixture Binding.oldRemotePeer [] [] do
    CryptoKeyPair pair <- liftAff $ generateKeyPair (ec ecdsa p384) true [ sign, verify ]
    key <- liftAff $ unsafeStringify <$> exportKey jwk pair.publicKey
    lift do
      AA.modify \s -> s { runtimeOptions = s.runtimeOptions { privateKey = Just $ unsafeCoerce pair.privateKey } }
      PerspectRol role <- getPerspectRol Binding.localPeer
      let
        properties =
          if known then Object.insert DEP.perspectivesUsersPublicKey [ Value key ] role.properties
          else Object.delete DEP.perspectivesUsersPublicKey role.properties
      void $ cacheEntity Binding.localPeer $ PerspectRol role { properties = properties }
      void $ cacheEntity (RoleInstance $ unwrap publicAuthor) $
        PerspectRol role { id = RoleInstance $ unwrap publicAuthor, properties = properties }
    action key
