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
import Data.Array (length)
import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (over, unwrap)
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
import Perspectives.Persistence.PendingTransactionStore (getAllPendingTransactions, storePendingTransaction)
import Perspectives.Persistence.ResourceVersionStore (getResourceVersion, setResourceVersion)
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Class.PersistentType (cacheInDomeinFile, getEnumeratedRole)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), PerspectivesUser(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..), StateIdentifier(..))
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.RunMonadPerspectivesTransaction (runEmbeddedIfNecessaryAwaitingSettlement, runMonadPerspectivesTransaction', runMonadPerspectivesTransactionAwaitingSettlement)
import Perspectives.Sync.DateTime (SerializableDateTime(..))
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.HandleTransaction (executeTransaction, verifyTransaction)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Transaction (PublicKeyInfo, Transaction(..))
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..), deduplicatePublicKeys, removeUnusedPublicKeys)
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)
import Simple.JSON (unsafeStringify, writeJSON)
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
    Binding.withFixture Binding.oldRemotePeer [] [] do
      lift do
        PerspectRol role <- getPerspectRol Binding.localPeer
        void $ cacheEntity (RoleInstance $ unwrap publicAuthor) $
          PerspectRol role { id = RoleInstance $ unwrap publicAuthor }
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
      allowPropertyUpdates
      signed <- signAs localAuthor
      executeTransaction $ transaction [ signed ] $
        EM.singleton publicAuthor { key, transportKey: "unused", deltas: [] }
      PerspectRol role <- lift $ getPerspectRol Binding.visibleId
      liftAff $ equal (Just [ Value "updated" ]) $ Object.lookup (unwrap Binding.visibleProperty) role.properties

  test "unblocks signed transactions on dependency arrival and clears stale records on replay" $
    withSigningFixture true \key -> do
      allowPropertyUpdates
      first <- signAs localAuthor
      second <- signVersionAs 2 "second" localAuthor
      let
        keys = EM.singleton publicAuthor { key, transportKey: "unused", deltas: [] }
        pending = over TransactionForPeer (\t -> t { timeStamp = SerializableDateTime top }) $
          transaction [ second ] keys
        dependency = transaction [ first ] keys
      runPeerTransaction pending
      runPeerTransaction pending
      blocked <- lift getAllPendingTransactions
      liftAff $ equal 1 (length blocked)
      lift $ runMonadPerspectivesTransaction' false (ENR Binding.userType) do
        executeTransaction dependency
        beforeSettlement <- lift getAllPendingTransactions
        PerspectRol before <- lift $ getPerspectRol Binding.visibleId
        liftAff do
          equal 1 (length beforeSettlement)
          equal (Just [ Value "updated" ]) $ Object.lookup (unwrap Binding.visibleProperty) before.properties
      PerspectRol role <- lift $ getPerspectRol Binding.visibleId
      remaining <- lift getAllPendingTransactions
      version <- lift $ getResourceVersion propertyResourceKey
      liftAff do
        equal (Just [ Value "second" ]) $ Object.lookup (unwrap Binding.visibleProperty) role.properties
        equal 2 version
        equal 0 (length remaining)
      lift $ storePendingTransaction dependency
        [ { author: localAuthor, resourceKey: propertyResourceKey, expectedVersion: 1 } ]
      runPeerTransaction dependency
      afterReplay <- lift getAllPendingTransactions
      liftAff $ equal 0 (length afterReplay)

  test "revalidates pending signatures and retains an invalid record after replay" $
    withSigningFixture true \key -> do
      signed <- signAs localAuthor
      let
        tampered = over SignedDelta (\d -> d { encryptedDelta = "{}" }) signed
        pending = transaction [ tampered ] $
          EM.singleton localAuthor { key, transportKey: "unused", deltas: [] }
      lift $ storePendingTransaction pending
        [ { author: localAuthor, resourceKey: propertyResourceKey, expectedVersion: 1 } ]
      runPeerTransaction $ transaction [] EM.empty
      remaining <- lift getAllPendingTransactions
      liftAff $ equal 1 (length remaining)

  for_ [ false, true ] \awaitSettlement ->
    test ("retries pending peers after local settlement, awaiting=" <> show awaitSettlement) $
      withSigningFixture true \key -> do
        allowPropertyUpdates
        second <- signVersionAs 2 "after local settlement" localAuthor
        let
          pending = transaction [ second ] $
            EM.singleton publicAuthor { key, transportKey: "unused", deltas: [] }
          runSettled =
            if awaitSettlement then runMonadPerspectivesTransactionAwaitingSettlement
            else runMonadPerspectivesTransaction'
        runPeerTransaction pending
        blocked <- lift getAllPendingTransactions
        liftAff $ equal 1 (length blocked)
        lift $ runSettled true (ENR Binding.userType) do
          lift $ setResourceVersion propertyResourceKey 1
          lift $ runEmbeddedIfNecessaryAwaitingSettlement true (ENR Binding.userType) (pure unit)
          stillBlocked <- lift getAllPendingTransactions
          liftAff $ equal 1 (length stillBlocked)
        PerspectRol role <- lift $ getPerspectRol Binding.visibleId
        remaining <- lift getAllPendingTransactions
        liftAff do
          equal (Just [ Value "after local settlement" ]) $ Object.lookup (unwrap Binding.visibleProperty) role.properties
          equal 0 (length remaining)

runPeerTransaction :: TransactionForPeer -> MonadPerspectivesTransaction Unit
runPeerTransaction incoming =
  lift $ runMonadPerspectivesTransaction' false (ENR Binding.userType) $
    executeTransaction incoming

allowPropertyUpdates :: MonadPerspectivesTransaction Unit
allowPropertyUpdates = do
  EnumeratedRole subject <- lift $ getEnumeratedRole Binding.userType
  let
    grant (Perspective p) = Perspective p
      { propertyVerbs = EM.singleton (ContextState $ StateIdentifier $ unwrap Binding.contextType)
          [ PropertyVerbs (PSet [ ENP Binding.visibleProperty ]) (PSet [ Consult, SetPropertyValue ]) ]
      }
  lift do
    cacheInDomeinFile Binding.userType $ EnumeratedRole subject { perspectives = map grant subject.perspectives }
    setResourceVersion propertyResourceKey 0

propertyResourceKey :: String
propertyResourceKey = unwrap Binding.visibleId <> "#" <> unwrap Binding.visibleProperty

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
transaction deltas publicKeys = TransactionForPeer
  { author: localAuthor
  , perspectivesSystem: ContextInstance "def:#test-system"
  , timeStamp: SerializableDateTime bottom
  , deltas
  , publicKeys
  }

signAs :: PerspectivesUser -> MonadPerspectivesTransaction SignedDelta
signAs = signVersionAs 1 "updated"

signVersionAs :: Int -> String -> PerspectivesUser -> MonadPerspectivesTransaction SignedDelta
signVersionAs version value author =
  over SignedDelta (\d -> d { author = author }) <$>
    signDelta
      ( writeJSON
          { resourceKey: propertyResourceKey
          , resourceVersion: version
          , deltaType: "SetProperty"
          , id: RoleInstance "visible"
          , roleType: Binding.visibleType
          , property: Binding.visibleProperty
          , values: [ Value value ]
          , subject: ENR Binding.userType
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
