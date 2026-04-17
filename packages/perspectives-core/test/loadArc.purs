module Test.LoadArc where

import Prelude

import Control.Monad.AvarMonadAsk (modify)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Free (Free)
import Control.Promise (Promise, toAffE)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log, logShow)
import Foreign (unsafeToForeign)
import Main (createAccount_, runPDR, runPDR_)
import Perspectives.Authenticate (getPrivateKey)
import Perspectives.DomeinCache (removeDomeinFileFromCouchdb)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (saveMarkedResources)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Representation.Class.PersistentType (typeExists)
import Perspectives.Representation.TypeIdentifiers (ContextType(..))
import Perspectives.RunPerspectives (runPerspectivesWithoutCouchdb)
import Perspectives.SetupCouchdb (createUserDatabases)
import Perspectives.SetupUser (setupUser)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Sync.Transaction (UninterpretedTransactionForPeer(..))
import Perspectives.TypePersistence.LoadArc.FS (loadAndCompileArcFile, loadCompileAndCacheArcFile', loadCompileAndSaveArcFile, loadCompileAndSaveArcFile')
import Simple.JSON (write)
import Test.Perspectives.Utils (clearUserDatabase, runP, withSystem)
import Test.Unit (TestF, suite, test, testOnly)
import Test.Unit.Assert (assert)
import Unsafe.Coerce (unsafeCoerce)

-- | Reads accounts/orn2j1nh3q_test3_keypair.json, imports both keys via SubtleCrypto
-- | (ECDSA P-384), and stores them in IDB under <guid>_privateKey / <guid>_publicKey.
foreign import loadKeypairImpl :: String -> Effect (Promise Unit)

loadKeypair :: String -> Aff Unit
loadKeypair = loadKeypairImpl >>> toAffE

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "src/model"

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.loadArc" do

  testOnly "Test `setupUser`"
    ( do
        let userName = "testuser"
        let
          pouchdbUser =
            { systemIdentifier: userName <> "_macbook"
            , perspectivesUser: userName
            , userName: Nothing
            , password: Nothing
            , couchdbUrl: Nothing
            }

        -- Load the keypair from file into IDB before calling createAccount_.
        -- The key names follow authenticate.purs: takeGuid(perspectivesUser) <> "_privateKey" / "_publicKey".
        -- For perspectivesUser "testuser", takeGuid returns "testuser" unchanged (no '#' present).
        loadKeypair userName
        createAccount_
          pouchdbUser
          defaultRuntimeOptions
          Nothing
    -- runPDR_ userName
    --   (write pouchdbUser)
    --   defaultRuntimeOptions
    --   \_ -> pure unit
    )

  test "Test `setupUser`" do
    runPerspectivesWithoutCouchdb "testUser"
      ( do
          addAllExternalFunctions
          key <- getPrivateKey
          modify \(s@{ runtimeOptions: ro }) -> s { runtimeOptions = ro { privateKey = unsafeCoerce key } }
          getSystemIdentifier >>= createUserDatabases
          setupUser (UninterpretedTransactionForPeer <$> Nothing)
          saveMarkedResources
      )

  test "Test `withSystem` and `runPerspectivesWithoutCouchb`" do
    runPerspectivesWithoutCouchdb "testUser" do
      withSystem $ typeExists (ContextType "model://perspectives.domains#System") >>= case _ of
        false -> liftAff $ assert "System should be in the database" false
        true -> pure unit

  -- test "Load a model file and store it in Couchdb: reload and compare with original" $ runPerspectivesWithoutCouchdb "testUser" do
  --   -- 1. Load and save a model.
  --   messages <- runP $ withSystem $ loadCompileAndSaveArcFile' "contextAndRole" testDirectory
  --   if null messages
  --     then pure unit
  --     else do
  --       logShow messages
  --       assert "The file could not be saved" false
  --   -- 2. Reload it from the database into the cache.
  --   retrievedModel <- runP $ retrieveDomeinFile $ ModelUri "model:ContextAndRole"
  --   -- 3. Reload the file without caching or saving.
  --   r <- runP $ withSystem $ loadAndCompileArcFile "contextAndRole" testDirectory
  --   -- 4. Compare the model in cache with the model from the file.
  --   -- logShow retrievedModel
  --   case r of
  --     Left e -> assert ("The same file loaded the second time fails: " <> show e) false
  --     Right (Tuple reParsedModel _) -> do
  --       -- logShow (changeRevision Nothing reParsedModel)
  --       assert "The model reloaded from couchdb should equal the model loaded from file."
  --         -- NOTICE: this test will fail because retrievedModel will be Stable and reParsedModel will be Readable.
  --         (eq (changeRevision Nothing retrievedModel) (changeRevision Nothing (toStableDomeinFile reParsedModel)))
  --   runP $ removeDomeinFileFromCouchdb (ModelUri "model:ContextAndRole")

  test "Load model:System and cache it" do
    messages <- runP do
      addAllExternalFunctions
      -- 1. Load the required model:Couchdb.
      _ <- loadCompileAndCacheArcFile' "couchdb" modelDirectory
      -- 2. Load the required model:Serialise.
      _ <- loadCompileAndCacheArcFile' "serialise" modelDirectory
      -- 3. Try to load PerspectivesSystem.
      log "Starting on model:PerspectivesSystem"
      loadAndCompileArcFile "perspectivesSysteem" modelDirectory
    case messages of
      Left m -> do
        logShow messages
        assert "The file could not be parsed or compiled" false
      _ -> pure unit

  test "Load simpleChat and cache it" $ runP $ withSystem do
    messages <- loadAndCompileArcFile "simpleChat" modelDirectory
    case messages of
      Left m -> do
        logShow messages
        liftAff $ assert "The file could not be parsed or compiled" false
      _ -> pure unit

  test "Load TestBotActie and cache it" $ runP $ withSystem do
    messages <- loadAndCompileArcFile "testBotActie" modelDirectory
    case messages of
      Left m -> do
        logShow messages
        liftAff $ assert "The file could not be parsed or compiled" false
      _ -> pure unit

  test "Load model:Couchdb from file and store it in Couchdb" do
    messages <- runP do
      catchError (loadCompileAndSaveArcFile' "couchdb" modelDirectory)
        \e -> logShow e *> pure []
    if null messages then pure unit
    else do
      logShow messages
      assert "The file could not be parsed, compiled or saved" false
    runP $ removeDomeinFileFromCouchdb (ModelUri "model:Couchdb")

  test "Load model:Serialise from file and store it in Couchdb" do
    messages <- runP do
      catchError (loadCompileAndSaveArcFile' "serialise" modelDirectory)
        \e -> logShow e *> pure []
    if null messages then pure unit
    else do
      logShow messages
      assert "The file could not be parsed, compiled or saved" false
    runP $ removeDomeinFileFromCouchdb (ModelUri "model:Serialise")

  test "Load a model file and instances and store it in Couchdb" do
    messages <- runP do
      catchError
        ( do
            void $ loadCompileAndCacheArcFile' "couchdb" modelDirectory
            void $ loadCompileAndCacheArcFile' "serialise" modelDirectory
            loadCompileAndSaveArcFile "perspectivesSysteem" modelDirectory
        )
        \e -> logShow e *> pure []
    if null messages then pure unit
    else do
      logShow messages
      assert "The file could not be parsed, compiled or saved" false
    runP do
      removeDomeinFileFromCouchdb (ModelUri "model:System")
      clearUserDatabase
