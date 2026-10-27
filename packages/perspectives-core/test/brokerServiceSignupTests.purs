module Test.BrokerServiceSignupTests where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes, head, null)
import Data.Either (Either(..), isRight)
import Data.Foldable (any, elem, for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, bracket, error, finally, launchAff_)
import Effect.Aff.AVar (tryRead)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign.Object (fromFoldable)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Perspectives.AMQP.RabbitMQManagement (virtualHost)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), MonadPerspectives, (##=), (##>))
import Perspectives.Cuid2 (cuid2)
import Perspectives.DataUpgrade.PatchModels (patchModels)
import Perspectives.DataUpgrade.RecompileLocalModels (recompileLocalModel)
import Perspectives.Identifiers (modelUri2LocalName, unversionedModelUri)
import Perspectives.Instances.ObjectGetters (binding, context, getEnumeratedRoleInstances, getUnlinkedRoleInstances)
import Perspectives.Logging (ansiCyan, ansiRed, infoTest, traceTest)
import Perspectives.ModelDependencies (identifiableFirstName, identifiableLastName, mySocialEnvironment, socialEnvironmentPersons, sysUser)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.PerspectivesState (defaultRuntimeOptions, setTopicLogLevel)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), IndexedContext(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runMonadPerspectivesTransaction', shareWithPeers)
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), Stable, StableIdMapping)
import Perspectives.Sidecar.ToStable (toStable)
import Perspectives.TypePersistence.LoadArc (loadCompileAndStoreArcFile_)
import Test.LocalCouchdbTestSupport (addLocalCouchdbCredentials)
import Test.Layer3Scaffold (LogConfiguration, ModelTest, applyLogConfigurationToPDRs)
import Test.PDRInstance (SynchronisationResult, noBus, pollUntil, snapshotExists, startPDRInstanceFromSnapshotWithHook, testPouchdbUser, withPDRCached)
import Test.PDRInstance.Types (PDRInstance, runInPDR)
import Test.RabbitMQCtl (addAdminUser, deleteQueue, deleteUser, purgeOwnQueue, queueExists, queueHasConsumer, userExists)
import Test.Test.PDRInstance.SubscribePDRtoAMQP (subscribePDRtoAMQP)
import Test.SinglePDRScaffold (TestModelLoadMethod(..), emptyLogConfiguration, executeModelTest, loadModel)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  results <- for [ endSubscriptionTestContext, leaderTerminatesContractTestContext ] \terminationTest -> do
    result <- getSignupResultsForTermination terminationTest
    pure $ Tuple terminationTest result
  liftEffect $ runTest do
    for_ results \(Tuple terminationTest result) ->
      suite terminationTest (signupSuite result)

type SignupResults =
  { signupResults :: Array SynchronisationResult
  , aliceSawBob :: Boolean
  , bobCouchdbOutcome :: Maybe CouchdbOutcome
  -- Bob's RabbitMQ account and queue, as they were while he was subscribed.
  , bobCredentials :: Maybe BrokerCredentials
  -- Set after either termination scenario; Nothing if we could not check.
  , afterTermination :: Maybe TerminationOutcome
  }

type BrokerCredentials = { login :: String, queueId :: String }

type CouchdbOutcome =
  { hasAliceServer :: Boolean
  , hasOwnAccount :: Boolean
  }

type TerminationOutcome =
  { aliceContractGone :: Boolean
  , rabbitUserGone :: Boolean
  , rabbitQueueGone :: Boolean
  }

getSignupResults :: Aff SignupResults
getSignupResults = getSignupResultsForTermination endSubscriptionTestContext

-- Each termination consumes the subscription, so restore fresh PDRs for each scenario.
getSignupResultsForTermination :: String -> Aff SignupResults
getSignupResultsForTermination terminationTest =
  bracket
    ( do
        adminPassword <- liftEffect $ cuid2 "brokerServiceSignupTestAdmin"
        exists <- userExists rabbitAdminName
        when exists $ deleteUser rabbitAdminName
        addAdminUser virtualHost rabbitAdminName adminPassword
        pure adminPassword
    )
    (\_ -> deleteUser rabbitAdminName)
    (runSignupScenario terminationTest)

runSignupScenario :: String -> String -> Aff SignupResults
runSignupScenario terminationTest adminPassword = do
  aliceSnapshotExists <- snapshotExists aliceSnapshotDirectory
  unless aliceSnapshotExists
    $ throwError
    $ error ("Required Alice post-reboot snapshot is missing: " <> aliceSnapshotDirectory)

  bracket
    (startPDRInstanceFromSnapshotWithHook (testPouchdbUser "alice") defaultRuntimeOptions (Just ansiCyan) noBus aliceSnapshotDirectory purgeOwnQueue)
    (\alice -> finally alice.shutdown (cleanupAliceSubscription alice))
    \alice ->
      withPDRCached (testPouchdbUser "bob") defaultRuntimeOptions (Just ansiRed) noBus bobSnapshotDirectory \bob -> do
        addLocalCouchdbCredentials alice
        addLocalCouchdbCredentials bob

        applyLogConfigurationToPDRs alice bob signupTestModelConfiguration.setupLogConfiguration

        -- CouchdbManagement has been patched in the universe snapshots already
        patchSomeModels alice
        patchSomeModels bob

        -- Alice compiles first and coins the stable ids; Bob reuses her mapping so both
        -- PDRs share the same CUIDs for the test model's types.
        aliceMapping <- compileTestModel alice Nothing

        for_ signupTestModelConfiguration.testModelLoadMethods (loadModel bob)

        void $ compileTestModel bob (Just aliceMapping)

        bobTestApp <- pollUntil 100 (Milliseconds 100.0)
          "Broker signup test app to appear in Bob's PDR"
          ( runInPDR bob do
              IndexedContext indexed <- toStable (IndexedContext signupTestModelConfiguration.indexedTestContext)
              traceTest ("Looking up stable indexed context for signup test: " <> show indexed)
              lookupIndexedContext indexed
          )

        aliceTestApp <- pollUntil 100 (Milliseconds 100.0)
          "Broker signup test app to appear in Alice's PDR"
          ( runInPDR alice do
              IndexedContext indexed <- toStable (IndexedContext signupTestModelConfiguration.indexedTestContext)
              lookupIndexedContext indexed
          )

        giveAliceAdminCredentials alice adminPassword
        ensureAliceSubscribed alice

        let
          tests = map
            ( \testCase ->
                if testCase.testContextTypeName == endSubscriptionTestContext then
                  testCase { testContextTypeName = terminationTest }
                else testCase
            )
            signupTestModelConfiguration.tests

        results <- for tests \testCase -> do
          let
            runsOnAlice = testCase.testContextTypeName == createCouchdbAccountTestContext
              || testCase.testContextTypeName == leaderTerminatesContractTestContext
            testPdr =
              if runsOnAlice then alice
              else bob
            testApp =
              if runsOnAlice then aliceTestApp
              else bobTestApp

          when (testCase.testContextTypeName == terminationTest) $ giveAliceAdminCredentials alice adminPassword
          applyLogConfigurationToPDRs alice bob
            (fromMaybe signupTestModelConfiguration.setupLogConfiguration testCase.logConfiguration)

          result <- executeModelTest
            testPdr
            testApp
            testCase.testContextTypeName
            emptyLogConfiguration
            signupTestModelConfiguration

          runInPDR testPdr do
            traceTest ("Broker Service test result: " <> show result)

          -- Check synchronisation before a later test can terminate the subscription.
          aliceSawBob <-
            if testCase.testContextTypeName == signupTestContext then
              pollUntil 120 (Milliseconds 500.0)
                "Alice to receive Bob's BrokerContract account-holder details"
                do
                  seesBob <- aliceHasBobAccount alice
                  pure if seesBob then Just true else Nothing
            else pure false

          couchdbOutcome <-
            if testCase.testContextTypeName == createCouchdbAccountTestContext then do
              settled <- attempt $ pollUntil 120 (Milliseconds 500.0)
                "Bob to receive Alice's Couchdb Server and his own Accounts role"
                do
                  outcome <- getBobCouchdbOutcome bob
                  pure
                    if outcome.hasAliceServer && outcome.hasOwnAccount then Just outcome
                    else Nothing
              case settled of
                Right outcome -> pure $ Just outcome
                Left _ -> Just <$> getBobCouchdbOutcome bob
            else pure Nothing

          -- Remember Bob's account and queue before a later test terminates the subscription.
          credentials <-
            if testCase.testContextTypeName == signupTestContext then runInPDR bob currentBrokerCredentials
            else pure Nothing

          pure { result, aliceSawBob, couchdbOutcome, credentials }

        let bobCredentials = head $ catMaybes $ map _.credentials results
        let bobCouchdbOutcome = head $ catMaybes $ map _.couchdbOutcome results

        -- Termination must remove the contract on Alice's side and the user and queue from RabbitMQ.
        afterTermination <- case bobCredentials of
          Nothing -> pure Nothing
          Just credentials -> do
            outcome <- attempt $ pollUntil 120 (Milliseconds 500.0)
              "Alice's contract to be removed and Bob's RabbitMQ user and queue to be deleted"
              do
                aliceStillHasBob <- aliceHasBobAccount alice
                userStillThere <- userExists credentials.login
                queueStillThere <- queueExists virtualHost credentials.queueId
                pure
                  if aliceStillHasBob || userStillThere || queueStillThere then Nothing
                  else Just unit
            if isRight outcome then pure $ Just { aliceContractGone: true, rabbitUserGone: true, rabbitQueueGone: true }
            else do
              aliceStillHasBob <- aliceHasBobAccount alice
              userStillThere <- userExists credentials.login
              queueStillThere <- queueExists virtualHost credentials.queueId
              pure $ Just
                { aliceContractGone: not aliceStillHasBob
                , rabbitUserGone: not userStillThere
                , rabbitQueueGone: not queueStillThere
                }

        pure { signupResults: map _.result results, aliceSawBob: any _.aliceSawBob results, bobCouchdbOutcome, bobCredentials, afterTermination }

ensureAliceSubscribed :: PDRInstance -> Aff Unit
ensureAliceSubscribed alice = do
  existing <- runInPDR alice currentBrokerCredentials
  case existing of
    Just _ -> throwError $ error "Alice's base snapshot must not already contain a broker subscription"
    Nothing -> pure unit
  brokerService <- runInPDR alice do
    IndexedContext index <- toStable (IndexedContext myBrokersContext)
    myBrokers <- lookupIndexedContext index >>= case _ of
      Nothing -> throwError $ error "Alice's Broker Services app is missing"
      Just ctx -> pure ctx
    managedBrokers <- toStable (EnumeratedRoleType managedBrokersRole)
    services <- myBrokers ##= getEnumeratedRoleInstances managedBrokers >=> binding >=> context
    case head services of
      Nothing -> throwError $ error "Alice has no managed broker service to subscribe to"
      Just service -> pure service
  subscribePDRtoAMQP brokerService alice
  void $ pollUntil 120 (Milliseconds 500.0)
    "Alice's registered RabbitMQ account and active queue consumer"
    do
      credentials <- runInPDR alice currentBrokerCredentials
      case credentials of
        Nothing -> pure Nothing
        Just { login, queueId } -> do
          accountExists <- userExists login
          hasConsumer <- queueHasConsumer virtualHost queueId
          pure if accountExists && hasConsumer then Just unit else Nothing

cleanupAliceSubscription :: PDRInstance -> Aff Unit
cleanupAliceSubscription alice = do
  credentials <- runInPDR alice currentBrokerCredentials
  for_ credentials \{ login, queueId } -> do
    accountExists <- userExists login
    when accountExists $ deleteUser login
    exists <- queueExists virtualHost queueId
    when exists $ deleteQueue virtualHost queueId

rabbitAdminName :: String
rabbitAdminName = "bssu_test_admin"

-- | Give the administrator of Alice's broker service the credentials of the temporary RabbitMQ administrator.
-- | They are not shared with peers.
giveAliceAdminCredentials :: PDRInstance -> String -> Aff Unit
giveAliceAdminCredentials pdr password = runInPDR pdr do
  IndexedContext myBrokersIndex <- toStable (IndexedContext myBrokersContext)
  mMyBrokers <- lookupIndexedContext myBrokersIndex
  for_ mMyBrokers \myBrokers -> do
    managedBrokersType <- toStable (EnumeratedRoleType managedBrokersRole)
    administratorType <- toStable (EnumeratedRoleType "model://perspectives.domains#BrokerServices$BrokerService$Administrator")
    adminUserName <- toStable (EnumeratedPropertyType "model://perspectives.domains#BrokerServices$BrokerService$Administrator$AdminUserName")
    adminPassword <- toStable (EnumeratedPropertyType "model://perspectives.domains#BrokerServices$BrokerService$Administrator$AdminPassword")
    brokerServices <- myBrokers ##= getEnumeratedRoleInstances managedBrokersType >=> binding >=> context
    for_ brokerServices \brokerService -> do
      administrators <- brokerService ##= getEnumeratedRoleInstances administratorType
      for_ administrators \administrator -> do
        void $ runMonadPerspectivesTransaction' doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser) do
          setProperty [ administrator ] adminUserName Nothing [ Value rabbitAdminName ]
          setProperty [ administrator ] adminPassword Nothing [ Value password ]
        infoTest ("Gave Alice's administrator " <> show administrator <> " the credentials of RabbitMQ administrator " <> rabbitAdminName)

patchSomeModels :: PDRInstance -> Aff Unit
patchSomeModels pdr = runInPDR pdr do
  setTopicLogLevel UPGRADE Debug
  patchModels
    ( fromFoldable
        [
        --   Tuple "model://perspectives.domains#CouchdbManagement" couchdbmanagement
        -- , Tuple "model://perspectives.domains#System" system
        ]
    )

-- success <- recompileLocalModel (ModelUri "model://perspectives.domains#CouchdbManagement")
-- infoTest ("Patched and recompiled CouchdbManagement in snapshot: " <> show success)
-- successSystem <- recompileLocalModel (ModelUri "model://perspectives.domains#System")
-- infoTest ("Patched and recompiled System in snapshot: " <> show successSystem)

-- | The credentials of the broker service this PDR is currently reading post from.
currentBrokerCredentials :: MonadPerspectives (Maybe BrokerCredentials)
currentBrokerCredentials = do
  bsAVar <- gets _.brokerService
  mbs <- liftAff $ tryRead bsAVar
  pure $ mbs <#> \{ login, queueId } -> { login, queueId }

foreign import couchdbmanagement :: String
foreign import brokerservices :: String

foreign import system :: String

compileTestModel :: PDRInstance -> Maybe StableIdMapping -> Aff StableIdMapping
compileTestModel pdr mMapping = do
  source <- readTextFile UTF8 testModelSource
  runInPDR pdr do
    infoTest ("Compiling and storing model from source: " <> testModelSource)
    compilationResult <- runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
      ( loadCompileAndStoreArcFile_
          (ModelUri testModel :: ModelUri Stable)
          source
          true
          (unsafePartial modelUri2LocalName $ unversionedModelUri testModel)
          testModelReadable
          Nothing
          mMapping
      )
    case compilationResult of
      Left errs -> throwError $ error ("Failed to compile and store model " <> testModel <> ": " <> show errs)
      Right (Tuple _ (Tuple _ mapping)) -> pure mapping

aliceHasBobAccount :: PDRInstance -> Aff Boolean
aliceHasBobAccount pdr = runInPDR pdr do
  IndexedContext myBrokersIndex <- toStable (IndexedContext myBrokersContext)
  mMyBrokers <- lookupIndexedContext myBrokersIndex
  case mMyBrokers of
    Nothing -> pure false
    Just myBrokers -> do
      managedBrokersType <- toStable (EnumeratedRoleType managedBrokersRole)
      accountsType <- toStable (EnumeratedRoleType brokerServiceAccountsRole)
      accountHolderType <- toStable (EnumeratedRoleType brokerContractAccountHolderRole)
      firstNameProperty <- toStable (EnumeratedPropertyType identifiableFirstName)
      lastNameProperty <- toStable (EnumeratedPropertyType identifiableLastName)

      brokerServices <- myBrokers ##= getEnumeratedRoleInstances managedBrokersType >=> binding >=> context
      contractMatches <- for brokerServices \brokerService -> do
        contracts <- brokerService ##= getUnlinkedRoleInstances accountsType >=> binding >=> context
        details <- for contracts \contract -> do
          accountHolders <- contract ##= getEnumeratedRoleInstances accountHolderType >=> binding
          for accountHolders \accountHolder -> do
            firstName <- accountHolder ##> getPropertyValues (ENP firstNameProperty)
            lastName <- accountHolder ##> getPropertyValues (ENP lastNameProperty)
            pure (firstName == Just (Value "bob") && lastName == Just (Value "bob_last"))
        pure $ any identity (join details)

      pure $ any identity contractMatches

getBobCouchdbOutcome :: PDRInstance -> Aff CouchdbOutcome
getBobCouchdbOutcome pdr = runInPDR pdr do
  IndexedContext myCouchdbAppIndex <- toStable (IndexedContext myCouchdbApp)
  mMyCouchdbApp <- lookupIndexedContext myCouchdbAppIndex
  mMySocialEnvironment <- lookupIndexedContext mySocialEnvironment
  case mMyCouchdbApp, mMySocialEnvironment of
    Just myCouchdbAppContext, Just mySocialEnvironmentContext -> do
      couchdbServersType <- toStable (EnumeratedRoleType couchdbServersRole)
      accountsType <- toStable (EnumeratedRoleType couchdbServerAccountsRole)
      servers <- myCouchdbAppContext ##= getEnumeratedRoleInstances couchdbServersType >=> binding >=> context
      accounts <- join <$> for servers (_ ##= getUnlinkedRoleInstances accountsType)
      accountFillers <- join <$> for accounts (_ ##= binding)
      ownPersons <- mySocialEnvironmentContext ##= getUnlinkedRoleInstances (EnumeratedRoleType socialEnvironmentPersons)
      pure
        { hasAliceServer: not $ null servers
        , hasOwnAccount: any (flip elem ownPersons) accountFillers
        }
    _, _ -> pure { hasAliceServer: false, hasOwnAccount: false }

signupSuite :: SignupResults -> TestSuite
signupSuite { signupResults, aliceSawBob, bobCouchdbOutcome, afterTermination } =
  suite "Broker Service signup tests" do
    for_ signupResults \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed in its PDR") do
          assert ("Test '" <> testName <> "' should succeed in its PDR") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Broker Service test failed: " <> show err) false

    test "Alice should see Bob's account-holder details" do
      assert "Alice should receive Bob's account-holder details through the Broker Service" aliceSawBob

    case bobCouchdbOutcome of
      Nothing -> test "Bob's Couchdb Server account outcome should be known" do
        assert "The Couchdb Server account test was not executed" false
      Just { hasAliceServer, hasOwnAccount } -> do
        test "Bob should receive Alice's Couchdb Server" do
          assert "Alice's Couchdb Server did not appear in Bob's Couchdb Management app" hasAliceServer
        test "Bob should receive an Accounts role filled with his own Persons instance" do
          assert "Bob's Couchdb Server account is absent or filled with another Persons instance" hasOwnAccount

    case afterTermination of
      Nothing -> test "Bob's RabbitMQ credentials should be known before terminating" do
        assert "Bob's broker credentials could not be read after signing up" false
      Just { aliceContractGone, rabbitUserGone, rabbitQueueGone } -> do
        test "The contract should be removed from Alice's PDR after termination" do
          assert "Alice's PDR still has Bob's contract" aliceContractGone
        test "Bob's user should be deleted from RabbitMQ after termination" do
          assert "Bob's RabbitMQ user still exists" rabbitUserGone
        test "Bob's queue should be deleted from RabbitMQ after termination" do
          assert "Bob's RabbitMQ queue still exists" rabbitQueueGone

-- pdrA is Alice; pdrB is Bob, regardless of which PDR executes the test.
type SignupModelConfiguration =
  { suiteName :: String
  , snapshotDirectory :: String
  , outputSnapshotDirectory :: Maybe String
  , testModel :: String
  , testModelLoadMethods :: Array TestModelLoadMethod
  , indexedTestContext :: String
  , testAppManager :: String
  , testsType :: String
  , testSucceededProperty :: String
  , testNameProperty :: String
  , testTimeLimit :: Milliseconds
  , setupLogConfiguration :: LogConfiguration
  , tests :: Array ModelTest
  }

signupTestModelConfiguration :: SignupModelConfiguration
signupTestModelConfiguration =
  { suiteName: "Broker Service signup tests"
  , snapshotDirectory: bobSnapshotDirectory
  , outputSnapshotDirectory: Nothing
  , testModel
  , testModelLoadMethods:
      -- Bob must have the models imported by the test model before compiling it.
      -- The test model itself is compiled by `compileTestModel`.
      [ LoadModelFromRepository { modelUri: rabbitMQModel }
      , LoadModelFromRepository { modelUri: brokerServicesModel }
      ]
  , indexedTestContext: indexedTestApp
  , testAppManager
  , testsType
  , testSucceededProperty
  , testNameProperty
  , testTimeLimit: Milliseconds 60000.0
  , setupLogConfiguration: emptyConfiguration
  , tests:
      [ { testContextTypeName: signupTestContext, logConfiguration: Just debugConfiguration }
      , { testContextTypeName: createCouchdbAccountTestContext, logConfiguration: Just emptyConfiguration }
      , { testContextTypeName: endSubscriptionTestContext, logConfiguration: Just emptyConfiguration }
      ]
  }

emptyConfiguration :: LogConfiguration
emptyConfiguration =
  { pdrA:
      [ { topic: TEST, logLevel: Trace }
      ]
  , pdrB:
      [ { topic: TEST, logLevel: Trace }
      ]
  }

debugConfiguration :: LogConfiguration
debugConfiguration =
  { pdrA:
      [
        { topic: TEST, logLevel: Trace }
      , { topic: BROKER, logLevel: Trace }
      -- , { topic: SYNC, logLevel: Trace }
      -- , { topic: DELTA, logLevel: Trace }
      -- , { topic: INSTALL, logLevel: Trace }
      -- , { topic: RESOURCE, logLevel: Trace }
      -- , { topic: STATE, logLevel: Trace }
      -- , { topic: MODEL, logLevel: Warn }
      -- , { topic: UPGRADE, logLevel: Trace }
      ]
  , pdrB:
      [ { topic: TEST, logLevel: Trace }
      , { topic: BROKER, logLevel: Trace }
      -- , { topic: SYNC, logLevel: Trace }
      -- , { topic: DELTA, logLevel: Trace }
      -- , { topic: RESOURCE, logLevel: Trace }
      -- , { topic: STATE, logLevel: Trace }
      -- , { topic: UPGRADE, logLevel: Trace }
      ]
  }

testModel :: String
testModel = "model://joopringelberg.nl#bssu4gc7kx@1.0"

testModelSource :: String
testModelSource = "src/model/brokerServiceSignupTests@1.0.arc"

-- As used in this test, we should use the unversioned uri!
testModelReadable :: String
testModelReadable = "model://joopringelberg.nl#BrokerServiceSignupTests"

rabbitMQModel :: String
rabbitMQModel = "model://perspectives.domains#m203lt2idk@2.0"

brokerServicesModel :: String
brokerServicesModel = "model://perspectives.domains#zjuzxbqpgc@7.0"

indexedTestApp :: String
indexedTestApp = testModelReadable <> "$BrokerServiceSignupTestsApp"

testAppManager :: String
testAppManager = testModelReadable <> "$TestApp$Manager"

testsType :: String
testsType = testModelReadable <> "$TestApp$Tests"

testSucceededProperty :: String
testSucceededProperty = testModelReadable <> "$Test$External$TestSucceeded"

testNameProperty :: String
testNameProperty = testModelReadable <> "$Test$External$TestName"

signupTestContext :: String
signupTestContext = testModelReadable <> "$SignUpToBrokerService"

createCouchdbAccountTestContext :: String
createCouchdbAccountTestContext = testModelReadable <> "$CreateCouchdbAccount"

endSubscriptionTestContext :: String
endSubscriptionTestContext = testModelReadable <> "$EndSubscription"

leaderTerminatesContractTestContext :: String
leaderTerminatesContractTestContext = testModelReadable <> "$Test_Leader_Terminates_Contract"

aliceSnapshotDirectory :: String
aliceSnapshotDirectory = "test/pdr-snapshot/universe/aliceAfterReboot"

bobSnapshotDirectory :: String
bobSnapshotDirectory = "test/pdr-snapshot/universe/bob"

myBrokersContext :: String
myBrokersContext = "model://perspectives.domains#BrokerServices$MyBrokers"

managedBrokersRole :: String
managedBrokersRole = "model://perspectives.domains#BrokerServices$BrokerServices$ManagedBrokers"

brokerServiceAccountsRole :: String
brokerServiceAccountsRole = "model://perspectives.domains#BrokerServices$BrokerService$Accounts"

brokerContractAccountHolderRole :: String
brokerContractAccountHolderRole = "model://perspectives.domains#BrokerServices$BrokerContract$AccountHolder"

myCouchdbApp :: String
myCouchdbApp = "model://perspectives.domains#CouchdbManagement$MyCouchdbApp"

couchdbServersRole :: String
couchdbServersRole = "model://perspectives.domains#CouchdbManagement$CouchdbManagementApp$CouchdbServers"

couchdbServerAccountsRole :: String
couchdbServerAccountsRole = "model://perspectives.domains#CouchdbManagement$CouchdbServer$Accounts"
