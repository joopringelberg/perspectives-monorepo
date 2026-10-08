module Test.BrokerServiceSignupTests where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (catMaybes, head)
import Data.Either (Either(..), isRight)
import Data.Foldable (any, for_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Perspectives.Assignment.Update (setProperty)
import Perspectives.Cuid2 (cuid2)
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, bracket, error, launchAff_)
import Control.Monad.AvarMonadAsk (gets)
import Effect.Aff.AVar (tryRead)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign.Object (fromFoldable)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), MonadPerspectives, (##=), (##>))
import Perspectives.DataUpgrade.PatchModels (patchModels)
import Perspectives.DataUpgrade.RecompileLocalModels (recompileLocalModel)
import Perspectives.Identifiers (modelUri2LocalName, unversionedModelUri)
import Perspectives.Instances.ObjectGetters (binding, context, getEnumeratedRoleInstances, getUnlinkedRoleInstances)
import Perspectives.Logging (infoTest, traceTest)
import Perspectives.ModelDependencies (identifiableFirstName, identifiableLastName, sysUser)
import Perspectives.AMQP.RabbitMQManagement (virtualHost)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.PerspectivesState (defaultRuntimeOptions, setTopicLogLevel)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), IndexedContext(..), PropertyType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), Stable, StableIdMapping)
import Perspectives.Sidecar.ToStable (toStable)
import Perspectives.TypePersistence.LoadArc (loadCompileAndStoreArcFile_)
import Test.LocalCouchdbTestSupport (addLocalCouchdbCredentials)
import Test.PDRInstance (SynchronisationResult, noBus, pollUntil, snapshotExists, startPDRInstanceFromSnapshotWithHook, testPouchdbUser, withPDRCached)
import Test.PDRInstance.Types (PDRInstance, runInPDR)
import Test.RabbitMQCtl (addAdminUser, deleteUser, purgeOwnQueue, queueExists, userExists)
import Test.SinglePDRScaffold (SinglePDRModelConfiguration, TestModelLoadMethod(..), LogConfiguration, emptyLogConfiguration, executeModelTest, loadModel)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  results <- getSignupResults
  liftEffect $ runTest (signupSuite results)

type SignupResults =
  { signupResults :: Array SynchronisationResult
  , aliceSawBob :: Boolean
  -- Bob's RabbitMQ account and queue, as they were while he was subscribed.
  , bobCredentials :: Maybe BrokerCredentials
  -- Set after the EndSubscription test; Nothing if we could not check.
  , afterTermination :: Maybe TerminationOutcome
  }

type BrokerCredentials = { login :: String, queueId :: String }

type TerminationOutcome =
  { aliceContractGone :: Boolean
  , rabbitUserGone :: Boolean
  , rabbitQueueGone :: Boolean
  }

getSignupResults :: Aff SignupResults
getSignupResults = do
  aliceSnapshotExists <- snapshotExists aliceSnapshotDirectory
  unless aliceSnapshotExists
    $ throwError
    $ error ("Required Alice post-reboot snapshot is missing: " <> aliceSnapshotDirectory)

  bracket
    (startPDRInstanceFromSnapshotWithHook (testPouchdbUser "alice") defaultRuntimeOptions Nothing noBus aliceSnapshotDirectory purgeOwnQueue)
    (_.shutdown)
    \alice ->
      withPDRCached (testPouchdbUser "bob") defaultRuntimeOptions Nothing noBus bobSnapshotDirectory \bob -> do
        addLocalCouchdbCredentials alice
        addLocalCouchdbCredentials bob

        -- Alice compiles first and coins the stable ids; Bob reuses her mapping so both
        -- PDRs share the same CUIDs for the test model's types.
        aliceMapping <- compileTestModel alice Nothing
        
        for_ signupTestModelConfiguration.testModelLoadMethods (loadModel bob)

        void $ compileTestModel bob (Just aliceMapping)

        for_ [ alice, bob ] \pdr -> runInPDR pdr do
          for_ signupTestModelConfiguration.setupLogConfiguration.pdr \{ topic, logLevel } -> setTopicLogLevel topic logLevel

        bobTestApp <- pollUntil 100 (Milliseconds 100.0)
          "Broker signup test app to appear in Bob's PDR"
          ( runInPDR bob do
              IndexedContext indexed <- toStable (IndexedContext signupTestModelConfiguration.indexedTestContext)
              traceTest ("Looking up stable indexed context for signup test: " <> show indexed)
              lookupIndexedContext indexed
          )

        -- Alice administers RabbitMQ through the management API. The snapshot holds no admin credentials for her,
        -- so we create a temporary RabbitMQ administrator for her.
        adminPassword <- liftEffect $ cuid2 "brokerServiceSignupTestAdmin"
        void $ attempt $ deleteUser rabbitAdminName
        addAdminUser virtualHost rabbitAdminName adminPassword

        results <- for signupTestModelConfiguration.tests \testCase -> do
          -- Alice's PDR shares the process output with Bob's; her state transitions and broker traffic explain what she does on termination.
          when (testCase.testContextTypeName == endSubscriptionTestContext) $ giveAliceAdminCredentials alice adminPassword
          when (testCase.testContextTypeName == endSubscriptionTestContext) $ runInPDR alice do
            setTopicLogLevel STATE Debug
            setTopicLogLevel BROKER Debug
          result <- executeModelTest
            bob
            bobTestApp
            testCase.testContextTypeName
            testCase.logConfiguration
            signupTestModelConfiguration

          runInPDR bob do
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

          -- Remember Bob's account and queue before a later test terminates the subscription.
          credentials <-
            if testCase.testContextTypeName == signupTestContext then runInPDR bob currentBrokerCredentials
            else pure Nothing

          pure { result, aliceSawBob, credentials }

        let bobCredentials = head $ catMaybes $ map _.credentials results

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

        void $ attempt $ deleteUser rabbitAdminName

        pure { signupResults: map _.result results, aliceSawBob: any _.aliceSawBob results, bobCredentials, afterTermination }

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

-- | The credentials of the broker service this PDR is currently reading post from.
currentBrokerCredentials :: MonadPerspectives (Maybe BrokerCredentials)
currentBrokerCredentials = do
  bsAVar <- gets _.brokerService
  mbs <- liftAff $ tryRead bsAVar
  pure $ mbs <#> \{ login, queueId } -> { login, queueId }

foreign import brokerservices :: String

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

signupSuite :: SignupResults -> TestSuite
signupSuite { signupResults, aliceSawBob, afterTermination } =
  suite "Broker Service signup tests" do
    for_ signupResults \result -> case result of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed in Bob's PDR") do
          assert ("Test '" <> testName <> "' should succeed in Bob's PDR") testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Broker Service test failed: " <> show err) false

    test "Alice should see Bob's account-holder details" do
      assert "Alice should receive Bob's account-holder details through the Broker Service" aliceSawBob

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

signupTestModelConfiguration :: SinglePDRModelConfiguration
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
  , setupLogConfiguration:
      { pdr:
          [ { topic: TEST, logLevel: Trace }
          -- , { topic: INSTALL, logLevel: Trace }
          -- , { topic: MODEL, logLevel: Warn }
          ]
      }
  , tests:
      [ { testContextTypeName: signupTestContext, logConfiguration: debugConfiguration }
      , { testContextTypeName: endSubscriptionTestContext, logConfiguration: debugConfiguration }
      ]
  }

debugConfiguration :: LogConfiguration
debugConfiguration =
  { pdr:
      [
        -- { topic: TEST, logLevel: Trace }
        { topic: BROKER, logLevel: Trace }
      , { topic: SYNC, logLevel: Trace }
      -- , { topic: INSTALL, logLevel: Trace }
      , { topic: RESOURCE, logLevel: Trace }
      , { topic: STATE, logLevel: Trace }
      -- , { topic: MODEL, logLevel: Warn }
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

endSubscriptionTestContext :: String
endSubscriptionTestContext = testModelReadable <> "$EndSubscription"

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
