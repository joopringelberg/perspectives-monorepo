module Test.BrokerServiceSignupTests where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (any, for_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, bracket, error, launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##=), (##>))
import Perspectives.Instances.ObjectGetters (binding, context, getEnumeratedRoleInstances, getUnlinkedRoleInstances)
import Perspectives.Identifiers (modelUri2LocalName, unversionedModelUri)
import Perspectives.Logging (infoTest, traceTest)
import Perspectives.ModelDependencies (identifiableFirstName, identifiableLastName, sysUser)
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
import Test.RabbitMQCtl (purgeOwnQueue)
import Test.PDRInstance.Types (PDRInstance, runInPDR)
import Test.SinglePDRScaffold (SinglePDRModelConfiguration, TestModelLoadMethod(..), emptyLogConfiguration, executeModelTest, loadModel)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  results <- getSignupResults
  liftEffect $ runTest (signupSuite results)

type SignupResults =
  { signupResult :: SynchronisationResult
  , aliceSawBob :: Boolean
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

        signupResult <- executeModelTest
          bob
          bobTestApp
          signupTestContext
          emptyLogConfiguration
          signupTestModelConfiguration

        runInPDR bob do
          traceTest ("Signup result: " <> show signupResult)

        aliceObservedBob <- pollUntil 120 (Milliseconds 500.0)
          "Alice to receive Bob's BrokerContract account-holder details"
          do
            seesBob <- aliceHasBobAccount alice
            pure if seesBob then Just true else Nothing

        pure { signupResult, aliceSawBob: aliceObservedBob }

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
signupSuite { signupResult, aliceSawBob } =
  suite "Broker Service signup tests" do
    case signupResult of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed in Bob's PDR") do
          assert "Bob should have a registered BrokerContract with the parties' user details" testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Broker Service signup failed: " <> show err) false

    test "Alice should see Bob's account-holder details" do
      assert "Alice should receive Bob's account-holder details through the Broker Service" aliceSawBob

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
          , { topic: BROKER, logLevel: Trace }
          , { topic: SYNC, logLevel: Trace }
          -- , { topic: INSTALL, logLevel: Trace }
          , { topic: RESOURCE, logLevel: Trace }
          , { topic: STATE, logLevel: Trace }
          -- , { topic: MODEL, logLevel: Warn }
          ]
      }
  , tests: [ { testContextTypeName: signupTestContext, logConfiguration: emptyLogConfiguration } ]
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
