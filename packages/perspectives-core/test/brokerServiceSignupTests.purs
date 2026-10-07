module Test.BrokerServiceSignupTests where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (any, for_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##=), (##>))
import Perspectives.Instances.ObjectGetters (binding, context, getEnumeratedRoleInstances)
import Perspectives.ModelDependencies (identifiableFirstName, identifiableLastName)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Query.UnsafeCompiler (getPropertyValues)
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), IndexedContext(..), PropertyType(..))
import Perspectives.Sidecar.ToStable (toStable)
import Data.Traversable (for)
import Test.PDRInstance (SynchronisationResult, noBus, pollUntil, snapshotExists, testPouchdbUser, withPDRCached)
import Test.PDRInstance.Types (PDRInstance, runInPDR)
import Test.LocalCouchdbTestSupport (addLocalCouchdbCredentials)
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
  , aliceSeesBob :: Boolean
  }

getSignupResults :: Aff SignupResults
getSignupResults = do
  aliceSnapshotExists <- snapshotExists aliceSnapshotDirectory
  unless aliceSnapshotExists
    $ throwError
    $ error ("Required Alice post-reboot snapshot is missing: " <> aliceSnapshotDirectory)

  withPDRCached (testPouchdbUser "alice") defaultRuntimeOptions Nothing noBus aliceSnapshotDirectory \alice ->
    withPDRCached (testPouchdbUser "bob") defaultRuntimeOptions Nothing noBus bobSnapshotDirectory \bob -> do
      addLocalCouchdbCredentials alice
      addLocalCouchdbCredentials bob

      for_ signupTestModelConfiguration.testModelLoadMethods (loadModel bob)
      bobTestApp <- pollUntil 100 (Milliseconds 100.0)
        "Broker signup test app to appear in Bob's PDR"
        ( runInPDR bob do
            IndexedContext indexed <- toStable (IndexedContext signupTestModelConfiguration.indexedTestContext)
            lookupIndexedContext indexed
        )

      signupResult <- executeModelTest
        bob
        bobTestApp
        signupTestContext
        emptyLogConfiguration
        signupTestModelConfiguration

      aliceObservedBob <- pollUntil 120 (Milliseconds 500.0)
        "Alice to receive Bob's BrokerContract account-holder details"
        do
          seesBob <- aliceSeesBob alice
          pure if seesBob then Just true else Nothing

      pure { signupResult, aliceSeesBob: aliceObservedBob }

aliceSeesBob :: PDRInstance -> Aff Boolean
aliceSeesBob pdr = runInPDR pdr do
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
        contracts <- brokerService ##= getEnumeratedRoleInstances accountsType >=> binding >=> context
        details <- for contracts \contract -> do
          accountHolders <- contract ##= getEnumeratedRoleInstances accountHolderType >=> binding
          for accountHolders \accountHolder -> do
            firstName <- accountHolder ##> getPropertyValues (ENP firstNameProperty)
            lastName <- accountHolder ##> getPropertyValues (ENP lastNameProperty)
            pure (firstName == Just (Value "bob") && lastName == Just (Value "bob_last"))
        pure $ any identity (join details)

      pure $ any identity contractMatches

signupSuite :: SignupResults -> TestSuite
signupSuite { signupResult, aliceSeesBob } =
  suite "Broker Service signup tests" do
    case signupResult of
      Right { testName, testSucceeded } ->
        test (testName <> " should succeed in Bob's PDR") do
          assert "Bob should have a registered BrokerContract with the parties' user details" testSucceeded
      Left { testName, err } ->
        test ("test '" <> testName <> "' failed with error") do
          assert ("Broker Service signup failed: " <> show err) false

    test "Alice should see Bob's account-holder details" do
      assert "Alice should receive Bob's account-holder details through the Broker Service" aliceSeesBob

signupTestModelConfiguration :: SinglePDRModelConfiguration
signupTestModelConfiguration =
  { suiteName: "Broker Service signup tests"
  , snapshotDirectory: bobSnapshotDirectory
  , outputSnapshotDirectory: Nothing
  , testModel
  , testModelLoadMethods:
      [ CompileModelFromSource
          { modelUri: testModel
          , sourcePath: testModelSource
          , modelUriReadable: testModelReadable
          , basedOnVersion: Nothing
          }
      ]
  , indexedTestContext: indexedTestApp
  , testAppManager
  , testsType
  , testSucceededProperty
  , testNameProperty
  , testTimeLimit: Milliseconds 180000.0
  , setupLogConfiguration:
      { pdr:
          [ { topic: TEST, logLevel: Trace }
          , { topic: BROKER, logLevel: Trace }
          , { topic: SYNC, logLevel: Trace }
          ]
      }
  , tests: [ { testContextTypeName: signupTestContext, logConfiguration: emptyLogConfiguration } ]
  }

testModel :: String
testModel = "model://joopringelberg.nl#bssu4gc7kx@1.0"

testModelSource :: String
testModelSource = "src/model/brokerServiceSignupTests@1.0.arc"

testModelReadable :: String
testModelReadable = "model://joopringelberg.nl#BrokerServiceSignupTests@1.0"

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
