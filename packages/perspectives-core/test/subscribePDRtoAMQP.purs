module Test.Test.PDRInstance.SubscribePDRtoAMQP where

import Prelude

import Data.Array (head)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Aff (Aff, bracket, error, throwError)
import Perspectives.Assignment.RunAction (runActionForObject, runContextAction)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##=), (##>))
import Perspectives.Extern.Couchdb (addModelToLocalStore_, createEntitiesDatabase)
import Perspectives.Identifiers (deconstructBuitenRol)
import Perspectives.Instances.ObjectGetters (binding_, getEnumeratedRoleInstances)
import Perspectives.Logging (errorBroker, infoTest, traceBroker, traceTest)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.PerspectivesState (getLogConfig, setLogConfig, setTopicLogLevel)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), IndexedContext(..), RoleType(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runMonadPerspectivesTransaction', shareWithPeers)
import Perspectives.Sidecar.ToStable (toStable)
import Test.PDRInstance.Types (PDRInstance, runInPDR)

type TopicLogLevelPair =
  { topic :: LogTopic
  , logLevel :: LogLevel
  }

withBracketedTopicLogLevel :: forall a. PDRInstance -> Array TopicLogLevelPair -> Aff a -> Aff a
withBracketedTopicLogLevel pdr topicLevels action =
  bracket
    ( do
        oldConfig <- runInPDR pdr getLogConfig
        runInPDR pdr do
          for_ topicLevels \{ topic, logLevel } -> setTopicLogLevel topic logLevel
        pure oldConfig
    )
    (\oldConfig -> runInPDR pdr $ setLogConfig oldConfig)
    (\_ -> action)

-- LET OP: we moeten in eerste instantie de beheerder en een eerste subscriber testen.
-- Dat betekent dat gebruik maken van de service niet gelijk is voor beide test PDR's.
-- Voor de beheerder is een BespokeDatabase nodig. Dat is niet meer of minder dan een Couchdb database. 
-- Mogelijk kan dat in een lokale Couchdb zijn?
-- Hoe komt de administrator aan een contract?
-- De client bezoekt de publieke versie van de BrokerService.
-- De client voert actie AddThisServer uit.
-- Dan in de App de actie Signup.

-------------------------------------------------------------------------------
---- MANAGE AMQP WITH PDR
-------------------------------------------------------------------------------
-- | Returns the external role of the public version of the created BrokerService instance.
manageAMQPwithPDR :: PDRInstance -> Aff ContextInstance
manageAMQPwithPDR pdr = runInPDR pdr do
  let bespokeDatabaseName = "cw_test_amqp_broker_service"
  -- let bespokeDatabaseName = "https://perspectives.domains/cw_ro6a1vrf9y/"
  runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
    do
      addModelToLocalStore_ [ amqpTestSetupModel ] (RoleInstance "Ignored")
      infoTest $ pdr.name <> " loaded AMQP test setup model"
      -- Create a 'public' database in the in-memory Pouchdb database, named "cw_test_amqp_broker_service". This is the database that will be used to store the public version of the BrokerService instance.
      createEntitiesDatabase [ "" ] [ bespokeDatabaseName ] [ "" ] (RoleInstance "Ignored")
      infoTest $ pdr.name <> " created public database for BrokerService instance"

  -- Create a BrokerService instance in the PDR, so that it can manage AMQP subscriptions.
  setupApp <- do
    IndexedContext setupApp <- toStable (IndexedContext "model://joopringelberg.nl#AMQPtestSetup$AMQPtestSetupApp")
    msetUpApp <- lookupIndexedContext setupApp
    case msetUpApp of
      Nothing -> throwError $ error $ pdr.name <> " could not find AMQPtestSetupApp context"
      Just s -> do
        infoTest $ pdr.name <> " found the AMQPtestSetupApp context"
        pure s

  -- First create an instance of ManagedBrokers
  -- and set its StorageLocation property to the URL of a Couchdb database that can be used to store the public version.
  runMonadPerspectivesTransaction' doNotShareWithPeers (CR $ CalculatedRoleType testSetupManager)
    $
      runContextAction testSetupManager "SetupBrokerService" (unwrap setupApp)
  infoTest $ pdr.name <> " ran SetupBrokerService action to create a BrokerService instance"

  -- Now run the action ConfigureBrokerService by the Manager.
  runMonadPerspectivesTransaction' shareWithPeers (CR $ CalculatedRoleType testSetupManager)
    $
      runContextAction testSetupManager "ConfigureBrokerService" (unwrap setupApp)
  infoTest $ pdr.name <> " ran ConfigureBrokerService action to configure the BrokerService instance"

  runMonadPerspectivesTransaction' doNotShareWithPeers (CR $ CalculatedRoleType testSetupManager)
    $
      runContextAction testSetupManager "SetCredentials" (unwrap setupApp)
  infoTest $ pdr.name <> " ran SetCredentials action to provide the Administrator with credentials for the BrokerService instance"

  -- Retrieve the address of the public version of the BrokerService instance, so that it can be used by the other PDR to subscribe to AMQP.
  IndexedContext brokerServicesAppStable <- toStable (IndexedContext brokerServicesApp)
  lookupIndexedContext brokerServicesAppStable >>= case _ of
    Nothing -> throwError $ error $ pdr.name <> " could not find BrokerServicesApp context"
    Just managedBrokersContext -> do
      stableManagedBrokers <- toStable (EnumeratedRoleType managedBrokers)
      managedBrokersInstances <- managedBrokersContext ##= getEnumeratedRoleInstances stableManagedBrokers
      traceTest $ pdr.name <> " retrieved ManagedBrokers instances: " <> show managedBrokersInstances <> " from BrokerServicesApp " <> show managedBrokersContext
      brokerServices <- traverse binding_ managedBrokersInstances
      case head brokerServices of
        Just (Just brokerServiceInstance) -> do
          infoTest $ pdr.name <> " retrieved public BrokerService instance address"
          pure $ ContextInstance $ "pub:" <> bespokeDatabaseName <> "#" <> takeGuid (deconstructBuitenRol (unwrap brokerServiceInstance))
        _ -> throwError $ error $ pdr.name <> " could not find any BrokerService instance in ManagedBrokers"

brokerServicesApp :: String
brokerServicesApp = "model://perspectives.domains#BrokerServices$MyBrokers"

managedBrokers :: String
managedBrokers = "model://perspectives.domains#BrokerServices$BrokerServices$ManagedBrokers"

publicBrokers :: String
publicBrokers = "model://perspectives.domains#BrokerServices$BrokerServices$PublicBrokers"

brokerServicesManager :: String
brokerServicesManager = "model://perspectives.domains#BrokerServices$BrokerServices$Manager"

brokerServiceVisitor :: String
brokerServiceVisitor = "model://perspectives.domains#BrokerServices$BrokerService$Visitor"

contractInUse :: String
contractInUse = "model://perspectives.domains#BrokerServices$BrokerServices$ContractInUse"

brokerContractAccountHolder :: String
brokerContractAccountHolder = "model://perspectives.domains#BrokerServices$BrokerContract$AccountHolder"

contractTerminated :: String
contractTerminated = "model://perspectives.domains#BrokerServices$BrokerContract$External$ContractTerminated"

-- We need the Stable identifier.
brokerServiceModel :: String
-- brokerServiceModel = "model://perspectives.domains#BrokerServices@6.1"
brokerServiceModel = "model://perspectives.domains#zjuzxbqpgc@6.1"

-------------------------------------------------------------------------------
---- AMQP TEST SETUP MODEL
-------------------------------------------------------------------------------

amqpTestSetupModel :: String
-- amqpTestSetupModel = "model://joopringelberg.nl#AMQPtestSetup@6.1"
amqpTestSetupModel = "model://joopringelberg.nl#whku0vufat@1.0"

testSetupManager :: String
testSetupManager = "model://joopringelberg.nl#AMQPtestSetup$TestSetupApp$Manager"

-------------------------------------------------------------------------------
---- SUBSCRIBE PDR TO AMQP
-------------------------------------------------------------------------------
subscribePDRtoAMQP :: ContextInstance -> PDRInstance -> Aff Unit
subscribePDRtoAMQP publicBrokerServiceInstance pdr =
  withBracketedTopicLogLevel pdr [ { topic: BROKER, logLevel: Trace } ] $ runInPDR pdr do
    -- Load the Broker model
    runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
      $
        addModelToLocalStore_ [ brokerServiceModel ] (RoleInstance "Ignored")
    infoTest $ pdr.name <> " loaded BrokerService model"

    -- Use the public version of the BrokerService instance to subscribe to AMQP.
    -- Run the context action AddThisServer in the context of the external role of the public BrokerService instance.
    runMonadPerspectivesTransaction' doNotShareWithPeers (CR $ CalculatedRoleType brokerServiceVisitor)
      $
        runContextAction brokerServiceVisitor "AddThisServer" (unwrap publicBrokerServiceInstance)

    -- Now sign up by running the Signup action.
    do
      IndexedContext brokerServicesAppStable <- toStable (IndexedContext brokerServicesApp)
      lookupIndexedContext brokerServicesAppStable >>= case _ of
        Nothing -> throwError $ error $ pdr.name <> " could not find BrokerServicesApp context"
        Just b@(ContextInstance bApp) -> do
          publicBrokersStable <- toStable (EnumeratedRoleType publicBrokers)
          publicBrokersInstances <- b ##= getEnumeratedRoleInstances publicBrokersStable
          case head publicBrokersInstances of
            (Just publicBrokerRoleInstance) -> do
              runMonadPerspectivesTransaction' shareWithPeers (CR $ CalculatedRoleType brokerServicesManager)
                $
                  runActionForObject (CR $ CalculatedRoleType brokerServicesManager) "SignUp" bApp (unwrap publicBrokerRoleInstance)
              infoTest $ pdr.name <> " executed SignUp action to subscribe to AMQP using the public BrokerService instance"
            _ -> throwError $ error $ pdr.name <> " could not find any Public BrokerService instance in BrokerServicesApp"

-------------------------------------------------------------------------------
---- UNSUBSCRIBE PDR FROM AMQP
-------------------------------------------------------------------------------
unsubscribePDRfromAMQP :: PDRInstance -> Aff Unit
unsubscribePDRfromAMQP pdr =
  withBracketedTopicLogLevel pdr [ { topic: BROKER, logLevel: Trace }, { topic: STATE, logLevel: Trace } ] $ runInPDR pdr do
    -- Retrieve the contract instance from the indexed context bs:MyBrokers.
    IndexedContext brokerServicesAppStable <- toStable (IndexedContext brokerServicesApp)
    lookupIndexedContext brokerServicesAppStable >>= case _ of
      Nothing -> errorBroker $ pdr.name <> " could not find BrokerServicesApp context"
      Just managedBrokersContext -> do
        traceBroker $ pdr.name <> " found BrokerServicesApp context: " <> show managedBrokersContext
        -- Then fetch the role instance ContractInUse: it is the (external role of the) BrokerContract for the PDR.
        contractInUseStable <- toStable (CalculatedRoleType contractInUse)
        mcontract <- managedBrokersContext ##> getRoleInstances (CR contractInUseStable)
        case mcontract of
          Nothing -> errorBroker $ pdr.name <> " could not find any BrokerContract instance in BrokerServicesApp"
          Just contractInstance -> do
            traceBroker $ pdr.name <> " found BrokerContract instance: " <> show contractInstance
            brokerContractAccountHolderStable <- toStable (EnumeratedRoleType brokerContractAccountHolder)
            -- Finally set the property ContractTerminated to true on that BrokerContract external role, which will trigger the termination of the contract and the unsubscription from AMQP.
            contractTerminatedStable <- toStable (EnumeratedPropertyType contractTerminated)
            runMonadPerspectivesTransaction' shareWithPeers (ENR brokerContractAccountHolderStable)
              $
                setProperty [ contractInstance ] contractTerminatedStable Nothing [ Value "true" ]
            traceBroker $ pdr.name <> " set ContractTerminated property to true on BrokerContract instance: " <> show contractInstance