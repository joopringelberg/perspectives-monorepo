module Test.Test.PDRInstance.SubscribePDRtoAMQP where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Aff (Aff, error, throwError)
import Perspectives.Assignment.RunAction (runActionForObject, runContextAction)
import Perspectives.CoreTypes ((##=))
import Perspectives.Extern.Couchdb (addModelToLocalStore_, createEntitiesDatabase)
import Perspectives.Identifiers (deconstructBuitenRol)
import Perspectives.Instances.ObjectGetters (binding_, getEnumeratedRoleInstances)
import Perspectives.Logging (infoTest, traceTest)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), EnumeratedRoleType(..), IndexedContext(..), RoleType(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.RunMonadPerspectivesTransaction (doNotShareWithPeers, runMonadPerspectivesTransaction', shareWithPeers)
import Perspectives.Sidecar.ToStable (toStable)
import Test.PDRInstance.Types (PDRInstance, runInPDR)

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
  runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
    do
      addModelToLocalStore_ [ amqpTestSetupModel ] (RoleInstance "Ignored")
      infoTest $ pdr.name <> " loaded AMQP test setup model"
      -- Create a 'public' database in the in-memory Pouchdb database, named "cw_test_amqp_broker_service". This is the database that will be used to store the public version of the BrokerService instance.
      createEntitiesDatabase [""] [bespokeDatabaseName] [""] (RoleInstance "Ignored")
      infoTest $ pdr.name <> " created public database for BrokerService instance"
  
  -- Create a BrokerService instance in the PDR, so that it can manage AMQP subscriptions.
  setupApp <- do
    IndexedContext setupApp <- toStable (IndexedContext "model://joopringelberg.nl#AMQPtestSetup$AMQPtestSetupApp")
    msetUpApp <- lookupIndexedContext setupApp
    -- First create an instance of ManagedBrokers
    -- and set its StorageLocation property to the URL of a Couchdb database that can be used to store the public version.
    case msetUpApp of
      Nothing -> throwError $ error $ pdr.name <> " could not find AMQPtestSetupApp context"
      Just s -> do
        infoTest $ pdr.name <> " is now Manager of a new BrokerService instance"
        pure s
  
  -- Now run the action SetupBrokerService by the Manager.
  runMonadPerspectivesTransaction' doNotShareWithPeers (CR $ CalculatedRoleType testSetupManager)
    $
    runContextAction testSetupManager "SetupBrokerService" (unwrap setupApp)
  infoTest $ pdr.name <> " ran SetupBrokerService action to create a BrokerService instance"

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
subscribePDRtoAMQP publicBrokerServiceInstance pdr = runInPDR pdr do
  -- Load the Broker model
  runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
    $
      addModelToLocalStore_ [ brokerServiceModel ] (RoleInstance "Ignored")
  infoTest $ pdr.name <> " loaded BrokerService model"
  
  -- Use the public version of the BrokerService instance to subscribe to AMQP.
  -- Run the context action AddThisServer in the context of the external role of the public BrokerService instance.
  runMonadPerspectivesTransaction' doNotShareWithPeers (CR $ CalculatedRoleType testSetupManager)
    $
    runContextAction brokerServiceVisitor "AddThisServer" (unwrap publicBrokerServiceInstance)
  
  -- Now sign up by running the Signup action.
  do
    lookupIndexedContext brokerServicesApp >>= case _ of
      Nothing -> throwError $ error $ pdr.name <> " could not find BrokerServicesApp context"
      Just b@(ContextInstance bApp) -> do
        publicBrokersInstances <- b ##= getEnumeratedRoleInstances (EnumeratedRoleType publicBrokers)
        case head publicBrokersInstances of
          (Just publicBrokerRoleInstance) -> do
            runMonadPerspectivesTransaction' doNotShareWithPeers (CR $ CalculatedRoleType testSetupManager)
              $
              runActionForObject (CR $ CalculatedRoleType brokerServicesManager) "SignUp" bApp (unwrap publicBrokerRoleInstance)
            infoTest $ pdr.name <> " executed SignUp action to subscribe to AMQP using the public BrokerService instance"
          _ -> throwError $ error $ pdr.name <> " could not find any Public BrokerService instance in BrokerServicesApp"
