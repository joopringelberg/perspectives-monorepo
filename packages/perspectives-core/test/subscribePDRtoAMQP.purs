module Test.Test.PDRInstance.SubscribePDRtoAMQP where

import Prelude

import Effect.Aff (Aff)
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Logging (infoTest)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction', shareWithPeers)
import Test.PDRInstance.Types (PDRInstance, runInPDR)

-- LET OP: we moeten in eerste instantie de beheerder en een eerste subscriber testen.
-- Dat betekent dat gebruik maken van de service niet gelijk is voor beide test PDR's.
-- Voor de beheerder is een BespokeDatabase nodig. Dat is niet meer of minder dan een Couchdb database. 
-- Mogelijk kan dat in een lokale Couchdb zijn?
-- Hoe komt de administrator aan een contract?
-- De client bezoekt de publieke versie van de BrokerService.
-- De client voert actie AddThisServer uit.
-- Dan in de App de actie Signup.

manageAMQPwithPDR :: PDRInstance -> Aff Unit
manageAMQPwithPDR pdr = runInPDR pdr do
  -- Load the Broker model
  infoTest $ pdr.name <> " loads test model in PDRA"
  runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
    $
      addModelToLocalStore_ [ brokerServiceModel ] (RoleInstance "Ignored")
  -- Create a BrokerService instance in the PDR, so that it can manage AMQP subscriptions.
  infoTest $ pdr.name <> " creates BrokerService instance in PDRA"
  -- First create an instance of ManagedBrokers.
  -- Set its StorageLocation property to the URL of a Couchdb database that can be used to store the public version.
  
  -- We need:
  -- - the URL of the service
  -- - the name of the exchange to subscribe to
  -- - the URL of the management endpoint of the AMQP broker
  -- - the URL of the SelfRegisterEndpoint of the AMQP broker
  -- - the PublicUrl where the BrokerService context can be reached by other PDRs

  pure unit

subscribePDRtoAMQP :: PDRInstance -> Aff Unit
subscribePDRtoAMQP pdr = runInPDR pdr do
  -- Load the Broker model
  infoTest $ pdr.name <> " loads test model in PDRA"
  runMonadPerspectivesTransaction' shareWithPeers (ENR $ EnumeratedRoleType sysUser)
    $
      addModelToLocalStore_ [ brokerServiceModel ] (RoleInstance "Ignored")

  pure unit

-- We need the Stable identifier.
brokerServiceModel :: String
-- brokerServiceModel = "model://perspectives.domains#BrokerServices@6.1"
brokerServiceModel = "model://perspectives.domains#zjuzxbqpgc@6.1"

