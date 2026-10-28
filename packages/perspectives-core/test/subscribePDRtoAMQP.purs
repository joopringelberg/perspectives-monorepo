module Test.Test.PDRInstance.SubscribePDRtoAMQP where

import Prelude

import Data.Array (filterA, null)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff (Aff, bracket, error, throwError)
import Perspectives.Assignment.RunAction (runActionForObject, runContextAction)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes (LogLevel, LogTopic, MonadPerspectives, (##=), (##>))
import Perspectives.Extern.Couchdb (addModelToLocalStore_)
import Perspectives.Identifiers (deconstructBuitenRol)
import Perspectives.Instances.ObjectGetters (binding_, getEnumeratedRoleInstances, getProperty)
import Perspectives.Instances.Me (isMe)
import Perspectives.Logging (infoTest)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.PerspectivesState (getLogConfig, setLogConfig, setTopicLogLevel)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..), externalRole)
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

-------------------------------------------------------------------------------
---- MANAGE AMQP WITH PDR
-------------------------------------------------------------------------------
-- | Credentials of the administrator of the RabbitMQ server that the managing PDR will administer.
type AMQPAdministratorCredentials =
  { adminUserName :: String
  , adminPassword :: String
  }

-- | Select the rebooted universe's broker, never an arbitrary first broker.
getManagedBrokerService :: MonadPerspectives { publicContext :: ContextInstance, administrator :: RoleInstance }
getManagedBrokerService = do
  IndexedContext brokerServicesAppStable <- toStable (IndexedContext brokerServicesApp)
  lookupIndexedContext brokerServicesAppStable >>= case _ of
    Nothing -> throwError $ error "BrokerServicesApp is missing; create the rebooted universe snapshot first"
    Just managedBrokersContext -> do
      stableManagedBrokers <- toStable (EnumeratedRoleType managedBrokers)
      managedBrokersInstances <- managedBrokersContext ##= getEnumeratedRoleInstances stableManagedBrokers
      identifier <- toStable (EnumeratedPropertyType (managedBrokers <> "$GivenIdentifier"))
      testBrokers <- filterA
        ( \role -> do
            name <- role ##> getProperty identifier
            pure (name == Just (Value "BigBangsBrokerService"))
        )
        managedBrokersInstances
      case testBrokers of
        [ testBroker ] -> binding_ testBroker >>= case _ of
          Just brokerServiceInstance -> do
            publicUrl <- toStable (EnumeratedPropertyType "model://perspectives.domains#BrokerServices$BrokerService$External$PublicUrl")
            location <- brokerServiceInstance ##> getProperty publicUrl
            url <- case location of
              Just (Value value) | value /= "" -> pure value
              _ -> throwError $ error "BigBangsBrokerService has no public storage URL"
            let service = ContextInstance (deconstructBuitenRol (unwrap brokerServiceInstance))
            administratorType <- toStable (EnumeratedRoleType "model://perspectives.domains#BrokerServices$BrokerService$Administrator")
            administrators <- service ##= getEnumeratedRoleInstances administratorType
            case administrators of
              [ administrator ] -> do
                mine <- isMe administrator
                unless mine $ throwError $ error "The restored user does not administer BigBangsBrokerService"
                pure
                  { publicContext: ContextInstance $ "pub:" <> url <> "#" <> takeGuid (unwrap service)
                  , administrator
                  }
              _ -> throwError $ error "Expected exactly one BigBangsBrokerService Administrator"
          Nothing -> throwError $ error "BigBangsBrokerService has no context binding"
        _ -> throwError $ error $ "Expected exactly one managed BigBangsBrokerService in the rebooted snapshot, found " <> show testBrokers

-- | Reuse management; only per-run RabbitMQ administrator credentials change.
reuseAMQPManagement :: AMQPAdministratorCredentials -> PDRInstance -> Aff ContextInstance
reuseAMQPManagement { adminUserName, adminPassword } pdr = runInPDR pdr do
  broker <- getManagedBrokerService
  username <- toStable (EnumeratedPropertyType "model://perspectives.domains#BrokerServices$BrokerService$Administrator$AdminUserName")
  password <- toStable (EnumeratedPropertyType "model://perspectives.domains#BrokerServices$BrokerService$Administrator$AdminPassword")
  runMonadPerspectivesTransaction' doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser) do
    setProperty [ broker.administrator ] username Nothing [ Value adminUserName ]
    setProperty [ broker.administrator ] password Nothing [ Value adminPassword ]
  infoTest $ pdr.name <> " reusing BigBangsBrokerService management"
  pure broker.publicContext

requireUnsubscribed :: MonadPerspectives Unit
requireUnsubscribed = do
  contracts <- getBrokerSubscriptions
  unless (null contracts) $ throwError $ error "Snapshot must not contain broker subscriptions; subscriptions are created afresh for each run"

getBrokerSubscriptions :: MonadPerspectives (Array RoleInstance)
getBrokerSubscriptions = do
  IndexedContext index <- toStable (IndexedContext brokerServicesApp)
  lookupIndexedContext index >>= case _ of
    Nothing -> throwError $ error "Snapshot has no BrokerServicesApp"
    Just app -> do
      contractsType <- toStable (CalculatedRoleType "model://perspectives.domains#BrokerServices$BrokerServices$Contracts")
      app ##= getRoleInstances (CR contractsType)

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
-- brokerServiceModel = "model://perspectives.domains#BrokerServices@7.0"
brokerServiceModel = "model://perspectives.domains#zjuzxbqpgc@7.0"

prepareAMQPSubscription :: MonadPerspectives Unit
prepareAMQPSubscription = do
  runMonadPerspectivesTransaction' doNotShareWithPeers (ENR $ EnumeratedRoleType sysUser) $
    addModelToLocalStore_ [ brokerServiceModel ] (RoleInstance "Ignored")
  requireUnsubscribed

-------------------------------------------------------------------------------
---- SUBSCRIBE PDR TO AMQP
-------------------------------------------------------------------------------
subscribePDRtoAMQP :: ContextInstance -> PDRInstance -> Aff Unit
subscribePDRtoAMQP publicBrokerServiceInstance pdr = do
  withBracketedTopicLogLevel pdr [ {-- topic: BROKER, logLevel: Trace }, { topic: RESOURCE, logLevel: Trace --}] $ runInPDR pdr do
    prepareAMQPSubscription

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
          matchingBrokers <- filterA
            ( \role -> do
                filler <- binding_ role
                pure (filler == Just (externalRole publicBrokerServiceInstance))
            )
            publicBrokersInstances
          case matchingBrokers of
            [ publicBrokerRoleInstance ] -> do
              runMonadPerspectivesTransaction' shareWithPeers (CR $ CalculatedRoleType brokerServicesManager)
                $
                  runActionForObject (CR $ CalculatedRoleType brokerServicesManager) "SignUp" bApp (unwrap publicBrokerRoleInstance)
              infoTest $ pdr.name <> " executed SignUp action to subscribe to AMQP using the public BrokerService instance" <> show publicBrokerRoleInstance
            _ -> throwError $ error $ pdr.name <> " expected exactly one PublicBrokers role for " <> show publicBrokerServiceInstance <> ", found " <> show matchingBrokers

-------------------------------------------------------------------------------
---- UNSUBSCRIBE PDR FROM AMQP
-------------------------------------------------------------------------------
unsubscribePDRfromAMQP :: PDRInstance -> Aff Unit
unsubscribePDRfromAMQP pdr =
  runInPDR pdr do
    contracts <- getBrokerSubscriptions
    unless (null contracts) do
      holder <- toStable (EnumeratedRoleType brokerContractAccountHolder)
      terminated <- toStable (EnumeratedPropertyType contractTerminated)
      runMonadPerspectivesTransaction' shareWithPeers (ENR holder) $
        setProperty contracts terminated Nothing [ Value "true" ]