// PDR VERSION 25.01
import couchdbmanagement from '../test/patches/couchdbManagement@12.4.arc';
import repositoryregistry from '../test/patches/repositoryRegistry@1.0.arc';
import amqptestmodel from '../test/patches/AMQPtestModel@1.0.arc';
import amqptestsetup from '../test/patches/AMQPtestSetup@1.0.arc';
import brokerservices from '../test/patches/BrokerServices@7.0.arc';
import system from '../test/patches/perspectivesSysteem@7.0.arc';
import uninstalltarget from '../test/patches/uninstallTarget@1.0.arc';
import transactionexecutiontests from '../test/patches/transactionExecutionTests@1.0.arc';
import twopdrdestructivetests from '../test/patches/twoPDRDestructiveTests@1.0.arc';

export { couchdbmanagement, repositoryregistry, amqptestmodel, amqptestsetup, brokerservices, system, uninstalltarget, transactionexecutiontests, twopdrdestructivetests }