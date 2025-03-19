import { PouchdbUser } from "perspectives-proxy";
import { InstallationData } from "./configureInstallation";
import { get as getValue } from 'idb-keyval';

export function getInstallationData(): Promise<InstallationData> {
  return Promise.all([
    getValue('deviceName'),
    getValue('couchdbUrl'),
    getValue('couchdbPort'),
    getValue('userName'),
    getValue('password'),
    getValue('identityFile'),
    getValue('perspectivesUserId')
  ]).then(([deviceName, couchdbUrl, couchdbPort, userName, password, identityFile, perspectivesUserId]) => ({
    deviceName, couchdbUrl, couchdbPort, userName, password, identityFile, perspectivesUserId
    , keyPair: null
  })) }

export function constructPouchdbUser(installationData: InstallationData): PouchdbUser {
  const couchdbUrl = installationData.couchdbUrl ? installationData.couchdbUrl + ":" + installationData.couchdbPort : undefined;
  return { userName: installationData.userName as string
         , password: installationData.password as string
         , couchdbUrl
         , systemIdentifier: installationData.perspectivesUserId as string + installationData.deviceName as string
         , perspectivesUser: installationData.perspectivesUserId as string};
}