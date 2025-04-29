/**
 * @description
 * This file is used to sync the local database with the remote CouchDB database.
 * We attach to the local database(s) and sync them with pre-existing remote CouchDB databases.
 * This allows us to edit the data in Couchdb and have it reflected in the local database.
 * This is useful for testing and development purposes.
 */

import PouchDB from "pouchdb-browser";

declare global {
  interface Window {
    __pouch_sync_active__?: boolean;
  }
}

const accountName = "admin"
const accountPassword = "admin"


export function syncWithCouchDB( databaseName: string ) {
  return new Promise<void>((resolve, reject) => {
    const localDB = new PouchDB(databaseName);
    const remoteDB = new PouchDB(`https://${accountName}:${accountPassword}@localhost:6984/${databaseName}`);
      // Add a global flag to avoid multiple syncs during dev reloads
    if (!window.__pouch_sync_active__) {
      window.__pouch_sync_active__ = true;

      localDB.sync(remoteDB, {live: false})
      .on('change', (info : any) => console.log('🔄', info))
      .on('error', (err : Error) => {
        console.error('❌ Sync error:', err)
        reject(err);
      })
      .on('complete', () => {
        console.log('✅ Initial sync complete');
        localDB.sync(remoteDB, {
          live: true,
          retry: true,
        }).on('change', (info : any) => {
          console.log('🔄 Change from sync:', info);
        }).on('error', (err : Error) => {
          console.error('❌ Sync error:', err);
        });
        resolve();
      });
    }
  });
}