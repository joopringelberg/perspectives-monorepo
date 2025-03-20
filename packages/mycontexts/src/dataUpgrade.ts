// BEGIN LICENSE
// Perspectives Distributed Runtime
// Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Full text of this license can be found in the LICENSE file in the projects root.
// END LICENSE

import {get, set} from 'idb-keyval';
const myContextsVersion = __MYCONTEXTS_VERSION__ as string; 

interface Upgrade {
  ( ) : Promise<void>;
}

// Runs the upgrade only if it is associated with a version that is higher than 
// the installed version but lower than or equal to the version that is running right now.
// This ensures that the upgrade is run only once in history for each installation.
// An upgrade must return a promise; its result is ignored.
export function runUpgrade( installedVersion : string, upgradeVersion : string, upgrade : Upgrade)
{
  if (((installedVersion < upgradeVersion) || !installedVersion) && upgradeVersion <= myContextsVersion)
  {
    return upgrade();
  }
}

// If the recorded version is lower than that provided as parameter value, it will be overwritten
// by that value.
export function setMyContextsVersion()
{
  return get("currentMyContextsVersion")
    .then( installedVersion =>
      {
        if ( installedVersion < myContextsVersion )
        {
          return set( "currentMyContextsVersion", myContextsVersion );
        }
      })
}

// If There was no previous record of `currentMyContextsVersion`, this function will initialize it to the current version that is running.
export function initializeMyContextsVersions()
{
  return get("CurrentPDRVersion")
    .then( v => 
      {
        if (v)
        {
          // This cannot be a first installation; otherwise there wouldn't be a value for CurrentPDRVersion.
          return get("currentMyContextsVersion")
            .then(( installedVersion =>
              {
                if ( !installedVersion )
                {
                  // No value for currentMyContextsVersion: we want to apply patch fixUser, so initialize to "0.22.0".
                  return set("currentMyContextsVersion", "0.22.0");
                }
                else
                {
                  return installedVersion;
                }
              }))
            .catch ( () => set("currentMyContextsVersion", "0.22.0") );
        }
        else 
        {
          // No previous installation, just initialize to the version we're installing.
          set("currentMyContextsVersion", myContextsVersion);
        }
      })
    // PROBABLY the keyval-store has been created, but the keyval object has been removed.
    .catch( e => {
      console.log(e);
      indexedDB.deleteDatabase("keyval-store");
    })
}

export function getInstalledVersion()
{
  return get("currentMyContextsVersion");
}

////////////////////////////////////////////////////////////////////////////////////
////    ACTUAL UPGRADES
////////////////////////////////////////////////////////////////////////////////////

export function toWWW( )
{ 
  return get("CurrentPDRVersion").then( v => {
    if (v)
    {
      // There is an installation.
      return get("deviceName").then( deviceName => {
        if (!deviceName)
        {
          // This is an inplace installation, so the upgrade applies.
          return getUserDocuments().then( userDocuments => {
            if (userDocuments.length > 0)
            {
              // We have a document in the localUsers database.
              const { userName, password, couchdbUrl, systemIdentifier, perspectivesUser } = userDocuments[0];
              // The deviceName is the string that remains by subtracting the perspectivesUser as prefix from systemIdentifier.
              const deviceName = systemIdentifier.substring(perspectivesUser.length);
              // couchdbUrl actually is the combination of scheme, host and port.
              const matches = couchdbUrl.match(/(https.*):(\d+)/);
              let host, port;
              if (matches)
              {
                host = matches[1];
                port = matches[2];
              }
              if (userName && password && couchdbUrl && host && port)
              {
                // This is Couchdb installation.
                // * set key installationComplete to the value "true".
                set("installationComplete", true);
                set("deviceName", deviceName);
                // * set key perspectivesUserId to the username in CouchDB.
                set("perspectivesUserId", userName);
                // Now set the couchdb url, user name and password.
                set("couchdbUrl", host);
                set("couchdbPort", port);
                set("userName", userName);
                set("password", password);
              }
              else
              {
                // This is an IndexedDB installation.
                // * set key installationComplete to the value "true".
                set("installationComplete", true);
                // * set key deviceName to an empty string.
                set("deviceName", deviceName);
                // * set key perspectivesUserId to the cuid that represents the user.
                set("perspectivesUserId", userName);
              }
            }});
        }
      });
    }
})
}

///////////////////////////////////////////////////////////////////////////////
//// ALLUSERS
///////////////////////////////////////////////////////////////////////////////
import Pouchdb from "pouchdb-browser";

// The IndexedDB database "localUsers"
const localUsers = new Pouchdb("localUsers");

function getUserDocuments() : Promise<UserDocument[]>
{
  return localUsers.allDocs({ include_docs: true }).then((data: CouchdbData) => data.rows.map((row: any) => row.doc));
}
type UserDocument = { userName : string, password : string, couchdbUrl : string, systemIdentifier : string, perspectivesUser : string }; 

type CouchdbData = { rows: UserDocument[] };