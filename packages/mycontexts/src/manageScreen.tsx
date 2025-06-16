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


import
  { Row
  , Col
  , Container,
  Button
  } from "react-bootstrap";

import 'bootswatch/dist/lumen/bootstrap.min.css';
import './splash.css';

import {thisAppsLocation} from "perspectives-react";

import { SharedWorkerChannelPromise as PDRHandler } from 'perspectives-proxy';
import { constructPouchdbUser, getInstallationData } from "./installationData";
import { clear } from 'idb-keyval';
import { startPDR } from "./startPDR";

export default function ManageScreen()
{
  const appLocation = thisAppsLocation();
  return  <div className="introductionSplash text-muted">
            <div className="bg-primary text-white pb-3">
              <Container>
                <h1 className="text-center pt-5">MyContexts</h1>
              </Container>
            </div>

            <Container>
              <Row className="pt-5">
                <h2>Manage MyContexts installations in this browser</h2>
              </Row>
              <Row>
                <p>This page contains several links to help you manage your MyContexts installation (or even installations).</p>
              </Row>
              <Row>
                <Col className="alert alert-danger">
                  Remove your installation from this browser (removes indexedDB, local storage, service workers, caches).
                </Col>
                <Col className="d-flex align-items-center">
                  <Button variant="danger" onClick={deleteAccountFromIndexedDB}>Remove all local data</Button>
                </Col>
              </Row>
              <Row>
                <Col className="alert alert-danger">
                  Removes your installation. Advised when you have your data in a local Couchdb Database.
                </Col>
                <Col className="d-flex align-items-center">
                  <Button variant="danger" onClick={deleteAccount}>Completely remove your installation</Button>
                </Col>
              </Row>
              <Row>
                <Col className="alert alert-success">
                  Recompile all your models. As MyContexts is under active development, it may occur that the <em>form</em> of the models
                  that you have copied into your local storage are no longer compatible with the version of MyContexts that comes from
                  the website. Your models then need to be <em>recompiled</em>. Usually this will happen automatically but we include this
                  facility just in case. It is harmless to run this operation when your models are still compatible.
                </Col>
                <Col className="d-flex align-items-center">
                <Button variant="success" onClick={recompileLocalModels}>Recompile local models</Button>
                </Col>
              </Row>
              <Row>
              <Col className="alert alert-light">
                  I have finished here. Just take me to my contexts!
                </Col>
                <Col className="d-flex align-items-center">
                  <Button variant="info" onClick={() => window.location.href = appLocation}>Go to MyContexts</Button>
                </Col>
              </Row>
              
            </Container>
          </div>
}

function deleteAccount()
  {
    startPDR();
    getInstallationData().then( data => {
      const user = constructPouchdbUser(data);
      clear();
      PDRHandler
        .then( pdrHandler => pdrHandler.removeAccount(user.userName, user))
          .then( () => {
            if ('serviceWorker' in navigator) {
              navigator.serviceWorker.getRegistrations().then((registrations) => {
                for (let registration of registrations) {
                  registration.unregister();
                }
              });
            if ('caches' in window) {
              caches.keys().then((cacheNames) => {
                cacheNames.forEach((cacheName) => {
                  caches.delete(cacheName);
                });
              });
            }
            localStorage.clear();
            indexedDB.databases().then((dbs) => {
              dbs.forEach((db) => {
                if (db.name && db.name !== 'keyval-store')
                {
                  indexedDB.deleteDatabase(db.name);
                }
              });
            });
          }
        });
      }
    );
  }

  function deleteAccountFromIndexedDB()
  {
      clear().then(() => {
        if ('serviceWorker' in navigator) {
          navigator.serviceWorker.getRegistrations().then((registrations) => {
            for (let registration of registrations) {
              registration.unregister();
            }
          });
        if ('caches' in window) {
          caches.keys().then((cacheNames) => {
            cacheNames.forEach((cacheName) => {
              caches.delete(cacheName);
            });
          });
        }
        localStorage.clear();
        indexedDB.databases().then((dbs) => {
          dbs.forEach((db) => {
            if (db.name && db.name !== 'keyval-store')
            {
              indexedDB.deleteDatabase(db.name);
            }
          });
        });
      }
    });
  }

  function recompileLocalModels()
  {
    startPDR();
    getInstallationData().then( data => {
      const user = constructPouchdbUser(data);
      PDRHandler.then( pdrHandler => pdrHandler.recompileLocalModels(user));
    });
  }