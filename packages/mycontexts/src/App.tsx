import * as React from 'react'
const { Component } = React;
import type {ReactElement} from 'react';
import 'bootswatch/dist/spacelab/bootstrap.min.css';
// import './assets/bootstrap.min.css';
import 'bootstrap-icons/font/bootstrap-icons.css';
import { set as setValue, get as getValue } from 'idb-keyval';
import ConfigureInstallation, { InstallationData, InstallationResult } from './configureInstallation';
import {initI18next} from "./i18next.js";
import { SaveKeyPair } from './saveKeypair.js';
import {i18next} from 'perspectives-react';
import { SharedWorkerChannelPromise as PDRHandler, PouchdbUser, RuntimeOptions } from 'perspectives-proxy';
import { constructPouchdbUser, getInstallationData } from './installationData.js';
import { startPDR } from './startPDR.js';
import { Button, Container, Row } from 'react-bootstrap';
import WWWComponent from './www.js';
import { getInstalledVersion, runUpgrade, setMyContextsVersion, toWWW } from './dataUpgrade.js';
import LoadingScreen from './loadingScreen.js';
import UpdateNotification from './updateNotification.js';

await initI18next();

interface AppState {
  phase: 'installationExists' | 'prepareInstallation' | 'installationError' | 'installing' | 'installationcomplete' | undefined,
  installationResult: InstallationResult,
  wwwComponentReady: boolean,
  updateAvailable: boolean,
}

export default class App extends Component<Record<string, never>, AppState>
{
  constructor(props: Record<string, never>)
  {
    super(props);
    const component = this;
    this.state =
      { phase: undefined
      , installationResult: { type: 'NoKeyPairData', perspectivesUserId: 'newuser' }
      , wwwComponentReady: false
      , updateAvailable: false
      };
    
    // ONLY listen for update notifications, don't check for updates
    if ('serviceWorker' in navigator) {
      // Listen for update messages from service worker via BroadcastChannel
      const channel = new BroadcastChannel('app-update-channel');
      channel.addEventListener('message', (event) => {
        console.log("Received message from broadcast channel:", event.data);
        if (event.data === 'NEW_VERSION_AVAILABLE') {
          console.log("Update available, showing notification");
          component.setState({updateAvailable: true});
        }
      });
    }
    
    this.runDataUpgrades().then( () => startPDR());
  }

  componentDidMount(): void {
    const params = new URLSearchParams(document.location.search.substring(1));
    if (params.get("reinstall") != undefined)
    {
      this.setState({ phase: 'prepareInstallation' });
    }
    else {
      getValue('installationComplete')
        .then((isComplete: boolean) => {
          if (isComplete) {
            this.setState({ phase: 'installationExists' });
          } else {
            this.setState({ phase: 'prepareInstallation' });
        }})
  }
  }

  componentDidUpdate(_prevProps: Readonly<Record<string, never>>, _prevState: Readonly<AppState>, _snapshot?: unknown): void {    
    if (this.state.phase === 'installing') {
      this.createAccount();
    }
  }

  createAccount(): void {
    let options : RuntimeOptions;
    let installationData: InstallationData, pouchdbUser: PouchdbUser;
      getInstallationData().then((data: InstallationData) => {
        installationData = data;
        options = 
          { isFirstInstallation: installationData.identityFile === undefined
            , useSystemVersion: null
            , myContextsVersion: __MYCONTEXTS_VERSION__
          };
        pouchdbUser = constructPouchdbUser(installationData);
      }).then(() => {
        PDRHandler.then( pdrHandler => 
            pdrHandler.createAccount( 
              installationData.perspectivesUserId as string,
              pouchdbUser,
              options,
              installationData.identityFile
             ))
        .then(() => {
          this.setState({ phase: 'installationcomplete' });
          setValue('installationComplete', true);
        })
        .catch(() => {
          this.setState({ phase: 'installationError' });
        });
      }
    )
  }

  // Shows the installation progress and offers the keypair download.
  // Also starts the actual installation.
  installing(data : InstallationResult): ReactElement
  {
    const component = this;
    return <Container>
      <Row><h2>{ i18next.t( "installation_title", {ns: 'mycontexts'}) }</h2></Row>
      {
        data.type === 'KeyPairData' ?
        <Row>
          <p>{ i18next.t( "savekeypairdialog_Message", {ns: "mycontexts"} ) }</p>
          <SaveKeyPair keypair={data.keyPair} perspectivesuserid={data.perspectivesUserId} />
        </Row>
        : null
      }
      {
        component.state.phase === 'installing' ?
        <Row className='pb-3'><p>Installing...</p></Row>
        : null
      }
      {
        component.state.phase === 'installationcomplete' ?
        <Row>
          <Button onClick={() => component.setState({ phase: 'installationExists' })}>
            { i18next.t( "installationcomplete_button", {ns: "mycontexts"} ) }
          </Button>
        </Row>
        : null
      }
      </Container>;
    }
  
  runDataUpgrades( )
    {
      // Make sure we have a version number. Initializes to the current version.
      return getInstalledVersion()
        .then( installedVersion => 
          {
            runUpgrade( installedVersion, "1.0.0", () => toWWW());
          })
        .then( () => setMyContextsVersion())
    }
  
  refreshApp() {
    const component = this;
    // Clear any in-memory caches first
    if ('caches' in window) {
      caches.keys().then(names => {
        names.forEach(name => {
          caches.delete(name);
        });
      });
    }
    
    // Add a small delay to allow cache clearing to complete
    setTimeout(() => {
      // Restart account creation.
      component.createAccount();
    }, 100);
  }

  handleUpdate() {
    console.log("User chose to update");
    // Tell service worker to skipWaiting
    if ('serviceWorker' in navigator && navigator.serviceWorker.controller) {
      navigator.serviceWorker.controller.postMessage('SKIP_WAITING');
      console.log("Sent SKIP_WAITING message to service worker");
    }
    this.setState({updateAvailable: false});
  }
  
  handleDismiss () {
    this.setState({updateAvailable: false});
  };

  render()
  {
    const component = this;
    const updateNotification = <UpdateNotification
              show={component.state.updateAvailable} 
              onUpdate={() => component.handleUpdate()} 
              onDismiss={() => component.handleDismiss()} 
            />

    switch (this.state.phase) {
      case 'installationExists':
        return (
          <>
            {!this.state.wwwComponentReady && <LoadingScreen />}
            <div style={{ display: this.state.wwwComponentReady ? 'block' : 'none' }}>
              <WWWComponent 
                onMounted={() => this.setState({ wwwComponentReady: true })}
              />
            </div>
            {updateNotification}
          </>);
      case 'prepareInstallation':
      return (<>
                <ConfigureInstallation callback={ (installationResult : InstallationResult) =>{
                  this.setState({ phase: 'installing', installationResult });
                }} />
                {updateNotification}
              </>);
      case 'installationError' :
      return (<>
              <div>
                <h2>{ i18next.t( "installationerror", {ns: "mycontexts"} ) }</h2>
                <Button onClick={() => this.refreshApp()}>
                  { i18next.t( "installationerror_button", {ns: "mycontexts"} ) }
                </Button>
                <p>{ i18next.t( "installationerror_message", {ns: "mycontexts"} ) }</p>
              </div>
              {updateNotification}
            </>);
      case 'installing':
        return (<>
                  {this.installing(this.state.installationResult)}
                  {updateNotification}
                </>);
      case 'installationcomplete':
        return (<>
                  {this.installing(this.state.installationResult)}
                  {updateNotification}
                </>);
      default:
      return <div>Work to do.</div>;
    }
  }
}
