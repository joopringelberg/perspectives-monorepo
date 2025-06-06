import React, { Component, ReactElement } from 'react'
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
import LoadingScreen from './LoadingScreen.js';

await initI18next();

interface AppState {
  phase: 'installationExists' | 'prepareInstallation' | 'installationError' | 'installing' | 'installationcomplete' | undefined,
  installationResult: InstallationResult,
  wwwComponentReady: boolean
}

export default class App extends Component<{}, AppState>
{
  constructor(props: {})
  {
    super(props);
    this.state =
      { phase: undefined
      , installationResult: { type: 'NoKeyPairData', perspectivesUserId: 'newuser' }
      , wwwComponentReady: false
      };
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

  componentDidUpdate(prevProps: Readonly<{}>, prevState: Readonly<AppState>, snapshot?: any): void {    
    if (this.state.phase === 'installing') {
      this.createAccount();
    }
  }

  createAccount(): void {
    const options : RuntimeOptions = 
      { isFirstInstallation: this.state.installationResult.type === 'KeyPairData' ? true : false
        , useSystemVersion: null
        , myContextsVersion: __MYCONTEXTS_VERSION__
      };

    let installationData: InstallationData, pouchdbUser: PouchdbUser, couchdbUrl: string | undefined;
      getInstallationData().then((data: InstallationData) => {
        installationData = data;
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

  render()
  {
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
          </>);
      case 'prepareInstallation':
      return <ConfigureInstallation callback={ (installationResult : InstallationResult) =>{
          this.setState({ phase: 'installing', installationResult });
        }} />;
      case 'installationError' :
      return <div>
              <h2>{ i18next.t( "installationerror", {ns: "mycontexts"} ) }</h2>
              <Button onClick={() => this.refreshApp()}>
                { i18next.t( "installationerror_button", {ns: "mycontexts"} ) }
              </Button>
              <p>{ i18next.t( "installationerror_message", {ns: "mycontexts"} ) }</p>
            </div>;
      case 'installing':
        return this.installing(this.state.installationResult);
      case 'installationcomplete':
        return this.installing(this.state.installationResult);
      default:
      return <div>Work to do.</div>;
    }
  }
}
