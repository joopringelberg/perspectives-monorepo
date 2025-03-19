import React, { Component, ReactElement } from 'react'
import 'bootswatch/dist/lumen/bootstrap.min.css';
import 'bootstrap-icons/font/bootstrap-icons.css';
import { set as setValue, get as getValue } from 'idb-keyval';
import ConfigureInstallation, { InstallationData, InstallationResult } from './configureInstallation';
import {initI18next} from "./i18next.js";
import { SaveKeyPair } from './saveKeypair.js';
import i18next from 'i18next';
import { SharedWorkerChannelPromise as PDRHandler, PouchdbUser, RuntimeOptions } from 'perspectives-proxy';
import { constructPouchdbUser, getInstallationData } from './installationData.js';
import { startPDR } from './startPDR.js';
import { Button, Container, Row } from 'react-bootstrap';
import WWWComponent from './www.js';
import { getInstalledVersion, runUpgrade, setMyContextsVersion, toWWW } from './dataUpgrade.js';

await initI18next();

interface AppState {
  phase: 'installationExists' | 'prepareInstallation' | 'installationError' | 'installing' | 'installationcomplete' | undefined,
  installationResult: InstallationResult,
}

export default class App extends Component<{}, AppState>
{
  constructor(props: {})
  {
    super(props);
    this.state =
      { phase: undefined
      , installationResult: { type: 'NoKeyPairData', perspectivesUserId: 'newuser' }
      };
    this.runDataUpgrades().then( () => startPDR());
  }

  componentDidMount(): void {
    getValue('installationComplete')
    .then((isComplete: boolean) => {
      if (isComplete) {
        this.setState({ phase: 'installationExists' });
      } else {
        this.setState({ phase: 'prepareInstallation' });
    }})}

  componentDidUpdate(prevProps: Readonly<{}>, prevState: Readonly<AppState>, snapshot?: any): void {
    let installationData: InstallationData, pouchdbUser: PouchdbUser, couchdbUrl: string | undefined;
    const options : RuntimeOptions = 
      { isFirstInstallation: this.state.installationResult.type === 'KeyPairData' ? true : false
        , useSystemVersion: null
        , myContextsVersion: __MYCONTEXTS_VERSION__
      };
    
    if (this.state.phase === 'installing') {
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
    )}
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
  

  render()
  {
    switch (this.state.phase) {
      case 'installationExists':
      return <WWWComponent />;
      case 'prepareInstallation':
      return <ConfigureInstallation callback={ (installationResult : InstallationResult) =>{
          this.setState({ phase: 'installing', installationResult });
        }} />;
      case 'installationError' :
      return <div>Installation error.</div>;
      case 'installing':
        return this.installing(this.state.installationResult);
      case 'installationcomplete':
        return this.installing(this.state.installationResult);
      default:
      return <div>Work to do.</div>;
    }
  }
}
