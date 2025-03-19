import React, { useState, ReactElement, FC, ChangeEvent, MouseEventHandler } from 'react';
import { Container, Navbar, Button, Modal, Form, Col, Row } from 'react-bootstrap';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Tooltip from 'react-bootstrap/Tooltip';
import { set as setValue, del as deleteValue} from 'idb-keyval';
import {init} from '@paralleldrive/cuid2';
import { takeCUID } from 'perspectives-react';
import i18next from 'i18next';
import { IdentityFile, IdentityFileUploader, KeyPair, KeyPairFileUploader } from './configurationComponents';
import { FAQModal } from './faqs';


const PUBLICKEY = "_publicKey";
const PRIVATEKEY = "_privateKey"; 

export interface InstallationData {
  perspectivesUserId: string | null;
  deviceName: string | null;
  keyPair: KeyPair | null;
  identityFile: IdentityFile | null;
  couchdbUrl: string | null;
  couchdbPort: number | null;
  userName: string | null;
  password: string | null;
}
export type InstallationResult = KeyPairData | NoKeyPairData;

export interface NoKeyPairData {type: 'NoKeyPairData', perspectivesUserId: string}

export interface KeyPairData {
  type: 'KeyPairData';
  perspectivesUserId: string;
  keyPair: KeyPair;
}


const ConfigureInstallation: FC<{callback: (data: InstallationResult) => void}> = ({callback}) => {
  const [showFAQPanel, setShowFAQPanel] = useState<boolean>(false);
  const [showInstallPanel, setShowInstallPanel] = useState<boolean>(false);
  return (
    <>
      <Navbar bg='primary' fixed="top">
        <Navbar.Brand className="mx-auto text-white">{ i18next.t( "configuration_Welcome", {ns: 'mycontexts'})}</Navbar.Brand>
      </Navbar>
      <Container fluid className="main-content" style={{ minHeight: '100vh', display: 'flex', flexDirection: 'column', justifyContent: 'flex-start', paddingTop: '120px' }}>
        <p>
          { i18next.t( "configuration_WelcomeMessage", {ns: 'mycontexts'} )}
        </p>
        <Button variant="primary" onClick={() => setShowFAQPanel(true)} className="mb-2">
          MyContexts FAQ's
        </Button>
        <Button variant="primary" onClick={() => setShowInstallPanel(true)}>
          { i18next.t( "conversationDialog_Install", {ns: 'mycontexts'}) }
        </Button>
      </Container>
      <FAQModal show={showFAQPanel} onHide={() => setShowFAQPanel(false)} />
      <InstallModal show={showInstallPanel} onHide={() => setShowInstallPanel(false)} callback={callback}/>
    </>
  );
};

export default ConfigureInstallation;

const InstallModal: FC<{ show: boolean; onHide: () => void, callback: (data: InstallationResult) => void }> = ({ show, onHide, callback }) => {
  const [deviceName, setDeviceName] = useState<string | null>(null);
  const [notMyFirstInstallation, setNotMyFirstInstallation] = useState<boolean>(false);
  const [advancedInstall, setAdvancedInstall] = useState<boolean>(false);
  const [useOwnDatabase, setUseOwnDatabase] = useState<boolean>(false);
  const [useOwnKey, setUseOwnKey] = useState<boolean>(false);
  const [keyPair, setKeyPair] = useState<KeyPair | null>(null);
  const [identityFile, setIdentityFile] = useState<IdentityFile | null>(null);
  const [couchdbUrl, setCouchdbUrl] = useState<string | null>(null);
  const [couchdbPort, setCouchDbPort] = useState<number | null>(null);
  const [userName, setUsername] = useState<string | null>(null);
  const [password, setPassword] = useState<string|null>(null);
  const [validated, setValidated] = useState(false);

  function validate(event: React.MouseEvent<HTMLButtonElement>): void {
    const form = document.getElementById('installForm') as HTMLFormElement;
    event.preventDefault();
    event.stopPropagation();
    if (form.checkValidity()) {
      handleInstall({ deviceName, keyPair, identityFile, couchdbUrl, couchdbPort, userName, password } as InstallationData, callback);
      }
    setValidated(true);
  }

  return (<Modal show={show} onHide={onHide} fullscreen dialogClassName="slide-in-bottom">
    <Modal.Header closeButton>
      <Modal.Title>{ i18next.t( "configurationDialog_Title", {ns: 'mycontexts'})}</Modal.Title>
    </Modal.Header>
    <Modal.Body>
      <Form noValidate validated={validated} id='installForm'>
        <Form.Group controlId="formDeviceName">
          <Form.Label>{ i18next.t("configurationDialog_deviceName", {ns: 'mycontexts'}) }</Form.Label>
          <Form.Control
            type="text"
            placeholder="E.g. mylaptop, mymobile, mytablet"
            value={deviceName || ''}
            required
            onChange={(e) => {
              setDeviceName(e.target.value)
              setValue('deviceName', e.target.value)
            }}
          />
          <Form.Control.Feedback type="invalid">{ i18next.t("configurationDialog_deviceNameFeedback", {ns: 'mycontexts'})}</Form.Control.Feedback>
        </Form.Group>
        <SliderWithTooltip 
          label={i18next.t("configurationDialog_NotFirstInstallation", {ns: 'mycontexts'})}
          tooltip={i18next.t("configurationDialog_NotFirstInstallationTooltip", {ns: 'mycontexts'})}
          callback={ on => {
            setNotMyFirstInstallation(on);
            if (!on) {
              setIdentityFile(null);
              deleteValue('identityFile');
              setKeyPair(null);
              deleteValue('keyPair');
            }}} />  
        {notMyFirstInstallation && (
          <Container className='pt-0'>
            <IdentityFileUploader setIdentityFile={setIdentityFile} />
            <KeyPairFileUploader setKeyPairFile={setKeyPair} />
          </Container>
        )}
        <SliderWithTooltip 
          label={ i18next.t("configurationDialog_advancedInstallation", {ns: 'mycontexts'})}
          tooltip={ i18next.t("configurationDialog_advancedInstallationTooltip", {ns: 'mycontexts'})}
          callback={setAdvancedInstall}
          />
        {advancedInstall && (
          <>
            <Slider 
              label={ i18next.t("configurationDialog_useOwnDatabase", {ns: 'mycontexts'}) }
              callback={setUseOwnDatabase} />
            {useOwnDatabase && (
              <Container className='mt-4 mb-4'>
                <Form.Group as={Row}>
                  <Form.Label column sm="3">
                    {i18next.t("configurationDialog_useOwnDatabase_Url", {ns: 'mycontexts'})}
                  </Form.Label>
                  <Col sm="9">
                    <Form.Control
                    type="url"
                    placeholder="https://mydatabase.com"
                    value={couchdbUrl || ''}
                    required
                    onChange={(e) => {
                      setCouchdbUrl(e.target.value);
                      setValue('couchdbUrl', e.target.value);
                    }}
                  />
                  <Form.Control.Feedback type="invalid">{ i18next.t("configurationDialog_useOwnDatabase_UrlFeedback", {ns: 'mycontexts'})}</Form.Control.Feedback>
                  </Col>
                </Form.Group>
                <Form.Group as={Row}>
                  <Form.Label column sm="3">
                    { i18next.t("configurationDialog_useOwnDatabase_Port", {ns: 'mycontexts'})}
                  </Form.Label>
                  <Col sm="9">
                    <Form.Control
                    type="number"
                    required
                    value={couchdbPort || ''}
                    min={1000}
                    max={65536}
                    onChange={(e) => {
                    setCouchDbPort(parseInt(e.target.value));
                    setValue('couchdbPort', e.target.value);
                    }}
                    />
                    <Form.Control.Feedback type="invalid">{ i18next.t("configurationDialog_useOwnDatabase_PortFeedback", {ns: 'mycontexts'})}</Form.Control.Feedback>
                  </Col>
                </Form.Group>
                <Form.Group as={Row}>
                  <Form.Label column sm="3">
                    { i18next.t("configurationDialog_useOwnDatabase_Username", {ns: 'mycontexts'})}
                  </Form.Label>
                  <Col sm="9">
                    <Form.Control
                    type="text"
                    placeholder="myusername"
                    value={userName || ''}
                    pattern="^[a-z0-9]+$"
                    required
                    onChange={(e) => {
                      setUsername(e.target.value);
                      setValue('userName', e.target.value);
                    }}
                    />
                  <Form.Control.Feedback type="invalid">{ i18next.t("configurationDialog_useOwnDatabase_UsernameFeedback", {ns: 'mycontexts'})}</Form.Control.Feedback>
                  </Col>
                </Form.Group>
                <Form.Group as={Row}>
                  <Form.Label column sm="3">
                    { i18next.t("configurationDialog_useOwnDatabase_Password", {ns: 'mycontexts'})}
                  </Form.Label>
                  <Col sm="9">
                    <Form.Control
                    type="text"
                    placeholder="mypassword"
                    required
                    value={password || ''}
                    onChange={(e) => {
                      setPassword(e.target.value);
                      setValue('password', e.target.value);
                      }}
                  />
                  <Form.Control.Feedback type="invalid">{ i18next.t("configurationDialog_useOwnDatabase_PasswordFeedback", {ns: 'mycontexts'})}</Form.Control.Feedback>
                  </Col>
                </Form.Group>
                </Container>
              )}

            <Slider 
              label={i18next.t("configurationDialog_useOwnKeys", {ns: 'mycontexts'})} 
              callback={setUseOwnKey} />
            {
              useOwnKey && (
                <Container>
                  <KeyPairFileUploader setKeyPairFile={setKeyPair} />
                </Container>)
              }
          </>
        )}

      </Form>
    </Modal.Body>
    <Modal.Footer>
      {/* Clear state and close dialog */}
      <Button variant="secondary" onClick={ ()  =>{
        // clear values from indexedDB
        deleteValue('deviceName');
        deleteValue('identityFile');
        // setValue( perspectivesUserId + PUBLICKEY, keyPair.publicKey )
        deleteValue('privateKey');
        deleteValue('publicKey');
        deleteValue('couchdbUrl');
        deleteValue('couchdbPort');
        deleteValue('userName');
        deleteValue('password');
        onHide()
      }}>
        {i18next.t( "genericClose", {ns: 'mycontexts'})}
      </Button>
      {/* Gather state and put in KeyVal database. Then start actual installation process */}
      <Button variant="primary" onClick={validate}>
      {i18next.t( "conversationDialog_Install", {ns: 'mycontexts'})}
      </Button>
    </Modal.Footer>
  </Modal>)};

function handleInstall ( { deviceName, keyPair, identityFile, couchdbUrl, couchdbPort, userName, password }: InstallationData, callback: (data: InstallationResult) => void ) {
  // A function that generates a CUID using the current epoch as fingerprint.
  const cuid2 = init({
    // A custom random function with the same API as Math.random.
    // You can use this to pass a cryptographically secure random function.
    random: Math.random,
    // the length of the id
    length: 10,
    // A custom fingerprint for the host environment. This is used to help
    // prevent collisions when generating ids in a distributed system.
    fingerprint: Date.now().toString(36)
    });

  let perspectivesUserId;
  if (identityFile) {
    // Get the perspectivesUsersId from the identity file
    // Save the identity file
    perspectivesUserId = takeCUID( identityFile.author )
    perspectivesUserId = identityFile.author;
  } else {
    perspectivesUserId = cuid2();
  }
  setValue('perspectivesUserId', perspectivesUserId);
  
  // If there is no privateKey, generate a new keypair.
  if (keyPair) {
    // Save the private key in a way that it cannot be exported.
    setValue( perspectivesUserId + PUBLICKEY, keyPair.publicKey )
    setValue( perspectivesUserId + PRIVATEKEY, keyPair.privateKey )
    callback({type: 'NoKeyPairData', perspectivesUserId});
  }
    else {
    // Generate a new keypair
    createKeypair(perspectivesUserId).then( ({ privateKey: exportedPrivateKey, publicKey: exportedPublicKey }) => {
      const keyPair = { privateKey: exportedPrivateKey, publicKey: exportedPublicKey };
      setValue( "keyPair", keyPair)
      // The pair should be downloaded for future use.
      callback({ type: 'KeyPairData', perspectivesUserId, keyPair});
    })
  }
  // Save the version of mycontexts.
  setValue('currentMyContextsVersion', __MYCONTEXTS_VERSION__);
};

// Given a CryptoKey, save the private key part in a way that it cannot be exported.
// Return the exported private key as a JsonWebKey.
function savePrivateKey (perspectivesUsersId: string, privateKey: CryptoKey) : Promise<JsonWebKey> {
  let exportedPrivateKey: JsonWebKey;
  return window.crypto.subtle.exportKey( "jwk", privateKey )
    .then( buff => 
      {
        exportedPrivateKey = buff;
        // We must save the exported private key because it appears as if it can only be exported once.
        return window.crypto.subtle.importKey( "jwk", buff, { name: "ECDSA", namedCurve: "P-384" }, false, ["sign"])
      } )
    .then( unextractablePrivateKey => setValue( perspectivesUsersId + PRIVATEKEY, unextractablePrivateKey))
    .then ( () => exportedPrivateKey)
}

function createKeypair (perspectivesUsersId: string) : Promise<KeyPair >
{
  let keypair : CryptoKeyPair, privateKey: JsonWebKey, publicKey: JsonWebKey;
  return window.crypto.subtle.generateKey(
      {
      name: "ECDSA",
      namedCurve: "P-384"
      },
      true, // extractable.
      ["sign", "verify"])
    .then( kp => keypair = kp)
    .then( () => setValue( perspectivesUsersId + PUBLICKEY, keypair.publicKey ) )
    .then( () => savePrivateKey( perspectivesUsersId, keypair.privateKey ) )
    .then( exportedPrivateKey => privateKey = exportedPrivateKey)
    .then( () => window.crypto.subtle.exportKey( "jwk", keypair.publicKey ) )
    .then( buff => publicKey = buff)
    .then( () => ({ privateKey, publicKey }) )
}

function SliderWithTooltip({ label, tooltip, callback }: { label: string, tooltip: string, callback: (e: any) => void }): ReactElement { 
  return (
    <Form.Group as={Row} controlId="formIdentityFile" className="mt-3">
    <Col sm={1}>
    <Form.Check
        type="switch"
        label=""
        onChange={(e) => callback(e.target.checked)}
      />
    </Col>
    <Col sm={11}>
      <OverlayTrigger
            placement="bottom-start"
            delay={{ show: 250, hide: 400 }}
            overlay={(props) => (
              <Tooltip id="MyContexts-tooltip" {...props} show={
                // eslint-disable-next-line react/prop-types
                props.show}>{tooltip}
              </Tooltip> )}
        >
        <Form.Label>{label}</Form.Label>
      </OverlayTrigger>
    </Col>
  </Form.Group>
);
}

function Slider({ label, callback }: { label: string, callback: (e: any) => void }): ReactElement {
  return (
    <Form.Group as={Row} className="mt-3">
    <Col sm={1}>
    <Form.Check
        type="switch"
        label=""
        onChange={(e) => callback(e.target.checked)}
      />
    </Col>
    <Col sm={11}>
      <Form.Label>{label}</Form.Label>
    </Col>
  </Form.Group>
);
}