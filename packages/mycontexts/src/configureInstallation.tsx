import * as React from 'react'
const { useState } = React;
import type {FC, ReactElement} from 'react';
import { Container, Navbar, Button, Modal, Form, Col, Row } from 'react-bootstrap';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Tooltip from 'react-bootstrap/Tooltip';
import { set as setValue, del as deleteValue} from 'idb-keyval';
import * as Cuid2 from '@paralleldrive/cuid2';
import { takeCUID } from 'perspectives-react';
import {i18next} from 'perspectives-react';
import { IdentityFile, IdentityFileUploader, KeyPair, KeyPairFileUploader } from './configurationComponents';
import { FAQModal } from './faqs';


const PUBLICKEY = "_publicKey";
const PRIVATEKEY = "_privateKey"; 
const TRANSPORT_PUBLICKEY = "_transportPublicKey";
const TRANSPORT_PRIVATEKEY = "_transportPrivateKey";

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
      <header>
        <Navbar bg='primary' fixed="top">
          <Navbar.Brand className="mx-auto text-white">{ i18next.t( "configuration_Welcome", {ns: 'mycontexts'})}</Navbar.Brand>
        </Navbar>
      </header>
      <main>
        <Container fluid className="main-content" style={{ minHeight: '100vh', display: 'flex', flexDirection: 'column', justifyContent: 'flex-start', paddingTop: '120px' }}>
          <h1 className="visually-hidden">{ i18next.t( "configuration_Welcome", {ns: 'mycontexts'})}</h1>
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
      </main>
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
      <Modal.Title as="h2">{ i18next.t( "configurationDialog_Title", {ns: 'mycontexts'})}</Modal.Title>
    </Modal.Header>
    <Modal.Body>
      <Form noValidate validated={validated} id='installForm'>
        <Form.Group controlId="formDeviceName">
          <Form.Label>{i18next.t("configurationDialog_deviceName", {ns: 'mycontexts'})}</Form.Label>
          <Form.Control
            type="text"
            placeholder="E.g. mylaptop, mymobile, mytablet"
            value={deviceName || ''}
            required
             
            pattern="^[a-zA-Z0-9_\-\.]+$"  // Allow only letters, numbers, underscores, hyphens and periods
            onChange={(e) => {
              const value = e.target.value;
              // Check if the value matches our allowed pattern
              // eslint-disable-next-line no-useless-escape
              if (/^[a-zA-Z0-9_\-\.]*$/.test(value)) {
                setDeviceName(value);
                setValue('deviceName', value);
              }
            }}
            // eslint-disable-next-line no-useless-escape
            isInvalid={!!deviceName && !/^[a-zA-Z0-9_\-\.]+$/.test(deviceName)}
          />
          <Form.Control.Feedback type="invalid">
            {!deviceName 
              ? i18next.t("configurationDialog_deviceNameFeedback", {ns: 'mycontexts'})
              : i18next.t("configurationDialog_deviceNameInvalidChars", {ns: 'mycontexts', defaultValue: "Device name can only contain letters, numbers, underscores, hyphens and periods (no spaces)."})
            }
          </Form.Control.Feedback>
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
        deleteValue('transportPrivateKey');
        deleteValue('transportPublicKey');
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
  const cuid2 = Cuid2.init({
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
  } else {
    perspectivesUserId = cuid2();
  }
  setValue('perspectivesUserId', perspectivesUserId);
  
  // If there is no privateKey, generate a new keypair.
  if (keyPair) {
    // Import provided JWKs into CryptoKeys and persist like createKeypair/savePrivateKey
    const importPublicKey = window.crypto.subtle.importKey(
      'jwk',
      keyPair.publicKey,
      { name: 'ECDSA', namedCurve: 'P-384' },
      true,
      ['verify']
    );
    const importPrivateKey = window.crypto.subtle.importKey(
      'jwk',
      keyPair.privateKey,
      { name: 'ECDSA', namedCurve: 'P-384' },
      true,
      ['sign']
    );
    const importTransportPublicKey = window.crypto.subtle.importKey(
      'jwk',
      keyPair.transportPublicKey,
      { name: 'RSA-OAEP', hash: 'SHA-256' },
      true,
      ['encrypt']
    );
    const importTransportPrivateKey = window.crypto.subtle.importKey(
      'jwk',
      keyPair.transportPrivateKey,
      { name: 'RSA-OAEP', hash: 'SHA-256' },
      true,
      ['decrypt']
    );

    Promise.all([importPublicKey, importPrivateKey, importTransportPublicKey, importTransportPrivateKey])
      .then(([pubKey, privKey, transportPubKey, transportPrivKey]) =>
        Promise.all([
          setValue(perspectivesUserId + PUBLICKEY, pubKey),
          savePrivateKey(perspectivesUserId, PRIVATEKEY, privKey, { name: 'ECDSA', namedCurve: 'P-384' }, ['sign']),
          setValue(perspectivesUserId + TRANSPORT_PUBLICKEY, transportPubKey),
          savePrivateKey(perspectivesUserId, TRANSPORT_PRIVATEKEY, transportPrivKey, { name: 'RSA-OAEP', hash: 'SHA-256' }, ['decrypt'])
        ])
      )
      .then(() => {
        // No need to return the pair for download; user supplied it
        callback({ type: 'NoKeyPairData', perspectivesUserId });
      });
  }
    else {
    // Generate a new keypair
    createKeypair(perspectivesUserId).then( ({ privateKey: exportedPrivateKey, publicKey: exportedPublicKey, transportPrivateKey, transportPublicKey }) => {
      const keyPair = { privateKey: exportedPrivateKey, publicKey: exportedPublicKey, transportPrivateKey, transportPublicKey };
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
function savePrivateKey (
  perspectivesUsersId: string,
  keySuffix: string,
  privateKey: CryptoKey,
  algorithm: EcKeyImportParams | RsaHashedImportParams,
  keyUsages: KeyUsage[]
) : Promise<JsonWebKey> {
  let exportedPrivateKey: JsonWebKey;
  return window.crypto.subtle.exportKey( "jwk", privateKey )
    .then( buff => 
      {
        exportedPrivateKey = buff;
        // We must save the exported private key because it appears as if it can only be exported once.
        return window.crypto.subtle.importKey( "jwk", buff, algorithm, false, keyUsages)
      } )
    .then( unextractablePrivateKey => setValue( perspectivesUsersId + keySuffix, unextractablePrivateKey))
    .then ( () => exportedPrivateKey)
}

function createKeypair (perspectivesUsersId: string) : Promise<KeyPair >
{
  let signingKeypair : CryptoKeyPair, transportKeypair: CryptoKeyPair, privateKey: JsonWebKey, publicKey: JsonWebKey, transportPrivateKey: JsonWebKey, transportPublicKey: JsonWebKey;
  return window.crypto.subtle.generateKey(
      {
      name: "ECDSA",
      namedCurve: "P-384"
      },
      true, // extractable.
      ["sign", "verify"])
    .then( kp => signingKeypair = kp)
    .then(() => window.crypto.subtle.generateKey(
      {
        name: 'RSA-OAEP',
        modulusLength: 4096,
        publicExponent: new Uint8Array([1, 0, 1]),
        hash: 'SHA-256'
      },
      true,
      ['encrypt', 'decrypt']))
    .then(kp => transportKeypair = kp)
    .then( () => setValue( perspectivesUsersId + PUBLICKEY, signingKeypair.publicKey ) )
    .then( () => savePrivateKey( perspectivesUsersId, PRIVATEKEY, signingKeypair.privateKey, { name: "ECDSA", namedCurve: "P-384" }, ["sign"]) )
    .then( exportedPrivateKey => privateKey = exportedPrivateKey)
    .then( () => window.crypto.subtle.exportKey( "jwk", signingKeypair.publicKey ) )
    .then( buff => publicKey = buff)
    .then(() => setValue(perspectivesUsersId + TRANSPORT_PUBLICKEY, transportKeypair.publicKey))
    .then(() => savePrivateKey(perspectivesUsersId, TRANSPORT_PRIVATEKEY, transportKeypair.privateKey, { name: 'RSA-OAEP', hash: 'SHA-256' }, ['decrypt']))
    .then(exportedPrivateKey => transportPrivateKey = exportedPrivateKey)
    .then(() => window.crypto.subtle.exportKey("jwk", transportKeypair.publicKey))
    .then(buff => transportPublicKey = buff)
    .then( () => ({ privateKey, publicKey, transportPrivateKey, transportPublicKey }) )
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