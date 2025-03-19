import { ChangeEvent, FC } from "react";
import { set as setValue} from 'idb-keyval';
import i18next from 'i18next';
import { Form } from "react-bootstrap";
import * as React from "react";

export interface IdentityFile {author: string}

export const IdentityFileUploader: FC<{ setIdentityFile: (json: IdentityFile | null) => void }> = ({ setIdentityFile: setIdentityFile }) => {
  const [feedback, setFeedback] = React.useState('');

  function handleIdentityFile(event: ChangeEvent<HTMLInputElement>)
  {
    const fileList = event.target.files;
    let json;
    if (fileList?.length) {
      fileList[0].text()
        .then(t => {
          json = JSON.parse(t);
          // If the file is an identity file, set the identity file.
          if (json.author && json.timeStamp && json.deltas && json.publicKeys) {
            event.target.setCustomValidity('');
            setFeedback('')
            setIdentityFile(json);
            setValue('identityFile', json);  
            }
          else {
            // Otherwise tell the end user this is not a correct Identity file. 
            event.target.setCustomValidity(i18next.t("uploadkeypairdialog_wrongIdentityJson", {ns: 'mycontexts'}));
            setIdentityFile(null)
            setFeedback(i18next.t("uploadkeypairdialog_wrongIdentityJson", {ns: 'mycontexts'}));
          }})
        .catch( () => {
          // If the file is not a JSON file, tell the end user to upload a JSON file
          event.target.setCustomValidity( i18next.t("uploadKeypairDialog_NoJson", {ns: 'mycontexts'}));
          setFeedback(i18next.t("uploadKeypairDialog_NoJson", {ns: 'mycontexts'}));
          setIdentityFile(null);
        });
      }
    event.target.reportValidity();
  }

  return (
    <Form.Group controlId="formIdentityFileUpload" className="mt-3">
      <Form.Label>{ i18next.t( "uploadkeypairdialog_UploadIdentityDoc", {ns: 'mycontexts'}) }</Form.Label>
      <Form.Control type="file" required onChange={handleIdentityFile}/>
      <Form.Control.Feedback type="invalid">
          {feedback}
        </Form.Control.Feedback>
    </Form.Group>
  );
}

export interface KeyPair { privateKey: JsonWebKey, publicKey: JsonWebKey }

export const KeyPairFileUploader: FC<{ setKeyPairFile: (json: KeyPair | null) => void }> = ({ setKeyPairFile: setKeyPairFile }) => {
  const [feedback, setFeedback] = React.useState('');
  function handleCryptoKeys(event: ChangeEvent<HTMLInputElement>)
  {
    const fileList = event.target.files;
    let json;
    if (fileList?.length) {
      fileList[0].text()
        .then(t => {
          json = JSON.parse(t);
                  // Check if the file is a keypair file
        if (json.privateKey && json.publicKey) {
          event.target.setCustomValidity('');
            setFeedback('')
            setKeyPairFile(json);
            // We don't store the keypair file in IndexedDB. It would compromise the security of the keypair.
            }
          else {
            // Otherwise tell the end user this is not a correct Identity file. 
            event.target.setCustomValidity(i18next.t("uploadkeypairdialog_wrongJson", {ns: 'mycontexts'}));
            setFeedback(i18next.t("uploadkeypairdialog_wrongJson", {ns: 'mycontexts'}));
            setKeyPairFile(null);
          }})
        .catch( () => {
          // If the file is not a JSON file, tell the end user to upload a JSON file
          event.target.setCustomValidity( i18next.t("uploadKeypairDialog_NoJson", {ns: 'mycontexts'}));
          setFeedback(i18next.t("uploadKeypairDialog_NoJson", {ns: 'mycontexts'}));
          setKeyPairFile(null);
        });
      }
    event.target.reportValidity();
  }

  return (
    <Form.Group controlId="formKeyFileUploadNotFirstInstallation" className="mt-3">
      <Form.Label>{ i18next.t( "uploadkeypairdialog_Title", {ns: "mycontexts"} )}</Form.Label>
      <Form.Control type="file" required onChange={handleCryptoKeys}/>
    <Form.Control.Feedback type="invalid">{ feedback }</Form.Control.Feedback>
  </Form.Group>
);
}