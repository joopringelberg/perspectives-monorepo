// UpdateNotification.tsx

import { Modal, Button } from 'react-bootstrap';
import { i18next } from 'perspectives-react';

interface UpdateNotificationProps {
  show: boolean;
  onUpdate: () => void;
  onDismiss: () => void;
}

const UpdateNotification: React.FC<UpdateNotificationProps> = ({ show, onUpdate, onDismiss }) => {
  console.log("UpdateNotification rendering, show=", show);
  
  return (
    <Modal show={show} onHide={onDismiss} backdrop="static" keyboard={false} centered>
      <Modal.Header>
        <Modal.Title>
          {i18next.t("update_available_title", {ns: 'mycontexts'})}
        </Modal.Title>
      </Modal.Header>
      <Modal.Body>
        {i18next.t("update_available_message", {ns: 'mycontexts'})}
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onDismiss}>
          {i18next.t("update_later", {ns: 'mycontexts'})}
        </Button>
        <Button variant="primary" onClick={onUpdate}>
          {i18next.t("update_now", {ns: 'mycontexts'})}
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default UpdateNotification;