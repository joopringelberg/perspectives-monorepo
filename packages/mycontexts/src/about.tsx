import React, { } from "react";
import { i18next, PerspectivesComponent, UserMessagingPromise } from "perspectives-react";
import { Button, Form } from "react-bootstrap";

export class About extends PerspectivesComponent<object, object> {
  constructor(props:  object) {
    super(props);
  }

  render() {
    return (
      <Form>
        <Form.Group className="mb-3">
          <Form.Label htmlFor="version-input">{ i18next.t("mycontexts_version")}</Form.Label>
          <Form.Control 
            id="version-input"
            className="accessible-form-control" 
            type="text" 
            readOnly 
            value={__MYCONTEXTS_VERSION__}
          />
        </Form.Group>
        <Form.Group className="mb-3">
          <Form.Label htmlFor="build-input">{ i18next.t("mycontexts_build")}</Form.Label>
          <Form.Control 
            id="build-input"
            className="accessible-form-control" 
            type="text" 
            readOnly 
            value={__BUILD__}
          />
        </Form.Group>
        <Button variant="primary" onClick={checkForUpdates}>
          {i18next.t("check_for_updates", { ns: 'mycontexts' })}
        </Button>
      </Form>
    );
  }
}

const checkForUpdates = () => {
  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.getRegistration().then(registration => {
      if (registration) {
        registration.update();
        UserMessagingPromise.then(um => {
          um.addMessageForEndUser({
            title: i18next.t("update_checking_title", {ns: 'mycontexts'}),
            message: i18next.t("update_checking_message", {ns: 'mycontexts'})
          });
        });
      }
    });
  }
};