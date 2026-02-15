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
        <Form.Group className="mb-3">
          <Form.Label htmlFor="pdr-version-input">{ i18next.t("pdr_version", {ns: "mycontexts"})}</Form.Label>
          <Form.Control 
            id="pdr-version-input"
            className="accessible-form-control" 
            type="text" 
            readOnly 
            value={__PDR_VERSION__}
          />
        </Form.Group>
        <Form.Group className="mb-3">
          <Form.Label htmlFor="keyboard-shortcuts">{i18next.t("about_shortcuts", {ns: "mycontexts"})}</Form.Label>
          <div id="keyboard-shortcuts" className="p-3 border rounded">
            <h2 className="column-heading">{i18next.t("about_navigation", {ns: "mycontexts"})}</h2>
            <ul>
              <li>{i18next.t("about_navigation_message", {ns: "mycontexts"})}</li>
              <li>{i18next.t("about_navigation_who", {ns: "mycontexts"})}</li>
              <li>{i18next.t("about_navigation_what", {ns: "mycontexts"})}</li>
              <li>{i18next.t("about_navigation_where", {ns: "mycontexts"})}</li>
              <li>{i18next.t("about_navigation_close", {ns: "mycontexts"})}</li>
              <li>{i18next.t("about_navigation_arrows", {ns: "mycontexts"})}</li>
              <li>{i18next.t("about_navigation_space", {ns: "mycontexts"})}</li>
            </ul> 
          </div>
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