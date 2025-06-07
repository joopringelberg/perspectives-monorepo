import React, { } from "react";
import { i18next, PerspectivesComponent } from "perspectives-react";
import { Form } from "react-bootstrap";
export class About extends PerspectivesComponent<{}, {}> {
  constructor(props: {}) {
    super(props);
  }

  render() {
    return (
      <Form>
        <Form.Group className="mb-3">
          <Form.Label>{ i18next.t("mycontexts_version")}</Form.Label>
          <Form.Control type="text" readOnly value={__MYCONTEXTS_VERSION__}/>
        </Form.Group>
        <Form.Group className="mb-3">
          <Form.Label>{ i18next.t("mycontexts_build")}</Form.Label>
          <Form.Control type="text" readOnly value={__BUILD__}/>
        </Form.Group>
      </Form>
    );
  }
}