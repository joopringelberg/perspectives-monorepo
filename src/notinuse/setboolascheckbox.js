import React from "react";
import PropTypes from "prop-types";

import {PSView} from "./reactcontexts.js";

import {Form, Row, Col, } from "react-bootstrap";

export default function SetBoolAsCheckbox ({propertyname, label})
{
  return  <PSView.Consumer>
            {sprops => {
              // If the value is [], val will be false;
              // If the value is ["true"], val will be true;
              // If the value is ["false"], val will be false.
              // So we have negation by failure (to have a value).
              const val = sprops.propval(propertyname)[0] == "true";
              return <Form.Group as={Row} controlId={propertyname + "_BoolAsCheckbox"}>
                <Col>
                  <input
                    id={propertyname + "_BoolAsCheckbox"}
                    type="checkbox"
                    defaultChecked={val ? "checked" : null}
                    disabled={val ? "disabled" : null}
                    // We start out with false and only allow changing once, so that must be to "true"!
                    onChange={() => sprops.propset(propertyname, "true" )} />
                </Col>
                <Form.Label>{label}</Form.Label>
              </Form.Group>;}}
          </PSView.Consumer>;

}

SetBoolAsCheckbox.propTypes =
  { propertyname: PropTypes.string
  , label: PropTypes.string
  , val: PropTypes.bool
  };
