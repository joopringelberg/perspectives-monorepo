import React, { FC } from "react";
import { Container, Row, Col, Button } from "react-bootstrap";
import i18next from "i18next";
import { KeyPair } from "./configurationComponents";

export const SaveKeyPair: FC<{keypair : KeyPair, perspectivesuserid: String}> = ({keypair, perspectivesuserid}) => {
  const base64Data = btoa(encodeURIComponent(JSON.stringify(keypair)));
  const fileName = perspectivesuserid + "_keypair.json";

  return (
    <Container>
      <Row>
        <Col>
          <Button 
            as="a"
            variant="primary" 
            href={`data:application/json;base64,${base64Data}`}
            download={fileName}
            >
            { i18next.t( "savekeypairdialog_Download", {ns: "mycontexts"} ) }
          </Button>
        </Col>
      </Row>
    </Container>
  );
  }
