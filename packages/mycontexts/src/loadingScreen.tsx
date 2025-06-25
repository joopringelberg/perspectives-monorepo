import * as React from 'react';
import { Container, Spinner, Row, Col } from 'react-bootstrap';
import {i18next} from 'perspectives-react';

const LoadingScreen = () => {
  return (
    <>
      {/* Add skip link for keyboard users */}
      <a href="#loading-content" className="skip-link visually-hidden-focusable">
        Skip to content
      </a>
      
      <Container fluid className="vh-100 d-flex flex-column justify-content-center align-items-center bg-primary" style={{'--bs-bg-opacity': '.2'} as React.CSSProperties}>
        {/* Add header landmark */}
        <header className="text-center">
          <Row>
            <Col>
              <img 
                src="AppImages/ios/256.png" 
                alt="MyContexts Logo" 
                className="mb-4"
                style={{ maxHeight: '80px' }} 
              />
            </Col>
          </Row>
        </header>
        
        {/* Add main landmark with appropriate ID for skip link */}
        <main id="loading-content" className="text-center">
          <Row>
            <Col>
              {/* Change h2 to h1 for proper document structure */}
              <h1 className="mb-4">{i18next.t("loading_welcome", {ns: 'mycontexts'})}</h1>
              <p className="mb-4 text-dark">{i18next.t("loading_message", {ns: 'mycontexts', version: __MYCONTEXTS_VERSION__, build: __BUILD__})}</p>
              <Spinner animation="border" role="status" variant="primary">
                <span className="visually-hidden">Loading...</span>
              </Spinner>
            </Col>
          </Row>
        </main>
      </Container>
    </>
  );
};

export default LoadingScreen;