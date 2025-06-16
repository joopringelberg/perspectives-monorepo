
import { Container, Spinner, Row, Col } from 'react-bootstrap';
import {i18next} from 'perspectives-react';

const LoadingScreen = () => {
  return (
    <Container fluid className="vh-100 d-flex flex-column justify-content-center align-items-center bg-primary" style={{'--bs-bg-opacity': '.2'} as React.CSSProperties}>
      <Row className="text-center">
        <Col>
          <img 
            src="AppImages/ios/256.png" 
            alt="MyContexts Logo" 
            className="mb-4"
            style={{ maxHeight: '80px' }} 
          />
        </Col>
      </Row>
      <Row className="text-center">
        <Col>
          <h2 className="mb-4">{i18next.t("loading_welcome", {ns: 'mycontexts'})}</h2>
          <p className="mb-4">{i18next.t("loading_message", {ns: 'mycontexts', version: __MYCONTEXTS_VERSION__, build: __BUILD__})}</p>
          <Spinner animation="border" role="status" variant="primary">
            <span className="visually-hidden">Loading...</span>
          </Spinner>
        </Col>
      </Row>
    </Container>
  );
};

export default LoadingScreen;