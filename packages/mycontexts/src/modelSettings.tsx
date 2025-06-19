import * as React from "react";
import { PDRproxy, Perspective } from "perspectives-proxy";
import { ModelDependencies, PerspectiveBasedForm, PerspectivesComponent } from "perspectives-react";
import { Accordion, Container } from "react-bootstrap";

interface ModelSettingsProps {
}

interface ModelSettingsState {
  perspectives: Perspective[];
}

export class ModelSettings extends PerspectivesComponent<ModelSettingsProps, ModelSettingsState>
{
  constructor(props: ModelSettingsProps) {
    super(props);
    this.state = {
      perspectives: [],
    };
  }

  componentDidMount() {
    const component = this;
    PDRproxy.then(pproxy => 
      pproxy.getSettings( (perspectives : Perspective[]) => {
        component.setState({ perspectives });
      }));
  }

  render() {
    return (<Container>
      <Accordion>{
        this.state.perspectives.map((perspective) => (
          <Accordion.Item key={perspective.id} eventKey={perspective.id}>
          <Accordion.Header>{perspective.displayName}</Accordion.Header>
          <Accordion.Body>
            <PerspectiveBasedForm 
              perspective={perspective} 
              showControls={false}
              cardtitle={ModelDependencies.roleWithIdProp}
              suppressIdentifyingProperty={true}/> 
          </Accordion.Body>
          </Accordion.Item>
        ))
      }</Accordion>
    </Container>)
  }
}