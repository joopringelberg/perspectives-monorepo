import * as React from "react"
import { createRef } from "react";
import { PDRproxy, RoleInstanceT, ContextAndName } from "perspectives-proxy";
import { i18next, ModelDependencies, PerspectivesComponent, PerspectiveTable } from "perspectives-react";
import { Accordion, ListGroup } from "react-bootstrap";

interface WiderContextsProps {
  externalrole?: RoleInstanceT;
}

interface WiderContextsState {
  widerContexts: ContextAndName[];
}

export class WiderContexts extends PerspectivesComponent<WiderContextsProps, WiderContextsState> {
  private domEl!: React.RefObject<HTMLDivElement | null>;
  constructor(props: WiderContextsProps) {
    super(props);
    this.state = {widerContexts: []};
    this.domEl = React.createRef<HTMLDivElement>();
  }

  componentDidMount = this.getWiderContexts;

  componentDidUpdate(prevProps: Readonly<WiderContextsProps>, prevState: Readonly<WiderContextsState>, snapshot?: any): void {
    if (this.props.externalrole !== prevProps.externalrole) {
      this.unsubscribeAll();
      this.getWiderContexts();
    }
  }

  getWiderContexts() {
    const component = this;
    if (component.props.externalrole)
    {
      PDRproxy.then((PDRproxy) => {
        component.addUnsubscriber(
          PDRproxy.getWiderContexts( component.props.externalrole!, widerContexts => {
            component.setState({ widerContexts: widerContexts });
          }));
      })
    }
  }

  openContext(externalRole: RoleInstanceT) {
    const component = this;
    component.domEl.current!.dispatchEvent(new CustomEvent('OpenContext', { detail: externalRole, bubbles: true }));
  }

  render() {
    const component = this;
    if (this.state.widerContexts.length === 0) {
      return null
    }
    else {
      return  (<div className="mb-2" ref={component.domEl}>
                <h3 className="column-heading">{i18next.t("wider_context", {ns: 'mycontexts'})}</h3>
                <ListGroup defaultActiveKey="#link1">
                {
                  this.state.widerContexts.map(({ externalRole, readableName }) => {
                    return (
                      <ListGroup.Item key={externalRole} action onDoubleClick={(_ : any) => component.openContext(externalRole)}>
                        {readableName}
                      </ListGroup.Item>
                    );
                  })
                }
                </ListGroup>
              </div>)
    }

    return (  <Accordion.Item eventKey="widercontexts" key="widercontexts" ref={component.domEl}>
                <Accordion.Header onClick={() => component.domEl.current?.dispatchEvent(new CustomEvent('OpenAccordionItem', {detail: "widercontexts", bubbles: true}))}>
                  { i18next.t( "wider_contexts_title", {ns: 'mycontexts'})}
                </Accordion.Header>
                <Accordion.Body>
                  <ListGroup defaultActiveKey="#link1">
                  {
                    this.state.widerContexts.map(({ externalRole, readableName }) => {
                      return (
                        <ListGroup.Item key={externalRole} action onDoubleClick={(_ : any) => component.openContext(externalRole)}>
                          {readableName}
                        </ListGroup.Item>
                      );
                    })
                  }
                  </ListGroup>
                </Accordion.Body>
              </Accordion.Item>);
  }
}