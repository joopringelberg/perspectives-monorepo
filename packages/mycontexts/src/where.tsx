import React from "react";
import { RoleInstanceT, ContextInstanceT, WhereTo } from "perspectives-proxy";
import { Component } from "react";
import { TableForms } from "./tableForms";
import { PinnedContexts } from "./pinnedContexts";
import { RecentContexts } from "./recentContexts";
import { Accordion, Container } from "react-bootstrap";
import { buildMarkDown, PSContext } from "perspectives-react";
import { WiderContexts } from "./widerContexts";

interface WhereProps {
  screenelements: WhereTo;
  showTablesAndForm: boolean;
  systemUser: RoleInstanceT;
  systemIdentifier: ContextInstanceT;
  openContext: RoleInstanceT | undefined;
}

interface WhereState {
  accordionOpen: string[];
}

export class Where extends Component<WhereProps, WhereState> {
  ref: React.RefObject<HTMLDivElement | null>;

  constructor(props: WhereProps) {
    super(props);
    this.ref = React.createRef();
    this.state = { accordionOpen: [] };
  }

  componentDidMount() {
    const component = this;
    if (this.ref.current) {
      this.ref.current.addEventListener(
        'OpenContext', 
        (e: Event) => {
          component.setState({accordionOpen: []});  
        }, 
        false);
      this.ref.current.addEventListener(
        'OpenAccordionItem',
        (e: Event) => {
          const customEvent = e as CustomEvent;
          if (component.state.accordionOpen.indexOf(customEvent.detail) === -1) {
            component.setState({accordionOpen: [customEvent.detail]});
          }
          else {
            component.setState({accordionOpen: []});
          }
        },
        false)
    }
  }

  render() {
    const component = this;
    return (<PSContext.Consumer>{ value => 
    (<div className="content-top-aligned">
      {this.props.screenelements.markdown.map((markdown, index) => 
          <div key={index} className="markdown">{ buildMarkDown(value.contextinstance, value.myroletype, markdown) }</div>
        )}
      <TableForms screenelements={this.props.screenelements.contextRoles} showTablesAndForm={this.props.showTablesAndForm} doubleclickOpensDetails={false} />
      <Accordion ref={this.ref} activeKey={this.state.accordionOpen} flush className="pb-3">
        <WiderContexts externalrole={component.props.openContext}/>
        <PinnedContexts systemuser={this.props.systemUser} />
        <RecentContexts systemuser={this.props.systemUser} openContext={this.props.openContext} systemIdentifier={this.props.systemIdentifier}/>
      </Accordion>
    </div>)
    }</PSContext.Consumer>);
  }
}
