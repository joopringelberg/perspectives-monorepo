import * as React from "react";
const { Component } = React;
import { RoleInstanceT, ContextInstanceT, WhereTo } from "perspectives-proxy";
import { TableForms } from "./tableForms";
import { PinnedContexts } from "./pinnedContexts";
import { RecentContexts } from "./recentContexts";
import { Accordion, Container } from "react-bootstrap";
import { buildMarkDown, i18next, ModelDependencies, PSContext } from "perspectives-react";
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
        (e : CustomEvent) => {
          component.setState({accordionOpen: []});  
        }, 
        false);
      this.ref.current.addEventListener(
        'OpenAccordionItem',
        (e : CustomEvent) => {
          if (component.state.accordionOpen.indexOf( e.detail ) === -1) {
            component.setState({accordionOpen: [e.detail]});
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
    (<div className="content-top-aligned px-0">
      {this.props.screenelements.markdown.map((markdown, index) => 
          <div key={index} className="markdown">{ buildMarkDown(value.contextinstance, value.myroletype, markdown) }</div>
        )}
      <WiderContexts externalrole={component.props.openContext}/>
      {
        this.props.screenelements.contextRoles.length > 0 ?
        <div>
          <h3 className="column-heading">{i18next.t("dive_in", {ns: 'mycontexts'})}</h3>
          <TableForms screenelements={this.props.screenelements.contextRoles} showTablesAndForm={this.props.showTablesAndForm} />
        </div>
        : null
      }
      <h3 className="column-heading">{i18next.t("jump_to", {ns: 'mycontexts'})}</h3>
      <Accordion ref={this.ref} activeKey={this.state.accordionOpen} flush className="pb-3">
        <PinnedContexts systemuser={this.props.systemUser} />
        <RecentContexts systemuser={this.props.systemUser} openContext={this.props.openContext} systemIdentifier={this.props.systemIdentifier}/>
      </Accordion>
    </div>)
    }</PSContext.Consumer>);
  }
}
