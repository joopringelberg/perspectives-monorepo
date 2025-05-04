import React from "react";
import { TableFormDef, RoleInstanceT, ContextInstanceT, WhereTo } from "perspectives-proxy";
import { Component } from "react";
import { TableForms } from "./tableForms";
import { PinnedContexts } from "./pinnedContexts";
import { RecentContexts } from "./recentContexts";
import { Accordion } from "react-bootstrap";
import { buildMarkDown, PSContext } from "perspectives-react";

interface WhereProps {
  screenelements: WhereTo;
  showTablesAndForm: boolean;
  systemUser: RoleInstanceT;
  systemIdentifier: ContextInstanceT;
  openContext: RoleInstanceT | undefined;
}
export class Where extends Component<WhereProps> {
  render() {
    return (<PSContext.Consumer>{ value => 
    <>
      {this.props.screenelements.markdown.map((markdown, index) => 
          <div key={index}>{ buildMarkDown(value.contextinstance, value.myroletype, markdown) }</div>
        )}
      <TableForms screenelements={this.props.screenelements.contextRoles} showTablesAndForm={this.props.showTablesAndForm} doubleclickOpensDetails={false} />
      <Accordion defaultActiveKey="0" flush>
        <PinnedContexts systemuser={this.props.systemUser} />
        <RecentContexts systemuser={this.props.systemUser} openContext={this.props.openContext} systemIdentifier={this.props.systemIdentifier}/>
      </Accordion>
    </>
    }</PSContext.Consumer>);
  }
}
