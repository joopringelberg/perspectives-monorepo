import React from "react";
import { TableFormDef, RoleInstanceT, ContextInstanceT } from "perspectives-proxy";
import { Component } from "react";
import { TableForms } from "./tableForms";
import { PinnedContexts } from "./pinnedContexts";
import { RecentContexts } from "./recentContexts";

interface WhereProps {
  screenelements: TableFormDef[];
  showTablesAndForm: boolean;
  systemUser: RoleInstanceT;
  systemIdentifier: ContextInstanceT;
  openContext: RoleInstanceT | undefined;
}
export class Where extends Component<WhereProps> {
  render() {
    // TODO: add indexed contexts and recent contexts.
    return (<><TableForms screenelements={this.props.screenelements} showTablesAndForm={this.props.showTablesAndForm} doubleclickOpensDetails={false} />
    <PinnedContexts systemuser={this.props.systemUser} />
    <RecentContexts systemuser={this.props.systemUser} openContext={this.props.openContext} systemIdentifier={this.props.systemIdentifier}/>
    </>);
  }
}
