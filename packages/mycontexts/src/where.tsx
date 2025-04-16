import React from "react";
import { TableFormDef, RoleInstanceT } from "perspectives-proxy";
import { Component } from "react";
import { TableForms } from "./tableForms";
import { PinnedContexts } from "./pinnedContexts";

interface WhereProps {
  screenelements: TableFormDef[];
  showTablesAndForm: boolean;
  systemUser: RoleInstanceT;
}
export class Where extends Component<WhereProps> {
  render() {
    // TODO: add indexed contexts and recent contexts.
    return (<><TableForms screenelements={this.props.screenelements} showTablesAndForm={this.props.showTablesAndForm} doubleclickOpensDetails={false} />
    <PinnedContexts systemuser={this.props.systemUser} />
    <p className='bg-light-subtle'>Rendering of the recent contexts.</p>
    </>);
  }
}
