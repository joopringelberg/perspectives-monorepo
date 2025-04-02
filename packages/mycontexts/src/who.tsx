import React from "react";
import { TableFormDef, Who as WhoDef } from "perspectives-proxy";
import { Component } from "react";
import { TableForms } from "./tableForms";

interface WhoProps {
  screenelements: WhoDef;
  showTablesAndForm: boolean;
}
export class Who extends Component<WhoProps> {
  render() {
    // TODO: add chat.
    return <>
      <p>Chat here</p>
      <TableForms screenelements={this.props.screenelements.userRoles} showTablesAndForm={this.props.showTablesAndForm} />
      </>;
  }
}
