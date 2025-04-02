import React from "react";
import { TableFormDef } from "perspectives-proxy";
import { Component } from "react";
import { TableForms } from "./tableForms";

interface WhoProps {
  screenelements: TableFormDef[];
  showTablesAndForm: boolean;
}
export class Who extends Component<WhoProps> {
  render() {
    // TODO: add chat.
    return <>
      <p>Chat here</p>
      <TableForms screenelements={this.props.screenelements} showTablesAndForm={this.props.showTablesAndForm} />
      </>;
  }
}
