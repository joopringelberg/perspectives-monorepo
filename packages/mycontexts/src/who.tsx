import React from "react";
import { FormElementDef, RoleInstanceT, TableFormDef } from "perspectives-proxy";
import { buildForm, buildTable } from "perspectives-react";
import { Component } from "react";
import { Container, Row } from "react-bootstrap";
import MSComponent, { SlidingPanelContentProps } from "./mscomponent";
import { TableForms } from "./tableForms";

interface WhoProps {
  screenelements: TableFormDef[];
  showTablesAndForm: boolean;
}
export class Who extends Component<WhoProps> {
  render() {
    // TODO: add chat.
    return <TableForms screenelements={this.props.screenelements} showTablesAndForm={this.props.showTablesAndForm} />;
  }
}
