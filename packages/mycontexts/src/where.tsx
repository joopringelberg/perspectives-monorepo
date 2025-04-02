import React from "react";
import { TableFormDef } from "perspectives-proxy";
import { Component } from "react";
import { TableForms } from "./tableForms";

interface WhereProps {
  screenelements: TableFormDef[];
  showTablesAndForm: boolean;
}
export class Where extends Component<WhereProps> {
  render() {
    // TODO: add indexed contexts and recent contexts.
    return (<><TableForms screenelements={this.props.screenelements} showTablesAndForm={this.props.showTablesAndForm} />
    <p className='bg-light-subtle'>Rendering of the recent contexts.</p>
    <p className='bg-light-subtle'>Rendering of the pinned contexts.</p>
    </>);
  }
}
