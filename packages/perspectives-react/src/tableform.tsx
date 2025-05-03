// OBSOLETE: This file is no longer used in the project. It was used to create a table form component that is now replaced by a more generic form component.


import React from "react";
import { ContextInstanceT, PDRproxy, RoleType, TableFormDef } from "perspectives-proxy";
import PerspectivesComponent from "./perspectivesComponent";
import modelDependencies from "./modelDependencies";

interface TableFormProps {
  contextinstance: ContextInstanceT;
  roletype: RoleType;
  fireandforget: boolean;
}

interface TableFormState {
  tableForm: TableFormDef;
} 
export class TableForm extends PerspectivesComponent<TableFormProps, TableFormState> {
  constructor(props : TableFormProps) {
    super(props);
  }

  componentDidMount(): void {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        component.addUnsubscriber(
          pproxy.getTableForm( modelDependencies.sysUser,
            component.props.contextinstance,
            component.props.roletype,
            tableForms => tableForms[0] ? component.setState({tableForm: tableForms[0]}) : null,
            component.props.fireandforget
          ))
      });
  }

  render() {
    if (this.state.tableForm) {
      return (
        <div>
          <h1>TableForm</h1>
        </div>
      );
    }
    else {
      return null;
    }
  }
}