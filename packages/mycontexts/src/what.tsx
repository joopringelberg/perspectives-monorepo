import React from "react";
import { MainScreenElements, TableFormDef, What as WhatDef } from "perspectives-proxy";
import { FreeFormScreen, PerspectivesComponent, PSContext } from "perspectives-react";
import { TableForms } from "./tableForms";

interface WhatProps {
  screenelements: WhatDef;
  showTablesAndForm: boolean;
}

export class What extends PerspectivesComponent<WhatProps>{
  
  constructor(props: WhatProps) {
    super(props);
  }

  render() {
    switch (this.props.screenelements.tag) {
      case "TableForms":
        return <TableForms screenelements={this.props.screenelements.elements as TableFormDef[]} showTablesAndForm={this.props.showTablesAndForm} doubleclickOpensDetails={true}/>;
      case "FreeFormScreen":
        return <PSContext.Consumer>{
          context => <FreeFormScreen 
            screen={this.props.screenelements.elements as MainScreenElements}
            contextinstance={context.contextinstance}
            contexttype={context.contexttype}
            myroletype={context.myroletype}
          />}
          </PSContext.Consumer>
    }}
}
