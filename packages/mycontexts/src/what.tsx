import React from "react";
import { MainScreenElements, TableFormDef, What as WhatDef } from "perspectives-proxy";
import { buildMarkDown, FreeFormScreen, PerspectivesComponent, PSContext } from "perspectives-react";
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
        const contextinstance = this.props.screenelements.elements.tableForms[0].table.widgetCommonFields.perspective.contextInstance;
        const myroletype = this.props.screenelements.elements.tableForms[0].table.widgetCommonFields.perspective.userRoleType;
        return (<>
          {this.props.screenelements.elements.markdown.map((markdown, index) => 
            <div key={index}>{ buildMarkDown(contextinstance, myroletype, markdown) }</div>
          )}
          <TableForms screenelements={this.props.screenelements.elements.tableForms} showTablesAndForm={this.props.showTablesAndForm} doubleclickOpensDetails={true}/>
          { <TableForms screenelements={this.props.screenelements.elements.tableForms as TableFormDef[]} showTablesAndForm={this.props.showTablesAndForm} doubleclickOpensDetails={true}/> }
        </>);
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
