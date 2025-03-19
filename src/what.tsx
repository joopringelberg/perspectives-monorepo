import React from "react";
import { MainScreenElements, What as WhatDef } from "perspectives-proxy";
import { FreeFormScreen, PerspectivesComponent, PSContext } from "perspectives-react";

interface WhatProps {
  screenelements: WhatDef;
}

export class What extends PerspectivesComponent<WhatProps>{
  
  constructor(props: WhatProps) {
    super(props);
  }

  render() {
    switch (this.props.screenelements.tag) {
      case "TableForms":
        return <p>TableForms come here</p>
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
