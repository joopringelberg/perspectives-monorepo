import React, { Component } from "react";
import { AppContext, PSContextType } from "./reactcontexts.js";

import "./components.css";
import { RoleInstanceT, Perspective } from "perspectives-proxy";
import TableItemContextMenu from "./tableItemContextMenu.js";

interface AccordionHeaderWithMenuProps {
  perspective: Perspective;
  roleinstance: RoleInstanceT;
}
export class AccordionHeaderWithmenu extends Component<AccordionHeaderWithMenuProps> {
  constructor(props : AccordionHeaderWithMenuProps) {
    super(props);
  }

  render() {
    const component = this;
  
    return (
      <div className="d-flex align-items-center w-100 justify-content-between m-0">
        <span>{this.props.perspective.displayName}</span>
        <AppContext.Consumer>
          {({ roleOnClipboard }) => 
            <TableItemContextMenu 
              perspective={component.props.perspective} 
              roleinstance={component.props.roleinstance}
              roleOnClipboard={roleOnClipboard}/>
        }
        </AppContext.Consumer>
      </div> 
    );
    }
}

