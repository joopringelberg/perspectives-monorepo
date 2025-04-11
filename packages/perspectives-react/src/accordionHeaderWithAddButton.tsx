import React, { Component } from "react";
import { AppContext, PSContextType } from "./reactcontexts.js";
import i18next from "i18next";

import "./components.css";
import { RoleInstanceT, Perspective, PDRproxy, ContextType } from "perspectives-proxy";
import { UserMessagingPromise } from "./userMessaging";
import CreateContextDropDown from "./createContextDropdown";
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

