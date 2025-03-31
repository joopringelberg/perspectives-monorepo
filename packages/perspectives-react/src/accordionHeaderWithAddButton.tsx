import React, { Component, createRef } from "react";
import {PSContextType, PSRol} from "./reactcontexts.js";
import i18next from "i18next";

import "./components.css";
import { RoleInstanceT, Perspective, PDRproxy, ContextType } from "perspectives-proxy";
import { UserMessagingPromise } from "./userMessaging";
import CreateContextDropDown from "./createContextDropdown";

interface AccordionHeaderWithAddButtonProps {
  perspective: Perspective;
  selectedroleinstance?: RoleInstanceT;
  onAddItem: () => void;
}
export class AccordionHeaderWithAddButton extends Component<AccordionHeaderWithAddButtonProps> {
  constructor(props : AccordionHeaderWithAddButtonProps) {
    super(props);
  }

  mayCreateInstance()
  {
    const perspective = this.props.perspective;
    return !perspective.isCalculated && perspective.verbs.includes("Create");
  }

  mayCreateContext()
  {
    const perspective = this.props.perspective;
    return !perspective.isCalculated && perspective.roleKind == "ContextRole" && perspective.verbs.includes("CreateAndFill");
  }

  createRole (receiveResponse : (roleInstance: RoleInstanceT) => void, contextToCreate? : ContextType | "JustTheRole")
  {
    const component = this;
    const roleType = component.props.perspective.roleType;
    PDRproxy.then( function (pproxy)
    {
      // If a ContextRole Kind, create a new context, too.
      if (  component.props.perspective.roleKind == "ContextRole" &&
            contextToCreate != "JustTheRole" &&
            contextToCreate &&
            roleType)
      {
        pproxy.createContext (
            {
              //id will be set in the core.
              prototype : undefined,
              ctype: contextToCreate,
              rollen: {},
              externeProperties: {}
            },
            roleType,                                                   // qualified role name
            component.props.perspective.contextIdToAddRoleInstanceTo,   // the context instance to add to.
            component.props.perspective.userRoleType)
          .then(contextAndExternalRole => contextAndExternalRole[1])
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("createContext_title", { ns: 'preact' }) 
              , message: i18next.t("createContext_message", {ns: 'preact', type: (component.context as PSContextType).contexttype})
              , error: e.toString()
              })));
        ;
      }
      else if (roleType)
      {
        pproxy
          .createRole (
            component.props.perspective.contextIdToAddRoleInstanceTo,
            roleType,
            component.props.perspective.userRoleType)
          .then( receiveResponse )
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("createRole_title", { ns: 'preact' }) 
              , message: i18next.t("createRole_message", {ns: 'preact', roletype: roleType})
              , error: e.toString()
              })))
    }
    });
  }

  render() {
    const component = this;
  
    return (
      <div className="d-flex align-items-center w-100 justify-content-between">
        <span>{this.props.perspective.displayName}</span>
                {
                  component.mayCreateContext() ?
                  <CreateContextDropDown 
                    contexts={component.props.perspective.contextTypesToCreate}
                    create={ contextToCreate => component.createRole( function() {}, contextToCreate)}
                  />
                  : component.mayCreateInstance() ?
                  <CreateContextDropDown 
                    contexts={{}}
                    create={ () => component.createRole( function() {}, "JustTheRole")}
                  />
                  : null
                }
      </div> 
    );
    }
}

