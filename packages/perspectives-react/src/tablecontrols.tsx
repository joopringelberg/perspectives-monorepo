// BEGIN LICENSE
// Perspectives Distributed Runtime
// Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Full text of this license can be found in the LICENSE file in the projects root.
// END LICENSE

import React from "react"; // 2
import PerspectivesComponent from "./perspectivesComponent";
import {ContextType, PDRproxy, Perspective, RoleInstanceT} from "perspectives-proxy";
import {AppContext, PSContextType} from "./reactcontexts.js";
import ActionDropDown from "./actiondropdown.js";
import CreateContextDropDown from "./createContextDropdown.js";
import {PSContext} from "./reactcontexts.js";
import TablePasteRole from "./tablepasterole.js";
import OpenPublicResource from "./openpublicresource.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

import
  { Navbar
  } from "react-bootstrap";
import "././styles/components.css";

////////////////////////////////////////////////////////////////////////////////
// TABLECONTROLS
////////////////////////////////////////////////////////////////////////////////
interface TableControlsProps
{
  perspective: Perspective;
  selectedroleinstance: RoleInstanceT;
}

export default class TableControls extends PerspectivesComponent<TableControlsProps>
{
  declare context: PSContextType;
  static contextType = PSContext;
  
  constructor( props : TableControlsProps)
  {
    super( props);
    this.runAction = this.runAction.bind(this);
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

  handleKeyDown (event : React.KeyboardEvent)
    {
      const component = this;
        switch(event.keyCode){
          case 13: // Return
          case 32: // Space
            component.createRole( function() {});
            event.preventDefault();
            event.stopPropagation();
            break;
        }
  }

  runAction( actionName : string)
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
          pproxy.action(
            component.props.selectedroleinstance
            , component.props.perspective.contextInstance
            , component.props.perspective.id
            , actionName
            , component.props.perspective.userRoleType) // authoringRole
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("action_title", { ns: 'preact' }) 
              , message: i18next.t("action_message", {ns: 'preact', action: component.state.actions[actionName]})
              , error: e.toString()
              })));  
      });
  }

  // Computes the actions available based on context- and subject state, combined with those
  // available based on object state.
  computeActions()
  {
    const props = this.props;
    const perspective = this.props.perspective;
    let objectStateActions = {};
    // It happens that the perspective is not always yet updated when we compute actions.
    // It also happens that the selectedroleinstance is not updated while the perspectives are?
    if (props.selectedroleinstance && perspective.roleInstances[ props.selectedroleinstance])
    {
      objectStateActions = perspective.roleInstances[ props.selectedroleinstance ].actions;
    }
    return Object.assign( {}, perspective.actions, objectStateActions );
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

  render ()
  {
    const component = this;
    const actions = component.computeActions();
    const instances = component.props.perspective.roleInstances;
    const instance = instances[component.props.selectedroleinstance];
    const mayCreateContext = component.mayCreateContext()
    const mayCreateRoleInstance = component.mayCreateInstance();
    if  ( mayCreateContext || 
          mayCreateRoleInstance ||
          Object.keys( actions ).length > 0 && Object.keys(instances).length > 0 ||
          instance && instance.publicUrl
        )
    {
      return  <Navbar bg="light" expand="lg" role="banner" aria-label="Controls for table">
                {
                  mayCreateContext ?
                  <CreateContextDropDown 
                    contexts={component.props.perspective.contextTypesToCreate}
                    create={ contextToCreate => component.createRole( function() {}, contextToCreate)}
                  />
                  : mayCreateRoleInstance ?
                  <CreateContextDropDown 
                    contexts={{}}
                    create={ () => component.createRole( function() {}, "JustTheRole")}
                  />
                  : null
                }
                {
                  (mayCreateContext || mayCreateRoleInstance) && instance && !instance.filler ?
                  <AppContext.Consumer>
                  { appcontext => <TablePasteRole 
                    systemexternalrole={appcontext.systemExternalRole}
                    contextinstance={component.props.perspective.contextInstance}
                    contexttype={component.props.perspective.contextType}
                    roletype={component.props.perspective.roleType}
                    selectedroleinstance={component.props.selectedroleinstance}
                    myroletype={component.props.perspective.userRoleType}
                    /> 
                  }
                </AppContext.Consumer>
                  : null
                }
                { Object.keys( actions ).length > 0 && Object.keys(instances).length > 0 ?
                  <ActionDropDown
                    actions={ actions }
                    runaction={ eventKey => component.runAction( eventKey as string )}
                  />
                  : null }
                {
                  instance && instance.publicUrl ?
                  <OpenPublicResource publicurl={ instance.publicUrl } rolekind={ component.props.perspective.roleKind }/>
                  :
                  null
                }
              </Navbar>;
    }
    else
    {
      return null;
    }
  }
}
