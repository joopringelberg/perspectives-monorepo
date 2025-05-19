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

import React, { Component, forwardRef, JSX } from "react";
import {Dropdown, NavDropdown} from 'react-bootstrap';
import i18next from "i18next";
import { ContextType, PDRproxy, Perspective, RoleInstanceT, RoleOnClipboard } from "perspectives-proxy";
import { UserMessagingPromise } from "./userMessaging";
import { PSContextType } from "./reactcontexts";
import { externalRole } from "./urifunctions";
import _ from "lodash";
import { addRoleToClipboard } from "./cardbehaviour";

interface TableItemContextMenuProps {
  perspective: Perspective;
  roleinstance: RoleInstanceT;
  roleOnClipboard?: RoleOnClipboard;
  systemExternalRole: RoleInstanceT;
}

interface TableItemContextMenuState {
  actions: string[];
  compatibleRole?: boolean;
}

export default class TableItemContextMenu extends Component<TableItemContextMenuProps, TableItemContextMenuState>
{
  ref: React.RefObject<HTMLTableRowElement| null>;
  constructor( props : TableItemContextMenuProps )
  {
    super(props);
    this.state = {actions: []};
    this.ref = React.createRef();
  }

  componentDidMount(): void {
    const component = this;
    if (component.props.roleOnClipboard !== undefined && !component.props.perspective.isCalculated)
    {
      PDRproxy.then( pproxy => pproxy.checkBindingP( component.props.perspective.roleType, component.props.roleOnClipboard!.roleData.rolinstance )
      .then( compatibleRole => 
        component.setState({compatibleRole})));
    }
  }

  componentDidUpdate(prevProps: Readonly<TableItemContextMenuProps>, prevState: Readonly<TableItemContextMenuState>, snapshot?: any): void {
    const component = this;
    if (!component.props.perspective.isCalculated && 
      component.props.roleOnClipboard &&
      (!_.isEqual(component.props.roleOnClipboard, prevProps.roleOnClipboard) || component.props.roleinstance !== prevProps.roleinstance))
    {
      PDRproxy.then( pproxy => pproxy.checkBindingP( component.props.perspective.roleType, component.props.roleOnClipboard!.roleData.rolinstance )
      .then( compatibleRole => 
        component.setState({compatibleRole})));
    }
  }

  computeActionItems(): JSX.Element[] 
  {
      // Computes the actions available based on context- and subject state, combined with those
    // available based on object state.
    const component = this;
    const props = this.props;
    const perspective = this.props.perspective;
    let objectStateActions = {};
    // It happens that the perspective is not always yet updated when we compute actions.
    // It also happens that the selectedroleinstance is not updated while the perspectives are?
    if (props.roleinstance && perspective.roleInstances[ props.roleinstance])
    {
      objectStateActions = perspective.roleInstances[ props.roleinstance ].actions;
    }
    const actionsMap = Object.assign( {}, perspective.actions, objectStateActions );

    const actions = Object.keys(actionsMap).map(
                          function(actionName)
                          {
                            return    <Dropdown.Item
                                        key={actionName}
                                        eventKey={actionName}
                                        onClick={ () => component.runAction( actionName )}
                                      >{
                                        actionsMap[actionName]
                                      }</Dropdown.Item>;
                          }
                        )
    if (actions.length > 0)
    {
      actions.push(<Dropdown.Divider key="BeforeActions"/>)
      return actions
    }
    else
    {
      return [];
    }
  }

  runAction( actionName : string)
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
          pproxy.action(
            component.props.roleinstance
            , component.props.perspective.contextInstance
            , component.props.perspective.id
            , actionName
            , component.props.perspective.userRoleType) // authoringRole
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("action_title", { ns: 'preact' }) 
              , message: i18next.t("action_message", {ns: 'preact', action: actionName})
              , error: e.toString()
              })));  
      });
  }


  // Yields one or two items:
  // - RemoveContext when the roleinstance is a contextrole and when the user is allowed to remove it.
  // - RemoveRole otherwise
  computeRemovalItems() : JSX.Element[] 
  {
    const component = this;
    const allowedtoremovecontext = () => component.props.perspective.verbs.includes("RemoveContext") || component.props.perspective.verbs.includes("DeleteContext")
    const allowedToRemoveRole = () => component.props.perspective.verbs.includes("Remove") || component.props.perspective.verbs.includes("Delete");
    const removalItems = [];
    if (allowedToRemoveRole())
    {
      removalItems.push( <Dropdown.Item
                            key="RemoveRole"
                            eventKey="RemoveRole"
                            onClick={ () => component.removeWithoutContext()}
                            className="text-danger"
                          >{
                            i18next.t("tableContextMenu_removerole", { ns: 'preact' }) 
                          }</Dropdown.Item>);
    }
    if (allowedtoremovecontext())
    {
      removalItems.push( <Dropdown.Item
                            key="RemoveContext"
                            eventKey="RemoveContext"
                            onClick={ () => component.removeWithContext()}
                            className="text-danger"
                          >{
                            i18next.t("tableContextMenu_removecontext", { ns: 'preact' })
                          }</Dropdown.Item>);
    }
    return removalItems;
  }

  removeWithContext() : Promise<void>
  {
    const component = this;
    return PDRproxy.then(
      function (pproxy)
      {
        pproxy
          .removeContext(
            component.props.roleinstance,
            component.props.perspective.roleType,
            component.props.perspective.userRoleType)
          .catch(e => UserMessagingPromise.then( um => 
            {
              um.addMessageForEndUser(
                { title: i18next.t("removeContext_title", { ns: 'preact' }) 
                , message: i18next.t("removeContext_message", {ns: 'preact' })
                , error: e.toString()
              });
            }));
      });
  }

  // This function is only called when there is a rolinstance value on the props.
  removeWithoutContext()
  {
    const component = this;
    return PDRproxy.then(
        function (pproxy)
        {
          pproxy
            .removeRole(
              component.props.perspective.roleType,
              component.props.roleinstance,
              component.props.perspective.userRoleType)
            .catch(e => UserMessagingPromise.then( um => 
              {
                um.addMessageForEndUser(
                  { title: i18next.t("removeRole_title", { ns: 'preact' }) 
                  , message: i18next.t("removeRole_message", {ns: 'preact' })
                  , error: e.toString()
                });
              }))
        });

  }

  computeFillItem(): JSX.Element[] 
  {
    const roleInstanceWithProps = this.props.perspective.roleInstances[this.props.roleinstance];
    if ((this.mayCreateContext() || this.mayCreateInstance()) && roleInstanceWithProps && !roleInstanceWithProps.filler && this.state.compatibleRole)
    {
      return [<Dropdown.Item
                key="Fill"
                eventKey="Fill"
                onClick={ () => this.fillRole()} 
              >{
                i18next.t("tableContextMenu_fill", { ns: 'preact' }) 
              }</Dropdown.Item>];
    }
    else
    {
      return [];
    }
  }
  // Notice that fillRole can only be called from the menu and the fill option is only 
  // available when the roleOnClipboard is compatible with the roleType of the perspective.
  fillRole()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        // (binder, binding, myroletype)
        pproxy.bind_(
          component.props.roleinstance,
          component.props.roleOnClipboard!.roleData.rolinstance,
          component.props.perspective.userRoleType);
      });
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

  computeAddItems() : JSX.Element[] 
  {
    const component = this;
    const contexts = component.props.perspective.contextTypesToCreate;
    const contextItems = 
      Object.keys(contexts).map( function(contextName)
            {
              return <Dropdown.Item
                      key={contextName}
                      eventKey={contextName}
                      onClick={ () => component.createRole( contextName as ContextType)}
                    >{
                      contexts[contextName]
                    }</Dropdown.Item>;
            })
    const justTheRole = <Dropdown.Item
                          key="JustTheRole"
                          eventKey="JustTheRole"
                          onClick={ () => component.createRole( "JustTheRole")}
                        >{
                          i18next.t("contextDropdown_title", { ns: 'preact' }) 
                        }</Dropdown.Item>
    if ( this.mayCreateContext() )
    {
      contextItems.unshift( justTheRole );
      contextItems.unshift(<Dropdown.Header key="NewItemsHeader">{ i18next.t( "tableContextMenu_new", {ns: 'preact'})}</Dropdown.Header>);
      contextItems.push(<Dropdown.Divider key="EndOfNewItems"/>);
      return contextItems;
    }
    else if ( this.mayCreateInstance() )
    {
      return  [ <Dropdown.Header key="NewItemsHeader">{ i18next.t( "tableContextMenu_new", {ns: 'preact'})}</Dropdown.Header>
              , justTheRole
              , <Dropdown.Divider key="EndOfNewItems" />];
    }
    else
    return [];
  }

  createRole ( contextToCreate? : ContextType | "JustTheRole")
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
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("createRole_title", { ns: 'preact' }) 
              , message: i18next.t("createRole_message", {ns: 'preact', roletype: roleType})
              , error: e.toString()
              })))
    }
    });
  }

  // If the role has a filler, we can open the context of the filler.
  // This item should not be shown when the role is a Contextrole or an ExternalRole.
  // Neither should it be shown when the role is a calculated role.
  computeOpenContextOfFillerItem() : JSX.Element[] 
  {
    const component = this;
    if (this.props.perspective.roleKind == "ContextRole" || this.props.perspective.roleKind == "ExternalRole" || this.props.perspective.isCalculated)
    {
      return [];
    }
    else if (this.props.perspective.roleInstances[this.props.roleinstance]?.filler)
    {
      return [<Dropdown.Item
                key="OpenContextOfFiller"
                eventKey="OpenContextOfFiller"
                onClick={ () => component.openContextOfFiller( component.props.perspective.roleInstances[component.props.roleinstance].filler!)}
              >{
                i18next.t("tableContextMenu_opencontextoffiller", { ns: 'preact' })
              }</Dropdown.Item>];
    }
    else
    {
      return [];
    }
  }

  openContextOfFiller( roleInstance : RoleInstanceT )
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        pproxy.getRolContext( roleInstance).then( context => component.ref.current?.dispatchEvent( new CustomEvent( 'OpenContext', { detail: externalRole( context ) , bubbles: true } )))
      });
  }
  
  computeTakeOnThisRoleItem() : JSX.Element[] 
  {
    const roleInstanceWithProps = this.props.perspective.roleInstances[this.props.roleinstance]; 
    if (roleInstanceWithProps?.isMe)
    {
      return [<Dropdown.Item
                key="TakeOnThisRole"
                eventKey="TakeOnThisRole"
                onClick={ () => this.takeOnThisRole()}
              >{
                i18next.t("tableContextMenu_takeonthisrole", { ns: 'preact' }) 
              }</Dropdown.Item>];
    }
    else 
    {
      return [];
    }
  }

  takeOnThisRole()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        pproxy.setPreferredUserRoleType(
          externalRole( component.props.perspective.contextInstance ),
          component.props.perspective.roleType);
      });
  }

  computeOpenDetailsItem() : JSX.Element[]
  {
    const component = this;
    return [<NavDropdown.Item
              key="OpenDetails"
              eventKey="OpenDetails"
              onClick={ () => {
                component.ref.current?.dispatchEvent( new CustomEvent( 'OpenWhereDetails',
                  { detail: 
                    { roleInstance: component.props.roleinstance
                    , roleType: component.props.perspective.roleType }
                  ,  bubbles: true } 
                ))
              }}
            >{
              i18next.t("tableContextMenu_opendetails", { ns: 'preact' }) 
            }</NavDropdown.Item>];
  }

  computeRestoreContextForUserItem() : JSX.Element[]
  {
    if (this.props.perspective.roleKind == "UserRole" || this.props.perspective.roleKind == "Public")
    {
      return [<Dropdown.Item
                key="RestoreContextForUser"
                eventKey="RestoreContextForUser"
                onClick={ () => this.restoreContextForUser()}
              >{
                i18next.t("tableContextMenu_restorecontextforuser", { ns: 'preact' })
              }</Dropdown.Item>];
    }
    else
    {
      return [];
    }
  }

  restoreContextForUser()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        pproxy.restoreContextForUser(
          component.props.perspective.contextInstance,
          component.props.roleinstance,
          component.props.perspective.roleType);
      })
      .catch(e => UserMessagingPromise.then( um =>
        um.addMessageForEndUser(
          { title: i18next.t("restoreContextForUser_title", { ns: 'preact' }) 
          , message: i18next.t("restoreContextForUser_message", {ns: 'preact'})
          , error: e.toString()
          })));
  }

  computePublicUrl() : JSX.Element[]
  {
    const roleInstanceWithProps = this.props.perspective.roleInstances[this.props.roleinstance];
    if (roleInstanceWithProps && roleInstanceWithProps.publicUrl)
    {
      const url = window.location.origin + "?opencontext=" + encodeURIComponent( roleInstanceWithProps.publicUrl );
      return [<Dropdown.Item
        key="PublicUrl"
        eventKey="PublicUrl"
        onClick={() => {
          if (roleInstanceWithProps.publicUrl) {
            navigator.clipboard.writeText(url)
              .then(() => {
                UserMessagingPromise.then(um =>
                  um.addMessageForEndUser({
                    title: i18next.t("copyPublicUrl_title", { ns: 'preact' }),
                    message: i18next.t("copyPublicUrl_message", { ns: 'preact' }),
                  })
                );
              })
              .catch(e => {
                UserMessagingPromise.then(um =>
                  um.addMessageForEndUser({
                    title: i18next.t("copyPublicUrl_error_title", { ns: 'preact' }),
                    message: i18next.t("copyPublicUrl_error_message", { ns: 'preact' }),
                    error: e.toString(),
                  })
                );
              });
          }
        }}
      >
        {i18next.t("tableContextMenu_publicurl", { ns: 'preact' })}
      </Dropdown.Item>];
    }
    else
    {
      return [];
    }
  }

  computeCopyItem() : JSX.Element[]
  {
    const component = this;
    const roleInstanceWithProps = this.props.perspective.roleInstances[this.props.roleinstance];
    if (roleInstanceWithProps)
    {
      return [<Dropdown.Item
                key="Copy"
                eventKey="Copy"
                onClick={ () => component.copy()}
              >{
                i18next.t("tableContextMenu_copy", { ns: 'preact' })
              }</Dropdown.Item>];
    }
    else
    {
      return [];
    }
  }

  copy() {
    const component = this;
    const roleInstanceWithProps = this.props.perspective.roleInstances[this.props.roleinstance];
    navigator.clipboard.writeText(this.props.roleinstance!);
    addRoleToClipboard(
      component.props.roleinstance!, 
      {
        roleData: {
          rolinstance: component.props.roleinstance!,
          cardTitle: roleInstanceWithProps.readableName || "No title",
          roleType: component.props.perspective.roleType,
          contextType: component.props.perspective.contextType,
        },
        addedBehaviour: ["fillARole"],
        myroletype: component.props.perspective.userRoleType,
      },
      component.props.systemExternalRole,
      component.props.perspective.userRoleType
      ).catch((e) =>
          UserMessagingPromise.then((um) =>
            um.addMessageForEndUser({
              title: i18next.t("clipboardSet_title", { ns: "preact" }),
              message: i18next.t("clipboardSet_message", { ns: "preact" }),
              error: e.toString(),
            })
          )
        );
}


  render()
  {
    const component = this;
    const items = [
      ...this.computeOpenDetailsItem(),
      ...this.computeCopyItem(),
      ...this.computeFillItem(),
      ...this.computeAddItems(),
      ...this.computeRemovalItems(),
      ...this.computeOpenContextOfFillerItem(),
      ...this.computeActionItems(),
      ...this.computeTakeOnThisRoleItem(),
      ...this.computeRestoreContextForUserItem(),
      ...this.computePublicUrl()
    ];
    {
      return <NavDropdown 
              ref={component.ref}
              title={<i className="bi bi-three-dots-vertical"></i>} 
              className="hide-caret"
              onClick={e => e.stopPropagation()}>
              {items}
            </NavDropdown>

    }
    }
}

// eslint-disable-next-line react/display-name, react/prop-types
interface CustomToggleProps {
  children: React.ReactNode;
  onClick: (e: React.MouseEvent<HTMLAnchorElement, MouseEvent>) => void;
  disabled: boolean;
}

const CustomToggle = forwardRef<HTMLAnchorElement, CustomToggleProps>(({ children, onClick, disabled }, ref) => (
  <a
    href=""
    ref={ref}
    className={disabled ? "disabledIcon" : "iconStyle"}
    onClick={(e) => {
      e.preventDefault();
      e.stopPropagation()
      if (!disabled)
      {
        onClick(e);
      }
    }}
  >
    {children}
    &#x25bc;
  </a>
));
