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

import React, { createRef } from 'react';

import {ContextInstanceT, ContextType, PDRproxy, RoleInstanceT, RoleOnClipboard, RoleType} from "perspectives-proxy";
import PerspectivesComponent from "./perspectivesComponent";
import {PSRoleInstances} from "./reactcontexts.js";
import { default as ModelDependencies } from "./modelDependencies.js";
import {PasteIcon} from '@primer/octicons-react';
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

import {OverlayTrigger, Tooltip} from "react-bootstrap";
import { OverlayInjectedProps } from 'react-bootstrap/esm/Overlay';
import "./components.css";

interface TablePasteRoleProps
{
  systemexternalrole: RoleInstanceT;
  contextinstance: ContextInstanceT;
  contexttype: ContextType;
  roletype: RoleType;
  selectedroleinstance?: RoleInstanceT;
  myroletype: RoleType;
}

interface TablePasteRoleState
{
  compatibleRole: boolean;
  roleOnClipboard?: RoleInstanceT;
  showRemoveContextModal: boolean;
}

export default class TablePasteRole extends PerspectivesComponent<TablePasteRoleProps, TablePasteRoleState>
{
  constructor( props : TablePasteRoleProps )
  {
    super(props);
    this.state =
      { compatibleRole: false
      , roleOnClipboard: undefined
      , showRemoveContextModal: false
    };
  }

  componentDidMount()
  {
    const component = this;
    let clipboardContent : RoleOnClipboard;
    PDRproxy.then(
      function(pproxy)
      {
        component.addUnsubscriber(
          pproxy.subscribeSelectedRoleFromClipboard(
            function (clipboardContents : RoleOnClipboard[])
            {
              const clipboardContent = clipboardContents[0];
              if ( clipboardContent && clipboardContent.roleData && clipboardContent.roleData.rolinstance && clipboardContent.roleData.rolinstance != component.state.roleOnClipboard  )
                {
                  // checkBinding( <(QUALIFIED)RolName>, <binding>, [() -> undefined] )
                  PDRproxy.then( pproxy => pproxy.checkBindingP( component.props.roletype, clipboardContent.roleData.rolinstance )
                    .then( compatibleRole => component.setState({compatibleRole, roleOnClipboard: clipboardContent.roleData.rolinstance})));
                }
              }
            ));
      });
  }

  handleKeyDown(e : React.KeyboardEvent)
  {
    const component = this;
    switch (e.code){
      case "Enter": // Return
      case "Space": // space
        component.pasteRole();
        e.stopPropagation();
        break;
    }
  }

  pasteRole()
  {
    const component = this;
    const {roleOnClipboard, compatibleRole} = component.state;
    if ( roleOnClipboard && compatibleRole)
    {
      if (component.props.selectedroleinstance)
      {
        PDRproxy.then(
          function (pproxy)
          {
            // (binder, binding, myroletype)
            pproxy.bind_(
              component.props.selectedroleinstance!,
              roleOnClipboard,
              component.props.myroletype);
          });
        }
      else
      {
        // component.context.bind( {rolinstance: roleOnClipboard} );
        PDRproxy.then(
          function (pproxy)
          {
            pproxy
              .bind(
                component.props.contextinstance,
                component.props.roletype, // may be a local name.
                component.props.contexttype,
                {properties: {}, binding: roleOnClipboard},
                component.props.myroletype)
              .catch(e => UserMessagingPromise.then( um => 
                {
                  um.addMessageForEndUser(
                    { title: i18next.t("fillRole_title", { ns: 'preact' }) 
                    , message: i18next.t("fillRole_message", {ns: 'preact' })
                    , error: e.toString()
                  });
                  component.setState({showRemoveContextModal: false})
                }));
            });
      }
    }
  }

  render()
  {
    const component = this;
    const renderTooltip = (props : OverlayInjectedProps) => (
    <Tooltip id="tablePasteRole-tooltip" {...props} show={
       // eslint-disable-next-line
      props.show}>
      { ( component.state.compatibleRole
          ? i18next.t("tablePasteRole_Create", {ns: "preact"})
          : i18next.t("tablePasteRole_Incompatible", {ns: "preact"})
        )
      }
    </Tooltip> );

    const eventDiv = createRef() as React.RefObject<HTMLDivElement>;

    if ( component.stateIsComplete(["roleOnClipboard"]) )
    {
      return  <OverlayTrigger
                      placement="left"
                      delay={{ show: 250, hide: 400 }}
                      overlay={renderTooltip}
                    >
                    <div
                        ref={eventDiv}
                        className="ml-3 mr-3"
                        aria-describedby="tablePasteRole-tooltip"
                        tabIndex={0}
                        onKeyDown={ e => component.handleKeyDown(e) }
                        onClick={ () => component.pasteRole()}
                    >
                    <PasteIcon
                      aria-label={ i18next.t("tablePasteRole_Alt", {ns: "preact"}) }
                      className={component.state.compatibleRole ? "iconStyle" : "disabledIcon"}
                      size="medium"
                    />
                    </div>
              </OverlayTrigger>;
    }
    else {
      return null;
    }
  }
}

TablePasteRole.contextType = PSRoleInstances;

