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

import React from 'react';

import {ContextInstanceT, ContextType, FillMode, PDRproxy, RoleInstanceT, RoleOnClipboard, RoleType} from "perspectives-proxy";
import PerspectivesComponent from "./perspectivesComponent";
import {PSRoleInstances} from "./reactcontexts.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

import {Button, OverlayTrigger, Tooltip} from "react-bootstrap";
import "././styles/components.css";

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
  providedTypeCompatible: boolean;
  requiredTypeCompatible: boolean;
  providedTypeName?: string;
  requiredTypeName?: string;
  roleOnClipboardName?: string;
  roleOnClipboard?: RoleInstanceT;
  showRemoveContextModal: boolean;
}

export default class TablePasteRole extends PerspectivesComponent<TablePasteRoleProps, TablePasteRoleState>
{
  constructor( props : TablePasteRoleProps )
  {
    super(props);
    this.state =
      { providedTypeCompatible: false
      , requiredTypeCompatible: false
      , roleOnClipboard: undefined
      , showRemoveContextModal: false
    };
  }

  componentDidMount()
  {
    const component = this;
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
                  Promise.all(
                    [
                      pproxy.checkBindingP(component.props.roletype, clipboardContent.roleData.rolinstance),
                      pproxy.getRoleNameP(clipboardContent.roleData.rolinstance),
                      pproxy.getMostGeneralAllowedBindingType(component.props.roletype, clipboardContent.roleData.rolinstance)
                    ]
                  )
                    .then(([providedTypeCompatible, providedTypeName, requiredType]) =>
                      component.setState(
                        { providedTypeCompatible
                        , requiredTypeCompatible: requiredType !== undefined
                        , providedTypeName
                        , requiredTypeName: requiredType?.readableName
                        , roleOnClipboardName: clipboardContent.roleData.cardTitle
                        , roleOnClipboard: clipboardContent.roleData.rolinstance
                        }))
                    .catch(() =>
                      component.setState(
                        { providedTypeCompatible: false
                        , requiredTypeCompatible: false
                        , providedTypeName: undefined
                        , requiredTypeName: undefined
                        , roleOnClipboardName: clipboardContent.roleData.cardTitle
                        , roleOnClipboard: clipboardContent.roleData.rolinstance
                        }));
                }
              }
            ));
      });
  }

  pasteRole(fillMode: FillMode)
  {
    const component = this;
    const {roleOnClipboard, providedTypeCompatible, requiredTypeCompatible} = component.state;
    const compatibleRole = fillMode === "provided" ? providedTypeCompatible : requiredTypeCompatible;
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
              component.props.myroletype,
              fillMode);
          });
        }
      else
      {
        PDRproxy.then(
          function (pproxy)
          {
            pproxy.createRole(component.props.contextinstance, component.props.roletype, component.props.myroletype)
              .then(newRole => pproxy.bind_(newRole, roleOnClipboard, component.props.myroletype, fillMode))
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
    if ( component.stateIsComplete(["roleOnClipboard"]) )
    {
      const providedTooltip = (
        <Tooltip id="tablePasteRole-provided-tooltip">
          {
            i18next.t(
              "tableContextMenu_fillWithProvidedType",
              { ns: "preact"
              , roleName: component.state.roleOnClipboardName
              , typeName: component.state.providedTypeName
              })
          }
        </Tooltip>
      );
      const requiredTooltip = (
        <Tooltip id="tablePasteRole-required-tooltip">
          {
            i18next.t(
              "tableContextMenu_fillWithRequiredType",
              { ns: "preact"
              , roleName: component.state.roleOnClipboardName
              , typeName: component.state.requiredTypeName
              })
          }
        </Tooltip>
      );

      return <div className="d-flex gap-2 align-items-center">
        <OverlayTrigger placement="left" delay={{ show: 250, hide: 400 }} overlay={providedTooltip}>
          <Button
            size="sm"
            variant="outline-secondary"
            disabled={!component.state.providedTypeCompatible}
            onClick={ () => component.pasteRole("provided")}
          >
            {i18next.t("pasteRole_ProvidedShort", {ns: "preact"})}
          </Button>
        </OverlayTrigger>
        <OverlayTrigger placement="left" delay={{ show: 250, hide: 400 }} overlay={requiredTooltip}>
          <Button
            size="sm"
            variant="outline-secondary"
            disabled={!component.state.requiredTypeCompatible}
            onClick={ () => component.pasteRole("required")}
          >
            {i18next.t("pasteRole_RequiredShort", {ns: "preact"})}
          </Button>
        </OverlayTrigger>
      </div>;
    }
    else {
      return null;
    }
  }
}

TablePasteRole.contextType = PSRoleInstances;
