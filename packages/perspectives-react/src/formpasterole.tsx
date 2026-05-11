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

import {FillMode, PDRproxy, RoleInstanceT, RoleOnClipboard} from "perspectives-proxy";
import PerspectivesComponent from "./perspectivesComponent";
import {PSRol, PSRolType} from "./reactcontexts.js";
import i18next from "i18next";

import {Button, OverlayTrigger, Tooltip} from "react-bootstrap";

import "././styles/components.css";

interface FormPasteRoleProps {
  systemexternalrole: RoleInstanceT;
}
interface FormPasteRoleState {
  providedTypeCompatible: boolean;
  requiredTypeCompatible: boolean;
  providedTypeName?: string;
  requiredTypeName?: string;
  roleOnClipboard: RoleInstanceT | undefined;
  roleOnClipboardName?: string;
}

export default class FormPasteRole extends PerspectivesComponent<FormPasteRoleProps, FormPasteRoleState>
{
  declare context: PSRolType;
  static contextType = PSRol;
  constructor( props : FormPasteRoleProps )
  {
    super(props);
    this.state =
      { providedTypeCompatible: false
      , requiredTypeCompatible: false
      , roleOnClipboard: undefined};
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
                      pproxy.checkBindingP(component.context.roltype, clipboardContent.roleData.rolinstance),
                      pproxy.getRoleNameP(clipboardContent.roleData.rolinstance),
                      pproxy.getMostGeneralAllowedBindingType(component.context.roltype, clipboardContent.roleData.rolinstance)
                    ]
                  )
                    .then(([providedTypeCompatible, providedTypeName, requiredType]) =>
                      component.setState(
                        { providedTypeCompatible
                        , requiredTypeCompatible: requiredType !== undefined
                        , providedTypeName
                        , requiredTypeName: requiredType?.readableName
                        , roleOnClipboard: clipboardContent.roleData.rolinstance
                        , roleOnClipboardName: clipboardContent.roleData.cardTitle
                        }))
                    .catch(() =>
                      component.setState(
                        { providedTypeCompatible: false
                        , requiredTypeCompatible: false
                        , providedTypeName: undefined
                        , requiredTypeName: undefined
                        , roleOnClipboard: clipboardContent.roleData.rolinstance
                        , roleOnClipboardName: clipboardContent.roleData.cardTitle
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
    if ( roleOnClipboard )
    {
      // No need to call checkBinding; it was done on mounting.
      if (compatibleRole && component.context.rolinstance )
      {
        component.context.bind_({rolinstance: roleOnClipboard}, fillMode);
      }
      else if ( compatibleRole )
      {
        component.context.bind(roleOnClipboard, fillMode);
      }
    }
  }

  render()
  {
    const component = this;
    if ( component.stateIsComplete(["roleOnClipboard"]) )
    {
      const providedTooltip = (
        <Tooltip id="formPasteRole-provided-tooltip">
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
        <Tooltip id="formPasteRole-required-tooltip">
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
