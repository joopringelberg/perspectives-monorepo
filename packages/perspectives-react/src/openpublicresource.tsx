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

import { createRef } from 'react';
import { string } from "prop-types";

import PerspectivesComponent from "./perspectivesComponent";
import { thisAppsLocation } from './utilities';
import {LinkExternalIcon} from '@primer/octicons-react';
import i18next from "i18next";

import {OverlayTrigger, Tooltip} from "react-bootstrap";
import { RoleKind } from 'perspectives-proxy';
import { OverlayInjectedProps } from 'react-bootstrap/esm/Overlay';

interface OpenPublicResourceProps {
  publicurl: string;
  rolekind: RoleKind;
}

interface OpenEvent {
  stopPropagation: () => void;
  preventDefault: () => void;
}

export default class OpenPublicResource extends PerspectivesComponent<OpenPublicResourceProps, {}>
{
  constructor( props : OpenPublicResourceProps )
  {
    super(props);
  }

  handleKeyDown(e : React.KeyboardEvent<HTMLDivElement>)
  {
    const component = this;
    switch (e.code){
      case "Enter": // Return
      case "Space": // space
        component.openPublicResource({stopPropagation: e.stopPropagation, preventDefault: e.preventDefault});
        break;
    }
  }

  openPublicResource(e : OpenEvent)
  {
    const component = this;
    e.stopPropagation();
    e.preventDefault();
    if (component.props.rolekind == "ContextRole" || component.props.rolekind == "ExternalRole")
    {
      window.open(thisAppsLocation() + "?opencontext=" + encodeURIComponent( component.props.publicurl) );
    }
    else
    {
      // TODO. RoleForm openen werkt niet meer.
      window.open(thisAppsLocation() + "?openroleform=" + component.props.publicurl
        // This information is not (yet) available.
        // + "&viewname=" + viewname +
        // (component.props.cardprop ? "&cardprop=" + component.props.cardprop : "")
        );
    }
  }

  render()
  {
    const component = this;
    const renderTooltip = (props : OverlayInjectedProps) => (
    <Tooltip id="openPublicResource-tooltip" {...props}>
      { i18next.t("openPublicResource", {ns: "preact"}) }
    </Tooltip> );

    const eventDiv = createRef() as React.RefObject<HTMLDivElement>;

    return  <OverlayTrigger
                      placement="left"
                      delay={{ show: 250, hide: 400 }}
                      overlay={renderTooltip}
                    >
                    <div
                        ref={eventDiv}
                        className="ml-3 mr-3"
                        aria-describedby="openPublicResource-tooltip"
                        tabIndex={0}
                        onKeyDown={ e => component.handleKeyDown(e) }
                        onClick={ e => component.openPublicResource(e)}
                    >
                    <LinkExternalIcon
                      aria-label={ i18next.t("openPublicResource", {ns: "preact"}) }
                      className="iconStyle"
                      size="medium"
                    />
                    </div>
              </OverlayTrigger>;
  }
}

