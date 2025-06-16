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

import { useState, useEffect, useRef } from "react";

import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Tooltip from 'react-bootstrap/Tooltip';
import { PDRproxy, RoleInstanceT } from "perspectives-proxy";
import { i18next, ModelDependencies, PerspectivesComponent } from "perspectives-react";
import { OverlayInjectedProps } from 'react-bootstrap/esm/Overlay';

interface ConnectedToAMQPProps {
  roleinstance: RoleInstanceT
  isOnline: boolean
}

interface ConnectedToAMQPState {
  isConnected: boolean
}

export default class ConnectedToAMQP extends PerspectivesComponent<ConnectedToAMQPProps>{
  private eventDiv!: React.RefObject<HTMLDivElement|null>;

  constructor(props: ConnectedToAMQPProps) {
    super(props);
    this.state = {
      isConnected: false
    };
  }

  componentDidMount(): void {
    const component = this;
    this.eventDiv = React.createRef();
    this.subscribeToConnectionProperty();
  }

  componentDidUpdate(prevProps: Readonly<ConnectedToAMQPProps>, prevState: Readonly<ConnectedToAMQPState>, snapshot?: any): void {
    if (prevProps.roleinstance !== this.props.roleinstance) {
      this.unsubscribeAll();
      this.subscribeToConnectionProperty();
    }
  }

  subscribeToConnectionProperty()
  {
    const component = this;
    PDRproxy.then(pproxy => 
      component.addUnsubscriber(
        pproxy.getProperty(
          component.props.roleinstance,
          ModelDependencies.connectedToAMQPBroker, 
          ModelDependencies.systemExternal,
          (values) => {
            // values is an array, with "true" or "false" as strings
            component.setState({ isConnected: values[0] === "true" });
          }
        )));

  }

  render() {
    const component = this;
    const renderTooltip = (props : OverlayInjectedProps) => (
      <Tooltip id="amqp-tooltip" {...props} show={
        props.show}>
        {i18next.t("app_connected_to_amqp_tooltip", 
          { ns: 'mycontexts'
          , connectionState: component.state.isConnected  && component.props.isOnline ? 
            "" :
            i18next.t("app_connected_to_internet_connected", { ns: 'mycontexts' })
          })
          }
      </Tooltip>
    );
  
    return <OverlayTrigger
    placement="left"
    delay={{ show: 250, hide: 400 }}
    overlay={renderTooltip}
  >
    <div
      ref={component.eventDiv}
      className="ml-3 mr-3 text-secondary"
      aria-describedby="amqp-tooltip"
      tabIndex={0}
    >{
      component.state.isConnected  && component.props.isOnline ? <span role="img" aria-label="Online" style={{ color: "green", marginRight: "8px" }}>🟢</span> :
      <span role="img" aria-label="Offline" style={{ color: "red", marginRight: "8px" }}>🔴</span>
    }
      
    </div>
  </OverlayTrigger>
;
  }
}
