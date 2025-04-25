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

import React, { useState, useEffect, useRef } from "react";
import "./App.css";


import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Tooltip from 'react-bootstrap/Tooltip';
import { PDRproxy, RoleInstanceT } from "perspectives-proxy";
import { i18next, ModelDependencies } from "perspectives-react";
import { OverlayInjectedProps } from 'react-bootstrap/esm/Overlay';

interface ConnectedToAMQPProps {
  roleinstance: RoleInstanceT
}

export default function ConnectedToAMQP(props : ConnectedToAMQPProps) 
{
  const [isConnected, setIsConnected] = useState<boolean>(false);
  const eventDiv = useRef(null);

  // useEffect hook to handle the asynchronous call
  useEffect(() => {
    let unsubscriber: any;
    
    PDRproxy.then(pproxy => 
      pproxy.getProperty(
        props.roleinstance,
        ModelDependencies.connectedToAMQPBroker, 
        ModelDependencies.systemExternal,
        (values) => {
          // values is an array, with "true" or "false" as strings
          setIsConnected(values[0] === "true");
        }
      ).then(unsub => {
        unsubscriber = unsub;
      })
    );

    // Cleanup function that runs when the component unmounts
    return () => {
      if (unsubscriber) {
        PDRproxy.then(pproxy => pproxy.send(unsubscriber, () => {}));
      }
    };
  }, [props.roleinstance]); // Re-run effect if roleinstance changes

  const renderTooltip = (props : OverlayInjectedProps) => (
    <Tooltip id="amqp-tooltip" {...props} show={
      props.show}>
      {i18next.t("app_connected_to_amqp_tooltip", { ns: 'mycontexts', connected: isConnected })}
    </Tooltip>
  );

  return isConnected ? 
    <OverlayTrigger
      placement="left"
      delay={{ show: 250, hide: 400 }}
      overlay={renderTooltip}
    >
      <div
        ref={eventDiv}
        className="ml-3 mr-3 text-secondary"
        aria-describedby="amqp-tooltip"
        tabIndex={0}
      >{
        isConnected ? <span role="img" aria-label="Online" style={{ color: "green", marginRight: "8px" }}>🟢</span> :
        <span role="img" aria-label="Offline" style={{ color: "red", marginRight: "8px" }}>🔴</span>
      }
        
      </div>
    </OverlayTrigger>
    : 
    <div/>;
}
