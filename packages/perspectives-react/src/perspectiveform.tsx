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


import {PDRproxy, CONTINUOUS, RoleInstanceT, ContextInstanceT, RoleType} from "perspectives-proxy";
import PerspectivesComponent from "./perspectivesComponent";
import PerspectiveBasedForm from "./perspectivebasedform.js";
import { mapRoleVerbsToBehaviourNames } from "./maproleverbstobehaviours.js";

////////////////////////////////////////////////////////////////////////////////
// PERSPECTIVEFORM
// This component retrieves a perspective from the PDR and bases a form on it.
// If just a role instance is provided, the form edits it in its original context
// (to which it is attached as an Enumerated role)
// and with the first available role type for the user in that context.
// If a context instance is provided, the user role type is computed from it.
// If a user role type is provided, that is used. It is required to be a role type
// that is actually available in the given or derived context!
////////////////////////////////////////////////////////////////////////////////

interface PerspectiveFormProps
{
  roleinstance: RoleInstanceT;
  contextinstance?: ContextInstanceT;
}

export default class PerspectiveForm extends PerspectivesComponent<PerspectiveFormProps>
{
  constructor(props : PerspectiveFormProps)
  {
    super(props);
    this.state = {perspective: undefined};
  }

  componentDidMount()
  {
    const component = this;
    PDRproxy.then(function(pproxy)
      {
        component.addUnsubscriber(
          // getPerspective (roleInstanceOfContext, perspectiveObjectRoleType /*OPTIONAL*/, receiveValues, fireAndForget, errorHandler)
          pproxy.getPerspective(
            component.props.roleinstance
            , undefined
            ,function( perspective )
            {
              console.log(perspective);
              if (perspective[0])
              {
                if ( Object.keys(perspective[0].roleInstances).length == 0 )
                {
                  history.back();
                }
                else
                {
                  component.setState({perspective: perspective[0]});
                }
              }
            }
            ,CONTINUOUS
          ));
      });
  }

  render ()
  {
    const component = this;
    const perspective = component.state.perspective;
    if (component.stateIsComplete())
    {
      return  <PerspectiveBasedForm
                perspective={perspective}
                roleinstance={component.props.roleinstance}
                behaviours={mapRoleVerbsToBehaviourNames( perspective )}
                cardtitle={ perspective.identifyingProperty }
                showControls={true}
              />;
    }
    else
    {
      return null;
    }
  }
}
