import React, { cloneElement } from "react";
import PropTypes from "prop-types";
import PerspectivesComponent from "./perspectivescomponent.js";
import {PSContext} from "./reactcontexts.js";
import {PDRproxy} from "perspectives-proxy";

// LocalRoleSpecialisation requires a PSContext as context and a prop
// `ofrole` that names a role name that is an aspect role of a local role.
// It supplies that local role name (qualified) on the props of its children,
// as the prop `specialisedRole`.
export default class LocalRoleSpecialisation extends PerspectivesComponent
{
  constructor (props)
  {
    super( props );
    this.state = {specialisedRole: undefined};
  }

  componentDidMount()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        pproxy.getLocalRoleSpecialisation(
          component.props.ofrole,
          component.context.contextinstance,
          function(specialisedRoles)
          {
            if (specialisedRoles.length == 0)
            {
              throw("Screen programming error: There are no specialisations of role " +
                component.props.ofrole + " in context type " + component.context.contexttype);
            }
            else
            component.setState( {specialisedRole: specialisedRoles[0]});
          });
      }
    );
  }

  render()
  {
    const component = this;
    if (component.stateIsComplete())
    {
      return cloneElement(
        component.props.children,
        { specialisedRole: component.state.specialisedRole });
    }
    else
    {
      return null;
    }
  }
}

LocalRoleSpecialisation.propTypes =
  { ofrole: PropTypes.string.isRequired
  };

LocalRoleSpecialisation.contextType = PSContext;
