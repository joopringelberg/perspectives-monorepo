import React, { Children } from "react";
import PerspectivesComponent from "./perspectivescomponent.js";
import {PSRoleInstances} from "./reactcontexts.js";

// When offered two children, renders the first if its PSRoleInstances context
// has no instances. If it has instances, renders the second.
// If only one child is available, just renders that.
export default class NoInstancesSwitcher extends PerspectivesComponent
{
  render ()
  {
    const component = this;
    let defaultElement, children;
    if (Children.count( component.props.children ) == 1)
    {
      children = component.props.children;
    }
    else
    {
      children = Children.toArray( component.props.children );
      defaultElement = children[0];
      children = children.slice(1);
    }
    // By using the previous instances, we make sure React does not update the children.
    if (component.context.instances.length == 0 )
    {
      if (defaultElement)
      {
        return defaultElement;
      }
      else
      {
        return null;
      }
    }
    else
    {
      return children;
    }
    }
  }

NoInstancesSwitcher.contextType = PSRoleInstances;

NoInstancesSwitcher.propTypes = {};
