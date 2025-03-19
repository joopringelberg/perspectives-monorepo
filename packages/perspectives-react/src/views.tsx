import React from "react";
import {string} from "prop-types";

import View from "./view.js";
import ExternalRole from "./externalrole.js";

interface ViewOnExternalRoleProps {
  viewname: string;
  children?: React.ReactNode;
}

// Access a View on the BuitenRol of a Context.
export function ViewOnExternalRole(props : ViewOnExternalRoleProps)
{
  return (<ExternalRole>
      <View viewname={props.viewname}>{props.children}</View>
    </ExternalRole>);
}

ViewOnExternalRole.propTypes = {
  viewname: string.isRequired
};
