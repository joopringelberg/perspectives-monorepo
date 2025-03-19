import React from "react";
import PropTypes from "prop-types";

import RoleInstanceIterator from "./roleinstanceiterator.js";
import RoleInstances from "./roleinstances.js";
import RoleDropZone from "./roleDropzone.js";
import NoInstancesSwitcher from "./noinstancesswitcher";
import {PSRoleInstances} from "./reactcontexts.js";

export default function Rol(props)
{
  const ariaLabel = props.ariaLabel ? props.ariaLabel : "Drop a role here";
  if (props.allowExtension)
  {
    return  <RoleInstances rol={props.rol}>
              <PSRoleInstances.Consumer>{ psroleinstances =>
                <RoleDropZone
                  ariaLabel={ariaLabel}
                  bind={psroleinstances.bind}
                  checkbinding={psroleinstances.checkbinding}
                >
                  <NoInstancesSwitcher>
                    <p>{ariaLabel}</p>
                    <RoleInstanceIterator>
                      {/*eslint-disable-next-line react/prop-types*/}
                      {props.children}
                    </RoleInstanceIterator>
                  </NoInstancesSwitcher>
                </RoleDropZone>
            }</PSRoleInstances.Consumer>
            </RoleInstances>;
  }
  else
  {
    return (<RoleInstances rol={props.rol}>
            <RoleInstanceIterator>
              {/*eslint-disable-next-line react/prop-types*/}
              {props.children}
            </RoleInstanceIterator>
      </RoleInstances>);
  }
}

Rol.propTypes =
  {
    // May be a local name that will be resolved to the embedding context given by
    // PSContext.
    // Can also be a qualified name.
    rol: PropTypes.string.isRequired
  , ariaLabel: PropTypes.string
  };
