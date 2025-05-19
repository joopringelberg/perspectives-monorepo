import React from "react";
import {PDRproxy, RoleDataProper, RoleInstanceT, RoleKind} from "perspectives-proxy";

import PerspectivesComponent from "./perspectivesComponent";
import {externalRole} from "./urifunctions.js";
import {PSRol, PSRolType, PSContextType} from "./reactcontexts.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

interface ExternalRoleProps
{
  children: React.ReactNode;
}

interface ExternalRoleState
{
  value?: PSRolType;
} 

export default class ExternalRole extends PerspectivesComponent<ExternalRoleProps, ExternalRoleState>
{
  declare context: PSContextType;
  static contextType = PSRol;

  constructor (props : ExternalRoleProps)
  {
    super(props);
    this.state = { value: undefined };
  }
  componentDidMount()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        pproxy.getRolType(externalRole( component.context.contextinstance! ))
          .then(
            function(rolType)
            {
              const updater = {value:
                { contextinstance: component.context.contextinstance
                , contexttype: component.context.contexttype
                , roltype: rolType
                , roleKind: "ExternalRole" as RoleKind
                , rolinstance: externalRole( component.context.contextinstance)
                // There is no effective implementation of any of these functions for external roles.
                , bind_: (ignore:RoleDataProper) => Promise.resolve(false)
                , bind: (ignore:RoleInstanceT) => Promise.resolve("" as RoleInstanceT)
                , checkbinding: (roleData : RoleDataProper) => Promise.resolve(true)
                , removerol: () => Promise.resolve()
                , removecontext: () => Promise.resolve()
                , isselected: false
                }};
              component.setState( updater );
            })
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("role_title", { ns: 'preact' }) 
              , message: i18next.t("role_message", {ns: 'preact', role: externalRole( component.context.contextinstance )})
              , error: e.toString()
              })));
      });
  }

  render ()
  {
    const component = this;
    if (component.state.value !== undefined)
    {
      return (<PSRol.Provider value={component.state.value!}>
        {component.props.children}
        </PSRol.Provider>);
    }
    else
    {
      return null;
    }
  }
}

// ExternalRole passes on a PSRol ReactContext.
