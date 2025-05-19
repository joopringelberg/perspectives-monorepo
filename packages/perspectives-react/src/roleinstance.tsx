import React, { Children, cloneElement, createRef } from "react";

import PerspectivesComponent from "./perspectivesComponent";

import {PSRol, PSRolType, DefaultPSRol} from "./reactcontexts";

import {RoleDataProper} from "perspectives-proxy";

import {PDRproxy, FIREANDFORGET, ContextInstanceT, RoleInstanceT, RoleType, RoleKind, ContextType} from "perspectives-proxy";


import BinaryModal from "./binarymodal.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

////////////////////////////////////////////////////////////////////////////////
// ROLEINSTANCE
////////////////////////////////////////////////////////////////////////////////
type RoleInstanceProps = {
  roleinstance?: RoleInstanceT;
  roletype: RoleType;
  rolekind: RoleKind;
  contextinstance: ContextInstanceT;
  contexttype: ContextType;
  myroletype: RoleType;
  allowedtoremovecontext: boolean;
  children: React.ReactNode[];
};

export default class RoleInstance extends PerspectivesComponent<RoleInstanceProps, (PSRolType & {showRemoveContextModal : boolean})>
{
  eventDiv: React.RefObject<HTMLDivElement | null>;

  constructor (props : RoleInstanceProps)
  {
    super(props);
    this.eventDiv = createRef();
    // We omit the functions.
    this.state = {
      ...DefaultPSRol,
      showRemoveContextModal: false
    };
    this.removeWithContext = this.removeWithContext.bind(this);
    this.removeWithoutContext = this.removeWithoutContext.bind(this);
  }

  componentDidMount ()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        // see RoleData (in perspectivesshape.js) for the structure of the roleData argument that binds to the parameter.
        function bind_(roleData : RoleDataProper) : Promise<boolean>
        {
          const filler = roleData.rolinstance;
          if (filler && component.props.roleinstance)
          {
            return pproxy 
              .bind_(
                component.props.roleinstance,
                filler,
                component.props.myroletype)
              .catch(e => UserMessagingPromise.then( um => 
                {
                  um.addMessageForEndUser(
                    { title: i18next.t("fillRole_title", { ns: 'preact' }) 
                    , message: i18next.t("fillRole_message", {ns: 'preact' })
                    , error: e.toString()
                  });
                  component.setState({showRemoveContextModal: false});
                  return false; // Ensure a boolean is returned
                }));
          }
          else return new Promise((resolve, reject) => { reject(false) });
      }

        // returns a promise for a RoleInstance.
        // see RoleData (in perspectivesshape.js) for the structure of the roleData argument that binds to the parameter.
        function bind (rolinstance : RoleInstanceT) : Promise<RoleInstanceT | void>
        {
          if (rolinstance)
          {
            return pproxy
              .bind(
                component.props.contextinstance,
                component.props.roletype,
                component.props.contexttype,
                {properties: {}, binding: rolinstance as unknown as string},
                component.props.myroletype)
              .catch(e => UserMessagingPromise.then( um => 
                {
                  um.addMessageForEndUser(
                    { title: i18next.t("fillRole_title", { ns: 'preact' }) 
                    , message: i18next.t("fillRole_message", {ns: 'preact' })
                    , error: e.toString()
                  });
                  component.setState({showRemoveContextModal: false})
                }));
          }
          return Promise.resolve(undefined);
        }

        // returns a promise for a boolean value.
        // see RoleData (in perspectivesshape.js) for the structure of the roleData argument that binds to the parameter.
        function checkbinding({rolinstance} : RoleDataProper)
        {
          // checkBinding( <(QUALIFIED)RolName>, <binding>, [() -> undefined] )
          // Where RolName identifies the role whose binding specification we want to compare with <binding>.
          return pproxy
            .checkBindingP(
              component.state.roltype!,
              rolinstance!);
        }

        function removerol()
        {
          if (component.state.roleKind == "ContextRole" && component.props.roleinstance && component.props.allowedtoremovecontext)
          {
            return pproxy.getBinding (
              component.props.roleinstance, 
              function( values )
              {
                if (values.length == 0)
                {
                  component.removeWithoutContext();
                }
                else 
                {
                  // Ask the user whether she wants to remove the context as well.
                  component.setState({showRemoveContextModal: true});
                }      
              }, 
              FIREANDFORGET).then( () => {});
          }
          else
          {
            return component.removeWithoutContext();
          }
        }
        function removecontext()
        {
          return component.removeWithContext();
        }
        component.setState( 
          { contextinstance: component.props.contextinstance
          , contexttype: component.props.contexttype
          , roltype: component.props.roletype
          , roleKind: component.props.rolekind
          , bind_
          , bind
          , checkbinding
          , removerol
          , removecontext
          , rolinstance: component.props.roleinstance
          , isselected: true // To accommodate the PSRol definition.
        });
      });
  }

  // This function is only called when there is a rolinstance value on the props.
  removeWithContext() : Promise<void>
  {
    const component = this;
    return PDRproxy.then(
      function (pproxy)
      {
        pproxy
          .removeContext(
            component.state.rolinstance!,
            component.state.roltype, // is always qualified.
            component.props.myroletype)
          .then( () => component.setState({showRemoveContextModal: false}))
          .catch(e => UserMessagingPromise.then( um => 
            {
              um.addMessageForEndUser(
                { title: i18next.t("removeContext_title", { ns: 'preact' }) 
                , message: i18next.t("removeContext_message", {ns: 'preact' })
                , error: e.toString()
              });
              component.setState({showRemoveContextModal: false});
            }));
      });
  }

  // This function is only called when there is a rolinstance value on the props.
  removeWithoutContext()
  {
    const component = this;
    return PDRproxy.then(
        function (pproxy)
        {
          pproxy
            .removeRole(
              component.state.roltype,
              component.state.rolinstance!,
              component.props.myroletype)
            .then( () => component.setState({showRemoveContextModal: false}) )
            .catch(e => UserMessagingPromise.then( um => 
              {
                um.addMessageForEndUser(
                  { title: i18next.t("removeRole_title", { ns: 'preact' }) 
                  , message: i18next.t("removeRole_message", {ns: 'preact' })
                  , error: e.toString()
                });
                component.setState({showRemoveContextModal: false})
              }))
        });

  }

  render ()
  {
    const component = this;
    let defaultElement, children;
    if (component.stateIsComplete(["rolinstance"]))
    {
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
      if (component.state.rolinstance)
      {
        return  <>
                  <PSRol.Provider value={component.state}>
                    {children}
                  </PSRol.Provider>
                  <BinaryModal
                    title={i18next.t("roleinstance_removecontext_title", { ns: 'preact' })}
                    message={i18next.t("roleinstance_removecontextmessage", { ns: 'preact' })}
                    show={component.state.showRemoveContextModal}
                    yes={component.removeWithContext}
                    no={component.removeWithoutContext}
                    close={() => component.setState({showRemoveContextModal: false})}
                    />
                </>;
      }
      else if ( defaultElement )
      {
        // The provided PSRol instance misses the `roleinstance` value, but
        // has an extra prop `createrole`.
        return  <PSRol.Provider value={component.state}>
                  {
                    cloneElement( defaultElement as React.ReactElement<any>)
                  }
                </PSRol.Provider>;
      }
      else
      {
        return <div/>;
      }
    }
    else
    {
      return null;
    }
  }
}
