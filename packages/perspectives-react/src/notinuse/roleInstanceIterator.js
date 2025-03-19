import React from "react";
import {PDRproxy} from "perspectives-proxy";//3

import PerspectivesComponent from "./perspectivescomponent.js";
import {PSRol, PSRoleInstances, PSContext} from "./reactcontexts";
import BinaryModal from "./binarymodal.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

// This component requires a PSRoleInstances context and iterates over its instances.
// It provides the PSRol context to each role instance. PSRol contains the functions:
//  - bind_ (fill the existing role instance)
//  - checkbinding (passed on from PSRoleInstances)
//  - removerol
// It provides the following interesting properties to each PSRol:
//  - isselected: a boolean that is true if the instance is the current value of the
//    cursor of the RoleInstances context.
//  - roleKind.

export default function RoleInstanceIterator (props)
{
  return <PSContext.Consumer>{ pscontext => <RoleInstanceIterator_
      myroletype={pscontext.myroletype}
      /*eslint-disable-next-line react/prop-types*/
    >{props.children}</RoleInstanceIterator_> }</PSContext.Consumer>;
}

class RoleInstanceIterator_ extends PerspectivesComponent
{
  constructor (props)
  {
    super(props);
    // We need to copy the instances to state in order to be able to compare them
    // with new instances in the context on an update.
    this.state.instances = undefined;
    // We keep a copy of the cursor to provide an optimisation in componentDidUpdate.
    // It allows us to recompute only two instances instead of all of them.
    // This copy has no role in instance selection and moving.
    this.state.cursor = undefined;
    this.state.showRemoveContextModal = false;
    this.removeWithContext = this.removeWithContext.bind(this);
    this.removeWithoutContext = this.removeWithoutContext.bind(this);
  }

  // Compute a PSRol instance for the rolInstance.
  computeInstanceData (rolInstance, rolBindingContext)
  {
    const component = this;
    // returns a function that returns a promise.
    function bind_(rolInstance)
    {
      return function({rolinstance})
        {
          if (rolinstance)
          {
            // checkBinding( <contexttype>, <localRolName>, <binding>, [() -> undefined] )
            return PDRproxy.then( pproxy =>
              pproxy
                .bind_(
                  rolInstance,
                  rolinstance,
                  component.props.myroletype))
                .catch(e => UserMessagingPromise.then( um => 
                  {
                    um.addMessageForEndUser(
                      { title: i18next.t("fillRole_title", { ns: 'preact' }) 
                      , message: i18next.t("fillRole_message", {ns: 'preact' })
                      , error: e.toString()
                    })
                  }))
              }
          else return new Promise((resolve, reject) => { reject(false) });          
        };
    }
    return (
      { contextinstance: component.context.contextinstance
      , contexttype: component.context.contexttype
      , roltype: component.context.roltype
      , roleKind: component.context.roleKind
      , bind_: bind_( rolInstance )
      , bind: rolBindingContext.bind
      , checkbinding: rolBindingContext.checkbinding
      , removerol: function()
        {
          if (component.context.roleKind == "ContextRole")
          {
            // Ask the user whether she wants to remove the context as well.
            component.setState({showRemoveContextModal: true});
          }
          else
          {
            component.removeWithoutContext();
          }
        }
      , removecontext: function()
        {
          component.removeWithContext();
        }
      , rolinstance: rolInstance
      , isselected: component.context.cursor === rolInstance
      });
  }

  removeWithContext()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        pproxy.removeContext(
          component.context.cursor,
          component.context.roltype,
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
          }))
      });
  }

  removeWithoutContext()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        pproxy
          .removeRol(
            component.context.roltype,
            component.context.cursor,
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

  componentDidMount ()
  {
    const component = this;
    const updater =
      { rolBindingContext:
          { contextinstance: component.context.contextinstance
          , contexttype: component.context.contexttype
          // Returns a promise for a boolean value.
          // see RoleData (in perspectivesshape.js) for the structure of the roleData argument that binds to the parameter.
          , bind: function({rolinstance})
              {
                if (rolinstance)
                {
                  return PDRproxy.then(
                    function (pproxy)
                    {
                      return pproxy
                        .bind(
                          component.context.contextinstance,
                          component.context.roltype,
                          component.context.contexttype,
                          {properties: {}, binding: rolinstance},
                          component.context.myroletype)
                        .catch(e => UserMessagingPromise.then( um => 
                          {
                            um.addMessageForEndUser(
                              { title: i18next.t("fillRole_title", { ns: 'preact' }) 
                              , message: i18next.t("fillRole_message", {ns: 'preact' })
                              , error: e.toString()
                            });
                            component.setState({showRemoveContextModal: false})
                          }));
                    });
                }
                else return new Promise((resolve, reject) => { reject(false) });
              }
          , checkbinding: component.context.checkbinding
          }
      , instances: component.context.instances
      , cursor: component.context.cursor
      };
    component.context.instances.forEach( function (rolInstance)
      {
        updater[rolInstance] = component.computeInstanceData(rolInstance, updater.rolBindingContext);
      });
    component.setState(updater);
  }

  componentDidUpdate (/*prevProps, prevState*/)
  {
    function equalArrays (array1, array2)
    {
      return array1.length === array2.length && array1.every(function(value, index) { return value === array2[index];});
    }
    const component = this,
      previousCursor = component.state.cursor,
      currentCursor = component.context.cursor;
    let updater = {};

    if ( !equalArrays( component.context.instances, component.state.instances ) )
    {
      updater = { instances: component.context.instances };
      component.context.instances.forEach( function (rolInstance)
        {
          updater[rolInstance] = component.computeInstanceData(rolInstance, component.state.rolBindingContext);
        });
      component.setState( updater );
    }
    // If only the cursor has changed, we just recompute the corresponding element.
    if ( currentCursor !== previousCursor )
    {
      updater[previousCursor] = component.computeInstanceData( previousCursor, component.state.rolBindingContext );
      updater[currentCursor] = component.computeInstanceData( currentCursor, component.state.rolBindingContext );
      updater.cursor = currentCursor;
      component.setState( updater );
    }
  }

  render ()
  {
    const component = this;
    if (component.stateIsComplete())
    {
      return  <>
                {component.state.instances.map(
                  function( rolInstance )
                  {
                    return  <PSRol.Provider key={rolInstance} value={component.state[rolInstance]}>
                              {component.props.children}
                              </PSRol.Provider>;
                  })}
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
    else
    {
      return null;
    }
  }
}

RoleInstanceIterator_.contextType = PSRoleInstances;

RoleInstanceIterator_.propTypes = {};
