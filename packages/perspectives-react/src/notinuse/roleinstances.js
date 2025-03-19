import React, { createRef } from "react";
import PropTypes from "prop-types";
import {PDRproxy, FIREANDFORGET} from "perspectives-proxy";

import PerspectivesComponent from "./perspectivescomponent.js";
import {PSRoleInstances, PSContext} from "./reactcontexts";
import {isQualifiedName} from "./urifunctions.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

// This component retrieves the instances of the Role type it finds on its props,
// from the context instance it finds in its React context (a PSContext type).
// If either changes, the instances are recomputed.
// It provides those instances (as PSRoleInstances.Provider) on its state and the functions:
//  - bind (create a role instance and fill it),
//  - createRole and
//  - checkbinding.
// It has a cursor position that can be set by the user pressing up and down arrow keys
// in the subtree of components (it sets a handler that listens for keypresses
// bubbling up from that tree). By default the cursor is the first of the instances.
// It also has an interesting property, roleKind.

// As an alternative, it takes a perspective and sets its internal state from that.

export default class RoleInstances extends PerspectivesComponent
{
  constructor (props)
  {
    super(props);
    const component = this;
    component.state.instances = undefined;
    this.eventDiv = createRef();
    this.handleKeyPress = this.handleKeyPress.bind(this);
    this.bind = this.bind.bind(this);
    this.checkBinding = this.checkBinding.bind(this);
  }

  componentDidMount ()
  {
    const component = this;
    component.getRoleInstances();
    if (component.stateIsComplete())
    {
      component.eventDiv.current?.addEventListener('SetCursor',
        function (e)
        {
          component.setcursor( e.detail );
          e.stopPropagation();
        },
        false);
    }
    // Mark the perspective, so we can detect that the PDR has
    // given us a new value.
    if ( component.props.perspective )
    {
      component.props.perspective.seenBefore = true;
    }
  }

  // Returns a promise for a boolean value.
  bind ({rolinstance})
  {
    const component = this;
    if (rolinstance)
    {
      PDRproxy.then(
        function (pproxy)
        {
          pproxy
            .bind(
              component.context.contextinstance,
              component.props.rol, // may be a local name.
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

  checkBinding({rolinstance}, callback)
  {
    const component = this;
    // checkBinding( <(QUALIFIED)RolName>, <binding>, [() -> undefined] )
    PDRproxy.then( pproxy => pproxy.checkBindingP( component.state.roltype, rolinstance ).then( callback ) );
  }

  setTheState(rolIdArr, roleKind, rolType)
  {
    const component = this;
    component.setState( function( oldState )
      {
        let nextCursor;
        if ( rolIdArr.indexOf(oldState.cursor) < 0 )
        {
          nextCursor = rolIdArr[0];
        }
        else {
          nextCursor = oldState.cursor;
        }
        return  { contextinstance: component.context.contextinstance
                , contexttype: component.context.contexttype
                , rol: component.props.rol
                , roltype: rolType // always qualified.
                , roleKind
                , instances: rolIdArr.sort()
                , cursor: nextCursor
                , createRole: function (receiveResponse)
                  {
                    PDRproxy.then( function (pproxy)
                    {
                      // If a ContextRole Kind, create a new context, too.
                      if (roleKind == "ContextRole" && component.props.contexttocreate)
                      {
                        pproxy.createContext (
                            {
                              //id will be set in the core.
                              prototype : undefined,
                              ctype: component.props.contexttocreate,
                              rollen: {},
                              externeProperties: {}
                            },
                            component.props.rol,                // qualified role name.
                            component.context.contextinstance,  // context instance to add to.
                            component.context.myroletype)
                          .then(
                          // [<externalRoleId>(, <contextRoleId>)?]
                            function(contextAndExternalRole)
                            {
                              // Return the new context role identifier!
                              receiveResponse( contextAndExternalRole[1] );
                            })
                          .catch(e => UserMessagingPromise.then( um => 
                            um.addMessageForEndUser(
                              { title: i18next.t("createContext_title", { ns: 'preact' }) 
                              , message: i18next.t("createContext_message", {ns: 'preact', type: component.context.contexttype})
                              , error: e.toString()
                              })));
                      }
                      else
                      {
                        pproxy
                          .createRole (
                            component.context.contextinstance,
                            rolType,
                            component.context.myroletype)
                          .then( newRoleId_ => receiveResponse( newRoleId_[0] ) )
                          .catch(e => UserMessagingPromise.then( um => 
                            um.addMessageForEndUser(
                              { title: i18next.t("createRole_title", { ns: 'preact' }) 
                              , message: i18next.t("createRole_message", {ns: 'preact', roletype: rolType})
                              , error: e.toString()
                              })))
                      }});
                  }
                , bind: component.bind
                , checkbinding: component.checkBinding
                };
      }
    );

  }

  getRoleInstances()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        // Will be called exactly once, with the qualified name of the role.
        function getRol(rolTypeArr)
        {
          const rolType = rolTypeArr[0];
          if (!rolType)
          {
            throw("Rol: could not establish qualified name of Rol '" + component.props.rol + "' for Context '" + component.context.contexttype + "'.");
          }
          // Get the role kind. No need to unsubscribe: the result won't change.
          pproxy.getRoleKind( rolType )
            .then( function(roleKindArr)
              {
                const roleKind = roleKindArr[0];
                // Get role instances.
                component.addUnsubscriber(
                  pproxy.getRol(
                    component.context.contextinstance,
                    rolType,
                    function(rolIdArr)
                    {
                      component.setTheState(rolIdArr, roleKind, rolType);
                    }
                ));
              })
            .catch(e => UserMessagingPromise.then( um => 
              um.addMessageForEndUser(
                { title: i18next.t("rolekind_title", { ns: 'preact' }) 
                , message: i18next.t("rolekind_message", {ns: 'preact', role: rolType })
                , error: e.toString()
                })));
        }

        if (component.props.perspective)
        {
          component.setTheState(
            Object.keys( component.props.perspective.roleInstances ),
            component.props.perspective.roleKind,
            component.props.perspective.roleType
          );
        }
        else
        {
          if (isQualifiedName( component.props.rol ))
          {
            getRol( [component.props.rol] );
          }
          else
          {
            pproxy.getUnqualifiedRolType(
              component.context.contexttype,
              component.props.rol,
              getRol,
              FIREANDFORGET
            );
          }
        }
      });
  }

  // This function takes care of re-rendering, when the eventDiv will have been deleted
  // and the listener has vanished with it. We re-establish the listener on the newly
  // created eventDiv instance.
  componentDidUpdate (prevProps, prevState)
  {
    const component = this;
    if ( component.stateIsComplete() && !prevState.instances )
    {
      component.eventDiv.current?.addEventListener('SetCursor',
        function (e)
        {
          component.setcursor( e.detail );
          e.stopPropagation();
        },
        false);
    }
    // When the contextinstance on the component's context changes, we need to recompute the role instances.
    // The same holds for when the `rol` prop changes.
    if ( component.props.perspective && !component.props.perspective.seenBefore )
    {
      component.props.perspective.seenBefore = true;
      component.getRoleInstances();
    }
    if  ( component.context.contextinstance !== component.state.contextinstance ||
          (prevProps.rol !== component.props.rol )
        )
    {
      component.getRoleInstances();
    }
  }

  // -1 if not found.
  indexOfCurrentRole ()
  {
    return this.state.instances.indexOf(this.state.cursor);
  }

  handleKeyPress (event)
    {
      const component = this;
      const i = component.indexOfCurrentRole();

      // keydown only occurs after the component received focus, hence if there are instances, there is a value for currentRole.
      if (component.state.cursor)
      {
        // Check for up/down key presses
        switch(event.keyCode){
          case 40: // Down arrow
            if ( i < component.state.instances.length - 1 )
            {
              component.setcursor( component.state.instances[i + 1] );
            }
            event.preventDefault();
            break;
          case 38: // Up arrow
            if (i > 0)
            {
              component.setcursor( component.state.instances[i - 1] );
            }
            event.preventDefault();
            break;
        }
      }
  }

  setcursor (cr)
  {
    if (cr !== this.state.cursor && cr)
    {
      this.setState( {cursor: cr} );
    }
  }

  render ()
  {
    const component = this;
    if (component.stateIsComplete(["cursor"]))
    {
      return (<PSRoleInstances.Provider value={component.state}>
          <div
            ref={component.eventDiv}
            onKeyDown={ component.handleKeyPress }
          >
            {component.props.children}
          </div>
        </PSRoleInstances.Provider>);
    }
    else return null;
  }
}

RoleInstances.contextType = PSContext;

RoleInstances.propTypes =
  {
    // May be a local name that will be resolved to the embedding context given by
    // PSContext.
    // Can also be a qualified name.
    rol: PropTypes.string.isRequired
    // fully qualified name: the type of Context to create.
    // The core loads the model that defines this type, if it is not locally available.
  , contexttocreate: PropTypes.string
  // When a perspective is provided, the RoleInstances does not retrieve information from the core itself.
  , perspective: PropTypes.object
  };
