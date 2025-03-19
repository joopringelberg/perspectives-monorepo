import React from "react";
import PropTypes from "prop-types";
import {PDRproxy} from "perspectives-proxy";
import PerspectivesComponent from "./perspectivescomponent.js";
import {PSContext} from "./reactcontexts";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

export default class ContextOfRole extends PerspectivesComponent
{
  constructor (props)
  {
    super(props);
    this.state.contextinstance = undefined;
    this.state.contexttype = undefined;
    this.state.myroletype = undefined;
  }

  componentDidMount ()
  {
    const component = this;
    let contextId;
    PDRproxy.then(
      function (pproxy)
      {
        // The context of the rol will be bound to the state prop 'contextInstance'.
        pproxy.getRolContext( component.props.rolinstance )
          .then( contextIds => contextId = contextIds[0] )
          .then( c => pproxy.getContextType(c) )
          .then( contextTypes => 
            {
              if ( !component.props.myroletype )
              {
                // Get it from the core.
                // This we subscribe to: it may change.
                component.addUnsubscriber(
                  pproxy.getMeForContext( component.props.rolinstance,
                    function(myroletype)
                    {
                      component.setState(
                        { contextinstance: contextId
                        , contexttype: contextTypes[0]
                        , myroletype: myroletype[0]
                        });
                    }
                    // Add user message here or throw.
                    ));
              }
              else {
                component.setState(
                  { contextinstance: contextId[0]
                  , contexttype: contextType[0]
                  , myroletype: component.props.myroletype
                  });
              }
            })
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("contextOfRole_title", { ns: 'preact' }) 
              , message: i18next.t("contextOfRole_message", {ns: 'preact', role: component.props.rolinstance})
              , error: e.toString()
              })));
      }
    );
  }

  componentDidUpdate (prevProps)
  {
    const component = this;
    if (component.props.rolinstance !== prevProps.rolinstance)
    {
      component.componentDidMount();
    }
  }

  render ()
  {
    const component = this;

    if (component.stateIsComplete())
    {
      return (<PSContext.Provider value={component.state}>
          {component.props.children}
        </PSContext.Provider>);
    }
    else
    {
      return null;
    }
  }

}

ContextOfRole.propTypes = {
  rolinstance: PropTypes.string.isRequired
};
