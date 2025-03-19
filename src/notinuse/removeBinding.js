import React, { Children, cloneElement } from "react";
import {PDRproxy} from "perspectives-proxy";

import PerspectivesComponent from "./perspectivescomponent.js";

import {PSContext} from "./reactcontexts.js"
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

export default function RemoveBinding (props)
{
  return <PSContext.Consumer>{ pscontext => <RemoveBinding_ myroletype={pscontext.myroletype}>{props.children}</RemoveBinding_> }</PSContext.Consumer>;
}

class RemoveBinding_ extends PerspectivesComponent
{
  render ()
  {
    const component = this;
    return Children.map(
      component.props.children,
      function(child)
      {
        return cloneElement(
          child,
          {
            // Can be applied to a PSRol context type.
            removeBinding: function({rolinstance})
            {
              PDRproxy.then(
                function (pproxy)
                {
                  pproxy
                    .removeBinding( rolinstance, component.props.myroletype )
                    .catch(e => UserMessagingPromise.then( um => 
                      um.addMessageForEndUser(
                        { title: i18next.t("unfill_title", { ns: 'preact' }) 
                        , message: i18next.t("unfill_message", {ns: 'preact' })
                        , error: e.toString()
                        })));              
                }
              );
            }
          });
      });
  }
}

// RemoveBinding passes on to the props of its children:
// removeBinding
