
import {ContextInstanceT, PDRproxy} from "perspectives-proxy";

import PerspectivesComponent from "./perspectivesComponent";
import {DefaultPSContext, PSContext, PSContextType} from "./reactcontexts.js";
import {UserMessagingPromise} from "./userMessaging.js";
import i18next from "i18next";

interface ContextInstanceProps
{
  contextinstance: ContextInstanceT;
  children?: React.ReactNode;
}

export default class ContextInstance extends PerspectivesComponent<ContextInstanceProps, PSContextType>
{
  declare context: PSContextType;
  static contextType = PSContext
  
  constructor (props: ContextInstanceProps)
  {
    super(props);
    this.state = {...DefaultPSContext}
  }
  componentDidMount()
  {
    const component = this;
    PDRproxy.then(
      function (pproxy)
      {
        pproxy.getContextType(component.props.contextinstance)
          .then(
            function(contextType)
            {
              component.setState(
                { contextinstance: component.props.contextinstance
                , contexttype: contextType
                , myroletype: component.context.myroletype } );
            })
          .catch(e => UserMessagingPromise.then( um => 
            um.addMessageForEndUser(
              { title: i18next.t("contextInstance_title", { ns: 'preact' }) 
              , message: i18next.t("contextInstance_message", {ns: 'preact'})
              , error: e.toString()
              })));
      }
    );
  }

  componentDidUpdate()
  {
    this.componentDidMount();
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

ContextInstance.contextType = PSContext;
