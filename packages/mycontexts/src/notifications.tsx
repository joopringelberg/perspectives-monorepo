// Displays the notifications of a context in a RoleTable.
// Because of the RoleTable, must be used in the subtree of a

import React from "react";
import { CONTINUOUS, FIREANDFORGET, PDRproxy, Perspective, RoleInstanceT, Unsubscriber } from "perspectives-proxy";
import { ModelDependencies, PerspectivesComponent, PerspectiveTable } from "perspectives-react";

interface NotificationsDisplayerProps
{
  systemcontextinstance: string;
  externalroleid: RoleInstanceT;
  shownotifications: boolean;
  navigateto: (state: any) => void;
}

interface NotificationsDisplayerState
{
  perspective: Perspective | undefined;
}

// PSContext provider.
export class NotificationsDisplayer extends PerspectivesComponent<NotificationsDisplayerProps, NotificationsDisplayerState>
{
  notifications: RoleInstanceT[];
  currentContextNotificationsUnsubscriber: Unsubscriber | undefined;
  constructor(props : NotificationsDisplayerProps)
  {
    super(props);
    this.notifications = [];
    this.state = {perspective: undefined};
    this.currentContextNotificationsUnsubscriber = undefined;
  }
  componentDidMount()
  {
    const component = this;
    PDRproxy.then( pproxy =>
      // getPerspective (roleInstanceOfContext, perspectiveObjectRoleType /*OPTIONAL*/, receiveValues, fireAndForget, errorHandler)
      pproxy.getPerspective(
        component.props.externalroleid,
        ModelDependencies.notifications,
        function( perspectiveArray )
        {
          component.setState({perspective: perspectiveArray[0]});
        },
        CONTINUOUS
      ).then( unsubscriber => component.currentContextNotificationsUnsubscriber = unsubscriber));
  }

  componentDidUpdate(prevProps : NotificationsDisplayerProps)
  {
    const component = this;
    if (prevProps.externalroleid != this.props.externalroleid)
    {
      PDRproxy.then( pproxy => 
        {
          // unsubscriber = {subject: req.subject, corrId: req.corrId}
          if (component.currentContextNotificationsUnsubscriber)
          {
            pproxy.send(component.currentContextNotificationsUnsubscriber, function(){});
          }
          pproxy.getPerspective(
            component.props.externalroleid,
            ModelDependencies.notifications,
            function( perspectiveArray )
            {
              component.setState({perspective: perspectiveArray[0]});
            },
            CONTINUOUS
          ).then( unsubscriber => component.currentContextNotificationsUnsubscriber = unsubscriber);
        });
    }
  }

  render()
  {
    const component = this;
    if (this.state.perspective === undefined)
    {
      return null;
    }
    else
    {
      // We have a perspective, produce a table.
      {
        return  component.state.perspective ? <PerspectiveTable perspective={component.state.perspective}/> : <div/>
      }
    }
  }

}

