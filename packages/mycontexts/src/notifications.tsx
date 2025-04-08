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

type NotificationData = {
  text: string;
  data: {roleId: RoleInstanceT};
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

    function generateNotifications( messages : NotificationData[] )
    {
      const next = messages.shift();
      if (next)
      {
        new Notification( next.text, {data: next.data});
        setTimeout(()=> generateNotifications( messages ), 1000);
      }
    }

    PDRproxy.then( pproxy =>
      {
        component.addUnsubscriber(
          pproxy.getRol (component.props.systemcontextinstance,
            ModelDependencies.allNotifications,
            function(notifications : RoleInstanceT[])
            {
              const oldNotifications = component.notifications;
              let newNotifications : RoleInstanceT[] = [];
              const notificationData = [];
              if ( oldNotifications.length === 0 && notifications.length > 1 )
              {
                newNotifications = [];
              }
              else
              {
                newNotifications = notifications.filter(x => !oldNotifications.includes(x));
              }
              component.notifications = notifications;
              console.log(newNotifications);
              if (component.props.shownotifications)
              {
                Promise.all( newNotifications.map( function(notification)
                  {
                    return new Promise((resolve, reject) => 
                    {
                      pproxy.getProperty(
                        notification,
                        ModelDependencies.notificationMessage,
                        ModelDependencies.notifications,
                        function( messages )
                        {
                          resolve( {text: messages[0], data: {roleId: notification}});
                        },
                        FIREANDFORGET
                      );                        
                    });
                  }) ).then( generateNotifications );
                }
            }));
        
        // getPerspective (roleInstanceOfContext, perspectiveObjectRoleType /*OPTIONAL*/, receiveValues, fireAndForget, errorHandler)
        pproxy.getPerspective(
          component.props.externalroleid,
          ModelDependencies.notifications,
          function( perspectiveArray )
          {
            component.setState({perspective: perspectiveArray[0]});
          },
          CONTINUOUS
        ).then( unsubscriber => component.currentContextNotificationsUnsubscriber = unsubscriber);
    } );
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

