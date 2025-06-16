// Displays the notifications of a context in a RoleTable.
// Because of the RoleTable, must be used in the subtree of a


import { CONTINUOUS, FIREANDFORGET, PDRproxy, Perspective, RoleInstanceT, Unsubscriber } from "perspectives-proxy";
import { ModelDependencies, PerspectivesComponent, PerspectiveTable } from "perspectives-react";

interface NotificationsDisplayerProps
{
  externalroleid: RoleInstanceT;
  shownotifications: boolean;
  showAllNavigations?: boolean;
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
    this.getNotifications();
  }

  componentDidUpdate(prevProps : NotificationsDisplayerProps)
  {
    if (prevProps.externalroleid != this.props.externalroleid)
    {
      this.getNotifications();
    }
  }

  getNotifications()
  {
    const component = this;
    PDRproxy.then( pproxy => 
      {
        // unsubscriber = {subject: req.subject, corrId: req.corrId}
        if (component.currentContextNotificationsUnsubscriber)
        {
          pproxy.send(component.currentContextNotificationsUnsubscriber, function(){});
        }
        if (component.props.showAllNavigations)
        {
          // Why getPerspectiveForUser? Because we want the perspective of the WWWUser, not User.
          pproxy.getPerspectiveForUser(
            component.props.externalroleid,
            ModelDependencies.allNotifications,
            ModelDependencies.WWWUser,
            function( perspectiveArray )
            {
              component.setState({perspective: perspectiveArray[0]});
            },
            CONTINUOUS
              ).then( unsubscriber => component.currentContextNotificationsUnsubscriber = unsubscriber);
        }
        else {
          // Why getPerspective? Because the API will sort out what role the end user has and will use its perspectives.
          pproxy.getPerspective(
            component.props.externalroleid,
            ModelDependencies.notifications,
            function( perspectiveArray )
            {
              component.setState({perspective: perspectiveArray[0]});
            },
            CONTINUOUS
              ).then( unsubscriber => component.currentContextNotificationsUnsubscriber = unsubscriber);
        }
      });
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

