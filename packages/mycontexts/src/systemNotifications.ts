import { FIREANDFORGET, PDRproxy, RoleInstanceT, Unsubscriber, ContextInstanceT } from "perspectives-proxy";
import { ContextInstance, ModelDependencies } from "perspectives-react";

type NotificationData = {
  text: string;
  data: {roleId: RoleInstanceT};
}

function generateNotifications(messages: NotificationData[]) {
  const next = messages.shift();
  if (next) {
    // Basic notification options
    const notificationOptions: NotificationOptions = {
      data: next.data,
      icon: "/www/AppImages/ios/50.png",  // Use absolute path with base URL
      badge: "/www/AppImages/ios/32.png"
    };
    
    // Only add requireInteraction on supported browsers
    const isChromeOrEdge = 
      navigator.userAgent.includes('Chrome') || 
      navigator.userAgent.includes('Edg');
    
    if (isChromeOrEdge) {
      notificationOptions.requireInteraction = true;
    }
    
    // Create notification with browser-appropriate options
    const notification = new Notification(next.text, notificationOptions);
    
    setTimeout(() => generateNotifications(messages), 1000);
  }
}

let oldNotifications : RoleInstanceT[] = [];

export function subscribeToAllNotifications (systemcontextinstance : ContextInstanceT) : Promise<Unsubscriber | undefined>{
  let permissionGranted = false;
  if ('Notification' in window) {
    permissionGranted = Notification.permission === 'granted';
  }
  if (permissionGranted) {
    return PDRproxy.then( pproxy =>
    {
      const unsubscriber = pproxy.getRol (systemcontextinstance,
        ModelDependencies.allNotifications,
        function(notifications : RoleInstanceT[])
        {
          let newNotifications : RoleInstanceT[] = [];
          if ( oldNotifications.length === 0 && notifications.length > 1 )
          {
            oldNotifications = notifications;
          }
          else
          {
            newNotifications = notifications.filter(x => !oldNotifications.includes(x));
          }
          notifications = notifications;
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
      });
      return unsubscriber;
    });
  }
  return Promise.resolve(undefined);
}
