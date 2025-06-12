import { FIREANDFORGET, PDRproxy, RoleInstanceT, Unsubscriber, ContextInstanceT } from "perspectives-proxy";
import { ContextInstance, ModelDependencies } from "perspectives-react";



type NotificationData = {
  text: string;
  data: {roleId: RoleInstanceT};
}

// In systemNotifications.ts
function generateNotifications(messages: NotificationData[]) {
  const next = messages.shift();
  if (next) {
    // Register a service worker if not already registered
    if ('serviceWorker' in navigator) {
      navigator.serviceWorker.register(`perspectives-serviceworker${__BUILD__}.js`)
        .then(registration => {
          // Basic notification options
          const notificationOptions = {
            data: { roleId: next.data.roleId },
            icon: "/www/AppImages/ios/50.png",
            badge: "/www/AppImages/ios/32.png",
             // This will cause each new notification to replace the previous one. That is ok, since we have our own 
            //  notification centre in the app.
            tag: "mycontexts-notification",
            actions: [{ action: 'open', title: 'Open' }] 
          } as NotificationOptions;

          // NOTICE: the requireInteraction option, supported in Chrome, actually 
          // prevents the notification from appearing!
          
          // Show notification through the ServiceWorker
          registration.showNotification(`(${next.data.roleId}) ` + next.text, notificationOptions);
        });
    } else {
      // Fallback for browsers without ServiceWorker
      const notification = new Notification(next.text, {
        data: next.data,
        icon: "/www/AppImages/ios/50.png"
      });
      
      notification.onclick = function() {
        console.log('Notification clicked!');
        window.focus();
        this.close();
        
        if (next.data.roleId) {
          document.dispatchEvent(new CustomEvent('OpenContext', {
            detail: next.data.roleId
          }));
        }
      };
    }
    
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
            // This happens once per session. We initialise the oldNotifications on all notifications available.
            oldNotifications = notifications;
          }
          else
          {
            // We had oldNotifications. Filter out the new ones.
            newNotifications = notifications.filter(x => !oldNotifications.includes(x));
            // Set the notifications we have just received, to the oldNotifications.
            oldNotifications = notifications;
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
