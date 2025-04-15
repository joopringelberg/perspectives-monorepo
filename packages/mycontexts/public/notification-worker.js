self.addEventListener('notificationclick', function(event) {
  console.log('Notification clicked!', event);
  
  // Close the notification
  event.notification.close();
  
  // Extract the roleId from the notification data
  const roleId = event.notification.data?.roleId;
  
  // Focus the client and dispatch the event
  if (roleId) {
    // This will focus the first client window
    event.waitUntil(
      clients.matchAll({ type: 'window' }).then(clientList => {
        if (clientList.length > 0) {
          return clientList[0].focus().then(client => {
            return client.postMessage({
              type: 'NOTIFICATION_CLICK',
              roleId: roleId
            });
          });
        }
        return clients.openWindow('/');
      })
    );
  }
});

// Listen for install event to cache files if needed
self.addEventListener('install', event => {
  self.skipWaiting();
});

// Activate immediately
self.addEventListener('activate', event => {
  event.waitUntil(clients.claim());
});

// Log any errors that might occur
self.addEventListener('error', (event) => {
  console.error('Service Worker error:', event.message, event.filename, event.lineno);
});