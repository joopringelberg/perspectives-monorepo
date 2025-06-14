if ("serviceWorker" in navigator) {
  let refreshing = false;
  let registration;
  
  // FIRST: Unregister any existing pagedispatcher worker
  // IMPLEMENTATIOPN NOTE: This is a cleanup step to ensure no old service workers interfere.
  // WE CAN REMOVE THIS ON THE NEXT RELEASE.
  navigator.serviceWorker.getRegistrations().then(registrations => {
    for(let reg of registrations) {
      const scriptURL = reg.active?.scriptURL || reg.waiting?.scriptURL || reg.installing?.scriptURL || '';
      if (scriptURL.includes('pagedispatcher') || scriptURL.includes('notification-worker')) {
        console.log('Unregistering old worker:', reg);
        reg.unregister();
      }
    }
  }).catch(error => {
    console.error('Error checking for old service workers:', error);
  });

  // Automatically detect the base path from the current URL
  const getBasePath = () => {
    // For development server
    if (window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1') {
      return window.location.pathname.includes('/www/') ? '/www/' : '/';
    }
    
    // For production server
    return window.location.pathname.includes('/www/') ? '/www/' : '/';
  };

  const basePath = getBasePath();

  // Register service worker and set up update checking
  navigator.serviceWorker.register(`${basePath}perspectives-serviceworker.js`)
    .then(reg => {
      console.log(`Perspectives-service worker registration succeeded (base: ${basePath}):`, reg);
      registration = reg;
      
      // Set up update checking interval
      setInterval(() => {
        registration.update();
        console.log('Checking for updates to perspectives-serviceworker...');
      }, 15 * 60 * 1000); // Check every 15 minutes
      
      // Listen for new service worker installation once, at registration time
      registration.addEventListener('updatefound', () => {
        const newWorker = registration.installing;
        console.log('New perspectives-service worker installing...');
        
        newWorker.addEventListener('statechange', () => {
          console.log(`Service worker state changed: ${newWorker.state}`);
          if (newWorker.state === 'installed' && navigator.serviceWorker.controller) {
            // Broadcast to the app that an update is available
            const channel = new BroadcastChannel('app-update-channel');
            channel.postMessage('NEW_VERSION_AVAILABLE');
            channel.close();
          }
        });
      });
      
      // Controller change event (triggers page reload when new service worker takes control)
      navigator.serviceWorker.addEventListener('controllerchange', () => {
        if (!refreshing) {
          refreshing = true;
          console.log('New service worker controller, refreshing page...');
          window.location.reload();
        }
      });
    })
    .catch(error => {
      console.error(`Perspectives-service worker registration failed:`, error);
    });

  // Listen for direct messages from service worker
  navigator.serviceWorker.addEventListener('message', (event) => {
    console.log('Message received from service worker:', event.data);
    if (event.data === 'NEW_VERSION_AVAILABLE') {
      // Notify the app about the update
      const channel = new BroadcastChannel('app-update-channel');
      channel.postMessage('NEW_VERSION_AVAILABLE');
      channel.close();
    }
  });
} else {
  console.error("Service workers are not supported.");
}
