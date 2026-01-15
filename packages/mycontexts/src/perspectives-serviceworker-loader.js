if ("serviceWorker" in navigator) {
  let refreshing = false;
  let registration;
  
  // Register service worker and set up update checking
  navigator.serviceWorker.register('perspectives-serviceworker.js')
    .then(reg => {
      console.log("Perspectives-service worker registration succeeded:", reg);
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
} else {
  console.error("Service workers are not supported.");
}
