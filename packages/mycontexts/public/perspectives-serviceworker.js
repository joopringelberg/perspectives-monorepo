// BEGIN LICENSE
// Perspectives Distributed Runtime
// Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
//
// Full text of this license can be found in the LICENSE file in the projects root.
// END LICENSE

////////////////////////////////////////////////////////////////////////////////
//// SERVICE WORKER
////////////////////////////////////////////////////////////////////////////////

const currentVersion = "3.0.0" + 413;
let previousVersion = '';

const cacheName = "mycontexts" + currentVersion;

// Define base path to handle potential base URL issues
const baseUrl = self.location.href.replace(/\/[^\/]*$/, '');

const appFiles = [
  "/www/index.html",
  "/www/manage.html",
  "/www/assets/main.js",
  "/www/assets/main.css"
];

const macIcons = ["512.png", "256.png", "128.png", "32.png", "16.png"].map(icon => "/AppImages/ios/" + icon);

// Join app files and icons
const toBeCached = appFiles.concat(macIcons);

self.addEventListener("install", (e) => {
  console.log(`[perspectives-serviceworker ${currentVersion}] Install`);
  
  e.waitUntil(
    (async () => {
      // Try to get previous version from cache
      const versioncache = await caches.open('version-info');
      const versionResponse = await versioncache.match('/version-info');
      if (versionResponse) {
        previousVersion = await versionResponse.text();
      }

      // Store current version in cache
      await versioncache.put('/version-info', new Response(currentVersion));

      console.log(`[perspectives-serviceworker ${currentVersion}] Previous version: ${previousVersion}, Current version: ${currentVersion}`);

      // If there was a previous version and it's different, notify the app
      if (previousVersion && previousVersion !== currentVersion) {
        try {
          // Notify all active clients directly
          const clients = await self.clients.matchAll();
          clients.forEach(client => {
            client.postMessage('NEW_VERSION_AVAILABLE');
            console.log(`[perspectives-serviceworker ${currentVersion}] Sent update notification to client`);
          });
          
          // Also broadcast the update
          const bc = new BroadcastChannel('app-update-channel');
          bc.postMessage('NEW_VERSION_AVAILABLE');
          console.log(`[perspectives-serviceworker ${currentVersion}] Broadcast update notification`);
          bc.close();
        } catch (error) {
          console.error(`[perspectives-serviceworker ${currentVersion}] Error notifying clients:`, error);
        }
      }

      const cache = await caches.open(cacheName);
      console.log("[perspectives-serviceworker ${currentVersion}] Caching all mycontext sources in cache: " + cacheName);
      
      // Cache files individually to better handle errors
      const cachePromises = toBeCached.map(async (url) => {
        try {
          // Check if URL is absolute or needs the base
          const resourceUrl = url.startsWith('http') ? url : baseUrl + url;
          console.log(`[perspectives-serviceworker ${currentVersion}] Attempting to cache: ${resourceUrl}`);
          
          // First check if resource exists before caching
          const checkResponse = await fetch(resourceUrl, { method: 'HEAD' })
            .catch(err => {
              console.log(`[perspectives-serviceworker ${currentVersion}] Resource not available: ${resourceUrl}`, err);
              return null;
            });
            
          if (checkResponse && checkResponse.ok) {
            await cache.add(resourceUrl);
            console.log(`[perspectives-serviceworker ${currentVersion}] Successfully cached: ${resourceUrl}`);
          } else {
            console.log(`[perspectives-serviceworker ${currentVersion}] Skipping unavailable resource: ${resourceUrl}`);
          }
        } catch (error) {
          console.error(`[perspectives-serviceworker ${currentVersion}] Error caching ${url}:`, error);
          // Continue despite errors - don't block installation
        }
      });
      
      // Wait for all cache operations to complete
      await Promise.allSettled(cachePromises);
      console.log("[perspectives-serviceworker ${currentVersion}] Caching complete");
    })()
  );
});

self.addEventListener("fetch", (e) => {
  e.respondWith(
    (async () => {
      const url = new URL( e.request.url )
      if ( toBeCached.find( fl => url.pathname == fl ) )
      {
        const r = await caches.match(e.request);
        if (r) {
          console.log(`[perspectives-serviceworker ${currentVersion}] Taking resource ${e.request.url} from cache: ${cacheName}.`);
          return r;
        }
        console.log( `[perspectives-serviceworker ${currentVersion}] ${e.request.url} should have been cached but is not. Fetching and caching it in cache ${cacheName}.`);
        const response = await fetch(e.request);
        const cache = await caches.open(cacheName);
        cache.put(e.request, response.clone());
        return response;
        // return await fetch(e.request);
      }
      else
      {
        // console.log( `[perspectives-serviceworker ${currentVersion}] Passing through this request without caching: ${e.request.url}`);
        return await fetch(e.request);
      }
    })(),
  );
});

self.addEventListener("activate", (e) => {
  e.waitUntil(clients.claim());
  e.waitUntil(
    caches.keys().then((keyList) => {
      return Promise.all(
        keyList.map((key) => {
          if (key === cacheName) {
            return;
          }
          console.log( "Deleting cache for key " + key);
          return caches.delete(key);
        }),
      );
    }),
  );
});


//////////////////////////////////////////////////////////////////
// Notification click handler
//////////////////////////////////////////////////////////////////

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

// Merge the message event listener
self.addEventListener('message', function(event) {
  // Check what type of message we're receiving
  if (event.data === 'SKIP_WAITING') {
    // Your existing skipWaiting logic
    self.skipWaiting();
    return;
  }
  
  // Handle port relay functionality from pagedispatcher
  if (event.data && event.data.messageType === "relayPort") {
    self.clients.matchAll()
      .then(function(clientList) {
        // If there is but one client, return a message immediately
        if (clientList.length == 1) {
          // Return the port sent by the first page. It will communicate with itself through it.
          clientList[0].postMessage({ "messageType": "youHost", port: event.data.port }, [event.data.port]);
        } else {
          clientList.forEach(function(client) {
            // Send to all pages except for the sender
            if (client.id === event.source.id) {
              return;
            } else {
              client.postMessage(event.data, [event.data.port]);
            }
          });
        }
      })
      .catch(function(error) {
        console.log("Failing in service worker port relay:" + error);
      });
    return;
  }
  
});

