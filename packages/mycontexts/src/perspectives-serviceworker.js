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

const cacheName = "mycontexts" + __MYCONTEXTS_VERSION__ + __BUILD__;

// Define base path to handle potential base URL issues
const baseUrl = self.location.href.replace(/\/[^\/]*$/, '');

const appFiles = [
  // We'll update this list during build, but add key files manually
  "/index.html",
  "/manage.html"
];

const macIcons = ["512.png", "256.png", "128.png", "32.png", "16.png"].map(icon => "/appimages/ios/" + icon);

// Join app files and icons
const toBeCached = appFiles.concat(macIcons);

self.addEventListener("install", (e) => {
  console.log("[Service Worker] Install");
  self.skipWaiting();
  
  e.waitUntil(
    (async () => {
      const cache = await caches.open(cacheName);
      console.log("[Service Worker] Caching all mycontext sources in cache: " + cacheName);
      
      // Cache files individually to better handle errors
      const cachePromises = toBeCached.map(async (url) => {
        try {
          // Check if URL is absolute or needs the base
          const resourceUrl = url.startsWith('http') ? url : baseUrl + url;
          console.log(`[Service Worker] Attempting to cache: ${resourceUrl}`);
          
          // First check if resource exists before caching
          const checkResponse = await fetch(resourceUrl, { method: 'HEAD' })
            .catch(err => {
              console.log(`[Service Worker] Resource not available: ${resourceUrl}`, err);
              return null;
            });
            
          if (checkResponse && checkResponse.ok) {
            await cache.add(resourceUrl);
            console.log(`[Service Worker] Successfully cached: ${resourceUrl}`);
          } else {
            console.log(`[Service Worker] Skipping unavailable resource: ${resourceUrl}`);
          }
        } catch (error) {
          console.error(`[Service Worker] Error caching ${url}:`, error);
          // Continue despite errors - don't block installation
        }
      });
      
      // Wait for all cache operations to complete
      await Promise.allSettled(cachePromises);
      console.log("[Service Worker] Caching complete");
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
          console.log(`[Service Worker] Taking resource ${e.request.url} from cache: ${cacheName}.`);
          return r;
        }
        console.log( `[Service Worker] ${e.request.url} should have been cached but is not. Fetching and caching it in cache ${cacheName}.`);
        const response = await fetch(e.request);
        const cache = await caches.open(cacheName);
        cache.put(e.request, response.clone());
        return response;
        // return await fetch(e.request);
      }
      else
      {
        // console.log( `[Service Worker] Passing through this request without caching: ${e.request.url}`);
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
