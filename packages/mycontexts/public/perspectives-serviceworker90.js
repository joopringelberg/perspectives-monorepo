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

const cacheName = "mycontexts" + "1.0.0" + 90;

// Define base path to handle potential base URL issues
const baseUrl = self.location.href.replace(/\/[^\/]*$/, '');

const appFiles = [
  "/index.html",
  "/manage.html",
  "/notification-worker.js",
  "/perspectives-pagedispatcher1.js",
  "/perspectives.webmanifest",
  "/vite.svg",
  "/AppImages/icons.json",
  "/assets/bootstrap-icons-BOrJxbIo.woff",
  "/assets/bootstrap-icons-BtvjY1KL.woff2",
  "/assets/browser-ponyfill-3vctGl9D.js",
  "/assets/browser-ponyfill-3vctGl9D.js.map",
  "/assets/favicon-Cq9lf0qF.png",
  "/assets/main-CMxx5bB4.js",
  "/assets/main-CMxx5bB4.js.map",
  "/assets/main-DgnOxtJM.css",
  "/assets/manage-B3zZouri.css",
  "/assets/manage-jAoza6SW.js",
  "/assets/manage-jAoza6SW.js.map",
  "/assets/mycontexts-Cq8gJUiJ.js",
  "/assets/mycontexts-Cq8gJUiJ.js.map",
  "/assets/mycontexts-shLx6kPH.js",
  "/assets/mycontexts-shLx6kPH.js.map",
  "/assets/perspectives-core-CRPcALjO.js",
  "/assets/perspectives-core-CRPcALjO.js.map",
  "/assets/perspectives-pageworker-n3GciPqm.js",
  "/assets/perspectives-pageworker-n3GciPqm.js.map",
  "/assets/perspectives-sharedworker-CbSQAxBl.js",
  "/assets/perspectives-sharedworker-CbSQAxBl.js.map",
  "/assets/startPDR-9bWlX152.js",
  "/assets/startPDR-9bWlX152.js.map",
  "/AppImages/ios/100.png",
  "/AppImages/ios/1024.png",
  "/AppImages/ios/114.png",
  "/AppImages/ios/120.png",
  "/AppImages/ios/128.png",
  "/AppImages/ios/144.png",
  "/AppImages/ios/152.png",
  "/AppImages/ios/16.png",
  "/AppImages/ios/167.png",
  "/AppImages/ios/180.png",
  "/AppImages/ios/192.png",
  "/AppImages/ios/20.png",
  "/AppImages/ios/256.png",
  "/AppImages/ios/29.png",
  "/AppImages/ios/32.png",
  "/AppImages/ios/40.png",
  "/AppImages/ios/50.png",
  "/AppImages/ios/512.png",
  "/AppImages/ios/57.png",
  "/AppImages/ios/58.png",
  "/AppImages/ios/60.png",
  "/AppImages/ios/64.png",
  "/AppImages/ios/72.png",
  "/AppImages/ios/76.png",
  "/AppImages/ios/80.png",
  "/AppImages/ios/87.png"
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
