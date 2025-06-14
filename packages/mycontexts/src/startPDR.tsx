    // This function entirely encapsulates the loading and initialisation of the PDR.
  // We distinghuish to cases:
  // 1. The PDR is loaded in a SharedWorker. 
  // 2. The PDR is loaded in the host page. 

import { configurePDRproxy } from "perspectives-proxy";
import PouchDB from "pouchdb-browser";
import { getInstallationData } from "./installationData";

///////////////////////////////////////////////////////////////////////////////////////////////
//               options    DEBUGGING ONLY
// Use this function on the console of the host page to force the service worker to update.
///////////////////////////////////////////////////////////////////////////////////////////////
declare global {
  interface Window {
    forceUpdateServiceWorker?: () => void;
    debugServiceWorker?: () => void;
    purgeServiceWorkers?: () => Promise<boolean>;
    showDatabaseInfo?: () => void;
  }
}

// Add this to a development tools page or button handler
function forceUpdateServiceWorker() {
  if ('serviceWorker' in navigator) {
    navigator.serviceWorker.getRegistration().then(registration => {
      if (registration) {
        console.log('Forcing service worker update...');
        // registration.update();
        registration.update().then(() => {
          console.log('Service worker unregistered. Reloading page...');
          // Optionally, you can reload the page after unregistering
          // to ensure the new service worker is activated.
          // This will reload the page after 1 second
          // to give the unregister process some time.
        setTimeout(() => {
          window.location.reload();
        }, 1000);
      })
    };
  })
}}

// For development only
function debugServiceWorker() {
  const status = {
    controller: !!navigator.serviceWorker.controller,
    registration: null as any,
  };
  
  navigator.serviceWorker.getRegistration().then(reg => {
    if (reg) {
      status.registration = {
        installing: !!reg.installing,
        waiting: !!reg.waiting,
        active: !!reg.active,
        updateViaCache: reg.updateViaCache
      };
    }
    console.table(status);
  });
}

async function purgeServiceWorkers() {
  if ('serviceWorker' in navigator) {
    try {
      // First, unregister all service workers
      const registrations = await navigator.serviceWorker.getRegistrations();
      console.log(`Found ${registrations.length} service worker registrations`);
      
      const unregisterPromises = registrations.map(registration => {
        console.log('Unregistering service worker scope:', registration.scope);
        return registration.unregister();
      });
      
      await Promise.all(unregisterPromises);
      
      // Clear all caches
      const cacheNames = await caches.keys();
      await Promise.all(
        cacheNames.map(cacheName => {
          console.log(`Deleting cache: ${cacheName}`);
          return caches.delete(cacheName);
        })
      );
      
      console.log('Service workers unregistered and caches cleared');
      
      // Force reload with cache clearing
      console.log('Reloading page with cache clearing...');
      window.location.href = window.location.href.split('#')[0] + 
                             '?cache=' + Date.now();
      
      return true;
    } catch (error) {
      console.error('Service worker purge failed:', error);
      return false;
    }
  }
  return false;
}

function showDatabaseInfo() {
  function showSize(db: any) {
    db.info().then((info : any) => {
      console.log(`Database ${db.name}:`);
      console.log(`  Document count: ${info.doc_count}`);
    }).catch((error : any) => {
      console.error(`Error getting info for database ${db.name}:`, error);
    });
  }
  if ('indexedDB' in window)
    {
      getInstallationData().then( installationData => {
        const entitiesDbName = installationData.perspectivesUserId! + installationData.deviceName! + "_entities";
        const invertedQueriesDbName = installationData.perspectivesUserId! + installationData.deviceName! + "_invertedqueries";
        const modelsDbName = installationData.perspectivesUserId! + installationData.deviceName! + "_models";
        showSize(new PouchDB(entitiesDbName));
        showSize(new PouchDB(invertedQueriesDbName));
        showSize(new PouchDB(modelsDbName));
    });
  }
}


// Expose for dev console
if (import.meta.env.DEV) {
  window.forceUpdateServiceWorker = forceUpdateServiceWorker;
  window.debugServiceWorker = debugServiceWorker;
  window.purgeServiceWorkers = purgeServiceWorkers;
  window.showDatabaseInfo = showDatabaseInfo;
}///////////////////////////////////////////////////////////////////////////////////////////////


  // As a result of calling this function, the two promises PDRHandler and PDRproxy are resolved.
export function startPDR()
{
  function isIOS() {
    return /iPhone|iPad|iPod/i.test(navigator.userAgent);
  }

  function isSafari() {
    return /^((?!chrome|android).)*safari/i.test(navigator.userAgent);
  }

  function isAndroid() {
    return /Android/i.test(navigator.userAgent);
  }

  function isFirefox() {
    return /firefox|fxios/i.test(navigator.userAgent);
  }

  // Preferrably we run the PDR in a SharedWorker. However, even while IOS supports SharedWorkers, it has no debugging 
  // facilities for SharedWorkers. As a result, we cannot use SharedWorkers on IOS.
  // Turns out that as of februari 2025, it runs in a SharedWorker even on IOS.
  // if (typeof SharedWorker != "undefined" && !isIOS())
  if ((isSafari() || isIOS() || isAndroid()) && !isFirefox())
    {
      // The proxy function configurePDRProxy will load the PDR using dynamic import.
      // As a result, the PDR runs in the current page (that we call the 'host page').
      import( "perspectives-pageworker" )
        .then( pageWorker => 
          configurePDRproxy( "hostPageChannel", { pageHostingPDRPort: pageWorker.default }))
        .catch( e => console.error("Error loading perspectives-pageworker:", e));
    } 
    else
  {
    // The proxy function configurePDRProxy will create a SharedWorker. The SharedWorker will load the PDR, using 'importScripts'
    // (dyanmic import is not supported in SharedWorkers).
    // As a result, the PDR runs in the SharedWorker.
    configurePDRproxy("sharedWorkerChannel", {});
  }
  // For easier debugging, attach to window in development
  if (import.meta.env.DEV) {
    window.forceUpdateServiceWorker = forceUpdateServiceWorker;
  }

}

