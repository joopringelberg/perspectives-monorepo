    // This function entirely encapsulates the loading and initialisation of the PDR.
  // We distinghuish to cases:
  // 1. The PDR is loaded in a SharedWorker. 
  // 2. The PDR is loaded in the host page. 

import { configurePDRproxy } from "perspectives-proxy";

  // As a result of calling this function, the two promises PDRHandler and PDRproxy are resolved.
export function startPDR()
{
  function isIOS() {
    return /iPhone|iPad|iPod/i.test(navigator.userAgent);
  }


  // Preferrably we run the PDR in a SharedWorker. However, even while IOS supports SharedWorkers, it has no debugging 
  // facilities for SharedWorkers. As a result, we cannot use SharedWorkers on IOS.
  // Turns out that as of februari 2025, it runs in a SharedWorker even on IOS.
  // if (typeof SharedWorker != "undefined" && !isIOS())
  if (true)
  {
    // The proxy function configurePDRProxy will create a SharedWorker. The SharedWorker will load the PDR, using 'importScripts'
    // (dyanmic import is not supported in SharedWorkers).
    // As a result, the PDR runs in the SharedWorker.
    configurePDRproxy("sharedWorkerChannel", {});
  }
  else
  {
    // The proxy function configurePDRProxy will load the PDR using dynamic import.
    // As a result, the PDR runs in the current page (that we call the 'host page').
    import( "perspectives-pageworker" ).then( pageWorker => configurePDRproxy( "hostPageChannel", { pageHostingPDRPort: pageWorker.default }));
  } 
}

