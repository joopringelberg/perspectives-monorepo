// The editor never hosts the PDR itself (that would start a second runtime); it only joins MyContexts' SharedWorker.
// perspectives-proxy's dynamic import of perspectives-core (page-hosted mode) is aliased to this module.
throw new Error("The ARC editor does not host a PDR; it only connects to the MyContexts SharedWorker.");
