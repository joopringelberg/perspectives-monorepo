import assert from "node:assert/strict";
import { test } from "node:test";
import { MessageChannel } from "node:worker_threads";
import { startTiming } from "../../perspectives-core/src/core/performance.js";
import { configurePDRproxy, getSharedWorkerConnection, PDR_SHARED_WORKER_NAME } from "../dist/perspectives-proxy.js";

function browserGlobals(t) {
  for (const key of ["document", "window", "SharedWorker", "pdrTraceTimings"]) {
    const descriptor = Object.getOwnPropertyDescriptor(globalThis, key);
    t.after(() => {
      if (descriptor) Object.defineProperty(globalThis, key, descriptor);
      else delete globalThis[key];
    });
  }
  globalThis.window = {};
  globalThis.document = {
    createElement: () => ({ style: {}, textContent: "", querySelector: () => ({ textContent: "" }) }),
    body: { style: { cursor: "default" }, appendChild() {} },
    head: { appendChild() {} },
    getElementById: () => ({ textContent: "" })
  };
}

test("a separately built page can join an explicitly named SharedWorker", t => {
  browserGlobals(t);
  const created = [];
  globalThis.SharedWorker = class {
    constructor(url, options) { created.push({ url: String(url), options }); this.port = {}; }
  };
  assert.equal(getSharedWorkerConnection(), undefined);
  configurePDRproxy("sharedWorkerChannel", {
    sharedWorkerUrl: "https://mycontexts.com/www/assets/perspectives-sharedworker",
    sharedWorkerName: "pdr-from-mycontexts"
  });
  assert.deepEqual(created, [{ url: "https://mycontexts.com/www/assets/perspectives-sharedworker", options: { type: "module", name: "pdr-from-mycontexts" } }]);
  assert.deepEqual(getSharedWorkerConnection(), { url: "https://mycontexts.com/www/assets/perspectives-sharedworker", name: "pdr-from-mycontexts" });
  assert.match(PDR_SHARED_WORKER_NAME, /^pdr-/);
});

for (const scenario of [
  { name: "startup tracing is disabled by default", traceTimings: undefined, initiallyEnabled: false },
  { name: "an explicit false leaves startup tracing disabled", traceTimings: false, initiallyEnabled: false },
  { name: "startup tracing is enabled before the first PDR startup call", traceTimings: true, initiallyEnabled: false },
  { name: "a joining page without the switch does not disable existing tracing", traceTimings: undefined, initiallyEnabled: true }
]) {
  test(scenario.name, { timeout: 5000 }, async t => {
    browserGlobals(t);
    if (scenario.initiallyEnabled) globalThis.pdrTraceTimings = true;
    else delete globalThis.pdrTraceTimings;
    t.mock.method(console, "info", () => {});
    const { handleClientRequest } = await import(
      `../../perspectives-core/src/core/proxy.js?startup-test=${encodeURIComponent(scenario.name)}`
    );
    const { port1, port2 } = new MessageChannel();
    t.after(() => { port1.close(); port2.close(); });
    const requests = [];
    let startupToken;
    const core = {
      runPDR: () => () => () => callback => () => {
        startupToken = startTiming("test.connection")("manager")();
        callback(true)();
      }
    };
    port2.onmessage = event => {
      requests.push(event.data.proxyRequest);
      handleClientRequest(core, { 1: port2 }, event);
    };
    const workerUrl = "https://mycontexts.com/www/assets/perspectives-sharedworker";
    globalThis.SharedWorker = class {
      constructor(url) {
        assert.equal(String(url), workerUrl);
        this.port = port1;
      }
    };
    configurePDRproxy("sharedWorkerChannel", {
      sharedWorkerUrl: workerUrl,
      traceTimings: scenario.traceTimings
    });
    port2.postMessage({ responseType: "WorkerResponse", serviceWorkerMessage: "channelId", channelId: 1000000 });
    assert.equal(await window.pdr.channel.runPDR("test", {}, {}), true);
    assert.deepEqual(requests, scenario.traceTimings === true
      ? ["enableTimingTracing", "runPDR"]
      : ["runPDR"]);
    assert.equal(startupToken !== null, scenario.initiallyEnabled || scenario.traceTimings === true);
    assert.equal(getSharedWorkerConnection().url, workerUrl);
  });
}
