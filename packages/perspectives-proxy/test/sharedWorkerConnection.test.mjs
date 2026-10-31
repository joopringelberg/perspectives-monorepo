import assert from "node:assert/strict";
import { test } from "node:test";
import { configurePDRproxy, getSharedWorkerConnection, PDR_SHARED_WORKER_NAME } from "../dist/perspectives-proxy.js";

test("a separately built page can join an explicitly named SharedWorker", () => {
  const created = [];
  globalThis.document = {
    createElement: () => ({ style: {}, textContent: "", querySelector: () => ({ textContent: "" }) }),
    body: { style: { cursor: "default" }, appendChild() {} },
    head: { appendChild() {} },
    getElementById: () => ({ textContent: "" })
  };
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
