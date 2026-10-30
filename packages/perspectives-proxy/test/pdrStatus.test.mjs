import assert from "node:assert/strict";
import { test } from "node:test";
import { configurePDRproxy, PDRproxy } from "../dist/perspectives-proxy.js";

test("navbar progress is retained, nonblocking, cleared and unsubscribed independently of overlays", async () => {
  const elements = [];
  globalThis.document = {
    createElement: () => ({ style: {}, textContent: "", querySelector: () => ({ textContent: "" }) }),
    body: { style: { cursor: "default" }, appendChild: element => elements.push(element) },
    head: { appendChild() {} },
    getElementById: () => ({ textContent: "" })
  };
  const port = {};
  globalThis.SharedWorker = class {
    constructor() { this.port = port; }
  };
  configurePDRproxy("sharedWorkerChannel", {});
  const proxy = await PDRproxy;
  const send = (action, message, target) =>
    port.onmessage({ data: { responseType: "PDRMessage", action, message, target } });
  const flush = () => new Promise(resolve => setImmediate(resolve));

  send("push", "Working on 3 incoming messages", "navbar");
  const received = [];
  const unsubscribe = proxy.subscribeSystemMessages(message => received.push(message));
  assert.deepEqual(received, ["Working on 3 incoming messages"]);
  assert.equal(elements[0].style.display, "none");
  assert.equal(document.body.style.cursor, "default");

  send("push", "Installing model");
  await flush();
  assert.equal(elements[0].style.display, "flex");
  assert.equal(document.body.style.cursor, "wait");
  send("remove", "", "navbar");
  await flush();
  assert.deepEqual(received, ["Working on 3 incoming messages", ""]);
  assert.equal(elements[0].style.display, "flex");
  assert.equal(document.body.style.cursor, "wait");

  send("remove", "Installing model");
  await flush();
  assert.equal(elements[0].style.display, "none");
  assert.equal(document.body.style.cursor, "auto");
  unsubscribe();
  send("push", "Working on 1 incoming message", "navbar");
  assert.equal(received.length, 2);
  assert.equal(elements[0].style.display, "none");
});
