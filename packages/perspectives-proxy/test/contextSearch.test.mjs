// SPDX-License-Identifier: GPL-3.0-or-later
// Run after pnpm run build: node --test test/contextSearch.test.mjs
import assert from "node:assert/strict";
import test from "node:test";
import { PerspectivesProxy } from "../dist/perspectives-proxy.js";

for (const [method, request, subject] of [
  ["getContextSearchModels", "GetContextSearchModels", ""],
  ["getContextSearchTypes", "GetContextSearchTypes", "model://example.org#installed"],
  ["getContextSearchInstances", "GetContextSearchInstances", "model://example.org#installed$context"],
]) {
  test(`${method} sends a one-shot request and preserves option identifiers and labels`, async () => {
    const proxy = new PerspectivesProxy({}, {});
    const options = [{ id: "context-instance-without-external-suffix", label: "Readable name" }];
    proxy.send = (message, receive) => {
      assert.deepEqual(message, { request, subject, onlyOnce: true });
      receive([JSON.stringify(options)]);
    };
    assert.deepEqual(await (method === "getContextSearchModels" ? proxy[method]() : proxy[method](subject)), options);
  });
}

test("context search handles empty responses and rejects malformed JSON", async () => {
  const proxy = new PerspectivesProxy({}, {});
  proxy.send = (_message, receive) => receive([]);
  assert.deepEqual(await proxy.getContextSearchModels(), []);
  proxy.send = (_message, receive) => receive(["{"]);
  await assert.rejects(proxy.getContextSearchModels(), SyntaxError);
});

test("context search propagates channel errors", async () => {
  const proxy = new PerspectivesProxy({}, {});
  const failure = new Error("Runtime unavailable");
  proxy.send = (_message, _receive, reject) => reject(failure);
  await assert.rejects(proxy.getContextSearchModels(), error => error === failure);
});
