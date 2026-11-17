// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import assert from "node:assert/strict";
import { test } from "node:test";
import { traceTimingImpl } from "../src/core/performance.js";
import {
  connectAndSubscribeImpl,
  deactivateImpl,
  sendImpl
} from "../src/core/amqpTransport/perspectives-stomp.js";

function transport(t, onConnection = () => {}) {
  t.mock.timers.enable({ apis: ["setTimeout"] });
  const published = [];
  const emitted = [];
  const messages = [];
  const receipts = new Map();
  const client = {
    connected: false,
    activate() {},
    deactivate() { this.connected = false; },
    subscribe() {},
    watchForReceipt(id, callback) { receipts.set(id, callback); },
    publish(frame) { published.push(frame); }
  };
  connectAndSubscribeImpl(client, {
    login: "test-login-secret", passcode: "test-password-secret",
    vhost: "/", topic: "peer", queueId: "queue-secret", traceTiming: traceTimingImpl
  }, message => message, () => {}, message => () => {
    emitted.push(message.body);
    messages.push(message);
    if (message.body === "connection") onConnection();
  });
  return {
    client, published, emitted, messages, receipts,
    send(id = "outbox-1") { sendImpl(client, "/topic/peer", id, "encrypted message"); },
    connect() {
      client.connected = true;
      client.onConnect();
      t.mock.timers.tick(0);
    },
    receipt(id = "outbox-1") {
      const callback = receipts.get(id);
      receipts.delete(id);
      callback();
    }
  };
}

test("connection replay and an earlier pending send publish only once", t => {
  const tx = transport(t, () => tx.send());
  tx.send();
  tx.send();
  tx.connect();
  t.mock.timers.tick(25);
  assert.equal(tx.published.length, 1);
  assert.deepEqual(tx.published[0], {
    destination: "/topic/peer",
    body: "encrypted message",
    headers: { receipt: "outbox-1" }
  });
  tx.receipt();
  assert.equal(tx.emitted.filter(body => body === "receipt:outbox-1").length, 1);
});

test("connected sends and stale outbox snapshots share one publication, even after receipt", t => {
  const tx = transport(t);
  tx.connect();
  tx.send();
  tx.send();
  tx.receipt();
  tx.send();
  assert.equal(tx.published.length, 1);
  tx.send("outbox-2");
  assert.equal(tx.published.length, 2);
});

for (const event of ["onDisconnect", "onWebSocketClose"]) {
  test(`${event} allows unacknowledged messages to be replayed on reconnection`, t => {
    const tx = transport(t, () => tx.send());
    tx.connect();
    const oldReceipt = tx.receipts.get("outbox-1");
    tx.client.connected = false;
    tx.client[event]();
    tx.connect();
    assert.equal(tx.published.length, 2);
    oldReceipt();
    assert.equal(tx.emitted.filter(body => body === "receipt:outbox-1").length, 0);
    tx.receipt();
    assert.equal(tx.emitted.filter(body => body === "receipt:outbox-1").length, 1);
  });
}

test("closing a connection cancels pending retry timers before replay", t => {
  const tx = transport(t, () => tx.send());
  tx.send();
  tx.client.onWebSocketClose();
  tx.connect();
  t.mock.timers.tick(25);
  assert.equal(tx.published.length, 1);
});

test("deactivation cancels pending sends", t => {
  const tx = transport(t);
  tx.send();
  deactivateImpl(tx.client);
  tx.client.connected = true;
  t.mock.timers.tick(25);
  assert.equal(tx.published.length, 0);
});

test("a missing underlying STOMP connection retries without a competing publication", t => {
  const tx = transport(t);
  tx.connect();
  let attempts = 0;
  tx.client.publish = frame => {
    if (++attempts === 1) throw new TypeError("There is no underlying STOMP connection");
    tx.published.push(frame);
  };
  tx.send();
  tx.send();
  t.mock.timers.tick(25);
  assert.equal(attempts, 2);
  assert.equal(tx.published.length, 1);
});

test("publication failure is surfaced and does not prevent a later send", t => {
  const tx = transport(t);
  tx.connect();
  const failure = new Error("Publish failed");
  tx.client.publish = () => { throw failure; };
  assert.throws(() => tx.send(), error => error === failure);
  const failedReceipt = tx.receipts.get("outbox-1");
  tx.client.publish = frame => tx.published.push(frame);
  tx.send();
  failedReceipt();
  assert.equal(tx.emitted.filter(body => body === "receipt:outbox-1").length, 0);
  tx.receipt();
  assert.equal(tx.published.length, 1);
  assert.equal(tx.emitted.filter(body => body === "receipt:outbox-1").length, 1);
});

test("exhausted connection retries report failure and release send ownership", t => {
  const tx = transport(t);
  const errors = t.mock.method(globalThis.console, "error", () => {});
  tx.send();
  for (let i = 0; i < 200; i++) t.mock.timers.tick(25);
  assert.equal(errors.mock.callCount(), 1);
  assert.equal(tx.published.length, 0);
  tx.client.connected = true;
  tx.send();
  assert.equal(tx.published.length, 1);
});

test("send ownership is isolated per client", t => {
  const first = transport(t);
  const secondPublished = [];
  const second = {
    connected: true,
    watchForReceipt() {},
    publish(frame) { secondPublished.push(frame); }
  };
  first.connect();
  first.send();
  sendImpl(second, "/topic/peer", "outbox-1", "encrypted message");
  assert.equal(first.published.length, 1);
  assert.equal(secondPublished.length, 1);
});

test("opt-in transport timing preserves delivery and logs IDs but no payloads or credentials", t => {
  const previous = globalThis.pdrTraceTimings;
  globalThis.pdrTraceTimings = true;
  t.after(() => {
    if (previous === undefined) delete globalThis.pdrTraceTimings;
    else globalThis.pdrTraceTimings = previous;
  });
  const output = t.mock.method(globalThis.console, "info", () => {});
  const tx = transport(t);
  tx.connect();
  let acknowledgements = 0;
  tx.client.forwardIncomingMessage({
    body: "private-message-body",
    headers: { receipt: "message-1", "message-id": "queue-secret-session-id" },
    ack() { acknowledgements++; }
  });
  const received = tx.messages.at(-1);
  assert.equal(received.messageId, "message-1");
  assert.equal(received.body, "private-message-body");
  assert.equal(received.pendingCount, 1);
  received.ack();
  assert.equal(acknowledgements, 1);
  assert.equal(received.markHandled(), 0);
  tx.send();
  tx.receipt();
  const lines = output.mock.calls.map(({ arguments: [line] }) => line);
  const events = lines.map(line => JSON.parse(line.slice("[PDR TIMING] ".length)));
  assert.deepEqual(events.map(({ stage, id }) => [stage, id]), [
    ["transport.connected", ""],
    ["transport.receive", "message-1"],
    ["transport.publish", "outbox-1"],
    ["transport.published", "outbox-1"],
    ["transport.receipt", "outbox-1"]
  ]);
  for (const secret of ["private-message-body", "encrypted message", "test-login-secret",
    "test-password-secret", "queue-secret"]) {
    assert.ok(lines.every(line => !line.includes(secret)));
  }
});

test("incoming messages without a receipt header get distinct safe local IDs", t => {
  const tx = transport(t);
  tx.client.forwardIncomingMessage({ body: "first", ack() {} });
  tx.client.forwardIncomingMessage({ body: "second", ack() {} });
  assert.deepEqual(tx.messages.map(({ messageId }) => messageId), ["incoming-1", "incoming-2"]);
});
