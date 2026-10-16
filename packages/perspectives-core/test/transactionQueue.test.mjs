// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import assert from "node:assert/strict";
import { test } from "node:test";
import { enqueue, newTransactionQueue } from "../src/core/transactionQueue.js";

function schedule(queue, user, name, order) {
  return enqueue(queue)(user)(() => order.push(name))();
}

test("a user overtakes waiting background work but never the active transaction", () => {
  const queue = newTransactionQueue();
  const order = [];
  const active = schedule(queue, false, "active", order);
  const peer = schedule(queue, false, "peer", order);
  const user = schedule(queue, true, "user", order);
  assert.deepEqual(order, ["active"]);
  active();
  assert.deepEqual(order, ["active", "user"]);
  user();
  assert.deepEqual(order, ["active", "user", "peer"]);
  peer();
});

test("both lanes retain FIFO order", () => {
  const queue = newTransactionQueue();
  const order = [];
  const active = schedule(queue, false, "active", order);
  const peer1 = schedule(queue, false, "peer1", order);
  const peer2 = schedule(queue, false, "peer2", order);
  const user1 = schedule(queue, true, "user1", order);
  const user2 = schedule(queue, true, "user2", order);
  active();
  user1();
  user2();
  peer1();
  peer2();
  assert.deepEqual(order, ["active", "user1", "user2", "peer1", "peer2"]);
});

test("background work progresses after at most four user admissions", () => {
  const queue = newTransactionQueue();
  const order = [];
  const active = schedule(queue, false, "active", order);
  const peer = schedule(queue, false, "peer", order);
  const users = Array.from({ length: 6 }, (_, i) => schedule(queue, true, `user${i}`, order));
  active();
  for (const release of users.slice(0, 4)) release();
  assert.deepEqual(order, ["active", "user0", "user1", "user2", "user3", "peer"]);
  peer();
  users[4]();
  users[5]();
  assert.deepEqual(order.slice(-2), ["user4", "user5"]);
});

test("cancelled waiters are removed and releases are idempotent", () => {
  const queue = newTransactionQueue();
  const order = [];
  const active = schedule(queue, false, "active", order);
  const cancelled = schedule(queue, true, "cancelled", order);
  const peer = schedule(queue, false, "peer", order);
  cancelled();
  cancelled();
  active();
  active();
  assert.deepEqual(order, ["active", "peer"]);
  peer();
  assert.equal(queue.active, null);
});

test("cancelling an admitted job advances the queue", () => {
  const queue = newTransactionQueue();
  const order = [];
  const cancel = schedule(queue, true, "user", order);
  const peer = schedule(queue, false, "peer", order);
  cancel();
  assert.deepEqual(order, ["user", "peer"]);
  peer();
});

test("queues are isolated per PDR and an idle queue resets the burst", () => {
  const first = newTransactionQueue();
  const second = newTransactionQueue();
  const order = [];
  const a = schedule(first, true, "a", order);
  const b = schedule(second, false, "b", order);
  assert.deepEqual(order, ["a", "b"]);
  a();
  b();
  assert.equal(first.userBurst, 0);
  assert.equal(second.userBurst, 0);
});
