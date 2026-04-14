// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later
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
// Full text of this license can be found in the LICENSE directory in the
// projects root.

// END LICENSE

// ---------------------------------------------------------------------------
// In-process AMQP stub for Layer 3 synchronisation tests
// ---------------------------------------------------------------------------
//
// This module provides a drop-in replacement for `perspectives-stomp.js` that
// routes messages in memory rather than over a real RabbitMQ/STOMP connection.
//
// Usage pattern (PureScript side):
//
//   bus  <- liftEffect createInProcessBus
//   let factoryA = makeStompClientFactory bus
//       factoryB = makeStompClientFactory bus
//   -- Install each factory in the respective PDR's PerspectivesState
//   -- before calling AMQP.IncomingPost.incomingPost.
//
// The two clients share the same `bus` object and therefore see each other's
// published messages.
//
// Interface compatibility
// -----------------------
// `connectAndSubscribeImpl` and `sendImpl` below match the signatures expected
// by `perspectives-stomp.purs` exactly:
//
//   connectAndSubscribeImpl(stompClient, params, emitStep, finishStep, emit)
//   sendImpl(stompClient, destination, receiptId, messageString)
//   unsubscribeImpl(stompClient, queueId)
//   createStompClientImpl(url)  -- provided as makeStompClientFactory
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// InProcessBus
// ---------------------------------------------------------------------------

/**
 * Create a new in-process message bus.
 *
 * The bus is a simple pub/sub registry:
 *   - `subscribe(topic, emitter)`  — register an emitter for a topic
 *   - `publish(topic, body)`       — deliver `body` to all registered emitters
 *
 * Both PDR instances receive the same bus object (passed via
 * `makeStompClientFactory`), so that a message published by PDR-A on the topic
 * of PDR-B is delivered directly to PDR-B's coroutine producer.
 */
export function createInProcessBus() {
  // Map<topic, Array<{queueId, emitter}>>
  const subscribers = new Map();

  return {
    subscribe(topic, queueId, emitter) {
      if (!subscribers.has(topic)) {
        subscribers.set(topic, []);
      }
      subscribers.get(topic).push({ queueId, emitter });
    },

    unsubscribe(queueId) {
      for (const [topic, subs] of subscribers.entries()) {
        const filtered = subs.filter(s => s.queueId !== queueId);
        subscribers.set(topic, filtered);
      }
    },

    /**
     * Publish `body` to all subscribers of `topic`.
     * `receiptId` is echoed back as a receipt to the publishing client so that
     * `stompClient.emitToPurescript({body: "receipt:" + receiptId})` fires and
     * the pending OutgoingTransaction document is deleted from the post DB.
     */
    publish(publishingClient, topic, receiptId, body) {
      const subs = subscribers.get(topic) || [];
      for (const { emitter } of subs) {
        // Deliver the message to the subscriber's coroutine emitter.
        emitter({ body, ack: noop });
      }
      // Echo back the receipt to the publishing client so that the
      // `watchForReceipt` callback fires and the post-DB doc is cleaned up.
      if (publishingClient && typeof publishingClient.emitToPurescript === "function") {
        publishingClient.emitToPurescript({ body: "receipt:" + receiptId });
      }
    }
  };
}

function noop() {}

// ---------------------------------------------------------------------------
// StompClient stub factory
// ---------------------------------------------------------------------------

/**
 * Return a factory `(url -> Effect StompClient)` that creates stub Stomp
 * clients backed by the given `bus`.
 *
 * Curried to match PureScript's `EffectFn1` calling convention: the PureScript
 * FFI wrapper calls `makeStompClientFactory(bus)(url)` to get the client.
 */
export function makeStompClientFactory(bus) {
  return function createStubClient(_url) {
    const client = {
      // Set by connectAndSubscribeImpl
      connectHeaders: {},
      debug: function() {},
      emitToPurescript: null,

      // Called by connectAndSubscribeImpl
      subscribe(destination, callback, headers) {
        const topic = stripPrefix(destination);
        const queueId = (headers && headers["x-queue-name"]) || topic;
        bus.subscribe(topic, queueId, callback);
        return { id: queueId, unsubscribe: () => bus.unsubscribe(queueId) };
      },

      // Called by sendImpl
      publish({ destination, body, headers }) {
        const topic = stripPrefix(destination);
        const receiptId = (headers && headers.receipt) || "";
        bus.publish(client, topic, receiptId, body);
      },

      // Called by connectAndSubscribeImpl
      watchForReceipt(receiptId, callback) {
        // Receipts are handled synchronously in bus.publish, so this is a no-op.
        // The receipt is emitted directly by bus.publish via emitToPurescript.
        void receiptId;
        void callback;
      },

      unsubscribe(queueId) {
        bus.unsubscribe(queueId);
      },

      // Called by connectAndSubscribeImpl to start the connection.
      // The stub connects immediately (synchronous), calling onConnect at once.
      activate() {
        if (typeof client.onConnect === "function") {
          client.onConnect();
        }
      },

      onConnect: null,
      onStompError: null,
      onDisconnect: null,
      onWebSocketClose: null,
      onUnhandledMessage: null,
    };
    return client;
  };
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Strip "/topic/" or "/queue/" prefix from a STOMP destination. */
function stripPrefix(destination) {
  if (destination.startsWith("/topic/")) return destination.slice(7);
  if (destination.startsWith("/queue/")) return destination.slice(7);
  return destination;
}

// ---------------------------------------------------------------------------
// FFI shims matching perspectives-stomp.purs foreign imports
// ---------------------------------------------------------------------------
//
// The PureScript stub module (stompStub.purs) imports these four functions
// under the same names as their counterparts in perspectives-stomp.purs.
// They are intentionally NOT exported from this file because the stub module
// provides its own foreign imports.  Instead, the *factory* functions above
// (createInProcessBus, makeStompClientFactory) are the public API.
