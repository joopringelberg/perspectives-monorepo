import assert from "node:assert/strict";
import { test } from "node:test";
import pageHostingPDRPort from "../src/perspectives-pageworker.js";

function createServiceWorkerContainer(controller = null) {
  const listeners = new Map();
  let currentController = controller;

  return {
    ready: Promise.resolve({ active: {} }),
    get controller() {
      return currentController;
    },
    addEventListener(type, listener) {
      listeners.set(type, listener);
    },
    removeEventListener(type, listener) {
      if (listeners.get(type) === listener) listeners.delete(type);
    },
    dispatchControllerChange() {
      listeners.get("controllerchange")?.();
    },
    setController(nextController) {
      currentController = nextController;
    }
  };
}

function installServiceWorkerContainer(serviceWorker) {
  const previousNavigator = Object.getOwnPropertyDescriptor(globalThis, "navigator");
  Object.defineProperty(globalThis, "navigator", {
    configurable: true,
    value: { serviceWorker }
  });
  return () => Object.defineProperty(globalThis, "navigator", previousNavigator);
}

test("relays the page port when an active service worker starts controlling the page", async () => {
  const serviceWorker = createServiceWorkerContainer();
  const restoreNavigator = installServiceWorkerContainer(serviceWorker);
  const relayed = [];
  const port = pageHostingPDRPort({});
  try {
    await new Promise(resolve => setImmediate(resolve));
    assert.equal(relayed.length, 0);

    serviceWorker.setController({
      postMessage(message, transfer) {
        relayed.push({ message, transfer });
      }
    });
    serviceWorker.dispatchControllerChange();
    serviceWorker.dispatchControllerChange();

    assert.equal(relayed.length, 1);
    assert.equal(relayed[0].message.messageType, "relayPort");
    assert.equal(relayed[0].transfer.length, 1);
  } finally {
    port.close();
    relayed[0]?.transfer[0].close();
    restoreNavigator();
  }
});

test("relays immediately when the page is already controlled", async () => {
  const relayed = [];
  const serviceWorker = createServiceWorkerContainer({
    postMessage(message, transfer) {
      relayed.push({ message, transfer });
    }
  });
  const restoreNavigator = installServiceWorkerContainer(serviceWorker);
  const port = pageHostingPDRPort({});
  try {
    await new Promise(resolve => setImmediate(resolve));
    serviceWorker.dispatchControllerChange();

    assert.equal(relayed.length, 1);
    assert.equal(relayed[0].message.messageType, "relayPort");
    assert.equal(relayed[0].transfer.length, 1);
  } finally {
    port.close();
    relayed[0]?.transfer[0].close();
    restoreNavigator();
  }
});
