# Layer 3 — In-Process Synchronisation Tests: Design Document

## Introduction

This document describes the design and implementation of **Layer 3** of the
Perspectives Core automated test suite: synchronisation tests that run two PDR
instances in the same Node.js process and exercise the full AMQP transport path
without requiring a real RabbitMQ broker.

It complements the high-level overview in
`design/nodejs-testing-architecture.md §3 (Layer 3)`.

---

## Goals

1. **Test the complete synchronisation path** — delta generation, signing, AMQP
   transport, and delta application — in a self-contained, fully automated way.
2. **No external services required** — no RabbitMQ, no CouchDB server; all state
   is kept in memory (PouchDB with `pouchdb-adapter-memory`).
3. **Simple test authoring** — a test author writes PureScript that calls
   `setProperty` on PDR-A and then `getProperty` on PDR-B, without any
   knowledge of the underlying transport mechanics.
4. **Minimal production-code changes** — a single new field in `PerspectivesState`
   is the only change to the production code path.

---

## Architecture

### The synchronisation path under test

```
PDR-A (userA)
  setProperty rolId propType [Value "hello"]
    └─► delta generation (Perspectives.Deltas)
          └─► sign delta
                └─► distributeTransaction
                      └─► sendTransactieToUserUsingAMQP(userB-topic, tfp)
                            └─► StompClient.publish("/topic/userB", ...)
                                  │
                                  │  InProcessBus (in-memory)
                                  ▼
PDR-B (userB)
  incomingPost (background fiber)
    └─► messageProducer emits TransactionForPeer
          └─► executeTransaction tfp
                └─► setProperty applied locally in PDR-B's PouchDB
```

After synchronisation, `PDR-B.getProperty(rolId, propType)` returns
`Just (Value "hello")`.

### Key components

| Component | Location | Role |
|---|---|---|
| `InProcessBus` | `src/core/amqpTransport/stompStub.js` | Shared in-memory pub/sub bus |
| `makeStompClientFactory` | `src/core/amqpTransport/stompStub.js` | Returns a stub client factory |
| `Perspectives.AMQP.Stomp.Stub` | `src/core/amqpTransport/stompStub.purs` | PureScript FFI bindings |
| `stompClientFactory` field | `src/core/coreTypes.purs` | Injectable factory in PDR state |
| `setStompClientFactory` | `src/core/perspectivesState.purs` | Installs the factory |
| `incomingPost` | `src/core/amqpTransport/incomingPost.purs` | Uses factory (not hard-coded) |
| `Test.Sync.TestUtils` | `test/sync/TestUtils.purs` | Two-PDR test scaffold |
| `Test.Sync.SetPropertyGetProperty` | `test/sync/SetPropertyGetProperty.purs` | First sync test |
| `Test.Layer3` | `test/Layer3.purs` | Entry point for `pnpm run test:layer3` |

---

## Production code changes

### 1 — Injectable Stomp client factory (`coreTypes.purs`)

A single new field is added to the `PerspectivesExtraState` row type:

```purescript
-- In type PerspectivesExtraState:
, stompClientFactory :: String -> Effect StompClient
```

The field holds a function that takes a broker URL and returns an `Effect`
producing a `StompClient`.  In production the field defaults to the real
`createStompClient` (imported from `perspectives-stomp.purs`); in tests it is
replaced with the in-process stub.

### 2 — Default value (`perspectivesState.purs`)

`newPerspectivesState` initialises the field with the real factory:

```purescript
, stompClientFactory: createStompClient
```

Two new helpers are added:

```purescript
getStompClientFactory :: MonadPerspectives (String -> Effect StompClient)
getStompClientFactory = gets _.stompClientFactory

setStompClientFactory :: (String -> Effect StompClient) -> MonadPerspectives Unit
setStompClientFactory f = modify \s -> s { stompClientFactory = f }
```

### 3 — Use factory in `incomingPost.purs`

The hard-coded call to `createStompClient` is replaced with a lookup of the
factory:

```purescript
-- Before:
stpClient <- liftEffect $ createStompClient (url)

-- After:
factory   <- getStompClientFactory
stpClient <- liftEffect $ factory url
```

This is the **only change** to the production code path for the real AMQP
transport.  The `createStompClient` import is removed from this module.

---

## The in-process message bus

### Design

The bus (`stompStub.js`) is a JavaScript object with three methods:

| Method | Signature | Description |
|---|---|---|
| `subscribe(topic, queueId, emitter)` | — | Register a callback for a topic |
| `unsubscribe(queueId)` | — | Remove a subscriber |
| `publish(client, topic, receiptId, body)` | — | Deliver message; echo receipt |

The bus is created once per test with `createInProcessBus()` and shared between
the two PDR instances via `makeStompClientFactory(bus)`.

### Stub StompClient behaviour

Each call to `makeStompClientFactory(bus)(url)` returns an object that mimics
the real Stomp client interface used by `perspectives-stomp.purs`:

| Operation | Real client | Stub |
|---|---|---|
| `activate()` | Opens WebSocket to RabbitMQ; calls `onConnect` asynchronously | Calls `onConnect` **synchronously** (connection is instant) |
| `subscribe(destination, callback, headers)` | Subscribes to STOMP destination over WebSocket | Registers `callback` in the `InProcessBus` for the topic |
| `publish({destination, body, headers})` | Sends frame over WebSocket | Calls `bus.publish(...)`, which invokes all registered callbacks |
| `watchForReceipt(receiptId, callback)` | Calls `callback` when broker sends receipt frame | Receipt is echoed back synchronously by `bus.publish` via `emitToPurescript` |
| `unsubscribe(queueId)` | Sends UNSUBSCRIBE frame | Removes from bus registry |
| `emitToPurescript(message)` | Set by `connectAndSubscribeImpl`; called on each incoming message | Same: set by `connectAndSubscribeImpl`, called by bus on delivery |

### Message flow in detail

1. **PDR-A** calls `setProperty`.
2. The transaction is distributed: `sendTransactieToUserUsingAMQP(userB-topic, tfp)`.
3. `sendToTopic(stompClient, userB-topic, receiptId, writeJSON tfp)` is called on
   PDR-A's stub client.
4. The stub's `publish` delegates to the bus: `bus.publish(clientA, "userB", receiptId, json)`.
5. The bus finds PDR-B's registered subscriber for topic `"userB"` and calls
   `emitter({body: json, ack: noop})`.
6. PDR-B's `emitToPurescript` is invoked.  This triggers the PureScript
   coroutine emitter that was set up in `connectAndSubscribeImpl`.
7. The `messageProducer` coroutine yields a `Right {body: tfp, ack: noop}`.
8. PDR-B's `transactionConsumer` calls `executeTransaction tfp`.
9. The bus also echoes `{body: "receipt:<receiptId>"}` back to PDR-A's stub
   client, which triggers `stompClient.emitToPurescript({body: "receipt:..."})`.
   This causes PDR-A's coroutine to yield a `Left (TypeMismatch "receipt" receiptId)`,
   and the `transactionConsumer` deletes the corresponding post-DB document.

### Synchronous vs asynchronous delivery

The bus delivers messages **synchronously within the same JavaScript turn**.
However, `executeTransaction` is an `Aff` computation that runs in the
PureScript async scheduler.  Tests must therefore `delay` a short time after
the mutation before asserting the result on the receiving side.  A delay of
`100–300 ms` is sufficient for in-process delivery.

---

## Test infrastructure

### `Test.Sync.TestUtils`

Provides:

```purescript
-- | Construct a PerspectivesState AVar for one PDR instance,
-- | with the in-process stub factory installed.
setupPDRState
  :: String             -- userName
  -> String             -- perspectivesUser
  -> String             -- systemId
  -> InProcessBus
  -> Aff (AVar PerspectivesState)

-- | Run a MonadPerspectives action in the PDR identified by stateAVar.
runInPDR :: forall a. AVar PerspectivesState -> MonadPerspectives a -> Aff a

-- | Run a test action with two PDR instances sharing an in-process bus.
withTwoPDRs
  :: (AVar PerspectivesState -> AVar PerspectivesState -> Aff Unit)
  -> Aff Unit
```

### Test anatomy

A complete Layer 3 test looks like this:

```purescript
test "property set on PDR-A is visible on PDR-B" do
  withTwoPDRs \stateA stateB -> do

    -- 0. Prerequisite setup (load model:System, set AMQP connectivity flag,
    --    start incomingPost background fibers).
    fiberA <- liftAff $ forkAff $ runInPDR stateA
      (withSystem do
        setConnectedToAMQPBroker true
        incomingPost brokerUrl)
    fiberB <- liftAff $ forkAff $ runInPDR stateB
      (withSystem do
        setConnectedToAMQPBroker true
        incomingPost brokerUrl)

    -- 1. Apply mutation in PDR-A.
    runInPDR stateA do
      runMonadPerspectivesTransaction' false authoringRole do
        setProperty [sharedRoleId] propType Nothing [Value "hello"]

    -- 2. Wait for delivery and application in PDR-B.
    delay (Milliseconds 200.0)

    -- 3. Assert the value is visible in PDR-B.
    mval <- runInPDR stateB (sharedRoleId ##> getProperty propType)
    assert "value should be 'hello'" (mval == Just (Value "hello"))

    -- 4. Tear down background fibers.
    killFiber (error "stop") fiberA
    killFiber (error "stop") fiberB
```

---

## Prerequisites and enablement guide

The test suites in `Test.Sync.SetPropertyGetProperty` are marked `suiteSkip`
until the following prerequisites are in place.  Check them off in order:

### Step 1 — Layer 2: in-memory PouchDB

The PouchDB adapter must be backed by `pouchdb-adapter-memory` in the Node.js
test build so that no file I/O or real CouchDB is required.

* Write `src/persistenceAPI.node.js` (or equivalent alias in
  `rollup.node.config.js`) that registers `pouchdb-adapter-memory` and sets
  `adapter: "memory"` on all PouchDB constructors.
* Verify that `pnpm run test:layer2` passes.

### Step 2 — System model availability

`incomingPost` and `distributeTransaction` rely on `model:System` being loaded.

* Add a fixture that loads `model:System` from a local file (not from a remote
  CouchDB).  The `Perspectives.DomeinFile` serialisation format is JSON; a
  compiled version can be embedded as a test fixture file.
* Alternatively, run the ARC compiler for `src/model/perspectivesSysteem.arc`
  during the test setup.

### Step 3 — Shared role instance seeding

Both PDR-A and PDR-B must have the same role instance in their databases for the
`setProperty → getProperty` test to make sense.

* Option A: Seed both databases from the same JSON fixture before the test.
* Option B: Create the role in PDR-A and let it propagate to PDR-B via the stub
  transport (a "create context" synchronisation test that runs first).

### Step 4 — Enable AMQP connectivity flag

`distributeTransaction` checks `connectedToAMQPBroker` on the external role of
the system context.  Set this flag to `"true"` in both PDR instances during
test setup:

```purescript
void $ runMonadPerspectivesTransaction' false sysUser
  (setProperty
    [RoleInstance $ buitenRol systemId]
    (EnumeratedPropertyType DEP.connectedToAMQPBroker)
    Nothing
    [Value "true"])
```

### Step 5 — Start `incomingPost` fibers

Fork `AMQP.IncomingPost.incomingPost` in a background fiber for each PDR
instance.  The stub `activate()` call is synchronous, so the subscription is
ready before the first mutation fires.

### Step 6 — Enable the test suites

Once all steps above are complete:

1. Change `suiteSkip` → `suite` in
   `test/sync/SetPropertyGetProperty.purs`.
2. Run `pnpm run test:layer3` and verify the tests pass.
3. Add the `test:layer3` step to the CI workflow alongside `test:layer1`.

---

## File inventory

| File | Status | Description |
|---|---|---|
| `src/core/coreTypes.purs` | Modified | Added `stompClientFactory` field to `PerspectivesExtraState` |
| `src/core/perspectivesState.purs` | Modified | Default factory; `getStompClientFactory`; `setStompClientFactory` |
| `src/core/amqpTransport/incomingPost.purs` | Modified | Uses factory from state |
| `src/core/amqpTransport/stompStub.js` | New | In-process message bus (JavaScript FFI) |
| `src/core/amqpTransport/stompStub.purs` | New | PureScript FFI bindings for the stub |
| `test/sync/TestUtils.purs` | New | Two-PDR test scaffold |
| `test/sync/SetPropertyGetProperty.purs` | New | First sync test (skipped until prerequisites land) |
| `test/Layer3.purs` | Modified | References `SetPropertyGetProperty.theSuite` |
| `design/layer3-sync-tests.md` | New | This document |

---

## FAQ

**Q: Why add a factory field to `PerspectivesState` instead of using a module-level
flag or a separate "test" Rollup build?**

A factory field keeps the injection point at the type level: it is visible in
`PerspectivesExtraState`, searchable in the codebase, and does not require
conditional compilation or separate build targets.  The overhead is a single
function-pointer field in the state record.

**Q: Why is `activate()` synchronous in the stub?**

The real Stomp client calls `onConnect` asynchronously after the WebSocket
handshake.  In the stub there is no handshake, so calling `onConnect`
immediately is correct.  The PureScript coroutine receives the `"connection"`
signal in the same tick, making test setup simpler (no need to wait for the
"connected" event before issuing mutations).

**Q: What happens if PDR-B's `incomingPost` is not running?**

`distributeTransaction` calls `sendTransactieToUserUsingAMQP` which calls
`sendToTopic`.  The stub's `publish` method iterates over registered subscribers
for the topic.  If PDR-B has not called `connectAndSubscribeImpl` yet (because
`incomingPost` has not started), there are no subscribers and the message is
silently dropped.  This is the same behaviour as the real AMQP transport when
the peer is offline.

**Q: Can I run more than two PDR instances?**

Yes — call `setupPDRState` once per instance, passing the same `bus`.  Messages
are routed by **topic** (= the unschemed PerspectivesUser identifier), so each
instance subscribes to its own topic.  There is no limit on the number of
instances sharing one bus.
