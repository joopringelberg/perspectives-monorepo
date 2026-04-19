# perspectives-proxy

`perspectives-proxy` is the browser/client bridge to the Perspectives Distributed Runtime (PDR).
This document explains the runtime interface architecture between:

- `perspectives-proxy` (client side),
- `perspectives-sharedworker` (default browser worker host),
- `perspectives-pageworker` (Safari/host-page fallback),
- `perspectives-core` (the PDR itself).

It intentionally focuses on architecture and message flow, not on a full API reference.

## 1. Architecture overview

```text
Client page (MyContexts / perspectives-react)
  └─ PerspectivesProxy (packages/perspectives-proxy)
       └─ SharedWorkerChannel (MessagePort)
            ├─ SharedWorker path:
            │    SharedWorker (packages/perspectives-sharedworker)
            │      └─ pdr.handleClientRequest(...)
            │           └─ InternalChannel.send(...)
            │                └─ Perspectives.Api request producer/consumer
            │
            └─ Host-page fallback path (Safari):
                 pageHostingPDRPort(...) (packages/perspectives-pageworker)
                   └─ service worker relays MessagePort between tabs/pages
                      └─ host page runs pdr.handleClientRequest(...)
                         └─ InternalChannel.send(...)
```

The key interface boundary is `handleClientRequest` (`packages/perspectives-core/src/core/proxy.js`):
it receives client messages over `MessagePort`, handles worker/control messages itself, and forwards API calls into the PDR API pipeline via the internal coroutine channel.

## 2. How a connection is established

### 2.1 SharedWorker flow (default)

1. The client calls `configurePDRproxy("sharedWorkerChannel", {})`.
2. `perspectives-proxy` creates a `SharedWorker` and wraps `worker.port` in `SharedWorkerChannel`.
3. In `perspectives-sharedworker`, `self.onconnect` stores the page port and sends:
   `{ responseType: "WorkerResponse", serviceWorkerMessage: "channelId", channelId: 1000000 * pageIndex }`.
4. The worker wires `port.onmessage` to `pdr.handleClientRequest(...)`.
5. The worker passes `sendPDRStatusMessage` into the PDR through `receivePDRStatusMessageChannel`.

### 2.2 Host-page flow (Safari fallback)

1. The client calls `configurePDRproxy("hostPageChannel", { pageHostingPDRPort })`.
2. `pageHostingPDRPort` creates a `MessageChannel`; `port1` stays in the client tab (`SharedWorkerChannel`), `port2` is relayed via the browser service worker.
3. The first page receives `"youHost"` and becomes PDR host; additional pages send ports that are relayed to this host.
4. The host page registers incoming ports and wires each to `pdr.handleClientRequest(...)`, the same as the SharedWorker path.

## 3. Technical protocol

- **Transport**: browser Channel Messaging API (`MessagePort.postMessage`, `onmessage`).
- **Serialization**: structured clone objects (plain JS records; no function transfer).
- **Routing**: correlation id (`corrId`) embeds channel identity:
  - channel id = `1_000_000 * channelIndex`,
  - request id increments per channel,
  - effective `corrId = channelId + requestCounter`,
  - worker derives destination channel with `Math.floor(corrId / 1000000)`.

## 4. Message structure

At runtime, four message families are exchanged:

1. **API request** (proxy → worker → PDR API):
   - shape: `{ request, subject, predicate, object, corrId, ... }`
   - processed by `Perspectives.Api.dispatchOnRequest`.
2. **API response** (PDR API → proxy):
   - `{ responseType: "APIresult", corrId, result, warnings }`
   - `{ responseType: "APIerror", corrId, error, warnings }`
3. **Worker/control request-response** (proxy ↔ worker shell):
   - request shape: `{ proxyRequest: "...", ... }`
   - response shape: `{ responseType: "WorkerResponse", serviceWorkerMessage: "...", ... }`
   - examples: `channelId`, `runPDR`, `pdrStarted`, `createAccount`.
4. **PDR status push messages** (worker/PDR → proxy):
   - `{ responseType: "PDRMessage", action, message }`
   - used for UI status events (including integrity-choice flow).

## 5. API functions vs infrastructure functions

### API functions (business/runtime API)

- Exposed as methods on `PerspectivesProxy` (for example `getRol`, `getProperty`, `createContext`, ...).
- They produce **API request** messages (`request: ...`) and are handled in `Perspectives.Api`.

### Infrastructure / worker-control functions (non-API)

- `configurePDRproxy`, `SharedWorkerChannel`, `pageHostingPDRPort`.
- `handleClientRequest` switch branch on `proxyRequest` (e.g. `runPDR`, `createAccount`, `unsubscribe`, logging controls).
- `createRequestEmitter` / `retrieveRequestEmitter`: connect PDR coroutine producer/consumer to the internal channel.
- `receivePDRStatusMessageChannel` / `pdrStatusMessageChannel`: wire status push messages from PDR to all connected clients.

These functions are part of connection setup, lifecycle, and message plumbing; they are not part of the domain API exposed to application code.

## 6. Build

```bash
corepack pnpm -C packages/perspectives-proxy run build
corepack pnpm -C packages/perspectives-proxy run lint
```
