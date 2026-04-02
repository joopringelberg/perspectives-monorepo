# Architecture for Testing and Mobile

This document responds to the architectural questions raised in GitHub issue *"Architectuur voor testen en mobiel"*.  It covers three topics:

1. **Converting MyContexts to a native mobile (and desktop) app.**
2. **Running the PDR as a Node.js application.**
3. **Automated testing strategy.**

---

## Background

MyContexts is a browser-based Progressive Web App (PWA) whose server-side component (the Perspectives Distributed Runtime, PDR) runs in a browser SharedWorker.  Data is stored in IndexedDB via PouchDB and peers synchronise by exchanging signed delta-sets over RabbitMQ/Stomp.

---

## Part 1 — Native Mobile (and Desktop) App

### Problem

As soon as a mobile browser moves to the background, the OS suspends the JavaScript runtime.  The Stomp connection to RabbitMQ drops and no incoming transactions are processed until the user returns to the app.  Notifications generated inside the PDR itself are therefore largely useless: the PDR is not running.

### Option A — Capacitor (recommended for mobile)

[Capacitor](https://capacitorjs.com/) wraps a web app in a native iOS/Android shell while keeping the full web stack intact.  Key properties:

| Property | Detail |
|---|---|
| Build input | The existing Vite/React build output (`dist/`) |
| Native bridge | Capacitor plugins for push notifications, background fetch, file access |
| Web runtime | WKWebView (iOS), Chrome WebView (Android) |
| SharedWorker | Supported in WKWebView as of iOS 16 and in Chrome WebView |
| PDR code change | Minimal: add Capacitor plugins, wire push notifications |

**Background synchronisation strategy with Capacitor:**

1. A lightweight native service (Capacitor Background Runner or a native plugin) maintains a persistent connection to an intermediary relay server (or a Firebase Cloud Messaging / APNs gateway).
2. When a new Perspectives transaction arrives at RabbitMQ, the relay server sends a silent push notification to the device.
3. The push wakes the native shell, which launches a background fetch task.
4. The background fetch starts the SharedWorker (or a page worker), which in turn starts the PDR.
5. The PDR reconnects to RabbitMQ, pulls pending transactions, and processes them.
6. A local notification is shown to the user if relevant.

This approach keeps all Perspectives logic inside the existing TypeScript/PureScript codebase and adds only a thin native layer.

### Option B — Tauri (recommended for desktop)

[Tauri](https://tauri.app/) packages a web app as a native desktop application using the OS-native WebView.  Compared with Electron:

| | Tauri | Electron |
|---|---|---|
| Bundle size | ~3–10 MB | ~100+ MB |
| Memory footprint | Low (uses system WebView) | High (ships Chromium) |
| Security model | Allowlist-based Rust backend | Node.js integration (broader attack surface) |
| Maturity | Stable v1 / active v2 | Very mature |
| Mobile support | Tauri v2 supports iOS & Android | No mobile support |

For the PDR specifically, Tauri v2 supports both desktop and mobile from a single codebase, making it a strong candidate if a single-target native build is desired.

**PDR placement with Tauri:**

The PDR can continue to run as a SharedWorker inside the embedded WebView.  Alternatively, for desktop, the PDR can be compiled to a Node.js application (see Part 2) and run as a Tauri sidecar process — a subprocess managed by Tauri that the WebView communicates with over a local IPC channel or TCP socket.

### Option C — Electron (legacy / fallback)

Electron remains the most mature option with the largest ecosystem.  An early version of MyContexts already ran on Electron.  The PDR can run as a Node.js process (see Part 2) inside the Electron main process, communicating with the renderer via IPC (`ipcMain` / `ipcRenderer`).

The main drawbacks are the large bundle size and the absence of native mobile support.

### Summary recommendation

| Platform | Recommended tool | PDR location |
|---|---|---|
| iOS / Android | Capacitor | SharedWorker in WebView; background tasks via native plugin |
| Desktop (macOS / Windows / Linux) | Tauri v2 | SharedWorker in WebView or Node.js sidecar |
| Cross-platform (incl. mobile) | Tauri v2 | SharedWorker in WebView |
| Desktop (if Electron is already in use) | Electron | Node.js main process |

---

## Part 2 — PDR as a Node.js Application

### Feasibility

The PDR is written in PureScript and compiled to ES modules.  Most of the PureScript code is platform-agnostic.  The platform-specific surface is small:

| Area | Browser code | Node.js equivalent |
|---|---|---|
| PouchDB adapter | `pouchdb-browser` (IndexedDB) | `pouchdb` (LevelDB via `leveldown`) |
| Fetch / CORS | `fetch` with `mode: "cors"` | Plain `node-fetch`; no CORS restrictions |
| Offline detection | `navigator.onLine` | Always-online assumption or `dns.lookup` probe |
| File / Blob API | `new File(...)`, `blob.text()` | `Buffer` API |
| Base64 encoding | `btoa` | `Buffer.from(...).toString('base64')` |
| AffJax / XHR | `affjax-web` (uses XHR2) | Use `affjax-node` or switch `affjax-web` to use `node-fetch` |
| IndexedDB key-value store (`idb-keyval`) | Browser IndexedDB | Replace with `keyv` or a simple JSON file |
| SharedWorker shell | `perspectives-sharedworker` | Not needed; use InternalChannel directly |

### Concrete changes required

#### 1. PouchDB

A Node.js-compatible drop-in replacement is already prepared at:

```
packages/perspectives-core/src/core/persistence/persistenceAPI.node.js
```

This file imports `pouchdb` (LevelDB backend) instead of `pouchdb-browser`, removes the CORS fetch wrapper, and replaces browser-only APIs.  To use it, the bundler/test runner must alias the `persistenceAPI.js` import to `persistenceAPI.node.js`.

Install the Node.js PouchDB package once:

```bash
pnpm add --save-dev pouchdb
```

#### 2. AffJax

`affjax-web` depends on the browser XHR2 API.  For Node.js, switch to the [`affjax-node`](https://pursuit.purescript.org/packages/purescript-affjax-node) package, which uses `node-fetch`.

In `spago.yaml`, replace `affjax-web` with `affjax-node` for the test build target, or create a dedicated `spago.test.yaml` / `spago.node.yaml` that overrides this dependency.

The modules that import `Affjax.Web` must conditionally import `Affjax.Node` (or use a thin compatibility shim).  The required API surface is:
- `Affjax.request` / `Affjax.get` / `Affjax.post`
- `Affjax.ResponseFormat.string`

#### 3. `idb-keyval` (IndexedDB key-value store)

`idb-keyval` is used in `Main.purs` for storing small installation parameters.  The Node.js equivalent is either:
- A flat JSON file managed by the `node-fs` package (already a spago dependency).
- The [`keyv`](https://github.com/jaredwray/keyv) library with a SQLite or memory adapter.

For testing purposes a simple in-memory implementation is sufficient.

#### 4. CORS

In the browser, all requests to `perspectives.domains` must use `mode: "cors"`.  In Node.js there are no CORS restrictions.  The `captureFetch` wrapper in `persistenceAPI.js` must be removed for Node.js — this is already done in `persistenceAPI.node.js`.

When the remote endpoint uses a self-signed certificate (local development), disable Node.js certificate checking:

```bash
export NODE_TLS_REJECT_UNAUTHORIZED="0"
# or, safer: point at the mkcert root CA
export NODE_EXTRA_CA_CERTS="$(mkcert -CAROOT)/rootCA.pem"
```

#### 5. Entry point

For running as a Node.js application (as opposed to just running tests), a minimal entry point is needed that:

1. Imports the PDR (`Main.purs` compiled output).
2. Sets up an `InternalChannel` (already available in `proxy.js`).
3. Exposes the `PerspectivesProxy` API over a local socket or via process stdin/stdout for communication with a GUI process.

A Tauri sidecar or an Electron main-process module would be the natural consumer of such an entry point.

### Database alternatives to IndexedDB

For a Node.js PDR the recommended storage backends are:

| Option | Pros | Cons |
|---|---|---|
| LevelDB (default `pouchdb`, via `classic-level`) | Zero config, ships with pouchdb | Not portable across machines |
| SQLite via `pouchdb-adapter-node-websql` | Single file, easy to back up | Requires native module |
| In-memory PouchDB (`pouchdb-adapter-memory`) | Fast, no persistence overhead | Data lost on restart — tests only |

For automated testing, the in-memory adapter is ideal because it requires no setup and leaves no state behind between test runs:

```bash
pnpm add --save-dev pouchdb-adapter-memory
```

```javascript
// In the test harness bootstrap:
import PouchDB from "pouchdb";
import PouchDBAdapterMemory from "pouchdb-adapter-memory";
PouchDB.plugin(PouchDBAdapterMemory);
// Then use `{ adapter: "memory" }` when creating databases.
```

---

## Part 3 — Automated Testing Strategy

### Current state

The test suite lives in `packages/perspectives-core/test/`.  It contains PureScript test modules for:
- ARC parser (phases 1, 2, 3)
- Query inversion
- Context and role data operations
- Synchronisation (`handleTransaction`)
- RabbitMQ transport
- ADT type comparisons

Most suites are commented out in `test/Main.purs` and the tests run end-to-end in the browser only.  There is no CI-driven automated run.

### Recommended layers

#### Layer 1 — Pure PureScript unit tests (no PDR startup, no database)

The following test suites require **no external dependencies** and can run with a plain `spago test` invocation in the Node.js target:

- `Test.Representation.ADT` — algebraic data type operations
- `Test.Perspectives.Representation.ADT.DisjunctiveNormalForm` — normal form rewriting
- `Test.Perspectives.Representation.ADT2` — advanced ADT
- `Test.Perspectives.Representation.ADT.SpecialisesADT` — type specialisation
- `Test.Parsing.Arc` — ARC parser phases 1–3
- `Test.Parsing.Arc.Expression` — expression parser
- `Test.Query.DescriptionCompiler` — query description compiler
- `Test.Query.Inversion` — inverted query construction

These should be the first to be enabled and run in CI.  Add a CI workflow step:

```yaml
- name: Run pure PureScript tests
  working-directory: packages/perspectives-core
  run: pnpm exec spago test --main Test.Main
```

#### Layer 2 — Integration tests with in-memory PouchDB

With the Node.js-compatible `persistenceAPI.node.js` and the `pouchdb-adapter-memory` plugin, the following suites can run without a real CouchDB or RabbitMQ instance:

- `Test.LoadArc` — load and compile ARC models
- `Test.ContextAndRole` — context and role CRUD
- `Test.Queries` — query evaluation against in-memory data
- `Test.Sync.HandleTransaction` — transaction application

These require:
1. `pouchdb` + `pouchdb-adapter-memory` installed.
2. The `persistenceAPI.js` import aliased to `persistenceAPI.node.js` in the rollup/esbuild test bundle.
3. `idb-keyval` stubbed with a simple in-memory implementation.
4. `affjax-node` replacing `affjax-web`.

#### Layer 3 — Synchronisation tests with a stubbed AMQP transport

Synchronisation is the most important area for automated testing (as stated in the issue) and also the most complex.

The strategy:

1. **Stub the RabbitMQ connection** by replacing the Stomp client with an in-process message bus.  The stub implements the same interface as `@stomp/stompjs` but routes messages in memory.
2. **Run two PDR instances** in the same Node.js process, each with its own in-memory PouchDB.
3. **Apply a mutation** via the API of PDR-A.
4. **Assert** that PDR-B receives the expected `TransactionForPeer` and that its database reflects the change.

This covers the full synchronisation path — delta generation, signing, transmission, and application — without requiring a real broker.

The stub can be injected by providing a factory function for the Stomp client in `PerspectivesState`.  This requires a small, targeted extension to the PDR state record to allow the AMQP client factory to be overridden at startup.

#### Layer 4 — End-to-end browser tests (existing, keep as is)

The current browser-based tests remain useful for regression testing of the full stack including the SharedWorker, service worker, and UI.  These are not replaced by the automated layers above.

### CI integration

A minimal GitHub Actions workflow for layers 1 and 2:

```yaml
name: PDR unit and integration tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: '22'
      - name: Install pnpm
        uses: pnpm/action-setup@v4
        with:
          version: '10.6.4'
      - name: Install dependencies
        run: pnpm install
      - name: Build PureScript packages
        working-directory: packages/perspectives-core
        run: pnpm exec spago build
      - name: Run tests
        working-directory: packages/perspectives-core
        run: pnpm exec spago test --main Test.Main
```

### Test priorities

Based on the issue, the three highest-priority test areas are:

| Priority | Area | Layer | Key test modules |
|---|---|---|---|
| 1 | Synchronisation | 3 (AMQP stub) | `Test.Sync.HandleTransaction`, `Test.Sync.Channel` |
| 2 | ARC parser | 1 (pure) | `Test.Parsing.Arc`, `Test.Parsing.Arc.PhaseTwo`, `Test.Parsing.Arc.PhaseThree` |
| 3 | Type comparisons | 1 (pure) | `Test.Perspectives.Representation.ADT.SpecialisesADT`, `Test.Representation.ADT` |

---

## Summary

| Topic | Recommended action |
|---|---|
| Mobile native app | Adopt **Capacitor** for iOS/Android (wrap existing web app); use silent push + background fetch to wake the PDR |
| Desktop native app | Adopt **Tauri v2** for a lightweight cross-platform shell; PDR stays in the WebView SharedWorker |
| PDR on Node.js | Replace `pouchdb-browser` → `pouchdb`, `affjax-web` → `affjax-node`, stub `idb-keyval`; entry point via `InternalChannel` |
| Automated testing (parser) | Enable pure PureScript test suites in CI immediately; no external deps |
| Automated testing (sync) | Stub AMQP transport; run two PDR instances in-process with in-memory PouchDB |
