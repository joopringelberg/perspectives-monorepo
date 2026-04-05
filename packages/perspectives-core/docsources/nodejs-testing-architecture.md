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

---

## Tactical-Operational Elaboration

This section answers the follow-up questions raised after the strategic overview above.

---

### Part 1 — Capacitor vs. Tauri: choosing a path

#### 1.1 Capacitor vs. Tauri — detailed comparison

Both tools can package the existing MyContexts web app as a native application.  The relevant differences are:

| | Capacitor | Tauri v2 |
|---|---|---|
| **Language** of native layer | Swift/Kotlin (via plugins) | Rust |
| **Platform targets** | iOS, Android | macOS, Windows, Linux, iOS, Android (v2) |
| **WebView** | WKWebView (iOS), Chrome WebView (Android) | Same — uses the OS WebView |
| **Background execution** | First-class: Background Runner plugin, APNs/FCM push wakes the app | Limited: Tauri background tasks are desktop-only; no native push wake on mobile |
| **Existing code change** | None — the existing Vite output is used as-is | None for the web part |
| **Native-layer complexity** | Low — plugins written in Swift/Kotlin with a thin JS bridge | Medium — Rust backend, but excellent documentation |
| **Ecosystem** | Large; well-documented mobile plugins for push, camera, filesystem, etc. | Growing; focused on desktop, mobile support is newer in v2 |
| **Cross-platform single codebase** | Mobile only (iOS + Android) | Desktop + mobile from one Rust + Web codebase |
| **Push / background sync** | Mature: `@capacitor/push-notifications`, Background Runner | Desktop: OS notifications; mobile: work-in-progress (Tauri v2 mobile) |
| **PDR placement** | SharedWorker inside WebView | Same — SharedWorker inside WebView; or a Rust/Node.js sidecar for desktop |

**Recommendation for a single-tool cross-platform strategy:**

Tauri v2 is the better single-tool choice if desktop *and* mobile are needed from one codebase, but its mobile background-sync story is not yet mature.  Capacitor is more suitable if mobile is the immediate priority and push-driven background sync is required now.

A pragmatic two-stage approach:
1. **Short term — mobile**: Use Capacitor to publish iOS and Android apps.  The web app code is completely unchanged; only a thin Capacitor shell and two plugins are added.
2. **Medium term — desktop**: Use Tauri v2 for desktop packaging.  At this point, evaluate whether Tauri mobile has matured enough to consolidate into a single tool.

#### 1.2 Monorepo structure for a two-track approach

The `mycontexts` package is the only consumer of the native shell code.  The other packages (`perspectives-core`, `perspectives-react`, `perspectives-proxy`, etc.) are unaware of the native container.

**Proposed layout inside `packages/mycontexts/`:**

```
packages/mycontexts/
├── src/                        # Existing React/Vite app (unchanged)
├── public/                     # Existing static assets (unchanged)
├── capacitor.config.ts         # NEW: Capacitor project config
├── android/                    # NEW: Capacitor-generated Android project
├── ios/                        # NEW: Capacitor-generated iOS project
├── src-tauri/                  # NEW: Tauri Rust backend (desktop)
│   ├── Cargo.toml
│   ├── tauri.conf.json
│   └── src/
│       └── main.rs
└── package.json
```

`android/` and `ios/` are generated by `npx cap add android` / `npx cap add ios`.  They are managed by Capacitor and generally not hand-edited.

`src-tauri/` is the Tauri backend directory, following the standard Tauri layout.

**No new top-level packages are needed.**  Mobile and desktop builds are simply additional build targets within the existing `mycontexts` package.  The `package.json` gains a few extra scripts:

```json
"build:capacitor": "vite build && npx cap sync",
"build:android":   "vite build && npx cap sync && npx cap run android",
"build:ios":       "vite build && npx cap sync && npx cap run ios",
"build:tauri":     "tauri build"
```

#### 1.3 Impact on other packages

**`perspectives-core`, `perspectives-react`, `perspectives-proxy`, `perspectives-sharedworker`, `perspectives-pageworker`** are **not affected** by the native-shell transition.  They are bundled as part of the web layer that runs inside the native WebView, exactly as they do today in the browser.

The one possible exception is `perspectives-proxy`.  The `configurePDRproxy("sharedWorkerChannel")` call in `mycontexts/src/startPDR.tsx` could eventually be supplemented with a Capacitor-specific channel (e.g. `"capacitorChannel"`) if native IPC turns out to be faster or more reliable than the SharedWorker.  That would require a small addition to `perspectives-proxy`, but it is not required in a first iteration.

---

### Part 2 — PDR-N in the monorepo

#### 2.1 — 2.3 One package, two outputs (no split)

The short answer to all three questions is: **keep `perspectives-core` as a single package and produce both `PDR-B` (browser) and `PDR-N` (Node.js) from it using separate Rollup build configurations**.  There is no need for `perspectives-pdrb` and `perspectives-pdrn` packages.

**Rationale:**  The PDR PureScript source is 100 % platform-agnostic.  Only the JavaScript FFI layer (`persistenceAPI.js`, `idb-keyval.js`) and the `affjax-web` PureScript dependency are browser-specific.  These are already isolated at the module boundary.

**Concretely, the two build outputs would be:**

| Build | Output file | PouchDB | AffJax | `idb-keyval` | Entry |
|---|---|---|---|---|---|
| PDR-B (browser) | `dist/perspectives-core.js` | `pouchdb-browser` | `affjax-web` | browser IndexedDB | existing `rollup.config.js` |
| PDR-N (Node.js) | `dist/perspectives-core.node.js` | `pouchdb` (LevelDB) | `affjax-node` | file/memory stub | new `rollup.node.config.js` |

**How Rollup aliasing works in practice:**

Rollup's `@rollup/plugin-alias` can transparently redirect a module import:

```js
// rollup.node.config.js
import alias from '@rollup/plugin-alias';

export default {
  input: './output/Main/index.js',
  output: { file: './dist/perspectives-core.node.js', format: 'es' },
  plugins: [
    alias({
      entries: [
        // Replace the browser PouchDB FFI file with the Node.js version
        { find: /(.*)persistenceAPI\.js$/, replacement: '$1persistenceAPI.node.js' },
        // Replace the browser idb-keyval FFI file with a Node.js stub
        { find: /(.*)idb-keyval\.js$/, replacement: '$1idb-keyval.node.js' },
      ]
    }),
    resolve({ preferBuiltins: true }),
    commonjs(),
    json(),
  ],
  external: ['eventsource'],
};
```

**AffJax — how to switch at the PureScript level:**

PureScript does not have conditional imports, but spago supports alternate dependency sets through `extraPackages` overrides.  For the Node.js build, create a second spago workspace target or a `spago.node.yaml` that lists `affjax-node` instead of `affjax-web`.

The five PureScript files that import `Affjax.Web` are:
- `src/core/persistence/persistenceAPI.purs`
- `src/core/persistence/couchdbFunctions.purs`
- `src/core/persistence/authentication.purs`
- `src/core/computedValues/utilities.purs`
- `src/core/amqpTransport/managementAPI.purs`

In all five files the import is `Affjax.Web as AJ`.  The `affjax-node` package exposes exactly the same `Affjax.Node as AJ` interface.  A one-line sed substitution in the spago pre-build step (or a pair of module aliases in a `spago.node.yaml` `extraPackages` override) is sufficient.  No logic changes are needed.

**`idb-keyval` Node.js stub:**

A minimal Node.js stub for `idb-keyval.js` already exists as a pattern — a simple in-memory `Map` backed by a JSON file:

```js
// src/core/idb-keyval.node.js
import { readFileSync, writeFileSync, existsSync } from 'fs';

const STORE_FILE = './idb-keyval-store.json';

function load() {
  if (existsSync(STORE_FILE)) {
    try { return new Map(Object.entries(JSON.parse(readFileSync(STORE_FILE, 'utf8')))); }
    catch { return new Map(); }
  }
  return new Map();
}

const store = load();

function save() {
  writeFileSync(STORE_FILE, JSON.stringify(Object.fromEntries(store)), 'utf8');
}

export const getValueByKeyImpl = function(key) {
  return function() {
    return Promise.resolve(store.has(key) ? store.get(key) : null);
  };
};

export const setKeyValueImpl = function(key, value) {
  return function() { store.set(key, value); save(); };
};

export function clear() { store.clear(); save(); }
```

The Rollup alias for `idb-keyval.js` → `idb-keyval.node.js` redirects to this stub automatically.

**`package.json` additions:**

```json
"build:node": "pnpm exec spago build && rollup -c rollup.node.config.js",
"test:node":  "pnpm exec spago test --main Test.Main"
```

**Summary: no package split is needed.**  The monorepo gains one new file (`rollup.node.config.js`), one new stub (`idb-keyval.node.js`), and a new build script.  `perspectives-core` stays as a single package.

---

### Part 3 — AI-assisted iterative test workflow

#### 3.1 Vision

The goal is a mostly-automated, iterative cycle:

```
(a) Write/expand test suites  →  (b) Run tests, produce issues
→  (c) Prioritise issues  →  (d) Fix issues  →  repeat
```

GitHub Copilot (as a coding agent) can today be used for steps (a) and (d).  Steps (b) and (c) can be automated with GitHub Actions.

#### 3.2 Proposed workflow

**Step (a) — Writing test suites**

Copilot can be instructed to generate PureScript `test-unit` test suites from:
- A specification document (e.g. the `docsources/` markdown files in this repo).
- An existing source module (e.g. "write tests for every exported function in `syncQuery.purs`").
- An issue description (e.g. "write a regression test for the behaviour described in issue #123").

A GitHub issue template `test-suite-request.md` with fields for *subject*, *source module*, and *specification* can be used to drive Copilot agent sessions.

**Step (b) — Running tests and creating issues**

A GitHub Actions workflow (`test-and-report.yml`) can:
1. Run `spago test --main Test.Main` on the Node.js target.
2. Parse the `test-unit` output (it produces TAP-compatible output or can be configured to do so).
3. For each failing test, use the GitHub API (`gh issue create`) to open an issue with:
   - The test name and failure message.
   - A link to the test source file and line number.
   - A label `auto: test-failure`.

**Step (c) — Prioritising issues**

GitHub Projects can automatically add `auto: test-failure` issues to a board.  Manual triage assigns a priority label.  Copilot can also be asked to suggest priorities by reading the issue list and the architecture documents.

**Step (d) — Fixing issues**

A Copilot agent session is triggered by a labelled issue.  The agent reads the issue, identifies the failing test and the relevant source module, makes the minimal fix, and opens a PR.  The PR is reviewed and merged.  The CI workflow then re-runs the test suite.

#### 3.3 Current limitations

- **Multiple concurrent Copilot instances**: GitHub Copilot Coding Agent currently supports one agent session per issue/PR at a time within a repository.  Parallelism across multiple issues is achievable by working on separate branches concurrently, but coordination must be manual.
- **PureScript knowledge**: Copilot's PureScript support is good at the module and function level but may need guidance for complex type-class hierarchies or monad-stack interactions.  The `docsources/` documents in this repo (including `query-inversion.md`, `type-comparison.md`) are the best way to give the agent the context it needs.
- **End-to-end tests**: Steps (b)–(d) require the PDR to run in Node.js (Part 2 above) so that the test suite can be executed in CI without a browser.  Until that is done, only Layer 1 (pure PureScript) tests can be automated.

#### 3.4 Immediate next steps

1. Enable Layer 1 (pure PureScript) tests in CI — uncomment the relevant suites in `test/Main.purs` and add the GitHub Actions workflow.
2. Write a `rollup.node.config.js` and the `idb-keyval.node.js` stub (as described in Part 2) so that Layer 2 integration tests can run in CI.
3. Create the `test-suite-request.md` issue template and try generating a new test suite via Copilot for one high-priority area (e.g. ARC parser phases 2–3).
4. After a first round of CI-driven failures, evaluate whether the issue-creation automation in step (b) is worth implementing.
