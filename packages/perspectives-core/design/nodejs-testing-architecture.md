# Architecture for Testing and Mobile

This document responds to the architectural questions raised in GitHub issue *"Architectuur voor testen en mobiel"*.  It covers four topics:

1. **Converting MyContexts to a native mobile (and desktop) app.**
2. **Running the PDR as a Node.js application.**
3. **Automated testing strategy.**
4. **Version updates for installable applications (Capacitor and Tauri).**

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
| AffJax / XHR | `affjax-web` (uses browser XHR) | Use `affjax-node` (uses `xhr2` npm package); aliased via Rollup — no source changes needed |
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

`affjax-web` depends on the browser XHR API.  For Node.js, use [`affjax-node`](https://pursuit.purescript.org/packages/purescript-affjax-node) instead, which uses the [`xhr2`](https://www.npmjs.com/package/xhr2) npm package (a Node.js XHR polyfill).  `Affjax.Node` provides **exactly the same API surface** as `Affjax.Web`: `request`, `printError`, `Request`, `Response`, `Error`.

**Implementation (no PureScript source changes required):**

The five PureScript source files that import `Affjax.Web as AJ` do **not** need to change.  Instead:

1. Add `affjax-node` alongside `affjax-web` in `spago.yaml` so spago compiles both modules:

   ```yaml
   dependencies:
     - affjax-node   # added for Node.js build
     - affjax-web    # kept for browser build
   ```

2. Add `xhr2` to `devDependencies` in `package.json` (required by the `affjax-node` FFI):

   ```bash
   pnpm add --save-dev xhr2@^0.2.1
   ```

3. In `rollup.node.config.js`, the alias plugin transparently redirects the compiled
   `output/Affjax.Web/index.js` module to `output/Affjax.Node/index.js` at bundle time:

   ```js
   {
     find: /^.*[/\\]Affjax\.Web[/\\]index\.js$/,
     replacement: path.join(__dirname, 'output/Affjax.Node/index.js'),
   }
   ```

   This works because `@rollup/plugin-alias` intercepts module IDs before path resolution.
   Since the compiled PureScript modules reference `../Affjax.Web/index.js`, the regex
   matches the entire import string and substitutes the Node.js driver.

**All three changes are already applied in this branch.**  After running `pnpm install` and `pnpm exec spago build`, the `build:node` script will produce a fully Node.js-compatible bundle.

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

## Part 4 — Version Updates for Installable Applications

### Current behaviour (PWA)

The existing MyContexts web app uses the browser's [Service Worker API](https://developer.mozilla.org/en-US/docs/Web/API/Service_Worker_API) to manage caching and updates.  The service worker in `src/perspectives-serviceworker.js` is versioned with `__MYCONTEXTS_VERSION__` and `__BUILD__` tokens that are substituted at build time.  When the browser detects that a new service worker file has been deployed, it:

1. Downloads and installs the new worker in the background.
2. Notifies the running app (via a `postMessage` from the service worker to its clients).
3. The app prompts the user and, upon acknowledgement, calls `skipWaiting()` + `clients.claim()` so that the new worker activates immediately.

This gives users a near-seamless, server-controlled update path: deploy a new build, and all open PWA clients receive the update within a few hours (or immediately on next reload).

The central question raised in the issue is: **can this update flow be preserved in native Capacitor and Tauri installations?**

---

### Option A — Capacitor: two independent update channels

Capacitor apps consist of two independent layers that are updated through completely different channels.

#### Native shell updates (app store)

The Capacitor native shell (the iOS `.ipa` or Android `.apk`/`.aab`) must be submitted to the Apple App Store or Google Play Store whenever:
- A Capacitor plugin is added, removed, or updated.
- The target iOS/Android SDK version changes.
- Any native Swift/Kotlin code changes.

App store submissions go through review (hours for Google Play, up to a few days for Apple).  For the Perspectives use case this path is only needed when the native plumbing changes — not for every new MyContexts feature release.

#### Web layer updates (over-the-air)

Because all MyContexts logic runs in the WebView as web content, the web bundle can be replaced without touching the native shell.  There are three approaches:

**A1. Remote web app (simplest)**

Configure Capacitor to load the web app from a remote URL instead of bundling it locally:

```typescript
// capacitor.config.ts
const config: CapacitorConfig = {
  appId: 'nl.perspectivesdesign.mycontexts',
  appName: 'MyContexts',
  server: {
    // In production, load from your deployment server
    url: 'https://mycontexts.com',
    cleartext: false,
  },
};
```

With this setup, every time the user opens the app the WebView loads the latest deployed web content — exactly like a browser.  The existing service worker update mechanism continues to work unchanged inside the WebView.

**Caveat**: The app must have network access on launch.  For fully offline-capable apps, this approach is not suitable.

**A2. Capacitor Live Updates plugin (`@capacitor/live-updates`)**

The `@capacitor/live-updates` plugin (part of Ionic's Appflow platform, but the core plugin is open source) provides a structured OTA update flow:

1. At app startup, the plugin checks a configured endpoint for the latest bundle version.
2. If a newer bundle is available, it downloads and stores it in the app's local storage.
3. On the next app restart the new bundle is loaded from local storage.

```typescript
import { LiveUpdates } from '@capacitor/live-updates';

// Check for update on app resume
App.addListener('resume', async () => {
  const result = await LiveUpdates.sync();
  if (result.activeApplicationPathChanged) {
    await LiveUpdates.reload();   // restart to apply update
  }
});
```

This mirrors the behaviour of the existing PWA service worker: the update is downloaded in the background, and the user sees the new version on next open (or immediately, if a reload is triggered).

**Bundle server**: The bundle server is simply a static file host that serves versioned Vite build output.  The endpoint can be a GitHub Release asset, an S3 bucket, or any HTTPS server.

**A3. Self-hosted OTA using the existing service worker**

Inside a Capacitor WebView the browser's service worker API is available on both Android (Chrome WebView) and iOS (WKWebView, iOS 16+).  This means the **existing `perspectives-serviceworker.js` update logic continues to work unchanged** when the Capacitor app loads from a remote URL (approach A1) or when the bundle is replaced via a custom mechanism.

For a local-bundle Capacitor app (no remote URL), the service worker caches assets from the local filesystem, so it cannot self-update — option A2 is required in that case.

#### Update flow comparison for Capacitor

| | Remote URL (A1) | Live Updates plugin (A2) | Service worker in WebView (A3) |
|---|---|---|---|
| Requires network on launch | Yes | No (uses cached bundle) | No |
| App store submission required | Never (web changes only) | Never (web changes only) | Never |
| Update visible to user | On every open | After background download + restart | On next reload (same as PWA) |
| Code changes in MyContexts | `capacitor.config.ts` only | Add `@capacitor/live-updates` | None (service worker already present) |
| Works fully offline | No | Yes | Yes (with existing service worker caching) |

---

### Option B — Tauri: built-in updater

Tauri v2 ships a first-party updater plugin (`tauri-plugin-updater`) that handles both the Rust backend and the bundled web assets in a single update package.

#### How the Tauri updater works

1. The running app periodically checks a JSON endpoint (configurable in `tauri.conf.json`) for a new version.
2. The endpoint returns a version string and download URLs for the target platform (e.g. `.dmg` for macOS, `.msi` for Windows, `.AppImage` for Linux).
3. The updater downloads the update bundle (which contains both the new Rust binary and the new web assets).
4. The update is applied on the next app restart.

```json
// src-tauri/tauri.conf.json (relevant section)
{
  "plugins": {
    "updater": {
      "active": true,
      "endpoints": [
        "https://mycontexts.com/releases/{{target}}/{{arch}}/{{current_version}}"
      ],
      "dialog": true,
      "pubkey": "<base64-encoded-public-key>"
    }
  }
}
```

All update bundles must be **cryptographically signed** with a key pair generated by `tauri signer generate`.  The public key is embedded in `tauri.conf.json`; the private key is kept secret (e.g. in a CI secret) and used at release time.

#### Triggering the update check in code

```rust
// src-tauri/src/main.rs
use tauri_plugin_updater::UpdaterExt;

fn main() {
    tauri::Builder::default()
        .plugin(tauri_plugin_updater::Builder::new().build())
        .setup(|app| {
            let handle = app.handle().clone();
            tauri::async_runtime::spawn(async move {
                update(handle).await.ok();
            });
            Ok(())
        })
        .run(tauri::generate_context!())
        .unwrap();
}

async fn update(app: tauri::AppHandle) -> tauri_plugin_updater::Result<()> {
    if let Some(update) = app.updater()?.check().await? {
        let mut downloaded = 0;
        update.download_and_install(
            |chunk_length, content_length| {
                downloaded += chunk_length;
            },
            || { /* on finish */ },
        ).await?;
        app.restart();
    }
    Ok(())
}
```

The TypeScript side can also trigger and display the update UI using the `@tauri-apps/plugin-updater` JavaScript package:

```typescript
import { check } from '@tauri-apps/plugin-updater';
import { ask, message } from '@tauri-apps/plugin-dialog';

async function checkForUpdates() {
  const update = await check();
  if (update?.available) {
    const yes = await ask(
      `Version ${update.version} is available.\nRelease notes: ${update.body}\n\nInstall now?`,
      { title: 'MyContexts Update', kind: 'info' }
    );
    if (yes) {
      await update.downloadAndInstall();
      // app will restart automatically
    }
  }
}
```

This closely mirrors the existing PWA update notification flow in MyContexts.

#### Update endpoint

The endpoint can be a static JSON file hosted on GitHub Releases (using the [Tauri action](https://github.com/tauri-apps/tauri-action) to generate update artefacts automatically):

```json
// Example update manifest served at the endpoint
{
  "version": "1.2.3",
  "notes": "Bug fixes and performance improvements",
  "pub_date": "2025-06-01T00:00:00Z",
  "platforms": {
    "darwin-x86_64": {
      "url": "https://github.com/org/mycontexts/releases/download/v1.2.3/mycontexts_1.2.3_x64.dmg",
      "signature": "<signature>"
    },
    "windows-x86_64": {
      "url": "https://github.com/org/mycontexts/releases/download/v1.2.3/mycontexts_1.2.3_x64-setup.exe",
      "signature": "<signature>"
    },
    "linux-x86_64": {
      "url": "https://github.com/org/mycontexts/releases/download/v1.2.3/mycontexts_1.2.3_amd64.AppImage",
      "signature": "<signature>"
    }
  }
}
```

The [Tauri GitHub Action](https://github.com/tauri-apps/tauri-action) can generate this manifest and sign the artefacts automatically as part of a CI/CD release workflow.

#### Delta updates (partial updates)

Tauri v2's updater supports **NSIS-based delta updates** on Windows (only the changed files are downloaded).  On macOS and Linux, full bundle updates are used.  For MyContexts the bundles are small enough that delta updates are not a priority.

---

### Comparison: update mechanisms across deployment targets

| Deployment target | Update mechanism | User prompt? | Requires app store? | Code changes in MyContexts |
|---|---|---|---|---|
| **PWA (current)** | Service worker (`perspectives-serviceworker.js`) | Yes (existing flow) | No | None |
| **Capacitor — web changes** | Remote URL or Live Updates plugin | Configurable | No | `capacitor.config.ts` or add plugin |
| **Capacitor — native shell changes** | App Store / Play Store | No (OS-managed) | Yes | None |
| **Tauri desktop** | `tauri-plugin-updater` + signed bundle | Yes (dialog) | No (self-distributed) | Add `@tauri-apps/plugin-updater` |
| **Electron (legacy)** | `electron-updater` (from `electron-builder`) | Yes (dialog) | No | Add `electron-updater` |

### Key findings

1. **The existing service worker update logic is fully reusable in Capacitor** when the app loads its web content from a remote URL (approach A1).  The update experience is identical to the current PWA.

2. **For a locally-bundled Capacitor app**, the `@capacitor/live-updates` plugin provides an equivalent OTA mechanism without requiring app store submissions for web-layer changes.

3. **Tauri's built-in updater** provides a polished, cryptographically secure update flow for desktop apps.  The user-facing update dialog can be driven from TypeScript and closely mirrors the existing PWA notification pattern.

4. **In all three cases**, updates to the web layer (TypeScript, PureScript, React components, ARC models) do **not** require app store review or distribution through a third party.  Only changes to the native shell (Capacitor plugins, Tauri Rust backend) require a traditional release process.

---

## Summary

| Topic | Recommended action |
|---|---|
| Mobile native app | Adopt **Capacitor** for iOS/Android (wrap existing web app); use silent push + background fetch to wake the PDR |
| Desktop native app | Adopt **Tauri v2** for a lightweight cross-platform shell; PDR stays in the WebView SharedWorker |
| PDR on Node.js | Replace `pouchdb-browser` → `pouchdb`, `affjax-web` → `affjax-node`, stub `idb-keyval`; entry point via `InternalChannel` |
| Automated testing (parser) | Enable pure PureScript test suites in CI immediately; no external deps |
| Automated testing (sync) | Stub AMQP transport; run two PDR instances in-process with in-memory PouchDB |
| **Version updates (Capacitor)** | Load web app from remote URL (service worker update works unchanged) or use `@capacitor/live-updates` for locally-bundled OTA |
| **Version updates (Tauri)** | Use `tauri-plugin-updater`; serve signed update manifests from GitHub Releases via Tauri GitHub Action |

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

Rollup's `@rollup/plugin-alias` can transparently redirect a module import.  All three aliases are now in `rollup.node.config.js`:

```js
alias({
  entries: [
    // Replace the browser PouchDB FFI file with the Node.js version
    { find: /persistenceAPI\.js$/, replacement: path.join(__dirname, 'src/core/persistence/persistenceAPI.node.js') },
    // Replace the browser idb-keyval FFI file with a Node.js stub
    { find: /idb-keyval\.js$/, replacement: path.join(__dirname, 'src/core/idb-keyval.node.js') },
    // Redirect affjax-web compiled output to affjax-node compiled output
    { find: /^.*[/\\]Affjax\.Web[/\\]index\.js$/, replacement: path.join(__dirname, 'output/Affjax.Node/index.js') },
  ]
}),
```

**AffJax — switch implemented via Rollup alias (no PureScript source changes):**

`affjax-node@1.0.0` uses the [`xhr2`](https://www.npmjs.com/package/xhr2) npm package (a Node.js XHR polyfill) and provides `Affjax.Node` — an exact drop-in replacement for `Affjax.Web` with the same `request`, `printError`, `Request`, `Response`, `Error` API surface.

The switch is fully implemented in this branch:
- `spago.yaml`: `affjax-node` added alongside `affjax-web` (both compile; only one is bundled per target)
- `package.json`: `xhr2@^0.2.1` added to `devDependencies`
- `rollup.node.config.js`: adds the alias entry that redirects the compiled `output/Affjax.Web/index.js` to `output/Affjax.Node/index.js` at Rollup bundle time

```js
// rollup.node.config.js — alias entry for AffJax
{
  find: /^.*[/\\]Affjax\.Web[/\\]index\.js$/,
  replacement: path.join(__dirname, 'output/Affjax.Node/index.js'),
}
```

The five PureScript files (`persistenceAPI.purs`, `couchdbFunctions.purs`, `authentication.purs`, `utilities.purs`, `managementAPI.purs`) continue to import `Affjax.Web as AJ` unchanged.  The alias is completely transparent.

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


---

### Part 4 — Version updates in practice

#### 4.1 Capacitor — recommended update strategy

For a MyContexts Capacitor app the recommended strategy is a **two-tier update model**:

| Tier | What changes | Update path | Review required |
|---|---|---|---|
| Web layer | React/TypeScript code, PureScript PDR, ARC models, CSS | OTA (remote URL or Live Updates plugin) | None |
| Native shell | Capacitor plugins, iOS/Android SDK target, permissions | App Store / Play Store submission | Yes (Apple/Google) |

In practice, 95 % of MyContexts releases are web-layer-only changes.  Only plugin upgrades or OS-SDK changes require an app store release.

**Recommended approach: remote URL loading**

For MyContexts the simplest and most maintainable approach is to configure Capacitor to load the web app from the existing deployment server:

```typescript
// capacitor.config.ts
const config: CapacitorConfig = {
  appId: 'nl.perspectivesdesign.mycontexts',
  appName: 'MyContexts',
  server: {
    url: 'https://mycontexts.com',
    cleartext: false,
    allowNavigation: ['*.perspectives.domains'],
  },
};
```

With this configuration:
- The existing service worker (`perspectives-serviceworker.js`) handles update detection and notification — **no new code required**.
- The existing version-bump mechanism (`build.json`, `__MYCONTEXTS_VERSION__` token) is the only release trigger needed.
- The app requires network access to load initially but caches assets via the service worker for subsequent offline use.

**Alternative: Live Updates plugin for fully offline apps**

If the app must be fully usable without a network connection on first launch, the `@capacitor/live-updates` plugin can be added:

```bash
pnpm add @capacitor/live-updates
npx cap sync
```

The plugin fetches update bundles from a custom endpoint (or Ionic Appflow) and applies them on next restart, independently of the network state at launch time.

#### 4.2 Tauri — recommended update strategy

For a MyContexts Tauri desktop app, use the built-in `tauri-plugin-updater`:

**Setup (once per project):**

```bash
# Generate a key pair for signing updates
tauri signer generate -w ~/.tauri/mycontexts.key

# Add the updater plugin
cargo add tauri-plugin-updater
pnpm add @tauri-apps/plugin-updater
```

Add the public key to `tauri.conf.json`:

```json
{
  "plugins": {
    "updater": {
      "pubkey": "<paste public key here>",
      "endpoints": ["https://mycontexts.com/releases/{{target}}/{{arch}}/{{current_version}}"],
      "dialog": true
    }
  }
}
```

**Release workflow (GitHub Actions):**

The [tauri-action](https://github.com/tauri-apps/tauri-action) GitHub Action can be used to:
1. Build signed release bundles for all three desktop platforms in parallel.
2. Upload artefacts to a GitHub Release.
3. Update the update manifest JSON served at the endpoint URL.

```yaml
# .github/workflows/release-tauri.yml (excerpt)
- uses: tauri-apps/tauri-action@v0
  with:
    tagName: v__VERSION__
    releaseName: 'MyContexts v__VERSION__'
    releaseBody: 'See the assets to download this version and install.'
    releaseDraft: true
    prerelease: false
  env:
    TAURI_SIGNING_PRIVATE_KEY: ${{ secrets.TAURI_PRIVATE_KEY }}
    TAURI_SIGNING_PRIVATE_KEY_PASSWORD: ${{ secrets.TAURI_KEY_PASSWORD }}
```

**User experience:**

The update check is triggered at startup (Rust side) and can also be triggered from the TypeScript side via the `@tauri-apps/plugin-updater` package.  This allows reusing the existing "new version available" notification in the MyContexts React UI:

```typescript
// In MyContexts startup code (mycontexts/src/App.tsx or equivalent)
import { check } from '@tauri-apps/plugin-updater';

async function checkTauriUpdate() {
  const update = await check();
  if (update?.available) {
    // Reuse the existing "update available" notification UI
    notifyUserOfUpdate(update.version, update.body ?? '', async () => {
      await update.downloadAndInstall();
      // The app restarts automatically after install
    });
  }
}
```

#### 4.3 Keeping parity between PWA and native update flows

To maintain a consistent update experience across all deployment targets:

1. **Single version source of truth**: the `version` field in `packages/mycontexts/package.json` is the canonical version.  The service worker, Capacitor config, and `tauri.conf.json` all derive from it.

2. **Unified release script**: a single `pnpm run release` script (at the `mycontexts` package level) should:
   - Bump the version in `package.json`.
   - Build the Vite web output.
   - Build the Capacitor bundles and push to the OTA endpoint.
   - Build the Tauri desktop bundles and publish a GitHub Release.
   - Deploy the updated web app (triggering the service worker update for PWA users).

3. **Notification parity**: the React-side "update available" notification component can be triggered by all three update mechanisms:
   - Service worker → existing `message` event from `perspectives-serviceworker.js`.
   - Capacitor Live Updates → custom event after `LiveUpdates.sync()` returns `activeApplicationPathChanged: true`.
   - Tauri updater → call `checkTauriUpdate()` at startup, described in §4.2 above.

#### 4.4 Summary of immediate next steps (update-related)

1. **For a Capacitor MVP**: add `server.url` to `capacitor.config.ts` pointing to the production deployment.  The existing service worker handles all update logic automatically.
2. **For a Tauri MVP**: run `tauri signer generate`, add the public key to `tauri.conf.json`, and add the `tauri-action` release workflow to `.github/workflows/`.
3. **For production Capacitor (offline-capable)**: add `@capacitor/live-updates`, configure the bundle endpoint, and trigger `LiveUpdates.sync()` on app resume.
