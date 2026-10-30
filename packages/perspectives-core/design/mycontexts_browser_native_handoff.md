# MyContexts / Perspectives — Browser, PWA and Native Application Handoff

**Status:** recommended design and staged implementation plan. The platform choices below are Capacitor for mobile and Tauri for desktop, subject to the stated proof-of-concept gates. **Prepared:** 2026-10-28.

## Current architecture

MyContexts is a local-first distributed application. Its PureScript Perspectives Distributed Runtime (`perspectives-core`) runs in a browser **SharedWorker**, used by multiple tabs/pages via `perspectives-proxy` and the React UI (`perspectives-react`). Local state is stored in **PouchDB on IndexedDB**. Peers exchange deltas over **RabbitMQ/STOMP via WSS**. A prior Electron packaging approach existed. The browser application has PWA capabilities. The objective is to retain a largely shared application/runtime while making it installable and functional on desktop and mobile, including reliable offline messaging and as much background synchronization as OS policies allow.

## Problem statement

A browser SharedWorker and STOMP socket may continue while pages remain alive, but browser throttling, PWA suspension and OS lifecycle rules can interrupt the connection. iOS is particularly restrictive; Android also restricts background work. A native wrapper **does not by itself** grant indefinite background WebSocket execution. Therefore, message delivery must not depend on the recipient remaining continuously connected. Push notifications should ideally serve as a *silent wake/sync signal* where possible, with application logic deciding whether a user-facing notification is appropriate; silent execution cannot be guaranteed by mobile operating systems.

## Platform options

| Platform | Shared code | Local persistence | Live network | Background caveat |
|---|---|---|---|---|
| Browser/PWA | Existing React + PureScript | PouchDB/IndexedDB (or RxDB/IndexedDB) | WebSocket when active | Mobile browser may suspend all JS/workers |
| Capacitor iOS/Android | React + PureScript in WebView, native plugins | Verify IndexedDB/WebView behavior; consider SQLite via adapter | WebSocket when foreground | APNs/FCM + constrained native background tasks; no guaranteed persistent socket |
| Tauri desktop | React + PureScript with native shell | IndexedDB/WebView or native DB adapter | WebSocket while app process runs | Handle sleep, networking and reconnection |
| Tauri mobile | Shared UI/runtime where supported | Verify storage and worker compatibility | Foreground sockets | Same mobile OS background restrictions |

### PWA baseline

Keep the browser/PWA as the lowest-common-denominator deployment. Use Service Worker/Push API for supported push behavior, but distinguish a Service Worker (event-driven lifecycle) from the long-lived SharedWorker containing the PDR. On iOS, web push requires supported installed web-app conditions and user permission; do not assume background JS can execute arbitrarily.

### Capacitor mobile

Capacitor was considered a pragmatic way to preserve the React UI while adding native bridges for APNs/FCM, local persistence and potentially networking. Confirm **SharedWorker support inside each target WebView**; it may differ from desktop browsers. If unsupported, plan a runtime-hosting abstraction or alternate Worker/MessageChannel/BroadcastChannel arrangement rather than assuming the current SharedWorker bundle will work unchanged. For local data, consider a common RxDB-facing API with IndexedDB in browser and SQLite in mobile, but benchmark replication, transactions and migration before selecting it.

For Android, a foreground service can support some longer-running tasks but has battery, permission and user-visible notification implications. For iOS, silent remote notifications/background fetch are opportunistic and OS-scheduled, not a reliable always-on worker. Native push requires a server-side trusted component holding APNs/FCM credentials; the existing registration backend could potentially be extended as a push proxy.

### Tauri desktop and mobile

Tauri desktop offers an installable shell and access to native capabilities while preserving a web frontend. A background process can generally keep a connection alive while the app is running, but sleep and OS lifecycle still require recovery. Tauri mobile does not evade iOS/Android restrictions. An alternative previously explored was a **native Rust/libp2p component** beneath the React UI for richer networking/P2P; that is a distinct research track and is not automatically solved by wrapping the UI.

## Recommended separation of concerns

```text
Perspectives / PDR domain logic
  |-- Messaging interface (RabbitMQ/STOMP today; possible PGMQ later)
  |-- Local persistence interface (PouchDB/IndexedDB today; possible RxDB/SQLite)
  |-- Push / wakeup interface (Web Push, APNs, FCM)
  |-- Platform lifecycle interface (active, background, resume, connectivity)
  `-- UI via perspectives-proxy / perspectives-react

Platform adapters:
  Browser/PWA | Capacitor iOS/Android | Tauri desktop/mobile
```

Keep the core independent of native SDKs. The native shell should implement platform APIs, not duplicate domain logic. Test whether the PureScript runtime can execute in a worker, WebView or native-hosted context with the required IPC guarantees.

## Delivery model and connection to the messaging handoff

The separate messaging handoff evaluates replacing RabbitMQ/STOMP with **PGMQ (durable inbox) + Supabase Realtime (foreground notification)**. That model complements, but does not depend on, native packaging:

1. Persist the full delta in a durable queue before considering it sent.
2. Notify an active client via Realtime WebSocket or existing STOMP-style signaling.
3. On startup, reconnect and resume, **always drain** the durable inbox regardless of whether a notification was seen.
4. On mobile background, APNs/FCM/Web Push may indicate pending work; delivery of a silent push and immediate execution are **best-effort**. User-visible notifications or delayed sync may be necessary.
5. Ack only after safely applying the message locally; use idempotency to survive redelivery.

This avoids the false goal of maintaining a permanently connected mobile client. It also preserves the user's preference that the app, not the push provider, decide which messages deserve user attention.

## Critical questions for the implementing AI

- Which browser and mobile WebViews actually support the current SharedWorker + IndexedDB + PureScript bundle? Test on real devices.
- How will multiple tabs/windows coordinate one PDR instance in browser and desktop-native environments?
- What is the mobile runtime host when SharedWorker is unavailable? How does the UI communicate with it?
- Can local data migrate safely from PouchDB to RxDB or SQLite without losing revisions or distributed state?
- What background operations are truly required: delivery, processing, user alerts, or simply eventual sync?
- Which push modes can perform limited silent processing on each platform, and what is the fallback if the OS refuses?
- Does P2P/libp2p justify a native networking plugin, or is broker-mediated durable messaging sufficient?
- How do reconnection, replay, idempotency and crash recovery work across all platforms?
- How can the browser, Capacitor and Tauri builds share tests, release artifacts and protocol compatibility?

## Suggested proof-of-concept sequence

1. Instrument current browser behavior: suspend/resume, worker lifetime, STOMP disconnects, missed messages.
2. Validate the durable-inbox/reconnect-drain protocol in the browser first.
3. Build a minimal Capacitor iOS/Android shell and test SharedWorker, storage, foreground Realtime/STOMP and native push on physical devices.
4. Build a minimal Tauri desktop shell and test lifecycle, persistence and IPC.
5. Select platform-specific persistence and runtime adapters only after tests; do not assume a native wrapper solves background networking.
6. Add end-to-end tests for offline sender/recipient, delayed delivery, duplicate deltas, interrupted processing and OS suspension.

## Relationship to companion document

Read **`mycontexts_messaging_supabase_handoff.md`** for PGMQ, Supabase Realtime, queue topology, authentication, file references, cost assumptions and open messaging questions. These two documents describe **complementary decisions**: (a) reliable messaging and cloud infrastructure, and (b) runtime packaging, mobile background execution and platform adapters.

## Platform decisions

### Mobile: Capacitor

Use **Capacitor for iOS and Android**. MyContexts is already a React/Vite web application, so Capacitor keeps the web application as the app UI and adds native capabilities through a JavaScript bridge and mobile plugins. That is the smallest conceptual change for this codebase and gives a direct path to APNs/FCM registration, native notifications, lifecycle events, and (if needed) SQLite.

Do not choose Tauri Mobile for the first mobile release. Its Rust command/plugin model could be useful if substantial native Rust functionality becomes necessary, but it adds a second native integration model without solving the main mobile constraint: iOS and Android control when background work runs. Reconsider only if the Capacitor proof of concept cannot meet a concrete requirement, or a separately justified Rust component is selected.

Capacitor is a packaging and native-API choice, not a promise of an always-running PDR. Keep the PWA supported, and treat mobile push/background execution as best-effort. The application must recover by reconnecting and draining pending messages whenever it starts or resumes.

### Desktop: Tauri

Use **Tauri for the first native desktop shell**, sharing the MyContexts web UI and keeping platform-specific APIs behind a small bridge. Tauri offers a native desktop application without making a second UI, while its desktop process can keep foreground/background synchronization alive as long as the application is running. It still needs normal sleep, network-loss, and crash recovery.

Before implementation, inspect and, if available, run the earlier Electron packaging work. Reuse it only if it is maintained and materially reduces risk; do not make maintaining Electron and Tauri desktop shells a project goal. If Tauri cannot host the existing web bundle, PDR runtime, and required storage reliably in the proof of concept, stop and compare a revived Electron shell before committing to a desktop-specific rewrite.

### What these decisions do not decide

- Keep the existing PWA/browser deployment as the reference and fallback.
- Retain PouchDB/IndexedDB initially, subject to real-device persistence and quota tests. The RxDB + Supabase direction in [`../docsources/database-backend-options.md`](../docsources/database-backend-options.md) is a separate backend migration, not a prerequisite for native packaging. Do not switch databases as part of the shell proof of concept.
- Keep RabbitMQ/STOMP initially. The PGMQ + Supabase Realtime proposal is a separate messaging decision and requires its own authorization, delivery, load, and cost validation.
- Do not claim offline delivery based on a wrapper or push provider. Validate the existing RabbitMQ postbox against a durable server-side inbox contract: idempotent application, acknowledgement after local persistence, and unconditional inbox drain on startup/resume. Whether it remains RabbitMQ-based or moves to PGMQ remains open.

## Shared-code design

Keep the existing monorepo packages as the shared product: the React/TypeScript UI in `packages/mycontexts`, the public PDR API and domain logic in `packages/perspectives-core`, and the existing proxy and worker packages. Do not fork the UI, ARC models, or PDR for either native shell.

Add shell-specific configuration and bootstrap code alongside the MyContexts application, not inside PDR domain logic. The shared app should depend on narrow interfaces for:

- **PDR hosting and IPC:** preserve the proxy's request/response contract and `MessagePort` boundary. Select SharedWorker, a dedicated worker, or a page-hosted runtime per platform only after testing. The current page-worker path relies on the browser Service Worker for port relay; do not assume that path works unchanged in a native WebView.
- **Platform services:** lifecycle/resume, connectivity, notification registration/display, secure credential storage, file selection/sharing, and app links. Browser implementations remain the default; Capacitor and Tauri provide adapters where required.
- **Persistence:** keep the existing PouchDB-facing behavior until an adapter is justified. Never clear or silently replace a user's local database during shell upgrades; migrations need versioning, backup/recovery, and real-device tests.

Keep native code limited to these adapters and shell concerns. Share application behavior and tests wherever possible, and put unavoidable platform-specific behavior behind explicit interfaces rather than scattered platform checks. Native bridge calls must expose only the minimum required capabilities; never put APNs/FCM credentials or other server secrets in a client bundle.

## Staged implementation plan

Each phase has an explicit exit gate. Stop and revisit the platform choice if a gate fails; do not work around a missing OS capability by assuming a native shell guarantees background execution.

### Phase 0 — Define behavior and validate feasibility

1. Agree on product requirements separately for message delivery, message processing, user-visible alerts, and time-to-sync. Define acceptable delays when a mobile OS suspends the app.
2. Measure current browser behavior: PDR worker lifetime, STOMP reconnects, missed-message recovery, IndexedDB persistence/quota, and multi-tab behavior.
3. Build throwaway minimal shells to verify the existing Vite bundle, worker/runtime startup, PouchDB storage, proxy IPC, and foreground reconnect on desktop and on physical iOS/Android devices. Verify APNs/FCM registration separately from silent background execution.
4. Confirm mobile and desktop signing, developer accounts/toolchains, supported OS versions, and the feasibility of testing on physical devices.

**Exit gate:** each target can start the current app, open the PDR, persist and reload data without loss, and reconnect in the foreground. Record unsupported APIs and platform-specific fallbacks before adding production abstractions.

#### Phase 0 repository audit (2026-10-29)

- `packages/mycontexts/src/startPDR.tsx` currently selects a page-hosted PDR for Safari, iOS, and Android (except Firefox); other browsers use a SharedWorker. The page-worker path relays client ports through the Service Worker. This is a code-path inventory, not evidence that a worker or socket survives suspension.
- The core already has a two-PDR Node.js performance harness. Its default in-process-bus mode measures sender action time, time until Bob observes the result, and end-to-end time. Optional profiling records selected incoming-work spans and message/delta/byte counts. It measures PDR processing and synchronization in Node, not browser throttling, worker lifetime, IndexedDB persistence, or mobile OS behavior.
- `pnpm run test:performance:collector` was validated with Node's test runner: **12 tests passed**. The full performance runner was not run: this checkout has no pnpm dependencies or built performance bundle, and the required Alice/Bob snapshot-completion files are absent. Its runner intentionally refuses to bootstrap missing snapshots. Do not treat the collector tests as performance samples.
- No browser/PWA session or physical iOS/Android device was available for this pass. Browser lifecycle, real WebView worker support, storage durability, and foreground/background reconnect remain unmeasured. The product's acceptable background-sync delay and target OS/version matrix also need owner confirmation.

On a development machine with dependencies and prepared snapshots, run the performance harness from `packages/perspectives-core` to establish a **Node two-PDR baseline**; use `--profile` for internal spans:

```bash
pnpm run build:performance
node scripts/performance-runner.mjs --profile --output /tmp/mycontexts-pdr-performance.json
```

Keep this result separate from the browser/device matrix. Measure browser behavior with an instrumented MyContexts build in each target browser and on physical devices, recording PDR host start/stop, STOMP disconnect/reconnect, missed-message recovery, IndexedDB state after restart, and multiple-tab behavior across foreground, suspension, and resume.

### Phase 1 — Introduce only the shared seams proven necessary

1. Add a runtime-host adapter around the existing proxy/PDR startup path, preserving its message contract.
2. Add a platform-services interface for lifecycle, connectivity, notifications, secure storage, and file APIs only where the proof of concept needs them.
3. Keep browser/PWA implementations working and covered by regression tests; make native adapters opt-in at build/runtime.
4. Add startup/resume recovery hooks that reconnect and drain pending messages. Do not put database or messaging-provider choices into the platform interface unless a real API difference requires it.

**Exit gate:** the browser build still behaves as before, and the same app-level tests can run against browser and native adapters.

### Phase 2 — Ship a desktop shell

1. Package the production MyContexts web build in Tauri; retain the existing PWA build and hosting path.
2. Verify PDR hosting, PouchDB persistence, multiple-window behavior, app links, updates, sleep/resume, offline operation, and reconnection.
3. Define secure defaults for the WebView, Tauri command allowlist, local files, and application updates. Add signing and platform-specific packaging for the selected desktop operating systems.

**Exit gate:** installation, upgrade, restart, and recovery preserve local state; all native commands are narrowly scoped; and a network interruption does not lose queued work.

### Phase 3 — Ship mobile shells

1. Add Capacitor iOS and Android projects that load the same MyContexts app build; keep native project files and plugins isolated from shared UI/runtime logic.
2. Verify IndexedDB/PouchDB behavior and storage persistence on supported WebViews before considering a SQLite adapter. Define and test an explicit migration path before changing storage.
3. Integrate native lifecycle and push registration. Use push as a hint to check for work, not as the message payload or sole delivery mechanism. Show user-visible notifications only according to product policy; document that silent execution is opportunistic.
4. Test on real iOS and Android devices, including permission denial, force quit, OS suspension, network changes, and app upgrade.

**Exit gate:** the app always recovers pending work on foreground start/resume, data survives app restarts/upgrades, and UX describes the actual background-delivery limits.

### Phase 4 — Make asynchronous messaging reliable across shells

1. Specify the durable inbox contract: authorization, recipient isolation, idempotency key, retry/visibility timeout, poison-message handling, acknowledgement only after safe local apply, retention, and observability.
2. Keep the current transport behind an interface while validating reconnect + unconditional drain in the browser first. Evaluate PGMQ/Supabase separately against the companion handoff, including atomic notification/outbox behavior, tenant isolation, queue topology, scale, and full operating cost.
3. Make every shell use the same receive/apply/ack protocol. Verify duplicates, delayed push, missed Realtime/STOMP signals, crashes during apply, and app reinstall/recovery.

**Exit gate:** message delivery does not depend on a continuously connected client or a delivered push; retries and duplicates cannot apply a transaction twice.

### Phase 5 — Release and maintain

1. Define separate browser, desktop, and mobile build/release jobs, versioning, signing, crash reporting, and rollback procedures. Keep credentials and signing materials out of the repository and client bundles.
2. Publish a supported-platform matrix and state the background-sync guarantees precisely.
3. Track shell-specific dependencies and native SDK/plugin versions independently from shared app dependencies; remove a platform adapter only through a documented migration.

## Working on the native branch and the default branch

Use **two Git worktrees**, so both branches have separate working directories and can be open in separate editor windows. From the repository parent directory, after fetching the default branch if needed:

```bash
git worktree add -b <default-branch> ../perspectives-monorepo-main origin/<default-branch>
git worktree add -b native/mycontexts-shells ../perspectives-monorepo-native origin/<default-branch>
```

Replace `<default-branch>` with the repository's actual default branch. If the current checkout already is the default branch, it can serve as the first worktree; create only the native worktree. Do not check out the same branch in both worktrees.

- Use the main worktree for ordinary product changes and the native worktree for shell-specific work. Install dependencies in each worktree (`pnpm install`); pnpm may share its package store, but each checkout needs its own workspace links and generated build output.
- Keep native-only configuration and platform projects isolated. When native work reveals a general-purpose seam needed by browser/master, implement and review that seam as a small change on the default branch, then merge or cherry-pick it into the native branch.
- Regularly merge/rebase the default branch into the native branch and resolve conflicts there. Keep commits small, avoid long-lived divergence in shared UI/core files, and send independently useful shared changes upstream early.
- Run each dev server from its own worktree. Use distinct ports, browser profiles/app identifiers, and local test accounts/databases so the two versions do not overwrite each other's build artifacts or test data.
- Native builds may require platform-specific machines (notably macOS/Xcode for iOS signing/builds). Keep shell code portable and use CI or a suitable machine for platform builds rather than committing generated native build output.
