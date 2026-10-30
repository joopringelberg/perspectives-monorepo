# MyContexts / Perspectives — Browser, PWA and Native Application Handoff

**Status:** synthesis of a separate earlier browser-vs-native design discussion and related project context; not an approved implementation plan. **Prepared:** 2026-10-28.

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
