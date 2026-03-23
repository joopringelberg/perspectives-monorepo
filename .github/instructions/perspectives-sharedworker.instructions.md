---
applyTo: "packages/perspectives-sharedworker/**"
---

# perspectives-sharedworker – Copilot Instructions

## What This Package Is

`perspectives-sharedworker` provides the **browser SharedWorker shell** that hosts the Perspectives Distributed Runtime (PDR). It wraps `perspectives-core` so the PDR runs in a [SharedWorker](https://developer.mozilla.org/en-US/docs/Web/API/SharedWorker), shared across all browser tabs of the same origin.

Used on browsers that support SharedWorker: **Chrome, Firefox, Edge**.

For browsers that do not support SharedWorker (Safari), see `perspectives-pageworker`.

**Language**: JavaScript (single source file)  
**Build tool**: Rollup  
**Output**: `dist/perspectives-sharedworker.js`

---

## Directory Structure

```
packages/perspectives-sharedworker/
├── src/
│   └── perspectives-sharedworker.js   # SharedWorker entry point
├── dist/                               # Build output
├── rollup.config.js
└── package.json
```

---

## How It Works

The SharedWorker connects to `perspectives-core` via an **InternalChannel** instance (created by the PDR itself) and connects to GUI clients via the [Channel Messaging API](https://developer.mozilla.org/en-US/docs/Web/API/Channel_Messaging_API). Multiple browser tabs can connect to the same SharedWorker instance.

Setup flow:
1. `mycontexts` (or another client) calls `configurePDRproxy("sharedWorkerChannel")` from `perspectives-proxy`
2. `perspectives-proxy` creates a `SharedWorkerChannel` that points to this worker
3. The worker loads `perspectives-core` and sets up the InternalChannel
4. Messages are relayed between the channel port and the PDR's coroutine producer

---

## Build

```bash
cd packages/perspectives-sharedworker
pnpm run build    # Rollup → dist/perspectives-sharedworker.js
pnpm run watch    # Watch mode
```

---

## Common Pitfalls

1. **SharedWorker lifecycle**: A SharedWorker stays alive as long as at least one connecting tab is open. State is shared across tabs — be careful about concurrent mutations.
2. **No direct DOM access**: SharedWorkers run in a worker context without access to `window` or DOM.
3. **Bundling**: The worker file is separately bundled by Rollup; it is loaded by the browser via a `new SharedWorker(url)` call in `perspectives-proxy`. The URL must be correct relative to the deployed app.
