---
applyTo: "packages/perspectives-pageworker/**"
---

# perspectives-pageworker – Copilot Instructions

## What This Package Is

`perspectives-pageworker` is the **Safari fallback** for running the Perspectives Distributed Runtime (PDR) in a browser. Safari does not support SharedWorkers, so the PDR runs instead in a dedicated **hosting page** (an `<iframe>` or popup window in the same origin).

The page worker connects to the core via an `InternalChannel` (same as the SharedWorker) and communicates with the client tab via the [Channel Messaging API](https://developer.mozilla.org/en-US/docs/Web/API/Channel_Messaging_API).

**Language**: JavaScript  
**Build tool**: Rollup  
**Output**: `dist/perspectives-sharedworker.js` (note: same filename as sharedworker for drop-in compatibility)

---

## Directory Structure

```
packages/perspectives-pageworker/
├── src/
│   ├── perspectives-pageworker.js   # Page worker entry point
│   └── perspectives-pageworker.d.ts # TypeScript declarations
├── dist/
├── rollup.config.js
└── package.json
```

---

## How It Works

The setup is analogous to `perspectives-sharedworker`, but runs in a normal page context rather than a SharedWorker:

1. `mycontexts` detects Safari (no SharedWorker support) and calls `configurePDRproxy("sharedWorkerChannel")` — the proxy handles the difference internally
2. The proxy loads this page worker instead of a SharedWorker
3. The PDR initialises in the page context and communicates back via channel messaging

---

## Build

```bash
cd packages/perspectives-pageworker
pnpm run build    # Rollup → dist/perspectives-pageworker.js
pnpm run watch    # Watch mode
```

---

## Common Pitfalls

1. **Output filename**: The output is named `perspectives-sharedworker.js` for drop-in compatibility with the SharedWorker deployment. Do not rename it.
2. **Safari-only path**: Changes here affect only Safari users. Always test in Chrome/Firefox with `perspectives-sharedworker` and in Safari with this package.
3. **Page lifecycle**: Unlike a SharedWorker, this "worker" lives in a normal page. It can be garbage-collected if the hosting page/iframe is closed.
