---
applyTo: "packages/perspectives-proxy/**"
---

# perspectives-proxy – Copilot Instructions

## What This Package Is

`perspectives-proxy` is the **TypeScript bridge** between GUI clients (React components) and the Perspectives Distributed Runtime (PDR). It exposes a single shared `PDRproxy` Promise that resolves to a `PerspectivesProxy` instance, hiding the underlying communication channel from the client.

It supports three connection architectures:
1. **Internal channel** — core and GUI in the same JS process (Electron / integrated client)
2. **SharedWorker channel** — core in a SharedWorker, GUI in a browser tab (Chrome/Firefox)
3. **Page worker channel** — core in a hosting page, GUI in a browser tab (Safari fallback)

(TCP architecture was made obsolete and is commented out as a reference.)

**Language**: TypeScript  
**Build tool**: Rollup (produces a UMD module for singleton sharing)  
**Output**: `dist/perspectives-proxy.js`

---

## Directory Structure

```
packages/perspectives-proxy/
├── src/
│   ├── perspectives-proxy.ts      # Main implementation; PDRproxy, channels, PerspectivesProxy class
│   ├── perspectivesshape.d.ts     # TypeScript type definitions for the API shape
│   ├── globals.d.ts               # Global ambient type declarations
│   ├── declarations.d.ts          # Module declarations
│   └── types/                     # Additional type files
├── rollup.config.js
├── tsconfig.json
└── package.json
```

---

## Key Concepts

### PDRproxy Promise
```typescript
// consumers import:
import { PDRproxy } from 'perspectives-proxy';

// and then await it:
const proxy = await PDRproxy;
proxy.getRol(contextId, roleType, callback);
```

The promise resolves once one of the `configurePDRproxy` functions is called.

### Channel Types
- **`InternalChannel`** — direct function calls when core runs in the same process. The core calls `createRequestEmitterImpl` to set up a coroutine Producer; the proxy consumes it.
- **`SharedWorkerChannel`** — uses the [Channel Messaging API](https://developer.mozilla.org/en-US/docs/Web/API/Channel_Messaging_API) to communicate with a SharedWorker.
- **`PageWorkerChannel`** — same messaging API, but the worker runs in a dedicated hosting page (Safari).

### configurePDRproxy
Clients call one of these to fulfil `PDRproxy`:
```typescript
configurePDRproxy("sharedWorkerChannel")   // MyContexts (browser, SharedWorker/PageWorker)
configurePDRproxy("internalChannel")        // Integrated / Electron client
```

### PerspectivesProxy class
Wraps the channel and exposes typed methods corresponding to PDR API calls (getRol, getProperty, createContext, etc.). All methods use callbacks/subscriptions, not Promises, matching the PDR's coroutine-based push model.

---

## Build

```bash
cd packages/perspectives-proxy
pnpm run build    # Rollup → dist/perspectives-proxy.js
pnpm run watch    # Watch mode
pnpm run lint     # ESLint
```

---

## Design Notes

- **Singleton requirement**: The proxy must be a single shared instance. `mycontexts` bundles it; `perspectives-react` and `perspectives-core` declare it as an external dependency so they all share the same instance.
- **UMD output**: The Rollup config produces a UMD module so it can be loaded as a global in the browser and also imported as a module.
- **TypeScript shape definitions**: `perspectivesshape.d.ts` is the canonical source of truth for the API types. It is also re-exported by `perspectives-react` for use by client applications.

---

## Common Pitfalls

1. **Only `mycontexts` bundles this package** — `perspectives-react` and `perspectives-core` both declare `perspectives-proxy` as an external. Never bundle it into a library.
2. **`PDRproxy` resolves only once** — calling `configurePDRproxy` a second time will have no effect. Ensure it is called exactly once during app startup.
3. **Callback-based API** — methods on `PerspectivesProxy` use callbacks (not Promises) because the PDR pushes updates reactively. Wrap in a Promise only when you need a one-shot value.
