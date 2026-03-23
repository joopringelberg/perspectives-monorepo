---
applyTo: "packages/purescript-lru-cache/**"
---

# purescript-lru-cache – Copilot Instructions

## What This Package Is

`purescript-lru-cache` is a **PureScript wrapper** around the JavaScript [`lru-cache`](https://github.com/isaacs/node-lru-cache) library. It provides fixed-size caches with a **Least Recently Used** eviction policy, used extensively in `perspectives-core` for caching deltas, resource versions, and query results.

**Language**: PureScript 0.15.15 (with JavaScript FFI)  
**Build tool**: spago 0.93.44  
**Package name**: `perspectives-lru-cache` (alias `lrucache` in `perspectives-core`'s spago.yaml)  
**Registry**: purs registry 60.6.0

---

## Directory Structure

```
packages/purescript-lru-cache/
├── src/
│   └── LRUCache.purs          # PureScript API; wraps lru-cache JS library
├── test/
├── spago.yaml
├── spago.lock
└── package.json
```

---

## API

Module: `LRUCache`

Key functions:
- `newCache :: CreateOptions -> Effect (Cache k v)` — create a new LRU cache
- `set :: Cache k v -> k -> v -> Effect Unit` — insert/update an entry
- `get :: Cache k v -> k -> GetOptions -> Effect (Maybe v)` — retrieve an entry
- `delete :: Cache k v -> k -> Effect Unit` — remove an entry
- `clear :: Cache k v -> Effect Unit` — empty the cache
- `defaultCreateOptions` / `defaultGetOptions` — sensible defaults

### Caches in perspectives-core
The `PerspectivesExtraState` record (in `coreTypes.purs`) holds four LRU cache fields:
- `deltaCache :: DeltaCache` — per-document-ID delta lists
- `resourceVersionCache :: ResourceVersionCache` — per-safe-key resource versions
- `resourceDeltasCache :: ResourceDeltasCache` — per-resource range query results
- `roleInstanceDeltasCache :: RoleInstanceDeltasCache` — per-role-instance range query results

`deltaCache` and `resourceVersionCache` are used by `DeltaStore` and `ResourceVersionStore`. These two stores use `MonadPerspectives` (not `MonadPouchdb`) specifically to access the caches via `gets _.deltaCache`.

The two result-set caches (`resourceDeltasCache`, `roleInstanceDeltasCache`) are invalidated by `storeDelta` (on new delta) and `updateDeltaApplied`.

---

## Limitations

This library does **not** support:
- Keys other than `String`
- The `fetch` method of lru-cache
- Reading cache configuration options after creation
- The `thisp` context object in `forEach`

---

## Build & Test

```bash
cd packages/purescript-lru-cache
pnpm exec spago build
pnpm exec spago test
```

---

## Publishing

1. Bump version in `package.json`
2. Commit, create tag, push tag

---

## Common Pitfalls

1. **String-only keys** — all cache keys must be strings. There is no type-level enforcement of this beyond the PureScript type signature.
2. **Effect, not Aff** — all cache operations are `Effect`, not `Aff`. They are synchronous FFI calls to the underlying JS lru-cache. Wrap in `liftEffect` when using inside `Aff` or `MonadPerspectives`.
3. **Cache misses return `Nothing`** — always handle the `Maybe` from `get`; do not use `unsafePartial fromJust`.
