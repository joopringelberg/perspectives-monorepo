---
applyTo: "packages/purescript-avar-monadask/**"
---

# purescript-avar-monadask – Copilot Instructions

## What This Package Is

`purescript-avar-monadask` provides **AVar-backed shared mutable state** for PureScript programs that use `MonadAsk`. It gives a `StateT`-like interface over an `AVar` accessible through `ask`, enabling safe shared memory for parallel asynchronous processes.

This is a foundational dependency of `perspectives-core` — the `PerspectivesState` AVar is accessed through this interface.

**Language**: PureScript 0.15.15  
**Build tool**: spago 0.93.44  
**Package name**: `avar-monadask`  
**Registry**: purs registry 60.6.0

---

## Directory Structure

```
packages/purescript-avar-monadask/
├── src/
│   └── Control/
│       └── Monad/
│           └── AvarMonadAsk.purs    # Core module: get, gets, put, modify
├── spago.yaml
├── spago.lock
└── package.json
```

---

## API

The module `Control.Monad.AvarMonadAsk` provides:
- `get` — read the full state from the AVar
- `gets` — read a projected sub-value: `gets _.someField`
- `put` — replace the full state
- `modify` — update the state with a function

These mirror the `Control.Monad.State.Class` interface but operate on an AVar (safe for concurrent async use with `Aff`) rather than a pure `StateT`.

### Usage in perspectives-core
```purescript
-- Access deltaCache LRU cache:
gets _.deltaCache

-- Access resourceVersionCache:
gets _.resourceVersionCache
```

---

## Build & Test

```bash
cd packages/purescript-avar-monadask
pnpm exec spago build
```

No external local path dependencies — all dependencies come from the purs registry.

---

## Publishing

1. Bump version in `package.json`
2. Commit, create tag, push tag

---

## Common Pitfalls

1. **`MonadAff` constraint required** — callers must be in a monad that is a member of `MonadAff` to access the AVar. Pure `StateT` computations cannot use this.
2. **AVar concurrency** — `modify` is not atomic across multiple `Aff` fibers by default. If you need transactional updates spanning multiple operations, use `AVar.take` / `AVar.put` explicitly.
3. **No `StateT` wrapping** — this deliberately avoids `StateT` to prevent issues when combining with `Aff`-based concurrency (see the linked discussion in the README).
