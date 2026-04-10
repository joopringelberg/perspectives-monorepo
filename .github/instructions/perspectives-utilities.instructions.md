---
applyTo: "packages/perspectives-utilities/**"
---

# perspectives-utilities – Copilot Instructions

## What This Package Is

`perspectives-utilities` is a **PureScript utility library** providing common helper functions used across multiple Perspectives packages (`perspectives-core`, `perspectives-apitypes`, `serialisable-nonempty-arrays`).

**Language**: PureScript 0.15.15  
**Build tool**: spago 0.93.44  
**Registry**: purs registry 60.6.0

---

## Directory Structure

```
packages/perspectives-utilities/
├── src/
│   └── Perspectives/
│       └── Utilities.purs    # Utility functions
├── test/
├── spago.yaml
├── spago.lock
└── package.json
```

---

## Build & Test

```bash
cd packages/perspectives-utilities
pnpm exec spago build    # Compile
pnpm exec spago test     # Run tests
```

This package has **no extra local dependencies** — `spago.yaml` only uses packages from the registry.

---

## Publishing

When releasing a standalone version:
1. In `package.json`: bump the version number
2. Commit, create tag, push tag

---

## Common Pitfalls

1. This package is a **leaf dependency** with no local path references — keep it free of dependencies on other Perspectives monorepo packages to avoid circular deps.
2. Functions added here should be genuinely reusable across at least two packages. Package-specific utilities belong in the package that uses them.
