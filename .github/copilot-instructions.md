# Perspectives Monorepo – Copilot Onboarding Guide

## Overview

This is the **Perspectives Monorepo**, consolidated from 12 previously independent repositories in spring 2025. It implements the **Perspectives Distributed Runtime (PDR)** and its surrounding tooling. The PDR is a peer-to-peer runtime that interprets models written in the **Perspectives Language (ARC)**, enabling structured co-operation between users without central servers.

The main product running on top of the PDR is **MyContexts** (`packages/mycontexts`), a browser-based progressive web app.

---

## Repository Structure

```
perspectives-monorepo/
├── packages/
│   ├── perspectives-core/          # PureScript PDR – the core runtime
│   ├── mycontexts/                 # React/Vite browser app (the end-user product)
│   ├── perspectives-react/         # Reusable React UI component library
│   ├── perspectives-proxy/         # TypeScript bridge between GUI and PDR
│   ├── perspectives-sharedworker/  # Browser SharedWorker shell for the PDR
│   ├── perspectives-pageworker/    # Page-hosted PDR worker (Safari fallback)
│   ├── perspectives-apitypes/      # PureScript API types shared by core and clients
│   ├── perspectives-utilities/     # PureScript utility functions
│   ├── perspectives-tcp/           # Node.js TCP/AMQP transaction collection server
│   ├── perspectives-highlightjs/   # Highlight.js language definition for ARC
│   ├── language-perspectives-arcII/# VS Code extension for ARC syntax coloring
│   ├── purescript-avar-monadask/   # PureScript: AVar in MonadAsk shared memory
│   ├── purescript-lru-cache/       # PureScript: LRU cache wrapper
│   └── serialisable-nonempty-arrays/ # PureScript: serializable non-empty arrays
├── .github/
│   ├── copilot-instructions.md     # This file
│   ├── instructions/               # Per-package Copilot instruction files
│   └── workflows/
│       └── weekly-report.yml
├── lerna.json                      # Lerna (version management, independent versioning)
├── pnpm-workspace.yaml             # pnpm workspace definition
└── package.json                    # Root workspace scripts
```

---

## Technology Stack

| Layer | Technology |
|---|---|
| Core runtime | **PureScript** 0.15.15, compiled with **spago** 0.93.44 |
| Core output | ES module bundle via **Rollup** |
| GUI library | **React** 19, **TypeScript** 5.8, built with **Rollup** |
| App | **Vite** 6, **TypeScript**, React, **pnpm** workspaces |
| Worker shells | Vanilla JS / TypeScript, bundled with Rollup |
| TCP server | **Node.js** / TypeScript |
| Package manager | **pnpm** 10.6.4 |
| Monorepo tooling | **Lerna** 8 (independent versioning), **pnpm** workspaces |
| Code formatting | **purs-tidy** (PureScript), **ESLint** (JS/TS) |
| Commits | **commitizen** + **commitlint** (Conventional Commits) |

---

## Key Concepts

### The Perspectives Language (ARC)
ARC is a domain-specific language for modelling structured co-operation. An ARC model describes:
- **Contexts** (e.g. a meeting, a sale) with a `domain` declaration
- **Roles** inside contexts (User roles, Enumerated roles, Calculated roles)
- **Perspectives** – what a user role can see and do
- **Properties** on roles
- **Actions** triggered by state conditions
- **Screens** – declarative UI definitions

### The PDR Architecture
The PDR runs in a browser **SharedWorker** (or a page worker on Safari). The GUI communicates with it via the `perspectives-proxy` layer:

```
MyContexts (React app)
  └── perspectives-react (React components)
        └── perspectives-proxy (TypeScript PDRproxy)
              └── perspectives-sharedworker / perspectives-pageworker
                    └── perspectives-core (PureScript, runs in worker)
                          └── PouchDB (in-browser document store)
```

### Inter-Package Dependency Graph (runtime)
```
perspectives-core
  ├── perspectives-apitypes    (PureScript API types)
  ├── perspectives-utilities   (PureScript utilities)
  ├── purescript-avar-monadask (AVar/MonadAsk)
  ├── purescript-lru-cache     (LRU caches)
  └── serialisable-nonempty-arrays
perspectives-react
  ├── perspectives-proxy
  └── perspectives-highlightjs
mycontexts
  ├── perspectives-core
  ├── perspectives-react
  ├── perspectives-proxy
  ├── perspectives-sharedworker
  └── perspectives-pageworker
```

---

## Build & Development Commands

### Root workspace
```bash
# Install all dependencies
pnpm install

# Build all JS/TS packages
pnpm run build

# Format all PureScript sources (purs-tidy)
pnpm run fmt

# Build all PureScript packages with spago
pnpm run ps:build

# Conventional commit helper
pnpm run commit
```

### Per-package (run from package directory or with pnpm -F)
```bash
# PureScript packages (perspectives-core, perspectives-apitypes, etc.)
pnpm exec spago build        # Compile PureScript
pnpm run build               # Full build (fmt + spago + rollup)
pnpm run fmt                 # Format PureScript with purs-tidy

# JavaScript/TypeScript packages
pnpm run build               # Rollup or Vite build
pnpm run watch               # Watch mode
pnpm run lint                # ESLint
```

### Versioning
```bash
pnpm run version:patch   # Bump patch version for all changed packages
pnpm run version:minor   # Bump minor version
pnpm run version:major   # Bump major version
```

---

## Spago Workspace (PureScript packages)

The PureScript packages (`perspectives-core`, `perspectives-apitypes`, `perspectives-utilities`, `purescript-avar-monadask`, `purescript-lru-cache`, `serialisable-nonempty-arrays`) each have a `spago.yaml` file.

In the monorepo, `perspectives-core/spago.yaml` references sibling packages via **local `path:` entries** (e.g. `path: ../perspectives-apitypes`). When publishing independently, these switch to `git:` + `ref:` entries. **Never commit with `git:` entries active** if you are working locally; keep `path:` entries enabled.

Registry used: **purs registry 60.6.0**

---

## Code Formatting Rules

- **PureScript**: always run `purs-tidy format-in-place` before committing (enforced by lint-staged on `.purs` files).
- **JavaScript/TypeScript**: ESLint with rules in `eslint.config.js` at root and per-package level.
- **Commits**: must follow Conventional Commits (`feat:`, `fix:`, `chore:`, etc.). Use `pnpm run commit` for the interactive commitizen prompt.

---

## Per-Package Instructions

Detailed instructions for each package are in `.github/instructions/`:

| Package | Instruction file |
|---|---|
| perspectives-core | [perspectives-core.instructions.md](instructions/perspectives-core.instructions.md) |
| mycontexts | [mycontexts.instructions.md](instructions/mycontexts.instructions.md) |
| perspectives-react | [perspectives-react.instructions.md](instructions/perspectives-react.instructions.md) |
| perspectives-proxy | [perspectives-proxy.instructions.md](instructions/perspectives-proxy.instructions.md) |
| perspectives-sharedworker | [perspectives-sharedworker.instructions.md](instructions/perspectives-sharedworker.instructions.md) |
| perspectives-pageworker | [perspectives-pageworker.instructions.md](instructions/perspectives-pageworker.instructions.md) |
| perspectives-apitypes | [perspectives-apitypes.instructions.md](instructions/perspectives-apitypes.instructions.md) |
| perspectives-utilities | [perspectives-utilities.instructions.md](instructions/perspectives-utilities.instructions.md) |
| perspectives-tcp | [perspectives-tcp.instructions.md](instructions/perspectives-tcp.instructions.md) |
| perspectives-highlightjs | [perspectives-highlightjs.instructions.md](instructions/perspectives-highlightjs.instructions.md) |
| language-perspectives-arcII | [language-perspectives-arcII.instructions.md](instructions/language-perspectives-arcII.instructions.md) |
| purescript-avar-monadask | [purescript-avar-monadask.instructions.md](instructions/purescript-avar-monadask.instructions.md) |
| purescript-lru-cache | [purescript-lru-cache.instructions.md](instructions/purescript-lru-cache.instructions.md) |
| serialisable-nonempty-arrays | [serialisable-nonempty-arrays.instructions.md](instructions/serialisable-nonempty-arrays.instructions.md) |

---

## Common Pitfalls

1. **spago `path:` vs `git:` entries** – always use `path:` locally; switch to `git:` only when publishing a standalone package version.
2. **PureScript output directory** – spago writes compiled output to `output/`. The rollup build reads from `output/Main/index.js`. Never edit files in `output/`.
3. **pnpm not npm** – always use `pnpm` (not `npm` or `yarn`). The `packageManager` field in `package.json` pins the version.
4. **purs-tidy before commit** – the pre-commit hook runs purs-tidy on staged `.purs` files. If you skip the hook, CI may reject misformatted code.
5. **Model dependencies sidecar** – `perspectives-core` has a `prebuild` script (`transpile-model-deps.mjs`) that generates `src/core/modelDependencies.purs`. It is allowed to fail silently (offline / no versions) and the build continues. Do not manually edit `src/core/modelDependencies.purs`; it is auto-generated.
6. **SSL certificates for local dev** – MyContexts requires self-signed certificates in a `certificates/` directory at the monorepo root (see `localdevelopment.md`).
