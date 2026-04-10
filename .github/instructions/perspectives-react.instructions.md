---
applyTo: "packages/perspectives-react/**"
---

# perspectives-react – Copilot Instructions

## What This Package Is

`perspectives-react` is the **reusable React UI component library** for the Perspectives project. It exports React components and TypeScript utilities that build a GUI which hides the connection to `perspectives-core` behind the `PDRproxy` abstraction from `perspectives-proxy`.

It is consumed by `mycontexts` and any other Perspectives-based client application.

**Language**: TypeScript / TSX (React components) + some plain JS  
**Build tool**: Rollup  
**Output**: `dist/perspectives-react-components.js` (ES module)

---

## Directory Structure

```
packages/perspectives-react/
├── src/
│   ├── perspectives-react-components.ts   # Package entry point / barrel export
│   ├── perspectivesshape.d.ts             # Core TypeScript types (API shape)
│   ├── perspectivesComponent.tsx          # Base reactive component class
│   ├── contextInstance.tsx                # Context instance rendering
│   ├── roleinstance.tsx                   # Role instance rendering
│   ├── screen.tsx                         # Screen dispatcher (model-driven screens)
│   ├── screendefinitioninterpreter.tsx    # Interprets screen definitions from PDR
│   ├── standardscreen.tsx                 # Standard screen layout
│   ├── perspectivebasedform.tsx           # Form rendering driven by perspectives
│   ├── perspectivetable.tsx               # Table rendering
│   ├── tableform.tsx                      # Combined table+form layout
│   ├── reactcontexts.tsx                  # React context providers/consumers
│   ├── urifunctions.tsx                   # ARC URI helpers
│   ├── modelDependencies_source.ts        # Source for auto-generated model deps
│   ├── modelDependencies.ts               # Auto-generated (do not edit)
│   ├── types/                             # Additional TypeScript type definitions
│   ├── lang/                              # i18n resources
│   └── notinuse/                          # Archived components (excluded from build)
├── rollup.config.js
├── tsconfig.json
├── package.json
└── bumpVersions.sh
```

---

## Build

```bash
cd packages/perspectives-react
pnpm run build
# equivalent to: rollup -c
```

The `prebuild` script runs `transpile-model-deps.mjs` (from `perspectives-core/scripts/`) to regenerate `src/modelDependencies.ts`.

```bash
pnpm run watch    # Rollup in watch mode
pnpm run lint     # ESLint
pnpm run lint:fix # ESLint with auto-fix
```

---

## Key Architectural Patterns

### PerspectivesComponent
`perspectivesComponent.tsx` is the base class for reactive components. It sets up subscriptions to the PDR via `PDRproxy` and re-renders when the PDR pushes updates.

### PDRproxy Usage
Components import `PDRproxy` (a Promise) from `perspectives-proxy` and call methods on it to:
- Query contexts and roles
- Subscribe to role/property changes (receiving callbacks)
- Execute actions

**Never import `perspectives-proxy` inside a component as a direct function call** — always await `PDRproxy` which is a Promise resolving to the proxy instance.

### Screen Definition Interpreter
`screendefinitioninterpreter.tsx` receives a serialised screen definition (JSON) from the PDR and renders it as React. The screen definition is produced by `perspectives-core`'s `contextualiseScreen`. Screen element types include:
- `TabDef` / `RowDef` / `ColumnDef`
- `TableDef` / `FormDef` / `TableFormDef`
- `MarkDownDef` / `MarkDownConstantDef`
- `WhenDef` (evaluated server-side; client only sees resolved elements)

### React Contexts
`reactcontexts.tsx` defines React context providers that make the current context instance, role instance, and perspective available deep in the component tree.

---

## External Dependencies (bundled vs external)

Per `rollup.config.js`:
- **Bundled**: `prop-types`, `react-loadable`, `perspectives-highlightjs`, markdown libraries, lodash, etc.
- **External** (not bundled, expected from calling app):
  - `react`, `react-dom`
  - `perspectives-proxy` (must be a shared singleton)
  - `perspectivesGlobals` (caller-provided global with `host` member)
  - `importModule` (caller-provided dynamic import wrapper)

---

## Common Tasks

| Task | File(s) |
|---|---|
| Add a new screen element type | `screendefinitioninterpreter.tsx`, `perspectivesshape.d.ts` |
| Add a new form control | `formcontrols.tsx`, `smartfieldcontrol.tsx` |
| Add a new action dropdown item | `actiondropdown.tsx` |
| Modify table rendering | `perspectivetable.tsx`, `tablerow.tsx`, `tablecell.tsx` |
| Add a new React context value | `reactcontexts.tsx` |
| Add i18n strings | `lang/` directory |
| Modify URI helper functions | `urifunctions.tsx` |

---

## Common Pitfalls

1. **`modelDependencies.ts` is auto-generated** — edit `modelDependencies_source.ts` instead.
2. **`perspectives-proxy` must be a singleton** — never bundle it; always declare as external in Rollup so the caller's instance is shared with `perspectives-core`.
3. **`perspectivesGlobals` and `importModule` are externals** — the calling application must provide them on `window`.
4. **`notinuse/` directory** — files here are explicitly excluded from the build. Do not import from them.
5. **Rollup, not Vite** — this package uses Rollup directly, not Vite. Configuration is in `rollup.config.js`.
