---
applyTo: "packages/perspectives-core/**"
---

# perspectives-core – Copilot Instructions

## What This Package Is

`perspectives-core` is the **Perspectives Distributed Runtime (PDR)** — the heart of the Perspectives project. It is written entirely in **PureScript** (0.15.15) and compiled with **spago** 0.93.44. After PureScript compilation, the output is bundled into a single ES module (`dist/perspectives-core.js`) using **Rollup**.

The PDR interprets models written in the **Perspectives Language (ARC)**, manages persistent state in **PouchDB** (in-browser document store), synchronises state between peers over **AMQP** (via Stomp), and exposes an API consumed by `perspectives-proxy` / `perspectives-react`.

---

## Directory Structure

```
src/
├── Main.purs                       # Entry point
├── globalState.purs                # Top-level state helpers
├── arcParser/                      # ARC language parser (multi-phase)
│   ├── arcAST.purs                 # AST data types
│   ├── arcParser.purs              # Phase 1: tokenize + parse → AST
│   ├── arcParserPhaseTwo.purs      # Phase 2: name resolution + type checking
│   ├── arcParserPhaseThree.purs    # Phase 3: inverted query indexing
│   └── ...
├── core/
│   ├── coreTypes.purs              # Central type definitions (MonadPerspectives, etc.)
│   ├── perspectivesState.purs      # Runtime state record and helpers
│   ├── perspectivesAPI.purs        # Public API (called by proxy)
│   ├── assignment/                 # Role/context creation, modification, deletion
│   ├── authentication/             # User authentication, keys
│   ├── computedValues/             # Computed role/property values, CouchDB functions
│   ├── dependencyTracking/         # Assumption tracking for reactive queries
│   ├── errorHandling/              # Error types and reporting
│   ├── instancePersistance/        # Create/save/load context and role instances
│   ├── instances/                  # Role and context instance getters
│   ├── modelTranslation/           # Translation table for model internationalisation
│   ├── persistence/                # CouchDB/PouchDB, delta store, version store
│   ├── queries/                    # Query evaluation engine
│   ├── sync/                       # Delta generation, transaction synchronisation
│   ├── typePersistence/            # DomeinFile (compiled model) persistence
│   ├── typeChecking/               # Type checker
│   └── typerepresentation/         # Runtime type representations
├── model/                          # Built-in ARC model files (*.arc)
└── amqpTransport/                  # AMQP/Stomp transport layer
```

---

## Core Architecture Concepts

### MonadPerspectives
The central monad is `MonadPerspectives`, defined in `coreTypes.purs`:
```
MonadPerspectives = ReaderT (AVar PerspectivesState) Aff
```
It gives access to a shared mutable state record (via an `AVar`) and runs asynchronous effects.

**Important monad aliases** (all defined in `coreTypes.purs`):
- `MP` = `MonadPerspectives`
- `MPT` = `MonadPerspectivesTransaction` — wraps `MonadPerspectives` with transaction state
- `MPQ` = `MonadPerspectivesQuery` = `ArrayT AssumptionTracking`
- `AssumptionTracking` = `WriterT (ArrayWithoutDoubles InformedAssumption) MonadPerspectives`

**Lifting**: `lift2InContext = lift <<< lift <<< lift` lifts `MonadPerspectives` into `InContext` (= `ReaderT Context MonadPerspectivesQuery`).

### State Record (PerspectivesState / PerspectivesExtraState)
State is a large record held in an AVar. Key fields (from `coreTypes.purs` and `perspectivesState.purs`):
- `domeinCache` — in-memory cache of compiled ARC models (`DomeinFile`)
- `contextInstances` / `rolInstances` — in-memory caches for instances
- `deltaCache` — LRU cache for deltas (per document ID)
- `resourceVersionCache` — LRU cache for resource versions
- `resourceDeltasCache` / `roleInstanceDeltasCache` — LRU caches for range query results

`DeltaStore` and `ResourceVersionStore` use `MonadPerspectives` (not `forall f. MonadPouchdb f`) to access these LRU caches via `gets _.deltaCache` / `gets _.resourceVersionCache`.

### ARC Parser (Multi-Phase)
1. **Phase 1** (`arcParser.purs`): tokenize and parse ARC source → raw AST (`arcAST.purs`)
2. **Phase 2** (`arcParserPhaseTwo.purs`): name resolution, type inference, perspective construction
3. **Phase 3** (`arcParserPhaseThree.purs`): inverted query indexing (which roles/properties track which queries)

The compiled output is a `DomeinFile` stored in PouchDB and cached in `domeinCache`.

### Delta System
Mutations produce **deltas** (change records). Delta types:
- `UniverseContextDelta` / `UniverseRoleDelta` — creation/deletion
- `ContextDelta` — role membership changes
- `RoleBindingDelta` — filler changes
- `RolePropertyDelta` — property changes

`DeltaStore` stores all deltas per document. `restoreResource` in `instancePersistance/restoreResource.purs` can reconstitute a missing resource by replaying its deltas.

### Screen Contextualisation
`screenContextualisation.purs` evaluates server-side conditions (`when` constructs, `condition` on MarkDown elements) during screen serialisation:
- `contextualiseScreen` must be used (not `addPerspectives`) for who/what/where screens so `WhenTableFormItemDef` wrappers are evaluated before serialisation.
- `allEnumeratedRoles` does **not** include the External role; check `roleType == externalRoleType contextType` before `allEnumeratedRoles`.


### Model files

ARC model files are in `src/model/`. They are compiled by the PDR and currently only through the MyContexts WebApp interface. They are stored in PouchDB as `DomeinFile` documents. The `domeinCache` field of the state record holds the in-memory cache of compiled models.
Models are published in a DNS domain. Each model URI is deterministically mapped to the URL where its DomeinFile can be found. They are stored in Couchdb. 
The most important domains are:
- perspectives.domains, which maps to https://perspectives.domains/models_perspectives_domains
- joopringelberg.nl, which maps to https://joopringelberg.nl/models_joopringelberg_nl
For example: the model model://perspectives.domains#System@6.3 can be found at 	https://perspectives.domains/models_perspectives_domains/perspectives_domains-tiodn6tcyc@6.3.json
Each model has three attachments:
- storedQueries.json: the inverted queries for detecting modelled state change and computing peers affected by a change;
- stableIdMapping.json: the mapping from stable IDs in the model to the generated internal CUIDs used in the compiled model;
- translationTable.json: the translation of type names, markdown texts, etc to internationalised versions (currently only English and Dutch).

---

## Build

```bash
# From packages/perspectives-core/
pnpm run build
# Equivalent to: pnpm run fmt && pnpm exec spago build && rollup -c
```

Steps:
1. `prebuild`: runs `transpile-model-deps.mjs` (may silently skip if offline)
2. `fmt`: formats all `.purs` files with `purs-tidy`
3. `spago build`: compiles PureScript to JS in `output/`
4. `rollup -c`: bundles `output/Main/index.js` → `dist/perspectives-core.js`

**Output**: `dist/perspectives-core.js` (ES module, with source map)

---

## Testing

```bash
pnpm exec spago test
```

Tests use the `test-unit` PureScript library. Test sources are in the `test/` directory of the sibling PureScript packages (`perspectives-apitypes`, `perspectives-utilities`, etc.). `perspectives-core` itself does not have a separate `test/` directory — integration is tested through the broader system.

---

## Local Spago Dependencies

`spago.yaml` workspace uses **local `path:` references** to sibling packages:
```yaml
extraPackages:
  apitypes:
    path: ../perspectives-apitypes
  avar-monadask:
    path: ../purescript-avar-monadask
  lrucache:
    path: ../purescript-lru-cache
  perspectives-utilities:
    path: ../perspectives-utilities
  serializablenonemptyarray:
    path: ../serialisable-nonempty-arrays
  subtlecrypto:
    path: node_modules/purescript-yoga-subtlecrypto
```

When publishing a standalone version, switch `path:` to `git: + ref:` (commented lines in `spago.yaml`). Always switch back to `path:` after publishing.

---

## Code Style

- All source files start with the GPL-3.0-or-later license header.
- Module names follow `Perspectives.*` hierarchy.
- Operator aliases `(##=)`, `(##>)`, `(##>>)`, `(###=)`, `(###>)`, `(###>>)` are defined in `coreTypes.purs` for query composition.
- Use `purs-tidy` for formatting — enforced by lint-staged.
- `Simple.JSON` is used for JSON serialisation/deserialisation throughout.

---

## Key Files for Common Tasks

| Task | File(s) |
|---|---|
| Add/modify API endpoint | `src/core/perspectivesAPI.purs` |
| Add a new delta type | `src/core/sync/`, `src/core/persistence/deltaStore.purs`, `src/core/persistence/deltaStoreTypes.purs` |
| Modify ARC syntax | `src/arcParser/arcAST.purs`, `src/arcParser/arcParser.purs` |
| Add a screen element type | `src/core/typerepresentation/screenDefinition.purs`, `src/arcParser/arcAST.purs`, `src/core/typePersistence/screenContextualisation.purs` |
| Add a computed value | `src/core/computedValues/` |
| Modify state record | `src/core/coreTypes.purs` (type def), `src/core/perspectivesState.purs` (constructor + helpers) |
| Add a new property range | `src/core/typerepresentation/` |
| Persistence layer change | `src/core/persistence/` |

---

## Common Pitfalls

1. **Never edit `output/`** — this is spago's generated output, overwritten on every build.
2. **Never edit `src/core/modelDependencies.purs`** — auto-generated by `scripts/transpile-model-deps.mjs`.
3. **LRU cache access requires `MonadPerspectives`**, not the more general `MonadPouchdb`. See `deltaStore.purs` and `resourceVersionStore.purs`.
4. **`DeltaStoreRecord` is in `deltaStoreTypes.purs`**, not `deltaStore.purs`, to avoid circular imports with `coreTypes.purs`.
5. **Inverted queries** must be re-indexed whenever query structure changes. Phase 3 of the ARC parser handles this.
6. **`populateScreen` must call `contextualiseScreen`** (not `addPerspectives`) for screens with `when` conditions; otherwise `WhenTableFormItemDef` wrappers survive to serialisation as `{ tag: "WhenTableFormDef" }`.
