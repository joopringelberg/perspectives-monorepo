---
applyTo: "packages/perspectives-apitypes/**"
---

# perspectives-apitypes – Copilot Instructions

## What This Package Is

`perspectives-apitypes` defines the **PureScript API types** used for communication between `perspectives-core` and its clients (via TCP or internal channel). It also defines the serialisation format for creating contexts and roles through the API.

**Language**: PureScript 0.15.15  
**Build tool**: spago 0.93.44  
**Registry**: purs registry 60.6.0

---

## Directory Structure

```
packages/perspectives-apitypes/
├── src/
│   └── Perspectives/
│       └── ApiTypes.purs          # Request/Response types, RequestType enum, serialisation
├── test/                          # spago test sources
├── spago.yaml
├── spago.lock
└── package.json
```

---

## Key Types

- **`Request`** — a record containing a `RequestType` and parameters; sent by clients to the PDR
- **`Response`** — a record returned by the PDR to clients
- **`RequestType`** — enumeration of all supported API calls (e.g. `GetRol`, `GetRolBinding`, `CreateContext`, etc.)
- Context/Role serialisation format — simplified records (not full `PerspectContext`/`PerspectRol`) for transporting created resources through the API

### Namespace prefix expansion (automatic)
| Prefix | Namespace |
|---|---|
| `sys` | `model:System` |
| `usr` | `model:User` |
| `cdb` | `model:Couchdb` |

### Indexed name expansion (automatic)
| Indexed Name | Expansion |
|---|---|
| `usr:Me` | `model:User$MijnSysteem$User_0001` |

---

## Build & Test

```bash
cd packages/perspectives-apitypes
pnpm exec spago build   # Compile PureScript
pnpm run test           # Run tests with spago test
```

---

## Spago Dependencies

`spago.yaml` depends on:
- `perspectives-utilities` (local `path:` in monorepo)
- `serializablenonemptyarray` (local `path:` in monorepo)
- `simple-json` for JSON serialisation

When publishing standalone, switch `path:` entries to `git: + ref:` (commented lines in `spago.yaml`). Switch back to `path:` when developing locally.

---

## Publishing

When releasing a standalone version:
1. In `spago.yaml`: update `ref:` values for `perspectives-utilities` and `serializablenonemptyarray`, and comment out their `path:` sections
2. In `package.json`: bump the version number
3. Commit, create tag, push tag
4. Switch `spago.yaml` back to local `path:` entries

---

## Common Pitfalls

1. **`simple-json`** is the serialisation library — use `readJSON'` / `writeJSON` throughout; do not introduce `argonaut` or other JSON libraries.
2. **Namespace/indexed name expansion** happens automatically — do not manually expand these in API callers.
3. **Circular dependency risk** — this package is imported by `perspectives-core`; be careful not to introduce reverse dependencies.
