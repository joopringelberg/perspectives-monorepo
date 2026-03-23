---
applyTo: "packages/serialisable-nonempty-arrays/**"
---

# serialisable-nonempty-arrays – Copilot Instructions

## What This Package Is

`serialisable-nonempty-arrays` is a **PureScript wrapper** for `Data.Array.NonEmpty` that adds `Decode`/`Encode` instances (argonaut) and `ReadForeign`/`WriteForeign` instances (simple-json). It makes non-empty arrays round-trip through JSON serialisation.

**Language**: PureScript 0.15.15  
**Build tool**: spago 0.93.44  
**Package name**: `serializablenonemptyarray`  
**Registry**: purs registry 60.6.0

---

## Directory Structure

```
packages/serialisable-nonempty-arrays/
├── src/
│   └── ...                    # Serialisable non-empty array wrapper
├── test/
├── spago.yaml
├── spago.lock
├── packages.dhall             # Legacy dhall config (kept for reference)
├── packages.template.dhall
└── package.json
```

---

## Build & Test

```bash
cd packages/serialisable-nonempty-arrays
pnpm exec spago build
pnpm exec spago test
pnpm run build    # also runs spago build
```

---

## Spago Dependencies

`spago.yaml` depends on:
- `perspectives-utilities` (local `path:` in monorepo)

When publishing standalone: switch `path:` to `git: + ref:`, bump `package.json` version, commit, tag, push. Then switch back to `path:`.

---

## Publishing

1. In `spago.yaml`: update `ref:` for `perspectives-utilities`, comment out `path:`, uncomment `git:` and `ref:`
2. Bump version in `package.json`
3. Commit, create tag, push tag
4. Switch `spago.yaml` back to local `path:` entry

---

## Common Pitfalls

1. **`path:` vs `git:` in spago.yaml** — always use `path:` locally; only switch to `git:` when publishing.
2. **`packages.dhall` is legacy** — the primary build config is `spago.yaml`. The `.dhall` files are kept for historical reference; do not use them for builds.
3. **`simple-json` is the serialisation library** — use `ReadForeign`/`WriteForeign` instances rather than argonaut where possible, for consistency with the rest of the Perspectives codebase.
