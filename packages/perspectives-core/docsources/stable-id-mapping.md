## Stable ID mapping sidecar

This document explains how stable identifiers are handled during ARC compilation using a sidecar file stored next to each DomeinFile document.

### What is it?

- Sidecar filename: `stableIdMapping.json`
- Location: stored as an attachment alongside the DomeinFile document (same place as translation tables and stored queries)
- Purpose: keep type identifiers stable across model refactors by recording alias mappings and a snapshot of previously seen keys

### When is it used?

The mapping is applied after Phase Two and before Phase Three of the ARC pipeline.

1) Phase Two produces a `DomeinFileRecord` (DFR).
2) We load the sidecar (creating an empty one if missing).
3) We merge heuristically to augment alias maps based on the last-seen snapshot vs the current DFR.
4) We apply aliases into the DFR (contexts, enumerated roles, enumerated properties) without changing canonical keys.
5) Phase Three runs as usual on the alias-augmented DFR.
6) After Phase Three, CUIDs are assigned and the updated sidecar is persisted alongside the DomeinFile document.

### Data model

The sidecar stores two things:

- Aliases
  - `contexts :: Object String` — maps old context FQN to current canonical FQN
  - `roles :: Object String` — maps old role FQN to current canonical FQN
  - `properties :: Object String` — maps old property FQN to current canonical FQN
  - `views :: Object String` — maps old view FQN to current canonical FQN
  - `states :: Object String` — maps old state FQN to current canonical FQN
  - `actions :: Object String` — maps old action FQN to current canonical FQN
  - We never rename canonical keys. Instead we add alias keys, so both old and new identifiers are recognized.

- Snapshots (last seen)
  - `contextKeys :: Object ContextKeySnapshot`
  - `roleKeys :: Object RoleKeySnapshot`
  - `propertyKeys :: Object PropertyKeySnapshot`
  - `viewKeys :: Object ViewKeySnapshot`
  - `stateKeys :: Object StateKeySnapshot`
  - `actionKeys :: Object ActionKeySnapshot`
  - These capture lightweight features of each type (e.g., names, parents, local properties) to enable heuristic matching.

- Stable CUIDs (compact unique identifiers, one per type)
  - `contextCuids :: Object String` — canonical FQN → CUID
  - `roleCuids :: Object String`
  - `propertyCuids :: Object String`
  - `viewCuids :: Object String`
  - `stateCuids :: Object String`
  - `actionCuids :: Object String`
  - These are permanent. Once assigned, a CUID is never changed for the same type identity.

### Heuristics at a glance

- Keys: `ContextKey`, `RoleKey`, `PropertyKey`, `ViewKey` capture identifying features.
- Scoring: weighted similarity with `rank` and `rankBest` to match previous keys to current candidates.
- Thresholds: conservative cutoffs per category (contexts/roles ≥ 0.5; properties/views ≥ 0.7).
- Result: If a previous key best-matches a new key above the threshold, we add the previous FQN as an alias to the new FQN.

### Alias application

`applyAliases` inserts alias keys into the DFR tables for:

- Contexts: additional keys are added to the `contexts` table
- Enumerated roles: extra role keys land in `enumeratedRoles`
- Enumerated properties: extra property keys land in `enumeratedProperties`
- Views and states: similarly augmented

Canonical keys remain unchanged. The alias records ensure old FQNs continue to resolve.

### Cleanup / pruning of stale aliases

As a model evolves, aliases accumulate. A stale alias arises when:

1. Type `A` is renamed to `B` → alias `A → B` is recorded.
2. Later, a new type named `A` is introduced.

Without cleanup, the old alias `A → B` would silently redirect the new `A` to `B`'s data — the wrong type.

The function `pruneStaleAliases` (in `Perspectives.Sidecar.UniqueTypeNames`) removes alias entries whose key now appears as a canonical FQN in the current model. It also removes the corresponding CUID entry for the stale key so that the reintroduced type receives a freshly minted CUID rather than inheriting the one that belongs to the previously renamed type.

Pruning is applied **automatically** at the start of every `mergeStableIdMapping` and `planCuidAssignments` call — the modeller does not need to trigger it manually. After pruning:

- The new `A` is treated as a brand-new type and receives its own fresh CUID.
- The old `B` keeps its original CUID (unchanged).
- No stale redirect from `A` to `B` remains in the alias map.

`pruneStaleAliases` is also exported for use in tooling that needs explicit cleanup outside the normal compilation pipeline.

### Design notes

- Placement: Alias application is performed between Phase Two and Phase Three to maximize stability while keeping Phase Three unaware of the source of aliases.
- Growth control: Aliases accumulate over time but are automatically pruned when a previously-aliased FQN re-appears as a canonical type in the current model (see "Cleanup / pruning" above).
- Persistence: The updated sidecar (aliases + refreshed snapshots + CUIDs) is persisted as an attachment on the DomeinFile document after each successful compilation.

### Operational guarantees

- Empty sidecar is a no-op.
- A sidecar with snapshots but no aliases only starts adding aliases when a high-confidence rename is detected.
- Canonical identifiers are never rewritten — only supplemented.
- Stale aliases (alias key now canonical) are removed automatically on each compile.
