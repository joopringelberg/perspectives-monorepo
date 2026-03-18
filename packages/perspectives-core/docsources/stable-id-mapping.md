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

Note: write-back/persistence of the updated sidecar (aliases + refreshed snapshots) is intentionally deferred for now.

### Data model

The sidecar stores two things:

- Aliases
  - `contextAliases :: Map ContextFqn (Array ContextFqn)`
  - `roleAliases :: Map RoleFqn (Array RoleFqn)`
  - `propertyAliases :: Map PropertyFqn (Array PropertyFqn)`
  - We never rename canonical keys. Instead we add alias keys, so both old and new identifiers are recognized.

- Snapshots (last seen)
  - `contextKeys :: Array ContextKeySnapshot`
  - `roleKeys :: Array RoleKeySnapshot`
  - `propertyKeys :: Array PropertyKeySnapshot`
  - These capture lightweight features of each type (e.g., names, parents, local properties) to enable heuristic matching.

### Heuristics at a glance

- Keys: `ContextKey`, `RoleKey`, `PropertyKey` capture identifying features.
- Scoring: weighted similarity with `rank` and `rankBest` to match previous keys to current candidates.
- Thresholds: conservative cutoffs per category (e.g., contexts/roles around 0.5; properties slightly higher).
- Result: If a previous key best-matches a new key above the threshold, we add the previous FQN as an alias to the new FQN.

### Alias application

`applyAliases` inserts alias keys into the DFR tables for:

- Contexts: additional keys are added to the `contexts` table
- Enumerated roles: extra role keys land in `enumeratedRoles`
- Enumerated properties: extra property keys land in `enumeratedProperties`

Canonical keys remain unchanged. The alias records ensure old FQNs continue to resolve.

### Design notes

- Placement: Mapping is applied between Phase Two and Phase Three to maximize stability while keeping Phase Three unaware of the source of aliases.
- Growth: Adding aliases increases the tables. Downstream code that enumerates types should either dedupe or operate on canonical sets when needed.
- Persistence: The current implementation updates the mapping in-memory. Persisting the augmented sidecar is planned as a follow-up.

### Operational guarantees

- Empty sidecar is a no-op.
- A sidecar with snapshots but no aliases only starts adding aliases when a high-confidence rename is detected.
- Canonical identifiers are never rewritten—only supplemented.

### Next steps (optional)

- Persist updated `stableIdMapping.json` after successful compilation.
- Extend alias injection if additional reference-bearing structures require it.
- Document any deduplication constraints for downstream enumeration users.
