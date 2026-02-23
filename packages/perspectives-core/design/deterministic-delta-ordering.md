# Deterministic Delta Ordering

## Problem

Perspectives is a distributed system where multiple peers can simultaneously
modify the same resources (contexts, roles, bindings, properties). Deltas
describing these mutations can arrive at different installations in different
orders — due to transport delays or offline periods. Currently there is no
ordering or conflict resolution mechanism; only idempotency checks prevent
duplicate resource creation.

We need a scheme that guarantees all installations converge to the same state,
regardless of the order in which deltas arrive.

## Design

### Core concept: resource version number

Each **resource-key** carries a single version number (`resourceVersion :: Int`).
This number is a property of the *state of the resource*, not of any particular
author. Every delta that mutates a resource-key increments this number by one.
The version number is included in the delta so that receivers can detect gaps
and conflicts.

### Resource-key granularity

Granularity is at the property level. Each delta operates on exactly one
resource-key:

| Delta type              | Resource-key                              | Example                                       |
|-------------------------|-------------------------------------------|-----------------------------------------------|
| `UniverseContextDelta`  | `ContextInstance`                          | `"model://perspectives.domains#Sys$Ctx$0001"` |
| `UniverseRoleDelta`     | `RoleInstance`                             | `"model://...#Guest$0001"`                     |
| `ContextDelta`          | `RoleInstance`                             | (role-to-context link)                         |
| `RoleBindingDelta`      | `RoleInstance + "#binding"`               | `"...#Guest$0001#binding"`                     |
| `RolePropertyDelta`     | `RoleInstance + "#" + PropertyType`        | `"...#Guest$0001#model://...#Name"`            |

**Required change:** `UniverseRoleDelta` currently operates on
`roleInstances :: SerializableNonEmptyArray RoleInstance`. This must be split
into individual deltas (one per instance) so that each delta maps to exactly
one resource-key. Affected files:

- `src/core/sync/typesForDelta.purs`
- `src/core/instancePersistance/saveUserData.purs`
- `src/core/assignment/update.purs`

### New fields on deltas

Add to `SignedDelta` (in `src/core/sync/signedDelta.purs`), or to the
serialised delta payload within `encryptedDelta`:

```purescript
resourceVersion :: Int    -- version of the resource *after* this delta
resourceKey     :: String -- identifies the resource this delta operates on
```

### Version number administration

Maintain a persistent `Map ResourceKey Int` in a dedicated PouchDB database
(robust across restarts).

Rules:

| Event                                    | Action                                                                   |
|------------------------------------------|--------------------------------------------------------------------------|
| Resource created                         | `version = 0`                                                            |
| Local mutation                           | Read `n`, stamp delta with `resourceVersion = n + 1`, store `n + 1`      |
| Received delta, `v = n+1`               | Apply delta, set local version to `v`                                    |
| Received delta, `v > n+1`               | Gap detected — block entire transaction (see below)                      |
| Received delta, `v = n`, different value | Conflict — resolve (see below)                                           |
| Received delta, `v <= n`, no conflict    | Outdated — store in delta-store, do not apply                            |

### Conflict resolution: Last-Writer-Wins with tiebreaker

When two deltas target the same resource-key with the same `resourceVersion`
but from different authors:

1. **Tiebreaker:** the author whose `PerspectivesUser` identifier is
   alphabetically higher wins.
2. **Winner** is applied (or was already applied if it is the local delta).
3. **Loser** is stored in the delta-store with `applied = false`. If the local
   delta is the loser: **roll back** the local change and apply the winning
   delta.

**Exception — modify always wins over delete:**
If one of the conflicting deltas is a deletion and the other a modification,
the modification wins regardless of author ID. If the deletion was already
executed: reconstruct the resource from the delta-store (see Undo design) and
apply the modification.

### Transaction blocking on gaps

In `handleTransaction.purs` (`executeTransaction`):

- On receipt: scan all deltas in the transaction. If any delta has
  `resourceVersion > local + 1` for any resource-key, deltas are missing.
  **Block the entire transaction.** This is necessary because automatic actions
  (state-triggered effects) can be causally dependent on earlier deltas within
  the same transaction.
- **Persistent storage:** blocked transactions are stored in a
  `pendingTransactions` PouchDB database, indexed by the
  `(author, resourceKey, expected version)` tuple that is missing. A session
  can be interrupted at any moment; on restart, pending transactions are
  reloaded.
- **Unblocking:** whenever a delta is processed that advances a resource-key's
  version, check whether any pending transactions are now unblocked (all gaps
  filled).
- **Timeout:** after a configurable period (e.g. 24 hours), a pending
  transaction may be executed anyway to prevent indefinite blocking. Conflicts
  are then handled per the rules above.

### Delta-store

A new PouchDB database replaces the current practice of storing deltas on
resources themselves.

- **Document ID:** `<resourceKey>_<resourceVersion>_<author>` (deterministic,
  unique — in a conflict two deltas share `resourceVersion` but have different
  authors).
- **Fields:**
  - `resourceKey :: String`
  - `resourceVersion :: Int`
  - `author :: PerspectivesUser`
  - `signedDelta :: SignedDelta`
  - `deltaType :: String` (for deserialization)
  - `applied :: Boolean`
- All deltas are stored here — locally created and received.
- **Replaces** current storage on resources: remove `universeContextDelta`,
  `universeRoleDelta`, `contextDelta`, `bindingDelta`, `propertyDeltas` from
  `PerspectContext` and `PerspectRol` in
  `src/core/instances/instanceRepresentation.purs`.
- Update all accessor functions in `src/core/instances/contextAndRole.purs`
  to read from the delta-store.

### Rewrite serialiseAsDeltas

`src/core/assignment/serialiseAsDeltas.purs` currently reads deltas from
resources. Rewrite to query the delta-store per resource-key, sorted by
`resourceVersion`.

### Adapt delta construction

All functions that construct deltas must populate `resourceVersion` and
`resourceKey`:

- `constructEmptyContext` in `src/core/instancePersistance/createContext.purs`
- `addRoleInstanceToContext`, `removeRoleInstancesFromContext`, `addProperty`,
  `removeProperty`, `deleteProperty`, `setProperty` in
  `src/core/assignment/update.purs`
- `setFirstBinding`, `replaceBinding`, `removeBinding`,
  `synchroniseRoleRemoval` in `src/core/instancePersistance/saveUserData.purs`

### Adapt TransactionForPeer

The `SignedDelta` payload (serialised JSON within `encryptedDelta`) now
includes `resourceVersion` and `resourceKey`. The trial deserialization in
`executeDelta` is automatically compatible once the types are updated.

## Verification

- **Convergence test:** two installations modify the same property
  simultaneously (same `resourceVersion`) — verify both converge to the same
  value (winner on author ID).
- **Modify-wins-over-delete:** delete a role on installation A, modify a
  property on that role on installation B — verify the role is restored on A.
- **Gap detection:** send transactions out of order — verify the blocked
  transaction is only executed once the gap is filled.
- **Pending persistence:** block a transaction, restart the session — verify
  the pending transaction is still present and processed when the gap is filled.

## Decisions

| Decision                | Chosen                                                              | Over                                         |
|-------------------------|---------------------------------------------------------------------|----------------------------------------------|
| Ordering mechanism      | Single resource version number                                      | Lamport clock + per-author seq nr            |
| Resource-key granularity| Property level                                                      | Instance level                               |
| Delete vs. modify       | Modify always wins                                                  | LWW / delete wins                            |
| Gap handling            | Block entire transaction                                            | Selective buffering / execute + correct       |
| Pending storage         | Persistent (PouchDB)                                                | In-memory only                               |
| Delta storage           | Separate delta-store, no duplication on resources                   | On resources / both                          |
| Conflict UX             | Last-Writer-Wins, no user interaction                               | Show conflict to user                        |
