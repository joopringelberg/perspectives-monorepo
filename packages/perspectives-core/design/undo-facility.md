# Undo Facility

## Problem

The deterministic delta ordering design (see `deterministic-delta-ordering.md`)
requires the ability to roll back applied deltas in two scenarios:

1. **Conflict resolution:** a locally applied delta turns out to be the loser
   in a tiebreaker; its effect must be reversed before the winning delta is
   applied.
2. **Modify wins over delete:** a resource was physically deleted, but a
   modification delta arrives that should take precedence; the resource must be
   reconstructed.

As a secondary benefit, a limited user-facing undo feature can be offered.

## Design

### Prerequisite: monotonic delta-store

This design depends on the delta-store from the ordering design. Because that
store grows monotonically (deltas are never removed, only marked
`applied = true/false`), any prior state of a resource can be reconstructed by
replaying its deltas.

### Resource reconstruction

Implement:

```purescript
reconstructResource :: ResourceKey -> MonadPerspectives (Maybe Resource)
```

- Query the delta-store for all deltas with `applied = true` for the given
  resource-key, sorted by `resourceVersion`.
- Replay them sequentially. Skip delete-deltas when a later modify exists
  (per the "modify wins" rule).
- Reconstruct the `PerspectRol` or `PerspectContext`.
- **Replay order for roles:**
  `UniverseRoleDelta (ConstructEmptyRole)` →
  `ContextDelta (AddRoleInstancesToContext)` →
  `RoleBindingDelta` →
  `RolePropertyDelta`s.
  This matches the order already used in
  `src/core/assignment/serialiseAsDeltas.purs`.

### Inverse deltas

For each delta type, define its inverse:

| Delta type                    | Inverse                                              |
|-------------------------------|------------------------------------------------------|
| `ConstructEmptyContext`       | `RemoveExternalRoleInstance`                          |
| `ConstructEmptyRole`          | `RemoveRoleInstance`                                  |
| `AddRoleInstancesToContext`   | `RemoveRoleInstance` + context detachment             |
| `SetFirstBinding`             | `RemoveBinding`                                      |
| `ReplaceBinding`              | `SetFirstBinding` with `oldFiller`                   |
| `RemoveBinding`               | `SetFirstBinding` with previous filler (from store)  |
| `AddProperty`                 | `RemoveProperty` with same values                    |
| `SetProperty`                 | `SetProperty` with previous value (from store)       |
| `RemoveProperty`              | `AddProperty` with the removed values                |
| `DeleteProperty`              | `AddProperty` with all prior values                  |
| `RemoveRoleInstance`          | Full reconstruction via `reconstructResource`        |
| `RemoveExternalRoleInstance`  | Full context reconstruction                          |

Implement:

```purescript
inverseDelta :: SignedDelta -> DeltaStore -> MonadPerspectives (Maybe SignedDelta)
```

Some inverses require looking up prior values in the delta-store.

### Internal undo for conflict resolution

**Scenario A — Modify after delete:**

1. Resource R has been deleted (locally or via peer delta).
2. A modify-delta arrives with equal or higher `resourceVersion`.
3. `reconstructResource(R)` restores R from the delta-store.
4. Store R in the active database; update the context (`rolInContext`) if R is
   a role, or reconstruct the full context if R is an external role.
5. Apply the modify-delta.

**Scenario B — Losing local delta (tiebreaker):**

1. Local delta with `resourceVersion = n` has been applied.
2. Peer delta with `resourceVersion = n` arrives; peer wins tiebreaker.
3. Compute inverse of local delta, apply it.
4. Apply winning peer delta.
5. In delta-store: mark local delta `applied = false`, peer delta
   `applied = true`.

### User-facing undo (limited)

An end user can undo their most recent transaction:

- Maintain an `undoStack :: Array TransactionId` in application state (not
  persistent across restarts — acceptable).
- On "Undo": retrieve the most recent transaction's deltas from the
  delta-store.
- For each delta, check whether the resource-key's `resourceVersion` still
  equals the value left by the user's delta (= no peer has modified the
  resource since).
- **If yes:** compute and apply inverse deltas in reverse order; distribute
  them as a new transaction to peers (with incremented `resourceVersion`).
- **If no:** undo is not possible — inform the user.
- **Limitation:** undo applies only to the user's most recent transaction per
  resource-key. Deeper undo would conflict with peer modifications.

### Adapt deletion logic

Physical deletion in `saveUserData.purs` (`removeRoleInstance`,
`removeContextInstance`) and `persistent.purs` (`removeEntiteit`) becomes
**logical deletion**: remove the resource from cache and active database, but
retain all deltas in the delta-store.

Add a `deleted :: Boolean` flag per resource-key in the version-number map, so
that incoming modify-deltas can detect that reconstruction is needed.

### Delta-store compaction (optional, future)

The monotonic delta-store grows unboundedly. Optionally:

- After a configurable period (e.g. 30 days), deltas older than that period
  can be removed, provided the current resource state is correctly persisted.
- This limits undo capability in time.
- Implement as a background task.

## Verification

- **Convergence after rollback:** two installations modify the same property
  simultaneously — the loser rolls back — verify both end up with the same
  value.
- **Modify wins over delete:** delete a role on A, modify a property on that
  role on B — verify the role is restored on A with the correct property value.
- **User undo — success:** make a change, undo — verify the resource returns
  to its original state and peers receive the undo transaction.
- **User undo — blocked:** make a change, let a peer modify the resource,
  attempt undo — verify undo is refused and the user is informed.
- **Integration test:** multiple PouchDB instances with simulated latency and
  offline periods — verify eventual convergence.

## Decisions

| Decision          | Chosen                                                                              | Over                      |
|-------------------|-------------------------------------------------------------------------------------|---------------------------|
| Undo scope        | Primarily internal for conflict resolution; limited user feature as secondary benefit | User-feature only / internal only |
| Deletion model    | Logical (remove from active DB, keep deltas in store)                               | Physical (irrecoverable)  |
| User undo depth   | Most recent own transaction only                                                    | Arbitrary depth           |
| Compaction        | Deferred to future work                                                             | Immediate                 |
