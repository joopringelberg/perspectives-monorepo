# Lamport Clocks for Per-Author Delta Ordering

This document describes the per-author sequence number tracking (Lamport clock) mechanism used to detect missed or out-of-order deltas in the collaborative sync protocol, and how it is used to implement modify-wins-over-delete conflict resolution.

> **Source modules:** `Perspectives.Sync.SignedDelta` (`signedDelta.purs`), `Perspectives.PerspectivesState` (`perspectivesState.purs`), `Perspectives.Authenticate` (`authenticate.purs`), `Perspectives.Sync.HandleTransaction` (`handleTransaction.purs`), `Perspectives.SaveUserData` (`saveUserData.purs`), `Perspectives.TypesForDeltas` (`typesForDelta.purs`).

---

## Background

In Perspectives, peers collaborate by exchanging _transactions_: ordered collections of signed deltas. Because peers can go offline and work independently, there is no global clock. Deltas from different authors are interleaved non-deterministically. Two problems arise:

1. **Missed deltas** – A peer may go offline and miss some deltas from another author.
2. **Concurrent modifications** – Two peers can independently modify or delete the same resource while both are offline. When they reconnect, their transactions must be reconciled.

Lamport clocks (per-author monotonically increasing sequence numbers) provide a lightweight logical time that solves both problems without requiring a global clock or a shared database.

---

## Data Model

### `SignedDelta` (`signedDelta.purs`)

Every delta is wrapped in a `SignedDelta` record:

```purescript
newtype SignedDelta = SignedDelta
  { author          :: PerspectivesUser
  , encryptedDelta  :: String
  , signature       :: Maybe String
  , sequenceNumber  :: Maybe Int      -- Lamport clock: author's counter at signing time
  }
```

`sequenceNumber` is `Maybe Int` for backward compatibility: existing deltas stored in the delta-store before this feature was introduced are deserialized with `sequenceNumber = Nothing`.

### `PerspectivesExtraState` (`coreTypes.purs`)

Two fields are added to the runtime state:

```purescript
, outgoingSequenceNumber  :: Int                    -- next seq to assign when signing a delta
, incomingSequenceNumbers :: Map PerspectivesUser Int  -- per-author expected-next incoming seq
```

---

## Signing (`authenticate.purs`)

`signDelta` calls `nextOutgoingSequenceNumber` (get-and-increment) and embeds the result:

```purescript
signDelta encryptedDelta = do
  author  <- lift getPerspectivesUser
  seqNum  <- lift nextOutgoingSequenceNumber
  ...
  pure $ SignedDelta { author, encryptedDelta, signature, sequenceNumber: Just seqNum }
```

`nextOutgoingSequenceNumber` is safe within a `MonadPerspectivesTransaction` because transaction execution is serialized via `transactionFlag`.

---

## Receiving (`handleTransaction.purs`)

`executeTransaction'` calls `checkSequenceNumber` before processing each delta:

```purescript
verifyAndExcecuteDelta s@(SignedDelta { author, sequenceNumber }) = do
  checkSequenceNumber author sequenceNumber
  (lift $ verifyDelta s) >>= executeDelta s
```

`checkSequenceNumber` compares the incoming `seqNum` against `incomingSequenceNumbers[author]`:

| Relation | Meaning | Action |
|---|---|---|
| `seqNum > expected` | **Gap** – some deltas were missed | Log warning; advance counter; process delta |
| `seqNum == expected` | **Expected** | Advance counter; process delta |
| `seqNum < expected` | **Duplicate or out-of-order** | Log note; do not advance counter |

Recovery of missed deltas (gap case) is not yet implemented; the counter is advanced to resume tracking from the new position.

---

## Modify-Wins-Over-Delete (`saveUserData.purs`, `handleTransaction.purs`)

### Problem

Two peers A and B go offline simultaneously:
- **A** deletes role instance R.
- **B** modifies a property of R.

When they reconnect:
- A receives B's modification → A must **restore** R and apply the modification.
- B receives A's deletion → B must **ignore** the deletion.

### Solution: Lamport-clock snapshot in deletion deltas

#### On the deleting side (`saveUserData.purs` – `synchroniseRoleRemoval`)

When A creates a `RemoveRoleInstance` delta for R, it reads the role's stored `propertyDeltas` to find all authors who have modified the role's properties. For each such author X, it looks up `incomingSequenceNumbers[X]` — the _expected-next_ sequence number from X at deletion time — and stores the result in the delta:

```
knownModifierSeqs :: Maybe (Object Int)
-- maps authorString → expectedNextSeqNum from that author (at deletion time)
```

This snapshot encodes "how up-to-date A was with each modifier's work at the time of deletion".

#### On the receiving side (`handleTransaction.purs` – `executeUniverseRoleDelta`)

When B receives A's `RemoveRoleInstance` delta:

1. B looks up the local copy of R (if any).
2. B calls `hasConcurrentModification localPropertyDeltas knownModifierSeqs`.
3. For each local property delta with `author = X` and `sequenceNumber = N`:
   - `expectedNext = knownModifierSeqs[X]` (or 0 if X is absent from the map).
   - If `N >= expectedNext`: A had **not** processed this delta from X → it was created concurrently → **modification wins**.
4. If any local delta is concurrent, the deletion is ignored (logged). Otherwise, the deletion proceeds.

#### Correctness

| Scenario | Result |
|---|---|
| B offline, modified R at seq N_B; A offline, deleted R without seeing N_B | `knownModifierSeqs` omits B (or has B's old expectedNext ≤ N_B) → `N_B >= expectedNext` → **modification wins** ✓ |
| A was online when B modified R, then A intentionally deleted R | A had processed N_B, so `expectedNext = N_B + 1 > N_B` → `N_B < expectedNext` → **deletion wins** ✓ |
| Old delta without `knownModifierSeqs` (backward-compatible) | `knownModifierSeqs = Nothing` → `hasConcurrentModification` returns `false` → **deletion proceeds** ✓ |

#### Role restoration (Case 1)

For the case where A (the deleter) receives B's property modification for a role A has already deleted, restoration is enabled by including the role's construction deltas (`universeRoleDelta`, `contextDelta`) in B's modification transaction. When A processes these, the construction of R is idempotent (no-op if R already exists), and then the property delta is applied normally.

---

## Helper Functions (`perspectivesState.purs`)

| Function | Signature | Purpose |
|---|---|---|
| `nextOutgoingSequenceNumber` | `MonadPerspectives Int` | Get-and-increment `outgoingSequenceNumber` |
| `getExpectedIncomingSequenceNumber` | `PerspectivesUser -> MonadPerspectives Int` | Look up expected-next from a given author (0 if unknown) |
| `updateIncomingSequenceNumber` | `PerspectivesUser -> Int -> MonadPerspectives Unit` | Advance expected-next to `seqNum + 1` |

---

## Limitations and Future Work

- **Gap recovery**: When deltas are missed (gap in sequence numbers), the current implementation only logs a warning. A future implementation should retrieve and replay the missed deltas.
- **Context roles**: The modify-wins-over-delete logic currently handles thing roles and user roles. Context roles (with their nested contexts and external roles) are deferred to a later phase as described in issue #200.
- **Multi-author conflicts**: The Lamport clock approach correctly handles the case where the modification and deletion come from different authors, but more complex three-way conflicts (e.g., two different peers both modifying different properties while a third deletes) may require additional analysis.
