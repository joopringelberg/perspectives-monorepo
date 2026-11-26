# Delta Ordering and Conflict Resolution

> **Relevant modules**
> - `Perspectives.Sync.HandleTransaction` (`src/core/sync/handleTransaction.purs`) — receive pipeline, gap check, version tracking
> - `Perspectives.Sync.Deltas` (`src/core/sync/deltas.purs`) — outgoing distribution, history forwarding, replay order
> - `Perspectives.Persistence.DeltaStore` (`src/core/persistence/deltaStore.purs`)
> - `Perspectives.Persistence.ResourceVersionStore` (`src/core/persistence/resourceVersionStore.purs`)
> - `Perspectives.Persistence.PendingTransactionStore` (`src/core/persistence/pendingTransactionStore.purs`)
> - `Perspectives.Persistence.PeerDeliveryStore` (`src/core/persistence/peerDeliveryStore.purs`)
> - `Perspectives.Sync.PropertyHistory` (`src/core/sync/propertyHistory.purs`) — property checkpoints and reconstruction
> - `Perspectives.Sync.ConstructionBaseline`, `Perspectives.Sync.ConstructionHistory` (`src/core/sync/construction*.purs`)
> - `Perspectives.Sync.Causal` (`src/core/sync/causal.purs`) — per-role causal clocks and the causal epoch
> - `Perspectives.TypesForDeltas` (`src/core/sync/typesForDelta.purs`)
>
> All paths are relative to `packages/perspectives-core`.
>
> **Related documentation**
> - `docsources/transaction-execution.md` — transaction lifecycle and phase 1/2 processing (not duplicated here)
> - `design/deterministic-delta-ordering.md` — the original design document
> - `design/delta-representation-redesign.md` — proposed successor design (transport encryption, dropping legacy support, undo metadata)

### Reading guide and implementation status

This document started as a design and has been extended as features were
added. It was reviewed against the code in November 2026 (issue #572) and
restructured: §§1–9 describe **what the code does now**, §10 is the **design
for unblocking** (phases 1–3 implemented, see §10.9), §11 lists **known issues
and gaps**.
Where earlier versions described intended behaviour as if it existed, that has
been corrected.

| Mechanism | Status |
|---|---|
| Per-key resource versions, deterministic conflict tiebreak | Implemented; late property winners re-derive from history (§5.1, §6.1) |
| Modify-wins-over-delete, role restoration | Implemented |
| Gap detection, blocking a whole transaction | Implemented |
| Persisting blocked transactions (pending store) | Implemented (§3.3, §7.3) |
| **Unblocking** blocked transactions | Implemented: drain after each incoming transaction and at startup (§10.3, §10.4) |
| Keeping transactions whose execution failed | Implemented: stored as pending and retried (§10.5) |
| **Timeout** for blocked transactions | **Not implemented** — replaced by escalation design in §10.7 |
| Property history retention and signed checkpoints | Implemented |
| Construction baselines for (re)constructed roles and contexts | Implemented |
| Peer delivery knowledge (suppress re-forwarding) | Implemented, with a contiguous watermark for checkpoints and a reset on key change (§3.4, §8.3) |
| Per-role causal clocks, causal epoch, quarantine | Implemented |
| Receiver-driven repair (requesting missing history) | Implemented (§10.6) |

---

## 1. Problem: convergence in a distributed system

Perspectives is a distributed system in which multiple peers can simultaneously
modify the same resources — contexts, role instances, bindings, and properties.
Deltas describing those mutations are exchanged through a message queue
(AMQP). Because of transport delays and offline periods they can arrive at
different installations in different orders.

Without additional coordination every installation would apply incoming deltas
in its own arrival order and the peers would diverge over time. The scheme
described here aims to make every installation converge to the **same final
state** regardless of arrival order. It rests on three pillars:

1. **Ordering per resource key** — every mutation carries a version number for
   the key it changes (§2); receivers apply versions in order (§5) and
   **block** transactions that skip versions (§7).
2. **Deterministic conflict resolution** — concurrent mutations producing the
   same version are resolved identically everywhere (§6).
3. **History cut-offs** — retained history is bounded; signed checkpoints and
   construction baselines let a peer start from a certified state instead of
   the full history (§8).

Pillar 1 requires that blocked transactions are eventually unblocked; this is
the subject of §10.

---

## 2. Resource keys and version numbers

### 2.1 Resource-key granularity

Each delta operates on exactly one *resource key*:

| Delta type | Resource key |
|---|---|
| `UniverseContextDelta` | `ContextInstance` (unwrapped string) |
| `UniverseRoleDelta` | `RoleInstance` (unwrapped string) |
| `ContextDelta` | `RoleInstance` (unwrapped string) |
| `RoleBindingDelta` | `RoleInstance + "#binding"` |
| `RolePropertyDelta` | `RoleInstance + "#" + PropertyType` |

The binding and property keys are **sub-resource keys**: they extend a
role-instance key with a `#`-separated suffix. A sub-resource key contains two
`#` characters: the first is the scheme separator inside the role-instance
identifier (e.g. `def:#somecuid`), the second separates role and sub-resource.

Note that `UniverseRoleDelta` (construction, removal) and `ContextDelta`
(membership, move) share the role key: membership and lifecycle of a role are
ordered on a single counter.

### 2.2 Version numbers

Each resource key carries an integer counter in the ResourceVersionStore (§3.2).
The counter is **local and monotonic**: it is never reset, also not when a role
with the same identifier is deleted and constructed again.

How the sender stamps deltas:

| Operation | Stamped `resourceVersion` |
|---|---|
| `ConstructEmptyContext`, `ConstructExternalRole`, `ConstructEmptyRole` | **0**, without incrementing the counter (`createContext.purs`, `createRole.purs`) |
| `AddExternalRole`, `AddRoleInstancesToContext` (membership) | `incrementResourceVersion` → **1** for a new role, higher for a reconstructed one |
| Move, removal, binding and property operations | `incrementResourceVersion`, i.e. local counter + 1 |
| `PropertyCheckpoint` (format 3) | the version of the boundary it certifies (no increment) |

A delta thus carries the version the key has *after* it is applied. On receipt,
an applied delta sets the local counter to the delta's `resourceVersion`
(never lower — see issue I-11).

Consequences that matter later:

- Constructors are version 0, so they never cause gaps and never win or lose a
  version conflict against non-constructor deltas. Recreating an identifier
  yields another version-0 constructor; the construction baseline (§8.2)
  decides which constructor is current.
- A version number does **not** identify a delta. Two authors can produce the
  same version (a conflict), and the gap check only counts versions; it cannot
  tell *whose* delta filled a version. Property deltas below the local
  counter are therefore re-examined rather than discarded (§5.1, issue I-4).

Both `resourceVersion` and `resourceKey` are stored inside every `DeltaRecord`.

---

## 3. Stores

### 3.1 DeltaStore

All deltas — locally created and received — are kept in the PouchDB database
`{systemIdentifier}_deltastore` (`Perspectives.Persistence.DeltaStore`).

**Document ID** (`deltaStoreDocIdWithDeltaId`):

```
<safeResourceKey>_v<resourceVersion>_<authorGuid>|<shortDeltaId>
```

- `safeResourceKey` — the key converted by `safeKey`: the URI scheme prefix
  (`def:`, `loc:`, `rem:`, `model://…#`) is stripped and property-type URIs are
  shortened to `<firstSegment>$<lastSegment>`. `safeKey` is idempotent, so raw
  and already-safe keys normalise to the same identity.
- `_v<resourceVersion>` — decimal version (not numerically sortable as a
  string; readers sort numerically).
- `authorGuid` — the GUID part of the author's `PerspectivesUser`.
- `shortDeltaId` — the first 16 characters of the content hash
  `computeDeltaId author payload`.
  It distinguishes two different deltas of the same author at the same
  version (e.g. a reconstruction constructor at version 0).

**Record:**

```purescript
newtype DeltaStoreRecord = DeltaStoreRecord
  { _id             :: String
  , _rev            :: Maybe String
  , resourceKey     :: String         -- safe form
  , resourceVersion :: Int
  , author          :: PerspectivesUser
  , deltaId         :: Maybe String   -- content hash; used for duplicate detection
  , signedDelta     :: SignedDelta    -- the original, signed delta
  , deltaType       :: String         -- constructor name, for filtering
  , applied         :: Boolean        -- executed, or suppressed by conflict resolution
  , disposition     :: Maybe String   -- why: see §5.2
  , contextKey      :: Maybe String   -- only for ContextDelta records
  }
```

`storeDelta` is idempotent (an existing ID is a no-op). Incoming deltas are
persisted through `storeIncomingDeltaWithHistory`, local ones through
`storeLocalDeltaWithHistory`; both also run property retention (§8.1).
`findDeltaByDeltaId` detects exact duplicates.

| Retrieval | Returns |
|---|---|
| `getDelta docId` | One record |
| `getDeltasForResource key` | All retained records for a key, sorted numerically by version |
| `getDeltasForResourceByDeltaType key type` | As above, filtered |
| `getDeltasForRoleInstance role` | All records for a role and its sub-resources (range query) |

### 3.2 ResourceVersionStore

`{systemIdentifier}_resourceversions` holds `{ resourceVersion :: Int }` per
safe key.

| Function | Behaviour |
|---|---|
| `getResourceVersion key` | Current version; **0** if unknown |
| `initResourceVersion key` | Sets 0 if untracked |
| `setResourceVersion key v` | Sets `v` unconditionally (callers must guard with `max`) |
| `incrementResourceVersion key` | Increments and returns the new value; 1 if unknown |

`getResourceVersion` returns 0 both for unknown keys and for keys at version 0;
§5.3 explains how this ambiguity is resolved for constructors.

### 3.3 PendingTransactionStore

`{systemIdentifier}_pendingtransactions` holds transactions blocked by version
gaps (§7) and transactions whose execution threw an exception (§10.5):

```purescript
newtype PendingTransactionRecord = PendingTransactionRecord
  { _id :: String                    -- pendingTransactionDocId (below)
  , _rev :: Maybe String
  , transaction :: TransactionForPeer -- the complete, still signed transaction
  , missingDeltas :: Array MissingDelta -- gaps found at the last attempt; [] after a failure
  , status :: String                 -- "Blocked" | "Failed"
  , attempts :: Int                  -- number of failed executions
  , lastError :: Maybe String
  , blockedSince :: Maybe Number     -- ms since the epoch, set when first stored
  , historyRequestedAt :: Maybe Number -- ms since the epoch of the last history request (§10.6)
  , historyRequests :: Int           -- number of history requests sent for this record
  }
```

- The document ID is deterministic:
  `pending_<author>_<timeStamp>_<shortHash>`, where `shortHash` is the first 16
  hex digits of the SHA-256 of the canonicalised transaction
  (`pendingTransactionDocId`). Redelivery of the same transaction maps to the
  same record.
- `storePendingTransaction` is an upsert: it replaces `missingDeltas`, sets the
  status to `Blocked` and keeps `attempts`, `lastError` and the history-request
  bookkeeping; `blockedSince` is set only on the first store.
- `recordHistoryRequest` sets `historyRequestedAt` and increments
  `historyRequests`.
- `recordFailedTransaction` increments `attempts` and records `lastError`; at
  `maxFailedAttempts` (5) the status becomes `Failed`.
- `removePendingTransactionFor` / `removePendingTransactionById` delete a record
  with its current revision; removing an absent record is a no-op.
- `getAllPendingTransactions` enumerates the `pending_` ID range; unreadable
  documents are skipped. Records written before `status`/`attempts` existed
  (old ID format) are read as `Blocked` with 0 attempts, so they are retried
  too.
- The stored transaction is always the transaction *as received* (before
  scaffolding deltas and duplicate keys are filtered out), so a blocked and a
  failed attempt of the same transaction share one record.

The store survives restarts. It is read by the drain
(`Perspectives.Sync.PendingTransactions`, §10.3).

### 3.4 PeerDeliveryStore

Records, per (destination peer, resource key), which stored deltas have been
**durably queued** for that peer (`knownIds`), the highest version queued
(`maxVersion`) and the **contiguous watermark** (`contiguousVersion`): the
highest version v such that the peer was queued a record at every retained
version up to v, or a checkpoint at or above it. It is sender-side *belief*,
not receiver acknowledgement: "queued" does not mean "received", let alone
"applied". It is used to avoid forwarding the same retained history repeatedly
(§8.3).

- The document ID is the JSON array `[destination, resourceKey]` (both as safe
  keys), so all documents of one peer form one ID range.
- `recordQueuedPeerDeltas` (after the outbox write) adds the queued ids,
  raises `maxVersion` and extends the watermark with `extendContiguousVersion`:
  a known checkpoint lifts it to its version; above that it advances through
  ascending retained versions as long as a known record exists at each.
  Versions never retained locally are skipped; an unknown checkpoint bars it.
- `forgetPeerDeliveryKnowledge` deletes all documents of a peer (I-7).
- `lowerPeerDeliveryKnowledge` (on a history request) forgets the ids of
  retained records at or above the requested version and lowers the watermark
  below it. `maxVersion` is kept: it records what the peer has been entitled
  to see and bounds history answers (§10.6).

### 3.5 Causal and quarantine stores

`{systemIdentifier}_causal` and `{systemIdentifier}_quarantinedtransactions`
are described in §9.

---

## 4. The receive pipeline

### 4.1 Entry points

`executeTransaction` is called from:

- the AMQP consumer (`amqpTransport/incomingPost.purs`): **current-epoch
  messages are acknowledged before execution**; legacy-epoch messages only
  after they have been quarantined;
- the post-database path (`sync/incomingPost.purs`, via
  `runIncomingTransaction`): the source document is deleted after handling;
- `ImportTransaction` in `perspectivesAPI.purs`, and the transaction import in
  `couchdbExternalFunctions.purs`.

The AMQP consumer runs each transaction in its own outermost, non-sharing
transaction (`runMonadPerspectivesTransaction' false …`) with background
priority.
Incoming execution does not forward deltas (`addDeltaWithStorage` queues
nothing while `isExecutingIncomingDeltas`).

Both transport entry points call `executeTransactionWithOutcome`, catch
exceptions, and hand the result to `settleIncomingTransaction`
(`sync/pendingTransactions.purs`): an exception stores the transaction as a
pending record (§10.5), an executed or quarantined transaction removes any
pending record for it. After each message they run the drain (§10.4). Because
current-epoch AMQP messages are acknowledged early, the pending store is the
**only durable copy** of a blocked or failed transaction. Only a failure to
write that pending record itself still loses the transaction (and stops the
consumer, as before). Decryption failures happen before this point and are not
stored.

### 4.2 Processing sequence

1. **Causal epoch** (`executeTransaction`) — a transaction without the current
   epoch is quarantined (§9.3); nothing is executed.
2. **Verification** (`verifyTransaction`) — public-key data, delta signatures,
   actor ownership and causal metadata; payloads without ordering fields are
   rejected. Produces a map `PerspectivesUser → CryptoKey`.
3. **Key deltas** (`executeTransaction'`) — the deltas in `publicKeys` are
   stored and executed **unconditionally**, before the gap check. They are
   idempotent, so re-execution on retry is harmless.
4. **Ordering info** — `resourceKey`, `resourceVersion` per verified delta.
5. **Construction baselines** — `validatedConstructionBaselines` selects and
   authorises baselines contained in the transaction (§8.2).
6. **Historical filter** — deltas `coveredWhileAbsent` (observed by a persisted,
   authorised deletion of an absent role) are excluded from the gap check.
7. **Gap check** — `checkForGapsWith`, with per-key floor
   `max(localVersion, baselineVersion, checkpointVersion)` (§7.2). Any gap
   blocks the **whole** transaction: it is written to the pending store, and
   steps 8–9 do not happen.
8. **Replay order** — deltas are sorted by `compareForReplay`
   (`deltaTypeSortPriority`; property-like deltas by key and version, checkpoint
   first).
9. **Version tracking** — `executeDeltaWithVersionTracking` per delta (§5).

---

## 5. The central algorithm: `executeDeltaWithVersionTracking`

```
executeDeltaWithVersionTracking baselines signedDelta stringified resourceKey resourceVersion author
```

### 5.1 Branches, in evaluation order

1. **Construction baseline record** — the delta is the selected constructor or
   membership of a baseline for this key (or the context template of a
   baseline): execute, set the counter to `max(local, baseline.version)`,
   disposition `ConstructionBaseline`.
2. **Exact duplicate** (`findDeltaByDeltaId`) — ignored.
3. **Empty resource key** — ignored (there is no unversioned fallback).
4. **Covered while absent** — stored as historical evidence, counter raised,
   disposition `CoveredByDeletion`, not executed.
5. **Checkpoint** — executed only when `resourceVersion > localVersion`
   (never overwrites equally new or newer state), disposition `Checkpoint`.
6. **Below the local counter** (`resourceVersion < localVersion`).
   - *Property operations* (`reconsiderLateProperty`): the delta may be the
     winner of a conflict at its own version that arrived late, or fill a
     version this installation never received. If its version is at or above
     the key's checkpoint and no stored record at that version takes
     precedence (§6.1), the records at that version are marked not applied,
     the delta is stored as applied with disposition `LateWinner`, and the
     property is re-derived from its full history
     (`restorePropertyFromHistory`). The counter is not changed. Otherwise it
     is stored with `applied = false`, disposition `Outdated`.
   - *Other deltas*: stored with `applied = false`, disposition `Outdated`,
     not executed. Their operations are absolute (bindings, role lifecycle,
     membership), so the later version already determines the final state.
7. **Same version** (`resourceVersion == localVersion`) — fresh creation or
   conflict (§5.3).
8. **Next version** (`resourceVersion > localVersion`; gaps were excluded in
   §7) — deletions go through modify-wins (§6.2); sub-resource deltas for an
   absent role may first restore the role (§6.3); otherwise execute and set the
   counter.

### 5.2 Dispositions

| Disposition | `applied` | Meaning |
|---|---|---|
| `Applied` | true | Executed in the normal or winning-conflict path |
| `ConflictLoser` | false | Lost the author tiebreak (§6.1) |
| `LateWinner` | true | Property delta below the local counter that wins at its version; the property was re-derived from history |
| `Outdated` | false | Version below the local counter (and, for properties, losing at its version or below the checkpoint) |
| `Checkpoint` | true | Certified baseline state applied |
| `ConstructionBaseline` | true | Selected constructor/membership/context template |
| `CoveredByDeletion` | true | Historical evidence observed by an authorised deletion |
| `ModifyWinsSuppressedDelete` | false | Deletion suppressed by a concurrent modification |
| `SupersededByConstruction` | false | Deletion observed by a later live construction |

When a conflict winner replaces earlier records at the same version, those are
marked `applied = false` (`updateDeltaApplied`).

### 5.3 Same version: fresh creation versus conflict

`getResourceVersion` returns 0 for unknown keys. When `resourceVersion == 0`
and no record exists at version 0 for the key, the delta is the first one ever
seen here: it is executed directly.

Otherwise it is a write-write conflict, resolved by the tiebreak (§6.1). When
the incoming author wins: the delta is executed (deletions via the deletion
rules), stored records at that version are marked not applied, and for
property operations the property is re-derived from its history
(`restorePropertyFromHistory`), so that a later version built on the loser is
re-applied on top of the winner.

### 5.4 Execution errors

`executeDelta` catches errors of non-property deltas and only logs a warning;
the delta is still stored as applied and the counter advanced (issue I-12).
Property errors (including failed instance-level authorisation, §6.4) and
checkpoint errors propagate: nothing of that delta is recorded, and the
exception aborts the incoming transaction. Earlier deltas of that transaction
are not rolled back.

---

## 6. Conflict rules

### 6.1 Tiebreak

Two different deltas at the same version of the same key: the author whose
(unschemed) `PerspectivesUser` identifier is **alphabetically highest** wins;
two different deltas of one author at one version (possible only after counter
loss, §9.3) are ordered by their signed content. The order is a total order
on signed deltas (`conflictPrecedence` in `propertyHistory.purs`), used both by
`existingDeltaWins` when a delta arrives and by `propertyHistory` when a
property is reconstructed, so both agree on every installation. A checkpoint
competes with the original winner it certifies.

**Resulting invariant for properties.** The value of a property is the fold,
in version order, of the winners per version, starting from the newest
checkpoint. Every path that applies a property delta either extends that fold
(next version, winning same-version conflict on the newest version) or
re-derives it from history (winning conflict, late winner). Installations that
hold the same records therefore hold the same value, whatever the arrival
order. A v(n+1) that its author built on a losing v(n) is replayed on top of
the winner of v(n): its intention is rebased, not preserved (issue I-5).

### 6.2 Modify wins over delete

For an incoming role deletion (`RemoveRoleInstance`,
`RemoveExternalRoleInstance`, `RemoveUnboundExternalRoleInstance`):

- Deletion permission is checked and the causal tombstone recorded.
- If a later live construction observed the deletion
  (`supersededByConstruction`), the deletion is suppressed
  (`SupersededByConstruction`).
- Otherwise `isDeletionSuppressedByModifyWins` compares each applied property
  or binding modification's causal clock with the deletion's signed causal
  context. A modification **not covered** by that context is concurrent and
  suppresses the deletion (`ModifyWinsSuppressedDelete`). Resource versions
  alone cannot establish causality across keys.
- `RetractRoleInstance` is never suppressed: leaving a context wins.

The same rules apply when an incoming deletion wins a same-version conflict.

### 6.3 Role restoration

A property or binding delta for an absent role whose role counter is > 0
triggers `restoreRoleFromDeltaStore`, unless the delta was observed by an
authorised deletion (§5.1 step 4):

1. Stored deletion records are marked `applied = false`.
2. Role-level non-deletion records (constructor, membership) are re-executed.
3. Applied binding records are replayed in version order; each property is
   reconstructed from its newest checkpoint and applied tail
   (`restorePropertyHistories`), resolving same-version conflicts by original
   author.

### 6.4 Inherited-property authorisation and rejected updates

Ordinary incoming property operations first use
`hasPerspectiveOnPropertyWithVerbByImplication`: a perspective must grant the
property and verb, and its object ADT must imply the role declaring that
property, using the existing CNF type comparison. Alternative filler types can
therefore establish permission through a shared aspect without implying one
particular storage role type. This check does not establish instance provenance;
a successful type-level check bypasses the instance witness.
Static statement compilation retains `hasPerspectiveOnPropertyWithVerb`,
including its check against the statement's target role type.
If implication cannot prove permission, `hasInstancePropertyPermission` looks
for a concrete witness:

1. Follow the signed author's inverse binding links, confirming every link
   against the filled role's actual binding. Visit each role at most once.
2. In those roles' contexts, evaluate the declared subject role and require its
   result to include a role actually filled by that author.
3. Evaluate the perspective's object query, including filters, with that actor
   and context. Scoped query evaluation interprets `me` as the original author,
   not the receiving installation.
4. Require the requested property verb and resolve a permitted property through
   the same property-bearing-role resolver used for writes. Both the resolved
   storage instance and property must match the incoming operation. This also
   respects aspect-property replacements and properties shadowing a filler.

This permits a write through, for example, `Participant -> Persons ->
NonPerspectivesUsers` even when `Persons` admits several alternative filler
types. It does not weaken `isPerspectiveOnRoleFilledWithRoleType` from type
implication to overlap. Static compilation continues using that type-level rule.
Received historical operations consider permissions in any declared state; the
editing state need not still be active on receipt. Property checkpoints retain
their separate disclosure/winner verification.

A rejected property execution logs an error and propagates failure before
recording that operation's causal clock, advancing its resource version, or
storing it as applied. Earlier successful operations in the transaction are not
rolled back. The exception propagates out of the transaction; the
entry point then stores the whole transaction as a pending record and the
drain retries it, up to `maxFailedAttempts` times (§10.5). Operations applied
before the exception are skipped as duplicates on retry.

Older runtimes could warn about an unauthorised property but still store its
delta as applied. Upgrading does not rewrite those records, and a plain resend
is skipped as a duplicate. To repair such data, make a new authorised edit on
the originating installation (change and restore the value if necessary), or
repeat the scenario with fresh instances. Do not remove history or bypass
signature/conflict checks to force old deltas to replay.

The witness depends on *evidence* the receiver must already hold: the author's
chain of filled roles up to the subject role (for example `PerspectivesUsers <-
Persons <- Employees <- Ambassadeur`) and the object's binding chain down to the
storage (`Deelnemer -> Persons -> NonPerspectivesUsers`). A property change only
ships the second chain (`serialiseDependencies`); the first is assumed to have
arrived with the subject role. If it did not (for example because it was
serialised by a runtime that failed to compute construction baselines in
`TheWorld`/`TheSocialEnvironment`), every retry fails in the same way and the
transaction stays pending until `maxFailedAttempts` (I-17). When the instance
check fails, it logs (`AUTH`) the author identities found, the roles the author
transitively fills and their contexts, so the missing link can be identified.
The incoming transaction error includes both the type-level permission failure
and the instance-witness failure, including that same author-chain evidence.
Failure to establish a witness does not by itself prove which link is missing.

---

## 7. Gap detection and blocking

### 7.1 Why entire transactions are blocked

Deltas in one transaction can depend on each other: a sender's automatic action
may have produced delta _B_ because of delta _A_. If any delta in an incoming
transaction is out of sequence, the **entire transaction** is blocked rather
than executed partially.

### 7.2 Gap check

`checkForGapsWith lookupFloor deltaInfos` groups the deltas by safe key, sorts
each group by version and walks it from `floor + 1`: versions below the
expected one are ignored (they will be outdated or conflicts), an equal version
advances the expectation, a higher version is a gap. Per key at most one
`MissingDelta` is reported:

```purescript
type MissingDelta =
  { author          :: PerspectivesUser  -- author of the delta that exposed the gap
  , resourceKey     :: String
  , expectedVersion :: Int               -- first missing version
  }
```

The floor per key is `max(localVersion, baselineVersion, checkpointVersion)`,
where the last two only come from baselines and checkpoints **in the same
transaction**. A baseline lifts the floor of its role key only; a checkpoint
only of its own property key.

`MissingDelta` records only the first missing version, not the version the
transaction actually needs (`firstVersionInTransaction - 1`); a retry must
therefore recompute gaps from the transaction itself (§10.3).

### 7.3 Storing blocked transactions

The original (unfiltered) `TransactionForPeer` is stored with its gaps
(`storePendingTransaction`, an upsert under a deterministic ID, §3.3), and
`executeTransactionWithOutcome` returns `Blocked gaps`. Key deltas have already
been applied (§4.2 step 3). Storing the same transaction again (redelivery, or a
retry that blocks again) updates the existing record.

Pending transactions are retried by the drain (§10.3, §10.4).

---

## 8. History cut-offs

Three mechanisms let peers avoid shipping or requiring a key's full history.
They are cut-offs on two different axes:

- **Receiver side**: checkpoints and baselines raise the gap-check floor
  (§7.2), so a receiver may start a key from a certified state.
- **Sender side**: retention removes old property history; peer delivery
  knowledge suppresses forwarding of what was already queued for a peer.

### 8.1 Property history retention and checkpoints

Stored property keys retain a signed reconstruction checkpoint and the ten most
recent subsequent logical versions. Conflicting deltas at a retained version
share a history position. Retention runs when local or incoming property deltas
are persisted; untouched old properties are not swept on startup. It does not
change the monotonic ResourceVersionStore counters.

ARC's `fullhistory` property attribute opts out of this pruning:

```arc
property WorkflowBusy (Boolean)
property AgreedPrice (Number, fullhistory)
```

The policy belongs to the actual property storing the value, after filler/aspect
replacement resolution. Missing metadata in older compiled models means the
bounded default. Opting in preserves subsequently received history; it cannot
recover already removed changes. The attribute grants no access rights and does
not cause the archive to be resent on every ordinary update.

**Checkpoint protocol and trust.** A checkpoint uses delta format **3**, retains
the property payload shape, and adds `historyCheckpoint` containing the values
immediately before its boundary and the original signed winning change. Its
outer signature certifies the resolved state at that boundary. Original changes
retain their own signatures; the disclosing installation signs a new
certificate rather than modifying them.

An authenticated disclosing peer may certify state without replacement-write
permission. This is a trusted state assertion by that peer, not a proof that
every omitted historical operation was supplied. Ordinary perspective selection
still determines which data is disclosed. Receivers verify the certificate and
original winner, check their resource/type/version agreement, and reject
unsigned, nested, malformed or identity-key checkpoints. The original winner's
author, not the certifier, determines conflicts at the boundary.

For gap checking, verified checkpoints cover earlier versions of their own key
only. Execution starts with the checkpoint and then applies newer property
versions numerically. An equally new or newer local state is never overwritten.
Recovery reconstructs property state from the checkpoint and effective tail,
including relative additions/removals and boundary conflicts.

The checkpoint is persisted before covered records are deleted. Interrupted
pruning leaves redundant evidence that a later write can clean up. Delayed old
packets cannot repopulate compacted history. Pending and outgoing transactions
are not rewritten: encrypted outbox packets remain intact. Initialisation
transfers state, not a promise to re-execute discarded historical workflow
transitions.

**Operational evidence.** Creation, membership, binding and deletion histories
are not compacted. Public-key and transport-key establishment proofs remain
independently verifiable. The latest `UploadFile` operation remains available
even before the checkpoint boundary; public publication processes it after
property replay so it reads final metadata.

Only checkpoint-aware runtimes can consume compacted history; there is no
capability negotiation or historical backfill.

### 8.2 Construction baselines

A model- or user-determined identifier can be recreated after deletion.
Creation disclosure must not replay an earlier lifecycle's removals, especially
when publishing a public representation. The runtime derives a baseline from
existing signed DeltaStore records (`Perspectives.Sync.ConstructionBaseline`):

- Select the causally maximal compatible role constructor. Its role type,
  context and resource identity must match the live instance.
- Select the compatible membership that observes that constructor. Context
  type references must agree, including their revision; external membership
  must also share its constructor's subject and author.
- For a context, include a compatible `ConstructEmptyContext` template and its
  external role's baseline. Context constructors have no causal stamp:
  equivalent compatible templates are interchangeable.
- Export these records, not earlier constructors, memberships or removals.
  Historical evidence stays in the DeltaStore for conflict handling.

`Perspectives.Sync.ConstructionHistory` retrieves the selected records for
dependency serialisation, first-binding filler disclosure,
perspective-assumption disclosure, calculated-user initialisation and author
verification bundles. Callers keep their own recipient and delivery policies.

On reception, construction permissions are validated before a stamped, complete
baseline's membership version becomes the gap-check floor **for that role key
only**. A missing context additionally requires its external baseline and a
compatible context template. Version-zero constructors and identical stored
context templates can reconstruct absent resources; counters never reset, and
repeated disclosure is idempotent.

An absent role's new constructor must observe every known deletion and have a
membership version beyond its local role counter. A deletion observed by a
later live construction cannot remove that construction. Concurrent maximal
constructors or memberships are reported as ambiguous and require explicit
reconciliation. Unstamped historical records cannot establish causal recency.
Baselines do not redefine historical property or binding contents.

### 8.3 Peer delivery knowledge

When retained history is forwarded through `addStoredDeltas`, records whose
delivery id is in the peer's `knownIds` are skipped, and a checkpoint is skipped
when the peer's `contiguousVersion` is at or beyond it. (It used to be
`maxVersion`. That is unsound: versions below it may have been withheld from
the peer, for example by a filtered perspective, while a later version was
shipped live, and then pruned. The checkpoint is then the only record that
covers them; issue I-9.) Knowledge is written after the
outgoing transaction has been durably queued. State entry that newly grants a
perspective uses `addStoredDeltaForInitialVisibility`, which bypasses knowledge:
a newly visible resource is always reinitialised.

### 8.4 Evaluation

A cut-off is sound when four conditions hold:

1. **Completeness** — the cut-off record carries the complete state of its key
   at the boundary.
2. **Verifiability** — the receiver can authenticate and authorise it.
3. **Monotonicity** — it is applied before the tail and never lowers newer
   local state.
4. **Availability** — whenever a receiver needs it, it can obtain it.

Checkpoints satisfy 1–3 for property keys; baselines satisfy 1–3 for role keys
(with the stated limitation for concurrent maximal constructors). Condition 4
is where the design is weak:

- **Asymmetric coverage.** Only property and role keys have cut-offs. Binding
  keys, context keys and the `#binding` history of earlier lifecycles are always
  shipped in full. That is correct (they are never compacted), but a receiver
  that misses one binding delta has no way to skip past it (issue I-8).
- **Floors are per transaction.** A checkpoint or baseline lifts the floor only
  if it arrives in the same transaction as the tail, or was applied earlier.
  If it arrives in an earlier transaction that is itself pending, the later
  transaction blocks too. This is only acceptable if unblocking retries pending
  transactions in a loop until no progress is made (§10.3).
- **Knowledge is belief, not fact.** Delivery knowledge records "queued", not
  "applied". Whenever the receiver did not apply the queued delta — it was
  quarantined, rejected for missing authorisation (§6.4), lost by an exception
  after the early AMQP acknowledgement, or lost because the peer was restored
  from backup or reinstalled — the sender will not forward it or a covering
  checkpoint again on the knowledge-based paths (state-dependent perspectives,
  perspective assumptions in `collectAffectedContexts.purs`). Without repair
  the receiver would be blocked on that key permanently (issues I-7, I-9).
- **Live paths do not consult knowledge.** A delta created now is shipped to
  whoever has a perspective now (`addDelta`), whether or not the peer has the
  versions before it. If those were withheld earlier, the peer blocks.

The cut-off design is sound as a *state transfer* mechanism. It is completed as
a *repair* mechanism by unblocking (§10): retry when the missing data arrives
(§10.3), and ask for it when it does not (§10.6). Knowledge is reset when a
peer's keys change (I-7), and a checkpoint is withheld only from peers that are
known to have every version up to it (I-9).

---

## 9. Per-role causality, epoch and quarantine

### 9.1 Causal stamps

Every newly signed role-affecting mutation contains `causal`:

```
{ epoch, dot: { actor, counter }, context: [{ actor, counter }, ...] }
```

The actor combines the canonical author identity with a persistent random
installation nonce. Counters and observed clocks are shared across a role's
creation, membership, binding and property keys, not across unrelated roles.
Reservations are persisted before signing; unused counter gaps are harmless.
Resource versions continue to order each individual key.

`{systemIdentifier}_causal` stores the installation nonce, epoch, per-role
observed clocks and a frontier of authorised deletions. A deletion observes a
modification only if its context covers the modification's entire causal
clock. Compacted checkpoints carry `causalContext`, the union of their
compacted prefix. Older checkpoints without causal metadata represent baseline
history. Context creation has no role clock.

Causal stamps are used for deletion/modification and construction decisions
only. Same-key conflicts (§6.1) and gap checks (§7) do not use them; for
properties, convergence comes from the version-ordered fold of winners (§6.1,
issue I-5).

This is authenticated causal bookkeeping, not Byzantine proof: authenticated
authors can assert observations, and checkpoint certifiers retain the trust
described in §8.1.

### 9.2 Upgrade procedure

The upgraded runtime activates `perspectives-causal-1` on its first causal
reservation or incoming transaction. There is no separate activation switch.

1. Stop model editing and other mutation-producing activity on **all**
   installations. Keep them online until traffic has settled.
2. Ensure outgoing messages have been **received and processed** by their
   destinations, not merely sent or acknowledged by the broker. Drain incoming
   queues and resolve pending version-gap transactions as well. *(With
   unblocking unimplemented, pending transactions can only be resolved by
   inspecting the pending database by hand.)*
3. Stop all old runtimes, back up their data, then upgrade all participants
   before resuming activity. Do not run mixed old/new writers.

Existing ordered history becomes the shared pre-transition baseline; this
assumes that peers' mutually visible state has converged before cutover.

### 9.3 Quarantine

All newly assembled transactions include `causalEpoch`. Transactions with a
missing or unsupported epoch go to `{systemIdentifier}_quarantinedtransactions`.
`getQuarantinedTransactions` enumerates the decoded transactions and reasons;
stable content IDs make redelivery idempotent. No key, instance or
resource-version mutation from these transactions is executed. AMQP
acknowledges them only after durable storage; the cleartext incoming-post path
removes its source document only after successful handling.

There is no automatic replay or quarantine UI. Do not add a current epoch to an
old packet to force replay. Backups must include the causal and quarantine
databases and preserve the installation nonce: never operate two live copies of
the same installation. Storage migration copies these databases as well.

### 9.4 Supported delta representations

Payloads without `resourceKey`/`resourceVersion`, and old role deltas with
plural `roleInstances`, are rejected (issue #196). Supported:

- Format 1: ordered, unmarked payloads with singular `roleInstance`.
- Format 2: explicit format markers and revisioned type references.
- Format 3: signed property checkpoints (§8.1).
- Format 4: revisioned type references plus signed per-role causal metadata.

---

## 10. Design: unblocking

### 10.1 Requirements

- **R1 Liveness** — a blocked transaction is retried as soon as the versions it
  needs may have arrived, including after a restart.
- **R2 Same rules** — a retry goes through the complete pipeline of §4.2
  (epoch, verification, baselines, gap check). No shortcut through saved
  `MissingDelta` snapshots: they only record the first missing version per key
  and cannot reflect checkpoints or baselines that arrived since.
- **R3 Transitivity** — unblocking one transaction may unblock others; retries
  continue until a pass makes no progress.
- **R4 Isolation** — each retry is its own outermost, non-sharing transaction,
  admitted as background work. It never runs inside the transaction that
  triggered it (embedded transactions keep their parent's admission; nesting a
  retry would deadlock on the transaction flag and would merge unrelated
  rule cascades).
- **R5 Idempotence** — storing, re-storing and removing pending records are
  safe under redelivery, crashes and concurrent triggers.
- **R6 Repair** — when the missing versions do not arrive by themselves, the
  receiver asks for them (§10.6).
- **R7 Visibility** — the user and the modeller can see that data is waiting.
- **R8 No blind execution** — a timeout never applies a transaction across a
  gap silently.

### 10.2 Store changes

> **Implemented** (phase 1): deterministic ID, upsert, enumeration, removal,
> and the fields `status`, `attempts` and `lastError` (§3.3). Not yet
> implemented: `firstBlockedAt`, `lastAttemptAt`, `historyRequestedAt`, the
> `Escalated` status and the in-memory index; they belong to phases 3–4.

```purescript
newtype PendingTransactionRecord = PendingTransactionRecord
  { _id :: String
  , _rev :: Maybe String
  , transaction :: TransactionForPeer
  , missingDeltas :: Array MissingDelta
  , status :: String            -- "Blocked" | "Failed" | "Escalated"
  , firstBlockedAt :: Number    -- epoch ms
  , lastAttemptAt :: Number
  , attempts :: Int
  , lastError :: Maybe String
  , historyRequestedAt :: Maybe Number
  }
```

- **Deterministic ID** — `pending_<authorGuid>_<timeStamp>_<shortHash>`, where
  `shortHash` is a content hash of the transaction (reuse `computeDeltaId`).
  Sorting the IDs approximates arrival order per author.
- **Upsert** — `storePendingTransaction` reads the existing record, keeps
  `firstBlockedAt`, `attempts` and `historyRequestedAt`, updates
  `missingDeltas`/`lastAttemptAt`, and writes with the current `_rev`. This
  fixes issue I-3 and makes redelivery idempotent.
- **Enumeration** — `getAllPendingTransactions` uses
  `documentsInRange dbName "pending_" "pending_\xFFFF"` and returns records
  with their `_rev`.
- **Removal** — `removePendingTransaction` with the record's current `_rev`,
  only after the retry's transaction has settled.
- **In-memory index** (optional optimisation) — a set of *awaited keys*
  (resource keys with a gap in any pending record), rebuilt at startup. A
  finished incoming transaction triggers a drain only if it advanced an awaited
  key. Without the index, a drain after every incoming transaction is still
  cheap while the pending store is empty (one `allDocs` call), so the index can
  be added later.

### 10.3 Drain algorithm

> **Implemented** in `Perspectives.Sync.PendingTransactions`
> (`drainPendingTransactions`), with these deviations from the design below:
>
> - `executeTransactionWithOutcome` returns
>   `Executed | Blocked gaps | Quarantined | Rejected reason`; `Rejected` covers
>   a transaction that fails verification (nothing is changed).
>   `executeTransaction` remains as a wrapper that discards the outcome.
> - There is no separate signature-free `pendingGaps`. Instead the drain has two
>   modes. `AfterIncoming` retries a `Blocked` record only if it has no recorded
>   gaps (a failed execution) or if, for at least one recorded `MissingDelta`,
>   the local version of its key has reached `expectedVersion`. A record whose
>   recorded gaps have not moved cannot have become executable: local counters
>   only rise when versions are applied in sequence. `AtStartup` retries every
>   record once (including `Failed` ones), so records written by an older
>   runtime, or blocked on keys that advanced for other reasons, are
>   reconsidered. Later passes always use `AfterIncoming`. The full pipeline
>   recomputes the gaps on every retry (R2).
> - `Rejected` on a retry counts as a failed attempt; on first receipt it is
>   dropped, as before.
> - The re-entrancy guard (`PendingDrainGuard` in `PerspectivesState`) has
>   `draining` and `dirty` flags; a drain requested during a drain sets `dirty`,
>   which causes one more pass.
> - `detectPublicStateChanges` runs after each retry, inside its admission.

`executeTransaction` is changed to **return** an outcome instead of only
storing it:

```purescript
data TransactionOutcome = Executed | Blocked (Array MissingDelta) | Quarantined
```

The entry points (and the drain) decide what to do with the outcome; the
pending record is written in one place (`storePendingTransaction`, upsert).

A cheap, signature-free pre-check is factored out of `executeTransaction'`:

```purescript
-- Recompute gaps from the transaction itself: ordering info is readable from
-- the signed payloads; checkpoints and baselines in the transaction lift the
-- floor exactly as in §7.2. Verification happens again on real execution.
pendingGaps :: TransactionForPeer -> MonadPerspectivesTransaction (Array MissingDelta)
```

The drain:

```
drainPendingTransactions :: MonadPerspectives Unit
drainPendingTransactions =
  if draining then set dirty := true          -- re-entrancy guard
  else do
    draining := true
    repeat
      dirty := false
      progress := false
      records <- getAllPendingTransactions
                 |> filter (status == "Blocked")
                 |> sortBy (timeStamp, author)
      for records \r ->
        gaps <- pendingGaps r.transaction        -- cheap pre-check (R2)
        unless (null gaps) (continue)            -- still blocked, nothing to do
        outcome <- try $ withTransactionPriority BackgroundTransaction
                       $ runMonadPerspectivesTransaction' false sysUser
                       $ executeTransaction r.transaction   -- full pipeline (R2, R4)
        case outcome of
          Right Executed     -> removePendingTransaction r; progress := true
          Right Quarantined  -> removePendingTransaction r; progress := true
          Right (Blocked g)  -> storePendingTransaction r.transaction g   -- upsert
          Left err           -> recordFailure r err   -- attempts+1, lastError; status Failed after N
      detectPublicStateChanges
    until not progress && not dirty
    draining := false
```

Notes:

- **Order.** Retrying in `(timeStamp, author)` order replays one author's
  transactions in the order they were made, so chains `T3 → T2 → T1` unblock in
  one pass when `T1` arrives; the loop (R3) handles chains across authors.
- **Pre-check monotonicity.** A key that was gap-free stays gap-free: local
  counters only rise, so lower versions become outdated, never gaps. Hence a
  transaction whose `pendingGaps` is empty is worth a full retry, and one with
  gaps is not.
- **Key deltas** are re-executed on every retry; they are idempotent.
- **Epoch.** The retry checks the epoch again; a transaction stored under an
  older epoch is quarantined rather than executed.
- **Failure.** An exception in a retry must not stop the drain. After N
  (e.g. 5) failed attempts the record's status becomes `Failed` and it is no
  longer retried automatically; it is shown to the user (§10.7).
- **Concurrency.** The drain itself holds no transaction admission; each retry
  is admitted separately with background priority, so user transactions
  interleave.

### 10.4 Triggers

> **Implemented:** all three. Both transport consumers
> (`amqpTransport/incomingPost.purs`, `sync/incomingPost.purs`) call
> `drainPendingTransactions AfterIncoming` after each message, outside the
> message's admission and transaction, whatever its outcome; and
> `drainPendingTransactions AtStartup` before the consumer starts. `Main.purs`
> forks a loop that drains every 10 minutes. Each drain ends by sending the
> history requests that are due (§10.6). `ImportTransaction` and the identity document
> import in `initSystem` store blocked transactions but do not trigger a drain;
> the next incoming message or restart does.

1. **After settlement of an outermost incoming transaction** that executed
   (outcome `Executed`): schedule the drain after the transaction flag has been
   raised, e.g. as a `SettledTransaction` continuation
   (`PendingSettledStack`, dispatched by `runMonadPerspectivesTransaction'`) or
   directly after `runMonadPerspectivesTransaction'` returns in the entry
   points. It must never run inside the triggering transaction (R4). Retries
   themselves do not trigger a new drain; the loop takes care of that via
   `progress`.
2. **At startup**, after the incoming queue consumer is running, so that
   records stored before a restart are reconsidered (R1).
3. **Periodically** (every 10 minutes), for the time-based steps of §10.6 and
   §10.7. A drain without pending records costs one range read.

Local mutations are *not* a trigger. A local write on an awaited key raises the
counter past the gap without supplying the missing delta, so the gap check would
pass for the wrong reason. That case is covered by the late-winner rule for
property deltas (§5.1, issue I-4), not by unblocking.

### 10.5 Failed executions become pending records

> **Implemented** (`settleIncomingTransaction`, `recordFailedTransaction`).

A transaction whose execution throws (for example a property rejected for
missing instance authorisation, §6.4) used to be lost after the early AMQP
acknowledgement, and the exception stopped the AMQP consumer. The entry points
now catch such exceptions and store the transaction with `status = "Blocked"`,
`missingDeltas = []`, `attempts + 1` and `lastError`.
The next drain retries it; if the authorisation evidence has arrived in the
meantime it succeeds, otherwise it reaches `Failed` after N attempts. This makes
the "can be retried" promise of §6.4 real. Deltas that were applied before the
exception are skipped on retry as duplicates.

### 10.6 Receiver-driven repair: history requests

> **Implemented** in `Perspectives.Sync.HistoryRequest`
> (`sync/historyRequest.purs`). Deviations from the original design are marked
> *(deviation)*.

Waiting suffices when the missing deltas are merely delayed. It does not when
the sender believes they were delivered (§8.4). The receiver therefore asks.

**When** (`historyRequestDue`). For a `Blocked` record with recorded gaps
(`missingDeltas` not empty): the first request 10 minutes after `blockedSince`;
then after a back-off of 1 hour, doubling per request, at most 24 hours.
Failed executions (no gaps) are never the subject of a request. Requests are
sent at the end of every drain (§10.4), so also by the periodic drain. Records
of our own transactions (another installation of ourselves) are skipped: we
never ship deltas to ourselves.

**Request.** All due records of one author are combined into one message:

```purescript
type HistoryRange = { resourceKey :: String, fromVersion :: Int, toVersion :: Int }

type HistoryRequest =
  { requester :: String   -- unschemed PerspectivesUser
  , addressee :: String   -- unschemed PerspectivesUser
  , requests :: Array HistoryRange -- at most 100
  , timeStamp :: Number   -- ms since the epoch
  }
```

Per missing delta (`historyRanges`), `fromVersion` is the expected version and
`toVersion` is the lowest version of that key in the pending transaction minus
1. The request is signed like a delta (`signDeltaMP`), wrapped as
`HistoryRequestMessage { historyRequest :: SignedDelta }`, encrypted for the
author's transport key and sent in an ordinary `EncryptedTransactionForPeer`
envelope (with empty `publicKeys`). It is not a delta: it never enters the
DeltaStore. It is sent to the author of the pending transaction, who
necessarily applied those versions before building on them. The addressee is
part of the signed content, so a request cannot be replayed to another peer.

**Receiver of the request** (`answerHistoryRequest`, dispatched by the AMQP
consumer when the decrypted payload is a `HistoryRequestMessage`; it is
acknowledged and never stored as pending):

1. Require a signature, verify it, check that `requester` is the signer and
   `addressee` is us, and that the requester is a known PerspectivesUser.
   Otherwise log and answer nothing.
2. *(deviation)* **Authorisation by delivery knowledge**, not by a perspective
   check. Per range, the answer is capped at the requester's `maxVersion` for
   that key: the highest version ever queued for it. A key never queued for the
   requester is not answered. A history answer therefore never discloses more
   than ordinary forwarding did. Re-evaluating perspectives for an isolated
   resource key outside its context is not possible in general; the cost is
   that a requester whose perspective was revoked can still obtain versions it
   was earlier entitled to.
3. *(deviation)* **Lower delivery knowledge** for (requester, key) with
   `lowerPeerDeliveryKnowledge`: drop the ids of retained records with versions
   ≥ `fromVersion` and lower the watermark below it; `maxVersion` is kept as the
   authorisation bound of step 2. This repairs issue I-7 for this key.
4. **Select** (`selectHistory`) from the retained records of the key: if the
   record at `fromVersion` is retained, all non-checkpoint records in
   [`fromVersion`, cap] (with the version-0 constructors if `fromVersion ≤ 1`);
   otherwise (it was pruned) the newest checkpoint in the range plus the records
   after it, up to cap.
5. Answer with one ordinary, sharing transaction in which every selected
   record is added with `addStoredDeltaForInitialVisibility` for the requester
   only. It goes through the outbox, which records delivery knowledge again.
6. The answer is processed by the requester's normal pipeline, after which the
   drain retries the pending transaction.

**Limitations.**

- *No closure.* The answer contains the requested keys only. If the requester
  also lacks, say, the constructor of the role whose property it asks for, that
  surfaces as a new gap or a failed execution, and is asked for in turn.
- *No fallback.* If the author cannot answer (offline, deleted, or it pruned
  the range without a checkpoint), no other peer is asked. Escalation (§10.7)
  is the remedy. Asking another author of the key, as recorded in the
  requester's DeltaStore, remains possible.
- *AMQP only.* The CouchDB channel (`sync/incomingPost.purs`) does not
  dispatch history requests.

### 10.7 Escalation instead of timeout

The earlier design executed a pending transaction after 24 hours "and let the
conflict rules handle the rest". That is unsafe: relative property operations
(`AddProperty`, `RemoveProperty`) applied across a gap produce a state that no
other peer has, and the later arrival of the missing delta is classified as
`Outdated` (issue I-4: for properties now re-derived as a late winner, but
only when the history is complete). Instead:

1. After a configurable period (default 24 hours) with unanswered history
   requests, or when a record becomes `Failed`, set its status to `Escalated`.
2. Show a non-blocking system message (`pushSystemMessage`) with the number of
   waiting transactions per peer.
3. Expose `getPendingTransactions` through the API (analogous to
   `getQuarantinedTransactions`) for a diagnostic screen: author, age, keys,
   missing versions, last error.
4. Offer two explicit user actions: **request again**, and **discard** (remove
   the record; the user accepts that this peer's change is not applied).
   Forcing execution across a gap is not offered as a default; if it is ever
   added, it must re-derive each affected property from history afterwards.

### 10.8 Tests

`test/causalSync.purs` covers tests 1, 2, 4 and 7 and the startup drain of
test 5 (with a record written in the same process, not across a restart), plus
storing an exception as a pending record. `test/peerDelivery.purs` covers the
contiguous watermark, the checkpoint rule of I-9, forgetting and lowering
knowledge, rejection of unsigned history requests, and the pure parts of
§10.6 (ranges, back-off, selection). Tests 3, 6 and 8 remain to be written.
The original list:

1. `T2` (v2) blocked; `T1` (v1) arrives → `T2` executes after `T1` settles and
   its record is removed.
2. Chain: `T3`, `T2` arrive before `T1` → all execute in one drain, in order.
3. Cross-author chain requiring two passes.
4. Redelivery of a blocked transaction does not throw; one record remains.
5. Restart: a pending record written before shutdown is drained at startup.
6. A checkpoint arriving in a separate, later transaction unblocks a tail.
7. A retry that throws records `lastError` and does not stop the drain; after N
   attempts the status is `Failed`.
8. Two-PDR test (`test/PDRInstance.purs` harness, fresh processes): delivery
   knowledge claims a delta that the receiver lost; a history request repairs
   it.

### 10.9 Phasing

1. **Done.** Store fixes (§10.2), outcome type, drain with triggers 1–2,
   tests 1, 2, 4 and 5 (in-process).
2. **Done.** Failed executions as pending records (§10.5), test 7.
3. **Done.** History requests and knowledge reset (§10.6); the two-PDR test 8
   remains.
4. Escalation UI (§10.7).

---

## 11. Known issues and gaps

Severity: **H** can cause permanent divergence or permanent blocking; **M**
can cause wrong intermediate state or lost work under plausible conditions;
**L** robustness or clarity.

| # | Sev | Issue | Where | Suggested fix |
|---|---|---|---|---|
| I-1 | H | **Fixed** (§10.3, §10.4). Blocked transactions were never retried: `getAllPendingTransactions` returns `[]` and nothing calls it. Every gap is permanent; later transactions of the same peer touching the same keys block as well. | `pendingTransactionStore.purs` | §10.2–§10.4 |
| I-2 | M | No timeout or escalation; the user cannot see waiting data. | — | §10.7 |
| I-3 | M | **Fixed** (§3.3). Pending doc ID was `pending_<author>_<timeStamp>` and stored without `_rev`: redelivery of a blocked transaction (or any future re-store) conflicts and throws after the AMQP ack. | `storePendingTransaction` | Deterministic ID with content hash, upsert (§10.2) |
| I-4 | H | **Fixed** (§5.1, `reconsiderLateProperty`, disposition `LateWinner`). A late property delta that wins at its version (or fills an unknown version above the checkpoint) re-derives the property from history. Below the checkpoint it remains `Outdated`; non-property deltas remain `Outdated` (absolute operations). **`Outdated` is not the same as stale.** A delta with `resourceVersion < localVersion` may be the *winner* of a conflict at its version that arrived late. Example: A and B write v4 concurrently, A > B; B then writes v5 = `AddProperty z`. At B, A's v4 arrives at local v5: `Outdated`, discarded. At A, B's v4 loses and B's v5 is applied on A's v4. For relative operations A ends with {x, z}, B with {y, z}. The same happens when a third peer's gap is "closed" by the losing v4, or by a local write on an awaited key. Absolute operations (set, bindings, role lifecycle) converge in their final state. | `executeDeltaWithVersionTracking`, outdated branch | For property operations: persist, and if the incoming delta would win at its version (or no record exists there and it is above the checkpoint), re-derive the property with `restorePropertyFromHistory`, exactly as the conflict-winner branch already does. |
| I-5 | M | **Mitigated** (§6.1 invariant): with I-4 and I-13 fixed, every installation computes the same version-ordered fold of winners, so properties converge. Remaining: a v(n+1) built on a losing v(n) is rebased on the winner rather than merged by intent; this is the documented semantics. Resolving conflicts from causal stamps remains an option, not a correctness requirement. Version counters carry no causality. A v(n+1) built on a losing v(n) is applied as normal; per-role causal stamps exist but are not used for same-key conflicts. Combined with I-4 this is the root of property divergence. | §6.1, §7 | Short term: I-4's re-derivation. Long term: resolve property conflicts from the causal stamps (I-4 then becomes a special case). |
| I-6 | M | Replay order only sorts by version within priority 5 (properties, moves). For one key, `SetFirstBinding` (4) runs before `RemoveBinding`/`ReplaceBinding` (6) regardless of version, and role `Move` (5) before `Remove` (6). A transaction `SetFirstBinding v1, RemoveBinding v2, SetFirstBinding v3` replays as v1, v3, v2: v2 becomes `Outdated` and its side effects (e.g. inverse binding cleanup) are skipped. | `compareForReplay`, `sortTransactionDeltas` | Keep the type priority across keys, but within one resource key order strictly by version (constructors at version 0 sort first naturally). |
| I-7 | H | **Fixed** (§3.4, §10.6): knowledge is lowered per key on a history request, and forgotten for a peer when its signing or transport key changes (`executeRolePropertyDelta`). The installation nonce is deliberately not a trigger: one user may run several installations; a restore from backup keeps keys and is repaired by history requests. Peer delivery knowledge means "queued", never reset. A peer that lost or rejected a delta (restore from backup, reinstall, quarantine, rejected authorisation, exception after early ack) never receives it or a covering checkpoint again on knowledge-based paths → permanent gap. | `peerDeliveryStore.purs`, `addStoredDeltas` | Reset on history request (§10.6); also reset all knowledge for a peer when its installation nonce or key changes. |
| I-8 | M | **Mitigated** (§10.6): history answers carry the retained binding history from the missing version. Binding, context and role-membership keys have no checkpoint. A single missed binding delta can only be repaired by resending exactly that delta. Baselines lift only the role key; property and binding gaps in the same transaction still block it. | §8.4 | Answering history requests with full binding history (§10.6) suffices; a binding checkpoint is optional. |
| I-9 | M | **Fixed** for stored-history paths (§8.3): a checkpoint is withheld only from peers whose contiguous watermark reaches it. `addStoredDeltas` always receives the full retained history, so no other part of a range is skipped. Live `addDelta` paths still ship v(n+k) without v(n..); the resulting gap is repaired by a history request (§10.6). Non-initial shipping paths (state-dependent or filtered perspectives, perspective assumptions in `collectAffectedContexts.purs`) can send version n+k to a peer that never received n..n+k-1, relying on knowledge (I-7). | `collectAffectedContexts.purs`, `serialiseAsDeltas.purs` | Same as I-7; optionally ship a checkpoint whenever knowledge's `maxVersion` is below the first shipped version - 1. |
| I-10 | H | **Fixed** for execution failures (§10.5); the early acknowledgement remains, so a failure to *write* the pending record, or to decrypt, still loses the message. Current-epoch AMQP messages are acknowledged before execution. If `storePendingTransaction` fails (I-3) or execution throws (§5.4, §6.4), the transaction is gone; only the sender's knowledge remains, which says "delivered". | `amqpTransport/incomingPost.purs` | Store failed executions as pending (§10.5); alternatively acknowledge only after execution or pending storage. |
| I-11 | L | `setResourceVersion` is unconditional; most callers guard with `max`, some rely on the branch condition. A future caller can lower a counter and reopen applied versions. | `resourceVersionStore.purs` | Make `setResourceVersion` monotonic (`max`) and add an explicit `resetResourceVersion` if ever needed. |
| I-12 | M | Still open; now that failed transactions are retried (§10.5), the suggested fix is possible, but it changes which transactions fail and must be evaluated per delta type. Non-property execution errors are swallowed: the delta is stored as `Applied` and the counter advanced although nothing happened. The peer silently diverges and later deltas build on a state that does not exist. | `executeDelta` | Treat like property errors: propagate, and rely on §10.5 for retry. |
| I-13 | L | **Fixed** (§6.1, `conflictPrecedence`): signed content is the secondary tiebreak, shared by arrival-time resolution and history reconstruction. `hasAuthorGreaterOrEqual` used `>=`, so the same author at the same version counts as a winner over itself. Duplicates are normally caught by `deltaId`; of two *different* deltas of one author at one version (only possible after counter loss, e.g. restore from backup) the later arrival always loses, so the outcome depends on arrival order. | `executeDeltaWithVersionTracking` | Compare `deltaId` as a secondary tiebreak. |
| I-14 | L | Concurrent version-0 constructors by different authors for the same deterministic identifier: the first is executed as "fresh", the second resolved by author tiebreak; context constructors carry no causal stamp. Constructors are idempotent so state converges, but the recorded winner may differ between peers. | §5.3 | Accept; document that constructor winners are not significant. |
| I-15 | L | **Partly mitigated**: what a restored peer lost is repaired by history requests (§10.6). Counter reuse remains. Restoring a peer from backup reuses its counters for new writes (resource versions and causal counters), creating same-author same-version deltas at its peers. The doc warns against two live copies, but not against restore-then-continue. | §9.3 | Rotate the installation nonce on restore and announce it, which triggers a knowledge reset (I-7). |
| I-17 | M | Inherited-property authorisation (§6.4) needs the author's filled-role chain up to the subject role at the receiver. Property shipping does not include that chain, and no repair path requests it: when it is missing, the transaction fails `UnauthorizedForProperty` on every retry. | `hasInstancePropertyPermission`, `addDeltasForPropertyChange` | Ship the author's subject-role chain (bindings plus construction histories) with property deltas whose permission is instance-based, or answer such rejections with a history request for the subject role's bindings (§10.6). |
| I-16 | L | The previous version of this document claimed every mutation increments the version, linked a non-existent `docs/transaction-execution.md`, omitted the `deltaId`/`disposition`/`contextKey` fields, and presented §6.4/§6.5 as existing behaviour. | this file | Corrected in this revision. |

### Priorities

1. ~~I-1, I-3, I-10 (unblocking phases 1–2)~~ — done.
2. ~~I-4 (+ I-5, I-13): divergence of multi-valued properties under ordinary
   concurrency~~ — done.
3. ~~I-7, I-9 (history requests): repair after data loss~~ — done; the
   two-PDR test remains.
4. I-6, I-12: correctness of replay within a transaction.
5. The rest.

---

## 12. Decision rationale

| Decision | Chosen approach | Alternative considered |
|---|---|---|
| Ordering unit | Single integer version per resource key | Lamport clock + per-author sequence number |
| Conflict winner | Author with alphabetically highest ID, then signed content; properties re-derived as a version-ordered fold of winners | Last write wins on arrival order |
| Modify vs. delete | Concurrent modification wins; observed historical writes do not | Treating all historical changes as concurrent |
| Gap response | Block entire transaction | Execute available deltas and patch later |
| Persistence | PouchDB (survives restarts) | In-memory only |
| History bound | Signed property checkpoints + ten versions; baselines for construction | Full history everywhere |
| Unblocking | Drain after settlement, full re-verification, loop until no progress (§10) | Re-check saved `MissingDelta` snapshots only |
| Long-blocked transactions | History request (§10.6), then escalation to the user (§10.7, *planned*) | Execute anyway after 24 hours |
| History-request authorisation | Bounded by what was queued to the requester before (`maxVersion`) | Perspective evaluation per requested key |
| Checkpoint redundancy | Contiguous delivery watermark | Highest queued version |
