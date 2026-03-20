# Delta Ordering and Conflict Resolution

> **Relevant modules**
> - `Perspectives.Sync.HandleTransaction` (`packages/perspectives-core/src/core/sync/handleTransaction.purs`)
> - `Perspectives.Persistence.DeltaStore` (`packages/perspectives-core/src/core/persistence/deltaStore.purs`)
> - `Perspectives.Persistence.ResourceVersionStore` (`packages/perspectives-core/src/core/persistence/resourceVersionStore.purs`)
> - `Perspectives.Persistence.PendingTransactionStore` (`packages/perspectives-core/src/core/persistence/pendingTransactionStore.purs`)
> - `Perspectives.Sync.LegacyDeltas` (`packages/perspectives-core/src/core/sync/legacyDeltas.purs`)
> - `Perspectives.TypesForDeltas` (`packages/perspectives-core/src/core/sync/typesForDelta.purs`)
>
> **Related documentation**
> - `packages/perspectives-core/docs/transaction-execution.md` — transaction lifecycle and phase 1/2 processing (no duplication here)
> - `packages/perspectives-core/design/deterministic-delta-ordering.md` — original design document

---

## 1. Problem: convergence in a distributed system

Perspectives is a distributed system in which multiple peers can simultaneously modify the same resources — contexts, role instances, bindings, and properties.  Deltas describing those mutations are exchanged through a message queue (AMQP).  Because of transport delays and offline periods they can arrive at different installations in different orders.

Without additional coordination every installation would independently apply incoming deltas in its own arrival order and the peers would diverge over time.  The scheme described in this document guarantees that every installation converges to the **same final state** regardless of the order in which deltas arrive.

The central mechanism is *deterministic delta ordering*, implemented around the function `executeDeltaWithVersionTracking` in `Perspectives.Sync.HandleTransaction`.

---

## 2. Resource keys and version numbers

### 2.1 Resource-key granularity

Each delta operates on exactly one *resource key*.  The mapping from delta type to resource key is:

| Delta type | Resource key |
|---|---|
| `UniverseContextDelta` | `ContextInstance` (unwrapped string) |
| `UniverseRoleDelta` | `RoleInstance` (unwrapped string) |
| `ContextDelta` | `RoleInstance` (unwrapped string) |
| `RoleBindingDelta` | `RoleInstance + "#binding"` |
| `RolePropertyDelta` | `RoleInstance + "#" + PropertyType` |

The `RoleBindingDelta` and `RolePropertyDelta` keys are called **sub-resource keys** because they extend a role-instance key with a `#`-separated suffix.  A sub-resource key always contains *two* `#` characters: the first is the scheme separator inside the role-instance identifier (e.g. `def:#somecuid`), the second separates the role from the sub-resource.

### 2.2 Version numbers

Each resource key carries a single integer version number that counts the total number of applied mutations to that resource across all authors.

- When a resource is first created, its version starts at **0** (no mutations yet).
- Every local mutation increments the version by 1 before the delta is stamped and distributed.  The delta carries the version number it produces, i.e. `resourceVersion = localVersion + 1`.
- When a received delta is applied the local version for that key is set to the delta's `resourceVersion`.

Both `resourceVersion` and `resourceKey` are stored inside every `DeltaRecord` (the record type that is the underlying representation of all concrete delta types in `Perspectives.TypesForDeltas`).

---

## 3. Delta storage (the DeltaStore)

All deltas — both locally created and received from peers — are stored in a dedicated PouchDB database.  The module responsible is `Perspectives.Persistence.DeltaStore`.

### 3.1 Database name

The database is named `{systemIdentifier}_deltastore`, where `systemIdentifier` is the local installation's identifier (obtained via `getSystemIdentifier`).

### 3.2 Document ID format

The PouchDB document ID for a stored delta is deterministic:

```
<safeResourceKey>_v<resourceVersion>_<safeAuthorGuid>
```

- **`safeResourceKey`** — the resource key converted to a URL-safe form by `safeKey`.  The function strips the URI scheme prefix (`def:`, `loc:`, `rem:`, `model://…#`) so that forward slashes and percent-encoded sequences that break CouchDB reverse proxies are eliminated, and shortens property-type URIs to `<firstSegment>$<lastSegment>`.
- **`_v<resourceVersion>`** — the literal character `_v` followed by the decimal version number.
- **`<safeAuthorGuid>`** — the CUID part of the author's `PerspectivesUser` identifier.

Because two deltas that conflict on the same resource version differ only in their authors, their document IDs differ in the final segment.  This makes all deltas uniquely addressable and allows range queries over a resource's version history.

### 3.3 DeltaStoreRecord fields

```purescript
newtype DeltaStoreRecord = DeltaStoreRecord
  { _id           :: String         -- deterministic document ID (see 3.2)
  , _rev          :: Maybe String   -- PouchDB revision
  , resourceKey   :: String         -- the resource key (URL-safe form)
  , resourceVersion :: Int          -- version produced by this delta
  , author        :: PerspectivesUser
  , signedDelta   :: SignedDelta    -- the original, signed delta
  , deltaType     :: String         -- serialised constructor name (for filtering)
  , applied       :: Boolean        -- whether this delta was actually applied
  }
```

The `applied` flag records whether the delta was executed or was suppressed by conflict resolution (see §5).  It enables the modify-wins-over-delete restoration logic (see §5.3).

### 3.4 Storing deltas

`storeDelta` checks for an existing document with the same ID and is a no-op if found, making storage idempotent.  `storeDeltaFromSignedDelta` extracts the ordering fields from the `encryptedDelta` JSON string and delegates to `storeDelta` with `applied = true` (used for locally created deltas).

### 3.5 Retrieving deltas

| Function | Returns |
|---|---|
| `getDelta docId` | A single delta by exact document ID |
| `getDeltasForResource resourceKey` | All deltas for a resource key, sorted by document ID (which sorts by version because the format is `…_v<n>_…`) |
| `getDeltasForResourceByDeltaType resourceKey deltaType` | As above, filtered by `deltaType` |
| `getDeltasForRoleInstance roleInstanceId` | All deltas for a role instance **and** all of its sub-resources (properties, binding) using a PouchDB range query from `safeKey` to `safeKey + U+FFFF`, then filtering to exclude accidental prefix matches |

---

## 4. Resource version tracking (the ResourceVersionStore)

Version numbers are kept in a second PouchDB database, named `{systemIdentifier}_resourceversions`.  The module is `Perspectives.Persistence.ResourceVersionStore`.

Each document stores a single `{ resourceVersion :: Int }` value keyed by the URL-safe form of the resource key.

| Function | Behaviour |
|---|---|
| `getResourceVersion key` | Returns the current version; **0** if the key is unknown |
| `initResourceVersion key` | Sets version to 0 if not yet tracked; no-op otherwise |
| `setResourceVersion key v` | Sets version to `v`; creates the document if absent |
| `incrementResourceVersion key` | Atomically increments the version and returns the new value; starts at 1 if the key is unknown |

When constructing a local delta the caller first calls `incrementResourceVersion` to obtain the next version, stamps the delta with that version, and distributes it.  This ensures that the delta's `resourceVersion` is always the version of the resource *after* the delta is applied.

---

## 5. The central algorithm: `executeDeltaWithVersionTracking`

The function `executeDeltaWithVersionTracking` is called for every non-legacy delta in an incoming transaction (after gap checking; see §6).  It implements the full conflict-resolution logic.

```
executeDeltaWithVersionTracking signedDelta stringified resourceKey resourceVersion author
```

The function reads the local version `localVersion = getResourceVersion resourceKey` and branches on the relationship between `resourceVersion` and `localVersion`.

### 5.1 Legacy delta (`resourceVersion < 0`)

A delta with `resourceVersion = -1` is a *legacy delta* (see §7).  The function:
1. Executes the delta unconditionally.
2. Calls `incrementResourceVersion` to assign it the next version and stamps it in the DeltaStore with `applied = true`.

### 5.2 Outdated delta (`resourceVersion < localVersion`)

The incoming version is behind the local version.  The delta describes a state of the resource that was valid in the past but has since been superseded.  The function:
- Stores the delta in the DeltaStore with `applied = false`.
- Does **not** execute it.

### 5.3 Conflict (`resourceVersion == localVersion`)

Two deltas from different authors claim to produce the same version of the resource.  This is the classic write-write conflict.

**Special case — fresh resource creation (`resourceVersion == 0`, no prior deltas):**
`getResourceVersion` returns 0 both for resources that have never been tracked and for resources that are genuinely at version 0.  When the incoming `resourceVersion` is 0 *and* the DeltaStore contains no existing deltas for this resource key, the "conflict" is spurious: the incoming delta is simply the first mutation ever applied to this resource at the receiving installation.  In this situation the function executes the delta directly without any conflict-resolution check and stores it with `applied = true`.

**General conflict resolution:**
When there *are* existing deltas at version 0 for this resource (meaning another author already claimed version 0), or when `resourceVersion > 0` and both versions are equal, this is a genuine write-write conflict.

**Tiebreaker rule:** the author whose `PerspectivesUser` identifier is *alphabetically highest* wins deterministically on all installations.

The function queries the DeltaStore for all already-stored deltas at the same `resourceVersion` for the same `resourceKey`.  If any stored delta has an author ≥ the incoming author, the incoming delta loses; otherwise the incoming author wins.

- **Winner:** execute the delta; store with `applied = true`.
- **Loser:** store with `applied = false`; do not execute.

Note: the same comparison is made independently by every installation.  Because PerspectivesUser identifiers are globally unique and compared by their full string value, the outcome is the same everywhere.

### 5.4 Normal case (`resourceVersion > localVersion`)

This is the expected case: the incoming delta is exactly `localVersion + 1` (gaps have already been ruled out by §6).  Two sub-rules apply before the delta is executed.

#### 5.4.1 Modify-wins-over-delete

If the incoming delta is a **role-deletion delta** (delta type is one of `RemoveRoleInstance`, `RemoveExternalRoleInstance`, or `RemoveUnboundExternalRoleInstance`):

The function calls `isDeletionSuppressedByModifyWins`:
- It retrieves all deltas for the role instance and its sub-resources from the DeltaStore.
- If any sub-resource delta (property or binding) is stored with `applied = true`, a concurrent modification exists.
- When such a modification is found: the deletion is **suppressed**.  The local version is advanced to the incoming version and the delta is stored with `applied = false`.

If the incoming delta is a **sub-resource modification** (binding or property) and the role instance does not exist:
- If the role's resource version is > 0, the role existed and was subsequently deleted.
- The function calls `restoreRoleFromDeltaStore` to reconstruct the role from the DeltaStore (see §5.5) before executing the property or binding modification.

In all other normal cases the delta is executed and stored with `applied = true`.

#### 5.4.2 No resourceKey

If `resourceKey` is the empty string (a legacy delta that could not be parsed; see §7) the delta is executed without any version tracking.

### 5.5 Role restoration from the DeltaStore

`restoreRoleFromDeltaStore` re-applies creation and sub-resource deltas from the DeltaStore in version order:

1. All stored deletion deltas for the role are marked `applied = false`.
2. All non-deletion, role-level deltas (i.e. deltas whose resource key equals the role-instance ID, such as `ConstructEmptyRole` and `AddRoleInstancesToContext`) are re-executed regardless of their `applied` flag.  Role creation is idempotent, and these deltas may have been stored with `applied = false` when they arrived in the same transaction as a later modification (outdated path).
3. All sub-resource deltas (property and binding) that are currently `applied = true` are re-executed in version order.

---

## 6. Gap detection and stalled transactions

### 6.1 Why entire transactions are blocked

Automatic actions (state-triggered `on entry` / `on exit` blocks) within a transaction can be causally dependent on earlier deltas in the same transaction.  If delta _B_ was generated as a consequence of delta _A_, and _A_ has not yet been applied, then _B_ cannot safely be executed in isolation.  Therefore, if **any** delta in an incoming transaction is out of sequence, the **entire transaction** is blocked.

### 6.2 Gap check

Before executing any delta, `executeTransaction'` calls `checkForGaps`:

```
checkForGaps :: Array { resourceKey, resourceVersion, author } -> MonadPerspectivesTransaction (Array MissingDelta)
```

For each delta the function compares `resourceVersion` with `localVersion + 1`.  A gap exists when `resourceVersion > localVersion + 1`.  Each gap is recorded as a `MissingDelta`:

```purescript
type MissingDelta =
  { author          :: PerspectivesUser
  , resourceKey     :: String
  , expectedVersion :: Int   -- localVersion + 1
  }
```

Deltas with an empty resource key (legacy, no ordering info) are excluded from gap checking.

### 6.3 Storing blocked transactions

When gaps are detected the entire `TransactionForPeer` is written to a third PouchDB database, named `{systemIdentifier}_pendingtransactions`.  The module is `Perspectives.Persistence.PendingTransactionStore`.

A `PendingTransactionRecord` stores the full transaction together with the list of `MissingDelta` values that caused the block.  The document ID follows the pattern `pending_<authorId>_<timestamp>`.

Because the store is persistent the pending transaction survives a session restart.

### 6.4 Unblocking

Whenever a new delta is processed that advances a resource-key's version, the implementation should check whether any pending transaction is now unblocked (all its `MissingDelta` entries are satisfied).  `getAllPendingTransactions` in `PendingTransactionStore` is intended to support this; its full query-based implementation is currently a TODO (the placeholder returns an empty array).

### 6.5 Timeout

After a configurable period (the design specifies 24 hours as an example) a pending transaction may be executed anyway to prevent indefinite blocking.  When this happens the conflict-resolution rules of §5 handle any version mismatches that remain.  The timeout mechanism is not yet implemented in the current code.

---

## 7. Legacy delta handling

### 7.1 What is a legacy delta?

Legacy deltas are deltas whose inner JSON payload does **not** contain the `resourceKey` and `resourceVersion` fields.  They were created by peers running an older version of the PDR, before the move to versioned resources.

The old `UniverseRoleDelta` also used a `roleInstances :: SerializableNonEmptyArray RoleInstance` field instead of a single `roleInstance :: RoleInstance`.

### 7.2 Detection

`executeTransaction'` (in `handleTransaction.purs`) tries to extract ordering fields from each verified delta string using `extractOrderingInfo`.  If that fails (the fields are absent), it falls back to `extractLegacyResourceKey` from `Perspectives.Sync.LegacyDeltas`.

`extractLegacyResourceKey` attempts to parse the JSON string as each legacy delta type in sequence and derives the resource key using the same conventions as new deltas:

| Legacy type | Derived resource key |
|---|---|
| `LegacyUniverseContextDelta` | `unwrap id` |
| `LegacyUniverseRoleDelta` | `unwrap (head roleInstances)` |
| `LegacyContextDelta` | `unwrap roleInstance` |
| `LegacyRoleBindingDelta` | `unwrap filled <> "#binding"` |
| `LegacyRolePropertyDelta` | `unwrap id <> "#" <> unwrap property` |

If a resource key can be derived, the delta is tagged with `resourceVersion = -1` (a sentinel value).  If no key can be derived at all, it is tagged with `resourceKey = ""` and `resourceVersion = 0`.

### 7.3 Execution

When `executeDeltaWithVersionTracking` receives a delta with `resourceVersion < 0` (§5.1) it:
- Executes the delta immediately (no conflict-resolution check).
- Assigns the next available version via `incrementResourceVersion`.
- Stores the delta in the DeltaStore with `applied = true`.

Deltas with an empty resource key bypass all version tracking and are executed directly.

### 7.4 Conversion types

`Perspectives.Sync.LegacyDeltas` defines read-only counterparts of all five delta types (`LegacyUniverseContextDelta`, `LegacyUniverseRoleDelta`, `LegacyContextDelta`, `LegacyRoleBindingDelta`, `LegacyRolePropertyDelta`) and converter functions that produce new-format delta values with `resourceKey = ""` and `resourceVersion = 0`.

Inside `executeDelta` (the innermost function that parses and dispatches a single delta) the same legacy types are used as the final fallback in the JSON-parsing cascade.

---

## 8. Processing sequence summary

When an incoming `TransactionForPeer` is received:

1. **Verify signatures** — `verifyTransaction` checks all public-key data and all delta signatures, building a map of `PerspectivesUser → CryptoKey`.
2. **Execute key deltas** — key-management deltas from the `publicKeys` field are applied to establish author identities in the local store.
3. **Extract ordering info** (step 1 in `executeTransaction'`) — for each delta the ordering fields are extracted; legacy deltas receive sentinel values.
4. **Gap check** (step 2) — if any versioned delta has `resourceVersion > localVersion + 1`, the whole transaction is stored in the pending-transaction store and processing stops.
5. **Execute with version tracking** (step 3) — if no gaps, each delta is handled by `executeDeltaWithVersionTracking`, which applies the rules of §5 (legacy, outdated, conflict, or normal).

---

## 9. Decision rationale

| Decision | Chosen approach | Alternative considered |
|---|---|---|
| Ordering unit | Single integer version per resource key | Lamport clock + per-author sequence number |
| Conflict winner | Author with alphabetically highest ID | Last write wins on arrival order |
| Modify vs. delete | Modification always wins | Last-writer wins / delete always wins |
| Gap response | Block entire transaction | Execute available deltas and patch later |
| Persistence | PouchDB (survives restarts) | In-memory only |
| Timeout | Planned: 24-hour threshold | No timeout (block forever) |
