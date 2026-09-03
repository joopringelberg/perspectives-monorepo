# Delta Representation Redesign Proposal

## Purpose

This document proposes the next delta representation for the PDR. It is a
design proposal only: it does **not** prescribe an implementation sequence and
it does **not** require migration of existing deltas.

The proposal is intended to replace the current transitional state in which the
runtime:

- supports both current and legacy delta representations;
- stores a signed delta largely as an opaque blob;
- uses the same structure for local persistence and for transport concerns;
- must stay compatible with a future undo facility.

## Drivers

The redesign should meet four goals simultaneously:

1. **Efficient storage and retrieval**
   - exact delta lookup by deterministic key;
   - efficient replay per resource;
   - efficient retrieval of all deltas belonging to a role, including its
     binding and property sub-resources.
2. **Phase out legacy deltas**
   - no migration of historic deltas;
   - a new Perspectives Universe can start cleanly from one surviving
     installation.
3. **Future-ready undo**
   - conflict rollback and user undo should build on the same representation;
   - reconstruction should not depend on legacy parsing paths.
4. **Payload confidentiality**
   - transport should no longer rely on TLS alone for payload confidentiality;
   - encryption must not destroy indexability of locally stored deltas.

## Current limitations

The current design in `deterministic-delta-ordering.md` is already a major
improvement, but it still has a few structural limitations:

1. **Legacy support leaks into the main flow.**
   Incoming transactions still need fallback parsing, sentinel
   `resourceVersion = -1`, and special handling for empty `resourceKey`.
2. **The stored delta is only partly indexable.**
   Important fields are copied out into `DeltaStoreRecord`, but the actual delta
   semantics still live inside `signedDelta.encryptedDelta`.
3. **Transport and persistence are conflated.**
   The field name `encryptedDelta` suggests confidentiality, while in practice
   the payload is plain JSON plus a signature.
4. **Document-ID ordering should no longer rely on plain decimal text.**
   Lexicographic ordering of `..._v1`, `..._v2`, `..._v10` is not the same as
   numeric ordering. A redesign should make ordering explicit and stable.

## Proposed direction

### 1. Split the delta into three layers

The next representation should distinguish between:

1. **Canonical delta header** — small, clear-text, indexable metadata.
2. **Canonical delta payload** — the actual mutation data.
3. **Transport envelope** — optional encryption wrapper for a specific sender →
   receiver hop.

This separation keeps local persistence queryable while making transport
encryption possible.

### 2. Canonical delta header

Every delta should carry a clear-text header that is stored locally and also
travels with the delta:

```text
deltaFormatVersion
universeEpoch
deltaId
transactionId
transactionPosition
author
authoringRole
resourceKey
baseResourceKey
contextKey?
resourceVersion
deltaFamily
deltaOperation
isDeletion
createdAt
payloadHash
inverseStrategy
```

#### Field rationale

| Field | Purpose |
|---|---|
| `deltaFormatVersion` | Hard cut-over to the new representation. |
| `universeEpoch` | Prevent accidental mixing of deltas from the old and new universe. |
| `deltaId` | Stable identifier for exact lookup and references from undo/conflict metadata. |
| `transactionId`, `transactionPosition` | Preserve transaction grouping and replay order inside one transaction. |
| `resourceKey`, `resourceVersion` | The main ordering pair already used today. |
| `baseResourceKey` | Groups a role with its `#binding` and property sub-resources. |
| `contextKey` | Keeps context-related retrieval efficient without inspecting payloads. |
| `deltaFamily`, `deltaOperation` | Avoid repeated trial deserialisation when filtering by delta kind. |
| `payloadHash` | What is signed and verified, independent of transport encryption. |
| `inverseStrategy` | Declares how undo/rollback is derived for this delta. |

`baseResourceKey` is the important new indexing field. For a role-level delta it
equals `resourceKey`; for `role#binding` and `role#property` deltas it equals
the role key. This removes the need to infer grouping from string prefixes.

### 3. Canonical delta payload

The payload remains type-specific, but it should be stored as canonical JSON
that matches the header. The important change is that the payload is no longer
the only place where the runtime can discover what the delta is about.

The payload should additionally distinguish:

- **after-state data** — the mutation to apply;
- **before-state data** when the inverse is cheap to store;
- **reconstruction-based undo** when storing the full before-state would be too
  large.

Recommended rule:

| Delta category | Undo data strategy |
|---|---|
| Property and binding updates | Store enough `before` data in the payload to invert locally. |
| Context/role creation | Inverse is the corresponding delete operation. |
| Context/role deletion | Use replay/reconstruction rather than embedding full snapshots in every delta. |

This keeps most deltas small while reducing lookup work for common undo cases.

### 4. Local persistence model

The delta store should persist the canonical form, not just the transport form.

```text
StoredDelta
  _id
  header
  payload
  signature
  applied
  supersededBy?
  undoneBy?
```

#### Document ID

Keep deterministic IDs, but make version ordering explicit:

```text
<universeEpoch>|<safeResourceKey>|<zero-padded-resourceVersion>|<authorGuid>
```

Two consequences matter:

1. exact lookup still remains a direct key lookup;
2. range scans over one resource now sort numerically as well as
   lexicographically.

The version segment should be zero-padded to a fixed width that is comfortably
larger than any realistic resource history.

#### Query patterns

| Query | Mechanism |
|---|---|
| Exact delta by ID | direct `_id` lookup |
| All deltas for one resource | range query on `resourceKey + paddedVersion` |
| All deltas for a role and its sub-resources | indexed filter on `baseResourceKey` |
| All context-link deltas for a context | indexed filter on `contextKey` |
| Latest applied delta | `ResourceVersionStore.latestAppliedDeltaId` |

### 5. ResourceVersionStore should become a small resource-head store

The current `ResourceVersionStore` should be extended from:

```text
resourceVersion
```

to:

```text
resourceVersion
deleted
latestDeltaId
latestAppliedDeltaId
```

This keeps the high-frequency lookup path cheap while making reconstruction and
undo decisions more explicit.

- `deleted = true` avoids inferring deletion from absence alone.
- `latestAppliedDeltaId` gives a direct pointer to the visible head of the
  resource.

## Cryptography proposal

### 1. Keep both signing and encryption

The answer to "do we need both?" is **yes**.

- **Signature** proves authorship and integrity of the canonical delta.
- **Encryption** protects payload confidentiality during transport.

They solve different problems and should not replace one another.

### 2. What should be signed?

The original author should sign:

```text
canonical(header without transport-only fields) + canonical(payload)
```

Equivalently, the signature may cover the header plus `payloadHash`, provided
the hash is computed over canonical payload JSON.

This is important because it lets intermediate peers re-encrypt a payload for a
different recipient **without** invalidating the author's signature.

### 3. What should be encrypted?

Encrypt the **payload**, not the header.

The header must remain readable so that receivers can:

- route the delta;
- decide where to store it;
- perform version checks;
- index it locally.

### 4. Transport strategy

Use a transport envelope per sender → receiver hop:

```text
TransportEnvelope
  header
  ciphertext
  encryptedContentKey / keyId
  iv / nonce
  authorSignature
  transportSender
```

When installation `A` forwards a delta originally authored by `C` to `B`:

1. `A` reads the canonical payload from its local delta store.
2. `A` keeps `C`'s signature unchanged.
3. `A` encrypts the payload for `B`.
4. `B` decrypts the payload and verifies `C`'s signature.

This avoids the need for `A` to possess `C`'s secrets while still protecting
transport traffic.

### 5. Should deltas be stored locally encrypted?

**Not by default.**

The proposed default is:

- local delta-store stores canonical payload in plain form;
- transport uses payload encryption;
- local machines rely on existing platform/browser protections for at-rest
  security.

Reason:

1. the delta store is on the hot path for replay, reconstruction, sharing, and
   future undo;
2. storing everything encrypted locally would either force frequent decryption
   or require duplicating clear-text indexes;
3. the issue at hand is transport confidentiality first.

If at-rest encryption later becomes a separate requirement, the same split
header/payload design supports encrypting only the stored payload while keeping
the header indexable.

### 6. Synchronisation cost expectation

Payload encryption/decryption will add cost, but it should be modest relative to
network latency, signature verification, PouchDB I/O, and replay work.

This should be validated empirically, not guessed. The implementation phase
should benchmark at least:

- 100, 1,000, and 10,000 delta sync batches;
- small property deltas versus larger reconstruction payloads;
- local replay with and without transport decryption.

The proposal therefore assumes transport encryption is acceptable unless the
benchmarks show otherwise.

## Legacy phase-out

No migration is proposed.

Instead, the cut-over should happen together with the restart of the
Perspectives Universe described in the issue:

1. choose one surviving installation as bootstrap source;
2. create a fresh universe with a new `universeEpoch`;
3. allow only `deltaFormatVersion = 2` deltas in that universe;
4. reject legacy deltas completely;
5. remove the runtime paths that parse legacy delta shapes and sentinel version
   values.

This gives a clean protocol boundary without trying to rewrite or resign old
deltas that may no longer be attributable to reachable installations.

## Undo compatibility

The redesign should keep the monotonic-history approach from
`undo-facility.md`: deltas are not removed, but their status can change.

Recommended status fields:

- `applied`
- `supersededBy`
- `undoneBy`

Undo then becomes "append new inverse deltas and update status metadata",
instead of mutating history.

### Reconstruction rule

Reconstruction should always use:

1. `baseResourceKey` to gather the relevant history;
2. numeric `resourceVersion` ordering;
3. only deltas that are currently effective (`applied = true` and not
   superseded/undone for the purpose at hand).

### Conflict rollback rule

When two deltas collide on the same `(resourceKey, resourceVersion)`:

- the winner is still determined deterministically;
- the loser is retained in history;
- rollback uses either:
  - the payload's `before` data; or
  - replay/reconstruction, depending on `inverseStrategy`.

## Recommended consequences for the relevant modules

These are design consequences only:

| Module | Consequence |
|---|---|
| `Perspectives.TypesForDeltas` | delta payloads should remain type-specific, but the shared canonical header should be made explicit. |
| `Perspectives.Persistence.DeltaStore` | store/query canonical headers and payloads; stop depending on legacy payload inspection for normal routing. |
| `Perspectives.Persistence.DeltaStoreTypes` | replace the transitional record with a true stored-delta shape, including `baseResourceKey` and explicit status references. |
| `Perspectives.Persistence.ResourceVersionStore` | evolve into a resource-head store with `deleted` and latest-delta pointers. |
| `Perspectives.Sync.SignedDelta` | split canonical signature concerns from transport-envelope concerns. |
| `Perspectives.Sync.LegacyDeltas` | remove after the universe cut-over. |

## Decisions

| Decision | Chosen |
|---|---|
| Legacy handling | hard cut-over after universe restart; no migration |
| Local representation | canonical header + canonical payload + signature |
| Transport representation | per-hop encrypted envelope around the payload |
| Header visibility | unencrypted and indexable |
| Local at-rest encryption | deferred; not the default |
| Undo support | hybrid: store `before` data for small deltas, replay for structural deletion |
| Resource grouping | explicit `baseResourceKey` |
| Delta ordering in IDs | zero-padded version segment |
| Old/new universe separation | explicit `universeEpoch` |

## Verification during implementation

When this design is implemented, verify at least:

1. **Exact lookup:** fetch a delta by `_id` without deserialising unrelated
   deltas.
2. **Role reconstruction:** reconstruct a role from role-level plus
   sub-resource deltas gathered through `baseResourceKey`.
3. **Legacy rejection:** a legacy delta is rejected immediately in the new
   universe.
4. **Forwarding:** `A` forwards a `C`-authored delta to `B`; `B` decrypts it and
   verifies `C`'s signature successfully.
5. **Undo rollback:** a losing local property delta is reversed using stored
   `before` data.
6. **Delete-versus-modify:** a deleted role is reconstructed and then modified
   correctly.
