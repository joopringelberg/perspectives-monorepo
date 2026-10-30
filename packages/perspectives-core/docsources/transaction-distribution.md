# Transaction Distribution to Peers

This document describes how the PDR distributes the deltas of a sharing transaction to peers and to public stores. It covers how recipients are determined, how a transaction is customised and encrypted per peer, and how the PDR handles peers whose `TheWorld$PerspectivesUsers` instance is known only as a published (`pub:`) resource.

Distribution is Step 2.3 of [Transaction Execution](transaction-execution.md). The code lives mainly in `Perspectives.Deltas` (`src/core/sync/deltas.purs`) and `Perspectives.Sync.Transaction`.

---

## Overview

```
addDelta                      (while the transaction runs)
  └─ computeUserRoleBottom    user role instance  →  TransactionDestination
       stored in Transaction.userRoleBottoms

phase2 → distributeTransaction (only for sharing transactions)
  ├─ save changed DomeinFiles
  ├─ sortTransactionDeltas
  ├─ addPublicKeysToTransaction
  └─ distributeTransactie'
       ├─ transactieForEachUser   one TransactionForPeer per destination
       ├─ sendPeerTransactions    group, encrypt, send over AMQP (Peer destinations)
       └─ sendTransactie          return TransactionForPeer for PublicDestinations
                                  (phase2 executes them against the public store)
```

---

## 1. Recipients: `computeUserRoleBottom`

Every `DeltaInTransaction` names the user role instances that have a perspective on the delta (`users`). When `addDelta` adds a delta to the transaction, it maps each such user role instance to a `TransactionDestination` with `computeUserRoleBottom`. The result is cached in `Transaction.userRoleBottoms`.

```purescript
data TransactionDestination
  = PublicDestination RoleInstance
  | Peer UnschemedResourceIdentifier PerspectivesUser
```

`computeUserRoleBottom rid` returns:

- **Nothing**, for the fictive `def:#serializationuser`.
- **`PublicDestination rid`**, if `rid` is an instance of a public proxy role (a `public` user role in ARC).
- **`Peer guid resource`**, if the role's filler chain bottoms out in a `TheWorld$PerspectivesUsers` instance (or another non-`NonPerspectivesUsers` user role such as `Onlookers`), and that user's `Cancelled` property is not `"true"`.
  - `guid` is the unschemed identifier of the user, for example `alice`.
  - `resource` is the schemed `PerspectivesUsers` instance that the PDR reads the peer's data from (see section 5).
- **Nothing**, if the chain bottoms out elsewhere or the user has been cancelled.

`addDelta` filters out the local user (`notIsMe`) before computing destinations. `transactieForEachUser` also never sends a delta back to its own author.

---

## 2. Customising per destination: `transactieForEachUser`

For every delta, the destinations of its `users` are looked up in `userRoleBottoms` and de-duplicated. The delta is appended to a `TransactionForPeer` per destination, in a `Map TransactionDestination TransactionForPeer`.

The `Eq` and `Ord` instances of `TransactionDestination` compare a `Peer` **on its unschemed guid only**. A peer reached through both a `pub:` and a `def:` resource is therefore a single destination and receives a single transaction. Within one transaction, the map keeps the schemed resource of the first entry inserted for that peer.

---

## 3. Public destinations

For a `PublicDestination`, `sendTransactie` returns the `TransactionForPeer` instead of sending it. `phase2` (in `Perspectives.RunMonadPerspectivesTransaction`) then:

- computes the publication URL from the public role's `at` expression;
- expands the deltas so that they target `pub:` identifiers;
- executes them locally against the public database.

These deltas never travel over AMQP. See [Public Resource Identifiers](public-resource-identifiers.md) for the `pub:` scheme.

---

## 4. Sending to peers: `sendPeerTransactions`

1. **Grouping.** Each `TransactionForPeer` is first stripped of public-key information for authors who do not occur in its deltas (`removeUnnecessaryKeys`). Peer transactions with identical serialisations are grouped, so one encrypted message serves several recipients.
2. **Transport keys.** `recipientTransportKey` reads `TheWorld$PerspectivesUsers$TransportPublicKey` from the schemed `resource` carried in the `Peer` destination. If a recipient has no transport key, that recipient is skipped.
3. **Encryption.** The payload is encrypted once with a symmetric key (`encryptForRecipients`). That key is wrapped for every recipient. Each wrapped key is labelled with the recipient's **unschemed** guid.
4. **Sending.** `sendTransactieToUserUsingAMQP` publishes the `EncryptedTransactionForPeer` to the AMQP topic named by the unschemed guid. Each PDR's queue is bound to its own guid as routing key. The message is also stored in the `OutgoingTransactions` post database until the broker sends a receipt. If there is no broker connection, it is only stored and is sent later.

On the receiving side, `incomingPost` uses the PDR's own unschemed guid to find its wrapped key. `executeTransaction` then runs the deltas. The `UniverseRoleDelta` and property deltas for the author's `PerspectivesUsers` instance create or update a local `def:#<guid>` copy of the sender (including its public keys).

---

## 5. `pub:` versus `def:` PerspectivesUsers resources

### Identities

A `PerspectivesUser` can appear in three forms:

| Form | Example | Where it is used |
|---|---|---|
| Unschemed guid | `alice` | Delta authors, AMQP topic, message ids, wrapped-key recipients, `OutgoingTransaction.receiver` |
| Local (`def:`) resource | `def:#alice` | The peer's `PerspectivesUsers` instance in the local store, created by that peer's own deltas |
| Public (`pub:`) resource | `pub:https://perspectives.domains/cw_bigbangsdatabase/#alice` | A `PerspectivesUsers` instance read from a published context |

`deltaAuthor2ResourceIdentifier` turns an unschemed guid into `def:#<guid>`; a `pub:` identifier is left as it is.

### The problem

A PDR can learn about a peer purely through a published context before that peer has ever sent it a transaction. An example is signing up to a published BrokerService: the BrokerService's Administrator is filled with `pub:…#alice`. Deltas for that peer then bottom out in the `pub:` resource.

Previously, the destination was reduced to `Peer "alice"`, and the transport key was read from `deltaAuthor2ResourceIdentifier "alice"`, which is `def:#alice`. That resource did not exist locally. As a result:

- the referential-integrity fixer was triggered;
- `getProperty` swallowed the error;
- the transport key was `Nothing`;
- the transaction was **silently dropped**.

### The rule

`Peer` carries both the unschemed guid and a schemed resource. `computeUserRoleBottom` chooses the resource as follows:

1. If the chain bottoms out in a `def:` (or other non-public) resource, use it.
2. If it bottoms out in a `pub:` resource, check whether the local copy `def:#<guid>` exists (`entityExists`, which does not trigger the fixer).
   - If it exists, **prefer the `def:` copy**. The peer's own deltas keep it up to date, for example after key rotation or cancellation.
   - Otherwise, use the `pub:` resource.

The `Cancelled` check and the transport-key lookup both read from the chosen resource. Everything that identifies the peer on the wire uses only the unschemed guid.

Once the peer's first transaction has been received, `def:#<guid>` exists, and later transactions use the local copy.

### Known limitations

These paths still assume that every peer has a local `def:` `PerspectivesUsers` instance. They may fail in the same way for a peer that is known only through a `pub:` resource:

- `getPkInfo` in `addPublicKeysToTransaction` (`Perspectives.Deltas`). It re-schemes delta authors with `deltaAuthor2ResourceIdentifier`. This matters when forwarding deltas authored by such a peer.
- `tryGetPublicKey` in `Perspectives.Authenticate`.
- `Cancelled` lookups in `Perspectives.TypePersistence.PerspectiveSerialisation`.
- Other uses of `deltaAuthor2ResourceIdentifier` in general.

Also, until the peer's first transaction arrives, the PDR reads the peer's data from the published copy. A key rotation or cancellation is noticed only after the peer has republished.

---

## Related modules

- `Perspectives.Deltas`: `addDelta`, `computeUserRoleBottom`, `distributeTransaction`, `transactieForEachUser`, `sendPeerTransactions`, `sendTransactieToUserUsingAMQP`
- `Perspectives.Sync.Transaction`: `Transaction`, `TransactionDestination`
- `Perspectives.Sync.TransactionForPeer`: `TransactionForPeer`, `EncryptedTransactionForPeer`
- `Perspectives.RunMonadPerspectivesTransaction`: `phase2`, which calls distribution and executes public-destination deltas
- `Perspectives.AMQP.IncomingPost`: the receiving side
- `Perspectives.Instances.ObjectGetters`: `deltaAuthor2ResourceIdentifier`
- `Perspectives.ResourceIdentifiers`: `isInPublicScheme`
