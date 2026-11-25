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
  │  ├─ save changed DomeinFiles
  │  ├─ sortTransactionDeltas
  │  ├─ addPublicKeysToTransaction
  │  └─ transactieForEachUser       one TransactionForPeer per destination
  ├─ publicRoleTransactions         execute PublicDestinations against the public store,
  │                                 then saveMarkedPublicResources
  └─ sendPeerTransactions           group, encrypt, send over AMQP (Peer destinations)
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

### Historical deltas and transaction membership

Dependency serialization forwards retained history that has not already been
durably queued for the destination. It does not hash or persist stored records
again when their content IDs are available. Newly created deltas follow the
normal persistence path, including property-history retention.

The persistent `{systemIdentifier}_peerdelivery` database records content IDs per
peer/resource pair only after the original encrypted packet is saved in the
outbox. An assembly or outbox failure cannot suppress an unqueued change. Retries
preserve the original packet. Conflicting and late historical changes are tracked
by content ID, not merely by their version number. Knowledge IDs are intersected
with retained history, so bounded properties do not acquire an unbounded delivery
index.

Newly gained perspectives explicitly initialise retained history even if the
peer saw it during an earlier period of visibility. This includes empty/deleted
property state. Ordinary known-visibility updates send only new changes; a new
checkpoint is omitted when the destination already knows its covered version.
Public-store distribution does not use peer-delivery knowledge.

Stored properties retain a checkpoint plus ten recent logical versions by
default; `fullhistory` disables audit-history pruning for that property. Identity
key proofs remain intact, and the latest file-publication operation is retained
as operational evidence. See [delta-ordering.md](delta-ordering.md#81-property-history-retention-and-checkpoints).

Each transaction maintains an in-memory membership set alongside its ordered
delta array. Membership compares the entire signed delta (author, payload and
optional signature) and the ordered recipient array, exactly like the previous
duplicate check. Different recipients or signatures remain distinct. The array
preserves first-insertion order, and recipient destinations are refreshed even
for duplicate additions.

New and embedded transactions start with an empty index; test deserialization
reconstructs it from the delta array. After distribution, the array and index are
cleared together so subsequent transaction phases can add the same deltas again.
The index is neither persisted nor sent to peers.

### Replacing a user-role filler

A replacement initializes the new peer just like a first-time fill. The replacement
delta is recorded first; the deferred `RoleUnbinding` assignment then updates both
binding links through `changeRoleBinding`.

Before calling `handleNewPeer` and `addDeltasForPerspectiveObjects`,
`changeRoleBinding` updates the local role's `isMe` flag and the context's `me`.
This order is essential when handing a locally occupied role to a remote peer:
both `handleNewPeer` and `addDelta` would otherwise exclude the new recipient
because of the outgoing user's stale `isMe` flag.

An alternative `me` must actually represent the local user and must not be the
outgoing role. A preferred user role type is retained only if it still computes a
local participant other than the outgoing role; otherwise the preference is
cleared. This applies to enumerated and calculated preferences alike. Selection
then tries the current/remaining preferred role type, followed by other local
user-role types; if no alternative exists, the context's `me` is cleared.

Both preference changes and clearing `me` use the shared setters that schedule
`Me` subscriptions for refresh. Subscribers therefore receive the alternative
role instance and type, or empty results when no local participation remains.
The context itself is not automatically removed.

For a non-local user role, `handleNewPeer` serializes the role type's perspectives
with `newPeer = true`, including a minimal self-perspective when none is modelled.
These initialization deltas join the current transaction and are addressed through
the new filler chain. Binding removal and roles scheduled for removal do not
initialize a new peer.

Initialization also follows the roles transitively filled by the changed role.
If the peer newly fills U, and U already fills V, V's perspectives are serialized
as well, in V's context. The same applies to further filled roles and branches,
including user roles reached through non-user roles. Only user roles contribute
perspective serializations; non-user roles are traversed, not granted perspectives.
All these deltas join the same transaction and reach the same peer through their
current filler chains. Each role's perspectives still limit which data is sent.

On local-to-remote replacement, transitively filled roles may also retain cached
local-user flags. These flags are cleared before selecting alternative local roles
and serializing perspectives, so they cannot suppress the new peer's initialization.
The existing alternative-role selection also updates their contexts' `me`,
preferences, and `Me` subscriptions.

---

## 2. Customising per destination: `transactieForEachUser`

For every delta, the destinations of its `users` are looked up in `userRoleBottoms` and de-duplicated. The delta is appended to a `TransactionForPeer` per destination, in a `Map TransactionDestination TransactionForPeer`.

The envelope's `author` identifies the current sending PDR's user, without a
storage scheme; `perspectivesSystem` identifies that user's sending installation.
This also applies to public-destination transactions, where the installation
publishes on behalf of the public role. The encrypted AMQP envelope copies the
same sender identity.

New envelopes also carry `causalEpoch: "perspectives-causal-1"`. This marks a
transaction assembled by the upgraded runtime, not the age of every retained
delta it discloses. Old ordered history can therefore initialise a newly
entitled peer as baseline evidence without rewriting original signatures.
The complete transaction, including the epoch, is encrypted. Persisted old
outbox packets retain their original framing and are quarantined on receipt
after cutover; they are never automatically upgraded or replayed. See
[the coordinated cutover procedure](delta-ordering.md#92-upgrade-procedure).

An envelope does not inherit the author of its first delta: reconstruction may
forward historical deltas from other users, and filtering may remove that first
delta altogether. Each signed delta retains its original author and signature.
Recipient exclusion and verification-bundle selection still use those individual
delta authors, not the envelope sender. Merely forwarding a transaction therefore
does not add the sender's verification bundle.

The `Eq` and `Ord` instances of `TransactionDestination` compare a `Peer` **on its unschemed guid only**. A peer reached through both a `pub:` and a `def:` resource is therefore a single destination and receives a single transaction. Within one transaction, the map keeps the schemed resource of the first entry inserted for that peer.

For `Peer` destinations, customisation removes the creation scaffolding of the
private `TheWorld` and `TheSocialEnvironment` containers: `ConstructEmptyContext`,
`ConstructExternalRole`, and `AddExternalRole`. Every installation creates these
locally. Public-key bundles whose authors have no remaining deltas are removed,
and transactions left without deltas are dropped. Contents of these containers,
including memberships, bindings and external properties, are not excluded.

`PublicDestination` transactions keep this scaffolding so publication can create
public copies. Explicitly `pub:`-addressed resources are not filtered either.
Local bootstrap history remains available for restoration; it is not deleted.

### Verification bundles and storage aliases

`addPublicKeysToTransaction` collects verification information using each delta
author's original identifier, then combines bundles by unschemed identity.
Consequently, a transaction containing both `alice` and `pub:...#alice` authors
carries one verification bundle for Alice. If a public locator is present, it
remains the bundle's representative identifier so receivers can still fetch
public resources.

Matching bundles retain the union of their signed evidence, without changing
delta authors, payloads, signatures or resource identifiers. Signing and
transport keys must agree; conflicting key material causes an explicit error
rather than silently choosing one alias. JSON key ordering and whitespace do
not affect this comparison.

---

## 3. Public destinations

`publicRoleTransactions` selects the `TransactionForPeer`s for `PublicDestination`s. `phase2` (in `Perspectives.RunMonadPerspectivesTransaction`) then:

- computes the publication URL from the public role's `at` expression;
- expands the deltas so that they target `pub:` identifiers;
- executes them locally against the public database;
- saves the public resources marked for storage (`saveMarkedPublicResources`).

Only then does it send the peer transactions. So a peer that learns about a public resource can retrieve it: it does not have to wait for the periodic persistence pass. These deltas never travel over AMQP. See [Public Resource Identifiers](public-resource-identifiers.md) for the `pub:` scheme.

---

## 4. Sending to peers: `sendPeerTransactions`

1. **Grouping.** Each `TransactionForPeer` is first stripped of public-key information for identities who do not occur in its deltas or the original winners embedded in checkpoints (`removeUnusedPublicKeys`). This comparison ignores storage schemes, so a public-locator bundle still supports an ordinary author identifier. Peer transactions with identical serialisations are grouped, so one encrypted message serves several recipients.
2. **Transport keys.** `recipientTransportKey` reads `TheWorld$PerspectivesUsers$TransportPublicKey` from the schemed `resource` carried in the `Peer` destination. If a recipient has no transport key, that recipient is skipped.
3. **Encryption.** The payload is encrypted once with a symmetric key (`encryptForRecipients`). That key is wrapped for every recipient. Each wrapped key is labelled with the recipient's **unschemed** guid.
4. **Sending.** `sendTransactieToUserUsingAMQP` publishes the `EncryptedTransactionForPeer` to the AMQP topic named by the unschemed guid. Each PDR's queue is bound to its own guid as routing key. The message is also stored in the `OutgoingTransactions` post database until the broker sends a receipt. If there is no broker connection, it is only stored and is sent later.

### Coordinating outgoing sends

The outgoing message is saved before publication and deleted only after a broker
receipt. On connection, `sendOutgoingPost` replays the messages still stored.
This replay can overlap normal sending, including a transport retry waiting for
the actual STOMP connection even when the model already reports a connection.

The JavaScript transport reserves each message/receipt ID per client connection.
Normal sending, connection replay and retry timers share that reservation, so
they cannot publish the same message independently. The reservation retains only
send bookkeeping, not the message body. It remains after receipt because an
outbox snapshot taken before deletion may still attempt to send that ID.

Disconnect, WebSocket closure and explicit deactivation cancel pending timers and
clear the reservations. A subsequent connection can therefore replay messages
whose receipts were not received. Receipts from an invalidated send attempt are
ignored. Publication errors release the reservation and propagate; exhausted
connection retries log an error and leave the durable message available for a
later attempt.

This prevents redundant publication within a connection, not all duplicate
delivery: reconnecting after a lost receipt can legitimately resend a message.

On the receiving side, `incomingPost` uses the PDR's own unschemed guid to find its wrapped key. `executeTransaction` then runs the deltas. The `UniverseRoleDelta` and property deltas for the author's `PerspectivesUsers` instance create or update a local `def:#<guid>` copy of the sender (including its public keys).

Before importing verification keys or executing deltas, `executeTransaction`
also removes predefined-container scaffolding and unused key bundles. Thus
redundant histories received from older senders do not create or retain identities
needed only to authenticate those histories. Identity-document exports use the
same filtering rule.

Receivers also combine legacy duplicate bundles before importing their evidence.
Verified signing keys are indexed by unschemed identity, allowing both public and
ordinary delta authors to use the single bundle. A locally known identity key is
preferred; the retained public locator is used when no local key is available.
All content deltas still undergo signature verification.

The transaction record shape is unchanged. However, receivers of a single bundle
supporting mixed author aliases must support identity-based verification; older
receivers that require an exact bundle-map entry for every author alias need to
be updated.

### Opt-in timing diagnostics

Timing diagnostics are separate from log-topic thresholds and the performance
test collector. They are disabled by default.

To capture a manager's own broker connection from the start, open MyContexts
with `?pdrTraceTimings=true` on its page URL, for example:

```text
https://mycontexts.com/www/?pdrTraceTimings=true
```

Use `&pdrTraceTimings=true` if the URL already has query parameters. Only the
literal value `true` enables the switch. MyContexts sends an enable command over
the existing proxy port before any PDR startup request, in both SharedWorker and
page-worker modes. This enables `globalThis.pdrTraceTimings` in the execution
context that actually hosts the PDR; the worker script URL and name are unchanged.
The host console reports `[PDR] Startup timing tracing enabled`.

For a **cold-start trace**, first close all tabs connected to that PDR, including
standalone ARC editor tabs, then open the tracing URL. Adding the parameter to an
already-running installation enables future spans but cannot recover earlier
ones. A later tab without the parameter does not disable tracing in a shared PDR.
Remove the parameter and close all connected tabs to return to the default on
the next start, or disable it directly in the PDR's console as shown below.
The URL switch does not enable SYNC payload logging or normalisation measurements.

For an already-running PDR, use the **PDR SharedWorker's DevTools console** (or
the page worker's execution context) before signup:

```javascript
globalThis.pdrTraceTimings = true;
```

Enable this independently for the subscriber and manager. Setting it in the
ordinary page console does not enable a separate worker. The flag is not
persistent: enable it again after restarting the worker. Keep the manager running
and enable tracing before the subscriber starts signup; there is no need to pause
execution or enable verbose query/perspective logging.

Collect console lines beginning with `[PDR TIMING]` (Info level), along with
WebSocket frame timestamps. Disable tracing afterwards:

```javascript
globalThis.pdrTraceTimings = false;
```

Each line contains JSON with an event (`start`, `end`, or `event`), stage, ID, UTC
wall-clock timestamp (`at`) and worker-local monotonic timestamp (`monotonicMs`).
Span starts and ends share a unique worker-local `span` number; ends include
`elapsedMs` and `succeeded`. Durations use the monotonic clock, not the wall clock.
Turning tracing off suppresses outstanding span completions too.

| Stage | What it measures or marks |
|---|---|
| `transport.connected` | Entry into the STOMP connection callback |
| `transport.receive` → `consumer.receive` | JavaScript arrival through producer buffering and envelope decoding to consumer arrival |
| `consumer.ack` | Sending the consumer ACK, before decryption or execution |
| `connection.state` | Connection-state transaction, including its admission wait and state cascade |
| `outbox.replay`, `outbox.save`, `outbox.receipt-delete` | Durable outgoing-post work |
| `incoming.decrypt` | Transport decryption and transaction JSON decoding |
| `incoming.admission.requested` → `incoming.admission.granted` | Incoming transaction admission, including the deliberate event-loop yield |
| `admission.wait`, `admission.yield` | Queue waiting and the event-loop yield separately, for user and background work |
| `transaction.flag.wait` | Waiting for the transaction flag after priority admission, where applicable |
| `transaction.action`, `transaction.cascade` | Initial transaction action versus subsequent state/action cascades |
| `incoming.verify`, `incoming.execute` | Verification versus delta execution, persistence, ordering and version checks |
| `distribution.prepare`, `public.publish`, `peer.send` | Recipient preparation, public publication and encrypted peer sending |
| `transport.publish`, `transport.published`, `transport.receipt` | Attempted publication, successful return from the publish call and broker receipt callback |
| `incoming.transaction`, `incoming.public-states`, `incoming.total` | Incoming transaction, public-state follow-up and complete post-ACK processing |

Incoming transport/consumer stages use the publisher's receipt ID when available;
otherwise a local `incoming-N` ID is carried unchanged from callback to consumer.
Outgoing stages use the durable outgoing-post ID. Transaction stages use the
local transaction number; their spans occur inside the corresponding incoming or
connection span. Generic admission stages identify the priority lane. No message
bodies, keys, credentials or exception details are included.

Compare the WebSocket receive timestamp with `transport.receive` to detect
delayed JavaScript callback execution, then compare `transport.receive` with
`consumer.receive` to detect consumer/producer backlog. Inspect overlapping
connection, admission and transaction spans to locate the wait. Spans are nested
and must not be added together. Monotonic timestamps and span numbers cannot be
compared between workers; cross-device wall-clock comparisons require aligned
clocks. Tracing itself adds console overhead and does not change ACK ordering or
delivery guarantees.

#### Headless broker signup

With the three development domains routed locally, and local Apache, CouchDB and
RabbitMQ running, use `pnpm run test:brokerServiceSignup:diagnostic` from
`packages/perspectives-core`. This requires `mkcert`, local `rabbitmqctl` access,
and a completed `rebootedUniverse/alice` test snapshot.

The diagnostic enables timing before starting either PDR, restores the manager,
creates a fresh subscriber, uses real STOMP and
the ordinary public-resource `AddThisServer`/`SignUp` actions, and enables SYNC
tracing. It checks the manager's receipt of the subscriber's account-holder
details and the subscriber's active queue, then waits for both PDRs to settle.
Unlike the full signup suite, it does not create a CouchDB account or run contract
termination tests. Temporary RabbitMQ accounts and queues are removed on exit.
Each invocation uses a fresh Node process and has a five-minute execution limit.

`signup.*` spans distinguish startup, management setup, manager subscription,
subscriber subscription, handshake and cleanup. Both PDRs share one process:
generic transaction numbers can overlap, whereas transport receipt IDs identify
individual messages. Node uses in-memory PouchDB, and snapshot restoration skips
the browser's API startup/model-upgrade pass, so browser latencies need not match.
Keep captured SYNC logs private: unlike timing records, they can contain payloads.

The diagnostic also enables `globalThis.pdrMeasureNormalisation = true` before
startup and prints aggregate `[PDR NORMALISATION]` records on completion, including
failure exits. This independent flag is off by default in ordinary PDRs.
Measurements cover role lookup, expansion and CNF conversion on runtime cache
misses and compilation calls, not cache hits or compound-type composition.
Records separate PDRs and calls started during
model compilation from runtime calls, and include workflow-phase breakdowns.
Each phase is captured when a call starts, even if it finishes in a later phase.

Reports contain request counts, distinct role identifiers, success/failure and
unfinished counts, cumulative elapsed time, and the ten roles with the greatest
repeated elapsed cost. A repeated request starts after a successful call for the
same role in that PDR and compilation context; phase breakdowns additionally
require an earlier successful call in the same phase. No per-call logs or
normalised results are retained. The counters observe repetition, not actual
cache hits: they do not model definition changes or eviction. Cumulative timings
can overlap and include asynchronous waits, so repeated elapsed time is not a
prediction of end-to-end speedup. Runtime cache behaviour and invalidation are
described in [type-comparison.md](type-comparison.md#the-combined-shortcut-toconjunctivenormalform_).

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

- `Perspectives.Deltas`: `addDelta`, `computeUserRoleBottom`, `distributeTransaction`, `transactieForEachUser`, `publicRoleTransactions`, `sendPeerTransactions`, `sendTransactieToUserUsingAMQP`
- `Perspectives.Sync.Transaction`: `Transaction`, `TransactionDestination`
- `Perspectives.Sync.TransactionForPeer`: `TransactionForPeer`, `EncryptedTransactionForPeer`
- `Perspectives.RunMonadPerspectivesTransaction`: `phase2`, which calls distribution and executes public-destination deltas
- `Perspectives.AMQP.IncomingPost`: the receiving side
- `Perspectives.Instances.ObjectGetters`: `deltaAuthor2ResourceIdentifier`
- `Perspectives.ResourceIdentifiers`: `isInPublicScheme`
