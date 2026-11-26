# Research question: historical authorization provenance for property deltas

## Objective

Investigate how a receiver could verify that an incoming property delta was
authorized in the author's causal state when it was constructed, rather than
only against the receiver's current instance state.

This is a research task, not a request to implement a particular solution.
Assess what existing history and causal metadata can establish, what additional
signed provenance would be needed, and what limitations would remain.

## Current type-level check

Incoming property authorization uses
`hasPerspectiveOnPropertyWithVerbByImplication` in
`src/core/typerepresentation/typeLevelObjectGetters.purs`, called by
`roleHasPerspectiveOnPropertyWithVerb` in
`src/core/typeChecking/authorization.purs`.

For a subject role type, property type and property verb, it searches the
subject's perspectives, including role aspects, for a perspective satisfying:

1. The perspective grants the requested property and verb in a declared state.
2. Its object abstract type, `PerspectiveObjectADT`, implies the property's
   declaring role type, `AddingRoleType`, using existing CNF type comparison.

For example:

```text
Ambassadeur has a perspective on Deelnemer granting SetPropertyValue for Street.

Deelnemer -> Persons -> (PerspectivesUsers OR NonPerspectivesUsers)

Both filler alternatives have aspect Addressable, which declares Street.
Therefore PerspectiveObjectADT implies Addressable.
```

This establishes a type-level permission, not the relationship between the
signer, the particular perspective object and the particular storage instance.
A successful type-level check currently bypasses the instance fallback.

When implication fails, the existing `hasInstancePropertyPermission` fallback
tries to reconstruct a concrete witness from locally available bindings and
query results. The compiler's separate `hasPerspectiveOnPropertyWithVerb`
predicate remains unchanged and retains its target-role check.

## Proposed enrichment of the signed property delta

In addition to the operation, values and actual storage target, record:

- The perspective subject instance: the concrete acting role.
- The declared subject role type: whose perspectives authorize the operation.
- The perspective object instance: the object through which the property was
  accessed, before resolving its storage.
- The declared object role type.
- The context instance anchoring evaluation of the subject and perspective.

These members must be covered by the author's signature.

The context is an evaluation anchor, not necessarily the containing context of
both concrete instances. Calculated roles can select instances stored in other
contexts and having other concrete role types. The declared subject type must
therefore not be inferred from the subject instance's stored type.

The existing property delta identifies the property-bearing instance, for
example NonPerspectivesUsers, rather than the original Deelnemer object.
Preserve the distinction between that storage endpoint and the authorization
object, including property replacements and storage-key/version semantics.

With the proposed members, a receiver could verify:

1. The signed author fills the supplied subject instance, directly or indirectly.
2. Evaluating the declared subject role in the supplied context yields that
   subject instance.
3. A perspective of the declared subject type grants the property and verb.
4. Evaluating that perspective's object query for the supplied actor and context
   selects the supplied object instance, respecting filters and self-only rules.
5. Resolving the property from that object reaches the delta's exact storage
   instance and stored property, respecting replacements.

The intended proof path is:

```text
signer -> subject instance
       -> applicable perspective in evaluation context
       -> selected object instance
       -> exact property storage
```

## Model and causal provenance already present

Do not assume that unversioned runtime type identifiers mean that signed type
references lack versions. Signed delta serialization carries revisioned type
references; normal runtime lookup uses unversioned identifiers.
See [Model and Type Versioning in Persistent Deltas](model-and-type-versioning.md).
That design distinguishes a type's semantic revision from the containing
model's release version and specifies immutable releases and dependency locks.
Verify which parts of that design are implemented before relying on them.

Property-delta signing already includes a causal stamp, reserved for the
property-bearing role. Per-role causal clocks cover that role's mutations, not
all other roles and properties read during authorization. Determine whether
existing causal metadata is sufficient to identify any relevant historical
authorization state; do not assume that it supplies a cross-resource snapshot.

Relevant implementation areas include:

- `src/core/sync/versionedDelta.purs`
- `src/core/sync/causal.purs`
- `src/core/persistence/causalStore.purs`
- `src/core/persistence/deltaStore.purs`
- `src/core/persistence/propertyHistoryStore.purs`
- `src/core/sync/propertyHistory.purs`
- `src/core/sync/constructionHistory.purs`
- [Delta ordering and recovery](../docsources/delta-ordering.md)

## Remaining limit

The proposed signed members identify an asserted authorization path. They do
not identify the historical bindings and filter inputs against which to
evaluate it.

For example, after a legitimate write is signed:

- The acting subject may be rebound to another signer.
- A property used in a perspective filter may change, excluding the object.
- A calculated subject or object query may yield different instances.

A receiver using its current state may reject an originally authorized write.
Conversely, its current state may allow a write that was unauthorized in the
author's state.

A signature establishes attribution to the signing key and protects the
assertion's contents. It does not by itself prove the historical authorization
assertion true. Versioned type references address model/type provenance, but do
not identify historical instance state.

## Questions to investigate

1. What precise authorization-time policy is feasible: the author's evidenced
   causal state, the receiver's current state, or another explicitly defined
   policy? Distinguish causal order from wall-clock signing time.
2. Can existing signed deltas, per-role clocks and construction histories
   reconstruct the bindings and query inputs needed for the proposed proof?
3. What minimal additional signed information would identify those dependencies:
   exact delta identifiers, per-resource versions, causal frontiers, a read set,
   or another representation? Compare concrete alternatives.
4. How would dependencies be captured for calculated roles, filters, `me`,
   aspect-property replacements and queries using absence of data? What cannot
   be replayed reliably, such as time-sensitive or external inputs?
5. What historical model artifacts and dependency locks must remain available to
   evaluate the recorded type revisions correctly?
6. How do property-history pruning, checkpoints, deleted/recreated roles and
   retained binding history constrain reconstruction? Distinguish original
   signed evidence from trusted checkpoint assertions.
7. How would the necessary evidence reach the receiver without disclosing data
   it is not entitled to see? Can existing dependency shipping and history
   requests be reused, and what changes would be required?
8. What can stop an author from presenting a stale but once-authorized state or
   omitting an observed revocation? What policy applies to concurrent revocation
   and writes? State which guarantees are impossible without stronger trust or
   coordination.
9. How should unavailable evidence differ from proven lack of permission in
   pending/retry/quarantine handling? Never treat missing evidence as permission.
10. What are the storage, wire-size, query-evaluation and implementation costs?
    Can a staged implementation reuse current infrastructure without embedding
    complete state snapshots in every delta?

## Expected outcome

Produce an evidence-backed assessment referencing implementation symbols and
existing design documents. Clearly separate:

- What is already implemented.
- What current retained history can prove.
- What needs additional signed metadata, retention or evidence transport.
- Guarantees that depend on trust assumptions or an explicit revocation policy.

Recommend the smallest coherent design, with alternatives, limitations and a
few concrete scenarios demonstrating authorized historical replay, false
rejection prevention, unauthorized-write rejection and concurrent revocation.
Do not change runtime code as part of this investigation.

## Decisions and implementation status

**Stage 0 (implemented).** No change to the delta format.

- `verifyTransaction` rejects unsigned peer deltas (including key-establishing
  deltas): authorship must be attributable before anything is executed.
- `provenUnauthorizedPropertyDeltas` quarantines a transaction when a property
  delta's signed subject type has no perspective granting any property with the
  required verb in the receiver's model. The quarantine reason names the signed
  author. Every other authorization failure is treated as missing evidence: the
  operation is not applied and the transaction is retried as a pending record.
  Missing evidence never permits.
- The non-repudiation window (checkpoint plus ten retained versions, unbounded
  for `fullhistory` properties and for binding/membership/constructor history)
  is documented in `docsources/delta-ordering.md` §8.1.

**Stage 1 (decided, not implemented).**

- The signed property delta carries the subject instance, the object instance
  and the **evaluation context instance** from which the subject query starts.
  The context cannot be derived from the subject instance: a calculated subject
  can be the result of a query that starts in a context other than the
  subject's own context.
- The authority bundle may disclose the sender's intermediate filled roles
  between the signer and the subject instance. This privacy cost is accepted:
  these roles are largely exposed as soon as the receiver knows the sender.
- Targeted history requests for missing evidence use the signed instances to
  name the resources to request.
