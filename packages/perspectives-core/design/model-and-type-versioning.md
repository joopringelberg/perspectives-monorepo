# Model and Type Versioning in Persistent Deltas

## Status

This document records design decisions for adding model and type version
provenance to Perspectives models and deltas.

The immediate goal is not to implement complete compatibility checking. The
immediate goal is to preserve enough authenticated information in the new delta
format that compatibility policies can be implemented later without another
Perspective Universe reboot.

This design complements:

- [Delta Representation Redesign Proposal](delta-representation-redesign.md)
- [Deterministic Delta Ordering](deterministic-delta-ordering.md)
- [Undo Facility](undo-facility.md)

## 1. Motivation

A Perspectives model is a versioned package of context, role and property
types. Models import other models. A PDR installation currently keeps one
unversioned installed representation of each model and its types.

This produces three related problems:

1. A model to be installed may require a newer version of an imported model
   than the version currently installed.
2. Updating an installed model may require newer versions of one or more
   imports and may affect other installed dependents.
3. An incoming delta may have been authored using a different version of a
   referenced type than the receiver has installed.

The third problem is strategically important because deltas are permanent,
signed historical records. Together, persisted deltas form the history of the
visible Perspective Universe. Their signed payload cannot be amended later
without invalidating the author's signature.

The current universe reboot provides a rare opportunity to change this payload.
If type-version provenance will ever be needed, it must be included now or a
future compatibility implementation will lack trustworthy historical input.

## 2. Terminology

### 2.1 Model identity and version

A stable model identifier identifies a model lineage and functions as a
namespace for its types. A model version identifies an immutable release in
that lineage.

```text
model identity:  model://example.org#Sales
model version:   3.2
model release:   model://example.org#Sales@3.2
```

For this rollout, model versions use only the `MAJOR.MINOR` subset of SemVer:

```text
MAJOR.MINOR
```

Both components are decimal non-negative integers. Patch numbers,
pre-release labels and build metadata are intentionally out of scope for this
delta format version.

Published model releases must be immutable:

```text
stable model identifier + version -> exactly one model artifact
```

A digest may additionally protect artifact integrity, but a digest does not
replace a version: a version provides ordering while a digest only establishes
content identity.

### 2.2 Stable type identity

A stable type identifier identifies a conceptual type across model versions.
It contains the stable identifier of its owning model. The owning model does
not therefore need to be repeated in a versioned type reference.

In delta format version 2, stable model and type identifiers are carried
unescaped and must therefore not themselves contain `@`. The final `@` in a
revisioned reference separates the stable identifier from its `MAJOR.MINOR`
revision suffix.

### 2.3 Type revision

A type revision is the version of the owning model in which the compiled
semantics of that type were introduced or last changed.

A type revision is not bumped merely because the owning model is released. An
unchanged type retains its earlier revision. This avoids creating artificial
differences between deltas that refer to semantically unchanged types.

### 2.4 Resource version

A resource version orders mutations of a context, role, binding or property
resource. It is unrelated to model versions and type revisions.

The implementation and documentation must keep these concepts distinct:

```text
model version       publication order of a model package
type revision       last model version that changed a type's compiled semantics
resource version    mutation order of an instance-level resource
delta format version serialization schema of a delta
CouchDB _rev         database document revision
```

## 3. Compatibility Is Not One Relation

Compatibility has several meanings in Perspectives.

### 3.1 Compilation compatibility

A model is compilation-compatible with a set of dependency releases if the
compiler can resolve and type-check every expression against those exact
releases.

This is substantially decidable at model compilation time. It does not prove
that the modeller's intended cooperative behaviour is preserved.

### 3.2 Existing-data compatibility

Existing contexts and roles may or may not remain valid under a newer model.
Examples include:

- a property range changes and stored values no longer fit;
- a role binding restriction becomes more specific and an existing filler is
  no longer permitted;
- a role or property becomes functional while multiple instances or values
  already exist;
- a user role is removed, potentially cutting users off from contexts;
- a role becomes linked and stored data needs a derived link administration.

Some changes are harmless, some are automatically repairable, and some require
a migration choice by the modeller or end user.

### 3.3 Delta admissibility

Delta admissibility asks whether one concrete incoming mutation can be applied
under the receiver's installed model generation.

Examples of decidable checks include:

- every referenced type exists and has not been removed;
- a referenced role belongs to the relevant context;
- a referenced property is available on the role or its aspect closure;
- incoming values satisfy the current range and constraining facets;
- an incoming filler satisfies the current binding restriction;
- current cardinality permits the operation;
- the subject is authorized by current local perspectives.

Compatibility can be operation- and state-dependent. For example, changing a
property from relational to functional may reject an `AddProperty` when a value
already exists while still allowing a `SetProperty` that replaces all values
with one value.

### 3.4 Behavioural compatibility

A delta can trigger much more than a structural mutation. Current Perspectives
execution can involve:

- perspectives and authorization;
- state transitions;
- automatic actions and notifications;
- inverted queries and dependency tracking;
- synchronization and peer selection;
- effects contributed by imported or dependent models.

It is generally impossible to prove that these behaviours preserve the
modeller's intention across versions. A newly added action may repair an old
omission or introduce an unwanted side effect; the runtime cannot infer which.

This limits automatic compatibility checking. Model versions establish update
direction, but they do not prove behavioural substitutability.

## 4. Logical Monotonicity and Its Limit

Some type evolution can be described as logical monotonicity. Adding a property
to a role, for example, expands the theory without invalidating implications
that used only the old property set. This supports useful static reasoning about
role specialization and model expressions.

Perspectives models are not purely monotonic theories, however. They include
negation, type reflection, perspectives, states and effects. Even an apparently
monotonic addition may change a reflected property count or activate new
behaviour.

The design therefore uses monotonic reasoning where it is sound and useful,
but does not promote it to a general claim of behavioural compatibility.

## 5. Version Selection for Model Expressions

A model must be compiled against a reproducible dependency environment.
Compilation must not silently mean "whatever happens to be installed locally"
or "the latest available version".

Each import should eventually provide a modeller-declared version constraint:

```text
Sales requires Persons >= 2.1 and < 3.0
```

The compiler or dependency resolver selects an exact release satisfying that
constraint and records both:

```text
declared requirement: Persons >= 2.1 and < 3.0
resolved release:     Persons@2.4
```

The modeller, not the compiler, declares the oldest supported release. The
compiler can prove that a model compiles against a selected release; it cannot
infer the oldest release that preserves the modeller's intention.

Useful tooling should compile and test a model against:

- the declared lower bound;
- the exact locked dependency set used for publication;
- optionally, the newest releases allowed by the constraints.

## 6. Model Version Dependency Administration

### 6.1 Declared requirements

A model source or its publication manifest should contain explicit constraints
for direct imports. These constraints are part of the model's authored contract.

Unversioned imports may remain temporarily supported during transition, but a
published model should eventually be rejected or warned about when its imports
lack explicit requirements.

### 6.2 Resolved dependency lock

Every published compiled `DomeinFile` should record the exact direct dependency
releases against which it was compiled. Because each dependency records its own
lock, the transitive dependency environment can be reconstructed.

Conceptually:

```purescript
type ModelDependency =
  { modelId :: StableModelId
  , requirement :: VersionConstraint
  , resolvedVersion :: SemVer
  }
```

The existing `referredModels` field only records model references. It should be
supplemented or replaced by dependency records that preserve requirements and
resolved versions.

### 6.3 Installation and update resolution

Models are installation and update units. Individual types are not installed
or updated independently.

Before activating a model installation or update, the PDR should eventually:

1. Resolve the complete dependency graph.
2. Detect mutually incompatible constraints.
3. Check affected installed dependents.
4. Fetch the required immutable model releases and sidecars.
5. Determine required data migrations or repairs.
6. Compile or validate the complete generation.
7. Atomically activate the resulting package set.

Incoming deltas may reveal that a newer model release is needed, but an
untrusted peer delta must not silently force an installation-wide update. The
PDR should quarantine the delta, stage dependency resolution, and follow the
installation's update and consent policy.

### 6.4 Reconstructing the relevant model environment

A separate complete model-environment map need not be stored in every delta.
A revisioned type reference determines:

```text
type identifier
  -> owning model identifier
  -> model release at the recorded type revision
  -> exact resolved dependency lock of that release
  -> transitive compilation environment
```

This requires immutable releases and resolved dependency locks. Installed
models unrelated to the referenced model's dependency closure are not part of
this reconstructed environment.

## 7. Type Revision Administration

### 7.1 Type metadata

Compiled context, role and property definitions should carry their type
revision. Other named semantic artifacts may later need the same treatment.

Conceptually:

```purescript
type TypeVersionMetadata =
  { modelVersion :: SemVer
  , removedIn :: Maybe SemVer
  }
```

A live type has `removedIn = Nothing`. Removal history belongs in a model
manifest or sidecar because a removed type is no longer present in the current
`DomeinFile` type collections.

### 7.2 Computing the revision

When compiling model version `V`, compare each new compiled type with its
predecessor:

```text
new type                     -> revision V
unchanged compiled semantics -> retain predecessor revision
changed compiled semantics   -> revision V
removed type                 -> tombstone at V
```

The comparison should exclude non-semantic publication and presentation data,
for example CouchDB revisions, source positions and display names, unless a
specific runtime feature treats such a field as semantic.

It should include generated or contextualized consequences. A local role source
may be unchanged while an imported aspect changes its complete type,
perspectives or actions. In that case the compiled role semantics changed and
its revision may need to advance.

A conservative first implementation may stamp every type with the current model
version of the release being compiled. This is noisy but safe. Semantic
last-change detection can be added later without changing the delta format.

### 7.3 Tombstones

A tombstone records that a stable type identifier existed and was removed in a
particular model version. It distinguishes an unknown type from a deliberately
removed type and supports decisions about older incoming deltas.

Tombstones should be retained in model history metadata even though full old
model releases need not be installed locally.

## 8. Permanent Signed Delta Shape

### 8.1 Design requirement

Type-version provenance must be covered by the author's signature. Placing it
only in `SignedDelta`, `TransactionForPeer` or `DeltaStoreRecord` metadata would
allow it to be altered independently of the signed mutation.

Each delta is stored and replayed independently of its original transaction.
Consequently, each signed delta payload must carry the provenance required to
interpret that delta.

### 8.2 Conceptual shape

The in-memory design may use an explicit revisioned type abstraction:

```purescript
newtype RevisionedType typeId = RevisionedType
  { identifier :: typeId
  , modelVersion :: SemVer
  }
```

The common delta record should also carry an explicit format version:

```purescript
type DeltaRecord f =
  { deltaFormatVersion :: Int
  , subject :: RevisionedType RoleType
  , resourceKey :: String
  , resourceVersion :: Int
  | f
  }
```

Every type reference in a delta should be revisioned, including optional type
references. For the current delta families this includes:

| Delta family | Revisioned type references |
|---|---|
| `UniverseContextDelta` | subject, context type |
| `UniverseRoleDelta` | subject, context type, role type, authorized role when present |
| `ContextDelta` | subject, context type, role type, destination context type when present |
| `RoleBindingDelta` | subject, filled type, filler type and old filler type when present |
| `RolePropertyDelta` | subject, role type, property type |

The subject is included because authorization depends on its perspectives. A
target property may remain structurally unchanged while authorization semantics
change in the subject role.

### 8.3 Compact signed serialization

The pleasant in-memory shape need not be the stored wire shape. To minimize
permanent storage, serialize a revisioned type as one string by appending the
model version to the stable type identifier:

```text
<stable-type-identifier>@<model-version>
```

For example:

```json
{
  "deltaFormatVersion": 2,
  "roleType": "model://example.org#stable-model$stable-role@2.4",
  "property": "model://example.org#stable-model$stable-role$stable-property@1.8"
}
```

The deserializer splits the final version suffix and reconstructs
`RevisionedType`. Normal runtime type lookup continues to use the stable,
unversioned type identifier.

For delta format version 2, the syntax is fixed as:

```text
revisioned-type-reference := <stable-type-identifier> "@" <major> "." <minor>
major                    := DIGIT+
minor                    := DIGIT+
```

Readers split on the final `@` and validate the suffix as `MAJOR.MINOR`.
Because `@` is reserved as the separator, no escaping scheme is required in
this format version. If a future rollout needs richer version syntax or `@`
inside stable identifiers, that requires a new delta format version rather than
reinterpretation of version 2 payloads.

This representation is preferred over a separate `typeRevisions` array because
it:

- does not repeat type identifiers;
- cannot dissociate a type field from its revision entry;
- avoids a secondary lookup after deserialization;
- remains compact for deltas containing few type references.

### 8.4 Delta format version

`deltaFormatVersion` is independent of model, type and resource versions. It
provides an explicit decoder dispatch point for future delta evolution.

Readers should dispatch by format version rather than infer a format from
missing fields. Multiple historical formats can then remain readable without
rewriting signed payloads.

For this rollout:

- legacy deltas without an explicit `deltaFormatVersion` are treated as format
  1;
- reboot-era deltas with revisioned type references use `deltaFormatVersion =
  2`;
- readers dispatch explicitly on that version and must not guess a newer format
  from missing or extra fields.

## 9. Runtime Policy for Version Differences

The initial policy is deliberately conservative and incomplete.

### 9.1 Incoming revision is newer

The receiver lacks model knowledge used by the authoring PDR.

```text
quarantine delta
resolve and stage the newer owning model package and dependencies
apply installation/update consent policy
migrate and atomically activate if accepted
validate and execute the delta
```

A type referenced by such a delta should exist in the referenced newer release;
otherwise the release or delta is inconsistent.

### 9.2 Revisions are equal

Validate and execute the delta under the installed model.

### 9.3 Incoming revision is older

If the installed model contains a tombstone for the type, do not execute the
delta automatically.

Otherwise, validate it under the current installed model and execute it unless
a known incompatibility is detected. This is **optimistic backward acceptance**,
not proof of full compatibility.

Initial incompatibility checks should focus on concrete hazards:

- invalid property range or constraining facets;
- prohibited role filler;
- functional cardinality violation;
- lost context-role or role-property membership;
- incompatible kind change;
- failure of current authorization.

The receiver executes current local authorization, state, query and action
semantics. Installing an old target type in isolation would not reconstruct the
historical behavioural environment and is not the chosen policy.

## 10. Compatibility Knowledge

A single `earliestCompatibleVersion` is insufficient in the general case.
Compatibility may be non-contiguous and may depend on operation, value and
current instance state.

For example:

```text
v1: property range String
v2: property range Number
v3: property range String
```

Version `v3` may accept data from `v1` but not from `v2`.

Reusable compatibility administration should therefore be based on model
release transitions and explicit check categories, conceptually:

```text
(type id, from revision, to revision, operation class) -> compatibility rule
```

Possible outcomes include:

```text
identical
unconditionally compatible
validate current value/state
migration required
incompatible
removed
unknown
```

Only unconditional results should be cached as simple facts. Conditional checks
must be rerun against the concrete delta and current state.

The first implementation does not need this administration. It only needs to
preserve revision evidence in new deltas and model artifacts.

## 11. Delta Identity and Duplicate Receipt

### 11.1 Operation key and exact delta identity

The current deterministic store key combines:

```text
resourceKey + resourceVersion + author
```

This identifies an authored claim on one resource version and supports ordering
and conflict resolution. It does not prove that two payloads are identical.

Introduce a content-derived delta identifier:

```text
deltaId = SHA-256(author || exact UTF-8 bytes of the signed payload string)
```

The signature need not be included. Signing the same exact payload twice should
still identify one delta.

Keep these concepts separate:

```text
operation key -> ordering and conflicting claims
deltaId       -> exact duplicate detection
```

The `deltaId` need not be inside the signed payload because it is derived from
authenticated material. It should be stored as indexed metadata and cached.

### 11.2 Early availability check

An incoming `SignedDelta` can be checked before payload deserialization:

```text
compute deltaId
look up deltaId
if exact delta is already known:
  use its stored processing status
otherwise:
  verify signature
  deserialize payload
  perform gap, model, compatibility and authorization checks
  store the resulting disposition
```

A known delta may be skipped before signature verification because the stored
copy has already been verified and equality is established from the author and
exact payload bytes. A changed payload produces another identifier and must go
through normal verification.

Do not skip verification based only on unverified `resourceKey`,
`resourceVersion` and author fields.

### 11.3 Processing status

The statement "a delta is in the local store" does not imply "the delta has
been executed". Deltas can be stored but unapplied because they are outdated,
lose a conflict, await a missing predecessor, await a model update, or fail a
compatibility policy.

The current `applied :: Boolean` should eventually become or be supplemented by
an explicit disposition, for example:

```text
Applied
Outdated
ConflictLoser
MissingPredecessor
MissingModelVersion
IncompatibleTypeRevision
Quarantined
```

Invalid signatures should normally not enter the historical DeltaStore.

Early deduplication divides received deltas into:

```text
known and applied
known but unapplied
unknown
```

Known and applied deltas are omitted from ordinary receipt execution and gap
analysis. Known but unapplied deltas retain their disposition unless a deliberate
retry policy says that the blocking condition may have changed.

Replay for resource reconstruction is a separate mode: an applied historical
delta may intentionally be executed again to rebuild missing local state.

### 11.4 Same operation key, different delta id

Two deltas with the same resource key, resource version and author but different
payload identities are not duplicates. They indicate author equivocation,
corruption or a serialization defect.

The store must not silently overwrite one with the other. A future store key may
therefore include a delta-id suffix while retaining indexed operation-key
fields:

```text
<operation-key>|<short-delta-id>
```

Both signed records can then be retained as evidence and handled by an explicit
conflict policy.

## 12. Storage Considerations

The delta history may become extremely large. Permanent per-delta overhead must
therefore be justified.

The current decisions minimize repeated data by:

- appending a compact version to each type identifier instead of storing a
  separate type-revision table;
- deriving `deltaId` rather than storing it in the signed payload;
- retaining indexable storage metadata outside the signed payload where it can
  be deterministically reconstructed;
- avoiding a complete model-environment map in each delta;
- storing exact dependency locks once per immutable model release.

Further compression or binary encoding can be considered later, but the
canonical signed representation must remain deterministic. A change in
serialization bytes changes both signatures and `deltaId` values.

A PDR should serialize a locally authored delta once, sign those exact bytes,
and reuse the resulting `SignedDelta` for storage and transport. Receiving PDRs
should preserve the exact signed payload bytes rather than deserialize and
reserialize before storage.

## 13. Phased Delivery

The work is intentionally split so the universe can reboot before complete
compatibility reasoning exists.

### Phase 0: Freeze the permanent contracts

Before rebooting:

1. Specify the delta format version mechanism.
2. Specify compact revisioned type-reference syntax.
3. Include revision provenance for every explicit type reference.
4. Specify canonical serialization and signing bytes.
5. Specify `deltaId` derivation.
6. Preserve operation-key fields for deterministic ordering.
7. Define how old and future delta formats are dispatched.

These are difficult or impossible to retrofit into signed history.

### Phase 1: Capture provenance without enforcing compatibility

For the reboot release:

1. Add type revisions to compiled type representations.
2. Initially stamp every compiled type in a release with that release's own
   model version if semantic last-change calculation is not yet implemented.
3. Serialize type revisions into every newly authored delta.
4. Deserialize revisioned references while continuing existing execution
   behaviour.
5. Persist and index `deltaId`.
6. Add efficient exact-duplicate receipt handling.
7. Store explicit processing dispositions where practical.

This phase captures historical evidence. It need not yet retrieve newer model
releases or reject older incompatible deltas.

### Phase 2: Model dependency administration

1. Add explicit import version constraints.
2. Record exact resolved dependency locks in compiled models.
3. Enforce immutability of published model releases.
4. Add dependency-graph conflict detection for installation and update.
5. Make installed model generations reproducible and atomically activatable.

### Phase 3: Type history

1. Compare compiled types with predecessor releases.
2. Retain the predecessor revision for unchanged types.
3. Record tombstones for removed types.
4. Produce human-readable model/type difference reports.
5. Classify straightforward structural changes.

### Phase 4: Runtime compatibility checks

1. Compare incoming and installed type revisions.
2. Quarantine deltas requiring unavailable newer model knowledge.
3. Implement current-range and facet validation.
4. Implement role-filling validation.
5. Implement cardinality and membership validation.
6. Integrate model update and migration policy.
7. Cache only compatibility results that are genuinely unconditional.

### Phase 5: Authoring and release tooling

1. Check lower and upper dependency bounds in CI.
2. Warn about unversioned published imports.
3. Require modeller declarations for behavioural compatibility or migrations
   that cannot be inferred.
4. Present dependency and migration consequences before publishing or
   installing updates.

## 14. Decisions

| Topic | Decision |
|---|---|
| Installation unit | Update complete model packages and dependency closures, not individual types. |
| Runtime model storage | Keep one active unversioned lookup representation per installed model. |
| Type provenance | Record the owning model version in which compiled type semantics last changed. |
| Delta provenance | Revision every explicit type reference, including the subject. |
| Signed location | Put provenance inside the signed delta payload. |
| Compact storage | Serialize revision and stable type identifier as one string. |
| Model environment | Reconstruct it from immutable model releases and dependency locks; do not repeat it in each delta. |
| Fingerprints | Optional integrity/diff aid, not the primary compatibility or ordering mechanism. |
| Newer incoming revision | Quarantine and stage package-level update resolution. |
| Older incoming revision | Optimistically accept under current semantics unless a known incompatibility is found. |
| Removed types | Retain tombstones in model history metadata. |
| Compatibility scope | Automate structural and authorization checks; do not claim general behavioural equivalence. |
| Duplicate identity | Derive `deltaId` from author and exact signed payload bytes. |
| Ordering identity | Keep operation key separate from exact delta identity. |
| Rollout | Capture irreversible provenance during reboot; implement deeper checking in later phases. |

## 15. Open Questions

The following choices remain to be specified before implementation or before
the phase that needs them. For phase 0, the `MAJOR.MINOR` model-version syntax
and the final-`@` revisioned-type separator are now fixed.

1. Which compiled fields count as semantic when deciding whether a type revision
   advances?
2. How are dependency constraints expressed in ARC source and represented in a
   `DomeinFile`?
3. Where are tombstones and transition classifications published: in the
   `DomeinFile`, a sidecar, or the model manifest?
4. Which model updates may be installed automatically and which require user
   consent?
5. How should unresolved deltas be retained, retried and communicated to peers?
6. Should the DeltaStore document id eventually include a `deltaId` suffix to
   preserve same-author equivocations?
7. Which processing dispositions are terminal and which should be retried after
   a model, data or predecessor update?
8. How is canonical serialization specified and tested across PureScript and
   JavaScript implementations?

## 16. Strategic Conclusion

The PDR cannot fully infer semantic compatibility between model versions.
Perspectives types participate in executable, interconnected behaviour whose
intent ultimately belongs to the modeller.

It can nevertheless make strong practical guarantees when it has trustworthy
provenance:

- know which type revision governed an authored delta;
- determine whether the receiver is older or newer;
- retrieve the correct immutable model package and dependency closure;
- prevent concrete structural and authorization errors;
- distinguish removed, incompatible, quarantined and duplicate deltas;
- preserve evidence for future, more capable compatibility policies.

The universe reboot should therefore establish the permanent signed data needed
for those guarantees now. Full compatibility checking can and should follow in
phases.
