# Binder Lookup Through Role and Context Aspects

This chapter documents the design and runtime mechanism introduced by
[PR #558](https://github.com/joopringelberg/perspectives-monorepo/pull/558).
The change makes type-constrained `binder` lookup complete across role and
context aspect specialisations, and keeps API dependency tracking and
peer synchronisation consistent with that lookup.

It is **not** an implementation of the unrestricted `allfilledroles` function
requested in [issue #557](https://github.com/joopringelberg/perspectives-monorepo/issues/557).
The patch adds no keyword and no inference of a common result type for arbitrary
filled roles. The existing compiled `FilledF` step still carries a requested
enumerated role type and context type.

That scope statement describes the typed-binder patch. The subsequent minimal
operator and its distinct inherited result type are documented in
[All Filled Roles: Inherited Result Typing and Bulk Rebinding](allfilledroles.md).

## 1. Semantic contract

`binding` traverses from a filled role to its filler. `binder` traverses in the
opposite direction, selecting roles filled by the current role according to a
requested role type and context type. When no context is specified, the compiler
uses the requested role's declared context.

An aspect contributes type structure; it does not create another role instance
or another filler link. Consequently, querying an aspect must return the
concrete instances that specialise it, not imaginary instances of the aspect.
Both dimensions matter: the concrete role must match the requested role, and
its containing context must match the requested context.

For the direct inverse-link lookup, an entry matches when:

- the requested context is the concrete context or one of its transitive aspects;
- the requested role is the concrete role or one of its transitive aspects.

All matching instance identifiers are returned without duplicates. This is
nominal aspect-closure matching, not a new general ADT/CNF subtype algorithm.
It operates on the filler instance's stored inverse links, not by searching all
role instances or all installed models. Recursive filled-role traversal remains
a separate concern.

## 2. Why single-valued aliases cannot implement this contract

The filler record stores inverse links as:

```text
filledRoles[concrete context type][concrete role type] = [filled instance IDs]
```

The old reader resolved a requested aspect through `contextAliases` and
`roleAliases`, then read one concrete bucket. Each alias maps to only one
concrete type. That representation cannot express several specialisations
sharing an aspect.

For example, suppose one filler fills these three roles:

| Instance | Concrete context | Concrete role |
|---|---|---|
| first | Meeting | First |
| second | Meeting | Second |
| third | ChildMeeting | Second |

Both `First` and `Second` have `Meeting$Attendees` as an aspect, and
`ChildMeeting` has `Meeting` as an aspect. A binder request for `Attendees` in
`Meeting` must return all three. A single alias for `Attendees` can select only
`First` or `Second`; a single alias for `Meeting` can likewise select only one
context type. Later alias writes can therefore hide still-existing bindings.

The design decision is to treat the concrete `filledRoles` buckets as the
authoritative inverse-link data and expand their types at read time. The
persisted shape is unchanged: no new aspect-indexed copy of every link is
introduced, and no instance-data migration is required by this change.
`addRol_gevuldeRollen` still maintains the alias maps; this reader no longer
depends on them. This is not a repository-wide removal of aliases.

## 3. Lookup mechanism

[`rol_gevuldeRol`](../src/core/instances/contextAndRole.purs) now:

1. Iterates the concrete context keys in `filledRoles`, skipping empty role maps.
2. Accepts an exact context match immediately; otherwise reads the concrete
   context's available aspect closure and checks for the requested context.
3. Within matching contexts, performs the same check for each concrete role key.
4. Concatenates the matching arrays and applies `nub`.
5. Returns the **requested** context and role alongside the instances.

Reading model definitions makes this function monadic (`MP`) rather than pure.
[`getFilledRoles`](../src/core/instances/instanceObjectGetters.purs) lifts that
operation into the query monad. Exact matches do not need a model lookup for
that dimension. Removing a binding continues to modify its concrete bucket,
so removing one result cannot erase the other specialisations.

The trade-off is more lookup work: the reader checks the concrete buckets on
this filler rather than following two aliases. There is no new persistent
index or aspect-closure cache in this patch. Any future optimisation must
preserve the complete set of matches rather than reinstate a single target.

## 4. API assumptions must describe the query, not a selected result

Correct lookup alone would leave reactive API requests stale.
`getFilledRoles` records:

```text
FilledRolesAssumption(filler ID, requested context type, requested role type)
```

The assumption is recorded even when a successfully loaded filler currently
has no matching roles. It describes the selection the query depends on, not
one concrete type encountered while evaluating it.

[`findFilledRoleRequests`](../src/core/dependencyTracking/dependencies.purs)
implements the complementary mutation-side operation. Given a changed binding's
concrete context and role types, it expands both available aspect closures and
looks up every context/role combination in that filler's assumption register.
It deduplicates the resulting correlation identifiers.

Thus, a request for `Attendees` in `Meeting` is discoverable when either
`First` or `Second` changes, including `Second` in `ChildMeeting`. A request
registered while the result was empty is discoverable after a matching binding
is added. Keeping the requested types in assumptions is essential: recording
one concrete alias would lose dependencies on other or future specialisations.

## 5. Inverted-query indexing must cover the same specialisations

API assumption lookup and stored inverted-query lookup are distinct mechanisms.
The latter identifies affected modelled calculations and perspectives.

[`runtimeIndexForFilledQueries'`](../src/arcParser/invertedQueryIndexing.purs)
generates `RTFilledKey` candidates from the product of:

- filler role/context pairs in the filled role's completely expanded declared
  filler restriction;
- the concrete filled role and its role aspects;
- the actual filled context and its context aspects.

Each key contains a filler-side pair and a filled-side pair.
`runtimeIndexForFillerQueries'` derives the corresponding `RTFillerKey` keys
by swapping those sides. Sharing the expansion prevents the two directions
from developing different aspect-matching rules.

These are candidate keys, not permission grants or proof that every remaining
query applies. In `usersWithPerspectiveOnRoleBinding'`, the existing
`invertedQueryHasRoleDomain` filter still checks that the actual runtime
role-in-context equals or specialises the stored backward query's domain,
using `equalsOrSpecialisesRoleInContext`.

Both filler-side and filled-side context types now come from their actual
containing context instances, not their role types' lexical declarations.
Otherwise, a role in a specialised context can be indexed or filtered using
the wrong context type.

The change does not alter the stored binding-query execution convention:
compile time removes the first backward binding step, and runtime starts
from the corresponding instance already supplied by the binding delta.
See [Role-Binding Query Analysis](role-binding-query-analysis.md) and
[Query Inversion](query-inversion.md) for that convention.

## 6. Synchronisation must retain each result's context

[`createDeltasFromAssumption`](../src/core/sync/collectAffectedContexts.purs)
previously used the first binder result's context when collecting creation
and linking deltas for the whole result set. Aspect-aware results may span
different context instances and types, so that assumption is invalid.

For a `FilledRolesAssumption`, the runtime now loads each returned role and
calls `magic` with that role's own context and a singleton role array. This
collects the context, external-role and role creation/linking deltas needed
for each result. The separate loop collecting binding deltas for every filled
role remains. The existing guard for a role unavailable through
`tryGetPerspectEntiteit` also remains.

This is a correctness change to dependency serialisation, not a new access
policy: it uses the recipients already determined by the perspective machinery.

## 7. Unavailable aspect branches: intent and limits

[`availableRoleAspectsClosure` and `availableContextAspectsClosure`](../src/core/typerepresentation/typeLevelObjectGetters.purs)
preserve traversable branches when another branch's model is unavailable,
for example after a model has been removed while inverse links remain.
The closure includes the starting type and follows available aspect edges.
If reading one node's direct aspects fails, that node contributes no further
edges; traversal of other branches can continue.

Lookup and API request matching use both tolerant helpers. Binding-query key
generation uses the tolerant **context** closure, but still uses the strict
role closure and filler-restriction expansion. Binding insertion also retains
its strict closures. This patch therefore does not promise tolerance for
missing models throughout the binding subsystem.

There is an additional diagnostic limitation: `availableAspects` currently
catches any exception from reading a node's aspects, returns an empty array
of outgoing edges, and emits no diagnostic. It does not distinguish a missing
model from another failure. This documents the implemented behaviour, not a
general recommendation to suppress model-loading errors.

## 8. Regression evidence and maintenance invariants

The test **"Binder lookup includes every role and context specialising the
requested aspects"** in
[`test/layertests/propertyAnchors.purs`](../test/layertests/propertyAnchors.purs)
constructs the three-instance example above. It checks:

- direct lookup and a compiled `FilledF` both return exactly the three IDs,
  even with empty alias maps and an unrelated stale inverse-link bucket;
- missing role/context aspect branches do not hide available matches;
- removing the second binding leaves the first and third results;
- an initially empty request's recorded assumption is found for changes to
  both concrete specialisations, including with an unavailable aspect;
- both runtime key directions include the base role/context aspect pair,
  and an unavailable context branch does not prevent that key.

The test exercises request selection and key generation directly; it is not
an end-to-end notification-delivery or multi-peer synchronisation test.

Future changes must preserve four connected invariants:

1. **Selection completeness:** no matching concrete bucket is hidden by aliases.
2. **Reactive completeness:** assumptions retain requested types, and concrete
   binding changes can find those assumptions through aspects.
3. **Indexing consistency:** both inverted-query directions cover aspects,
   use actual containing contexts, and retain domain filtering.
4. **Serialisation completeness:** every selected role is serialised with its
   own context, not a context borrowed from another result.

Changing only the reader can produce correct one-off results while leaving
subscriptions stale or peers unable to reconstruct the selected resources.
