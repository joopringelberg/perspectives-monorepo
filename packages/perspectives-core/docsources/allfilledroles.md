# All Filled Roles: Inherited Result Typing and Bulk Rebinding

`allfilledroles` implements the minimal useful form of
[issue #557](https://github.com/joopringelberg/perspectives-monorepo/issues/557):
select all roles directly filled by an input instance, without enumerating
possible concrete role or context types at compilation time.

## Design decision: a guaranteed inherited lower bound

The complete runtime type of a filled role contains the declared types of that
role and its actual fillers. If an instance of type B fills A, B therefore
contributes to A's complete runtime type. Every result of `B >> allfilledroles`
has at least the input type, even when new concrete filled-role types are
introduced by subsequently installed models.

That guarantee does **not** identify the concrete filled role or its containing
context. Returning an ordinary `RDOM B` would let `context` infer B's context
for an instance actually living somewhere else, and would let authorization
mistake B for the target role. Instead, the result domain is
`FROLE (ADT RoleInContext)`: the ADT is the inherited lower bound contributed
by the filler. Its context coordinates describe that type's provenance,
not the selected role's containing context.

`FROLE` is serialized in query descriptions as `fillerRole`, and its identifiers
are normalized through the same sidecar mapping as ordinary role domains.
Calculated thing/user roles and expression variables preserve the domain.
Calculated context roles are rejected because there is no guarantee that
results are external roles.

This first version supports identity, aggregation/existence checks, concrete
`roleType` reflection, and use as bulk `bind_` targets. `binding` on a result
returns its direct filler as an ordinary typed role with the retained lower
bound. That is the path for reading the filler's properties or navigating its
context. Direct property lookup, containing-context navigation, and another
`allfilledroles` step on `FROLE` are rejected rather than inventing missing
type information. This is not inference of a greatest common concrete type.

## Selection and reactive assumptions

The parser produces `AllFilledRoles`; the compiler emits
`DataTypeGetter AllFilledRolesF`, with non-functional and non-mandatory
cardinality. Both the unsafe compiler and dependency-path interpreter use
[`getAllFilledRoles`](../src/core/instances/instanceObjectGetters.purs).

The getter flattens the concrete context/role buckets in the filler's
`filledRoles`, deduplicates identifiers, and records
`AllFilledRolesAssumption fillerId`, including for an empty selection.
It does not use aliases, traverse filled-role chains, search every database
role, or discover inverse links held only by another installation. In
particular, a public filler without local inverse administration cannot
magically enumerate private roles elsewhere.

[`findFilledRoleRequests`](../src/core/dependencyTracking/dependencies.purs)
includes the register entry keyed by `allfilledroles` alongside the existing
typed-binder dependencies. A binding insertion/removal/replacement can
therefore invalidate a selection even when its concrete bucket did not exist
at the time of subscription. The interpreter prepends each selected role
to the dependency path; an empty result still contributes its assumption.

## Inversion without a compile-time target type

The inverse of `AllFilledRolesF` is `DirectFillerF`, not the type-filtered
recursive binder/filler machinery. Inverted-query storage removes that first
backward binding step and starts the remaining query at the filler instance,
as it does for ordinary binder queries.

The new `RTAllFilledKey` contains **only a filler role/context pair**. Its
serialized key begins with `allfilledroles:` to distinguish it from ordinary
four-coordinate keys and their cache entries. It is stored with
`queryType: "RTFillerKey"` and uses the existing `RTFillerKeyView`: both forms
represent backward queries that runtime applies to the filler. The key decoder
recognizes this variant under that query type, including during sidecar
normalization. No new database view is required.

On a binding change, runtime derives candidate `RTAllFilledKey` keys from the
filler's expanded complete runtime type, fetches those queries through
`getFillerQueries`, and retains the existing stored-domain applicability filter.
No unknown filled-role identity is inserted into an index key.

When an all-filled assumption is serialized for peers, every selected role
is handled with its **actual** role type and containing context; its creation,
linking and binding deltas are collected. Selection alone grants no permission
and does not change the perspective machinery's recipient decisions.

## Bulk replacement with per-target permission checks

For example, in a role-origin action whose context has functional roles
`OldPerson` and `NewPerson` filled by Persons instances:

```arc
bind_ context >> NewPerson >> binding to context >> OldPerson >> binding >> allfilledroles
```

[`statementCompiler.purs`](../src/core/assignment/statementCompiler.purs)
recognizes a `FROLE` target range and emits `BindAll_`. The replacement remains
functional and must specialise the inherited lower bound, checked using the
existing CNF normalization/type comparison helpers. Ordinary `bind_` remains
single-target and retains its existing compile-time authorization behavior.

Both context-origin and role-origin assignment compilers evaluate the entire
target selection before mutation and invoke the shared
[`bindAllWithPermission`](../src/core/assignment/compileAssignment.purs).
It:

1. Deduplicates targets and reads the transaction's authoring role.
2. Checks each target's actual role type through the existing
   `roleHasPerspectiveOnRoleWithVerb` rules for `Fill` or `CreateAndFill`.
3. Skips a target with `UnauthorizedForRole`, logging its identifier and the
   authorization diagnostic. Other failures are not permission-shaped skips.
4. Validates every permitted target's concrete filler restriction using
   `checkBinding`, and checks for a cycle through the replacement's filler chain.
5. Only after these checks schedules the permitted replacements through
   `setBindingWithMode FillWithProvidedType`.

The authorization helper checks role verbs available in any state, matching
the existing statement authorization convention; this change does not introduce
a separate state-sensitive permission policy. An invalid replacement or cycle
fails explicitly before replacements are scheduled. A missing replacement
is an error. An empty target set with a valid replacement is a no-op.

Using `FillWithProvidedType` is deliberate: replace with the supplied Persons
instance itself, not a deeper filler such as its PerspectivesUsers instance.
The normal destructive phase performs the replacements. Unauthorized targets
retain their original bindings.

## Regression evidence

- [Expression parser tests](../test/layertests/expressionParser.purs) recognize
  the keyword as a query step rather than a variable.
- [Property-anchor tests](../test/layertests/propertyAnchors.purs) check empty
  assumptions, selection across concrete buckets, deduplication, compiled and
  interpreted execution, the inherited result domain, bulk action compilation,
  unrestricted inverted-query key persistence, and retrieval through a database
  with only the existing filler-query view.
- [Binding replacement tests](../test/bindingReplacement.purs) check mixed
  permitted/denied targets across contexts, duplicate targets, application of
  scheduled replacements, and invalid-filler rejection before scheduling.

The original typed-binder design remains documented in
[Binder Lookup Through Role and Context Aspects](binder-aspect-lookup.md).
