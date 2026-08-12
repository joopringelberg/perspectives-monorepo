# Role-Binding Query Analysis

This note documents the runtime call tree that starts at `usersWithPerspectiveOnRoleBinding` in `Perspectives.CollectAffectedContexts`, with the `System$Invitation$Inviter` → `Invitee` case as its guiding example.

---

## Investigated case

In `packages/perspectives-core/src/model/perspectivesSysteem@6.3.arc` the `Invitation` context defines:

```arc
user Inviter (mandatory) filledBy (Persons + PerspectivesUsers)
  perspective on Invitee
    props (FirstName, LastName, UserWithHasKey$HasKey) verbs (Consult)
```

Relevant stable identifiers:

- `System$Invitation$Inviter` = `model://perspectives.domains#tiodn6tcyc$nkllrx0n2x$h65m7d31n8`
- `System$Invitation$Invitee` = `model://perspectives.domains#tiodn6tcyc$nkllrx0n2x$qzucadvv8p`

These identifiers are also present locally in `src/core/modelDependencies.purs`.

---

## Call tree

For a `RoleBindingDelta`, the runtime path is:

1. `usersWithPerspectiveOnRoleBinding`
   - expands the affected instances recursively:
     - not only the directly filled role;
     - not only the direct filler, but also its fillers.
2. `usersWithPerspectiveOnRoleBinding'`
   - computes two runtime key sets:
     - `RTFillerKey` queries (`runtimeIndexForFillerQueries'`)
     - `RTFilledKey` queries (`runtimeIndexForFilledQueries'`)
   - fetches the corresponding stored inverted queries
   - filters them with `invertedQueryHasRoleDomain`
   - evaluates the surviving queries with `handleBackwardQuery`
   - optionally continues with `runForwardsComputation`
3. `handleBackwardQuery`
   - routes state-only queries into `invertedQueryResults`
   - routes perspective-bearing queries into `usersWithAnActivePerspective`
4. `usersWithAnActivePerspective`
   - interprets the compiled backward part
   - derives either:
     - contexts from which user roles must be collected; or
     - role instances whose states determine whether the perspective is active
5. `runForwardsComputation`
   - serialises dependencies and property values for the users found by the backward phase.

---

## Important invariant

For binding-related inverted queries, compile time removes the first backward step before storing the query:

- `RTFillerKey`: the original first backward step was `filler`
- `RTFilledKey`: the original first backward step was `filled`

As a consequence, runtime must **not** replay that first binding step. It must start from the role instances already present in the `RoleBindingDelta`:

- `handleBackwardQuery filler iq` for `RTFillerKey`
- `handleBackwardQuery filled iq` for `RTFilledKey`

When `runForwards` is enabled, the runtime then continues from the *other* side of the binding:

- for `RTFillerKey`: run forwards from `filled`
- for `RTFilledKey`: run forwards from `filler`

This is exactly what `usersWithPerspectiveOnRoleBinding'` currently does.

---

## What the runtime filter does

After fetching by runtime key, `usersWithPerspectiveOnRoleBinding'` still applies:

```purescript
filterA (invertedQueryHasRoleDomain contextType roleType)
```

This is not redundant. The runtime keys are intentionally broad enough to cover aspect specialisation and filler restrictions. The extra filter checks whether the **remaining stored backward query** is still applicable to the concrete role-in-context that triggered the delta.

The check uses `equalsOrSpecialisesRoleInContext`, so a concrete runtime role may specialise the stored domain.

---

## Conclusion from the code inspection

For the `Inviter`/`Invitee` case, the runtime code in `Perspectives.CollectAffectedContexts` is internally consistent with:

- the storage rules documented in `query-inversion.md`;
- the runtime key generation in `arcParser/invertedQueryIndexing.purs`;
- the compile-time comments in `arcParser/storeInvertedQueries.purs`.

In other words: the binding call tree does **not** appear to contain an obvious mismatch such as:

- starting the backward query from the wrong side of the binding;
- replaying a compile-time-removed first step;
- running the forward query from the wrong instance.

If this case still yields no users at runtime, the next most likely causes are:

1. the fetched inverted queries are incomplete or incorrect for this perspective;
2. `handleBackwardQuery` returns no contexts or roles for the stored backward query;
3. state filtering removes all candidate users after the backward phase.

---

## Practical debugging checklist

When debugging a concrete binding case, inspect these points in order:

1. Which `RTFillerKey` / `RTFilledKey` keys are generated?
2. Which stored queries are fetched for those keys?
3. Does `invertedQueryHasRoleDomain` keep or reject each query?
4. What does `handleBackwardQuery` return for each surviving query?
5. If contexts are returned, which user instances are collected from them?
6. If users are found, does `runForwardsComputation` serialise the expected dependency paths and properties?

If steps 1-3 already fail, the problem is more likely in inverted-query compilation or persistence than in the runtime application code.
