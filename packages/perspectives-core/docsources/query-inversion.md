# Query Inversion

This document describes the **query inversion** mechanism of the Perspectives Distributed Runtime (PDR). Query inversion is a compile-time transformation that allows the runtime to efficiently answer the question: *"Given that a particular data item (role instance, context, property value, role binding) has changed, which queries are affected, and which user roles need to be informed?"*

> **Source files (compile time):**
> - `packages/perspectives-core/src/core/queries/queryWithAKink.purs` — the `Kinked` module; the core inversion algorithm
> - `packages/perspectives-core/src/core/queries/invertFunctionDescription.purs` — the `Inversion` module; maps individual query steps to their inverses
> - `packages/perspectives-core/src/arcParser/setInvertedQueries.purs` — entry point called from PhaseThree
> - `packages/perspectives-core/src/arcParser/storeInvertedQueries.purs` — stores inverted queries indexed by type keys
> - `packages/perspectives-core/src/arcParser/invertedQueryIndexing.purs` — compute runtime and compile-time keys
> - `packages/perspectives-core/src/arcParser/arcParserPhaseThree.purs` — top-level call site (`invertPerspectiveObjects`)
>
> **Source files (data structures):**
> - `packages/perspectives-core/src/core/queries/invertedQuery.purs` — `InvertedQuery`, `QueryWithAKink`, `RelevantProperties`
> - `packages/perspectives-core/src/core/typePersistence/invertedQueryKey.purs` — `RunTimeInvertedQueryKey`
> - `packages/perspectives-core/src/core/typePersistence/storableInvertedQuery.purs` — persistence of inverted queries
>
> **Source files (runtime):**
> - `packages/perspectives-core/src/core/sync/collectAffectedContexts.purs` — runtime application of inverted queries
> - `packages/perspectives-core/src/core/sync/invertedQueryResult.purs` — `InvertedQueryResult` type

---

## Background

ARC models specify *perspectives*: a user role has a perspective on some other role type, which may be reached by a multi-step query. For example:

```arc
-- Simplified ARC notation
user Requester
  perspective on Approver >> binding >> Profile$Name
```

This perspective object query says: "starting from this context, follow the role `Approver`, then follow the `binding` (filler) of that role, then get the property `Profile$Name`." (*the example is simplified, because perspectives are on roles only; properties that fall in the perspective are listed seperately. However, for this explanation the distinction is irrelevant*).

When the PDR executes a transaction that modifies a data item visited by such a query — for example, when the value of `Profile$Name` changes — it must determine:

1. **Which role or context instances are affected** (so that state conditions can be re-evaluated), and
2. **Which user role instances need to receive synchronisation deltas** (so that they see the change in their perspective).

With thousands of contexts and queries, doing a forward scan ("run every query and check if the changed item appears in the result") would be too slow. Instead, the PDR pre-computes **inverted versions** of all queries at compile time, and at runtime applies only the relevant inverted queries starting from the changed item.

---

## Part 1 — Compile Time

### 1.1 Entry Point: `invertPerspectiveObjects`

The top-level compile-time function is `invertPerspectiveObjects` in `arcParserPhaseThree.purs`. It is called during **Phase 3** of the ARC parser, after all types have been resolved and perspectives have been constructed.

```purescript
-- arcParserPhaseThree.purs
invertPerspectiveObjects :: PhaseThree Unit
invertPerspectiveObjects = do
  df@{ id } <- lift $ State.gets _.dfr
  withDomeinFile id (DomeinFile df) (addInvertedQueries' df)
  where
  addInvertedQueries' { enumeratedRoles, calculatedRoles } = do
    traverse_ perspectivesInEnumeratedRole enumeratedRoles
    traverse_ perspectivesInCalculatedRole calculatedRoles
```

It iterates over every role in the DomeinFile — both Enumerated and Calculated — and calls `addInvertedQueriesForPerspectiveObject` for each perspective on each role. **Public** Calculated roles are skipped (they are handled by a different mechanism).

The key call site is:

```purescript
addInvertedQueriesForPerspectiveObject roleType p@(Perspective { object, ... }) = do
  runReaderT
    (setInvertedQueries [ roleType ] sPerProp roleStates object selfOnly authorOnly)
    (createModificationSummary p)
```

Note that `users` is set to `[ roleType ]` — the role type (Enumerated or Calculated) that owns the perspective. For Calculated user roles, this will be a `CR roleName` value.

### 1.2 Inverting a Perspective Object: `setInvertedQueries`

`setInvertedQueries` (`setInvertedQueries.purs`) receives:
- `users` — array of `RoleType` values (the user role that owns the perspective)
- `statesPerProperty` — states in which each property is visible
- `roleStates` — states in which the perspective itself is active
- `qfd` — the `QueryFunctionDescription` of the perspective object

It calls `invert qfd` (from `queryWithAKink.purs`) to produce all possible `QueryWithAKink` values, then for each one calls `storeInvertedQuery`.

### 1.3 The Core Algorithm: `invert` (module `Perspectives.Query.Kinked`)

The function `invert :: QueryFunctionDescription -> PhaseThree (Array QueryWithAKink)` is the heart of the system.

#### What is a Kink?

Given a query `q = s1 >> s2 >> s3`, a **kink** is a specific position in the query where it is "bent". At each kink position, the query is split into:
- A **backwards part**: the steps from that position back to the start, each individually inverted and in reversed order.
- A **forwards part**: the remaining steps from that position to the end (unchanged).

`invert` produces one `QueryWithAKink` for each kinkable position in the query.

#### `QueryWithAKink` Data Type

```purescript
-- Defined in Perspectives.InvertedQuery
data QueryWithAKink = ZQ (Maybe QueryFunctionDescription) (Maybe QueryFunctionDescription)
--                       ^ backwards (inverted)            ^ forwards (original direction)
```

For the query `s1 >> s2 >> s3`, `invert` produces (conceptually):

| Backwards (inverted) | Forwards (original) |
|---|---|
| `^s1`                | `s2 >> s3`          |
| `^s2 << ^s1`         | `s3`                |
| `^s3 << ^s2 << ^s1`  | (Nothing)           |

where `^sN` is the inverse of step `sN`, and `<<` represents reversed composition (`^s2 << ^s1` means "first apply `^s2`, then apply `^s1`").

The final entry (backwards goes all the way to the start, forwards is Nothing) is called a **complete inversion**.

#### Internal Representation: `QueryWithAKink_`

Internally, during inversion, the backwards part is represented as an array of separate steps `ZQ_ (Array QueryFunctionDescription) (Maybe QueryFunctionDescription)`. The `h` function in `invert` folds these into a right-associative composition.

#### Inverting Individual Steps: `invertFunction` (module `Perspectives.Query.Inversion`)

The function `invertFunction :: Domain -> QueryFunction -> Range -> PhaseThree (Maybe QueryFunction)` maps each query step to its inverse:

| Original step | Inverse step | Notes |
|---|---|---|
| `context` | `role <type>` | or `GetRoleInstancesForContextFromDatabaseF` for relational roles |
| `filler` (filledBy) | `fills <type> <context>` | the domain is the type of the filled role |
| `fills` (FilledF) | `filler` | the filled-to-filler direction |
| `externalRole` | `context` | |
| `propertyGetter p` | `Value2Role p` | |
| `identity` | (nothing) | identity steps are ignored |
| aggregate functions (`sum`, `min`, etc.) | `Value2Role` | to skip aggregate steps |
| `Constant` | (nothing) | constants cannot be inverted |

For `Calculated Role` getters and `Calculated Property` getters, `invertFunction` recursively inverts the underlying calculation.

#### Handling Composition, Union, Intersection, Filters, Variables

The `invert_` function handles recursive cases:

- **Composition** (`s1 >> s2`): kinks are produced both at `s1` and at `s2`, with appropriate combinations. The `comprehend` function generates all valid pairs of (backwards steps from the right sub-query) cross (backwards steps from the left sub-query), concatenating them in reversed order.
- **Union / Intersection** (`q1 | q2`, `q1 & q2`): inverted as the union of inversions of `q1` and `q2`.
- **Filter** (`filter source with criterium`): the criterium is inverted and a `FilterF` step is
  appended. For **regular perspective queries** the filter is later *removed* when storing the
  inverted query (see §1.5), because at runtime we want to detect the change even when the filter
  now evaluates to false (the user may have just *lost* visibility of an item).
  For **Calculated User role detection queries** (see §1.9) the filter is *kept* in both the
  backwards and forwards slots of the stored `QueryWithAKink` — see §1.9 for the full rationale.
- **Let\* (WithFrame / BindVariable)**: variable bindings are stored and substituted when the variable is later referenced.
- **Calculated Role** (`RolGetter (CR r)`): the calculation of the role is retrieved and inverted recursively.

### 1.4 Storing Inverted Queries: `storeInvertedQuery` (module `StoreInvertedQueries`)

`storeInvertedQuery` receives one `QueryWithAKink` and decides where to store it by examining the **first step of the backwards part**:

| First backwards step | Stored as | Triggered by |
|---|---|---|
| `Value2Role pt` | `RTPropertyKey` | Property value change for property `pt` on a role |
| `FilledF enr ctxt` | `RTFilledKey` | A filler is added to role `enr` in context `ctxt` (RoleBindingDelta) |
| `DataTypeGetter FillerF` | `RTFillerKey` | The filled role of a binding changes |
| `RolGetter (ENR role)` | `RTRoleKey` | A role instance of type `role` is added to/removed from a context |
| `DataTypeGetter ContextF` | `RTContextKey` | The context of a role instance changes (used when the query traverses `context`) |
| `GetRoleInstancesForContextFromDatabaseF et` | `RTRoleKey` | Similar to `RolGetter ENR` but for relational roles |

For `FilledF` and `FillerF` steps, the first backwards step is **dropped** from the stored inverted query (the `removeFirstBackwardsStep` helper). This is because at runtime the delta already provides the filled or filler role instance directly, so the step that would navigate to it is redundant.

For `RolGetter (ENR role)` steps, the first backwards step is also dropped, and a compensating `context` step is prepended to the forwards part.

Each stored inverted query is a `StorableInvertedQuery`:

```purescript
type StorableInvertedQuery =
  { queryType :: String          -- "RTPropertyKey", "RTRoleKey", etc.
  , keys :: Array String         -- serialized RunTimeInvertedQueryKey values
  , query :: InvertedQuery       -- the query itself (not yet compiled)
  , model :: ModelUri Readable   -- the model that contributed this query
  }
```

### 1.5 The `InvertedQuery` Record

```purescript
newtype InvertedQuery = InvertedQuery
  { description       :: QueryWithAKink
  , backwardsCompiled :: Maybe HiddenFunction   -- compiled at runtime, not at compile time
  , forwardsCompiled  :: Maybe HiddenFunction   -- compiled at runtime, not at compile time
  , users             :: Array RoleType          -- role types that have this perspective
  , modifies          :: Boolean                 -- true if the user can modify this element
  , states            :: Array StateIdentifier   -- states in which this perspective is active
  , statesPerProperty :: EncodableMap PropertyType (Array StateIdentifier)
  , selfOnly          :: Boolean                 -- personal/private perspective
  , authorOnly        :: Boolean                 -- only the author sees this
  }
```

Key observations:
- `users` contains **role types** (not instances). A `CR rName` entry means a Calculated user role has this perspective.
- `backwardsCompiled` and `forwardsCompiled` are `Nothing` when stored; they are compiled on first use at runtime (see §2.4).
- `users` is an **empty array** for state queries (queries that exist only to re-evaluate state conditions, not to synchronise peers).

### 1.6 Runtime Keys: `RunTimeInvertedQueryKey`

```purescript
data RunTimeInvertedQueryKey
  = RTPropertyKey { property :: EnumeratedPropertyType, role :: EnumeratedRoleType }
  | RTRoleKey     { context_origin :: ContextType, role_destination :: EnumeratedRoleType }
  | RTContextKey  { role_origin :: EnumeratedRoleType, context_destination :: ContextType }
  | RTFillerKey   { filledRole_origin, filledContext_origin,
                    fillerRole_destination, fillerContext_destination :: ... }
  | RTFilledKey   { fillerRole_origin, fillerContext_origin,
                    filledRole_destination, filledContext_destination :: ... }
```

Keys are serialized to strings for use as CouchDB view keys. The five CouchDB views (`RTPropertyKeyView`, `RTRoleKeyView`, `RTContextKeyView`, `RTFillerKeyView`, `RTFilledKeyView`) each index one category.

### 1.7 Persistence

At install time, inverted queries are saved into a dedicated CouchDB database (`invertedQueryDatabaseName`) via `saveInvertedQueries`. At runtime they are fetched by key and cached in an LRU cache (`queryCache`) to avoid repeated CouchDB lookups.

When a model is removed, `removeInvertedQueriesContributedByModel` deletes all its inverted queries from the local database.

---

## Part 2 — Runtime

The runtime module is `Perspectives.CollectAffectedContexts` (`collectAffectedContexts.purs`).

### 2.1 Two Purposes of Inverted Queries

Inverted queries serve two distinct purposes:

1. **State evaluation**: Determine which context or role instances have entered or left a state, so that automatic actions, notifications and state-dependent perspectives can be re-evaluated.
2. **Synchronisation**: Determine which user role instances have a perspective on the changed item, so that signed deltas describing the change can be sent to them.

The distinction is made by whether `users` is empty (state query) or not (perspective query).

### 2.2 Entry Points by Delta Type

Each type of data mutation triggers a corresponding lookup:

| Delta type | Entry function | Key type used |
|---|---|---|
| Role instance added or removed | `usersWithPerspectiveOnRoleInstance` | `RTContextKey`, `RTRoleKey` |
| Role binding set or replaced | `usersWithPerspectiveOnRoleBinding` | `RTFillerKey`, `RTFilledKey` |
| Property value changed | `aisInPropertyDelta` | `RTPropertyKey` |

These entry points are called from the assignment module (e.g., from `setFirstBinding`, `deleteRole`, `setProperty`).

### 2.3 Key Computation at Runtime

Before looking up inverted queries, the runtime must compute the appropriate runtime keys from the actual types of the instances involved. Because a role type may have multiple Aspect types, the runtime constructs multiple candidate keys.

- `runtimeIndexForContextQueries` / `runTimeIndexForRoleQueries` — for role-instance and context mutations, using the role type and all its Aspect role types paired with their lexical contexts.
- `runtimeIndexForFillerQueries` / `runtimeIndexForFilledQueries` — for binding mutations, using the complete expansion of the filler restriction type of the filled role.
- `runtimeIndexForPropertyQueries` — for property mutations, handling the case where the property is an Aspect property with possible aliasing.

### 2.4 Compilation on First Use

When an `InvertedQuery` is retrieved from the database, its `backwardsCompiled` and `forwardsCompiled` fields are `Nothing`. The function `compileBoth` lazily compiles them:

```purescript
compileBoth :: InvertedQuery -> MP InvertedQuery
compileBoth ac@(InvertedQuery iqr@{ description, backwardsCompiled }) = case backwardsCompiled of
  Just c -> pure ac   -- already compiled
  Nothing -> do
    backwards' <- traverse getHiddenFunction (backwards description)
    forwards'  <- traverse getHiddenFunction (forwards description)
    pure $ InvertedQuery iqr { backwardsCompiled = backwards', forwardsCompiled = forwards' }
```

The compiled functions are also stored in the LRU cache so that subsequent lookups with the same key return already-compiled queries.

### 2.5 `handleBackwardQuery`

The function `handleBackwardQuery :: RoleInstance -> InvertedQuery -> MonadPerspectivesTransaction (Array ContextWithUsers)` is the central workhorse.

Given a starting role instance (the one affected by the mutation) and an inverted query, it:

1. **Checks whether this is a state query** (empty `users`). If so:
   - Runs the backwards compiled function on the role instance to get context or role instances.
   - Creates a `ContextStateQuery` or `RoleStateQuery` result and adds it to the current transaction. These results are processed later in the transaction lifecycle to trigger state transitions.
   - Returns an empty array (no synchronisation users).

2. **For perspective queries** (non-empty `users`), calls `usersWithAnActivePerspective`.

### 2.6 `usersWithAnActivePerspective`

This function runs the compiled backwards query to find the context instances (or role instances, for implicit perspectives) that hold the user roles with a perspective. It then checks state conditions:

- **Context state** (`Cnt`): the context must be in the required state.
- **Subject state** (`Srole`): the user role instance must be in the required state.
- **Object state** (`Orole`): the perspective object (reachable via the forwards part) must be in the required state.

For each satisfied condition, it collects the user instances via `getRoleInstances userType`. **For Calculated user roles**, `getRoleInstances` evaluates the Calculated role's query to produce the actual user instances.

The function returns an array of `(ContextInstance, Array RoleInstance)` pairs.

### 2.7 `runForwardsComputation`

After finding the affected users via the backwards query, `runForwardsComputation` runs the **forwards part** of the `QueryWithAKink` (the original perspective object path from the kink position) using the query interpreter. It collects all dependency paths. Each dependency in a path represents a data item that was visited. These dependencies are then serialised into deltas via `serialiseDependencies`.

If there is no forwards part (`Nothing`), the starting role instance itself is the perspective object, and its properties are computed directly.

### 2.8 `magic` — Serialising Context Creation Deltas

When a role instance needs to be communicated to a peer, the peer must first be able to create the context that contains it. The function `magic` ensures this by adding the creation deltas for the context and its external role to the transaction, before the role deltas:

```purescript
magic :: ContextInstance -> Array RoleInstance -> EnumeratedRoleType -> Array RoleInstance -> MonadPerspectivesTransaction Unit
magic ctxt roleInstances rtype users = do
  (PerspectContext { buitenRol }) <- lift $ getPerspectContext ctxt
  contextDeltas  <- lift $ getDeltasForResource (unwrap ctxt)
  for_ contextDeltas \(DeltaStoreRecord { signedDelta }) ->
    addDelta $ DeltaInTransaction { users, delta: signedDelta }
  extRoleDeltas  <- lift $ getDeltasForResource (unwrap buitenRol)
  for_ extRoleDeltas \(DeltaStoreRecord { signedDelta }) ->
    addDelta $ DeltaInTransaction { users, delta: signedDelta }
  for_ roleInstances \roleInstance -> do
    roleDeltas <- lift $ getDeltasForResource (unwrap roleInstance)
    for_ roleDeltas \(DeltaStoreRecord { signedDelta }) ->
      addDelta $ DeltaInTransaction { users, delta: signedDelta }
```

`addDelta` deduplicates at the transaction level, so deltas are never sent twice to the same user within the same transaction.

### 2.9 `InvertedQueryResult`

The `InvertedQueryResult` type carries the results of state queries through the transaction:

```purescript
data InvertedQueryResult
  = ContextStateQuery (Array ContextInstance)
  | RoleStateQuery    (Array RoleInstance)
```

These are accumulated in the `Transaction` record and evaluated at the end of the transaction to determine which contexts and roles have changed state.

---

## Summary: Data Flow

```
ARC model compilation (Phase 3)
  └── invertPerspectiveObjects
        └── for each EnumeratedRole / CalculatedRole perspective:
              └── setInvertedQueries
                    └── invert (Kinked module)       -- produces Array QueryWithAKink
                          └── invertFunction          -- inverts individual steps
                    └── storeInvertedQuery            -- examines first backwards step
                          └── addStorableInvertedQuery -- stored in CouchDB (storedQueries.json)

Runtime (on data mutation)
  └── usersWithPerspectiveOnRoleInstance / usersWithPerspectiveOnRoleBinding / aisInPropertyDelta
        └── compute runtime keys (runtimeIndexFor*)
        └── getContextQueries / getRoleQueries / getFillerQueries / ...  (from CouchDB + LRU cache)
        └── compileBoth                               -- lazy compilation of backwards/forwards
        └── handleBackwardQuery
              ├── [state query] → createContextStateQuery / createRoleStateQuery
              │     → adds ContextStateQuery / RoleStateQuery to Transaction
              └── [perspective query] → usersWithAnActivePerspective
                    └── runs backwards compiled query → context/role instances
                    └── checks state conditions
                    └── getRoleInstances userType     -- for Calculated roles: evaluates the query
                    └── returns (context, users) pairs
        └── runForwardsComputation
              └── runs forwards compiled query via Query Interpreter
              └── serialiseDependencies → addDelta    -- creates deltas for peers
        └── magic                                    -- adds context creation deltas
```

---

## Note on Calculated User Roles and Inverted Queries

`invertPerspectiveObjects` processes Calculated user role perspectives in `perspectivesInCalculatedRole`. Consequently, the perspective object query of a Calculated user role **is** inverted. The resulting `InvertedQuery` stores the **Calculated role type** (`CR rName`) in its `users` field, not the resolved Enumerated type.

At runtime, when `usersWithAnActivePerspective` computes user instances, it calls `getRoleInstances (CR rName)`, which evaluates the Calculated role's query to produce the actual user role instances.

**Important limitation (partially resolved)**: The inverted query is stored on the type visited by
the perspective object query. It is indexed so that mutations to that type trigger the backwards
query, which navigates back to a context where the Calculated user role is defined. In that
context, the Calculated role is evaluated to find the actual user instances.

When the Calculated role's **query itself** traverses a role binding (filler/filled step), a
change to that binding can introduce entirely new user instances whose context has never been
serialised. The `invertCalculatedUsers` pass (§1.9) addresses this case.

---

## §1.9 `invertCalculatedUsers` — Detecting New Calculated User Instances

### Purpose

A Calculated User Role whose definition traverses a role binding (e.g. via `filler`, `fills`,
or an external database query such as `callExternal cdb:RoleInstances(...)`) can gain **new
instances** when a binding changes.  The regular `invertPerspectiveObjects` pass is not
sufficient for this: it inverts the *perspective object* query (what the user sees), not the
*user role calculation* itself.  So when a new role binding appears, there is no mechanism in
`invertPerspectiveObjects` to notice that a new Calculated User instance has come into
existence and that its context must be serialised for it.

`invertCalculatedUsers` fills this gap.  For every Calculated User Role it calls `invert` on
the role's own calculation query, and stores each resulting `QueryWithAKink` via
`storeCalculatedUserInvertedQuery`, which sets `calculatedUserRoleType = Just (CR id)` and
`users = []`.

### Special Semantics for Filter-based Queries

The running example from the issue is the `Contacts` role of the System model:

```arc
filter (callExternal cdb:RoleInstances("Persons") returns SocialEnvironment$Persons)
  with (exists PublicKey) and (not this == me)
```

`invert` produces (for the `exists PublicKey` criterium, assuming PublicKey is a property of
the filler `PerspectivesUsers`):

```
ZQ (Just (FilledF >> filter >> ExternalCoreContextGetter))
   (Just PropertyGetter_pk)
```

#### The Original Bug

Under the old storage code `storeInvertedQuery'` would drop the filter and store:

```
RTFilledKey  description = ZQ (Just ExternalCoreContextGetter)
                                (Just PropertyGetter_pk)
```

At runtime `handleNewCalculatedUsersForBinding filled filler iq` was called:
- backward `ExternalCoreContextGetter` on `filled` (Persons) → context **✓**
- forward `PropertyGetter_pk` on `fwStart = filler` (PerspectivesUser) → property **values** **✗**

The property value string was then passed as a `RoleInstance` to `serialisedAsDeltasFor_`,
causing runtime errors.

#### The Fix

For `mCalcUserRoleType = Just _` in the filter pattern case, `storeInvertedQuery'` now
produces the description:

```
ZQ (Just (filter >> source)) (Just filter)
```

i.e. both the backwards and forwards slots carry the `FilterF` expression.

After `setPathForStep FilledF` removes the `FilledF` first step, the stored description is:

```
RTFilledKey  description = ZQ (Just ExternalCoreContextGetter)
                                (Just filter)        ← FilterF, not PropertyGetter_pk
```

At runtime, `handleNewCalculatedUsersForBinding` now detects `forwardStartsWithFilter iq` and
uses `bwStart` (the filled Persons role) as the start for **both** the backwards and the
forwards query:

- backward `ExternalCoreContextGetter` on `filled` (Persons) → context **✓**
- forward `filter` on `filled` (Persons) → `[Persons]` if the filter passes **✓**

The result is a `calcUserInstances = [Persons role instance]` that is correctly treated as the
new Calculated User instance.

#### Property-Change Trigger

For scenario A (PublicKey directly on the Persons role), a corresponding `RTPropertyKey`
inverted query is also stored with:

```
description = ZQ (Just (filter >> ExternalCoreContextGetter)) (Just filter)
```

When PublicKey changes on a Persons role, `aisInPropertyDelta` detects
`isCalculatedUserQuery iq && forwardStartsWithFilter iq` and calls
`handleNewCalculatedUsersForBinding propertyBearingInstance propertyBearingInstance iq`,
applying both backward (`filter >> ECG`) and forward (`filter`) to the same Persons role
instance.

For scenario B (PublicKey on the filler PerspectivesUser), no `RTPropertyKey` is stored for
the Calculated User query because the first backwards step is `FilledF` (not `Value2Role`).
The property-change trigger for scenario B is a remaining limitation: only the role-binding
change (RTFilledKey) is detected.

#### The `not this == me` Criterion

The `not this == me` condition inverts to nothing useful (the `this` variable cannot be
inverted in a way that produces a meaningful key), so it does not generate a stored inverted
query.  At runtime the filter checks both criteria (`exists PublicKey` **and** `not this == me`)
before returning the role as a new Calculated User instance.  This is correct: the filter
criterium is evaluated in full when the forward `filter` step is applied.

### Runtime Flow

1. A new Persons role gets a PerspectivesUser filler (RTFilledKey fires).
2. `usersWithPerspectiveOnRoleBinding'` separates `isCalculatedUserQuery` queries from regular
   ones.
3. For each `calcUserFilledCalculation`, `handleNewCalculatedUsersForBinding filled filler iq`
   is called with `bwStart = filled`, `fwStart = filler`.
4. Because `forwardStartsWithFilter iq`, `forwardStart = bwStart = filled`.
5. Backward: `ExternalCoreContextGetter` on `filled` → `SocialEnvironment` context.
6. Forward: `filter` on `filled` → `[filled]` if both criterium conditions pass.
7. Context is serialised for the new user (the Persons role instance).

When a context defines two user roles U1 and U2 that both have a perspective on the same Calculated thing role O, `invertPerspectiveObjects` processes each user role's perspective independently:

1. For U1's perspective on O: `invert` runs on O's query, producing N `QueryWithAKink` records. Each is stored via `addStorableInvertedQuery` with `users = [U1]`.
2. For U2's perspective on O: `invert` runs again on the **same** query, producing another N `QueryWithAKink` records. Each is stored with `users = [U2]`.

The result is **2 × N** stored `InvertedQuery` documents — N with `users = [U1]`, N with `users = [U2]` — all sharing identical `description` (the same `QueryWithAKink`). At runtime, when an instance of O changes, both sets of records are fetched and the backwards computation runs twice over the same data.

This is **semantically correct but redundant**. A single record with `users = [U1, U2]` and a shared `description` would avoid the duplicated backwards traversal and halve the CouchDB storage for the inverted queries of O.

### Why a Simple Merge Is Not Straightforward

A naïve merge on equal `description` values is unsafe because the other fields of `InvertedQuery` can differ between U1 and U2:

- **`roleStates` / `statesPerProperty`**: U1 and U2 may have their perspective on O active in different subject states or context states. If we merge the records, a mutation would incorrectly notify both users regardless of which states are active.
- **`statesPerProperty`**: U1 may see a subset of O's properties that U2 does not. If we use a merged record, a property-value delta might reach U2 even though U2's perspective does not include that property.
- **`selfOnly` / `authorOnly`**: These flags further constrain which user instances are notified.

### Proposed (Deferred) Optimisation

A correct optimisation would separate the `InvertedQuery` record into two parts:

1. **Shared part** — `description`, `backwardsCompiled`, `forwardsCompiled`: the actual query execution logic, which is identical for all users when the perspective object is the same.
2. **Per-user part** — `users`, `roleStates`, `statesPerProperty`, `selfOnly`, `authorOnly`: the per-perspective constraints.

The shared part could be referenced by multiple per-user records (or stored once with an array of per-user constraint records). This would allow the backwards traversal to run only once, with the results dispatched to each user according to their individual constraints.

This is a **complex refactor** of the `InvertedQuery` data structure and the compile-time storage and runtime retrieval code. It is deferred to a future optimisation pass.

---

## Note on Redundancy Introduced by `invertCalculatedUsers` (Future Optimisation)

Phase 3 now runs two separate query-inversion passes:

1. **`invertPerspectiveObjects`** — inverts the perspective-object query for every user role that has a perspective. If a Calculated User role CU is a perspective object for some other user U, then CU's query is inverted with `users = [U]`.

2. **`invertCalculatedUsers`** — inverts the _calculation_ of every Calculated User role, tagging the result with `calculatedUserRoleType = Just (CR id)` and `users = []`. This is used at runtime to detect new Calculated User instances when a binding changes.

When a Calculated User role CU has _both_ (a) another user U with a perspective on CU, _and_ (b) a binding-traversing calculation, two sets of `InvertedQuery` records are generated for (some of) the same `QueryWithAKink` values:
- From `invertPerspectiveObjects`: stored with `users = [U]`, `calculatedUserRoleType = Nothing`
- From `invertCalculatedUsers`: stored with `users = []`, `calculatedUserRoleType = Just (CR CU_id)`

These two records have the same `description` but serve different purposes. The first is for synchronising U; the second is for detecting new CU instances. They cannot be merged because they carry different metadata.

A future optimisation could detect this overlap and avoid the redundant backwards traversal by sharing the backwards query execution while dispatching to both purposes. This is deferred alongside the broader per-user merging optimisation described above.

---

## §3 Correctness Analysis: Compile-time / Runtime Alignment

This section formally documents the correctness of the five inverted-query categories: key alignment between compile time and runtime, and domain/range ("kind") compatibility between what is stored and what is applied at runtime.

### §3.1 The `qfd` Invariant in `setPathForStep`

A frequently misread aspect of `storeInvertedQuery` / `setPathForStep` is what `qfd` represents. In
`setPathForStep qfd qwk …`, the first argument `qfd` is **the first step of the backwards path** of
`qwk` — which is the **inverse of the original query step at the kink point**. It is _not_ the
original kinked step itself.

Concretely, if the original query had a `ContextF` step at the kink, then `qfd` is an `RolGetter`
step (the inverse of `ContextF`). If the original query had a `RolGetter role` step at the kink,
then `qfd` is a `ContextF` step (the inverse of `RolGetter`).

This identity matters when reading the domain / range of `qfd`:

| Original kinked step | `qfd` (first backward step) | `domain qfd` | `range qfd` |
|---|---|---|---|
| `RolGetter (ENR role)` in context `ctx` | `ContextF` | `RDOM role` | `CDOM ctx` |
| `ContextF` on role `role` in `ctx` | `RolGetter role` | `CDOM ctx` | `RDOM role` |
| `FillerF` on filled `fld` | `FilledF fld ctx` | `RDOM filler` | `RDOM filled` |
| `FilledF` on filler `flr` | `FillerF` | `RDOM filled` | `RDOM filler` |
| property getter `p` on role `role` | `Value2Role p` | `VDOM p` | `RDOM role` |

### §3.2 RTPropertyKey — property value changes

**Compile-time key** (`typeLevelKeyForPropertyQueries`):
`qfd` is a `Value2Role pt` step (domain `VDOM pt`, range `RDOM roleType`).
The key is `RTPropertyKey { property: pt, role: roleType }`, derived from the range of `qfd`.

**Runtime key** (`runtimeIndexForPropertyQueries`):
Constructed from `(typeOfInstanceOnPath, propertyBearingType, property, replacementProperty)`.
Produces the same `RTPropertyKey { property, role }` structure, accounting for Aspect property
aliases.

**Description stored**: `qWithAK` unmodified (the full `ZQ backward forward`).
The backward starts with `Value2Role pt` (domain `VDOM pt`).

**At runtime** (`aisInPropertyDelta`):
`handleBackwardQuery propertyBearingInstance iq` is called.
At runtime `Value2Role` compiles to the identity function, so the `propertyBearingInstance`
(a `RoleInstance`) is passed through unchanged.

**Purpose**: RTPropertyKey queries are exclusively **state queries** (`users = []`). Perspective
synchronisation for property changes is handled separately by `addDeltasForPropertyChange`, which
uses RTContextKey queries (see §3.3). The comment in `aisInPropertyDelta` confirms:
`handleBackwardQuery` will not return any users for property queries.

**Domain/range invariant**: The `VDOM` domain annotation on `Value2Role` is a compile-time type
label only; at runtime the compiled function is identity regardless of domain. ✓

### §3.3 RTContextKey — role instance added/removed (ContextF kink)

**Kink point**: A `RolGetter (ENR role)` step in the original query — the query traverses _from_
a context _to_ a role instance.

**`qfd`** (first backward step): A `ContextF` step (inverse of `RolGetter`).
- `domain qfd = RDOM (ST (RoleInContext { context: ctx, role: roleType }))` — the role instance
  that the context step starts from.
- `range qfd = CDOM ctx` — the context it navigates to.

**Compile-time key** (`typeLevelKeyForContextQueries`):
`roleDomain qfd` extracts `RDOM roleType` and produces
`RTContextKey { role_origin: roleType, context_destination: ctx }`.

**Runtime key** (`runtimeIndexForContextQueries`):
Called with `(r: EnumeratedRoleType, c: ContextInstance)` (the role type and context type of the
changed role instance). Produces the same `RTContextKey { role_origin, context_destination }`.
Multiple keys are emitted for Aspect role types via `roleContextCombinations`.

**Description stored**: `qWithAK` unmodified — the backward starts with `ContextF` (domain `RDOM role`).

**Filter** (`invertedQueryHasRoleDomain cType rType`):
Checks `domain (backwards description) = RDOM adt` where `(cType, rType)` is the actual
role+context at runtime. Uses `equalsOrSpecialisesRoleInContext` so that a query stored for an
Aspect role also fires for its specialisations.

**At runtime** (`usersWithPerspectiveOnRoleInstance` — CONTEXT STEP branch):
`handleBackwardQuery roleInstance iq` is called with the newly added/removed role instance.
The backward's first step is `ContextF` (domain `RDOM role`) — exactly the kind of value
that `roleInstance` represents. ✓

### §3.4 RTRoleKey — role instance added/removed (RolGetter kink)

**Kink point**: A `ContextF` step in the original query — the query traverses _from_ a role
_to_ its context.

**`qfd`** (first backward step): A `RolGetter (ENR role)` step (inverse of `ContextF`).
- `domain qfd = CDOM ctx` — a context instance.
- `range qfd = RDOM (ST (RoleInContext { context: ctx, role: roleType }))`.

**Compile-time key** (`typeLevelKeyForRoleQueries`):
`roleRange qfd` extracts `RDOM roleType` and produces
`RTRoleKey { context_origin: ctx, role_destination: roleType }`.

**Runtime key** (`runTimeIndexForRoleQueries`):
Called with `(r: EnumeratedRoleType, c: ContextType)` when a role instance of type `r` is
added/removed from context `c`. Produces the same `RTRoleKey { context_origin: c, role_destination: r }`.

**Description transformation** (`removeFirstBackwardsStep` with compensating `ContextF`):
The `RolGetter` first-backward step has domain `CDOM ctx`, but at runtime we start from a
**role instance**, not a context. Therefore the first step is dropped. To compensate, a `ContextF`
step (domain `RDOM role`) is prepended to the forwards part so that the forward computation can
still reach the original context.

After removal: `domain (new backward) = range (RolGetter) = RDOM roleType`.

**Filter** (`invertedQueryHasRoleDomain cType rType`):
After step removal the backward's domain is `RDOM (ST (RoleInContext {context, role}))`.
The filter checks that the runtime role+context specialises this stored domain. ✓

**Special case** — when `removeFirstBackwardsStep` produces `ZQ Nothing _` (the full backward
consisted of only one `RolGetter` step), the description is silently discarded (`pure unit`).
This is correct: a backward path of only `RolGetter` means the query started at `ContextF` with
nothing preceding it. After removal there is nothing left to navigate, so no useful inverted query
can be formed.

**At runtime** (`usersWithPerspectiveOnRoleInstance` — ROLE STEP branch):
`handleBackwardQuery roleInstance iq` is called. After step removal, backward has domain `RDOM`
— matching the `roleInstance` argument. ✓

### §3.5 RTFillerKey — role binding changed (FillerF kink)

**Kink point**: A `FilledF` step in the original query — the query traverses _from_ a filler
role _to_ a filled role.

**`qfd`** (first backward step): A `FillerF` step (inverse of `FilledF`).
- `domain qfd = RDOM (ST (RoleInContext { ... filled role type ... }))` — the filled role.
- `range qfd = RDOM (ST (RoleInContext { ... filler role type ... }))` — the filler role.

**Compile-time key** (`typeLevelKeyForFillerQueries`):
`domain qfd` gives the filled role; `range qfd` gives the filler role.
Produces `RTFillerKey { filledRole_origin, filledContext_origin, fillerRole_destination, fillerContext_destination }`.
The query is **stored under the filled role type**, even though the filler is what changes.

**Runtime key** (`runtimeIndexForFillerQueries'`):
Called with `(filledType, filledContextType)` — the type of the role whose filler changed.
Produces the same `RTFillerKey { filledRole_origin = filledType, ... }`.

**Description transformation** (`removeFirstBackwardsStep` with no compensating step):
After removal: `domain (new backward) = range (FillerF) = RDOM fillerType`.

**Filter** (`invertedQueryHasRoleDomain fillerContextType fillerType`):
After step removal the backward's domain is `RDOM filler`. The filter checks that the runtime
filler type+context specialises this stored domain. ✓

**At runtime** (`usersWithPerspectiveOnRoleBinding'` — FILLER STEP branch):
`handleBackwardQuery filler iq` is called. After step removal, backward has domain `RDOM filler`
— matching the `filler` argument. ✓

**Single-step degenerate case**: When the original backward consists only of `FillerF`, step
removal produces `ZQ Nothing fwd`. In this case the stored description replaces `Nothing` with an
identity step `SQD ran IdentityF ran`, so the backward is always defined. ✓

### §3.6 RTFilledKey — role binding changed (FilledF kink)

**Kink point**: A `FillerF` step in the original query — the query traverses _from_ a filled
role _to_ its filler.

**`qfd`** (first backward step): A `FilledF fld ctx` step (inverse of `FillerF`).
- `domain qfd = RDOM (ST (RoleInContext { ... filler role type ... }))` — the filler role.
- `range qfd = RDOM (ST (RoleInContext { context: ctx, role: fld }))` — the filled role.

**Compile-time key** (`typeLevelKeyForFilledQueries`):
`domain qfd` gives the filler type; `range qfd` gives the filled type.
Produces `RTFilledKey { fillerRole_origin, fillerContext_origin, filledRole_destination, filledContext_destination }`.
The query is stored under the filled role (the one that receives the filler).

**Runtime key** (`runtimeIndexForFilledQueries'`):
Called with `(filledType, filledContextType)`.
Produces the same `RTFilledKey { ..., filledRole_destination = filledType, ... }`.

**Description transformation** (`removeFirstBackwardsStep` with no compensating step):
After removal: `domain (new backward) = range (FilledF) = RDOM filledType`.

**Filter** (`invertedQueryHasRoleDomain filledContextType filledType`):
After step removal the backward's domain is `RDOM filled`. The filter checks that the runtime
filled type+context specialises this stored domain. ✓

**At runtime** (`usersWithPerspectiveOnRoleBinding'` — FILLED STEP branch):
`handleBackwardQuery filled iq` is called. After step removal, backward has domain `RDOM filled`
— matching the `filled` argument. ✓

**Single-step degenerate case**: Same treatment as RTFillerKey — replaced with an identity step. ✓

### §3.7 The `invertedQueryHasRoleDomain` Filter

The filter `invertedQueryHasRoleDomain :: ContextType -> EnumeratedRoleType -> InvertedQuery -> MP Boolean`
is used for RTContextKey, RTRoleKey, RTFillerKey and RTFilledKey categories. It checks:

```
domain (backwards (description iq)) = RDOM adt
  where (ST (RoleInContext { context, role })) `equalsOrSpecialisesRoleInContext` adt
```

The direction of specialisation is: **runtime type specialises stored type**. This is correct
because:
- Stored queries may be indexed on Aspect types (more general).
- At runtime we encounter concrete (possibly more specific) role types.
- A concrete role that specialises an Aspect should trigger all queries stored for that Aspect.

`equalsOrSpecialisesRoleInContext` converts both sides to Conjunctive Normal Form using the full
Aspect hierarchy, making the check complete for all levels of specialisation.

### §3.8 Summary: Correctness Status

All five categories are correctly implemented. The table below summarises the key invariants:

| Category | Key indexed by | Domain of backward at runtime | Applied to |
|---|---|---|---|
| RTPropertyKey | property + role bearing the property | `VDOM` (identity at runtime) | `propertyBearingInstance` |
| RTContextKey | role type + context type | `RDOM role` | role instance (ContextF step) |
| RTRoleKey | context type + role type | `RDOM role` (after removing RolGetter) | role instance |
| RTFillerKey | filled role type | `RDOM filler` (after removing FillerF) | filler instance |
| RTFilledKey | filled role type | `RDOM filled` (after removing FilledF) | filled instance |

No keys are missed at runtime, no domain/range mismatches exist, and all filter conditions are
correctly directed.

The one architectural asymmetry worth noting: for RTFillerKey, the runtime filler type is used for
filtering (`invertedQueryHasRoleDomain fillerContextType fillerType`) but the **key itself is
indexed by the filled type**. This is by design: the query is stored under the filled role so that
it fires whenever that filled role's filler changes, regardless of the concrete filler type.
