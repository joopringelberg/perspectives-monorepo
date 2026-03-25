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
- **Filter** (`filter source with criterium`): the criterium is inverted and a `FilterF` step is appended. Crucially, the filter is later *removed* when storing the inverted query (see §1.5), because at runtime we want to detect the change even when the filter now evaluates to false (the user may have just *lost* visibility of an item).
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

**Important limitation**: The inverted query is stored on the type visited by the perspective object query. It is indexed so that mutations to that type trigger the backwards query, which navigates back to a context where the Calculated user role is defined. In that context, the Calculated role is evaluated to find the actual user instances.

However, when the Calculated role's **query itself** traverses a role binding (filler/filled step), a change to that binding can introduce entirely new user instances whose context has never been serialised. This is a known limitation and the subject of ongoing work.
