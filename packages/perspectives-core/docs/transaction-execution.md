# Transaction Execution Process

This document describes how Perspectives executes a transaction, covering the two entry paths (user-initiated and peer-received), the multi-phase processing loop, the design rationale, and observed design considerations.

> **Source modules:** `Perspectives.RunMonadPerspectivesTransaction` (`runMonadPerspectivesTransaction.purs`), `Perspectives.Sync.HandleTransaction` (`handleTransaction.purs`), `Perspectives.AMQP.IncomingPost` (`incomingPost.purs`), `Perspectives.Sync.Transaction` (`transaction.purs`), `Perspectives.ContextStateCompiler`, `Perspectives.RoleStateCompiler`.

---

## Background: The Production-Rule Model

Perspectives behaves like a production-rule system (a Post system). A change made by the end user triggers state transitions, which fire automatic actions (`on entry` / `on exit` blocks), which may themselves produce more changes, trigger more states, and so on.

Two conflicting goals must be balanced:

1. **Monotone simulation.** To help modellers reason about their models, destructive operations (unbinding roles, calling external destructive effects, removing contexts, removing roles) should be executed *last* — after all constructive operations have been processed. Within one pass this simulates a purely monotone data-collection phase, making the results easier to predict.
2. **Closed World Hypothesis (CWH).** State conditions may test for the *absence* of data (`not exists X`). This makes execution order observable: a state entered early may test that X does not yet exist, while a state entered later may see that X has already been created.

The implementation accepts this tension and mitigates it through the mechanisms described below.

---

## Transaction Record

Every transaction runs with a mutable `Transaction` record that accumulates all side-effects. The key fields are:

| Field | Type | Purpose |
|---|---|---|
| `deltas` | `Array DeltaInTransaction` | Signed deltas to be distributed to peers |
| `createdContexts` | `Array ContextInstance` | Contexts created during this pass (cleared per phase1 iteration) |
| `createdRoles` | `Array RoleInstance` | Roles created during this pass (cleared per phase1 iteration) |
| `rolesToExit` | `Array RoleInstance` | Roles whose states must be exited before physical removal |
| `scheduledAssignments` | `Array ScheduledAssignment` | Deferred operations: `ContextRemoval`, `RoleRemoval`, `RoleUnbinding`, `ExecuteDestructiveEffect` |
| `invertedQueryResults` | `Array InvertedQueryResult` | Context/role instance sets that need state re-evaluation (phase2) |
| `correlationIdentifiers` | `Array CorrelationIdentifier` | Query subscriptions to notify at transaction end |
| `untouchableContexts` | `Array ContextInstance` | Contexts marked for removal; no deltas or state evaluations should target them |
| `untouchableRoles` | `Array RoleInstance` | Roles marked for removal; same invariant |
| `postponedStateEvaluations` | `Array StateEvaluation` | State evaluations deferred because they depend on an untouchable resource |
| `executedStateKeys` | `Set String` | `(stateId, instanceId)` pairs already processed; prevents duplicate execution |
| `modelsToBeRemoved` | `Array ModelUri` | Models to be deleted after the transaction |
| `publicKeys` | `EncodableMap PerspectivesUser PublicKeyInfo` | Cryptographic public-key data accompanying the deltas |

---

## Two Entry Paths

### Path A – Own User Action

```
runMonadPerspectivesTransaction authoringRole action
  = runMonadPerspectivesTransaction' shareWithPeers authoringRole action
```

`shareWithPeers = true`. All deltas accumulated during the transaction will be distributed to the relevant peers at the end of phase 2.

Called from the PDR API when the end user performs a change (e.g. creating a role, setting a property, removing a context).

### Path B – Incoming Peer Transaction (`Perspectives.AMQP.IncomingPost`)

```
runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser) (executeTransaction body)
```

`share = false`. The deltas arrived from a peer who has already distributed them to all parties who should receive them. Re-distributing them would cause an infinite loop.

`executeTransaction` first verifies the cryptographic signatures of all deltas and the public-key information embedded in the transaction. If verification fails the whole transaction is rejected. On success, `executeTransaction'` is called to execute the individual deltas.

After the non-sharing transaction finishes, `detectPublicStateChanges` is called as a separate step (see below).

---

## Initialization: `runMonadPerspectivesTransaction'`

1. Create a fresh `Transaction` with the given `authoringRole` and an empty record.
2. **Lower the transaction flag** — an `AVar Boolean` that serialises concurrent transactions. Taking the value (lowering the flag) means "a transaction is now running". Callers block until the flag is available.
3. Assign a unique, monotonically increasing `transactionNumber` (for logging).
4. Execute the action (either the user action or `executeTransaction`), then immediately enter `phase1`.
5. **Raise the flag** again on success or failure (guaranteed by an error boundary).

Nested ("embedded") transactions are supported via `runEmbeddedTransaction` / `runEmbeddedIfNecessary`. These explicitly raise the flag so that `runMonadPerspectivesTransaction'` can take it again. A nesting depth counter (`transactionLevel`) is maintained for log indentation.

---

## Executing Incoming Deltas (`executeTransaction'`)

When processing a peer transaction, `executeTransaction'` processes each `SignedDelta` in order:

1. **Public-key deltas first**: Any key-management deltas in `publicKeys` are executed to establish the author's identity in the local store.
2. **Content deltas**: Each `SignedDelta` is deserialized (by trying each delta type in sequence: `RolePropertyDelta`, `RoleBindingDelta`, `ContextDelta`, `UniverseRoleDelta`, `UniverseContextDelta`). Authorization is checked for each delta. On success the corresponding update function is called. Failures are caught and logged silently. This is a deliberate partial-tolerance design: a single unexecutable or unauthorised delta (e.g. a reference to a resource that has since been removed, or a stale authorization) must not prevent the remaining deltas in the batch from being processed. Authentication failures are also silent ("For now, we fail silently on deltas that cannot be authenticated" — source comment); this is an acknowledged trade-off, not a permanent design goal.

Each delta execution can populate `createdContexts`, `createdRoles`, `rolesToExit`, `scheduledAssignments`, and `invertedQueryResults` in the running Transaction.

---

## Phase 1: Monotonic Actions and Exit Handling

`phase1` is entered after the initial action and recurses until a stable state is reached.

### Step 1.1 – Mark resources as untouchable

Before anything else, all `rolesToExit` are added to `untouchableRoles` and all `ContextRemoval` targets in `scheduledAssignments` are added to `untouchableContexts`. No delta and no state evaluation result should target a resource listed here. State evaluations that *do* depend on such a resource are deferred to `postponedStateEvaluations`.

### Step 1.2 – Snapshot and clear

The current values of `createdContexts`, `createdRoles`, and `rolesToExit` are captured, then those three fields are cleared in the Transaction. Any new items added to them during the steps below will be detected at the end to decide whether to recurse. `ContextRemoval` entries are intentionally left in `scheduledAssignments` (they are needed in phase 2 to perform the actual removal), but they are excluded from the snapshot used for the recursion check.

### Step 1.3 – Enter root states of newly created contexts

For each context in the snapshot of `createdContexts`, the root state(s) of its type are looked up and `enteringState` is called for each one (guarded by `executedStateKeys` to prevent double execution).

`enteringState`:
- Registers the state as active on the context instance.
- Runs every `automaticOnEntry` action whose `allowedUser` matches the current end user.
- Recursively enters any sub-states whose condition is satisfied.
- Automatic actions can produce new `createdContexts`, `createdRoles`, new `scheduledAssignments`, and `invertedQueryResults`.

If `share = false` (peer transaction), this step runs inside `runSharing`, which spawns an embedded *sharing* transaction so that own-user reactions are distributed to the user's peers.

### Step 1.4 – Enter root states of newly created roles

Same as step 1.3, but for `createdRoles` via `enteringRoleState`.

### Step 1.5 – Exit states of roles scheduled for removal

For each role in the snapshot of `rolesToExit` that still has active states, `queryUpdatesForRole` is called first (to prepare query updates), then `exitingRoleState` is called for the root state(s).

`exitingRoleState`:
- Recursively exits sub-states.
- Stops any repeating fibers associated with the state.
- Runs every `automaticOnExit` action.
- Removes the state from the role's active-state list.
- Clears the corresponding keys from `executedStateKeys` so the same state can be re-entered later in the same transaction if the role is re-created.

Runs under `runSharing` for non-sharing transactions.

### Step 1.6 – Exit states of contexts scheduled for removal

For each `ContextRemoval` in `scheduledAssignments` whose context still has active states, `exitContext` is called:
- Exits all root states via `exitingState`.
- Calls `stateEvaluationAndQueryUpdatesForContext`, which schedules role-level query updates and adds role instances to `rolesToExit`.

Runs under `runSharing` for non-sharing transactions.

### Step 1.7 – Update untouchable lists

Append any newly accumulated `rolesToExit` and new `ContextRemoval` targets to the untouchable lists. This preserves the invariant that the lists grow monotonically within the transaction.

### Step 1.8 – Recursion check

Phase 1 recurses if any of the following appeared since the snapshot was taken:
- New entries in `createdContexts`
- New entries in `createdRoles`
- New entries in `rolesToExit`
- New `ContextRemoval` entries in `scheduledAssignments`

If any of these conditions is true, go back to step 1.1.

### Step 1.9 – Execute deferred destructive operations (base case only)

When the recursion check is false (everything has stabilised), execute:
- `RoleUnbinding`: change or remove the filler of a role via `changeRoleBinding`.
- `ExecuteDestructiveEffect`: call external functions registered as destructive effects.

These items are then removed from `scheduledAssignments`. Only `ContextRemoval` and `RoleRemoval` items remain for phase 2.

### Transition to phase 2

Phase 1 ends by calling `phase2`.

---

## Phase 2: State Re-evaluation, Distribution, and Cleanup

### Step 2.1 – Recursive state evaluation

`invertedQueryResults` is cleared from the Transaction, then `recursivelyEvaluateStates` is called:

1. Compute `StateEvaluation` records from the inverted-query results (each `ContextStateQuery` or `RoleStateQuery` maps affected instances to their root states).
2. Deduplicate (`dedupeStateEvaluations`).
3. For each `StateEvaluation`, call `evaluateContextState` or `evaluateRoleState`:
   - If the state condition is **true** and the instance was *not* already in this state: call `enteringState` / `enteringRoleState` (triggers `automaticOnEntry`, may create new resources).
   - If the state condition is **false** and the instance *was* in this state: call `exitingState` / `exitingRoleState`.
   - If the state condition is **undetermined** (the evaluator cannot decide because it depends on an untouchable resource): add a `ContextStateEvaluation` / `RoleStateEvaluation` to `postponedStateEvaluations`.
   - If already in state but condition still true: evaluate sub-states only.
4. If new `invertedQueryResults` were added during this pass, recurse (go back to step 1 of this loop).

`executedStateKeys` prevents the same `(stateId, instanceId)` pair from being evaluated more than once per outer phase-1/phase-2 pass.

Runs under `runSharing` for non-sharing transactions.

### Step 2.2 – Check whether phase 1 must re-run

After state evaluation, the Transaction is inspected for new items added since phase 2 was entered. If any of the following are true, control returns to `phase1`:
- New `createdContexts`
- New `createdRoles`
- New `rolesToExit`
- New `ContextRemoval`, `RoleUnbinding`, or `ExecuteDestructiveEffect` in `scheduledAssignments`

### Step 2.3 – Distribute deltas (sharing transactions only)

When the transaction is sharing (`share = true`), `distributeTransaction` is called:
- First saves any changed domain files.
- Then partitions the deltas by recipient using `transactieForEachUser` (every delta names the user instances who should receive it; `userRoleBottoms` maps role instances to their outermost filler to find the actual `PerspectivesUser`).
- For each `Peer` destination: sends the `TransactionForPeer` via AMQP (or saves to the outgoing post database if not connected).
- For each `PublicDestination`: `expandDeltas` decorates the deltas with the public-resource storage URL, then `executeDeltas` applies them locally (writing to the public CouchDB store). These deltas are *not* sent anywhere.
- `deltas` is cleared from the Transaction to prevent re-execution.

When `share = false`, `distributeTransaction` is skipped and `publicRoleTransactions` is an empty map.

### Step 2.4 – Physical removal of contexts and roles

For each remaining `ContextRemoval` in `scheduledAssignments`: `removeContextInstance` is called, which physically deletes the context and its associated roles from the store.

For each `RoleRemoval`: `removeRoleInstance` physically deletes the role.

`scheduledAssignments`, `untouchableContexts`, and `untouchableRoles` are all cleared.

### Step 2.5 – Postponed state evaluations

If `postponedStateEvaluations` is non-empty, those state evaluations are now run via `evaluateStates` (the resources they depend on have been removed, so the conditions can now be evaluated). Afterwards the field is cleared and phase 2 recurses from the beginning.

### Step 2.6 – Remove models

If there are no postponed state evaluations, any models in `modelsToBeRemoved` are deleted from the store.

### Step 2.7 – Notify query subscribers

The `correlationIdentifiers` in the Transaction identify active query subscriptions in the client application. These are sorted in ascending order (to ensure components are never updated after they have been removed) and each registered `runner` function is invoked, which re-runs the corresponding query and pushes the new result to the client.

This is the final step; the result value from the original action is returned.

---

## The `runSharing` Mechanism

When `share = false`, calls to `runSharing` inside phase 1 and phase 2 spawn an embedded *sharing* transaction:

```purescript
runSharing false authoringRole t =
  lift $ runEmbeddedTransaction shareWithPeers authoringRole t
```

This embedded transaction runs with `share = true`, so any deltas it produces are distributed to the own user's peers. This is the mechanism that ensures the own user's *reactions* to a peer's changes (state entries, automatic actions) are correctly synchronised with other peers, while the original peer's deltas are not re-sent.

The `authoringRole` of the embedded transaction is the own user's role, so peers correctly attribute the reaction to the own user.

---

## Post-Transaction: `detectPublicStateChanges`

After a non-sharing (peer) transaction, the caller (`transactionConsumer` in `incomingPost.purs`) calls `detectPublicStateChanges`:

```
while publicRolesJustLoaded is non-empty:
  for each role in publicRolesJustLoaded:
    find all roles it fills (filler2filledFromDatabase_)
    re-evaluate states for each filled role (reEvaluatePublicFillerChanges)
  clear publicRolesJustLoaded
  repeat
```

Public roles (those loaded from a public CouchDB store) are tracked in `publicRolesJustLoaded` during the transaction. After the transaction, any local role instances that are filled by these public roles may have changed state. `detectPublicStateChanges` starts a new non-sharing transaction for each batch.

---

## Flow Diagram (simplified)

```
runMonadPerspectivesTransaction' share authoringRole action
│
├─ Lower transactionFlag (serial execution)
├─ Execute action (user change OR executeTransaction for peer)
│    └─ Accumulates deltas, createdContexts/Roles, scheduledAssignments, invertedQueryResults
│
└─ phase1 share authoringRole
     │
     ├─ Mark rolesToExit + ContextRemovals as untouchable
     ├─ Snapshot + clear createdContexts, createdRoles, rolesToExit
     ├─ [runSharing] Enter root states of new contexts  → may add more to createdContexts, createdRoles, scheduledAssignments, invertedQueryResults
     ├─ [runSharing] Enter root states of new roles
     ├─ [runSharing] Exit states of roles to exit
     ├─ [runSharing] Exit states of contexts to remove
     ├─ Update untouchable lists
     │
     ├─ IF new contexts/roles created OR new ContextRemovals: ──► recurse to phase1
     │
     ├─ Execute RoleUnbinding, ExecuteDestructiveEffect
     │
     └─ phase2 share authoringRole
          │
          ├─ [runSharing] recursivelyEvaluateStates (invertedQueryResults)
          │    └─ evaluateContextState / evaluateRoleState
          │         ├─ entering/exiting state → automaticOnEntry/Exit → more deltas/resources
          │         ├─ undetermined → postponedStateEvaluations
          │         └─ if new invertedQueryResults: recurse within recursivelyEvaluateStates
          │
          ├─ IF new createdContexts/Roles, rolesToExit, or destructive scheduledAssignments: ──► phase1
          │
          ├─ [if share] distributeTransaction → send to Peer destinations
          │                                  → execute on PublicDestination destinations
          ├─ Clear deltas
          ├─ Remove contexts (ContextRemoval) and roles (RoleRemoval)
          ├─ Clear scheduledAssignments, untouchables
          │
          ├─ IF postponedStateEvaluations non-empty:
          │    ├─ [runSharing] evaluateStates(postponedStateEvaluations)
          │    └─ recurse phase2
          │
          ├─ Remove models (modelsToBeRemoved)
          └─ Run correlationIdentifiers (client query updates)
```

After `runMonadPerspectivesTransaction'` returns (path B only):
```
detectPublicStateChanges
  └─ while publicRolesJustLoaded non-empty:
       run new non-sharing transaction to re-evaluate states for filled roles
```

---

## Design Considerations and Potential Issues

### 1. Destructive operations are deferred, but state exits are not
The stated goal is to delay destructive operations until all constructive operations have completed. Phase 1 successfully defers `RoleUnbinding`, `ExecuteDestructiveEffect`, `ContextRemoval`, and `RoleRemoval` to step 1.9 and phase 2, respectively. However, **state exits** (`automaticOnExit` actions) are executed as part of phase 1 (steps 1.5 and 1.6), interleaved with state entries. Exit actions can themselves be destructive (e.g. `delete`, `unbind`). Those produce further `ScheduledAssignment` items that are deferred to the *next recursive call* of phase 1, but the exit itself runs during the same phase-1 pass as the entries. Modellers should be aware that `on exit` blocks do not enjoy the same "last" guarantee as `remove context` / `remove role` statements.

### 2. `executedStateKeys` and state re-entry within one transaction
When a role exits its states (step 1.5), the corresponding state keys are removed from `executedStateKeys`. This allows the same state to be entered again for that role instance in the same transaction if the role is re-created. While intentional (supporting "delete and recreate" patterns), it creates a risk: if an automatic action on state entry recreates the same role and the role's state then causes the same exit again, the system will loop until some external condition breaks the cycle (e.g. a predicate becomes false). The `executedStateKeys` mechanism does **not** guard against this cross-iteration cycle.

### 3. CWH and construction ordering
When multiple deltas arrive in a single incoming transaction and are executed sequentially (step by step), the `invertedQueryResults` accumulated by each delta are not evaluated until phase 2. This means that state conditions checking `not exists X` may see a stale view of the world during the initial delta-execution pass: X may have been added by an earlier delta in the same batch but the state evaluating `not exists X` has not yet been re-evaluated. However, in phase 2 all affected instances are re-evaluated together, so the eventual outcome is correct.

### 4. Own-user reactions during peer transactions
The `runSharing` pattern (embedding a sharing sub-transaction inside a non-sharing outer transaction) correctly ensures that own-user reactions are distributed. One subtlety: the embedded sharing transaction runs `phase1` and `phase2` fully, including its own delta distribution. This means that state-triggered own-user actions can themselves trigger further state evaluations and distributions. Because each embedded transaction runs to completion before `runSharing` returns, these nested effects are fully resolved before the outer non-sharing transaction continues.

### 5. Serialisation of transactions
The transaction flag (`transactionFlag AVar`) serialises all top-level transactions. Only one transaction runs at a time for a given installation. Incoming peer transactions are processed one by one in `transactionConsumer`. This prevents race conditions between concurrent transactions but also means that a slow transaction (e.g. one that creates many resources and fires many states) will block subsequent ones.

### 6. `postponedStateEvaluations` and convergence
State evaluations whose condition depends on an untouchable resource are deferred to `postponedStateEvaluations`. After physical removal (step 2.4), these are re-evaluated. The resulting evaluations may in turn produce new changes, which is handled by the recursive call to phase 2. Convergence is not formally guaranteed; the modeller's responsibility is to design state models that do not cycle. In practice, once the untouchable resources are removed, conditions that depended on them resolve in one direction, and further cascades are bounded by the finite number of instances.

### 7. Order of `correlationIdentifiers` and client-side updates
Client query subscriptions (`correlationIdentifiers`) are run at the very end of phase 2, sorted ascending. Sorting prevents updating a client-side component after it has been removed (since removal uses lower IDs). This ordering guarantee depends on the assumption that components are assigned monotonically increasing IDs over their lifetime.

### 8. `detectPublicStateChanges` runs outside the main transaction
`detectPublicStateChanges` starts fresh non-sharing transactions after the main incoming-post transaction has completed. This means public-role state changes are processed asynchronously with respect to the peer transaction that caused them. If a peer transaction causes public roles to be loaded, and those roles affect local state conditions, those conditions will only be evaluated after the main transaction is fully committed. This is generally correct (the public data is now stable), but it means there can be a brief window between the peer transaction finishing and the public-state-triggered reactions completing.
