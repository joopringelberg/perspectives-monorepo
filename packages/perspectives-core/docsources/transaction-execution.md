# Transaction Execution Process

This document describes how Perspectives executes a transaction, covering the two entry paths (user-initiated and peer-received), the multi-phase processing loop, the design rationale, and observed design considerations.

> **Source modules:** `Perspectives.RunMonadPerspectivesTransaction` (`runMonadPerspectivesTransaction.purs`), `Perspectives.Sync.HandleTransaction` (`handleTransaction.purs`), `Perspectives.AMQP.IncomingPost` (`incomingPost.purs`), `Perspectives.Sync.Transaction` (`transaction.purs`), `Perspectives.ContextStateCompiler`, `Perspectives.RoleStateCompiler`, `Perspectives.Extern.RunAction` (`runActionExtern.purs`), `Perspectives.Assignment.RunAction` (`runAction.purs`).

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
4. **Push a fresh frame** onto the `PendingSettledStack` (see [§9](#9-once-settled-staged-actions-the-pendingsettledstack)).
5. Execute the action (either the user action or `executeTransaction`), then immediately enter `phase1`.
6. **Pop the frame** and, on success, hand its contents to `transactionWithTiming` for dispatch; on failure, discard it.
7. **Raise the flag** again on success or failure (guaranteed by an error boundary).

Nested ("embedded") transactions are supported via `runEmbeddedTransaction` / `runEmbeddedIfNecessary`. These explicitly raise the flag so that `runMonadPerspectivesTransaction'` can take it again. A nesting depth counter (`transactionLevel`) is maintained for log indentation. Because they go through the same `runMonadPerspectivesTransaction'` entry point, they push and pop their own `PendingSettledStack` frame too — see §9 for why this matters.

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

See [Transaction Distribution to Peers](transaction-distribution.md) for details, including how `pub:` and `def:` PerspectivesUsers resources are handled.

When `share = false`, `distributeTransaction` is skipped and `publicRoleTransactions` is an empty map.

### Step 2.4 – Physical removal of contexts and roles

For each remaining `ContextRemoval` in `scheduledAssignments`: `removeContextInstance` is called, which physically deletes the context and its associated roles from the store.

For each `RoleRemoval`: `removeRoleInstance` physically deletes the role.

For non-sharing transactions, an additional pass is now executed immediately after these removals: when the phase-2 entry snapshot contained `invertedQueryResults` and at least one physical removal (`ContextRemoval` or `RoleRemoval`) was executed, the same `invertedQueryResults` are evaluated again (via `recursivelyEvaluateStates` under `runSharing`).

This second pass closes the timing gap where an earlier embedded sharing pass evaluated a condition before the resource was physically removed in the outer non-sharing transaction. It is especially relevant for conditions of the form `not exists ...` that become true only after removal.

After this optional post-removal pass, `scheduledAssignments`, `untouchableContexts`, and `untouchableRoles` are cleared.

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
          ├─ [if non-sharing and removals happened] re-run recursivelyEvaluateStates on phase-2 entry invertedQueryResults
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

Additionally, for non-sharing transactions that physically remove contexts or roles, phase 2 now performs a targeted post-removal replay of the phase-2 entry `invertedQueryResults`. This covers the complementary timing case where `not exists X` only becomes true *after* physical removal in the outer transaction.

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

### 9. `once settled` staged actions: the `PendingSettledStack`

ARC action bodies and automatic `do` effects in `on entry` or `on exit` transitions can split their statements into stages separated by `once settled`, with or without `letA` bindings:

```arc
letA
  version <- create role cm:ModelManifest$Versions in ...
in
  Versions$Version = VersionNumber for version

  once settled
    Store = "Repository" for version >> binding

  once settled
    AutoUpload = true for version >> binding
```

Without `letA`, place `once settled` at the same indentation as the first stage's statements and indent the next stage beneath it. In `do for Author once settled`, the header delays the first stage; a `once settled` inside its body delays the next stage until the first has settled.

Each stage is compiled into a separate `Updater` (`Perspectives.Representation.Action.ActionEffect`, see `Perspectives.Query.StatementCompiler.compileActionEffect`). At run time (`Perspectives.CompileActionEffect.compileActionEffectWith`), the first stage runs synchronously as part of the current transaction; every later stage is handed to `scheduleSettledTransaction` (`Perspectives.CompileTimeFacets`), which is supposed to run it only once the current logical transaction — including everything it triggers — has *settled*.

**The bug (fixed 2026-09-24).** `scheduleSettledTransaction` used to `put` a `SettledTransaction` directly onto the global `transactionWithTiming` AVar the moment a stage finished, mid-action, before `phase1`/`phase2` of the *enclosing* transaction had even run. `forkTimedTransactions` (Main.purs) picks such entries up and runs them as an ordinary new `runMonadPerspectivesTransaction`, which serialises against every other transaction via `transactionFlag`. That looks race-free — except `runEmbeddedTransaction` (used whenever an automatic-action cascade needs an embedded *sharing* sub-transaction, e.g. to re-broadcast an own-user reaction while processing an incoming peer transaction) **momentarily raises `transactionFlag`** so it can take it down again itself. That raise is visible on the *global* AVar, not just to its own nested call — so a queued `SettledTransaction` sitting in `forkTimedTransactions`, waiting on the same flag, could slip through that window and run *before* the outer transaction's own cascade (e.g. a `ReadyToMake` state creating and binding the very role the settled stage targets) had finished. This surfaced as an intermittent `(NoRoleInstanceToSetProperty)` warning for properties set in a `once settled` stage that depends on a binding created earlier in the same automatic action.

**The fix.** `scheduleSettledTransaction` no longer dispatches immediately. `PerspectivesState` now holds a `pendingSettledTransactions :: PendingSettledStack` (`Perspectives.CoreTypes`) — a mutable stack of frames, each an `Array RepeatingTransaction` of not-yet-dispatched `SettledTransaction`s (backed by an `Effect.Ref`, created via the same "pure factory + `unsafePerformEffect`" pattern already used for `LRUCache`). `scheduleSettledTransaction` appends to the *top* frame instead of touching the AVar.

`runMonadPerspectivesTransaction'` (`whenFlagIsDown`) pushes a new, empty frame right after taking the transaction flag down, and pops it right before raising the flag again:
- on success, *after* `phase1`/`phase2` have fully run, the popped frame's entries are `put` onto `transactionWithTiming` one by one (this is the only place that AVar is written to for settled stages now);
- on failure, the popped frame is discarded (a failed transaction's staged continuations do not run).

Because every `runMonadPerspectivesTransaction'` call — top-level *or* embedded — creates its own fresh `Transaction` record but shares the *same* `PendingSettledStack`, the push/pop pairs nest exactly like a call stack:
- a stage scheduled while an **embedded** sub-transaction is running is appended to *that* sub-transaction's own frame, and is dispatched when *that* embedded call finishes (before it returns control to its caller) — well before the outer transaction's own frame is even considered for draining;
- a stage scheduled by the **outer** action is only dispatched once phase1/phase2 of the *entire* outer transaction — including every embedded sub-transaction it triggered — has completed.

In other words, `once settled` continuations are now both **chained** (stage *n+1* is only constructed once stage *n* has run) and **stacked** (an outer frame is only drained once every frame nested inside it has been drained first) — matching what the name always implied, rather than racing a global semaphore against unrelated flag-raise windows opened for a different purpose.

See `Perspectives.CoreTypes` (`PendingSettledStack`, `newPendingSettledStack`, `pushPendingSettledFrame`, `popPendingSettledFrame`, `appendPendingSettled`), `Perspectives.CompileTimeFacets.scheduleSettledTransaction`, and `Perspectives.RunMonadPerspectivesTransaction.whenFlagIsDown`.

### 10. `runContextAction` / `runRoleAction`: depth-first, awaited action invocation

The ARC statements `runContextAction <ArcIdentifier> for <ArcIdentifier> in <step>` and `runRoleAction <ArcIdentifier> for <ArcIdentifier> on <step> in <step>` let one action directly invoke another, for a role filled by the local user ("me") — possibly a different role than the one authoring the calling action. Unlike the ordinary `once settled` dispatch described above, which is intentionally **breadth-first** across independently-scheduled chains (a newly-produced continuation queues behind whatever else is already waiting for the transaction flag), these statements give **depth-first** semantics: the call does not return to the calling action's next statement until the invoked action's entire chain — its synchronous stage *and* every `once settled` stage it (recursively) schedules — has fully settled.

**Why a new QueryFunction was avoided.** `Perspectives.CompileAssignment` / `Perspectives.CompileRoleAssignment` sit upstream of `Perspectives.RunMonadPerspectivesTransaction` in the module graph (reached via `Perspectives.ContextStateCompiler`/`Perspectives.RoleStateCompiler`). Compiling these statements to a new `QueryFunction` handled directly in those compiler modules would import the transaction-embedding machinery into an upstream module and create a cycle. Instead, `Perspectives.Query.StatementCompiler` compiles both statements to the *existing* `QF.ExternalEffectFullFunction` call (`"RunContextActionEffect"` / `"RunRoleActionEffect"`), with the action name and the qualified user role type baked in as `QF.Constant PString` arguments and the context (and, for role actions, the object) supplied as the one "extra" argument that `ExternalEffectFullFunction`'s existing arity dispatch resolves to the resource instance. The real implementations are registered as hidden functions in `Perspectives.Extern.RunAction`, a module that — being looked up dynamically through `hiddenFunctionCache` rather than statically imported by the compiler — is free to depend on `Perspectives.RunMonadPerspectivesTransaction` without creating a cycle.

**Runtime behaviour.** `runContextActionEffect` / `runRoleActionEffect` resolve the user role instance via `getMeInRoleAndContext`; if none is filled by the local user, they do nothing (this is the authorization boundary — there is no further perspective/verb check). Otherwise they invoke the target action through two new functions that mirror `runEmbeddedTransaction` / `runEmbeddedIfNecessary`, but replace their dispatch strategy:

- `runMonadPerspectivesTransactionAwaitingSettlement` is a variant of `runMonadPerspectivesTransaction'` that, instead of calling `dispatchSettledFrame` (which hands the popped frame to `transactionWithTiming` for asynchronous pickup), calls `drainSettledFrame`, which runs each entry **synchronously**, via a recursive call to `runSettledEntryAwaitingSettlement`.
- `runEmbeddedIfNecessaryAwaitingSettlement` is a variant of `runEmbeddedIfNecessary` that calls the above instead of `runMonadPerspectivesTransaction'`.

Because `PendingSettledStack` push/pop already nests like a call stack (see §9), this composes correctly with the invoked action's own `once settled` stages without any special-casing: a stage scheduled while running inside this embedded, awaiting call is appended to *that call's own* frame, and `drainSettledFrame` (not the ordinary async dispatch) is what pops and runs it — before the embedding call returns to its caller. If the invoked action itself calls `runContextAction`/`runRoleAction` again, the same mechanism nests arbitrarily deep, always depth-first.

**Scope.** Only the invoked action's own chain is drained synchronously. Anything scheduled on the *calling* action's own (outer, ordinary) transaction frame — e.g. unrelated automatic actions triggered elsewhere in the same transaction — is unaffected and still dispatched the ordinary, asynchronous, breadth-first way once that outer transaction settles.

**Error handling and attribution.** A failure anywhere in the invoked chain propagates up to the generic `ExternalEffectFullFunction` error boundary in `Perspectives.CompileAssignment` / `Perspectives.CompileRoleAssignment`, which logs it and swallows it, exactly as for any other `callEffect` failure; the calling transaction is not aborted, and nothing already done deeper in the invoked chain is rolled back. The embedded transaction's `authoringRole` — and therefore the authoring role captured for its own `once settled` continuations — is the resolved *target* user role, not the calling action's authoring role.

See `Perspectives.RunMonadPerspectivesTransaction` (`runMonadPerspectivesTransactionAwaitingSettlement`, `runEmbeddedIfNecessaryAwaitingSettlement`, `drainSettledFrame`, `runSettledEntryAwaitingSettlement`), `Perspectives.Extern.RunAction` (`runContextActionEffect`, `runRoleActionEffect`), and `Perspectives.Query.StatementCompiler` (the `RunContextAction` / `RunRoleAction` cases of `describeAssignmentStatement`).

### 11. Variable bindings are private to a fiber

Queries and actions use variable bindings: the PDR binds `currentcontext`, `currentactor`, `notifieduser` and `currentobject` before it runs a state's or action's effect, and `letA`/`letE` bind their own variables. These bindings live in an `Environment (Array String)` (`Perspectives.Instances.Environment`), a stack of frames: `pushFrame` opens a new scope, `addBinding` adds to the top frame, `lookupVariableBinding` searches from the top down, and `restoreFrame` returns to an earlier scope.

**The bug (fixed 2026-10-06).** The environment used to be a single field, `variableBindings`, in `PerspectivesState`, and that state is shared by *every* fiber running against the PDR: API requests (including the calculated property getters that the GUI runs outside of any transaction), transactions forked by `forkTimedTransactions` (`once settled`, `after`, repeating), incoming post, the clocks, and so on. The transaction flag serialises *transactions*, but not the query evaluation that happens outside them. And the save/push/restore pattern (`withFrame`, `withFrame_` in the unsafe compiler, the `WithFrame` cases of the query interpreter and the assignment compilers, `runSettledTransaction` in Main.purs) spans `Aff` suspensions. So two fibers could interleave like this:

1. fiber B pushes a frame (saving the environment as it is now);
2. fiber A binds `currentcontext` (into B's frame, as it happens to be on top);
3. B restores its saved environment — A's binding is gone, or an older, stale value for it is back.

This surfaced in the browser during Reboot Universe: the `once settled` stages of `AddModel$External$CreateVersion` (repositoryTools) for one model wrote to the version role of *another* model that was being added concurrently, so Serialise and Utilities were never installed. In node the timing happened to be different, so the run succeeded.

There was a second leak in the FFI: `ENV.empty` is a single, shared JavaScript object, and `ENV.addVariable` changes the top frame *in place*. A binding made without first pushing a frame was therefore written into one global object, visible to every fiber.

**The fix.** The environment is no longer part of the state. The reader of `MonadPouchdb` (`Perspectives.Persistence.Types`) is now a `PouchdbContext`:

```purescript
type PouchdbContext f =
  { state :: AVar (PouchdbState f)              -- shared by all fibers
  , variableBindings :: Ref VariableBindings    -- private to this run
  }
```

- `runMonadPouchdbWithState` (and therefore `runMonadPerspectives` / `runPerspectivesWithState`, the single way every fiber is started) creates a fresh `Ref` holding a fresh frame on top of `ENV.empty`. Two fibers therefore never share a frame object, even if they mutate it in place.
- The `Parallel` instance gives each parallel branch its own `Ref` with a fresh frame on top of the parent's bindings.
- `MonadAsk`/`MonadReader` still yield just the state `AVar`, so `Control.Monad.AvarMonadAsk` (`gets`, `modify`, …) and `HasPerspectivesState` are unaffected.
- The binding functions in `Perspectives.PerspectivesState` (`getVariableBindings`, `setVariableBindings`, `addBinding`, `lookupVariableBinding`, `withFrame`, `pushFrame`, `restoreFrame`) read and write that `Ref` via `variableBindingsRef`. No code outside them touches the environment directly.

**Consequence for deferred work.** A fiber now starts with *no* bindings at all. Work that is deferred to another fiber can therefore no longer (accidentally) see the scheduling fiber's bindings; it must capture what it needs when it is scheduled. `Perspectives.CompileTimeFacets` does this:
- `scheduleSettledTransaction` (`once settled` time facets and later `letA` stages) captures `currentcontext`, `currentactor` and `notifieduser` (`contextVariableNames`) in addition to the `letA` variable names; `runSettledTransaction` (Main.purs) and `runSettledEntryAwaitingSettlement` rebind them in a new frame.
- `after`, repeating (`Forever`) and `RepeatFor` time facets capture the same names and wrap the transaction with `withCapturedBindings`, which rebinds them in a new frame when the transaction eventually runs.

See `Perspectives.Persistence.Types` (`PouchdbContext`, `runMonadPouchdbWithState`, `variableBindingsRef`), `Perspectives.PerspectivesState` (the variable binding functions), `Perspectives.CompileTimeFacets` (`contextVariableNames`, `captureBindings`, `withCapturedBindings`), and the regression tests in `test/variableBindings.purs` (part of `pnpm run test:layer1`).

## Transaction performance experiments

### Running the specialized two-PDR setup

From `packages/perspectives-core`, with the normal pnpm/PureScript dependencies
installed:

```bash
pnpm run test:performance:collector
pnpm run test:performance --warmup 1 --repetitions 5 --output /tmp/pdr-memory.json
pnpm run test:performance:amqp --warmup 1 --repetitions 5 --profile --output /tmp/pdr-amqp.json
```

The runner requires existing Alice and Bob snapshots at
`test/pdr-snapshot/newdeltas/alice` and `test/pdr-snapshot/newdeltas/bob`, the same
snapshots used by the destructive synchronization suite. These are local fixtures,
not committed credentials. Prepare them using the existing two-PDR workflow
before benchmarking. The AMQP variant additionally requires a reachable broker
and snapshots with valid, mutually connected broker-service contracts. The memory
variant establishes the peer connection during untimed setup. No benchmark
creates a new universe or writes post-test snapshots.
The existing AMQP scaffold enables broker trace logging. Explicitly equalize
logging in the experiment configuration before attributing a cross-transport
timing difference solely to network/broker work.

Each warm-up or measured round runs the complete configured suite in a **fresh
child process**, restoring both snapshots and compiling the model before any
action timer starts. This avoids reusing cached test outcomes or mutated
databases. Discarded warm-up rounds warm external services/filesystem caches,
**not** the next process's JIT or PDR caches. The workload order is fixed;
individual scenarios later in a round may benefit from earlier cache activity.
The default runner does not isolate identical cold/warm actions within one PDR.
For query-reuse experiments, define separately named preparatory and measured
scenarios that exercise the same query paths within a round; scenario identifiers
in a configuration must be unique.

`scripts/performance-runner.mjs` accepts `--mode memory|amqp`, `--warmup`,
`--repetitions`, `--timeout-ms`, `--profile` and `--output`. Defaults are one
warm-up round, five measured rounds and a five-minute worker timeout. Output is
JSON, both on stdout and in the output file (default `performance-report.json`).
Use `/tmp` output paths to keep experimental data outside the repository.
Any failed warm-up, semantic assertion, setup, timeout or worker execution makes
the command exit nonzero; the report retains the outcomes and missing scenarios.

Each scenario reports:

- `senderActionMs`: Alice's synchronous `RunTest` transaction, including its
  cascade and distribution, excluding test-context preparation.
- `bobCompletionMs`: additional time after Alice returns until Bob's success
  condition is observed; this includes polling, not pure receiver execution.
- `endToEndMs`: time from Alice's action start through result observation.
- Completion flags and status, distinguishing an elapsed failure/timeout from a
  successfully observed result.

Per-scenario summaries exclude warm-up rounds and report sample count, minimum,
median, mean, nearest-rank p95, maximum and population standard deviation.
Available elapsed failure timings are included, with separate success/failure
counts; inspect these counts before comparing timing distributions.

`--profile` enables incoming-operation spans only during the measured action
window. Normal runtime execution has no active collector. Spans cover decryption,
`executeTransaction`, the enclosing incoming transaction/cascade, and public-state
processing. The enclosing cascade **includes** `executeTransaction`, so these
durations must not be added together. Spans are wall-clock durations, including
asynchronous waits; the enclosing cascade also includes acquiring the transaction
flag. They are not CPU-time measurements. The session combines incoming activity in
both PDRs, including automatic reaction traffic, rather than attributing every
span solely to Bob. Counts include received messages, decrypted deltas,
public-key entries, wrapped keys, ciphertext-string UTF-8 bytes and decrypted
payload UTF-8 bytes. Ciphertext bytes are **not** total wire bytes. No payload,
key material or peer identifier is retained.

Profiles freeze at result observation or failure, not at global quiescence.
Each span reports started (`count`), `completed`, `unfinished` and `failed`
counts; `totalMs` includes completed spans only. Inspect unfinished counts before
interpreting a phase total. An incoming message retains its original profiling
session across asynchronous processing, so late phases cannot attach themselves
to the following scenario's session.

For function-level CPU attribution, build once and use Node's existing CPU
profiler on the same runner:

```bash
pnpm run build:performance
node --cpu-prof --cpu-prof-dir=/tmp scripts/performance-runner.mjs --warmup 0 --repetitions 1 --profile --output /tmp/pdr-profile.json
```

The forked workers inherit Node profiling flags. Inspect their profiles rather
than only the orchestration process's profile, and correlate hot functions with
the timed phases. CPU profiles also include untimed setup and do not measure
asynchronous database/network wait time. Profiled and unprofiled runs should be
compared separately because instrumentation adds overhead.

### Comparing experiments

Performance experiments must retain the two-PDR correctness checks: a fast sender
is not useful if Bob has not received the intended changes. Snapshot restoration,
connection establishment, model compilation and test-context preparation are
setup costs, not action-processing costs. A sender action includes its synchronous
state cascade and outgoing distribution; observing Bob's result also includes
transport, queueing and the scaffold's polling interval. Do not subtract these
overlapping measurements or label result-observation latency as receiver CPU time.

Use the in-memory transport first to isolate runtime work, then repeat over
RabbitMQ to assess delivery and queueing. Keep the model, snapshots, logging
levels, machine, runtime versions and workload identical across comparisons.
Report cold and warm runs separately: repeated actions may reuse compiled queries
and cached instances, whereas restoring a snapshot resets runtime caches.
Discard warm-up samples from aggregates, but still require them to pass their
semantic assertions. Record failed or timed-out trials instead of silently
excluding them. Median and tail latency are more informative than a single run;
do not impose hardware-dependent timing thresholds in correctness tests.

The existing `TwoPDRDestructiveTests@1.0.arc` model provides role, property,
binding and context-removal workloads with receiver-side success conditions.
For further experiments, supply a `SynchronisationModelConfiguration` for a model
with the same Leader/Follower/RunTest contract. Vary one dimension at a time:

| Experiment | Comparison | What to measure |
|---|---|---|
| Structural neighbourhood | New recipient versus an established recipient; shallow versus deep filler paths | Delta count, payload size, sender latency, receiver latency |
| Author metadata | One versus multiple authors; first contact versus repeated contact | Key-metadata size and signature/key processing |
| Query reuse | First action versus repeated equivalent actions | Compilation/cache-miss work and state-cascade latency |
| Fan-out | One versus several equivalent recipients, then recipients with different permissions | Dependency/property collection and distribution time |
| Persistence and replay | New deltas versus duplicates; ordered delivery versus version gaps | Delta/version-store work and pending recovery, with correctness checks |

### Optimization proposals, not changes to synchronization semantics

These are hypotheses to test, not measured speedups. Establish a baseline and
retain authorization, signature, resource-version and receiver-result checks
before accepting an optimization.

1. **Measure query-cache misses before adding another cache.**
   `src/core/typePersistence/storableInvertedQuery.purs:getQueriesFromCache`
   already caches the compiled results produced by `compileBoth`, including empty
   results. The earlier assumption that all inverted queries are compiled anew is
   therefore no longer generally true. Profile misses and other compilation paths
   (including calculated getters and state/action compilers) separately.
   Any further compiled-function cache must be per PDR, bounded and invalidated
   when contributing models are installed, replaced or removed. Cache executable
   functions, not query results or captured fiber-local variable bindings.
   Test model updates and cross-PDR isolation as well as cold/warm speed.

2. **Reduce repeated dependency collection for equivalent recipients.**
   `src/core/sync/collectAffectedContexts.purs` collects paths, assumptions and
   properties for peers. Reuse work within one transaction only when the
   perspective, context, state-dependent permissions and resulting visibility
   agree. Role type alone is not a safe grouping key: `selfonly`, `authoronly`,
   author identity and instance-specific state can produce different payloads.
   Compare final visible deltas for every recipient, including negative tests
   proving private information is not shared.
   `src/core/sync/deltas.purs:sendPeerTransactions` already groups identical
   serialized transactions and encrypts once with multiple wrapped recipient
   keys; the remaining opportunity is *before* that grouping, not removing
   necessary per-recipient key wrapping.

3. **Omit known structural neighbourhoods only with evidence of receipt.**
   Having a role in a context on the sender does not prove that every recipient
   installation has received that context, its external role and the required
   historical deltas. First measure how much of the payload is repeated
   neighbourhood data. A future acknowledged, per-installation resource/version
   inventory could allow omission, with full bootstrap on first contact, after
   snapshot restoration, on a newly added installation and after recovery.
   Exercise out-of-order delivery and missing-resource recovery before relying on
   such knowledge; preserve the deltas needed for authorization and version-gap
   handling.

4. **Avoid re-sending author keys only with an authenticated knowledge protocol.**
   `src/core/sync/deltas.purs:removeUnnecessaryKeys` already excludes authors that
   do not occur in a peer's deltas. Further savings require proof that the
   recipient installation knows the exact signing-key version and its identity
   evidence, not just that it has met the user. Test first contact, key rotation,
   multiple installations, lost acknowledgments and restored snapshots.
   Missing keys must trigger safe retrieval/retry, never skipped verification.
   Measure both byte savings and verification cost before introducing the
   inventory/acknowledgment overhead.

5. **Reduce repeated parsing and persistence lookups within a received batch.**
   `src/core/sync/handleTransaction.purs` extracts ordering information, checks
   gaps, verifies authors and applies resource-versioned deltas. Investigate
   decoding each envelope once and reusing immutable metadata, and batching
   independent reads of resource versions. Maintain ordered writes and duplicate
   handling; do not parallelize authorization or mutation against evolving
   context/role state. Profile database work separately from cryptography and
   state cascades before choosing the next implementation.

Finally, long transaction-flag hold time explains GUI blocking but is not itself
permission to split a transaction. Yielding between safe computation steps may
improve responsiveness; splitting or reordering mutations can change state
transitions, destructive scheduling and `once settled` behavior. Verify those
semantics and browser responsiveness independently of Node benchmark throughput.
