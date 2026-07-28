# Transaction Execution — Test Script Catalogue

This document describes a set of human-readable test scripts that together exercise every significant path through `Perspectives.RunMonadPerspectivesTransaction` (`runMonadPerspectivesTransaction.purs`).  Each script states:

* **Path exercised** — which steps from [the algorithm description](transaction-execution.md) are under test.
* **Model setup** — the minimal ARC declarations needed (described in prose; no ARC code yet).
* **Initial state** — which instances exist before the triggering action.
* **Triggering action** — what the user does (or what peer delta arrives).
* **Expected outcome** — what should be observable in the PDR state after the transaction completes.

Scripts are grouped by the algorithmic region they exercise.  Cross-references use the step numbers from `transaction-execution.md` (e.g. "Step 1.3", "Step 2.1").

---

## Group 1 — Baseline: Single-pass Phase 1, No Recursion

These scripts verify that the most basic paths through phase 1 work correctly: a single action produces a single round of state entry or exit, with no further cascades.

### T01 — Context creation with no automatic actions

**Path exercised:** Step 1.3 (enter root states of created contexts), step 1.8 recursion check is false, then phase 2 with no `invertedQueryResults`.

**Model setup:** A context type `C` whose root state has no `on entry` or `on exit` automatic actions, and no sub-states.

**Initial state:** No instance of `C` exists.

**Triggering action:** The user creates an instance of `C`.

**Expected outcome:** The new context instance has its root state registered as active.  No further actions fire.  The transaction terminates in a single phase-1 pass and a trivial phase-2 pass.

---

### T02 — Role creation with no automatic actions

**Path exercised:** Step 1.4 (enter root states of created roles).

**Model setup:** A context type `C` containing an enumerated role type `R`.  The root state of `R` has no automatic actions.

**Initial state:** An instance of `C` exists; `R` is absent.

**Triggering action:** The user creates an instance of `R` inside the existing `C`.

**Expected outcome:** The new role instance has its root state registered as active.  No further actions fire.

---

### T03 — Context creation triggers `on entry` property assignment

**Path exercised:** Step 1.3 — `automaticOnEntry` fires and writes a property value.

**Model setup:** A context type `C` with a role `R` and a Boolean property `P` on `R`.  The root state of `C` has an `on entry` action: `P = true` for an existing instance of `R`.

**Initial state:** An instance of `C` and an instance of `R` exist.  The state of `C` has not been entered yet (i.e. this is a fresh context creation in the same transaction).

**Triggering action:** The user creates a new instance of `C` together with `R`.

**Expected outcome:** After the transaction, `P` on `R` equals `true`.  No new contexts or roles are created, so phase 1 does not recurse.

---

### T04 — Role creation triggers `on entry` property assignment

**Path exercised:** Step 1.4 — `automaticOnEntry` for a role state fires and writes a property.

**Model setup:** Context type `C` with role type `R`.  `R` has a Boolean property `Q`.  The root state of `R` has an `on entry` action: `Q = true`.

**Initial state:** An instance of `C` exists.

**Triggering action:** The user creates a new instance of `R` in that context.

**Expected outcome:** `Q` on the new role instance equals `true`.

---

### T05 — Role removal triggers `on exit` property assignment (no cascade)

**Path exercised:** Step 1.5 (exit states of roles scheduled for removal).

**Model setup:** Context type `C` with roles `R` and `Recorder`.  `Recorder` has a Boolean property `Exited`.  The root state of `R` has an `on exit` action: `Exited = true` on the single `Recorder` instance.

**Initial state:** Instances of `C`, `R`, and `Recorder` exist.  `R` is in its root state.

**Triggering action:** The user removes the instance of `R`.

**Expected outcome:** Before `R` is physically deleted, the exit action fires and sets `Exited = true` on `Recorder`.  `R` is physically removed in phase 2.

---

## Group 2 — Phase 1 Recursion

These scripts verify that phase 1 recurses correctly when an action produces new contexts, roles, or removal candidates (Step 1.8 check is true).

### T06 — Context creation cascades: `on entry` creates another context

**Path exercised:** Step 1.3 fires `automaticOnEntry`; that action accumulates a new context in `createdContexts`; Step 1.8 is true; phase 1 recurses; the second iteration processes the newly created context.

**Model setup:** Two context types `Outer` and `Inner`.  The root state of `Outer` has an `on entry` action: create an instance of `Inner` (via `bind` / `new context`).  `Inner` has a Boolean property `Ready` that its root state's `on entry` action sets to `true`.

**Initial state:** No instances of `Outer` or `Inner`.

**Triggering action:** The user creates an instance of `Outer`.

**Expected outcome:** After the transaction, one `Outer` and one `Inner` instance exist.  `Inner.Ready` is `true`.  Phase 1 ran at least two iterations.

---

### T07 — Role creation cascades: `on entry` creates another role

**Path exercised:** Step 1.4 recurses because an `on entry` action creates a second role.

**Model setup:** Context type `C` with roles `Trigger` and `Result`.  The root state of `Trigger` has an `on entry` action: create `Result`.  The root state of `Result` has an `on entry` action: set Boolean property `Done = true` on `Result`.

**Initial state:** An instance of `C` exists; neither `Trigger` nor `Result` is present.

**Triggering action:** The user creates an instance of `Trigger`.

**Expected outcome:** Both `Trigger` and `Result` are created.  `Result.Done` is `true`.

---

### T08 — Role exit cascades: `on exit` creates a new role

**Path exercised:** Step 1.5 fires `automaticOnExit`; the exit action creates a new role, placing it in `createdContexts/createdRoles`; Step 1.8 is true; phase 1 recurses.

**Note (design consideration):** As documented in Design Consideration 1 of `transaction-execution.md`, `on exit` actions do not enjoy the "last" guarantee.  This test verifies that their constructive output is nonetheless processed correctly in the next recursive phase-1 call.

**Model setup:** Context type `C` with roles `Ephemeral` and `Archive`.  The root state of `Ephemeral` has an `on exit` action: create an instance of `Archive` and set `Archive.Timestamp = now`.

**Initial state:** Instances of `C` and `Ephemeral` exist.

**Triggering action:** The user removes `Ephemeral`.

**Expected outcome:** `Ephemeral` is removed.  An `Archive` instance is created with a `Timestamp` property set.

---

### T09 — Context removal cascades through multiple roles

**Path exercised:** Step 1.6 (exit states of contexts scheduled for removal) → `stateEvaluationAndQueryUpdatesForContext` → new `rolesToExit` added → Step 1.8 true → phase 1 recurse.

**Model setup:** Context type `C` with two enumerated roles, `A` and `B`.  Both have root states with observable `on exit` actions (e.g. setting a flag on an external recorder context).  The removal of `C` should exit both roles' states.

**Initial state:** An instance of `C`, `A`, and `B` all exist with active states.

**Triggering action:** The user removes the context instance of `C`.

**Expected outcome:** Both `A` and `B` exit their root states (their `on exit` actions fire).  The context is physically removed in phase 2.  The recorder flags are set correctly.

---

## Group 3 — Deferred Destructive Operations (Step 1.9)

### T10 — RoleUnbinding deferred until phase-1 base case

**Path exercised:** Step 1.9 — `RoleUnbinding` is accumulated during an `on entry` action but executed only after all recursive phase-1 iterations are complete.

**Model setup:** Context type `C` with roles `Filler`, `Filled` (filled by `Filler`), and `Result`.  The root state of `C` has an `on entry` action: `unbind Filled`.  The root state of `Filled` has an `on exit` action: set `Result.WasUnbound = true`.

**Initial state:** Instances of `C`, `Filler`, and `Filled` exist.  `Filled` is bound to `Filler`.

**Triggering action:** The user creates a fresh instance of `C` (or modifies a property that triggers the state; the point is that the root state of `C` is entered for the first time).

**Expected outcome:** The `unbind` happens *after* any constructive `on entry` actions on newly created resources complete.  `Filled.WasUnbound` is `true`.  The order of observable events is: all constructive steps first, then the unbind.

---

### T11 — ExecuteDestructiveEffect deferred until phase-1 base case

**Path exercised:** Step 1.9 — an `ExecuteDestructiveEffect` scheduled assignment is deferred.

**Model setup:** Context type `C` with a role `R` and an external function registered as a destructive effect (for example, a call that writes a sentinel value to an external store).  The root state of `R` schedules this effect via an `on entry` action.

**Initial state:** An instance of `C` exists.

**Triggering action:** The user creates an instance of `R`.

**Expected outcome:** The destructive effect is called exactly once, after any constructive actions have completed.  No second invocation occurs.

---

## Group 4 — Context Removal Path

### T12 — Full context removal lifecycle

**Path exercised:** Steps 1.1 (untouchable marking), 1.6 (exit context states), 2.4 (physical removal), 2.5 (if postponed evaluations are generated).

**Model setup:** Context type `C` with roles `Member` and `Counter` (in a parent context).  The root state of `C` has an `on exit` action: decrement `Counter.Count`.  A state condition on the parent context checks `not exists C` and sets a flag when true.

**Initial state:** An instance of `C` exists inside a parent context that has an active state with an active subscriber to the `not exists C` condition.

**Triggering action:** The user removes the `C` instance.

**Expected outcome:**
1. `C` is added to `untouchableContexts`.
2. The exit action fires and `Counter.Count` is decremented.
3. The parent-context state that checks `not exists C` is evaluated *after* physical removal of `C` (via `postponedStateEvaluations`), and finds the condition true.
4. `C` is physically removed.

---

## Group 5 — Phase 2: State Re-evaluation via InvertedQueryResults

### T13 — Property change causes state entry in phase 2

**Path exercised:** Step 2.1 — `invertedQueryResults` from a property change are processed; state condition becomes true; `enteringState` is called.

**Model setup:** Context type `C` with role `R` and a Boolean property `Trigger` on `R`.  A context state of `C` has condition `R >> Trigger` (true when `Trigger` is set).  The `on entry` action of that state sets `Result = true` on another role `Outcome`.

**Initial state:** An instance of `C` and `R` exist.  `Trigger` is not set.  The state is not active.

**Triggering action:** The user sets `R.Trigger = true`.

**Expected outcome:** Phase 2 picks up the inverted-query result for `Trigger`, evaluates the state, finds it true, and calls `enteringState`.  `Outcome.Result` is set to `true`.

---

### T14 — Property change causes state exit in phase 2

**Path exercised:** Step 2.1 — state condition becomes false; `exitingState` is called.

**Model setup:** Same as T13, but the state is already active when the action triggers.

**Initial state:** An instance of `C` and `R` exist.  `Trigger` is `true`.  The state is active.  `Outcome.Result` is `true`.

**Triggering action:** The user sets `R.Trigger = false`.

**Expected outcome:** Phase 2 evaluates the state, finds it false, calls `exitingState`.  The `on exit` action sets `Outcome.Result = false`.

---

### T15 — Multiple inverted-query results deduplicated in phase 2

**Path exercised:** Step 2.1 — `dedupeStateEvaluations` prevents a `(stateId, instanceId)` pair from being evaluated more than once even when multiple `invertedQueryResults` point to the same state.

**Model setup:** Context type `C` with two roles `R1` and `R2`, both of which appear in the condition of the same context state (e.g. `exists R1 and exists R2`).  A counter on a recorder role tracks how many times the `on entry` action executes.

**Initial state:** An instance of `C` exists.  Neither `R1` nor `R2` is present.

**Triggering action:** The user creates both `R1` and `R2` in a single transaction (e.g. via an `on entry` action that creates both).

**Expected outcome:** Both `invertedQueryResults` accumulate (one for `R1`, one for `R2`), but the state is evaluated exactly once (the counter on the recorder is incremented exactly once).

---

### T16 — State evaluation in phase 2 creates a new context → phase 1 re-runs (Step 2.2)

**Path exercised:** Step 2.1 produces new `createdContexts`; Step 2.2 check is true; control returns to phase 1.

**Model setup:** Context type `Outer` with a role `Signal` and a Boolean property `Go` on `Signal`.  A state condition on `Outer` checks `Signal >> Go`.  The `on entry` action creates a new context `Inner`.  `Inner`'s root-state `on entry` action sets `Inner.Done = true`.

**Initial state:** An instance of `Outer` and `Signal` exist.  `Go = false`.  No `Inner` instances exist.

**Triggering action:** The user sets `Signal.Go = true`.

**Expected outcome:** Phase 2 evaluates the `Outer` state → condition is true → `enteringState` → `Inner` is created (added to `createdContexts`) → Step 2.2 triggers phase-1 re-run → `Inner`'s root-state `on entry` fires → `Inner.Done = true`.

---

## Group 6 — Untouchable Resources and Postponed State Evaluations

### T17 — State condition depending on an untouchable role deferred to postponedStateEvaluations

**Path exercised:** Step 2.1 — state condition is *undetermined* because the role it queries is in `untouchableRoles`; the evaluation is added to `postponedStateEvaluations`.  Step 2.5 — after physical removal (step 2.4), the postponed evaluation runs and now resolves.

**Model setup:** Context type `C` with roles `Guard` and `Watcher`.  A state condition on `C` checks `not exists Guard`.  The `on entry` action of that state sets `Watcher.Safe = true`.

**Initial state:** `C`, `Guard`, and `Watcher` exist.  The state with condition `not exists Guard` is *not* active (because `Guard` exists).

**Triggering action:** The user removes `Guard`.

**Expected outcome:**
1. `Guard` is added to `untouchableRoles`.
2. During phase 2, the state evaluation for `not exists Guard` is undetermined (the runtime cannot safely evaluate against an untouchable resource) and is added to `postponedStateEvaluations`.
3. In step 2.4, `Guard` is physically removed.
4. In step 2.5, the postponed evaluation runs and finds `not exists Guard` is now true.
5. `enteringState` fires; `Watcher.Safe = true`.

---

### T18 — State condition depending on an untouchable context deferred to postponedStateEvaluations

**Path exercised:** Same path as T17 but for a context-level state condition and an untouchable context.

**Model setup:** Parent context `P` with a context role `Child` (filled by context type `C`) and a role `Observer`.  A state condition on `P` checks `not exists Child`.  The `on entry` action sets `Observer.ChildGone = true`.

**Initial state:** `P`, `Child` (an instance of `C`), and `Observer` all exist.  The state is not active.

**Triggering action:** The user removes `C` (which marks `C` as an untouchable context).

**Expected outcome:** Same deferral pattern as T17; after `C` is physically removed, the state fires and `Observer.ChildGone = true`.

---

## Group 7 — The `executedStateKeys` Guard

### T19 — Same state is not entered twice in a single transaction

**Path exercised:** The `executedStateKeys` set prevents the `(stateId, instanceId)` pair from being processed more than once, even when multiple code paths converge on the same state.

**Model setup:** Context type `C` with a role `R`.  The root state of `C` has an `on entry` action that sets a counter property `EntryCount` (incrementing by 1) on a recorder role.

**Initial state:** An instance of `C` exists and is *not* yet in its root state (first time).

**Triggering action:** The user performs an action that triggers `enteringState` for `C`'s root state both via phase-1 (because `C` appears in `createdContexts`) and again via phase-2 inverted-query results.

**Expected outcome:** `EntryCount` equals exactly 1.  The `executedStateKeys` guard has prevented a double execution.

---

### T20 — State re-entry after role exit and re-create in the same transaction

**Path exercised:** Step 1.5 clears the `executedStateKeys` entries for the exiting role; a later phase-1 iteration can therefore re-enter the same root state for a newly recreated instance of the same type.

**Model setup:** Context type `C` with role `R` and a recorder property `Cycles` on an `Observer` role.  The root state of `R` has an `on entry` action that increments `Cycles` and an `on exit` action that decrements `Cycles`.

**Initial state:** One instance of `R` exists with its root state active.  `Cycles = 1`.

**Triggering action:** The user removes the instance of `R` and immediately (within the same explicit action or via an automatic reaction) creates a new instance of `R`.

**Expected outcome:** The exit fires (`Cycles = 0`), the state keys for the old `R` are cleared, the new `R` enters its root state (`Cycles = 1` again).  Final value: `Cycles = 1`.

---

## Group 8 — CWH / Monotone-Simulation Tension

These scripts directly test the two conflicting design goals.

### T21 — CWH: `not exists X` evaluated correctly when X is created in the same transaction

**Path exercised:** Step 2.1 re-evaluates state conditions in phase 2 after all deltas from the initial action have been applied.  A state that checks `not exists X` must see the *current* world, not a stale snapshot from before X was created.

**Background:** As documented in Design Consideration 3 of `transaction-execution.md`, when multiple deltas arrive together, `invertedQueryResults` from each delta are not evaluated until phase 2, so phase 2 is the first point where the full picture is visible.

**Model setup:** Context type `C` with role `X` and a role `Watcher`.  A state condition on `C` checks `not exists X`.  The `on entry` action of that state sets `Watcher.NoX = true`.

**Initial state:** No instance of `C` or `X`.

**Triggering action:** Create an instance of `C` *and* an instance of `X` in a single transaction (e.g. via an `on entry` action on `C` that immediately creates `X`).

**Expected outcome:** When phase 2 evaluates the `not exists X` condition, `X` already exists (because both `C`'s state entry and `X`'s creation happened in the same transaction).  The condition is false; `Watcher.NoX` is *not* set.  This verifies that the CWH evaluation is deferred to the point where the full world is visible.

---

### T22 — CWH: `not exists X` correctly enters state when X is absent

**Path exercised:** Same state condition as T21, but X is not created.

**Model setup and initial state:** Same as T21, but the `on entry` action of `C` does not create `X`.

**Triggering action:** Create an instance of `C`.

**Expected outcome:** Phase 2 evaluates `not exists X` and finds it true.  `Watcher.NoX = true`.  This is the complementary case to T21.

---

### T23 — Monotone simulation: destructive op follows constructive ops

**Path exercised:** Step 1.9 — a `RoleUnbinding` is deferred; before it executes, all constructive actions (role and context creation) from the same transaction complete.

**Model setup:** Context type `C` with roles `A`, `B` (filled by `A`), and `Log`.  An `on entry` state action on `C` creates `B`, fills it with `A`, then schedules an unbind of `B`.  Another `on entry` action on the newly created `B` sets `Log.Constructed = true`.

**Initial state:** An instance of `C` and `A` exist.  `B` does not yet exist.

**Triggering action:** Trigger the root state of `C` for the first time (e.g. by setting a property).

**Expected outcome:** `B` is created and its own `on entry` fires (`Log.Constructed = true`) *before* the unbind executes (step 1.9).  This demonstrates that destructive operations are deferred past all constructive ones.

---

## Group 9 — Peer Transaction Path (Non-Sharing / runSharing)

### T24 — Peer transaction: own-user reaction distributed via embedded sharing transaction

**Path exercised:** Path B entry → `executeTransaction'` → phase 1 with `share = false` → `runSharing` spawns embedded sharing transaction for own-user `on entry` reaction → embedded transaction distributes deltas to own user's peers.

**Setup requirements:** Two PDR instances, Alice and Bob.  Both have peer relationships.

**Model setup:** A shared context type `Chat` with roles `Initiator` (Alice) and `Participant` (Bob).  A state of `Chat` (active for both) has an `on entry` action for Bob: set `Bob's AckProperty = true` on `Participant`.

**Initial state:** Alice and Bob are both participants in a `Chat` instance.  Bob's `AckProperty` is `false`.  The relevant state is not yet active.

**Triggering action:** Alice creates a delta (e.g. sets a property) that satisfies the state condition for the `Chat` state.  This delta arrives at Bob's PDR as an incoming peer transaction.

**Expected outcome:**
1. Bob's PDR processes Alice's delta in a non-sharing transaction.
2. The state evaluation runs inside `runSharing`, spawning an embedded sharing transaction attributed to Bob.
3. Bob's `on entry` action fires and sets `AckProperty = true`.
4. The embedded sharing transaction distributes Bob's delta (the `AckProperty` change) to Alice.
5. Alice's PDR ultimately sees `Bob.AckProperty = true`.

---

### T25 — Peer transaction: own-user reaction triggers further state cascade

**Path exercised:** Embedded sharing transaction (from `runSharing`) itself triggers `createdContexts` → the embedded sharing transaction's own phase 1 recurses.

**Setup requirements:** Same two-PDR setup as T24.

**Model setup:** Same `Chat` context.  Bob's `on entry` action creates a new context `Acknowledgement`.  `Acknowledgement`'s root state `on entry` sets `Acknowledgement.Registered = true`.

**Triggering action:** Same as T24 — Alice's delta satisfies the state condition.

**Expected outcome:** The embedded sharing transaction creates `Acknowledgement`, recurses through phase 1, fires the `Acknowledgement` root-state entry, sets `Registered = true`, and distributes both the `Acknowledgement` creation and the property delta to Alice.

---

## Group 10 — Transaction Serialisation

### T26 — Concurrent transactions are serialised

**Path exercised:** The `transactionFlag` AVar (step 0 of initialization) ensures that a second transaction does not begin until the first has completed.

**Setup:** Single PDR.

**Scenario:** Two user actions are fired in rapid succession (e.g. within the same test harness step) before the first transaction has finished.

**Expected outcome:** The second transaction blocks on the flag until the first has raised it.  Both transactions complete with correct results; no interleaving of their side-effects occurs.  Observable invariant: a "started" log entry for transaction N+1 never appears before the "ended" log entry for transaction N.

---

## Summary Table

| ID  | Phase tested | Key mechanism under test |
|-----|-------------|--------------------------|
| T01 | 1 (no recursion) | Minimal context creation, state entry |
| T02 | 1 (no recursion) | Minimal role creation, state entry |
| T03 | 1 (no recursion) | Context state `on entry` property assignment |
| T04 | 1 (no recursion) | Role state `on entry` property assignment |
| T05 | 1 (no recursion) | Role state `on exit` |
| T06 | 1 (recursion) | `on entry` creates context → phase-1 recurse |
| T07 | 1 (recursion) | `on entry` creates role → phase-1 recurse |
| T08 | 1 (recursion) | `on exit` creates role → phase-1 recurse |
| T09 | 1 (recursion) | Context removal cascades through roles |
| T10 | 1.9 | `RoleUnbinding` deferred to phase-1 base case |
| T11 | 1.9 | `ExecuteDestructiveEffect` deferred to phase-1 base case |
| T12 | 2.4 | Full context removal lifecycle |
| T13 | 2.1 | Property change → state entry in phase 2 |
| T14 | 2.1 | Property change → state exit in phase 2 |
| T15 | 2.1 | Multiple `invertedQueryResults` deduplicated |
| T16 | 2.2 | Phase-2 state entry creates context → phase-1 re-run |
| T17 | 2.5 | Untouchable role → `postponedStateEvaluations` |
| T18 | 2.5 | Untouchable context → `postponedStateEvaluations` |
| T19 | executedStateKeys | Same state not entered twice |
| T20 | executedStateKeys | State re-entry after role exit+recreate |
| T21 | CWH | `not exists X` blocked when X created in same transaction |
| T22 | CWH | `not exists X` enters state when X absent |
| T23 | monotone | Destructive op deferred past constructive ops |
| T24 | runSharing | Peer transaction → own-user reaction distributed |
| T25 | runSharing | Own-user reaction triggers further cascade |
| T26 | serialisation | Concurrent transactions serialised by flag |
