# Weekly Progress Report - 2026-06-16

## Executive Summary

This week delivered a landmark milestone in test infrastructure: a full in-process synchronisation test harness enabling two PDR instances to exchange transactions without a live RabbitMQ broker. Alongside that, the inverted query subsystem received formal correctness analysis confirming all five query categories are correctly implemented, and several code quality improvements landed in the interpreter and warning subsystems.

---

### 🎯 Top Achievements (Max 5)

- **Layer 3 in-process sync test infrastructure** (#339): An injectable AMQP transport stub and the `withTwoPDRs`/`connectPDRs` scaffolding now allow two PDR instances to run in the same Node.js process and exchange real transactions over an in-memory pub/sub bus. This removes the RabbitMQ dependency from integration tests and unblocks a full suite of multi-peer synchronisation tests.

- **Formal correctness analysis of the inverted query subsystem** (#419): All five inverted query categories (RTPropertyKey, RTContextKey, RTRoleKey, RTFillerKey, RTFilledKey) were formally verified for key alignment, domain/range consistency, and correct filter direction. The analysis confirmed the subsystem is correctly implemented and was documented in `query-inversion.md` §3.

- **Centralized transaction warning humanization** (#427): Warning constructors used during transaction execution were updated to carry typed Perspectives identifiers instead of pre-rendered strings, and all humanization logic was consolidated into `Perspectives.Error.Pretty`. This eliminates duplicated `toReadable` calls at call sites and ensures consistent, readable log output.

- **Query interpreter coverage and exhaustivity fix** (#423): Missing handlers for `MeF`, `RoleTypeConstant`, `ContextTypeConstant`, `PublicRole`, `PublicContext`, `RegExMatch`, `IsInStateF`, and `BQD FillsF` were added to `interpretSQD`/`interpretBQD`, closing the gap with compiler coverage. The `UnsafeCompiler` exhaustivity warning was resolved by splitting the oversized `compileFunction_` into four dedicated `Partial` subfunctions.

- **Role-binding inverted-query call-tree analysis** (#421): The runtime path through `usersWithPerspectiveOnRoleBinding` was documented end-to-end using the `Invitation$Inviter`/`Invitee` case as a concrete example, clarifying the compile-time/runtime key invariant for `RTFillerKey`/`RTFilledKey` queries and narrowing the search space for the open Invitation delta delivery issue.

---

### 🐛 Key Bugs Fixed (Max 3)

- **Floating row context menu clipped at viewport bottom** (#425): The `RowContextMenu` component always anchored its top edge at the click coordinates, making lower items unreachable near the viewport bottom. A `useLayoutEffect` now measures the rendered menu height and flips it above the trigger point when it would overflow, with a `visibility: hidden` guard preventing any positional flash.

- **Interpreter coverage gaps** (#423): Several query function constructors reachable by the compiler had no corresponding runtime handlers in `queryFunctionInterpreter.purs`, meaning queries involving those constructors would fail silently at runtime. All missing cases are now handled.

---

### 📊 Impact (2-3 sentences total)

The Layer 3 test scaffold is the most significant infrastructure investment in months: it enables deterministic, broker-free multi-peer integration tests and creates a repeatable path to validating the entire synchronisation pipeline. Formal verification of the inverted query subsystem removes a long-standing uncertainty about correctness and gives the team a reference document for future maintenance. The interpreter and warning improvements reduce hidden runtime risk and maintenance cost in the query evaluation and transaction logging paths.

---

### ⚠️ Concerns (If any, max 3)

- **Layer 3 sync tests still disabled**: The `SetProperty → GetProperty` test suite is `suiteSkip`-ped pending in-memory PouchDB wiring and system-model loading (#339). Until those prerequisites are met, the new infrastructure is untested end-to-end.

- **Invitation delta delivery issue unresolved**: The analysis in #421 confirmed the runtime binding path is internally consistent, but the root cause of `Invitation$Inviter` not receiving deltas when `Invitation$Invitee` is created remains open. The next investigation step targets stored inverted queries and backward/state filtering results.

---

### 🎯 Focus for Next Week (Max 3)

- **Enable Layer 3 sync tests**: Wire in-memory PouchDB and load the system model in the test scaffold so the `SetProperty → GetProperty` suite can move from `suiteSkip` to an active, passing test.

- **Resolve Invitation synchronisation bug**: Use the documented call-tree and invariant analysis to inspect stored inverted queries and state-filter results for the `Invitation$Inviter` perspective, and fix any misalignment found.

- **Validate warning humanization in production logs**: Confirm that the typed warning constructors and centralized humanization produce correct, readable output across all transaction execution paths in real multi-user scenarios.

---

**Report generated:** 2026-06-16
**Reporting period:** Week ending 16 June 2026
**Merged PRs analyzed:** #427, #425, #423, #421, #419, #339
**Word count:** ~490 words
