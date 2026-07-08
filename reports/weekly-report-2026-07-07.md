# Weekly Progress Report - 2026-07-07

## Executive Summary

This week delivered two important synchronisation correctness fixes — closing silent data-delivery failures for calculated properties and over-aggressive state-evaluation loop prevention — alongside a major architectural refactor promoting `MonadPerspectives` to a newtype. Test infrastructure continued to expand with a new single-PDR test rig (Layer4) and a network-based regression suite for ARC model compilation.

---

### 🎯 Top Achievements (Max 5)

- **MonadPerspectives promoted to newtype with capability class (#450)**: `MonadPerspectives` was elevated from a bare type alias to a newtype with a new `MonadPerspectivesClass` capability class, enabling polymorphic functions with a tighter monad contract. The change cascaded across ~20 files and introduces 13 derived newtype instances plus a `runMonadPerspectives` unwrapper.

- **Layer4 single-PDR model-based test rig (#447)**: A new `Layer4.purs` scaffold enables model-driven tests entirely within a single PDR instance, complementing the two-PDR Layer3 infrastructure. Scripts `build:layer4` and `test:layer4` were added to `package.json` following the existing Layer3 pattern.

- **Regression test for ARC model compilation from perspectives.domains (#452)**: A new test fetches all published models from `perspectives.domains`, sorts them topologically by declared dependencies, and compiles each through all three ARC phases — failing the suite if any parse or compile error is detected. The test is marked `testOnly` (network-dependent) and is wired into the Layer2 runner.

---

### 🐛 Key Bugs Fixed (Max 3)

- **Synchronisation silent failure for calculated properties (#444)**: When a peer's perspective included a calculated property (e.g. `P = context >> extern >> Q`), changes to the underlying enumerated property `Q` did not notify the peer. The fix iterates over all `CP` entries in `statesPerProperty`, inverts the calculation, composes the backwards path with the perspective's path, and stores the combined `QueryWithAKink` under the leaf enumerated property.

- **State evaluation loop guard too coarse (#449)**: `computeStateEvaluations` always resolved to the root state, making the `executedStateKeys` guard (`stateId × instanceId`) block all state evaluations for a resource within a single transaction. Threading the exact `StateIdentifier` from each `InvertedQueryResult` allows multiple distinct states of the same resource to be independently evaluated.

---

### 📊 Impact (2-3 sentences total)

The two synchronisation fixes (#444, #449) address correctness failures that cause silent data-delivery gaps in production multi-user scenarios, making the runtime substantially more reliable. The `MonadPerspectives` newtype refactor (#450) tightens the core monad's type contract and unblocks future polymorphic APIs, at the cost of broad but contained cascading changes. The expanded test infrastructure (Layer4 + regression suite) meaningfully raises confidence in both single-PDR model logic and end-to-end ARC compilation correctness.

---

### ⚠️ Concerns (If any, max 3)

- **MonadPerspectives newtype scope**: The promotion affected ~20 files with three distinct coercion fix patterns; while the build compiles cleanly, the breadth increases the risk of subtle behavioral regressions that only surface at runtime.

- **Layer4 model constants are placeholders**: `testModel`, `indexedTestContext`, and related constants are aligned with `SynchronisationTestModel` naming but must be updated once the target single-PDR model is published — until then Layer4 cannot execute real test cases.

---

### 🎯 Focus for Next Week (Max 3)

- **Publish and wire the Layer4 test model**: Replace placeholder name constants in `Layer4.purs` with a published single-PDR test model so the new infrastructure can run real model-based tests.

- **Validate MonadPerspectives newtype in integration scenarios**: Run Layer3 and Layer4 suites with the newtype build to confirm that coercion changes across ~20 files introduced no behavioral regressions in multi-PDR synchronisation.

- **Promote ARC regression test to CI**: Evaluate options for making the `perspectives.domains` compilation test runnable in CI (e.g. cached model snapshot) so it provides continuous protection against compilation regressions.

---

**Report generated:** 2026-07-07
**Reporting period:** Week ending 7 July 2026
**Merged PRs analyzed:** #444, #447, #449, #450, #452
**Word count:** ~490 words
