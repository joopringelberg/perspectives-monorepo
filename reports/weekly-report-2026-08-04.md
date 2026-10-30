# Weekly Progress Report - 2026-08-04

## Executive Summary

This week delivered one correctness fix in the query inversion engine and a substantial documentation artefact for the transaction execution algorithm. The query inversion bug fix closes a real notification gap, while the test script catalogue lays the groundwork for systematic testing of the most complex algorithm in the PDR.

---

### 🎯 Top Achievements (Max 5)

- **Fixed query inversion for enumerated properties on filler chains (#478)**: When a user perspective referenced an enumerated property that lived on a filler role, severing the fill relation silently failed to notify the affected user. The fix mirrors the already-working calculated-property branch, making filler-chain inversion consistent across both property kinds.

- **Produced a 26-script test catalogue for `RunMonadPerspectivesTransaction` (#474)**: The transaction execution algorithm had no systematic test coverage; this catalogue covers all major call-tree paths — single-pass phases, recursion, deferral, CWH/monotone tension, peer transactions, and serialisation. These prose scripts are the specification from which ARC model definitions and PureScript test cases will be built.

---

### 🐛 Key Bugs Fixed (Max 3)

- **Severing a fill relation did not notify users of enumerated properties on filler roles (#478, fixes #477)**: The `ENP` branch in `storeInvertedQueries` was a no-op (`pure unit`), so RTFilledKey/RTFillerKey inversions were never stored for enumerated properties accessed through a filler chain. The fix constructs the missing virtual query and calls `invert_`, restoring correct change-notification behaviour.

---

### 📊 Impact (2-3 sentences total)

The query inversion fix restores correct reactive behaviour for a class of perspective definitions that is common in real models — any user who can see a property on a filler role will now receive updates when the fill relation is severed. The transaction execution test catalogue is a force-multiplier: 26 targeted scripts reduce the risk of silent regressions in the PDR's most complex algorithm and give future contributors a clear map of what to test.

---

### ⚠️ Concerns (If any, max 3)

- **Test scripts remain prose, not yet executable**: The 26 scripts from #474 are human-readable specifications; no ARC models or PureScript test cases have been created yet. Until they are, the algorithm's correctness still relies solely on manual review.

- **Enumerated-property filler inversion lacks dedicated regression tests**: The #478 fix is validated by the existing model compilation tests but has no Layer4 or StateTestModel entry specifically targeting the repaired path. A future regression could go unnoticed without explicit coverage.

---

### 🎯 Focus for Next Week (Max 3)

- **Convert T01–T05 (single-pass phase 1) test scripts into executable ARC models and PureScript tests**: Starting with the simplest group from the catalogue will validate the scaffolding and produce the first automated regression coverage for `runMonadPerspectivesTransaction`.

- **Add a Layer4 test for enumerated-property filler-chain inversion**: A targeted test modelling the `TestRole7↔Filler1` scenario from #478 will lock in the fix and prevent silent regression.

- **Continue language feature work or address any follow-up issues from the filler-removal redesign (#472)**: If model authors report migration issues with the `unbind`/`unbind_` removal, prioritise unblocking them.

---

**Report generated:** 2026-08-04
**Reporting period:** Week ending 4 August 2026
**Merged PRs analyzed:** #474, #478, #476
**Word count:** ~430 words
