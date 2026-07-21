# Weekly Progress Report - 2026-07-14

## Executive Summary

This week's key delivery was a correctness fix for typed comparison semantics in the query engine, closing a class of silent equality bugs caused by string-based operand evaluation. Test coverage received significant attention: Layer4 model-driven scenarios were expanded across a broad set of query-step forms, and the `perspectives.domains` regression compilation test was merged to provide ongoing protection against ARC compilation regressions.

---

### 🎯 Top Achievements (Max 5)

- **Typed comparison semantics in the query engine (#460)**: Comparisons such as `1 + 1 = 2` previously failed at runtime because operands were compared as their string representations (`"2.0"` vs `"2"`). The fix introduces a shared range-aware comparison path that parses operands to their `VDOM` type before applying equality and ordering operators across numeric, boolean, date, and duration ranges.

- **Expanded Layer4 model-based test coverage (#458)**: `Test.Layer4` previously exercised only three query-step forms; this expansion adds dedicated `case Test_*` scenarios for `DataTypeGetter`, `TypeGetter`, `DataTypeGetterWithParameter`, `UnaryCombinator`, `BinaryCombinator`, and sequence composition (`ComposeSequenceF`). Each case follows the established scaffold pattern, giving comprehensive coverage of what `compileStep`/`compileSimpleStep` actually compile today.

- **ARC model compilation regression test (#452)**: A new test fetches all published models from `perspectives.domains`, sorts them topologically by declared dependencies, and compiles each through all three ARC phases — failing the suite if any parse or compile error is detected. The test is marked `testOnly` (network-dependent) and wired into the Layer2 runner.

---

### 🐛 Key Bugs Fixed (Max 3)

- **Query equality and ordering broken for numeric and typed values (#460)**: Equality checks like `=` and ordering checks like `<` operated on string representations of evaluated operands, causing `"2.0" = "2"` to return `false` even though the underlying numeric value was equal. The fix aligns the compiled and interpreted query paths on a shared typed comparison helper so that string-like ranges retain string semantics while all other range types use proper typed operators.

---

### 📊 Impact (2-3 sentences total)

The typed comparison fix (#460) closes a category of silent, hard-to-diagnose evaluation errors that would have produced wrong state transitions and incorrect property values in any model using arithmetic results in equality conditions. The Layer4 expansion (#458) substantially raises confidence in the query compilation pipeline by covering query-step forms that were previously entirely unverified at the model level. Together these changes improve both runtime correctness and the safety net available to catch regressions going forward.

---

### ⚠️ Concerns (If any, max 3)

- **Layer4 test model dependencies**: Several Layer4 test cases reference model-level constructs that must remain consistent with the ARC model file; any future model refactoring could silently break test coverage without compiler errors.

- **Regression test requires live network access**: The `perspectives.domains` compilation test is skipped in offline CI, meaning it provides no protection in automated pull-request pipelines until a cached-snapshot strategy is implemented.

---

### 🎯 Focus for Next Week (Max 3)

- **Promote the ARC regression test to CI**: Evaluate a cached model snapshot approach so the `perspectives.domains` compilation test runs on every PR and provides continuous protection against compilation regressions.

- **Validate typed comparison fix across all range types in Layer4**: Add or confirm Layer4 model cases that exercise equality and ordering on date, duration, and boolean ranges to ensure the full typed comparison path is covered end-to-end.

- **Finalise and publish the Layer4 single-PDR test model**: Replace any remaining placeholder name constants in `Layer4.purs` with the published model so the expanded test cases can execute against real runtime behaviour.

---

**Report generated:** 2026-07-14
**Reporting period:** Week ending 14 July 2026
**Merged PRs analyzed:** #460, #458, #452
**Word count:** ~490 words
