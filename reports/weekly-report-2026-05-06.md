# Weekly Progress Report - 2026-05-06

## Executive Summary

This week delivered significant advancements across GUI polish, query language capabilities, and runtime reliability. The MyContexts user interface received a comprehensive visual refresh, while the PDR gained powerful new query filtering and screen definition features. A critical bug fix resolved state synchronization failures in compound role types.

---

### 🎯 Top Achievements (Max 5)

- **Complete GUI visual refresh**: Implemented warm copper/parchment color scheme replacing default blue tones, featuring resizable three-column desktop layout with drag handles and keyboard navigation. Added lighter palette refinements (#faf7f0 cream background, #c8b89e tan accordions) with reduced form density and improved accessibility.

- **Property reference values in screens**: Added `fillproperty ... from ...` syntax enabling model authors to bind candidate values for property inputs via query expressions. Screen widgets now support per-property dropdown options beyond facet enumerations, with full client-side integration through SmartFieldControl.

- **Advanced type filtering in queries**: Introduced `typeFilter ... with ...` query step combining compile-time type narrowing with runtime instance filtering. Supports union/intersection type expressions with proper domain mismatch diagnostics and simplified ADT range resolution.

- **Fixed critical state synchronization bug**: Resolved empty inverted query keys for compound `filledBy` clauses (e.g., `SUM [PerspectivesUsers, NonPerspectivesUsers]`). State conditions like `exists LastName` now correctly re-trigger on property changes, not just role creation.

- **Enhanced UI interactions**: Context actions moved to dedicated navbar dropdown (⚡ icon), WiderContexts converted to accordion, table captions removed, and "You are [role] in [context]" prefix added to wide-screen titles. Focus rings on click eliminated for cleaner interaction patterns.

---

### 🐛 Key Bugs Fixed (Max 3)

- **Empty inverted query keys for SUM types**: `typeLevelKey*` functions incorrectly used `computeCollection singleton` with Heyting algebra intersection (`disj = intersect`), producing empty key arrays for compound role types. Replaced with `allLeavesInADT` using standard Foldable union semantics, fixing all five key types (RTPropertyKey, RTFilledKey, RTFillerKey, RTRoleKey, RTContextKey).

- **State calculations never re-triggered**: Inverted queries for property-dependent states stored with empty keys, so `aisInPropertyDelta` found no calculations to execute on property updates. The fix ensures state conditions evaluate both on role creation and subsequent property changes.

- **Missing type coverage in query dependencies**: The bug affected 10+ queries in System model across multiple key types, causing silent failures where state transitions dependent on property values would not fire when those properties changed.

---

### 📊 Impact (2-3 sentences total)

The GUI refresh transforms MyContexts from generic Bootstrap styling to a cohesive, accessible visual identity while improving usability through resizable layouts and streamlined navigation. The `fillproperty` and `typeFilter` features expand ARC's expressive power for defining sophisticated data-driven interfaces and type-safe queries. The inverted query bug fix eliminates a critical class of state synchronization failures affecting reactive calculations throughout the runtime.

---

### ⚠️ Concerns (If any, max 3)

- **Breaking change risk**: The GUI color scheme and layout changes are pervasive (13 files in PR #380, 10 files in PR #373); comprehensive manual testing recommended before user deployment to catch edge cases in theme variable cascading or column resize edge conditions.

- **Query compilation complexity**: The `typeFilter` implementation adds significant ADT intersection/simplification logic in `compileBinaryStep`. While test coverage validates core cases, compound type expressions with deep aspect hierarchies may expose edge cases in production models.

---

### 🎯 Focus for Next Week (Max 3)

- **Validate GUI changes in production contexts**: Conduct thorough testing of color scheme, resizable columns, and navigation changes across different screen sizes, browsers, and user workflows to ensure accessibility compliance and identify any visual regressions.

- **Extend property reference value coverage**: Consider adding `fillproperty` support to additional widget types beyond detail/form/table/master, and evaluate performance implications when candidate queries execute on large datasets.

- **Monitor inverted query performance**: Track whether the expanded key sets from `allLeavesInADT` (union vs intersection) impact query indexing performance or memory usage in models with deep role hierarchies and extensive state calculations.

---

**Report generated:** 2026-05-06
**Reporting period:** Week ending 6 May 2026
**Merged PRs analyzed:** #380, #377, #373, #362, #358
**Word count:** ~495 words
