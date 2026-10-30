# Weekly Progress Report - 2026-05-12

## Executive Summary

This week delivered critical runtime fixes for calculated user roles and contextual bindings, plus enhanced role-filling workflows. Three major bugs resolved: query inversion crashes, type narrowing errors, and clipboard semantics. Users now see live transaction processing status and gain explicit control over role-filling type resolution.

---

### 🎯 Top Achievements (Max 5)

- **Explicit role-filling semantics**: Split clipboard role binding into provided-type vs required-type modes with dynamic context menu labels. Users now control whether to preserve offered instance type or resolve through binding chain.

- **Live transaction status**: Real-time pending-post count during PDR startup and message processing (e.g., "3 nog niet verwerkte gegevensberichten"). Eliminates confusion during lengthy transaction queues.

- **Fixed calculated-role type narrowing**: Resolved `RoleDoesNotBind` errors for `currentactor` bindings to calculated user roles. Compiler now preserves full `TypeTimeOnlyCalculatedRole` information.

- **Fixed filter-based query inversion**: Corrected runtime crash where filter-based Calculated User queries returned property values instead of role instances. State triggers now work for property-dependent user roles.

---

### 🐛 Key Bugs Fixed (Max 3)

- **Property values instead of role instances**: Filter queries stored `PropertyGetter` in forward slot, causing crashes. Fixed by storing `FilterF` expression in both slots for Calculated User detection.

- **Type narrowing lost calculated roles**: Contextual bindings used first-alternative ADT selection, losing calculated domains. New `makeTypeTimeOnlyRoleTypeStep` preserves full `RoleType` information.

- **Clipboard ignored instance types**: Fill operations always resolved to required type (often `PerspectivesUsers`). New `FillBindingMode` enables provided-type preservation.

---

### 📊 Impact (2-3 sentences total)

Query inversion and type narrowing fixes eliminate runtime crashes and compile-time rejections affecting calculated user roles throughout ARC models. Explicit fill semantics and live status feedback improve user experience—clipboard operations behave intuitively and users understand system state during processing. These changes strengthen runtime reliability and user confidence.

---

### ⚠️ Concerns (If any, max 3)

- **Filter inversion limitations**: Fix doesn't cover property-change triggers when determining property lives on filler role. May miss state updates in filler-property-driven calculations.

- **Dual fill-mode complexity**: Two fill options in context menus add cognitive load. Monitor for user confusion about provided vs required type resolution.

---

### 🎯 Focus for Next Week (Max 3)

- **Validate fixes in production**: Test type narrowing and query inversion against complex models with deep hierarchies, nested filters, and property-dependent states.

- **Monitor fill-mode adoption**: Track user behavior and gather feedback on whether the provided vs required distinction is clear.

- **Extend filler-property coverage**: Evaluate real-world impact of filter inversion limitation and design solution if needed.

---

**Report generated:** 2026-05-12
**Reporting period:** Week ending 12 May 2026
**Merged PRs analyzed:** #392, #390, #388, #386, #382, #380
**Word count:** ~485 words
