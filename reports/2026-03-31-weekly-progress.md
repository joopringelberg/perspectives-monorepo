# Weekly Progress Report — 2026-03-31

> **Audience:** Management / Stakeholders
> **Period:** Week ending 31 March 2026
> **Scope:** 7 merged pull requests (PRs #266, #281, #282, #284, #286, #290, #292)

---

## 1. Key Achievements

| Theme | Summary |
|---|---|
| **Type System Integrity** | Resolved a critical binding-chain resolution issue preventing SQL-based query evaluation in the TCP server by ensuring roles are filled with exact type matches rather than indirect bindings. |
| **Distributed Sync Correctness** | Fixed two silent data loss scenarios: peers missing updates when filter criteria become valid, and incorrect serialization attempts of public resources. |
| **TCP Server Hardening** | Implemented cascade-delete on context removal, property-based column lookup for flattened roles, and enhanced operational logging — closing three data integrity gaps. |
| **Calculated User Role Synchronization** | Eliminated a systemic gap where calculated user roles failed to receive initial context serialization on both context creation and new bindings. |
| **Documentation Progress** | Published comprehensive technical documentation for the query subsystem and delivered last week's management progress report (#290). |

---

## 2. Features Added & Bugs Fixed

### Critical Bug Fixes — Type System & Data Model

| PR | Fix |
|---|---|
| [#292](https://github.com/joopringelberg/perspectives-monorepo/pull/292) | **Fill with exact type only** — When role A requires filler type F but receives instance G (where G itself is filled by F), the system now automatically resolves to F at binding time. Previously storing G created indeterminate filler depth at compile time, breaking SQL-based query evaluation in `perspectives-tcp`. Added `findExactFiller` function that walks binding chains to find instances satisfying declared type constraints. |

### Bug Fixes — Distributed Synchronisation

| PR | Fix |
|---|---|
| [#286](https://github.com/joopringelberg/perspectives-monorepo/pull/286) | **Peer not synchronized when filter criterion becomes valid** — When a property used as a filter criterion for a calculated role (e.g., `Geregistreerden = filter Aanwezigen with Aanwezig`) changes to satisfy the filter, peers with perspectives on that calculated role now correctly receive synchronization deltas. The fix expands `isPerspectiveObject` to accept context queries starting with `FilterF` steps and routes them to new logic that emits role-creation deltas when the filter becomes satisfied. |
| [#282](https://github.com/joopringelberg/perspectives-monorepo/pull/282) | **Guard against serialising public resources** — When serializing a context for a new peer, dependency paths could traverse into public resources (`pub:` scheme), triggering spurious delta lookups or network fetches. The serialization pipeline now skips all delta-emitting operations for public resources, as peers should fetch them directly from public endpoints rather than via deltas. |
| [#281](https://github.com/joopringelberg/perspectives-monorepo/pull/281) | **Serialize new context for Calculated user roles on context creation and new bindings** — New contexts with Calculated user roles and new role bindings now correctly trigger full context serialization for affected users. Previously, calculated-role users received only incremental deltas (missing creation deltas) or no deltas at all. Implemented at both compile time (new `invertCalculatedUsers` phase-three pass) and runtime (new `handleNewCalculatedUsersForBinding` function). |

### TCP Server Improvements

| PR | Change |
|---|---|
| [#284](https://github.com/joopringelberg/perspectives-monorepo/pull/284) | **Cascade-delete context on RemoveExternalRoleInstance; property-based column lookup for flattened roles; logging improvements** — When an external role is removed, the TCP server now correctly cascade-deletes all role rows and the context row (previously left orphaned). Property delta handling switched from `roleType`-based to `propertyType`-based table lookup, fixing flattened role tables where properties from bound/filler roles are stored in the filled role's table. Suppressed misleading "No table configured" logs for external role delta types. |

### Documentation

| PR | Change |
|---|---|
| [#266](https://github.com/joopringelberg/perspectives-monorepo/pull/266) | **Query subsystem technical documentation** — Published comprehensive technical reference covering the query pipeline from Arc DSL parsing through compilation to executable PureScript closures, dependency tracking, and the interpreter. Chapters include parsing, query types, expression compiler, unsafe compiler, dependency collection, interpreter, and library extensions. Flagged several points of attention including broken getter cache, interpreter coverage gaps, and unsafe coercions. |
| [#290](https://github.com/joopringelberg/perspectives-monorepo/pull/290) | **Weekly progress report 2026-03-27** — Delivered last week's management-level progress report covering 10 merged PRs with analysis of distributed sync reliability, TCP maturation, and UX improvements. |

---

## 3. Impact on the Project

### Type System & SQL Query Evaluation
PR #292 closes a fundamental type system issue that would have prevented reliable SQL-based query evaluation in the TCP server. By ensuring roles are always filled with instances matching their declared type constraints (rather than indirect bindings), the system now has deterministic filler depth at compile time — a prerequisite for generating correct SQL joins. This was a blocking issue for production TCP deployments.

### Multi-Peer Data Integrity
Three synchronization fixes (#286, #282, #281) address scenarios where peers received incomplete, incorrect, or no data:
- **Filter criterion changes** (#286) — Previously, when a role instance satisfied a filter criterion, peers never received notification. This caused silent data loss in multi-user scenarios with filtered calculated roles.
- **Public resource serialization** (#282) — Attempting to serialize public resources could trigger unnecessary network fetches or log spurious errors, degrading performance and operational clarity.
- **Calculated user roles** (#281) — New contexts and new bindings failed to send initial state to calculated-role users, leaving them with incomplete local state that could never be corrected without manual intervention.

These fixes materially improve the correctness and completeness of the peer-to-peer synchronization pipeline.

### TCP Server Production Readiness
The cascade-delete implementation (#284) closes a data integrity gap: without it, removed contexts persist indefinitely in the reporting database, corrupting aggregate queries and reports. The shift to property-based column lookup fixes flattened role tables — a schema optimization strategy where properties from related roles are stored together. Combined with logging improvements, the TCP server moves closer to production readiness.

### Developer Knowledge Base
The query subsystem documentation (#266) captures complex design rationale that was previously held only in the original implementer's mind. The document identifies specific weaknesses (broken getter cache, interpreter gaps) that can now be addressed systematically rather than rediscovered through debugging.

---

## 4. Notable Patterns & Concerns

### ⚠️ Context Serialization Fragility Continues (Fourth Consecutive Week)
PR #281 (Calculated user roles) is the fourth consecutive week with a context serialization bug affecting peer synchronization. Pattern across weeks:
- Week 1: Basic serialization omissions
- Week 2: Screen rendering with empty role sets (#264)
- Week 3: Missing `SetFirstBinding` delta (#271), Onlooker recognition (#276)
- **Week 4: Calculated user role serialization (#281), public resource serialization (#282)**

The persistence of this pattern indicates that the context serialization pipeline — which must assemble and transmit a consistent snapshot to peers — has systemic edge cases not covered by automated tests. This is particularly concerning because serialization bugs cause silent data loss that users cannot detect.

### ⚠️ Type System Assumptions Breaking Under Real-World Use
PR #292 reveals that the type system was implicitly assuming direct type matches at binding time but not enforcing this. The issue only surfaced when TCP query generation attempted to determine join depth statically. This suggests the type system may have other implicit assumptions that aren't validated until downstream systems (like TCP) attempt to use them. A comprehensive audit of type system invariants would prevent similar discoveries.

### ⚠️ Filter-Based Calculated Roles Have Edge Cases
PR #286 exposes a gap in how filter-based calculated roles interact with the inverted query system. The fix works but the fact that `FilterF` steps require special handling in `isPerspectiveObject` and a dedicated `handleNewCalculatedUsersForBinding` code path suggests this feature was not fully integrated into the inverted query architecture. Other filter-related edge cases may exist.

### ✅ Systematic Approach to Calculated User Roles
PR #281 is noteworthy for its thoroughness: the fix addresses the issue at **both compile time** (new phase-three pass for inverting calculated user queries) and **runtime** (detecting new calculated users on binding changes). This two-level approach ensures the fix is structurally sound rather than a runtime patch.

### ✅ Documentation Alongside Implementation
The query subsystem documentation (#266) was merged in the same week as complex query-related fixes (#281, #286). This timing is ideal: the mental model is fresh, and the documentation can immediately help with debugging and future work in this area.

### ℹ️ High Defect Detection Rate
Five of seven PRs this week are bug fixes. While this reflects thorough issue tracking and active use of the system, it also suggests the test surface for distributed synchronization and type system invariants is insufficient to catch these issues before they reach users.

---

## 5. Recommended Focus Areas for Next Week

| Priority | Area | Rationale |
|---|---|---|
| 🔴 **Critical** | **Context serialization integration test suite** | Fourth consecutive week of serialization bugs (#281, #282 this week; #271, #276 last week; #264, #236 prior). A comprehensive test suite covering Enumerated/Calculated user roles, public resources, empty role sets, and new bindings is now essential to prevent continued data loss scenarios. |
| 🔴 **Critical** | **Type system invariant validation** | PR #292 reveals the type system had an unenforced assumption (exact type matches at binding time) that broke downstream systems. A systematic audit of type system invariants and their enforcement points is needed. Add assertions or compile-time checks for key invariants. |
| 🟠 **High** | **Filter-based calculated role comprehensive testing** | PR #286 required special-case handling for `FilterF` steps in multiple code paths. Comprehensive tests covering filter criterion changes (true→false, false→true), multiple filters, nested filters, and their interaction with peer synchronization will prevent similar gaps. |
| 🟠 **High** | **TCP server end-to-end validation** | PRs #292 (filler type resolution) and #284 (cascade-delete, flattened roles) both affect SQL generation/execution correctness. An end-to-end test suite that validates TCP server behavior against the PDR — including schema generation, delta processing, and query results — is needed. |
| 🟡 **Medium** | **Query subsystem technical debt** | The query documentation (#266) flagged several issues: broken getter cache, interpreter coverage gaps, 6-argument hard limit on external functions. These should be triaged and scheduled — the broken cache in particular may be causing unnecessary recompilation overhead. |
| 🟡 **Medium** | **Inverted query architecture review** | PRs #281 (calculated users) and #286 (filter criterion changes) both required adding new inverted query types and special-case handling. A review of the inverted query architecture to identify if it needs generalization would prevent future special cases. |

---

## Appendix: PR Details Summary

| PR | Type | Theme | LOC Changed |
|---|---|---|---|
| #292 | Bug | Type System | +56 / -13 |
| #286 | Bug | Distributed Sync | +16 / -6 |
| #282 | Bug | Distributed Sync | +32 / -18 |
| #281 | Bug | Distributed Sync | +586 / -42 |
| #284 | Feature | TCP Server | +2353 / -61 |
| #266 | Documentation | Query System | +798 / -0 |
| #290 | Documentation | Management | +108 / -0 |
| **Total** | | | **+3949 / -140** |

**Note:** PR #281 and #284 are substantial changes (586 and 2353 lines added respectively), indicating complex fixes that touched multiple subsystems. This further underscores the need for comprehensive integration testing to prevent regressions.

---

*Report generated on 2026-03-31.*
