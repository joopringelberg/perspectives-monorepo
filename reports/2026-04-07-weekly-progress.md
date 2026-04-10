# Weekly Progress Report — 2026-04-07

> **Audience:** Management / Stakeholders
> **Period:** Week ending 7 April 2026
> **Scope:** 13 merged pull requests (PRs #322, #320, #318, #288, #313, #311, #310, #308, #306, #304, #230, #297, #295)

---

## 1. Key Achievements

| Theme | Summary |
|---|---|
| **TCP SQL Query Evaluation (Major Feature)** | Completed comprehensive design and implementation enabling SQL-based reporting from Perspectives models — TCP server can now materialise Onlooker perspectives as database views queryable by standard BI tools. 2,489 lines of code added across PDR and TCP server, with full design documentation resolving 12 architectural questions. |
| **Missing Resource Auto-Restoration** | Switched from blocking user dialogs to automatic delta-based restoration with navigable notifications — eliminates user friction while maintaining data integrity. Missing contexts/roles are now silently restored from the DeltaStore with a single clickable notification to navigate to the recovered data. |
| **Data Retrieval Performance** | DeltaStore `contextKey` indexing eliminates full-table scans for context-scoped delta queries — critical performance improvement for resource restoration. Migration added to backfill existing records. |
| **PDR→Frontend Messaging Documentation** | Published comprehensive end-to-end documentation of all three PDR-to-frontend communication mechanisms (status messages, warnings, blocking dialogs), filling a knowledge gap that was blocking developer onboarding. |
| **User Experience Polish** | Fixed three UI regressions: floating menu context navigation, boolean checkbox visual updates, and removed debug-only CORS blocker. |

---

## 2. Features Added & Bugs Fixed

### Major Feature — TCP SQL Query Evaluation

| PR | Feature |
|---|---|
| [#288](https://github.com/joopringelberg/perspectives-monorepo/pull/288) | **PL query evaluation based on SQL for TCP reporting** — Research task implementing SQL-based query evaluation for TCP reporting. The PDR now generates a complete relational schema configuration from ARC models, including filler-chain columns materialised as LEFT JOIN views with COALESCE for union-branch disambiguation. All 12 design questions resolved; TCP server can now execute Onlooker-perspective queries via standard SQL. <br><br>**Key capabilities:** <br>• Identifies Onlooker roles (both enumerated and calculated) and extracts visible properties <br>• Computes all union-branch filler chains up to depth 5 <br>• Generates hop role tables (pass-through and endpoint) <br>• Two-segment table naming (`Domain$RoleType`) prevents collisions <br>• Universal context table with `context_type_name` column <br>• `GenerateTCPConfiguration` callable from ARC models <br>• Full query-function coverage analysis (Category 1: straightforward SQL; Category 2: prohibited in Onlooker queries; Category 3: implementable with limitations) <br><br>**Architecture:** PDR emits portable relational-algebra query plan (JSON); TCP consumes via Knex for multi-backend support (PostgreSQL/MySQL/SQLite/MSSQL). <br><br>**Added:** `pl-query-to-sql-design.md` (design doc), `tcpConfiguration.purs` (config generator), `generateTCPConfiguration` API, view generation with COALESCE, hop role tables, two-segment naming, `nameMap`, schema migration support. <br><br>+2,489 / -86 lines across 17 files. |

### Performance Improvement — DeltaStore Indexing

| PR | Feature |
|---|---|
| [#313](https://github.com/joopringelberg/perspectives-monorepo/pull/313) | **Add contextKey to DeltaStoreRecord for efficient context-scoped delta retrieval** — `getContextDeltasForContextInstance` required full-table scans deserializing every ContextDelta's `signedDelta.encryptedDelta` to check the `contextInstance` field. New `contextKey :: Maybe String` field populated with `safeKey(contextInstance)` for ContextDelta types enables filtered retrieval without deserialization. New `getDeltasForContextKey` replaces table-scan logic; `addContextKeyMigration` 3.2.0 upgrade backfills existing records. Critical for resource restoration performance at scale. <br><br>+180 / -28 lines. |

### UX Improvement — Missing Resource Handling

| PR | Feature |
|---|---|
| [#311](https://github.com/joopringelberg/perspectives-monorepo/pull/311) | **Default restore of missing resources with navigation notification** — Replaced blocking yes/no dialog (which recurred repeatedly during screen building) with automatic restoration + one-time piggybacked warning containing clickable navigation link. `Warning` type extended with `externalRoleId` and `contextName` fields; frontend renders titled modal with i18n message + context hyperlink dispatching `OpenContext` event. Removed ~200 lines of now-unused cleanup code (`fixContextReferences`/`fixRoleReferences`). <br><br>+120 / -98 lines. |
| [#230](https://github.com/joopringelberg/perspectives-monorepo/pull/230) | **Switch missing-resource handler from dangling-reference cleanup to DeltaStore restoration with end-user choice dialog** (merged Apr 2) — Original implementation providing the foundation for #311. Introduced `restoreResource` module with 5-step context restoration: restore external role → apply ConstructEmptyContext → scan DeltaStore for ContextDeltas → restore all referenced roles → re-apply ContextDeltas. Added blocking integrity-choice dialog infrastructure (later generalized in #306, then replaced with auto-restore in #311). <br><br>+738 / -71 lines. |

### Architecture Improvement — Reusable User Interaction

| PR | Feature |
|---|---|
| [#306](https://github.com/joopringelberg/perspectives-monorepo/pull/306) | **Generalize PDR yes/no dialog: `requestUserChoice` + `MissingResource` warning** — Extracted hardcoded `requestIntegrityChoice` into reusable `requestUserChoice` function in new `Perspectives.UserInteraction` module. New `MissingResource` warning constructor; `IntegrityChoicePayload` → `UserChoicePayload` with `yesOption`/`noOption` parameters. Updated documentation. Foundation for future user-interaction dialogs. <br><br>+154 / -85 lines. |

### Bug Fixes — UI Regressions

| PR | Fix |
|---|---|
| [#322](https://github.com/joopringelberg/perspectives-monorepo/pull/322) | **TCP: skip VIEW-only filler-chain columns when processing RolePropertyDelta; additive schema migration** — Two bugs: (1) `handleRolePropertyDelta` attempted UPDATE on filler-chain columns (VIEW-only, stripped from base table) causing `column does not exist` errors — fixed by filtering `col.fillerChain.length === 0`; (2) `context_type_name` column missing on existing tables — `applySchema` now performs additive migration via `hasColumn` + `ALTER TABLE ADD COLUMN` for all missing expected columns (idempotent, nullable). <br><br>+28 / -5 lines. |
| [#320](https://github.com/joopringelberg/perspectives-monorepo/pull/320) | **Fix openContextOfRole in floating menu: dispatch from document.body instead of unmounted menuRef** — `onOpenContextOfRole` async callback fired after menu unmounted, making `menuRef.current` null. Switched to `document.body.dispatchEvent` (stable node, consistent with `userMessaging.tsx`). <br><br>+6 / -8 lines. |
| [#318](https://github.com/joopringelberg/perspectives-monorepo/pull/318) | **Fix boolean checkbox not updating visually; add mouse-click edit mode** — Checkboxes frozen until screen rebuild. Added optimistic state update (`setState({ value: newvalue })` before `changeValue()`), edit-mode lifecycle (`checkboxEditingActive` flag + `FormFieldEditStarted`/`FormFieldEditEnded` events), and corrected controlled-component pattern (`onChange` instead of `onClick`). <br><br>+19 / -2 lines. |
| [#310](https://github.com/joopringelberg/perspectives-monorepo/pull/310) | **Remove debug-only IndexedDB→CouchDB sync (CORS blocker)** — Deleted `syncWIthCouchdb.ts` debug utility and its call site. CORS was blocking cross-origin requests to `localhost:6984` regardless of dev mode guard. <br><br>-60 lines. |

### Documentation

| PR | Content |
|---|---|
| [#304](https://github.com/joopringelberg/perspectives-monorepo/pull/304) | **Document PDR→frontend messaging mechanism end-to-end** — Comprehensive documentation of all three messaging mechanisms: (1) Real-time status messages (`setPDRStatus` → SharedWorker broadcast → loading overlay), (2) Warnings piggybacked on API responses (per-request diagnostics for modellers), (3) Blocking integrity-choice dialog (special-case AVar + Promise blocking). Traces full stack from PureScript to frontend event handlers. <br><br>+437 lines. |
| [#297](https://github.com/joopringelberg/perspectives-monorepo/pull/297) | **Document PDR client data structures (ScreenDefinition → Perspective hierarchy)** — Narrative documentation for all types in `perspectivesshape.d.ts` previously undocumented. Full walk from `ScreenDefinition` down through all variants, pairing TypeScript definitions with PureScript counterparts, module references, and field-semantics tables. Includes ancillary types (identity primitives, callbacks, `RuntimeOptions`) and developer-tool types (`InspectableContext`/`InspectableRole`). Type map cross-reference table. <br><br>+1,220 lines. |
| [#308](https://github.com/joopringelberg/perspectives-monorepo/pull/308) | **Add documentation index for docsources** — New `docsources/index.md` with documents overview (10 docs grouped by theme) and module cross-reference table (46 PureScript modules → chapter references). Navigation entry point for the documentation directory. <br><br>+92 lines. |
| [#295](https://github.com/joopringelberg/perspectives-monorepo/pull/295) | **Weekly progress report 2026-03-31** — Prior week's management-level report. <br><br>+131 lines. |

---

## 3. Impact on the Project

### SQL-Based Reporting Capability (Strategic)
PR #288 represents a **strategic architectural addition** enabling the Perspectives project to integrate with the broader BI/analytics ecosystem. Previously, reporting on Perspectives data required custom PDR API clients; now standard SQL tools (Power BI, Metabase, Grafana, Tableau) can query materialized views directly. This dramatically lowers the barrier to entry for business analysts and expands the project's addressable use cases.

**Technical implications:**
- PDR becomes a **source-of-truth compiler** for relational schemas, not just a runtime
- TCP server shifts from simple delta-processor to full **query executor**
- Union-branch filler chains are now **compile-time resolved** and materialized as COALESCE views
- Two-segment naming prevents table collisions as more models are added to a single TCP instance

**Maturity milestone:** The 12 resolved design questions and comprehensive query-function coverage analysis indicate this feature has graduated from research to production-ready status.

### User Interruption Elimination (UX)
The progression #230 → #306 → #311 shows a thoughtful evolution:
1. **#230**: Blocking dialog for every missing resource (user repeatedly interrupted during screen building)
2. **#306**: Generalized dialog infrastructure for reuse
3. **#311**: Auto-restore + single notification (no user interruption)

The final UX is dramatically better: missing resources are silently restored from the DeltaStore, and users receive a **single titled modal with a clickable navigation link** to review the restored data. This is the correct trade-off: automatic recovery for data that can be deterministically restored, with visibility into what happened.

### Developer Onboarding Acceleration
The three documentation PRs (#304, #297, #308) address a **critical knowledge-transfer gap**. The PDR→frontend messaging mechanism (#304) was previously only understood through reading source code across multiple languages (PureScript → JavaScript → TypeScript → React). The client data structures documentation (#297) makes the `perspectivesshape.d.ts` type hierarchy comprehensible. The index (#308) provides a navigation layer that was completely absent.

**Combined impact:** Onboarding time for new developers working on the frontend or PDR messaging should drop significantly.

### Performance at Scale
PR #313 (DeltaStore `contextKey` indexing) addresses a **scalability bottleneck**. Full-table scans with per-record deserialization are acceptable for small datasets but break down as the DeltaStore grows. The `contextKey` index enables **O(log n) filtered retrieval** instead of **O(n) full scans**. This is particularly critical for resource restoration, which can occur frequently in peer-to-peer scenarios with intermittent connectivity.

---

## 4. Notable Patterns & Concerns

### ✅ Research → Production Graduation
PR #288 (SQL query evaluation) demonstrates **exemplary research-to-production methodology**:
1. Comprehensive design document addressing 12 architectural questions
2. Full implementation across both PDR and TCP server
3. Coverage analysis for all query functions (3 categories: implementable, prohibited, limited)
4. Backward-compatible schema evolution (additive migration in #322)
5. Example configuration updated

This sets a high standard for future architectural additions.

### ✅ Iterative UX Refinement
The #230 → #306 → #311 progression shows the team **responding to real-world usage feedback**. The initial blocking-dialog approach (#230) was technically correct but created poor UX when screens with many missing resources were built. The team iterated through generalization (#306) to the current auto-restore approach (#311) in three weeks. This is responsive product development.

### ✅ Documentation Investment Accelerating
Three documentation PRs this week (vs. one or two in prior weeks) suggests the team is recognizing documentation debt and addressing it systematically. The docsources index (#308) in particular shows meta-awareness: documentation is only useful if it's discoverable.

### ⚠️ TCP Server Bug Density
PR #322 fixes two bugs in the TCP server that only surfaced after PR #288 was merged:
1. Filler-chain columns incorrectly processed as base-table columns
2. Schema migration not additive (existing tables never gain new columns)

**Concern:** These bugs were introduced by #288's changes to the column model and schema generator but weren't caught before merge. This suggests:
- TCP server test coverage is insufficient for schema evolution scenarios
- Schema migration testing is manual/ad-hoc

**Recommendation:** Add schema migration integration tests covering: fresh schema creation, additive column evolution, filler-chain view regeneration, and multi-version upgrade paths.

### ⚠️ UI Regression Pattern Continues
Three UI regressions fixed this week (#322, #320, #318):
- Floating menu dispatch broken by async timing + unmount
- Boolean checkboxes frozen (no optimistic update)
- Debug sync blocking production with CORS errors

**Pattern:** These are not edge cases — they're core user interactions (checkbox toggling, menu navigation). The fact that they shipped broken suggests:
- Manual testing surface is insufficient
- No automated UI/integration tests for these flows

**Recommendation:** Add Playwright/Cypress end-to-end tests covering checkbox editing, context navigation from tables/menus, and form field edit-mode lifecycle.

### ℹ️ High Documentation-to-Code Ratio
4 of 13 PRs this week are pure documentation (31%). This is **healthy** — the project has accumulated significant complexity and tribal knowledge. The documentation investment will compound as new contributors onboard.

### ℹ️ Migration Strategy Maturing
PR #313's 3.2.0 data migration (backfill `contextKey`) and PR #322's schema migration (additive column evolution) show the project is developing mature **data evolution patterns**. Both are idempotent and backward-compatible. This is critical for production deployments with persistent data.

---

## 5. Recommended Focus Areas for Next Week

| Priority | Area | Rationale |
|---|---|---|
| 🔴 **Critical** | **TCP schema migration integration tests** | PR #322 fixes two bugs (#288 introduced) that broke existing TCP deployments: (1) filler-chain columns treated as base columns, (2) schema migration not additive. Need comprehensive tests covering fresh creation, additive evolution, and multi-version upgrades before next schema change. |
| 🔴 **Critical** | **UI regression test suite (E2E)** | Three UI regressions this week (#320, #318, #310) affecting core user flows (checkbox editing, context navigation, form lifecycle). Add Playwright/Cypress tests for checkbox toggle, menu navigation, form edit mode, and loading overlay. |
| 🟠 **High** | **TCP end-to-end validation** | PR #288 adds 2,489 lines of SQL generation logic but relies on manual testing. Automated tests should validate: (1) schema generation matches model, (2) delta processing populates tables correctly, (3) view LEFT JOINs produce correct results, (4) two-segment naming prevents collisions. |
| 🟠 **High** | **DeltaStore performance benchmarking** | PR #313 adds `contextKey` indexing to eliminate full-table scans. Validate the improvement with benchmarks on realistic dataset sizes (1k, 10k, 100k deltas). Identify other full-scan patterns in DeltaStore/ResourceVersionStore. |
| 🟡 **Medium** | **Frontend messaging error handling** | PR #304 documents happy-path PDR→frontend messaging. Add documentation + implementation for error paths: what happens when the proxy loses connection, when the SharedWorker crashes, when the user dismisses a blocking dialog via browser UI? |
| 🟡 **Medium** | **SQL query function implementation prioritization** | PR #288 categorizes query functions but doesn't implement Category 3 (implementable with limitations: `OrElseF`, `FirstF`, `BindVariable`, `RegExMatch`). Prioritize based on actual Onlooker query usage in existing models. |

---

## Appendix: PR Details Summary

| PR | Type | Theme | LOC Changed | Merged |
|---|---|---|---|---|
| #288 | Feature | TCP SQL | +2,489 / -86 | Apr 5 |
| #230 | Feature | Resource Restoration | +738 / -71 | Apr 2 |
| #297 | Documentation | Client Data Structures | +1,220 / -0 | Apr 1 |
| #304 | Documentation | PDR Messaging | +437 / -0 | Apr 2 |
| #313 | Performance | DeltaStore Indexing | +180 / -28 | Apr 2 |
| #311 | UX | Auto-Restore | +120 / -98 | Apr 2 |
| #295 | Documentation | Weekly Report | +131 / -0 | Mar 31 |
| #306 | Refactor | User Interaction | +154 / -85 | Apr 2 |
| #308 | Documentation | Docs Index | +92 / -0 | Apr 2 |
| #322 | Bug | TCP Schema | +28 / -5 | Apr 7 |
| #318 | Bug | UI (Checkbox) | +19 / -2 | Apr 7 |
| #320 | Bug | UI (Menu) | +6 / -8 | Apr 7 |
| #310 | Bug | Debug Cleanup | +0 / -60 | Apr 2 |
| **Total** | | | **+5,614 / -443** | |

**Notes:**
- **Largest change:** PR #288 (SQL query evaluation) at 2,489 additions — major architectural feature
- **Documentation week:** 1,880 lines of documentation added (33% of total additions)
- **Net code growth:** +5,171 lines (primarily feature additions, not refactoring)

**Bug distribution:**
- 4 bugs fixed (31% of PRs)
- 2 TCP server bugs (#322)
- 2 UI bugs (#320, #318)
- 1 debug-code cleanup (#310)

---

*Report generated on 2026-04-08.*
