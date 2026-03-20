# Weekly Progress Report — 2026-03-20

> **Audience:** Management / Stakeholders
> **Period:** Week ending 20 March 2026
> **Scope:** 15 merged pull requests (PRs #222 – #254)

---

## 1. Key Achievements

| Theme | Summary |
|---|---|
| **Performance** | Introduced in-memory LRU caching for deltas and resource versions, reducing repeated database round-trips. |
| **Extensibility** | Delivered the `perspectives-tcp` package — a new Transaction Collection Point that enables TCP-based transaction transport. |
| **Type System** | Extended the `filledBy` constraint to support full boolean (OR-of-ANDs) expressions, making the type system significantly more expressive. |
| **UI Flexibility** | Shipped a generic `when` conditional construct for screen definitions, allowing model authors to show or hide screen sections based on run-time conditions. |
| **Observability** | Applied structured perspective logging across the transaction-handling subsystem and published two design/reference documents (delta ordering, type comparison). |

---

## 2. Features Added & Bugs Fixed

### New Features

| PR | Feature |
|---|---|
| [#254](https://github.com/joopringelberg/perspectives-monorepo/pull/254) | **LRU caching** — Delta and resource-version lookups are now served from in-memory LRU caches, cutting PouchDB/IndexedDB round-trips. |
| [#224](https://github.com/joopringelberg/perspectives-monorepo/pull/224) | **perspectives-tcp package** — New standalone package for a Transaction Collection Point over TCP, enabling alternative transport for distributed transactions. |
| [#227](https://github.com/joopringelberg/perspectives-monorepo/pull/227) | **filledBy OR-of-ANDs** — Role-fill constraints can now express disjunctions of conjunctions, making type-based access control more expressive. |
| [#222](https://github.com/joopringelberg/perspectives-monorepo/pull/222) | **Generic `when` construct** — Screen definitions (freeform and who/what/where) support a conditional `when <expression>` wrapper; evaluated server-side so the client receives only relevant elements. |
| [#252](https://github.com/joopringelberg/perspectives-monorepo/pull/252) | **Structured logging** — `Perspectives.Sync.HandleTransaction` now emits structured, perspective-aware log entries, improving operational insight. |

### Documentation

| PR | Document |
|---|---|
| [#238](https://github.com/joopringelberg/perspectives-monorepo/pull/238) | `delta-ordering.md` — Reference document on deterministic delta ordering. |
| [#229](https://github.com/joopringelberg/perspectives-monorepo/pull/229) | `type-comparison.md` — Documents the type comparison system in the Perspectives Distributed Runtime. |

### Bug Fixes

| PR | Fix |
|---|---|
| [#250](https://github.com/joopringelberg/perspectives-monorepo/pull/250) | **False conflict on resource creation** — Version-0 resources no longer trigger spurious conflicts during creation. |
| [#248](https://github.com/joopringelberg/perspectives-monorepo/pull/248) | **hasPerspectiveOnRole with aspect roles** — Fixed a failure when a perspective targets an aspect of the object role, preventing incorrect permission denials. |
| [#246](https://github.com/joopringelberg/perspectives-monorepo/pull/246) | **YAML MIME type normalisation** — MIME type comparison for YAML files now tolerates the `application/x-yaml` / `text/yaml` variants, fixing pattern-constraint validation on upload. |
| [#244](https://github.com/joopringelberg/perspectives-monorepo/pull/244) | **Role name hidden in canShowBoth mode** — Role names were invisible on first render; now correctly displayed. |
| [#242](https://github.com/joopringelberg/perspectives-monorepo/pull/242) | **File upload MIME validation & replacement** — Files are validated against declared constraints on upload; replacement is allowed when a property is in the FILLED state. |
| [#240](https://github.com/joopringelberg/perspectives-monorepo/pull/240) | **Orphaned IndexedRoles entry** — Removed a stale `sys:SocialMe` entry that could cause query inconsistencies. |
| [#236](https://github.com/joopringelberg/perspectives-monorepo/pull/236) | **Screen rendering regression** — Markdown `when` conditions were silently ignored and the External role form was hidden; both are now handled correctly. |
| [#232](https://github.com/joopringelberg/perspectives-monorepo/pull/232) | **Data upgrade — obsolete enumerated roles** — An automated migration removes obsolete `SocialEnvironment$Me` role instances from existing user databases. |

---

## 3. Impact on the Project

### Performance & Scalability
The LRU caching layer (#254) directly reduces latency for delta and version lookups. For users with large or busy contexts, this translates to noticeably snappier transaction processing without requiring any model or data changes.

### Reliability & Data Integrity
Three fixes (#250, #240, #232) address data-consistency issues that could silently corrupt state or produce wrong query results. The version-0 conflict fix (#250) is particularly important for new-resource creation, a frequent operation. The data-upgrade migration (#232) ensures clean state for all existing deployments.

### Developer & Model-Author Productivity
The `when` conditional (#222/#236), the OR-of-ANDs `filledBy` (#227), and the new structured logging (#252) together give model authors and developers significantly more control: conditional UI, richer fill constraints, and better runtime visibility with minimal additional code.

### Extensibility / Ecosystem
The `perspectives-tcp` package (#224) lays the groundwork for alternative transaction transports, opening a path toward non-browser (server-side, IoT, CLI) participation in the Perspectives network.

### Documentation
Two reference documents (#229, #238) reduce onboarding friction for new contributors and clarify guarantees for existing ones.

---

## 4. Notable Patterns & Concerns

### ✅ High Velocity with Good Coverage
Fifteen PRs were merged in one week, covering new features, bug fixes, data migrations, and documentation — a well-rounded sprint.

### ✅ Documentation Accompanies Features
Both the structured-logging and the `when` construct received dedicated design documents. This practice improves long-term maintainability.

### ⚠️ Accumulating Data-Migration Debt
Three PRs in this period (#240, #232, and implicitly #250) address residual data issues from earlier design decisions. While each migration is targeted and safe, the frequency suggests that the underlying model evolution process may benefit from a more formal versioning and migration strategy to prevent similar cleanup cycles.

### ⚠️ Screen Rendering Regression (#236)
A regression — conditions silently ignored and the External role form hidden — shipped and required a follow-up fix within the same period. Additional integration tests for screen-contextualisation logic would reduce the risk of similar regressions.

### ℹ️ New Package Boundary
The addition of `perspectives-tcp` as a standalone package is architecturally sound, but introduces a new dependency surface that needs CI coverage and a clear ownership/maintenance policy.

---

## 5. Recommended Focus Areas for Next Week

| Priority | Area | Rationale |
|---|---|---|
| 🔴 High | **Screen rendering test coverage** | The regression in #236 (and the follow-up in #244) highlights a gap in automated UI/screen tests. Adding integration tests for `contextualiseScreen` / `contextualiseMarkDownDef` paths will prevent regressions. |
| 🔴 High | **perspectives-tcp integration tests & CI** | The new package (#224) currently lacks demonstrated CI coverage. Before wider adoption, automated tests covering the TCP transport path are essential. |
| 🟠 Medium | **Data-migration strategy** | Formalise how model schema changes trigger user-database migrations, to reduce ad-hoc cleanup PRs (#232, #240). A versioned migration registry would make this manageable at scale. |
| 🟠 Medium | **LRU cache observability** | The new caches (#254) need monitoring hooks (hit/miss rates, eviction counts) so that cache sizing can be tuned in production. |
| 🟡 Low | **filledBy OR-of-ANDs validation** | Verify that the extended constraint language (#227) is exercised in existing model tests; add targeted tests for edge-case disjunctions. |
| 🟡 Low | **Documentation consolidation** | Three new docs were added across docsources and design directories. A single index or table of contents for developer documentation would aid discoverability. |

---

*Report generated by GitHub Copilot on 2026-03-20.*
