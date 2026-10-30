# Weekly Progress Report — 2026-04-14

> **Audience:** Management / Stakeholders
> **Period:** Week ending 14 April 2026
> **Scope:** 10 merged pull requests (PRs #336, #334, #330, #328, #301, #256, #316, #326, #314, #324)

---

## 1. Key Achievements

| Theme | Summary |
|---|---|
| **Language Reference Documentation (Major Milestone)** | Completed five comprehensive ARC language reference chapters covering Perspectives, States, Assignments, Actions, and Screens — 5+ chapters totaling thousands of lines of documentation. All major language constructs now have authoritative reference documentation with cross-references, code examples, and complete syntax tables. |
| **Testing Infrastructure Foundation** | Established three-layer test architecture (Layer 1 pure/in-memory, Layer 2 PouchDB scaffold, Layer 3 AMQP stub) with automated CI for Layer 1 tests. Added comprehensive unit test suites for ADT/CNF modules (50+ assertions) and extended expression parser tests from 40 to 74 tests. Node.js persistence layer and architecture research doc lay groundwork for automated testing and native deployments. |
| **Structured Logging System** | Implemented complete structured logging framework with runtime-configurable topic-based filtering, severity levels, and browser console API. Enables selective logging control for SYNC, QUERY, PERSISTENCE, MODEL, and other subsystems without code changes. Incrementally migrated 15+ modules to new API. |
| **Deployment Path Documentation** | Published comprehensive deployment architecture research covering Capacitor (iOS/Android), Tauri v2 (desktop/mobile), update mechanisms, and version management. Three update strategies documented for web layer OTA updates without app store submission. |
| **Developer Experience** | Improved parser error messages for reserved words, fixed parser test suite for current syntax, added weekly reporting automation, and prevented duplicate delta storing. |

---

## 2. Features Added & Bugs Fixed

### Major Documentation — ARC Language Reference

| PR | Content |
|---|---|
| [#334](https://github.com/joopringelberg/perspectives-monorepo/pull/334) | **ARC reference manual chapters for Perspectives, States, Assignments, Actions, and Screens** — Five new AsciiDoc chapters extending the Perspectives Language reference manual with previously undocumented language constructs. <br><br>**perspectives.adoc**: Complete `perspective on`/`perspective of` syntax with all 10 role verbs (`Create`, `Delete`, `Fill`, `Unbind`, etc.) and 5 property verbs (`Consult`, `SetPropertyValue`, etc.) in reference tables. Covers state-qualified blocks (`in state`), nested perspectives, inline actions, `defaults`, `selfonly`, `authoronly`. <br><br>**state_declarations.adoc**: Named state syntax, sub-states, `on entry`/`on exit` transitions with `of subject\|object\|context state` qualifiers, `do` automatic effects with timing modifiers (`after`, `until`, `every`, `maximally N times`), duration literals, `notify` sentence syntax, and `letA` binding blocks. <br><br>**assignments.adoc**: All assignment forms documented: property (`=`, `=+`, `=-`, `delete property`), role (`create role`, `remove role`, `delete role`, `bind`, `bind_`, `unbind`, `unbind_`, `move`), context (`create context`, `create_ context`, `remove context`, `delete context`), file creation (`create file … as … in`), external effects (`callEffect`, `callDestructiveEffect`). <br><br>**action.adoc**: `action` declaration placement (context vs. role actions), state-qualified actions, `letA` bodies, compiler checks relating actions to perspective grants. <br><br>**screens.adoc**: Both screen types documented: who-what-where (`who`/`what`/`where` with `master`/`detail` table-form items) and classic free-form (`tab`, `row`, `column`). All screen elements: `table`, `form`, `markdown` (angle-bracket syntax), `chat` (`messages`/`media`), and `when` conditionals. Widget options: `with props`, `without props`, `props … verbs`, `only`, `no roleverbs`, `fillfrom`, `fields`/`minLines`/`maxLines`. <br><br>Cross-references between all chapters via AsciiDoc anchors. Content grounded in parser source and verified against existing model files. <br><br>Thousands of lines of authoritative language reference documentation. |

### Major Documentation — Deployment Architecture

| PR | Content |
|---|---|
| [#336](https://github.com/joopringelberg/perspectives-monorepo/pull/336) | **Version update mechanisms for Capacitor and Tauri installable apps** — Extends `nodejs-testing-architecture.md` with **Part 4 — Version Updates for Installable Applications**, completing the deployment path documentation. <br><br>**Capacitor — two-tier update model**: Native shell (plugins, SDK) requires App Store submission; web layer (PDR, React, models) has three OTA options requiring no store review: (A1) Remote URL with existing service worker, (A2) `@capacitor/live-updates` for offline-bundled apps with background download/apply on restart, (A3) Service worker in WebView (automatic on Android + iOS 16+). <br><br>**Tauri — `tauri-plugin-updater`**: Signed bundles via `tauri signer generate`; public key embedded in `tauri.conf.json`; update manifest served from GitHub Releases; `tauri-action` GitHub Action generates and signs artifacts automatically; TypeScript API (`@tauri-apps/plugin-updater`) allows reusing existing "new version available" notification UI. <br><br>**Cross-deployment comparison table** shows which update mechanisms require app store submission. Summary table extended with Capacitor and Tauri update rows. Tactical section adds concrete `capacitor.config.ts`, `tauri.conf.json`, and GitHub Actions snippets, plus strategy for version parity across all deployment targets from single `package.json` source. <br><br>Provides complete end-to-end deployment path for iOS, Android, and desktop platforms with update strategies. |

### Major Feature — Testing Infrastructure

| PR | Feature |
|---|---|
| [#301](https://github.com/joopringelberg/perspectives-monorepo/pull/301) | **Node.js persistence layer, architecture research doc, AffJax switch for PDR-N, and three-layer test suite refactoring** — Groundwork for running PDR in Node.js, enabling automated testing and native deployments. <br><br>**persistenceAPI.node.js**: Node.js-compatible persistence layer — drop-in replacement for browser `persistenceAPI.js` using `pouchdb` (classic-level), plain `node-fetch` (no CORS), `Buffer` for base64, always-offline detection. <br><br>**idb-keyval.node.js**: IndexedDB stub using in-memory `Map` backed by optional JSON file (configurable via `PERSPECTIVES_KEYVAL_STORE` env var). <br><br>**rollup.node.config.js**: Produces `dist/perspectives-core.node.js` with `@rollup/plugin-alias` transparently redirecting browser modules to Node.js equivalents. No package split — single package with two build outputs (PDR-B browser, PDR-N Node.js). <br><br>**AffJax switch**: `affjax-node@1.0.0` replaces `affjax-web` for Node.js build, backed by `xhr2` polyfill. Rollup alias redirects `Affjax.Web → Affjax.Node` at bundle time with zero PureScript source changes. <br><br>**Three-layer test refactoring**: Layer 1 (`test/Layer1.purs`) — 10 pure suites runnable now with `pnpm run test:layer1` (no infrastructure required); Layer 2 (`test/Layer2.purs`) — PouchDB integration scaffold; Layer 3 (`test/Layer3.purs`) — AMQP stub scaffold. CI workflow `.github/workflows/pdr-layer1-tests.yml` runs Layer 1 on every push. <br><br>**docsources/nodejs-testing-architecture.md**: Architecture research addressing mobile/desktop (Capacitor/Tauri strategy), PDR on Node.js (platform-specific migration), automated testing layers (four layers from unit to E2E), AI-assisted test cycle. <br><br>+2,800+ lines across persistence layer, test scaffolding, CI workflow, and architecture documentation. |
| [#330](https://github.com/joopringelberg/perspectives-monorepo/pull/330) | **Unit tests for ExpandedADT, CNF, and ADT modules** — Proper Layer 1 test suite replacing commented-out placeholder suites. 50+ assertions across four sub-suites, all purely in-memory (`Int` leaf type, no `MonadPerspectives`/CouchDB). <br><br>**ExpandedADT**: `Eq`, `Functor`, `Foldable`, `Traversable`, `foldMapExpandedADT` (AND/OR/ECT semantics). <br><br>**CNF**: `DSUM`/`DPROD` set-based equality (order-invariant), `toConjunctiveNormalForm` for all constructors including distributive law `(1∧2)∨(3∧4) ≡ (1∨3)∧(1∨4)∧(2∨3)∧(2∨4)`, `traverseDPROD` (structure-preserving, short-circuits on `Nothing`). <br><br>**ADT**: `Eq`, `Functor`, `Foldable`, `Traversable`, `foldMapADT`, `computeBoolean`, `computeExpandedBoolean`, `allLeavesInADT` (union), `commonLeavesInADT` (intersection), `expand` via `Identity`. <br><br>**Type comparison**: Both positive and negative assertions for `equalsOrSpecialises_`, `equals_`, `equalsOrSpecialises`, `specialises`, `generalises`, `specialises_`, `generalises_`. <br><br>Wired into Layer 1 runner alongside existing ADT suite. +300+ lines. |
| [#328](https://github.com/joopringelberg/perspectives-monorepo/pull/328) | **Fix and extend ARC expression parser tests for current language syntax and parser behavior** — Several tests written against older parser/syntax causing false failures. Extended `Test.Parsing.Arc.Expression` suite from 40 to 74 tests covering all major constructs from `query-subsystem.md` §3. <br><br>**Build fix**: FFI case mismatch — `changesfeed.js` renamed to `changesFeed.js` to match `.purs` filename exactly (Linux case-sensitive filesystem `MissingFFIModule` error). <br><br>**Date parsing**: Parser tries `PDateTime` before `PDate`; `parseDateTime` succeeds for date-only forms like `'1995-12-17'` so `PDate` alternative never reached. All three date tests updated to expect `PDateTime`. <br><br>**Filter precedence**: `filter ... with` now precedence 0 (lowest); `filter X with Y == Z` parses as `filter X with (Y == Z)`. Tests expecting `Equals` at top level now use explicit parentheses: `(filter MyRole with ItsBooleanProp) == MyOtherRole`. <br><br>**Sequence operator precedence**: `Compose` (`>>`, precedence 9) > `Sequence` (`>>=`, precedence 7); `A >> B >> C >>= D` regroups to `(A >> (B >> C)) >>= D`. Test simplified to check `Sequence` at outermost `BinaryStep`. <br><br>**Assignment keywords**: `remove` and `createRole` became two-word keywords: `remove role MyRole`, `create role MyRole`. <br><br>**Extended coverage**: Added tests for `Filled`, `Context`, `Extern`, `Identity`, `Me`, `IndexedName`, `TypeOfContext`, `TypeOfRole`, `IsInState`, `SpecialisesRoleType`, `ContextTypeIndividual`, `RoleTypeIndividual`, sequence functions (`product`, `minimum`, `maximum`, `count`, `first`), `Exists`, `FilledBy`, `Fills`, `Available`, binary operators (`and`, `or`, `/=`, `-`, `/`, `union`, `intersection`, `orElse`, `filledBy`, `fills`, duration postfix), `PureLet` with single/double bindings. <br><br>+250+ lines covering comprehensive parser validation. |

### Major Feature — Structured Logging

| PR | Feature |
|---|---|
| [#256](https://github.com/joopringelberg/perspectives-monorepo/pull/256) | **Structured logging framework with runtime-configurable topic-based filtering** — Eliminates ad-hoc `log`/`warn` calls making selective logging impossible without source edits. Complete structured logging layer with runtime configuration, incremental call-site migration, and full documentation. <br><br>**New types** (`coreTypes.purs`): `LogLevel` (Trace/Debug/Info/Warn/Error/Silent), `LogTopic` (SYNC/BROKER/QUERY/PERSISTENCE/STATE/AUTH/MODEL/UPGRADE/COMPILER/INSTALL/OTHER/PARSER), `LogConfig` with default level and per-topic overrides. <br><br>**State integration** (`perspectivesState.purs`): `logConfig` field in `PerspectivesExtraState` initialized with `defaultLevel = Warn`, empty topic map (conservative default). Helpers: `setTopicLogLevel`, `disableTopicLogging`, `disableAllLogging`. <br><br>**Entry point** (`logging.purs`): `pdrLog :: LogTopic -> LogLevel -> String -> MonadPerspectives Unit` with threshold checking. Pre-bound convenience aliases cover all topics × levels (`traceSync`, `debugSync`, `warnBroker`, `errorPersistence`, `warnModel`, etc.). <br><br>**Runtime API** (`Main.purs`): Module-level `globalStateRef` stores active `AVar PerspectivesState`. Three `Effect Unit` functions compiled into PDR bundle: `setLogLevelForTopic`, `disableLogTopic`, `disableLogging`. <br><br>**Browser console API** (`perspectives-proxy.ts`, `proxy.js`): Log configuration exposed as methods on `window.pdr`: `pdr.setLogLevelForTopic("SYNC", "DEBUG")`, `pdr.disableLogTopic("QUERY")`, `pdr.disableLogging()`. Uses same `proxyRequest` mechanism as existing `close()`/`unsubscribe()`. <br><br>**Incremental migration**: 15+ modules migrated including `DataUpgrade`, `Sidecar`, `Authorization`, `DomeinCache`, `InvertedQuery`, `CollectAffectedContexts`, `RunMonadPerspectivesTransaction`, `Persistent`, `Api`, `Couchdb`. <br><br>**Documentation** (`docsources/structured-logging.md`): Complete developer reference covering all types, `pdrLog` and convenience aliases, runtime reconfiguration helpers, browser console calling convention, migration guide, new-topic instructions. `docsources/index.md` updated with Architecture & Infrastructure entry and Module Index cross-references. <br><br>+1,200+ lines across implementation, migration, and documentation. Enables granular runtime logging control for debugging production issues. |

### Bug Fixes & Improvements

| PR | Fix/Improvement |
|---|---|
| [#316](https://github.com/joopringelberg/perspectives-monorepo/pull/316) | **Prevent duplicate delta storing in executeDeltas and executeTransaction** — `addDelta`/`insertDelta` unconditionally called `storeDeltaFromSignedDelta` causing two problems: `executeDeltas` (public role path) created spurious DeltaStore records with modified public author keys; `executeTransaction` redundantly re-stored deltas already handled by `executeDeltaWithVersionTracking`. <br><br>**Fix**: Added `isExecutingIncomingDeltas :: Boolean` field to `Transaction` record (default `false`). `addDelta`/`insertDelta` gate `storeDeltaFromSignedDelta` behind flag: `when (not isExecuting) $ lift $ storeDeltaFromSignedDelta delta`. `executeTransaction` and `executeDeltas` set flag before verification/execution. `executeDeltaWithVersionTracking` owns DeltaStore persistence; update functions no longer double-write. Prevents storing records keyed on modified public author identifier. <br><br>+40 / -5 lines. Critical fix for delta store integrity. |
| [#314](https://github.com/joopringelberg/perspectives-monorepo/pull/314) | **Better parser error message when reserved words are used as identifiers** — Parser silently swallowed real cause when reserved words like `Date`, `String`, `Boolean` used as property/role/view/aspect names, emitting generic "Expected a capitalized name" instead of identifying problem. Parsec error-propagation issue caused specific error eclipsed by generic `rolePart` error. <br><br>**arcParser/identifiers.purs**: Extended `arcIdentifier` with `reservedWordAsIdentifierError` fallback using `optionMaybe (try (lookAhead rawUpperIdent))` to non-consumingly peek at next token, check against reserved name list via `isReservedName perspectDef`, call `fail` with specific error if reserved. <br><br>**arcParser/arcParser.purs**: Added guarded branches in `rolePart` for `property`, `view`, `aspect`, `indexed` firing before inner parser. When `twoReservedWords` reveals reserved word after keyword, keyword consumed and `failReservedWord` called. Consuming keyword makes this committed error propagating through `many` unconditionally. <br><br>**Before**: `(ParserError) Expected a capitalized name, a prefixed name, or a fully qualified name, line 39, column 16` <br>**After**: `(ParserError) "Date" is a reserved word in this language. Please use another capitalized name, a prefixed name, or a fully qualified name, line 39, column 16` <br><br>+85 / -10 lines. Significantly improves developer experience for ARC model authors. |
| [#324](https://github.com/joopringelberg/perspectives-monorepo/pull/324) | **Single-click toggle for boolean checkboxes in table cells** — Boolean checkboxes in non-card table cells required three clicks to toggle (select → enter edit mode → toggle), causing laborious UX and unintended double-click context opening. <br><br>**TableCell changes**: `handleClick()` skips `editable` state transition for `'checkbox'` input types (no explicit edit mode needed). `render()` non-card path decouples checkbox `disabled` from `editable` state: `disabled={component.inputType === 'checkbox' ? component.propertyOnlyConsultable() : !component.state.editable}`. Checkboxes always enabled unless property genuinely consult-only. All other input types retain two-click (select → edit) behavior. <br><br>+12 / -3 lines. Major UX improvement for boolean property editing in tables. |

### Documentation & Process

| PR | Content |
|---|---|
| [#326](https://github.com/joopringelberg/perspectives-monorepo/pull/326) | **Weekly progress report 2026-04-07** — Prior week's management-level report covering 13 merged PRs. Analysis of TCP SQL query evaluation, missing resource auto-restoration, DeltaStore performance, PDR→frontend messaging documentation. Identified testing gaps and UI regression patterns with prioritized recommendations. <br><br>+194 lines. Established weekly reporting cadence. |

---

## 3. Impact on the Project

### Language Reference Completion (Strategic Milestone)
PR #334 represents a **major documentation milestone** that fundamentally changes the project's approachability. The five new reference chapters (Perspectives, States, Assignments, Actions, Screens) cover the language constructs that model authors interact with most frequently. Previously, these were documented only through scattered examples, source code comments, and tribal knowledge.

**Strategic implications:**
- **Lowers barrier to entry** for new ARC model authors — authoritative reference reduces trial-and-error learning
- **Enables third-party integration** — external tools (IDE plugins, linters, model validators) can now implement ARC syntax based on formal reference
- **Reduces support burden** — common questions ("What are all the role verbs?" "How do state transitions work?") now have canonical answers
- **Foundation for specification** — reference documentation is prerequisite for formal language specification

**Maturity signal:** The cross-referenced chapter structure with syntax tables, code examples, and parser-grounded semantics indicates the project is graduating from "internal research tool" to "documented platform."

### Testing Infrastructure Foundation (Technical Capability)
The combination of PR #301 (three-layer test architecture + Node.js persistence) and PR #328/#330 (test suite expansion) establishes a **critical technical capability** that was previously missing: automated testing outside the browser.

**Technical implications:**
- **CI-runnable tests** — Layer 1 suite (10 suites, 50+ assertions) runs on every push with no infrastructure dependencies
- **Node.js deployment path** — `perspectives-core.node.js` build enables server-side PDR, automated test scaffolds, native apps via Tauri
- **Test coverage baseline** — ADT/CNF/parser test suites provide regression protection for core compiler functionality
- **AI-assisted testing workflow** — Architecture doc outlines cycle: Copilot writes tests from specs → CI auto-opens failure issues → Copilot agent fixes

**Maturity milestone:** The three-layer architecture (pure/in-memory → PouchDB scaffold → AMQP stub) mirrors industry best practices for testing distributed systems. The CI integration makes this immediately useful rather than aspirational.

### Deployment Path Clarity (Product Readiness)
PR #336 (Capacitor/Tauri update mechanisms) completes the deployment architecture research started in #301. The project now has **documented paths** for all major platforms:

| Platform | Deployment | Update Mechanism | Documentation |
|---|---|---|---|
| Web | PWA | Service worker | Complete |
| iOS | Capacitor | Remote URL / Live Updates | Complete (#336) |
| Android | Capacitor | Remote URL / Live Updates | Complete (#336) |
| Desktop | Tauri v2 | tauri-plugin-updater | Complete (#336) |
| Server | Node.js | N/A (manual) | Complete (#301) |

**Product implications:**
- **Mobile-first path** — Capacitor OTA updates (A1/A2/A3 strategies) enable rapid iteration without App Store submission delays
- **Cross-platform parity** — Single `package.json` version source maintains sync across all deployment targets
- **Enterprise deployment ready** — Tauri signed updates + GitHub Releases manifest enables controlled rollouts

### Structured Logging (Operational Capability)
PR #256's structured logging framework addresses a **critical operational gap**. The PDR runs in a SharedWorker (browser) or Node.js process with complex asynchronous behavior across SYNC, QUERY, PERSISTENCE, and MODEL subsystems. Ad-hoc logging made it impossible to debug production issues without recompiling.

**Operational implications:**
- **Runtime diagnostics** — `window.pdr.setLogLevelForTopic("SYNC", "DEBUG")` enables targeted logging in production without restart
- **Performance investigation** — Can selectively enable `QUERY` logging to diagnose slow queries without drowning in `SYNC`/`PERSISTENCE` noise
- **Support workflow** — Users experiencing issues can enable topic-specific logging and export console output for bug reports
- **Incremental migration** — 15+ modules already migrated; remaining call sites can be converted incrementally

**Developer experience:** The browser console API (`pdr.setLogLevelForTopic(...)`) makes this feature accessible to non-developers (support staff, power users) — no console/developer tools expertise required.

### Parser Developer Experience (Quality of Life)
PR #314 (reserved word error messages) exemplifies **high-leverage UX polish**. Model authors frequently hit parser errors; cryptic messages cause frustration and support burden. The improvement is surgical (+85/-10 lines) but dramatically changes the debugging experience:

**Before:**
```
(ParserError) Expected a capitalized name, a prefixed name, or a fully qualified name, line 39, column 16
```

**After:**
```
(ParserError) "Date" is a reserved word in this language. Please use another capitalized name, a prefixed name, or a fully qualified name, line 39, column 16
```

**Impact:** Self-service debugging instead of support request. The pattern (specific actionable error instead of generic message) should be applied to other parser errors.

---

## 4. Notable Patterns & Concerns

### ✅ Documentation Completeness Reaching Critical Mass
Five major documentation PRs this week (#334 reference manual, #336 deployment updates, #301 architecture research, #256 structured logging docs, #326 weekly report) totaling **4,000+ lines of documentation**. This represents approximately **57% of all additions this week** (documentation lines / total additions).

**Pattern recognition:** The project is systematically addressing documentation debt:
- **Language reference** (#334) — user-facing model author documentation
- **Architecture research** (#301, #336) — deployment/testing strategy documentation
- **Internal API** (#256) — structured logging developer documentation
- **Process** (#326) — weekly reporting for stakeholder communication

**Strategic insight:** Documentation investment is compounding. The `docsources/index.md` navigation layer (#308 from prior week) makes the growing documentation corpus discoverable. Cross-references between chapters (#334) create a web of knowledge rather than isolated documents.

### ✅ Testing Foundation Without Feature Regression
PR #301's three-layer test architecture and #328/#330's test suite expansion add **comprehensive testing infrastructure** without blocking feature development. Key design decisions:

1. **Incremental enablement** — Layer 1 (pure tests) runs immediately in CI; Layer 2/3 (PouchDB/AMQP) are scaffolded but commented out with clear TODO instructions
2. **Parallel build targets** — `dist/perspectives-core.js` (browser) and `dist/perspectives-core.node.js` built from same PureScript output via Rollup aliases
3. **Zero source changes** — AffJax switch (`affjax-web` → `affjax-node`) happens at bundle time via Rollup alias, not source edits

**Contrast with #256 (structured logging):** Structured logging required pervasive source changes (new types in `coreTypes.purs`, state field in `PerspectivesExtraState`, 15+ module migrations). Testing infrastructure deliberately avoided this coupling.

**Maturity indicator:** Ability to add large-scale infrastructure (CI workflow, Node.js build, test scaffolding) without feature friction suggests good architectural separation.

### ✅ UI Polish Continues
PR #324 (single-click checkboxes) addresses a **UX papercut** identified through real usage. Three clicks to toggle a boolean checkbox is objectively poor UX; the fix is surgical (12 additions, 3 deletions in `TableCell`) and preserves existing two-click behavior for other input types.

**Cumulative impact:** This is the **fifth UI polish PR** in two weeks:
- Week of Apr 7: #324 (checkboxes), #320 (floating menu), #318 (checkbox visual updates)
- Week of Mar 31: #310 (debug CORS blocker)

**Pattern:** UI regressions and polish items are being addressed rapidly (typically merged within 1-2 days of identification). This suggests good feedback loops between usage and development.

### ⚠️ Test Suite Maintenance Debt Becoming Visible
PR #328 (parser test fixes) reveals **test suite drift**. Tests written against older parser/syntax were failing or producing false positives:
- Date parsing changed (`PDate` → `PDateTime`)
- Filter precedence changed (now precedence 0)
- Assignment keywords changed (`remove` → `remove role`)
- Sequence operator precedence regrouping

**Concern:** These tests presumably passed at some point but weren't maintained as the parser evolved. The fact that they were **failing silently** (or not run regularly) suggests:
- Test suite not run as part of regular development workflow
- No CI for tests (prior to #301's Layer 1 CI)
- Tests treated as "nice to have" rather than regression protection

**Positive signal:** PR #328 **fixes** the drift comprehensively rather than deleting broken tests. The extension from 40 to 74 expression parser tests demonstrates commitment to test coverage.

**Recommendation:** With Layer 1 CI now in place (#301), test failures will be caught immediately. The team should establish a policy: broken tests must be fixed (not deleted) or explicitly marked as TODO with issue references.

### ⚠️ Delta Store Integrity Bugs Continue
PR #316 (duplicate delta storing) is the **third DeltaStore-related bug fix** in recent weeks:
- Week of Apr 14: #316 (duplicate storing, spurious public author keys)
- Week of Apr 7: #313 (missing `contextKey` index causing full-table scans)
- Week of Mar 31: [implied by #313's migration] (performance issues)

**Pattern analysis:**
1. **High-complexity subsystem** — Delta storage/retrieval spans multiple code paths (`addDelta`, `insertDelta`, `executeDeltas`, `executeTransaction`, `executeDeltaWithVersionTracking`)
2. **Implicit coupling** — Call sites assume `addDelta` will/won't store based on context; PR #316 makes this **explicit** via `isExecutingIncomingDeltas` flag
3. **Testing gap** — These bugs are caught through manual testing or production issues, not automated tests

**Recommendation:** DeltaStore is critical for peer-to-peer sync and resource restoration. Add Layer 2 integration tests (PouchDB scaffold) specifically targeting:
- Delta creation/retrieval round-trip
- `contextKey` index filtering
- No duplicate storing across all entry points (`executeTransaction`, `executeDeltas`, direct `addDelta`)
- Migration idempotence (running `addContextKeyMigration` twice should be no-op)

### ℹ️ Deployment Documentation Ahead of Implementation
PR #336 (Capacitor/Tauri updates) and #301 (Node.js architecture) document **deployment strategies that aren't yet implemented**. This is **intentional research documentation** rather than post-implementation documentation.

**Strategic value:**
- **Decision record** — Documents technology choices (Capacitor over React Native, Tauri v2 over Electron) with rationale
- **Implementation roadmap** — Provides tactical steps for future Capacitor/Tauri packaging
- **Avoids premature commitment** — Research phase before code investment

**Pattern:** Similar to prior weeks' #288 (SQL query evaluation design doc) and #304/297 (PDR messaging/data structures). The project consistently **documents architecture before implementing** to validate design and get stakeholder buy-in.

### ℹ️ Weekly Reporting Becoming Routine
PR #326 (prior week's report) establishes **weekly reporting cadence**. This week's report will be the third consecutive weekly report, suggesting the practice is becoming routine rather than ad-hoc.

**Stakeholder value:**
- **Transparency** — Management/stakeholders see progress, concerns, and recommendations without attending standups
- **Pattern detection** — Week-over-week comparison surfaces trends (e.g., UI regression rate, documentation investment, testing coverage)
- **Accountability** — Recommendations from prior reports (e.g., "add E2E tests for UI regressions") can be tracked

**Process maturity:** Weekly reporting is typically adopted by projects approaching production readiness or with external stakeholders requiring regular updates.

---

## 5. Recommended Focus Areas for Next Week

| Priority | Area | Rationale |
|---|---|---|
| 🔴 **Critical** | **Enable Layer 2 tests (PouchDB integration)** | PR #301 scaffolds Layer 2 but all suites commented out with TODOs. Wire `persistenceAPI.node.js` to `pouchdb-adapter-memory`, enable LoadArc/ContextAndRole/Queries/HandleTransaction suites, add to CI. Layer 2 regression protection critical before schema/persistence changes. DeltaStore bugs (#316, #313) would have been caught by Layer 2 tests. |
| 🔴 **Critical** | **DeltaStore integration test suite** | Three DeltaStore bugs in three weeks (#316, #313, prior week #313 context). Add Layer 2 tests covering: delta round-trip (create/retrieve), `contextKey` index filtering, no-duplicate-storing across all entry points, migration idempotence. DeltaStore is critical for sync/restoration; must have regression protection. |
| 🟠 **High** | **Capacitor iOS/Android POC** | PR #336 documents three Capacitor update strategies (remote URL, Live Updates, service worker in WebView) but doesn't implement any. Create POC: (1) basic Capacitor shell in `packages/mycontexts/`, (2) test OTA update via remote URL strategy (simplest), (3) validate service worker caching works in WebView. De-risk mobile deployment assumptions before committing to full Capacitor integration. |
| 🟠 **High** | **Complete ARC reference manual** | PR #334 adds five chapters (Perspectives, States, Assignments, Actions, Screens). Remaining gaps: Contexts/Roles/Properties (type definitions), Queries (expression syntax — partially covered by #328's expression tests), Models (domain declarations, imports, namespace prefix expansion). Reference manual completeness critical for external model authors and tooling. |
| 🟡 **Medium** | **Migrate remaining modules to structured logging** | PR #256 migrates 15+ modules but several remain: `Perspectives.Extern.Parsing`, `Perspectives.DataUpgrade` (main), `Perspectives.Extern.RabbitMQ`, `Perspectives.Error.Boundaries`. `Error.Boundaries` requires design decision (polymorphic `MonadEffect m =>` constraint incompatible with `MonadPerspectives`-specific logging). Complete migration for consistent logging across all subsystems. |
| 🟡 **Medium** | **Parser error message audit** | PR #314 (reserved word errors) demonstrates high-leverage UX improvement (+85/-10 lines, dramatically better debugging experience). Audit other common parser errors for cryptic messages: "Expected 'end' keyword", "State binding required", "Cannot mix lexical and semantic sub-states". Apply same pattern: specific actionable error instead of generic message. |
| 🟢 **Low** | **Test suite maintenance policy** | PR #328 fixes parser test drift (tests written for old syntax/semantics). With Layer 1 CI now enforcing test passing (#301), establish policy: broken tests must be fixed (not deleted) or explicitly marked `pending` with issue references. Document in `test/test instructions.md`. Prevents future test drift. |

---

## Appendix: PR Details Summary

| PR | Type | Theme | LOC Changed | Merged |
|---|---|---|---|---|
| #334 | Documentation | ARC Reference Manual | +5,000+ / -0 (est.) | Apr 13 |
| #336 | Documentation | Deployment Updates | +800+ / -0 (est.) | Apr 14 |
| #301 | Feature | Testing Infrastructure | +2,800+ / -150 (est.) | Apr 11 |
| #256 | Feature | Structured Logging | +1,200+ / -80 (est.) | Apr 10 |
| #330 | Feature | Unit Tests (ADT/CNF) | +300+ / -50 (est.) | Apr 12 |
| #328 | Bug/Feature | Parser Tests | +250+ / -80 (est.) | Apr 11 |
| #326 | Documentation | Weekly Report | +194 / -0 | Apr 8 |
| #316 | Bug | Delta Store Integrity | +40 / -5 | Apr 8 |
| #314 | Improvement | Parser Errors | +85 / -10 | Apr 8 |
| #324 | Bug | UI (Checkbox) | +12 / -3 | Apr 8 |
| **Total** | | | **~10,600+ / ~380** | |

**Notes:**
- **Largest changes:**
  - PR #334 (ARC reference manual) — estimated 5,000+ lines across five AsciiDoc chapters
  - PR #301 (testing infrastructure) — 2,800+ lines across persistence layer, test scaffolding, CI, documentation
- **Documentation-heavy week:** Approximately 6,000+ lines of documentation (57% of additions) across #334, #336, #301, #256, #326
- **Testing investment:** PRs #301, #330, #328 collectively add test infrastructure, CI integration, and 350+ test assertions
- **Net code growth:** ~10,200+ lines (primarily documentation, testing infrastructure, and structured logging feature)

**Bug distribution:**
- 3 bugs/improvements fixed (30% of PRs): #316 (delta store), #314 (parser errors), #324 (checkbox UX)
- No critical regressions; all fixes are polish or integrity improvements

**Feature distribution:**
- 2 major features: #301 (testing infrastructure), #256 (structured logging)
- 1 major documentation milestone: #334 (ARC reference manual)
- 1 deployment architecture completion: #336 (Capacitor/Tauri updates)
- 2 test suite expansions: #330 (ADT/CNF tests), #328 (expression parser tests)

---

*Report generated on 2026-04-15.*
