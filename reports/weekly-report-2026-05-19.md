# Weekly Progress Report - 2026-05-19

## Executive Summary

This week delivered a major feature release with typeahead widgets for large datasets, plus critical UI consistency fixes. The typeahead system enables efficient work with thousands of role instances using filtered views. Property ordering now honors ARC declarations, file metadata persists reliably, and calculated context roles gain compile-time validation.

---

### 🎯 Top Achievements (Max 5)

- **Typeahead widgets for large datasets**: Three new ARC keywords (`typeaheadfiller`, `typeaheadform`, `typeaheadfillfrom`) enable efficient role selection from thousands of candidates using CouchDB FilterValue views. Users see instant client-side filtering with keyboard navigation; total role counts below 20 display immediately.

- **Property ordering from ARC models**: Tables and forms now respect declared `with props` order from screen definitions. A property declared as `with props (FirstName, LastName, Aanwezig)` renders columns and fields in exactly that sequence, improving model-to-UI consistency.

- **Compiler validation for calculated context roles**: Invalid calculated context roles (those with non-external role ranges) now produce clear `NotAContextRole` errors at compile time. Prevents unclear runtime failures from malformed query pipelines.

- **Preserved PerspectivesFile metadata**: ARC source rendering no longer fails after repeated file uploads. Database field and filename now persist reliably across save operations, eliminating incomplete file descriptors.

---

### 🐛 Key Bugs Fixed (Max 3)

- **Lost database metadata on re-upload**: `saveFileAndProperty` failed to preserve database field across repeated saves, breaking ARC source display. Fixed by deriving `currentDatabase` from component state with fallback to existing property value.

- **Ignored property order declarations**: Runtime ignored `with props` ordering and used server-chosen sequences. Fixed by threading `requiredProperties` through serialization and reordering `availableProperties` with O(n) Set-based lookup.

- **Unclear calculated-role compilation errors**: Non-external role ranges compiled too far before failing. Added explicit guard in `compileAndSaveRole` with reusable `contextRoleRangeHasNonExternalRole` detection logic.

---

### 📊 Impact (2-3 sentences total)

Typeahead widgets eliminate dropdown performance bottlenecks for large role lists, enabling real-world deployments with thousands of participants or records per context. Property ordering and file metadata fixes improve user experience by matching runtime behavior to model declarations and preserving work across sessions. Compile-time validation catches modeling errors earlier, reducing debugging cycles.

---

### ⚠️ Concerns (If any, max 3)

- **Typeahead complexity**: 2,000+ line change touches 67 files across parser, compiler, database views, and three React components. Monitor for edge cases in production models.

- **FilterValue view indexing**: New CouchDB view indexes all roles with Filter aspect. Watch database size and replication performance in large installations.

---

### 🎯 Focus for Next Week (Max 3)

- **Validate typeahead in production**: Test keyboard navigation, selection workflows, and FilterValue view performance with real-world datasets exceeding 1,000 candidates.

- **Monitor property ordering edge cases**: Verify behavior when models evolve (properties added/removed) and `with props` lists become stale or incomplete.

- **Database view optimization**: Profile FilterValue view query performance and consider compound key strategies if indexing becomes a bottleneck.

---

**Report generated:** 2026-05-19
**Reporting period:** Week ending 19 May 2026
**Merged PRs analyzed:** #400, #394, #396, #375, #398
**Word count:** ~485 words
