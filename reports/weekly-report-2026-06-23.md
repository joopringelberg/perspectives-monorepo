# Weekly Progress Report - 2026-06-23

## Executive Summary

This week's work focused on data integrity and correctness across the Perspectives runtime. Four substantive changes landed: a versioned data-upgrade migrating legacy user identity to CUID-based IDs, a deterministic delta sort-at-distribution strategy replacing brittle insertion-order logic, automatic sidecar alias pruning to prevent FQN reuse corruption, and a correctness fix to filler type matching that aligns both execution paths through full CNF expansion.

---

### 🎯 Top Achievements (Max 5)

- **Legacy `PerspectivesSystem$User` identity migration** (#437): A new versioned upgrade function (`3.3.4`) detects installations that still store the user role under the legacy `def:#[system]$User` key, rewrites the document under the stable CUID-based key `def:#[system]$auftu9ldl2`, and repairs all backlinks (`binding`, `filledRoles`, `rolInContext`, `me`, `buitenRol`). Older installations can now upgrade cleanly without losing their user-role identity.

- **Sort-at-distribution delta ordering** (#429): Transaction deltas are now appended unconditionally during construction and sorted once in `distributeTransaction` using a deterministic `(priority, resourceVersion, resourceKey)` comparator covering all 17 delta types. The fragile `insertDelta`/`insertionPoint`/`withRoleInsertionPoint` machinery was removed, making the transaction pipeline simpler and more robust to new code paths.

- **Automatic sidecar alias pruning** (#433): `pruneStaleAliases` now removes alias entries whose key has become a canonical FQN in the current model, along with the corresponding CUID entry, so reintroduced type names receive a fresh CUID instead of inheriting the one belonging to the previously renamed type. The pruning runs automatically at the start of both `mergeStableIdMapping` and `planCuidAssignments`.

- **Filler type CNF normalization** (#435): `getFillerTypeRecursively` in both `UnsafeCompiler` and `Interpreter` now normalizes the requested ADT through `expandUnexpandedLeaves` and `toConjunctiveNormalForm` before comparison, aligning it with the cached `completeType` used on the candidate side. Focused Layer 1 regressions for CNF comparison semantics and recursive-matching edge cases were added.

---

### 🐛 Key Bugs Fixed (Max 3)

- **Legacy user-role lookup failure** (#437): After the canonical `PerspectivesSystem$User` suffix changed from `User` to `auftu9ldl2`, existing installations stored the role under the old key, causing all lookups against the new canonical id to fail silently. The migration function resolves this for all affected installations as part of the startup upgrade pipeline.

- **Filler type comparison divergence** (#435): `getFillerTypeRecursively` compared an unexpanded `ADT RoleInContext` against a CNF-expanded cached `completeType`, causing valid role bindings to be spuriously rejected when the declared type contained unexpanded leaves. Normalizing both sides through the same expansion path fixes the divergence.

- **Silent FQN reuse corruption via stale sidecar aliases** (#433): When a type `A` was renamed to `B` (recording alias `A → B`) and a new type named `A` was later introduced, the stale alias silently redirected the new `A` to `B`'s CUID, corrupting identity lookups. The pruning step removes such entries before any CUID assignment takes place.

---

### 📊 Impact (2-3 sentences total)

The user identity migration and sidecar alias pruning fixes are the highest-impact reliability improvements in recent weeks: they prevent silent data corruption and lookup failures that would otherwise surface only after deployment to real user installations. The delta sort-at-distribution refactor reduces maintenance risk by eliminating a class of ordering bugs that had recurred repeatedly as new code paths were added. Together, these changes materially improve upgrade safety and type-system correctness in the PDR.

---

### ⚠️ Concerns (If any, max 3)

- **Upgrade pipeline completeness**: The `3.3.4` migration assumes a sequential upgrade path; installations that skipped intermediate versions may need careful verification that all prior upgrade steps have run before this one executes.

- **Sidecar pruning is irreversible**: Once a stale alias is pruned and the corresponding CUID removed, the old rename history is lost. Models that rely on the alias for cross-version compatibility should be validated before upgrading the compiler.

---

### 🎯 Focus for Next Week (Max 3)

- **Validate the upgrade pipeline on real installations**: Run the `3.3.4` migration against a representative set of older user databases and confirm that backlinks are repaired correctly and no data is lost.

- **Re-enable and expand Layer 3 sync tests**: With delta ordering now deterministic, resume wiring in-memory PouchDB and loading the system model in the Layer 3 test scaffold to activate the `SetProperty → GetProperty` suite.

- **Continue investigating Invitation synchronisation bug**: The corrected filler type matching may affect the `Invitation$Inviter` delta delivery path; re-run the documented call-tree analysis against the updated code to determine whether the root cause is resolved or still present.

---

**Report generated:** 2026-06-23
**Reporting period:** Week ending 23 June 2026
**Merged PRs analyzed:** #429, #433, #435, #437
**Word count:** ~490 words
