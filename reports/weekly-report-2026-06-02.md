# Weekly Progress Report - 2026-06-02

## Executive Summary

This week focused on runtime stability and API robustness with two critical bug fixes. The delayed state-exit crash that caused `TypeError: J is not a function` has been resolved through proper fiber lifecycle management. Action resolution now handles both qualified and unqualified action names, eliminating failures when state-dependent actions are invoked by readable names.

---

### 🎯 Top Achievements (Max 5)

- **Stabilized delayed action lifecycle**: Delayed `on entry ... after` actions now properly deregister their fibers before execution, preventing state-exit cleanup from targeting active transaction paths. This eliminates the recurring `TypeError: J is not a function` crash that left the runtime in a stuck processing state.

- **Flexible action name resolution**: Object action API now accepts both fully-qualified action identifiers and readable/unqualified names (e.g., `CreateInvitation`). A new fallback lookup path matches by local action name across state-scoped action maps, mirroring existing context-action behavior.

---

### 🐛 Key Bugs Fixed (Max 3)

- **Delayed state-exit crash**: Postponed transaction fibers remained registered while their delayed transactions executed, causing state-exit cleanup to crash with `TypeError: J is not a function`. Fixed by deregistering fibers immediately before running delayed transaction bodies, ensuring one-shot delayed work doesn't collide with state lifecycle.

- **State-dependent action resolution failures**: Actions on `System$Invitation$External` and similar state-dependent perspectives failed when callers passed unqualified names while perspectives stored qualified identifiers. Added `getActionFromUnqualifiedName` helper and dual-path resolution in `Api.Action` to handle both name formats.

---

### 📊 Impact (2-3 sentences total)

These fixes directly address production stability issues affecting delayed actions and state transitions. Users can now rely on delayed action execution without runtime crashes, and action invocation APIs accept natural readable names without requiring fully-qualified identifiers. Both changes reduce runtime errors and improve developer experience when working with state-dependent behaviors.

---

### ⚠️ Concerns (If any, max 3)

- **Timed transaction queue continuity**: The fix to `PostponedTransaction` handling restores recursive queue consumption that was missing. Monitor for any side effects in systems with many concurrent delayed actions.

---

### 🎯 Focus for Next Week (Max 3)

- **Validate fiber lifecycle fixes in production**: Test delayed actions with varying durations and verify state-exit cleanup handles all timed transaction types correctly.

- **Monitor action resolution performance**: Dual-path lookup adds fallback overhead; profile action invocation in state-heavy models to ensure acceptable latency.

- **Document action name conventions**: Update API documentation to clarify that both qualified and unqualified action names are now supported for consistency with context actions.

---

**Report generated:** 2026-06-02
**Reporting period:** Week ending 2 June 2026
**Merged PRs analyzed:** #407, #404
**Word count:** ~415 words
