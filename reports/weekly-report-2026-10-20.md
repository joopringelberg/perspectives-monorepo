# Weekly Progress Report – 2026-10-20

### 🎯 Top Achievements

- **More responsive transactions (#537):** User-triggered mutations and model-loading requests no longer hold up independent GUI reads. Per-PDR FIFO user and background lanes prioritize user work while admitting background work after at most four user jobs.

- **Nonblocking incoming-work status (#537):** Incoming-message progress now appears in the bottom navbar without covering the GUI or changing the cursor; it clears when the backlog reaches zero.

- **Request timing diagnostics (commit `3b6ed39a2b82`):** The proxy records request durations, failures, stale timeouts, and overlay visibility across request categories, retaining up to 500 samples per browser installation for console summaries.

- **More reliable test runs (commits `784474e500c3`, `c99fab9ba764`, `cdd57041e299`):** PDR tests now start with clean in-memory databases, use shorter time limits and a shared snapshot, and T10 specifically tests a postponed absence condition after role removal.

- **Clearer model permissions (commit `d8b617f1ed34`):** Three built-in models adjust property visibility or removal permissions to expose the needed information and context-level operation.

### 🐛 Key Bugs Fixed

- **GUI reads delayed by transaction work (#537):** Pure `GetRoleBinders` and `CheckBinding` requests now use the ordinary read path, and canceled subscriptions cannot register late.

- **Incoming work blocked interaction (#537):** System progress uses a separate navbar status channel, preserving the existing blocking overlay for operations that need it.

- **Test data leaked between PDR instances (commit `784474e500c3`):** Process-wide in-memory databases are destroyed before a user's PDR is restarted or a snapshot restored.

### 📊 Impact

Users can continue reading and navigating while transaction work or incoming messages are processed, and user mutations receive fairer admission. Developers also gain persistent timing evidence and more reproducible tests.

### ⚠️ Concerns

- **Layer 1 check failed on merged #537:** The final check reported `couchdbManagement@12.4.arc captures server coordinates before binding the local server role` failed with “expected 2, got 0”; the cause remains unresolved.

- **Compiler warning work is branch-only:** Commit `250e811d00c6` adds a tested warning for peer perspectives on indexed root contexts, but was reachable only from `copilot/extra-compiler-check-indexed-root-contexts`, not a merged PR or `master`.

- **Validation was incomplete:** The PR activity comment reports CodeQL timed out and package ESLint still had 36 baseline errors.

### 🎯 Focus for Next Week

- Investigate the failing Layer 1 test and confirm a passing run after the fix.

- Decide whether to integrate the indexed-root perspective warning, and validate it on the target branch.

- Use the new timing summaries to evaluate transaction and overlay latency against real usage.
