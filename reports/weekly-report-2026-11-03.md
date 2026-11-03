# Weekly Progress Report – 2026-11-03

### 🎯 Top Achievements

- **Standalone ARC editor (#551):** A Monaco-based editor can open a model manifest from MyContexts, share its existing SharedWorker/PDR, and save through the established ARC attachment flow. It adds language-server diagnostics, completion, navigation, and reference tools without starting a second PDR.

- **Safer, clearer ARC parsing (#332, #546, #547):** Parser state now rolls back correctly after failed alternatives; grammar handling and expression parsing were rationalised while preserving ASTs and established precedence. A broad syntax suite and real-model compilation harness improve regression coverage; Layer 1 passed 427 tests (2 skipped).

- **Safer publishing and synchronization (direct commit; #539):** Public resources are saved before peers are notified, preventing peers from receiving references to resources not yet available. The compiler now warns when peer perspectives on indexed root contexts may trigger unwanted synchronization, and typed public-context references receive runtime checks.

- **More reproducible integration tests (direct commit):** Rebooted snapshots are separated from seed snapshots, AMQP subscriptions use fresh accounts/queues with cleanup, and snapshots are published only after nonempty successful runs.

- **Architecture and documentation groundwork (#498, #544; direct commits):** Native-shell and persistence/messaging handoffs frame Capacitor/Tauri and RxDB/Supabase/PGMQ as proposals, not shipped migrations. The language reference moved into a buildable monorepo package with contributor guidance.

### 🐛 Key Bugs Fixed

- **Public-resource race (direct commit):** Peers could learn of public resources before persistence, leading to missing-resource errors and a crashing integrity fixer; publication now saves public resources before sending peer transactions.

- **Parser state leaked across backtracking (#546):** Failed parses could leave context, state, or role labels mutated; scoped parser state now restores on both success and errors while preserving the original parse error.

### 📊 Impact

ARC authoring and parsing are better supported by integrated tooling and stronger regression tests, while public-resource delivery and test-state handling are more reliable. Native packaging and alternative persistence/messaging remain documented proposals, not shipped capabilities.

### ⚠️ Concerns

- **Editor validation is limited:** The cloud smoke test used a fake SharedWorker; it did not exercise a live PDR, CouchDB, or model compilation. The editor also requires same-origin SharedWorker hosting and does not currently support MyContexts’ page-hosted path.

- **Model-corpus validation remains incomplete:** The source compilation harness could not initialize its PDR in the reported environment because the snapshot and trusted CA setup were unavailable, although Layer 1 passed.

- **The pageworker fix is branch-only:** Do not assume the Safari/page-hosted port-relay race is fixed in the merged default branch.

### 🎯 Focus for Next Week

- Validate the editor against a live PDR and CouchDB, and decide how to support or explicitly scope page-hosted browsers.

- Run the model-source compilation corpus with the required snapshot and trusted CA, and report per-model outcomes.

- Test the pageworker race fix and decide whether to merge it; continue native-shell feasibility checks on real devices before committing to platform implementation.
