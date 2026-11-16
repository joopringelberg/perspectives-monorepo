# Weekly Progress Report – 2026-10-27

No PRs were merged during this window; the substantive commits reviewed were reachable from `master`.

### 🎯 Top Achievements

- **Safer RabbitMQ subscription lifecycle (commits `8afa3aac8c63`, `326577a96cf6`):** Broker termination now waits for confirmed account and queue deletion before removing local contract records; failures leave records available for inspection. Dedicated two-PDR tests exercise both administrator-led termination and account-holder unsubscribe, with checks of broker resources and contract removal.

- **Reliable local model recompilation (commits `5f5d87ca2e34`, `3eb99bde2ca9`):** Recompilation now invalidates cached state actions and queries and replaces obsolete inverted queries. A regression test covers moved states and verifies the updated definitions and query cache.

- **More configurable synchronization tests (commit `3569a8fa40b5`):** The test scaffold can patch and recompile installed models in both PDRs while preserving stable IDs, and aborts setup on patch failures. AMQP tests now compile the current test model and pass administrator credentials into the test harness.

- **Safer removal replay and clearer server setup (commits `197f08266b35`, `d9d22b0c97c6`):** Post-removal state evaluation checks that role and context subjects still exist before reading their states. A CouchDB model test now asserts that server coordinates are copied onto the created server context.

### 🐛 Key Bugs Fixed

- **Deleted subjects stalled state evaluation (`197f08266b35`):** Post-removal replay could try to repair already-deleted resources; non-repairing existence checks now exclude them while retaining surviving subjects.

- **Recompilation left stale behavior (`5f5d87ca2e34`, `3eb99bde2ca9`):** Cached state actions and query results could still refer to the prior model after local recompilation; the relevant caches and old model queries are now cleared.

- **Broker cleanup could outpace external deletion (`8afa3aac8c63`):** RabbitMQ deletion results now inform the model, which retains the local contract until account and queue deletion are confirmed.

### 📊 Impact

Broker-service teardown is less likely to leave external resources out of sync with local records, and model recompilation and post-removal processing use current definitions. Developers also have more realistic broker lifecycle coverage and reusable synchronization-test setup.

### ⚠️ Concerns

- **AMQP contract-termination test remains incomplete (`2b384354af45`):** The process runs, but the generic scaffold polls Bob for success after his connection is dismantled; the commit notes that this prevents the test from succeeding.

### 🎯 Focus for Next Week

- Adjust the AMQP test scaffold to observe termination success from the still-connected PDR, then confirm a passing run.

- Run the broker lifecycle tests against failed or delayed RabbitMQ deletion and verify that retained records can be safely retried or inspected.

- Validate the recompilation and deleted-subject regressions in the relevant core test suites.
