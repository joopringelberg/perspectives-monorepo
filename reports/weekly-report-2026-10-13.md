# Weekly Progress Report – 2026-10-13

## Executive Summary

This week improved cross-PDR contract termination, transaction delivery, and model recompilation, and added conditional ARC screens. It also specified phase 0/1 model-versioning, delivered two-PDR diagnostics, and expanded ARC references and editor support. Direct `master` commits and merged PRs are included; branch-only work is not.

---

### 🎯 Top Achievements

- **Reliable BrokerServices termination (commit `210175fbb356`):** Either party can terminate through an acknowledgement handshake, preventing loss of the update when RabbitMQ access is removed. Two-PDR tests check contract, user, and queue removal.

- **More expressive ARC screens (commit `06e127bea14c`):** Tabs can be conditional, and compilation warns when a screen references properties outside its user's perspective. The clock sensor gained a `today` value.

- **Versioned model foundations (#518; commit `6ed151c1c95e`):** Compiled types carry model versions, and signed deltas identify revisioned types; the design specifies the phase 0/1 contract. `Published` closes the model's Compile tab.

- **Transaction diagnostics (#531):** A two-PDR harness measures sender execution and recipient observation over in-memory or RabbitMQ, with opt-in profiling. The PR proposes optimizations but does not claim performance gains.

- **ARC guidance and editor support (#533, #529, #523):** New references explain communicated context and model syntax; VS Code colors newer timing, query, and action constructs.

---

### 🐛 Key Bugs Fixed

- **Lost peer transactions (`84af3444cc9d`):** Destinations retain the schemed `PerspectivesUsers` resource for the recipient's transport key, fixing silent drops when a peer is known through `pub:`.

- **Stale model state and identity (`462a3fc1a1b9`, `547c034ad403`):** Recompilation removes old inverted queries; alias pruning preserves a type's CUID when only its query changes.

- **Initialization and credentials (#531; `39012d0bc27b`):** Initial `model:System` installation skips an inapplicable conflict check; credential discovery handles roles without passwords.

---

### 📊 Impact

Users get more reliable peer communication and contract cleanup, while ARC authors gain conditional screens and clearer feedback. Versioning, profiling, and new references help developers evolve models and investigate transaction costs.

---

### ⚠️ Concerns

- **Version compatibility remains limited (#518):** Format `2` uses the versioned parser; other markers fall back to legacy parsing, and full compatibility checking is deferred.

- **Publication enforcement needs verification (`6ed151c1c95e`):** `Published` closes the Compile tab; verify it also prevents upload through the processing path.

- **An early #531 Layer 1 CI failure was followed by passing runs:** The failed runs' job logs were unavailable (the logs endpoint returned 404), so their cause could not be confirmed.

---

### 🎯 Focus for Next Week

- Run benchmarks on prepared snapshots, validate bottlenecks with profiles, and compare optimizations.

- Test unsupported delta markers and the `Published` upload path before relying on these contracts.
