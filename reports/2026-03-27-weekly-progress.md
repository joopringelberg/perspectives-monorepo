# Weekly Progress Report — 2026-03-27

> **Audience:** Management / Stakeholders
> **Period:** Week ending 27 March 2026
> **Scope:** 10 merged pull requests (PRs #261, #264, #266, #269, #271, #273, #276, #279, #281, #284)

---

## 1. Key Achievements

| Theme | Summary |
|---|---|
| **Distributed Sync Reliability** | Three fixes to the peer-to-peer transaction synchronisation pipeline eliminate silent data loss and incorrect peer exclusions in multi-user scenarios. |
| **TCP Server Maturation** | The `perspectives-tcp` package received cascade-delete support, smarter property-column lookup for flattened roles, and improved operational logging. |
| **User Experience Polish** | Misleading error messages and missing visual cues for disconnected peers have been eliminated, reducing user confusion. |
| **Developer Onboarding** | Comprehensive Copilot onboarding instructions were added for all 14 packages in the monorepo, significantly lowering the barrier for new contributors. |
| **Query Subsystem Documentation** | Technical documentation for the query subsystem was published, capturing design rationale and aiding future maintainability. |

---

## 2. Features Added & Bugs Fixed

### TCP Server Improvements

| PR | Change |
|---|---|
| [#284](https://github.com/joopringelberg/perspectives-monorepo/pull/284) | **Cascade-delete on RemoveExternalRoleInstance** — When an external role instance is removed from a context, the TCP server now correctly cascade-deletes the associated context row, preventing stale data. Also added property-based column lookup for flattened roles (enabling correct SQL mapping when roles share property names) and further logging improvements. |
| [#279](https://github.com/joopringelberg/perspectives-monorepo/pull/279) | **Debug logging for delta processing pipeline** — Structured debug-level log entries were added throughout the TCP delta processing path, making it significantly easier to trace and diagnose transaction flow in production. |

### Bug Fixes — Distributed Synchronisation

| PR | Fix |
|---|---|
| [#276](https://github.com/joopringelberg/perspectives-monorepo/pull/276) | **Onlookers not recognised as AMQP peers** — Onlooker roles (TCP-connected observers) were incorrectly excluded from the set of valid AMQP peers during transaction synchronisation. These peers now receive transactions they are entitled to, restoring correct multi-party data distribution. |
| [#271](https://github.com/joopringelberg/perspectives-monorepo/pull/271) | **Missing SetFirstBinding delta in context serialisation** — When a context was serialised and sent to a new peer, the `SetFirstBinding` delta was omitted. This caused the receiving peer to construct an incomplete context instance. The delta is now correctly included. |
| [#281](https://github.com/joopringelberg/perspectives-monorepo/pull/281) | **Calculated user roles not serialised on context creation / new bindings** — New contexts with Calculated user roles and new role bindings failed to send the correct serialised context to those users. This prevented calculated-role users from receiving their initial copy of a context. |

### Bug Fixes — Screen Rendering

| PR | Fix |
|---|---|
| [#264](https://github.com/joopringelberg/perspectives-monorepo/pull/264) | **Screen not constructed when chat role has no instances** — A missing guard caused screen construction to abort entirely when the chat role had zero instances (a normal and expected state). The screen now renders correctly in this case. |

### User Experience Improvements

| PR | Improvement |
|---|---|
| [#261](https://github.com/joopringelberg/perspectives-monorepo/pull/261) | **Misleading error on DisconnectedPeer context** — Opening a `DisconnectedPeer` context triggered a spurious "Could not retrieve context actions" error. The error has been eliminated; the context now opens silently and correctly. |
| [#273](https://github.com/joopringelberg/perspectives-monorepo/pull/273) | **Visual distinction for cancelled/disconnected peer cards** — Role cards for cancelled or disconnected peers now display a distinct visual treatment, making peer connection status immediately visible without requiring users to open the context. |

### Documentation & Developer Experience

| PR | Change |
|---|---|
| [#266](https://github.com/joopringelberg/perspectives-monorepo/pull/266) | **Query subsystem technical documentation** — A comprehensive reference document for the query evaluation engine has been published, covering architecture, query types, assumption tracking, and reactive update flows. |
| [#269](https://github.com/joopringelberg/perspectives-monorepo/pull/269) | **Copilot onboarding instructions for all 14 packages** — Structured Copilot instruction files (`.github/instructions/`) were created for every package in the monorepo, giving AI-assisted development tools accurate context about each package's purpose, conventions, and pitfalls. |

---

## 3. Impact on the Project

### Distributed Data Integrity
Three synchronisation fixes (#276, #271, #281) address scenarios where peers received incomplete or no data. In the P2P Perspectives model, a missed delta or an excluded peer can silently leave a user's local state diverged from the network's ground truth — with no immediate error visible to the user. These fixes materially improve the correctness and completeness of multi-user collaboration.

### TCP Server Production Readiness
The cascade-delete addition to `perspectives-tcp` (#284) closes a data-integrity gap: without it, a removed context's row would persist indefinitely in the reporting database. Combined with the improved logging (#279), the TCP server is now better suited for production deployments where data accuracy and operational observability are requirements.

### User Confidence
The two UX improvements (#261, #273) remove friction points that caused user confusion: a false error notification and an absent visual cue for peer connection status. These changes improve the perceived reliability of the application even when peers are intentionally disconnected or cancelled.

### Developer Productivity
The Copilot instructions (#269) and query subsystem documentation (#266) together represent a significant investment in long-term developer experience. New contributors and AI-assisted tooling now have structured, package-specific guidance — reducing ramp-up time and the likelihood of misaligned changes.

---

## 4. Notable Patterns & Concerns

### ⚠️ Context Serialisation Remains a Fragile Area
Three of the ten PRs this week (#271, #281, #264) relate to incorrect or incomplete context/screen construction. This is the third consecutive week with at least one context serialisation bug. The pattern indicates that the serialisation pipeline — which must assemble and transmit a consistent snapshot of context state to peers or render screens for clients — has edge cases that are not adequately covered by automated tests.

### ⚠️ Synchronisation Pipeline Complexity
Two further PRs (#276, #281) expose subtle issues in how the runtime determines which peers should receive which transactions. This logic is core to the correctness of the distributed system and failures here have data-integrity consequences that are hard for users to detect. The complexity warrants dedicated integration tests covering peer-selection and transaction dispatch.

### ✅ TCP Package Receiving Sustained Attention
`perspectives-tcp` was introduced two weeks ago and has received meaningful improvements in each subsequent week (#284, #279 this week; #279 previously). This iterative approach — ship early, then harden — is appropriate for a new package, and the logging improvements (#279) specifically enable faster feedback cycles.

### ✅ Documentation and Tooling Investment Pays Forward
Both the Copilot onboarding instructions (#269) and the query subsystem docs (#266) are non-functional improvements whose value compounds over time. Documentation produced alongside feature work — rather than as an afterthought — is a healthy practice.

### ℹ️ Moderate PR Volume with High Defect Ratio
Seven of the ten PRs are bug fixes or UX corrections. While this reflects thorough issue tracking, it also suggests that the test surface for the core distributed sync and screen rendering paths is not yet sufficient to catch these issues before merge.

---

## 5. Recommended Focus Areas for Next Week

| Priority | Area | Rationale |
|---|---|---|
| 🔴 High | **Context serialisation integration tests** | Three consecutive weeks of serialisation-related bugs (#271, #281, #264 this week; #236 two weeks ago) indicate a systemic gap. Targeted integration tests for context-to-peer serialisation and screen construction will prevent recurrence. |
| 🔴 High | **Peer-selection / transaction dispatch tests** | Fixes #276 and #281 reveal that the logic for determining which peers receive which transactions has untested edge cases. Adding tests for Onlooker, Calculated-role, and new-binding scenarios will protect this critical path. |
| 🟠 Medium | **TCP server end-to-end tests** | The cascade-delete (#284) and Onlooker recognition (#276) fixes should be backed by automated tests against the TCP pipeline. Ensures the server's SQL correctness can be verified without manual inspection. |
| 🟠 Medium | **Screen rendering edge-case coverage** | The chat-role empty-instance bug (#264) and prior External role rendering bugs suggest that the screen construction logic needs broader coverage of boundary conditions (empty role sets, Calculated roles, External roles). |
| 🟡 Low | **Copilot instructions validation** | Now that instructions exist for all 14 packages (#269), a brief review cycle to ensure accuracy and completeness — particularly for recently changed packages — will maintain their long-term value. |
| 🟡 Low | **Operational monitoring for TCP logging** | The debug logging added in #279 should be reviewed to determine which entries are suitable for promotion to structured INFO-level logs, enabling production dashboards or alerting. |

---

*Report generated by GitHub Copilot on 2026-03-27.*
