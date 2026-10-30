# Issue Keywords for Copilot Alerts

This file collects keywords that can be added to future issues to signal that a
specific technical topic is in scope.

Use these keywords in issue titles and/or descriptions when you want Copilot to
apply the matching documentation and existing implementation patterns.

## Initial keyword set (max 10)

| Keyword | Topic to signal | Primary documentation source |
|---|---|---|
| `TYPE-COMPARISON` | CNF-based type comparison and existing predicates | `packages/perspectives-core/docsources/type-comparison.md` (sections 4 and 5) |
| `TYPE-LATTICE` | Generalisation/specialisation reasoning in the type lattice | `packages/perspectives-core/docsources/type-comparison.md` (section 6) |
| `CNF-NORMALISATION` | ADT/ExpandedADT to CNF normalisation pipeline | `packages/perspectives-core/docsources/type-comparison.md` (section 3) |
| `QUERY-INVERSION` | Compile-time inversion and runtime affected-context lookup | `packages/perspectives-core/docsources/query-inversion.md` (Part 1 and runtime sections) |
| `QUERY-PIPELINE` | Expression parsing, compilation, interpreter, dependencies | `packages/perspectives-core/docsources/query-subsystem.md` (sections 2 to 8) |
| `TRANSACTION-PHASES` | Transaction entry paths, phase loop, scheduling behavior | `packages/perspectives-core/docsources/transaction-execution.md` (entry paths and phases) |
| `DELTA-ORDERING` | Deterministic ordering, version tracking, pending transactions | `packages/perspectives-core/docsources/delta-ordering.md` (sections 2 to 5) |
| `STRUCTURED-LOGGING` | Topic/level logging configuration and runtime controls | `packages/perspectives-core/docsources/structured-logging.md` (sections 2 and 3) |
| `MODEL-URI-RESOLUTION` | Mapping `ModelUri` to model/manifest/store endpoints | `packages/perspectives-core/docsources/model-uri-resolution.md` (sections 2 to 4) |
| `PEER-SERIALISATION` | Serialising contexts as deltas for peer reconstruction | `packages/perspectives-core/docsources/context-serialisation-for-peer.md` (entry point and top-level flow) |

## Notes

- Keep the list small and stable.
- Add a new keyword only when it maps to a recurring technical area with
  existing implementation guidance.
