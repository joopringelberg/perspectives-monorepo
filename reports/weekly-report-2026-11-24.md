# Weekly Progress Report – 2026-11-24

### 🎯 Top Achievements

- **Faster core runtime execution:** CNF simplification avoids repeated set construction, runtime role normalisation is cached per PDR, and historical delta transaction assembly is indexed. Reported signup and manager execution measurements improved substantially without sacrificing equivalent results.

- **More reliable synchronization and history:** Signed property-history checkpoints bound retained history, peers receive only the deltas they need, deletion provenance is preserved through exit processing, and incompatible or causally incomplete transactions are quarantined for recovery.

- **Stronger authorization and public-fill semantics:** Inherited properties are authorized through concrete instances, public-only filler restrictions are parsed and enforced across binding and synchronization operations, and rejected updates preserve the correct causal and resource state.

- **More expressive ARC and UI features:** ARC now supports difference set operators, relative table column weights, multiline table fields, date and datetime sequence extrema, and syntax highlighting for the new operators. React tables and identifying cards preserve their role behaviours while applying line constraints.

### 🐛 Key Bugs Fixed

- **Historical updates could be expensive or inconsistent:** Indexed membership, bounded history, cached normalisation, and checkpoint-aware recovery reduce repeated work while preserving recipient equality, ordering, and reconstruction semantics.

- **Received deletions could echo or reserve local versions:** Exit processing now carries deletion provenance and distinguishes received removals from locally generated exit-action effects.

- **Valid inherited property updates could be rejected:** Authorization now evaluates the signed author against the concrete subject, perspective object, and property storage when declared filler types are insufficient.

- **Public references and restricted fillers could be lost:** Binding operations and peer synchronization now retain public references and report existing private bindings instead of silently accepting invalid state.

- **Table and screen constraints were not propagated consistently:** Minimum and maximum line constraints now reach table rows, cells, and text controls, including multiline identifying cards.

### 📊 Impact

The core runtime should spend considerably less time normalising roles, assembling historical transactions, and simplifying type expressions. Synchronization now has bounded property history and clearer recovery behaviour, while authorization and public-fill rules protect state across peers. ARC authors and UI users gain richer set, table, and multiline-field support.

### ⚠️ Concerns

- **Layer 1 CI remains red:** The recent runs for public-only fillers, column weights, difference sets, and inherited-property authorization each reported 76 passing tests and one failing test. The report does not identify the failing test from the available tail logs, so this needs follow-up before treating the new core changes as fully validated.

- **Temporary performance instrumentation remains:** Timing and normalisation diagnostics are opt-in, but should be removed or formalized after the performance investigation is complete.

- **No pull requests were merged in this activity window:** All listed work was committed directly to the currently pushed branches.

### 🎯 Focus for Next Week

- Identify and fix the single failing Layer 1 test in each recent CI run, then rerun the affected workflow against the current branch.

- Validate the combined synchronization changes with fresh-process two-PDR scenarios, including bounded-history reconstruction, deletion provenance, public fillers, and quarantined transactions.

- Recheck browser builds and focused React tests for column weights, line constraints, and multiline identifying cards.

- Remove or document the remaining opt-in performance instrumentation once the measurements are no longer needed.
