# Weekly Progress Report – 2026-11-17

### 🎯 Top Achievements

- **More precise ARC querying and indexing (#556, #558):** `allfilledroles` selects roles with an inherited filler-type lower bound and permission-checked bulk `bind_`, using locally known inverse bindings. Inversion separates result anchors from filter dependencies so criteria do not become property-expansion starting points.

- **Context recovery in MyContexts (#563):** A translated finder searches models, context types, and local instances independently of App-role links. Users can open a result or copy it to relink.

- **More reliable peer synchronization (direct commits):** Replacement initializes recursively filled user roles and clears stale local-user state; transport omits local bootstrap scaffolding, deduplicates verification bundles by identity, and uses the sender as envelope author without changing signed-delta authors.

- **Safer context and action lifecycle handling (#565; direct commits `b03d08c`, `cdb9c05`):** Removing through calculated context roles targets the actual context, not a dangling external role. DomeinFile readers now reject legacy action shapes.

- **Clearer authoring and test maintenance (direct commits; #556):** The ARC editor shows readable property origins, including stable filler contributions; context menus suppress duplicate fill commands by identity. Test models and runners were reorganized, with coverage gaps documented.

### 🐛 Key Bugs Fixed

- **Contexts could survive attempted removal:** Calculated roles can yield external roles, so removing only that role left a restorable dangling context; compiler, API, and UI paths now remove the owning context.

- **Peer initialization could omit accessible data:** Stale `isMe` flags and non-recursive traversal suppressed synchronization; replacement now walks downstream roles while respecting their perspectives.

- **Property provenance exposed stable identifiers:** LSP origins now use the declaring role’s readable name and retain the full origin in the tooltip.

### 📊 Impact

Users can recover contexts that lost their App link, while peer role changes and context removal behave more consistently. ARC authors gain safer querying and clearer diagnostics; maintainers have a more explicit test map.

### ⚠️ Concerns

- **Two synchronization changes are not delivered:** Pending-transaction retries (#566) and strict legacy-delta rejection (#567) remain open draft PRs; neither should be treated as shipped. The latter is a format cutover, so rollout readiness matters.

- **Compatibility cutover:** Merged #565 intentionally rejects legacy persisted action shapes after the two-reboot migration; installations that have not completed that migration may be affected.

- **Test coverage gaps remain visible:** Final Layer 1 checks for #556 and #558 succeeded, but the inventory records Layer 2 suites left disabled and a standalone LSP run with 12 passing tests and one failing fixture.

### 🎯 Focus for Next Week

- Complete and validate pending-transaction recovery, and decide when the legacy-delta parser cutover is safe to merge.

- Fix the LSP fixture and verify Layer 2 prerequisites before treating its commented-out suites as coverage.

- Run broader integration checks for the new query-inversion and role-replacement synchronization paths.
