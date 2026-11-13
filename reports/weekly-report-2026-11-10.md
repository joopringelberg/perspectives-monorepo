# Weekly Progress Report – 2026-11-10

### 🎯 Top Achievements

- **Safer model publishing and translation workflows (direct commits):** Model uploads now preserve existing translations and other user attachments, reuse translation tables from a base version when appropriate, and reliably regenerate YAML after parsing. Translation generation handles malformed or unreadable files with actionable modeller warnings.

- **More complete model and context lifecycle handling (direct commits):** Removing a model now cleans up its stored model, indexed resources, queries, notifications, and caches; a new local context-removal action lets a user leave a context without removing it for peers. Regression cases cover model uninstall and local context removal.

- **More reliable synchronization (direct commits):** Private roles filled by public roles now send their binding deltas to peers who can see the filled role, and resource-key normalization is idempotent, preventing false version gaps from blocking synchronized property removals.

- **Safer and more expressive ARC screens (direct commits):** The parser now rejects `fillfrom` in details sections, screen validation correctly checks perspective identifiers, and forms can hide role cards or identifiers with `norolecard` and `noidentifier`.

- **ARC editor integrated and improved (direct commits):** MyContexts publishing now includes the editor, with word navigation, comment toggling, clickable diagnostics, a resizable feedback pane, and a command palette. The test corpus was refreshed, with 17 ARC compilation tests reported passing.

### 🐛 Key Bugs Fixed

- **Re-uploaded models lost translations and attachments:** Uploads now preserve existing translation and conversation files while replacing only generated attachments; malformed translation files are reported instead of silently leaving stale data.

- **Model removal left resources behind:** Removal now waits for deferred cleanup and removes indexed resources and model-owned runtime state, while avoiding duplicate scheduled removals.

- **Peers could miss updates or reject valid removals:** Binding deltas for private-to-public role bindings are no longer skipped, and repeated normalization no longer changes already-normalized resource keys.

### 📊 Impact

Model authors can update and translate models with less risk of losing work, while installations and peers handle model/context removal and synchronization more consistently. ARC authors also gain clearer validation and more flexible forms, alongside an editor now shipped with MyContexts.

### ⚠️ Concerns

- **Integration validation is not fully reported:** The activity cites focused delta and ARC-compilation test results, but does not report a full Layer 1 or two-peer integration run covering the new model-removal and local-context-removal behavior.

- **Editor integration coverage is unclear:** The editor is now included in MyContexts publishing, but no live-PDR end-to-end validation is recorded in this activity.

### 🎯 Focus for Next Week

- Run the model-uninstall and local-context-removal cases through the broader runtime and two-peer suites, and record the results.

- Exercise translation preservation and warning recovery across successive repository uploads, including a hand-edited invalid YAML file.

- Validate the published ARC editor with a live PDR and confirm its supported browser/worker paths.
