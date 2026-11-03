---
description: "Use when working on the standalone ARC editor, Monaco integration, language-server client, manifest source saving, or editor SharedWorker connection."
applyTo: "packages/arc-editor/**"
---

# arc-editor - Copilot Instructions

## What This Package Is

`arc-editor` is a standalone **TypeScript / Monaco / Vite** editor for the ARC source of a `VersionedModelManifest`. MyContexts opens it in a separate browser tab. It connects to the existing PDR through the same SharedWorker; it must not start or reinitialise a runtime.

See the [package README](../../packages/arc-editor/README.md) for launch parameters, supported language-server operations, and verification limitations.

## Key Files

| File | Responsibility |
|---|---|
| `src/main.ts` | Monaco setup, language providers, document management, navigation, and PDR connection |
| `src/arcLanguage.ts` | Monaco language configuration and Monarch tokenizer |
| `src/lspClient.ts` | Monaco-independent JSON-RPC client, document synchronisation, and diagnostic conversion |
| `src/manifestSession.ts` | Manifest subscriptions, source saving, author/published restrictions, and feedback |
| `src/launchParams.ts` | Launch parameter validation, worker identity, and same-origin checks |
| `src/navigation.ts`, `src/structure.ts` | Monaco-independent navigation and ARC structure helpers |
| `src/noPageHostedCore.ts` | Throwing stub that prevents accidental page-hosted PDR startup |
| `test/*.test.ts` | Node unit tests for the language configuration and editor helpers |
| `vite.config.ts` | Build base path and the core safety-stub alias |

## Runtime and Language-Server Integration

- Preserve the launch contract: `manifest`, `type`, `role`, `workerUrl`, and `workerName`. SharedWorker reuse requires the same origin, worker script URL, and worker name as MyContexts.
- Page-hosted mode is unsupported. Preserve the `perspectives-core` alias to `src/noPageHostedCore.ts`; do not bundle a local runtime or silently fall back to starting one.
- Use the existing `perspectives-proxy` connection pattern: configure the shared channel once, then await `PDRproxy`. Follow the [proxy instructions](perspectives-proxy.instructions.md) when changing that boundary.
- The PDR is the ARC language server (`Perspectives.Lsp.Server`). Route language requests through `proxy.lsp`; do not add a competing semantic parser or compiler to the editor. Consult the [core instructions](perspectives-core.instructions.md) for server-side changes.
- Preserve full-document LSP synchronisation, debounced edits, and document-version handling so stale responses cannot overwrite newer results.
- Keep Monaco-independent helpers free of Monaco/browser dependencies where practical, following the existing Node-testable modules.
- Installed model sources use `perspectives-model:` URIs and are read-only. Preserve that distinction when changing navigation, model loading, or saving.
- Completion is deliberately not offered continuously while typing; preserve explicit Ctrl+Space and property-list delimiter behaviour unless changing that UX intentionally.

## Manifest Editing and Saving

- Maintain continuous subscriptions to source, feedback, and manifest settings rather than treating initial values as a snapshot.
- Save through the existing `ManifestSession` flow: one `proxy.saveFile` call replaces the `ArcFile`. Do not directly assign `ArcSource` or unrelated manifest settings.
- Preserve author checks and the prohibition on saving published versions. Propagate save failures rather than displaying success or swallowing errors.
- Keep file metadata and filename derivation consistent with existing tests when changing the save flow.

## ARC Language Changes

- ARC is indentation-sensitive. Preserve two-space soft tabs and disabled indentation detection.
- Consult the [ARC language instructions](arc-language.instructions.md) when changing syntax-aware behaviour or examples.
- Keep the Monarch tokenizer aligned with implemented ARC syntax. For language changes, check the [VS Code grammar](language-perspectives-arcII.instructions.md), [Highlight.js grammar](perspectives-highlightjs.instructions.md), and [language reference](perspectives-reference.instructions.md) as relevant.

## Build and Validation

Run from the monorepo root:

```bash
# Node unit tests (Node >= 22.6)
pnpm -F arc-editor test

# Builds perspectives-proxy, type-checks editor sources, and bundles with Vite
pnpm -F arc-editor build

# Build and copy the editor into the MyContexts distribution when needed
pnpm -F arc-editor build:into-mycontexts
```

- Add or update tests alongside the affected helper. The TypeScript configuration includes `src`, not the test files; a successful build does not replace running the tests.
- For Monaco UI or SharedWorker integration changes, also verify the affected flow in a browser launched from MyContexts against a real PDR. Unit tests or a fake-worker smoke test do not establish real compilation, persistence, or end-to-end worker behaviour.
- `BUILD_PATH` overrides the Vite base path; otherwise the base comes from `../mycontexts/build.json`. Preserve deployment under the MyContexts editor path.
- `dist/` is generated; edit sources and configuration, not bundled output. `build:into-mycontexts` also writes to `../mycontexts/dist/arceditor`.
