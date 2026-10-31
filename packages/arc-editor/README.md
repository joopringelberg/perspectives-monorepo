# ARC editor (proof of concept)

A standalone Monaco editor for the ARC source of a `VersionedModelManifest`. It runs in its own browser tab and talks to the **same SharedWorker (and therefore the same PDR)** that MyContexts already runs. It never starts or reinitialises a PDR itself.

## How it works

1. **Launch.** In MyContexts, open a `VersionedModelManifest` and choose *Edit ARC source* from the context-actions menu. MyContexts opens `<base>/arceditor/` in a new tab, with these URL parameters:
   - `manifest`: the manifest's External role instance.
   - `type`: the context type.
   - `role`: the user's role type.
   - `workerUrl` and `workerName`: the URL and name of MyContexts' own SharedWorker, taken from `getSharedWorkerConnection()` in perspectives-proxy.
2. **Connect.** The editor calls `configurePDRproxy("sharedWorkerChannel", { sharedWorkerUrl, sharedWorkerName })`. A SharedWorker is shared only when origin, script URL and name are all identical, so the editor must be served from the same origin as MyContexts. If `pdrStarted()` returns false, the editor tells the user to open MyContexts and log in.
3. **Load.** The editor subscribes to `ArcSource`, `ArcFeedback`, `ArcFile`, `VersionedModelURI`, `DomeinFileName`, `Published`, `AutoUpload`, `Store`, `ApplyInSession` and `LastUpload`. These subscriptions keep the editor up to date continuously.
4. **Save** (button or Ctrl/Cmd+S). One `proxy.saveFile` call replaces the `ArcFile` attachment. This is exactly what uploading a file on the manifest screen does. The model's own states then compile, store and apply the source according to the manifest settings. The editor reads those settings but never writes them, and it never assigns `ArcSource` directly.

The editor refuses to save in two cases: the user is not an Author of the manifest, or the version is `Published`. Unsaved changes produce a warning when the tab is closed. Errors are shown as visible notices: missing source, runtime unavailable, and save failures.

Indentation uses 2 spaces with soft tabs, and indentation detection is turned off. Syntax colouring is a Monarch port of `packages/language-perspectives-arcII/syntaxes/arc.tmLanguage.yaml`.

### Deviation from the issue

The launch entry is part of MyContexts (`src/www.tsx`, `src/arcEditorLaunch.ts`), not an action in the model. A model action cannot open a browser tab, and adding one would mean republishing CouchdbManagement.

## Build, test, demo

```bash
pnpm install
pnpm -F arc-editor test                  # unit tests (node:test, Node >= 22.6)
pnpm -F arc-editor build                 # dist/, base = <mycontexts buildPath>arceditor/
pnpm -F arc-editor build:into-mycontexts # copy into ../mycontexts/dist/arceditor
```

`BUILD_PATH` overrides the base path; for example, use `BUILD_PATH=/` for the MyContexts dev server and copy `dist` to `mycontexts/public/arceditor`. Then:

1. Serve MyContexts with the editor under `arceditor/`.
2. Log in and open a manifest you are an Author of.
3. Choose *Edit ARC source*.
4. Edit the source and save.
5. Watch `ArcFeedback` change in both the editor and MyContexts.

## Limitations

- **SharedWorker required.** In page-hosted mode (Safari/iOS/Android) MyContexts has no SharedWorker. In that case the launch parameters are missing or unusable, and the editor shows an error.
- **Same origin required.** The editor must be served from the same origin as MyContexts.
- **Verification.** Verification in the cloud environment used only unit tests and a browser smoke test against a fake SharedWorker that speaks the proxy protocol. That test covered load, edit, save and feedback, and confirmed that a second page on the same worker sees the update. No real PDR, CouchDB or model compilation was exercised.
