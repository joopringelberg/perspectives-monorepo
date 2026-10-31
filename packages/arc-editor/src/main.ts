import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
// editor.api contains no editor contributions; hover (incl. marker messages) must be loaded explicitly.
import "monaco-editor/esm/vs/editor/contrib/hover/browser/hoverContribution";
import "monaco-editor/esm/vs/editor/contrib/gotoSymbol/browser/goToCommands";
import "monaco-editor/esm/vs/editor/contrib/gotoSymbol/browser/link/goToDefinitionAtPosition";
import "monaco-editor/esm/vs/editor/contrib/contextmenu/browser/contextmenu";
import EditorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import { configurePDRproxy, PDRproxy, SharedWorkerChannelPromise } from "perspectives-proxy";
import { ARC_LANGUAGE_ID, ARC_TAB_SIZE, arcLanguageConfiguration, arcMonarchLanguage } from "./arcLanguage.ts";
import { parseLaunchParams } from "./launchParams.ts";
import { diagnosticToMarker, DocumentSync, LspClient, modelUriOfSourceUri, type LspDiagnostic, type LspLocation } from "./lspClient.ts";
import { classifyFeedback, ManifestSession, type ProxyLike, type WatchedProperty } from "./manifestSession.ts";

(self as any).MonacoEnvironment = { getWorker: () => new EditorWorker() };

const $ = (id: string) => document.getElementById(id)!;
const saveButton = $("save") as HTMLButtonElement;

function notice(text: string | undefined, kind: "error" | "info" = "error") {
  const n = $("notice");
  n.hidden = !text;
  n.className = kind;
  n.textContent = text || "";
}

function badge(id: string, text: string, cls = "") {
  const el = $(id);
  el.textContent = text;
  el.className = "badge " + cls;
}

monaco.languages.register({ id: ARC_LANGUAGE_ID, extensions: [".arc"] });
monaco.languages.setLanguageConfiguration(ARC_LANGUAGE_ID, arcLanguageConfiguration as any);
monaco.languages.setMonarchTokensProvider(ARC_LANGUAGE_ID, arcMonarchLanguage as any);

// Created explicitly: a model the editor creates itself (via `value`) is disposed on setModel.
const mainModel = monaco.editor.createModel("", ARC_LANGUAGE_ID);
const editor = monaco.editor.create($("editor"), {
  model: mainModel,
  automaticLayout: true,
  readOnly: true,
  tabSize: ARC_TAB_SIZE,
  insertSpaces: true,
  detectIndentation: false,
  renderWhitespace: "boundary",
  guides: { indentation: true },
  minimap: { enabled: false },
});
mainModel.updateOptions({ tabSize: ARC_TAB_SIZE, insertSpaces: true });

let savedSource: string | undefined;   // The source as last loaded from / saved to the PDR.
let saving = false;
let session: ManifestSession | undefined;

const isDirty = () => savedSource !== undefined && mainModel.getValue() !== savedSource;

function refreshControls() {
  badge("dirty", isDirty() ? "Unsaved changes" : "", "dirty");
  const reason = session?.readOnlyReason();
  saveButton.disabled = !session || !!reason || saving || !isDirty();
  const showingMain = editor.getModel() === mainModel;
  editor.updateOptions({ readOnly: !session || !!reason || !showingMain });
  $("back").hidden = showingMain;
}

editor.onDidChangeModelContent(refreshControls);
editor.onDidChangeModel(refreshControls);

// ---------- Language Server (runs inside the PDR, see README) ----------
let lsp: LspClient | undefined;
let mainSync: DocumentSync | undefined;
const lspToMonaco = new Map<string, monaco.editor.ITextModel>();   // LSP uri -> Monaco model
const monacoToLsp = new Map<string, string>();                      // Monaco uri -> LSP uri
const backStack: { model: monaco.editor.ITextModel; position: monaco.IPosition | null }[] = [];

function registerModel(lspUri: string, model: monaco.editor.ITextModel) {
  lspToMonaco.set(lspUri, model);
  monacoToLsp.set(model.uri.toString(), lspUri);
}

function onLspNotification(method: string, params: any) {
  if (method !== "textDocument/publishDiagnostics") return;
  const model = lspToMonaco.get(params.uri);
  if (!model || model.isDisposed()) return;
  monaco.editor.setModelMarkers(model, "pdr",
    (params.diagnostics as LspDiagnostic[]).map(d => diagnosticToMarker(d, n => model.getLineContent(n), model.getLineCount())));
}

/** The Monaco model for an LSP uri; read-only sources of installed models are fetched from the PDR on first use. */
async function modelForLspUri(uri: string): Promise<monaco.editor.ITextModel | undefined> {
  const known = lspToMonaco.get(uri);
  if (known && !known.isDisposed()) return known;
  const modelUri = modelUriOfSourceUri(uri);
  if (!modelUri || !lsp) return undefined;
  const source = await lsp.request<{ text: string } | null>("perspectives/modelSource", { uri });
  if (!source) { notice(`No source available for ${modelUri}.`, "info"); return undefined; }
  const model = monaco.editor.createModel(source.text, ARC_LANGUAGE_ID,
    monaco.Uri.from({ scheme: "perspectives-model", path: "/" + encodeURIComponent(modelUri) }));
  model.updateOptions({ tabSize: ARC_TAB_SIZE });
  registerModel(uri, model);
  // Open it on the server too, so go-to-definition works inside the dependency.
  await lsp.notify("textDocument/didOpen", { textDocument: { uri, languageId: "arc", version: 1, text: source.text } });
  return model;
}

function showModel(model: monaco.editor.ITextModel, selection?: monaco.IRange) {
  if (editor.getModel() !== model) {
    backStack.push({ model: editor.getModel()!, position: editor.getPosition() });
    editor.setModel(model);
  }
  if (selection) { editor.setSelection(selection); editor.revealRangeInCenter(selection); }
  refreshControls();
}

$("back").addEventListener("click", () => {
  const prev = backStack.pop() ?? { model: mainModel, position: null };
  editor.setModel(prev.model);
  if (prev.position) { editor.setPosition(prev.position); editor.revealPositionInCenter(prev.position); }
  refreshControls();
});

monaco.languages.registerDefinitionProvider(ARC_LANGUAGE_ID, {
  async provideDefinition(model, position) {
    const uri = monacoToLsp.get(model.uri.toString());
    if (!lsp || !uri) return null;
    if (model === mainModel) await mainSync?.flush();
    const result = await lsp.request<LspLocation | LspLocation[] | null>("textDocument/definition", {
      textDocument: { uri }, position: { line: position.lineNumber - 1, character: position.column - 1 },
    });
    const locations = !result ? [] : Array.isArray(result) ? result : [result];
    const out: monaco.languages.Location[] = [];
    for (const l of locations) {
      const target = await modelForLspUri(l.uri);
      if (!target) continue;
      out.push({ uri: target.uri, range: {
        startLineNumber: l.range.start.line + 1, startColumn: l.range.start.character + 1,
        endLineNumber: l.range.end.line + 1, endColumn: l.range.end.character + 1 } });
    }
    return out;
  },
});

// The standalone editor cannot open other models by itself; switch models in place (with a back button).
monaco.editor.registerEditorOpener({
  openCodeEditor(_source, resource, selectionOrPosition) {
    const model = monaco.editor.getModel(resource);
    if (!model) return false;
    const sel: monaco.IRange | undefined = !selectionOrPosition ? undefined
      : "startLineNumber" in selectionOrPosition ? selectionOrPosition
      : { startLineNumber: selectionOrPosition.lineNumber, startColumn: selectionOrPosition.column,
          endLineNumber: selectionOrPosition.lineNumber, endColumn: selectionOrPosition.column };
    showModel(model, sel);
    return true;
  },
});

mainModel.onDidChangeContent(() => mainSync?.changed());

async function startLsp(proxy: { lsp(message: object): Promise<object[]> }) {
  try {
    lsp = new LspClient(m => proxy.lsp(m), onLspNotification);
    await lsp.initialize();
    const uri = mainModel.uri.toString();
    registerModel(uri, mainModel);
    mainSync = new DocumentSync(lsp, uri, () => mainModel.getValue());
    await mainSync.open();
  } catch (e) {
    lsp = undefined;
    notice(`The language server is unavailable (no live diagnostics or go-to-definition): ${e instanceof Error ? e.message : String(e)}`, "info");
  }
}

window.addEventListener("beforeunload", e => {
  if (isDirty()) { e.preventDefault(); e.returnValue = ""; }
});
window.addEventListener("pagehide", () => session?.close());

function onPropertyChange(prop: WatchedProperty) {
  const s = session!.state;
  switch (prop) {
    case "ArcSource": {
      const incoming = s.ArcSource ?? "";
      if (!isDirty()) {
        savedSource = incoming;
        if (mainModel.getValue() !== incoming) mainModel.setValue(incoming);
      } else if (incoming !== savedSource) {
        notice("The source on the manifest changed elsewhere while you have unsaved edits. Saving will overwrite it.", "info");
        savedSource = incoming;
      }
      if (s.ArcSource === undefined) notice("This manifest has no ARC source yet. Write one and save it to create the ArcFile.", "info");
      break;
    }
    case "ArcFeedback": {
      const kind = classifyFeedback(s.ArcFeedback);
      $("feedback").textContent = s.ArcFeedback || "No feedback yet.";
      badge("feedbackKind", { none: "", busy: "processing…", ok: "OK", error: "errors" }[kind], kind);
      break;
    }
    case "VersionedModelURI":
    case "DomeinFileName":
      $("model").textContent = s.VersionedModelURI || s.DomeinFileName || "…";
      document.title = `ARC editor – ${s.VersionedModelURI || s.DomeinFileName || ""}`;
      break;
    case "ArcFile":
      $("filename").textContent = session!.currentFile()?.fileName || "";
      break;
    default:
      $("settings").textContent =
        `AutoUpload: ${s.AutoUpload ?? "–"} · Store: ${s.Store ?? "–"} · ApplyInSession: ${s.ApplyInSession ?? "–"}` +
        (s.Published === "true" ? " · Published" : "") + (s.LastUpload ? ` · Last upload: ${s.LastUpload}` : "");
      if (s.AutoUpload !== "true" && prop === "AutoUpload") {
        notice("AutoUpload is off for this manifest: saving replaces the ArcFile but it will not be compiled (same as uploading a file on the manifest screen).", "info");
      }
  }
  refreshControls();
}

async function save() {
  if (!session || saveButton.disabled) return;
  const source = mainModel.getValue();
  saving = true;
  badge("saveStatus", "Saving…", "busy");
  refreshControls();
  try {
    await session.save(source);
    savedSource = source;
    badge("saveStatus", `Saved ${new Date().toLocaleTimeString()}`, "ok");
  } catch (e) {
    badge("saveStatus", "Save failed", "error");
    notice(`Saving failed: ${e instanceof Error ? e.message : String(e)}`);
  } finally {
    saving = false;
    refreshControls();
  }
}

saveButton.addEventListener("click", save);
editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS, save);

function withTimeout<T>(p: Promise<T>, ms: number, message: string): Promise<T> {
  return Promise.race([p, new Promise<T>((_, reject) => setTimeout(() => reject(new Error(message)), ms))]);
}

async function start() {
  const launch = parseLaunchParams(window.location.search, window.location.origin);
  if (!launch.ok) { notice(launch.error); return; }
  if (typeof SharedWorker === "undefined") {
    notice("This browser does not support SharedWorker, so the editor cannot share the PDR with MyContexts.");
    return;
  }
  const params = launch.params;
  // Connect to the SharedWorker that MyContexts uses: same origin, same script URL, same name.
  configurePDRproxy("sharedWorkerChannel", { sharedWorkerUrl: params.worker.url, sharedWorkerName: params.worker.name });
  try {
    const handler = await withTimeout(SharedWorkerChannelPromise, 10000, "No connection with the SharedWorker.");
    // Never start or reinitialise the PDR from here: it must already run for MyContexts.
    const started = await withTimeout(handler.pdrStarted(), 10000, "The SharedWorker did not answer.");
    if (!started) {
      notice("The Perspectives runtime is not running. Open MyContexts in this browser, log in, and launch the editor again from the manifest.");
      return;
    }
    const proxy = await PDRproxy;
    session = new ManifestSession(proxy as unknown as ProxyLike, params);
    await session.subscribe(
      onPropertyChange,
      (prop, e) => notice(`Could not read ${prop}: ${e}`));
    const reason = session.readOnlyReason();
    if (reason) notice(reason, "info");
    refreshControls();
    await startLsp(proxy as any);
  } catch (e) {
    notice(`Runtime unavailable: ${e instanceof Error ? e.message : String(e)}`);
  }
}

start();
