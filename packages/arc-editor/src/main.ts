import * as monaco from "monaco-editor/esm/vs/editor/editor.api";
import EditorWorker from "monaco-editor/esm/vs/editor/editor.worker?worker";
import { configurePDRproxy, PDRproxy, SharedWorkerChannelPromise } from "perspectives-proxy";
import { ARC_LANGUAGE_ID, ARC_TAB_SIZE, arcLanguageConfiguration, arcMonarchLanguage } from "./arcLanguage.ts";
import { parseLaunchParams } from "./launchParams.ts";
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

const editor = monaco.editor.create($("editor"), {
  value: "",
  language: ARC_LANGUAGE_ID,
  automaticLayout: true,
  readOnly: true,
  tabSize: ARC_TAB_SIZE,
  insertSpaces: true,
  detectIndentation: false,
  renderWhitespace: "boundary",
  guides: { indentation: true },
  minimap: { enabled: false },
});
editor.getModel()!.updateOptions({ tabSize: ARC_TAB_SIZE, insertSpaces: true });

let savedSource: string | undefined;   // The source as last loaded from / saved to the PDR.
let saving = false;
let session: ManifestSession | undefined;

const isDirty = () => savedSource !== undefined && editor.getValue() !== savedSource;

function refreshControls() {
  badge("dirty", isDirty() ? "Unsaved changes" : "", "dirty");
  const reason = session?.readOnlyReason();
  saveButton.disabled = !session || !!reason || saving || !isDirty();
  editor.updateOptions({ readOnly: !session || !!reason });
}

editor.onDidChangeModelContent(refreshControls);

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
        if (editor.getValue() !== incoming) editor.setValue(incoming);
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
  const source = editor.getValue();
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
  } catch (e) {
    notice(`Runtime unavailable: ${e instanceof Error ? e.message : String(e)}`);
  }
}

start();
