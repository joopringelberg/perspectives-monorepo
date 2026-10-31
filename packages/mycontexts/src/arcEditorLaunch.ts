// Launch support for the standalone ARC editor (packages/arc-editor).
// The editor is served at `<MyContexts base>arceditor/` and connects to the same SharedWorker (and hence PDR) as MyContexts.
// Only identity is handed over in the URL; the editor reads source, feedback and settings from the PDR itself.

const VERSIONED_MODEL_MANIFEST = /#CouchdbManagement(@[0-9.]+)?\$VersionedModelManifest$/;

export function isVersionedModelManifest(contextType?: string): boolean {
  return !!contextType && VERSIONED_MODEL_MANIFEST.test(contextType);
}

export type ArcEditorLaunch = {
  baseUrl: string;            // Absolute URL of the MyContexts base path (e.g. https://host/www/).
  manifest: string;           // External role instance of the VersionedModelManifest.
  contextType: string;        // Context type of the manifest, as reported by the PDR.
  userRoleType: string;       // The user role type MyContexts uses for this context.
  worker?: { url: string; name: string }; // The SharedWorker MyContexts is connected to.
};

export function arcEditorUrl({ baseUrl, manifest, contextType, userRoleType, worker }: ArcEditorLaunch): string {
  const url = new URL("arceditor/", baseUrl);
  url.searchParams.set("manifest", manifest);
  url.searchParams.set("type", contextType);
  url.searchParams.set("role", userRoleType);
  if (worker) {
    url.searchParams.set("workerUrl", worker.url);
    url.searchParams.set("workerName", worker.name);
  }
  return url.href;
}
