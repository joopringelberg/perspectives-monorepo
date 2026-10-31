// Parses the URL with which MyContexts opens the editor (see packages/mycontexts/src/arcEditorLaunch.ts).
// Only identity crosses the tab boundary; everything else is read from the shared PDR.

export type LaunchParams = {
  manifest: string;        // External role instance of the VersionedModelManifest.
  contextType: string;     // Context type of the manifest.
  userRoleType: string;    // User role type to author changes with.
  externalRoleType: string;
  worker: { url: string; name: string };
};

export type LaunchResult = { ok: true; params: LaunchParams } | { ok: false; error: string };

const EXTERNAL_PROPERTY_CUIDS = {
  ArcSource: "wkwyf9rg2q",
  ArcFeedback: "sp2animp5c",
  ArcFile: "yy61b8s5rf",
  VersionedModelURI: "hp8fq71nqg",
  Published: "gt6c8qghah",
  AutoUpload: "gzb2a572ti",
  Store: "sisjgd3asy",
  ApplyInSession: "l07ho4edfv",
  LastUpload: "xswkl4m5un",
} as const;

export function parseLaunchParams(search: string, pageOrigin: string): LaunchResult {
  const q = new URLSearchParams(search);
  const manifest = q.get("manifest");
  const contextType = q.get("type");
  const userRoleType = q.get("role");
  const workerUrl = q.get("workerUrl");
  const workerName = q.get("workerName");
  if (!manifest || !contextType || !userRoleType) {
    return { ok: false, error: "No manifest identified. Open this editor from a VersionedModelManifest in MyContexts (context actions > Edit ARC source)." };
  }
  if (!workerUrl || !workerName) {
    return { ok: false, error: "MyContexts did not hand over a SharedWorker connection. The editor requires MyContexts to run its PDR in a SharedWorker (not available in Safari/iOS/Android page-hosted mode)." };
  }
  let url: URL;
  try {
    url = new URL(workerUrl);
  } catch {
    return { ok: false, error: "Invalid worker URL." };
  }
  // A SharedWorker can only be shared within one origin; refuse anything else.
  if (url.origin !== pageOrigin) {
    return { ok: false, error: `Worker URL ${url.href} is not on this origin (${pageOrigin}).` };
  }
  return {
    ok: true,
    params: { manifest, contextType, userRoleType, externalRoleType: contextType + "$External", worker: { url: url.href, name: workerName } },
  };
}

// Fully qualified property type of a property on the manifest's external role.
export function externalProperty(params: LaunchParams, localName: keyof typeof EXTERNAL_PROPERTY_CUIDS | "DomeinFileName"): string {
  if (localName === "DomeinFileName") {
    return "model://perspectives.domains#tiodn6tcyc$mo66to1n9c$External$kpz5vhpz0n";
  }
  return `${params.externalRoleType}$${EXTERNAL_PROPERTY_CUIDS[localName]}`;
}

export function isAuthor(params: LaunchParams): boolean {
  return params.userRoleType === `${params.contextType}$wwijictoy2`;
}
