// Reads the state of one VersionedModelManifest from the PDR and saves edited ARC source to it.
// Saving does exactly what the ArcFile file control on the manifest screen does (perspectives-react perspectivesFile.tsx):
// one proxy.saveFile call that replaces the attachment and sets the ArcFile property. The model's own states
// (ProcessArc etc.) then compile/store/apply according to AutoUpload, Store, ApplyInSession and BasedOnVersion,
// which are only read here, never written.

import { externalProperty, isAuthor, type LaunchParams } from "./launchParams.ts";

export type PerspectivesFile = { fileName: string; propertyType: string; mimeType: string; database?: string; roleFileName: string };

// The subset of PerspectivesProxy that the session uses (allows tests with a fake).
export interface ProxyLike {
  getProperty(rolID: any, propertyName: any, roleType: any, receive: (values: string[]) => void, fireAndForget?: boolean, errorHandler?: (e: string) => void): Promise<any>;
  saveFile(perspectivesFile: any, file: File, myroletype: any): Promise<PerspectivesFile>;
  send(req: any, receive: (r: any) => void): Promise<any>;
}

export const WATCHED_PROPERTIES = ["ArcSource", "ArcFeedback", "ArcFile", "VersionedModelURI", "DomeinFileName", "Published", "AutoUpload", "Store", "ApplyInSession", "LastUpload"] as const;
export type WatchedProperty = typeof WATCHED_PROPERTIES[number];
export type ManifestState = Partial<Record<WatchedProperty, string>>;

export const ARC_MIME_TYPE = "text/arc";

export class ManifestSession {
  state: ManifestState = {};
  private unsubscribers: any[] = [];
  private proxy: ProxyLike;
  private params: LaunchParams;

  constructor(proxy: ProxyLike, params: LaunchParams) {
    this.proxy = proxy;
    this.params = params;
  }

  // Subscribes (continuously) to the watched properties; onChange is called with the property that changed.
  subscribe(onChange: (prop: WatchedProperty, state: ManifestState) => void, onError: (prop: WatchedProperty, e: string) => void): Promise<void> {
    return Promise.all(WATCHED_PROPERTIES.map(prop =>
      this.proxy.getProperty(
        this.params.manifest,
        externalProperty(this.params, prop),
        this.params.externalRoleType,
        values => { this.state = { ...this.state, [prop]: values[0] }; onChange(prop, this.state); },
        false,
        e => onError(prop, e))
        .then(u => { this.unsubscribers.push(u); })
    )).then(() => undefined);
  }

  close(): void {
    this.unsubscribers.forEach(u => this.proxy.send(u, () => {}));
    this.unsubscribers = [];
  }

  isPublished(): boolean {
    return this.state.Published === "true";
  }

  // Reason why saving is not possible, or undefined when it is.
  readOnlyReason(): string | undefined {
    if (!isAuthor(this.params)) return "You are not an Author of this manifest; the source is read-only.";
    if (this.isPublished()) return "This version has been published; its source can no longer be changed.";
    return undefined;
  }

  currentFile(): PerspectivesFile | undefined {
    if (!this.state.ArcFile) return undefined;
    try { return JSON.parse(this.state.ArcFile) as PerspectivesFile; } catch { return undefined; }
  }

  fileName(): string {
    return this.currentFile()?.fileName || `${this.state.DomeinFileName || "model"}.arc`;
  }

  save(source: string): Promise<PerspectivesFile> {
    const reason = this.readOnlyReason();
    if (reason) return Promise.reject(new Error(reason));
    const fileName = this.fileName();
    const file = new File([source], fileName, { type: ARC_MIME_TYPE });
    return this.proxy.saveFile(
      { fileName
      , propertyType: externalProperty(this.params, "ArcFile")
      , mimeType: ARC_MIME_TYPE
      , database: this.currentFile()?.database
      , roleFileName: this.params.manifest
      },
      file,
      this.params.userRoleType);
  }
}

export type FeedbackKind = "none" | "busy" | "ok" | "error";

export function classifyFeedback(feedback?: string): FeedbackKind {
  if (!feedback) return "none";
  if (/^OK/.test(feedback)) return "ok";
  if (/^Parsing and compiling/.test(feedback)) return "busy";
  return "error";
}
