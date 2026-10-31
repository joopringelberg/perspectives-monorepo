// Client side of the ARC Language Server that runs inside the PDR (see README, "Language Server").
// This module has no Monaco dependency so it can be unit tested in Node.

export type LspPosition = { line: number; character: number };
export type LspRange = { start: LspPosition; end: LspPosition };
export type LspLocation = { uri: string; range: LspRange };
export type LspDiagnostic = {
  range: LspRange;
  severity: number;          // 1 = Error, 2 = Warning
  code: string;              // The name of the PerspectivesError data constructor.
  source: string;
  message: string;
  data?: { positions: { line: number; column: number }[] };
};

type JsonRpcMessage = { jsonrpc: "2.0"; id?: number | string; method?: string; params?: any; result?: any; error?: { code: number; message: string } };

/** Sends one JSON-RPC message and resolves with the messages the server emitted in reaction (proxy.lsp). */
export type LspTransport = (message: object) => Promise<object[]>;

/** The scheme the server uses for read-only sources of installed models. */
export const MODEL_SOURCE_SCHEME = "perspectives-model:";

export class LspClient {
  private nextId = 1;
  private transport: LspTransport;
  private onNotification: (method: string, params: any) => void;

  constructor(transport: LspTransport, onNotification: (method: string, params: any) => void) {
    this.transport = transport;
    this.onNotification = onNotification;
  }

  private dispatch(messages: object[], id?: number): any {
    let result: any = undefined;
    for (const m of messages as JsonRpcMessage[]) {
      if (m.method && m.id === undefined) this.onNotification(m.method, m.params);
      else if (id !== undefined && m.id === id) {
        if (m.error) throw new Error(`${m.error.message} (${m.error.code})`);
        result = m.result;
      }
    }
    return result;
  }

  async request<T = any>(method: string, params: object | null): Promise<T> {
    const id = this.nextId++;
    const messages = await this.transport({ jsonrpc: "2.0", id, method, params });
    return this.dispatch(messages, id) as T;
  }

  async notify(method: string, params: object | null): Promise<void> {
    this.dispatch(await this.transport({ jsonrpc: "2.0", method, params }));
  }

  initialize(): Promise<{ capabilities: { textDocumentSync: number; definitionProvider: boolean } }> {
    return this.request("initialize", { processId: null, rootUri: null, capabilities: {} })
      .then(async r => { await this.notify("initialized", {}); return r; });
  }
}

/**
 * Full-document synchronisation of one document. Changes are debounced: the PDR compiles the whole file
 * on every change, so we only send the latest text after the user pauses typing.
 */
export class DocumentSync {
  private version = 0;
  private timer: ReturnType<typeof setTimeout> | undefined;
  private opened = false;
  private client: LspClient;
  readonly uri: string;
  private getText: () => string;
  private delayMs: number;

  constructor(client: LspClient, uri: string, getText: () => string, delayMs = 600) {
    this.client = client;
    this.uri = uri;
    this.getText = getText;
    this.delayMs = delayMs;
  }

  open(): Promise<void> {
    this.opened = true;
    return this.client.notify("textDocument/didOpen", {
      textDocument: { uri: this.uri, languageId: "arc", version: ++this.version, text: this.getText() },
    });
  }

  changed(): void {
    if (!this.opened) return;
    if (this.timer) clearTimeout(this.timer);
    this.timer = setTimeout(() => { this.timer = undefined; void this.flush(); }, this.delayMs);
  }

  flush(): Promise<void> {
    return this.client.notify("textDocument/didChange", {
      textDocument: { uri: this.uri, version: ++this.version },
      contentChanges: [{ text: this.getText() }],
    });
  }

  close(): Promise<void> {
    if (this.timer) clearTimeout(this.timer);
    this.opened = false;
    return this.client.notify("textDocument/didClose", { textDocument: { uri: this.uri } });
  }
}

/** Monaco's MarkerSeverity values (kept here to avoid a Monaco dependency). */
export const MarkerSeverity = { Error: 8, Warning: 4, Info: 2 } as const;

export type MarkerData = {
  startLineNumber: number; startColumn: number; endLineNumber: number; endColumn: number;
  severity: number; message: string; code: string; source: string;
};

/**
 * Convert an LSP diagnostic (0-based) to a Monaco marker (1-based).
 * The server gives a zero-width range when an error carries a single position; we widen it
 * to the word at that position, or else to the rest of the line, so the marker is visible.
 */
export function diagnosticToMarker(
  d: LspDiagnostic,
  lineContent: (lineNumber: number) => string,
  lineCount: number,
): MarkerData {
  const startLineNumber = Math.min(Math.max(d.range.start.line + 1, 1), Math.max(lineCount, 1));
  let startColumn = d.range.start.character + 1;
  let endLineNumber = Math.min(Math.max(d.range.end.line + 1, 1), Math.max(lineCount, 1));
  let endColumn = d.range.end.character + 1;
  if (startLineNumber === endLineNumber && startColumn === endColumn) {
    const text = lineContent(startLineNumber);
    const word = /^[\w$:#/.@-]+/.exec(text.slice(startColumn - 1));
    if (word) endColumn = startColumn + word[0].length;
    else {
      const indent = text.length - text.trimStart().length;
      startColumn = Math.min(startColumn, indent + 1);
      endColumn = Math.max(text.length + 1, startColumn + 1);
    }
    endLineNumber = startLineNumber;
  }
  return {
    startLineNumber, startColumn, endLineNumber, endColumn,
    severity: d.severity === 1 ? MarkerSeverity.Error : d.severity === 2 ? MarkerSeverity.Warning : MarkerSeverity.Info,
    message: d.message,
    code: d.code,
    source: d.source,
  };
}

/** The readable model URI in a `perspectives-model:` location, or undefined for other uris. */
export function modelUriOfSourceUri(uri: string): string | undefined {
  return uri.startsWith(MODEL_SOURCE_SCHEME) ? uri.slice(MODEL_SOURCE_SCHEME.length) : undefined;
}
