import test from "node:test";
import assert from "node:assert/strict";
import { diagnosticToMarker, DocumentSync, LspClient, MarkerSeverity, modelUriOfSourceUri, type LspDiagnostic } from "../src/lspClient.ts";

function fakeTransport(reply: (m: any) => object[]) {
  const sent: any[] = [];
  return { sent, transport: async (m: object) => { sent.push(m); return reply(m); } };
}

test("request resolves with the matching response and dispatches notifications", async () => {
  const notes: string[] = [];
  const { transport } = fakeTransport(m => [
    { jsonrpc: "2.0", method: "textDocument/publishDiagnostics", params: { uri: "u", diagnostics: [] } },
    { jsonrpc: "2.0", id: m.id, result: 42 },
  ]);
  const c = new LspClient(transport, method => notes.push(method));
  assert.equal(await c.request("x", null), 42);
  assert.deepEqual(notes, ["textDocument/publishDiagnostics"]);
});

test("request rejects on a JSON-RPC error", async () => {
  const { transport } = fakeTransport(m => [{ jsonrpc: "2.0", id: m.id, error: { code: -32601, message: "nope" } }]);
  await assert.rejects(new LspClient(transport, () => {}).request("x", null), /nope/);
});

test("DocumentSync increments versions and debounces changes", async () => {
  const { sent, transport } = fakeTransport(() => []);
  let text = "a";
  const sync = new DocumentSync(new LspClient(transport, () => {}), "u", () => text, 10);
  await sync.open();
  text = "ab"; sync.changed();
  text = "abc"; sync.changed();
  await new Promise(r => setTimeout(r, 40));
  assert.deepEqual(sent.map(m => m.method), ["textDocument/didOpen", "textDocument/didChange"]);
  assert.equal(sent[1].params.textDocument.version, 2);
  assert.equal(sent[1].params.contentChanges[0].text, "abc");
});

const diag = (line: number, ch: number, endLine = line, endCh = ch): LspDiagnostic => ({
  range: { start: { line, character: ch }, end: { line: endLine, character: endCh } },
  severity: 1, code: "UnknownRole", source: "perspectives-pdr", message: "m",
});

test("diagnosticToMarker widens a zero-width range to the word, else to the line", () => {
  const lines = ["domain model://x#M", "    filledBy sys:Foo$Bar here", "      "];
  const lc = (n: number) => lines[n - 1];
  const m = diagnosticToMarker(diag(1, 13), lc, 3);
  assert.deepEqual([m.startLineNumber, m.startColumn, m.endColumn], [2, 14, 25]);
  assert.equal(m.severity, MarkerSeverity.Error);
  const blank = diagnosticToMarker(diag(2, 6), lc, 3);
  assert.ok(blank.endColumn > blank.startColumn);
  const ranged = diagnosticToMarker(diag(0, 0, 1, 4), lc, 3);
  assert.deepEqual([ranged.startLineNumber, ranged.endLineNumber, ranged.endColumn], [1, 2, 5]);
  assert.equal(diagnosticToMarker(diag(99, 0), lc, 3).startLineNumber, 3);
});

test("modelUriOfSourceUri", () => {
  assert.equal(modelUriOfSourceUri("perspectives-model:model://perspectives.domains#System"), "model://perspectives.domains#System");
  assert.equal(modelUriOfSourceUri("inmemory://model/1"), undefined);
});
