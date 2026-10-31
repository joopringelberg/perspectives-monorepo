// Unit tests for the pure parts of the PDR Language Server (Perspectives.Lsp.Diagnostics, Perspectives.Lsp.Resolve).
// Run with `pnpm run test:lsp` (requires a prior `spago build`).
import test from "node:test";
import assert from "node:assert/strict";

// The module graph reaches browser-only dependencies (pouchdb-browser) that expect `self`.
globalThis.self ??= globalThis;
const D = await import("../output/Perspectives.Lsp.Diagnostics/index.js");
const R = await import("../output/Perspectives.Lsp.Resolve/index.js");
const M = await import("../output/Perspectives.Parsing.Messages/index.js");
const Maybe = await import("../output/Data.Maybe/index.js");
const Obj = await import("../output/Foreign.Object/index.js");
const T = await import("../output/Perspectives.Representation.TypeIdentifiers/index.js");
const Names = await import("../output/Perspectives.Names/index.js");

const pos = (line, column) => ({ line, column });
const fromMaybe = (m) => (m instanceof Maybe.Just ? m.value0 : null);

test("diagnostic code is the data constructor name; range spans start..end (0-based)", () => {
  const d = D.error2Diagnostic(M.UnknownVariable.create(pos(3, 5))("x"));
  assert.equal(d.code, "UnknownVariable");
  assert.equal(d.severity, 1);
  assert.deepEqual(d.range, { start: { line: 2, character: 4 }, end: { line: 2, character: 4 } });

  const d2 = D.error2Diagnostic(M.NotFunctional.create(pos(2, 3))(pos(2, 9))(null));
  assert.equal(d2.code, "NotFunctional");
  assert.deepEqual(d2.range, { start: { line: 1, character: 2 }, end: { line: 1, character: 8 } });
  assert.deepEqual(d2.data.positions, [pos(2, 3), pos(2, 9)]);
});

test("positions inside Maybe are collected; errors without positions go to 0:0", () => {
  const d = D.error2Diagnostic(M.Custom.create("boom"));
  assert.equal(d.code, "Custom");
  assert.deepEqual(d.range, { start: { line: 0, character: 0 }, end: { line: 0, character: 0 } });
  const u = D.error2Diagnostic(
    M.UnauthorizedForRole.create("Author")(T.ENR.create("model://x#M$U"))(T.ENR.create("model://x#M$T"))([])(Maybe.Just.create(pos(4, 2)))(Maybe.Just.create(pos(5, 7))),
  );
  assert.equal(u.code, "UnauthorizedForRole");
  assert.deepEqual(u.range, { start: { line: 3, character: 1 }, end: { line: 4, character: 6 } });
});

test("nameAt takes the name under the cursor and trims trailing separators", () => {
  assert.equal(fromMaybe(R.nameAt("    filledBy sys:PerspectivesSystem$User.")(20)), "sys:PerspectivesSystem$User");
  assert.equal(fromMaybe(R.nameAt("  thing Foo")(11)), "Foo");
  assert.equal(fromMaybe(R.nameAt("  a   b")(4)), null);
});

const defs = [
  { name: "model://x#M", kind: "context", line: 1, column: 1 },
  { name: "model://x#M$A", kind: "context", line: 3, column: 3 },
  { name: "model://x#M$A$Thing", kind: "enumeratedRole", line: 4, column: 5 },
  { name: "model://x#M$B", kind: "context", line: 7, column: 3 },
  { name: "model://x#M$B$Thing", kind: "enumeratedRole", line: 8, column: 5 },
];
const lines = [
  "domain model://x#M@1.0",
  "",
  "  case A",
  "    thing Thing",
  "      property P",
  "",
  "  case B",
  "    thing Thing",
  "      filledBy Thing",
];

test("enclosingScope finds the lexically enclosing definition", () => {
  assert.equal(fromMaybe(R.enclosingScope(defs)(lines)(5)), "model://x#M$A$Thing");
  assert.equal(fromMaybe(R.enclosingScope(defs)(lines)(9)), "model://x#M$B$Thing");
});

test("resolve prefers the candidate that shares most of the enclosing scope", () => {
  const r = R.resolve(defs)(Obj.empty)(Maybe.Just.create("model://x#M$B$Thing"))("Thing");
  assert.ok(r instanceof R.Local);
  assert.deepEqual(r.value0.map((d) => d.name), ["model://x#M$B$Thing"]);
});

test("resolve: qualified names match exactly or are external", () => {
  const local = R.resolve(defs)(Obj.empty)(Maybe.Nothing.value)("model://x#M$A");
  assert.ok(local instanceof R.Local);
  const ext = R.resolve(defs)(Names.defaultReadableNamespaces)(Maybe.Nothing.value)("sys:PerspectivesSystem");
  assert.ok(ext instanceof R.External);
  assert.equal(ext.value0, "model://perspectives.domains#System$PerspectivesSystem");
  assert.ok(R.resolve(defs)(Obj.empty)(Maybe.Nothing.value)("Nope") instanceof R.Unresolved);
});

test("selectionRange covers the local name on the declaration line", () => {
  assert.deepEqual(R.selectionRange(lines)(defs[2]), { start: { line: 3, character: 10 }, end: { line: 3, character: 15 } });
});
