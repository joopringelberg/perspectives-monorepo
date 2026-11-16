// SPDX-License-Identifier: GPL-3.0-or-later
// Run after spago build: node --test test/contextSearch.test.mjs
import assert from "node:assert/strict";
import test from "node:test";

globalThis.self ??= globalThis;
globalThis.__MYCONTEXTS__ ??= "https://mycontexts.com/";
const Search = await import("../output/Perspectives.ContextSearch/index.js");
const Api = await import("../output/Perspectives.ApiTypes/index.js");
const JSON = await import("../output/Simple.JSON/index.js");
const Either = await import("../output/Data.Either/index.js");
const Maybe = await import("../output/Data.Maybe/index.js");

const domain = "model://example.org#stablemodel";
const child = `${domain}$stablechild`;
const otherChild = `${domain}$otherchild`;
const nested = `${child}$nested`;
const context = (id, parent) => ({ id, context: parent });

test("model choices come from app external role types and keep stable model identifiers", () => {
  assert.deepEqual(Search.modelIdsFromAppTypes([
    `${domain}$stableapp$External`,
    "model://other.org#othermodel$otherapp$External",
  ]), [domain, "model://other.org#othermodel"]);
  assert.deepEqual(Search.modelIdsFromAppTypes([]), []);
});

test("multiple apps and versions from one model produce only one model choice", () => {
  assert.deepEqual(Search.modelIdsFromAppTypes([
    `${domain}$stableapp$External`,
    `${domain}$anotherapp$External`,
    `${domain}$stableapp$External@2.0`,
    `${domain}@1.0$stableapp$External`,
  ]), [domain]);
});

test("context search lists direct children, not the domain, deeper contexts, or another model", () => {
  const contexts = {
    [domain]: context(domain, Maybe.Nothing.value),
    [child]: context(child, new Maybe.Just(domain)),
    [otherChild]: context(otherChild, new Maybe.Just(domain)),
    [nested]: context(nested, new Maybe.Just(child)),
    unrelated: context("model://example.org#other$case", new Maybe.Just("model://example.org#other")),
  };
  assert.deepEqual(Search.topLevelContextTypes(domain)(contexts), [child, otherChild]);
});

test("context search uses declared parentage, not identifier prefix or depth", () => {
  const contexts = {
    misleading: context(`${domain}$looksTopLevel`, new Maybe.Just(child)),
    declared: context(`${domain}$contains$separators`, new Maybe.Just(domain)),
  };
  assert.deepEqual(Search.topLevelContextTypes(domain)(contexts), [`${domain}$contains$separators`]);
  assert.deepEqual(Search.topLevelContextTypes(domain)({}), []);
});

test("untranslated model labels use the readable unqualified model name, never its CUID", () => {
  const readable = "model://example.org#MyReadableModel";
  assert.equal(Search.modelSearchLabel(domain)(readable)(domain), "MyReadableModel");
  assert.equal(Search.modelSearchLabel(domain)(readable)(readable), "MyReadableModel");
  assert.equal(Search.modelSearchLabel(domain)(`${readable}@2.0`)(domain), "MyReadableModel");
});

test("model labels preserve an available translation", () => {
  assert.equal(Search.modelSearchLabel(domain)("model://example.org#MyModel")("Mijn model"), "Mijn model");
});

for (const name of ["GetContextSearchModels", "GetContextSearchTypes", "GetContextSearchInstances"]) {
  test(`request decoder recognizes ${name}`, () => {
    const result = JSON.read(Api.decodeRequestType)(name);
    assert.ok(result instanceof Either.Right);
    assert.ok(result.value0 instanceof Api[name]);
  });
}
