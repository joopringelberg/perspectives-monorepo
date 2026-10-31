import assert from "node:assert/strict";
import { test } from "node:test";
import { ARC_TAB_SIZE, arcLanguageConfiguration, arcMonarchLanguage } from "../src/arcLanguage.ts";

// Minimal re-implementation of Monarch's first-match rule selection for the root state (enough to check our rules).
function firstToken(line: string): string {
  for (const [re, action] of arcMonarchLanguage.tokenizer.root as any[]) {
    const m = new RegExp("^(?:" + re.source + ")").exec(line);
    if (!m) continue;
    if (typeof action === "string") return action;
    if (Array.isArray(action)) return action[0];
    const cases = action.cases;
    for (const key of Object.keys(cases)) {
      if (key === "@default") continue;
      if ((arcMonarchLanguage as any)[key.slice(1)].includes(m[0])) return cases[key];
    }
    return cases["@default"];
  }
  return "none";
}

test("ARC tokens are classified", () => {
  assert.equal(firstToken("domain model://perspectives.domains#X"), "keyword");
  assert.equal(firstToken("-- a comment"), "comment");
  assert.equal(firstToken("{- block"), "comment");
  assert.equal(firstToken("\"a string\""), "string");
  assert.equal(firstToken("perspective on Author"), "keyword.perspective");
  assert.equal(firstToken("callEffect p:Foo"), "keyword.statement");
  assert.equal(firstToken("sys:MySystem"), "type.identifier");
  assert.equal(firstToken("SetPropertyValue"), "constant.verb");
  assert.equal(firstToken("Boolean"), "type");
  assert.equal(firstToken("ArcFeedback"), "identifier");
  assert.equal(firstToken(">>= first"), "operator");
  assert.equal(firstToken("markdown <### Title"), "keyword.perspective");
});

test("two-space soft-tab indentation and block indentation", () => {
  assert.equal(ARC_TAB_SIZE, 2);
  const inc = arcLanguageConfiguration.indentationRules.increaseIndentPattern;
  assert.ok(inc.test("  case VersionedModelManifest"));
  assert.ok(inc.test("    on entry"));
  assert.ok(inc.test("      do for Author"));
  assert.ok(!inc.test("      property ArcFile (File)"));
  assert.equal(arcLanguageConfiguration.comments.lineComment, "--");
});
