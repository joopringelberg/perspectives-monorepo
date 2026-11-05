import assert from "node:assert/strict";
import { test } from "node:test";
import { feedbackEntries } from "../src/feedback.ts";
import { feedbackHeightLimits } from "../src/feedbackSplitter.ts";

test("parser and compiler feedback use one-based positions and the beginning of a range", () => {
  const text = "(ParserError) Expected a role\nline 12, column 3\n" +
    "(UnknownRole) Unknown role (between line 21, column 7 and line 21, column 14).";
  const entries = feedbackEntries(text);
  assert.deepEqual(entries.map(entry => entry.position), [
    { lineNumber: 12, column: 3 }, { lineNumber: 21, column: 7 },
  ]);
  assert.equal(entries.map(entry => entry.text).join(""), text);
});

test("positioned warnings are navigable, unpositioned messages never invent a location", () => {
  assert.deepEqual(feedbackEntries("(Warning) Check line 5, column 2")[0].position, { lineNumber: 5, column: 2 });
  for (const text of ["OK", "Parsing and compiling...", "(Warning) No source location.",
    "line 0, column 2", "line 2, column 0", "line 9007199254740992, column 1"]) {
    assert.equal(feedbackEntries(text)[0].position, undefined);
  }
  assert.deepEqual(feedbackEntries(""), []);
});

test("parser feedback can append its position directly to the parser message", () => {
  assert.deepEqual(feedbackEntries("(ParserError) Expected identifierline 6, column 4")[0].position,
    { lineNumber: 6, column: 4 });
});

test("feedback text is preserved, including multiline and unpositioned messages", () => {
  const text = "Warnings:\n(Warning) No position.\n  Details <script>\n(UnknownRole) line 3, column 9";
  const entries = feedbackEntries(text);
  assert.equal(entries.length, 3);
  assert.equal(entries.map(entry => entry.text).join(""), text);
  assert.equal(entries[1].position, undefined);
  assert.deepEqual(entries[2].position, { lineNumber: 3, column: 9 });
});

test("pane bounds leave room for both panes, even in a small window", () => {
  assert.deepEqual(feedbackHeightLimits(600), { min: 80, max: 520 });
  assert.deepEqual(feedbackHeightLimits(100), { min: 50, max: 50 });
  assert.deepEqual(feedbackHeightLimits(0), { min: 0, max: 0 });
  assert.deepEqual(feedbackHeightLimits(-6), { min: 0, max: 0 });
});
