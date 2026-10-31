import test from "node:test";
import assert from "node:assert/strict";
import { NavigationHistory } from "../src/navigation.ts";

test("back and forward walk the jump history; a new jump clears forward", () => {
  const h = new NavigationHistory<string>();
  assert.equal(h.back("a"), undefined);
  h.record("a");          // a -> b
  h.record("b");          // b -> c
  assert.equal(h.back("c"), "b");
  assert.equal(h.back("b"), "a");
  assert.ok(!h.canGoBack());
  assert.equal(h.forward("a"), "b");
  assert.equal(h.forward("b"), "c");
  assert.equal(h.forward("c"), undefined);
  h.back("c");
  h.record("b");          // b -> d
  assert.ok(!h.canGoForward());
  assert.equal(h.back("d"), "b");
});

test("limit and prune", () => {
  const h = new NavigationHistory<number>(2);
  h.record(1); h.record(2); h.record(3);
  h.prune(n => n !== 3);
  assert.equal(h.back(9), 2);
  assert.equal(h.back(2), undefined);
});
