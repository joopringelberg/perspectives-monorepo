import assert from "node:assert/strict";
import { test } from "node:test";
import { arcEditorUrl, isVersionedModelManifest } from "../../mycontexts/src/arcEditorLaunch.ts";
import { externalProperty, isAuthor, parseLaunchParams } from "../src/launchParams.ts";

const type = "model://perspectives.domains#CouchdbManagement$VersionedModelManifest";
const launch = {
  baseUrl: "https://mycontexts.com/www/",
  manifest: "def:#abc$External",
  contextType: type,
  userRoleType: type + "$Author",
  worker: { url: "https://mycontexts.com/www/assets/perspectives-sharedworker", name: "pdr-3.0.0-build-1" },
};

test("MyContexts recognises VersionedModelManifest contexts (with or without version)", () => {
  assert.ok(isVersionedModelManifest(type));
  assert.ok(isVersionedModelManifest("model://perspectives.domains#CouchdbManagement@12.4$VersionedModelManifest"));
  assert.ok(!isVersionedModelManifest("model://perspectives.domains#CouchdbManagement$ModelManifest"));
  assert.ok(!isVersionedModelManifest(undefined));
});

test("launch URL round-trips into the editor's launch parameters", () => {
  const url = new URL(arcEditorUrl(launch));
  assert.equal(url.pathname, "/www/arceditor/");
  const r = parseLaunchParams(url.search, url.origin);
  assert.ok(r.ok);
  if (!r.ok) return;
  assert.equal(r.params.manifest, launch.manifest);
  assert.equal(r.params.userRoleType, launch.userRoleType);
  assert.deepEqual(r.params.worker, launch.worker);
  assert.equal(externalProperty(r.params, "ArcFile"), type + "$External$ArcFile");
  assert.ok(isAuthor(r.params));
});

test("missing manifest or worker connection is reported", () => {
  assert.equal(parseLaunchParams("", "https://mycontexts.com").ok, false);
  const noWorker = new URL(arcEditorUrl({ ...launch, worker: undefined }));
  const r = parseLaunchParams(noWorker.search, noWorker.origin);
  assert.equal(r.ok, false);
  assert.match((r as { error: string }).error, /SharedWorker/);
});

test("a worker URL from another origin is refused", () => {
  const url = new URL(arcEditorUrl({ ...launch, worker: { url: "https://evil.example/w.js", name: "x" } }));
  const r = parseLaunchParams(url.search, "https://mycontexts.com");
  assert.equal(r.ok, false);
});
