import assert from "node:assert/strict";
import { test } from "node:test";
import { arcEditorUrl, isVersionedModelManifest } from "../../mycontexts/src/arcEditorLaunch.ts";
import { externalProperty, isAuthor, parseLaunchParams } from "../src/launchParams.ts";

const type = "model://perspectives.domains#xyfxpg3lzq$j4md0196ew";
const launch = {
  baseUrl: "https://mycontexts.com/www/",
  manifest: "def:#abc$External",
  contextType: type,
  userRoleType: type + "$wwijictoy2",
  worker: { url: "https://mycontexts.com/www/assets/perspectives-sharedworker", name: "pdr-3.0.0-build-1" },
};

test("MyContexts recognises stable VersionedModelManifest contexts (with or without version)", () => {
  assert.ok(isVersionedModelManifest("model://perspectives.domains#xyfxpg3lzq$j4md0196ew"));
  assert.ok(isVersionedModelManifest("model://perspectives.domains#xyfxpg3lzq@12.4$j4md0196ew"));
  assert.ok(!isVersionedModelManifest("model://perspectives.domains#xyfxpg3lzq$anothercontext"));
  assert.ok(!isVersionedModelManifest("model://perspectives.domains#CouchdbManagement$VersionedModelManifest"));
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
  assert.equal(externalProperty(r.params, "ArcFile"), type + "$External$yy61b8s5rf");
  assert.equal(externalProperty(r.params, "LastUpload"), type + "$External$xswkl4m5un");
  assert.equal(externalProperty(r.params, "DomeinFileName"), "model://perspectives.domains#tiodn6tcyc$mo66to1n9c$External$kpz5vhpz0n");
  assert.ok(isAuthor(r.params));
  assert.ok(!isAuthor({ ...r.params, userRoleType: type + "$Author" }));
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
