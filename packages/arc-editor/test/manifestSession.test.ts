import assert from "node:assert/strict";
import { test } from "node:test";
import { externalProperty, parseLaunchParams } from "../src/launchParams.ts";
import { classifyFeedback, ManifestSession, WATCHED_PROPERTIES, type ProxyLike } from "../src/manifestSession.ts";

const type = "model://perspectives.domains#xyfxpg3lzq$j4md0196ew";
function params(role = "$wwijictoy2") {
  const q = new URLSearchParams({ manifest: "def:#m1$External", type, role: type + role, workerUrl: "https://h/www/assets/perspectives-sharedworker", workerName: "pdr-x" });
  const r = parseLaunchParams("?" + q, "https://h");
  if (!r.ok) throw new Error(r.error);
  return r.params;
}

function fakeProxy(values: Record<string, string[]>) {
  const receivers: Record<string, (v: string[]) => void> = {};
  const saved: { pf: any; text: string; role: string }[] = [];
  const unsubscribed: any[] = [];
  const proxy: ProxyLike = {
    getProperty(rol, prop, roleType, receive) {
      assert.equal(rol, "def:#m1$External");
      assert.equal(roleType, type + "$External");
      receivers[prop] = receive;
      assert.match(prop, /^model:\/\/perspectives\.domains#[a-z0-9]{10}\$[a-z0-9]{10}\$External\$[a-z0-9]{10}$/);
      const name = WATCHED_PROPERTIES.find(name => externalProperty(params(), name) === prop);
      assert.ok(name);
      receive(values[name] || []);
      return Promise.resolve({ request: "Unsubscribe", subject: prop, corrId: 1 });
    },
    async saveFile(pf, file, role) {
      saved.push({ pf, text: await file.text(), role });
      return { ...pf, database: pf.database || "db" };
    },
    send(req) { unsubscribed.push(req); return Promise.resolve(); },
  };
  return { proxy, receivers, saved, unsubscribed };
}

test("subscribes to all watched properties and tracks continuous updates", async () => {
  const f = fakeProxy({ ArcSource: ["domain model://x#Y"], ArcFeedback: ["OK"], Published: ["false"] });
  const s = new ManifestSession(f.proxy, params());
  const changes: string[] = [];
  await s.subscribe(p => changes.push(p), () => {});
  assert.equal(changes.length, WATCHED_PROPERTIES.length);
  assert.equal(s.state.ArcSource, "domain model://x#Y");
  f.receivers[type + "$External$sp2animp5c"](["Parsing and compiling the Arc source file for x..."]);
  assert.equal(classifyFeedback(s.state.ArcFeedback), "busy");
  s.close();
  assert.equal(f.unsubscribed.length, WATCHED_PROPERTIES.length);
});

test("save replaces the ArcFile attachment exactly like the file control, keeping database and file name", async () => {
  const arcFile = JSON.stringify({ fileName: "mymodel.arc", propertyType: type + "$External$yy61b8s5rf", mimeType: "text/arc", database: "cw_models", roleFileName: "def:#m1$External" });
  const f = fakeProxy({ ArcFile: [arcFile], Published: ["false"], AutoUpload: ["true"], Store: ["Repository"], ApplyInSession: ["false"] });
  const s = new ManifestSession(f.proxy, params());
  await s.subscribe(() => {}, () => {});
  await s.save("domain model://x#Y\n  case A\n");
  assert.equal(f.saved.length, 1);
  assert.deepEqual(f.saved[0].pf, { fileName: "mymodel.arc", propertyType: type + "$External$yy61b8s5rf", mimeType: "text/arc", database: "cw_models", roleFileName: "def:#m1$External" });
  assert.equal(f.saved[0].text, "domain model://x#Y\n  case A\n");
  assert.equal(f.saved[0].role, type + "$wwijictoy2");
});

test("save without an existing ArcFile derives the file name from DomeinFileName", async () => {
  const f = fakeProxy({ DomeinFileName: ["perspectives_domains-mymodel"], Published: ["false"] });
  const s = new ManifestSession(f.proxy, params());
  await s.subscribe(() => {}, () => {});
  await s.save("x");
  assert.equal(f.saved[0].pf.fileName, "perspectives_domains-mymodel.arc");
  assert.equal(f.saved[0].pf.database, undefined);
});

test("published manifests and non-authors cannot save", async () => {
  const pub = fakeProxy({ Published: ["true"] });
  const s1 = new ManifestSession(pub.proxy, params());
  await s1.subscribe(() => {}, () => {});
  await assert.rejects(s1.save("x"), /published/);
  const other = fakeProxy({ Published: ["false"] });
  const s2 = new ManifestSession(other.proxy, params("$Visitor"));
  await s2.subscribe(() => {}, () => {});
  await assert.rejects(s2.save("x"), /not an Author/);
  assert.equal(pub.saved.length + other.saved.length, 0);
});

test("failed saves propagate", async () => {
  const f = fakeProxy({ Published: ["false"] });
  f.proxy.saveFile = () => Promise.reject(new Error("no write access"));
  const s = new ManifestSession(f.proxy, params());
  await s.subscribe(() => {}, () => {});
  await assert.rejects(s.save("x"), /no write access/);
});

test("feedback classification", () => {
  assert.equal(classifyFeedback(undefined), "none");
  assert.equal(classifyFeedback("OK. Model compiled"), "ok");
  assert.equal(classifyFeedback("(ParserError) line 3 ..."), "error");
});
