// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { test, mock } from "node:test";
import { statistics, aggregate, normalizeRounds, normalizeTrials } from "../scripts/performance-statistics.mjs";
import * as collector from "./layertests/PerformanceMeasurements.js";
import * as profiler from "../src/core/performance.js";

function timingCapture(t, enabled) {
  const previous = globalThis.pdrTraceTimings;
  globalThis.pdrTraceTimings = enabled;
  t.after(() => {
    if (previous === undefined) delete globalThis.pdrTraceTimings;
    else globalThis.pdrTraceTimings = previous;
  });
  const output = t.mock.method(globalThis.console, "info", () => {});
  return () => output.mock.calls.map(({ arguments: [line] }) =>
    JSON.parse(line.slice("[PDR TIMING] ".length)));
}

test("disabled timing does not read the clock or emit output", t => {
  const events = timingCapture(t, undefined);
  t.mock.method(globalThis.performance, "now", () => {
    throw new Error("Disabled timing must not read the clock");
  });
  profiler.traceTimingImpl("transport.receive", "message-1");
  assert.equal(profiler.startTiming("incoming.verify")("1")(), null);
  profiler.finishTiming(null)(false)();
  assert.deepEqual(events(), []);
});

test("timing spans correlate overlapping work and report monotonic durations and failure", t => {
  const events = timingCapture(t, true);
  let now = 10;
  t.mock.method(globalThis.performance, "now", () => now);
  const first = profiler.startTiming("incoming.verify")("1")();
  now = 12;
  const second = profiler.startTiming("incoming.verify")("2")();
  now = 20;
  profiler.finishTiming(second)(false)();
  now = 25;
  profiler.finishTiming(first)(true)();
  profiler.finishTiming(first)(true)();
  const rows = events();
  assert.deepEqual(rows.map(row => [row.event, row.id]), [
    ["start", "1"], ["start", "2"], ["end", "2"], ["end", "1"]
  ]);
  assert.notEqual(rows[0].span, rows[1].span);
  assert.equal(rows[2].span, rows[1].span);
  assert.equal(rows[3].span, rows[0].span);
  assert.equal(rows[2].elapsedMs, 8);
  assert.equal(rows[2].succeeded, false);
  assert.equal(rows[3].elapsedMs, 15);
  assert.equal(rows[3].succeeded, true);
  for (const row of rows) assert.ok(Number.isFinite(Date.parse(row.at)));
});

test("turning timing off suppresses events and outstanding span completion", t => {
  const events = timingCapture(t, true);
  const token = profiler.startTiming("incoming.decrypt")("message-1")();
  globalThis.pdrTraceTimings = false;
  profiler.traceTimingImpl("consumer.receive", "message-2");
  profiler.finishTiming(token)(true)();
  assert.equal(events().length, 1);
});

test("compiled timing wrapper preserves results and errors with tracing on or off", async t => {
  const events = timingCapture(t, false);
  const [runtime, aff, either] = await Promise.all([
    import("../output/Perspectives.Performance/index.js"),
    import("../output/Effect.Aff/index.js"),
    import("../output/Data.Either/index.js")
  ]);
  const timed = runtime.timed(aff.monadEffectAff)(aff.monadErrorAff);
  const run = action => new Promise((resolve, reject) => {
    aff.runAff(result => () => {
      if (result instanceof either.Left) reject(result.value0);
      else resolve(result.value0);
    })(action)();
  });
  const failure = new Error("private-error-detail");
  for (const enabled of [false, true]) {
    globalThis.pdrTraceTimings = enabled;
    assert.equal(await run(timed("test.success")("1")(aff.monadEffectAff.liftEffect(() => 42))), 42);
    await assert.rejects(run(timed("test.failure")("2")(
      aff.monadEffectAff.liftEffect(() => { throw failure; })
    )), error => error === failure);
  }
  const rows = events();
  assert.equal(rows.length, 4);
  assert.deepEqual(rows.filter(row => row.event === "end").map(row => row.succeeded), [true, false]);
  assert.ok(!JSON.stringify(rows).includes(failure.message));
});

async function transactionDeltaFixture(t) {
  const previousSelf = globalThis.self;
  globalThis.self ??= globalThis;
  t.after(() => {
    if (previousSelf === undefined) delete globalThis.self;
    else globalThis.self = previousSelf;
  });
  const [transaction, deltaEq, types, maybe, set, json, aff, either] = await Promise.all([
    import("../output/Perspectives.Sync.Transaction/index.js"),
    import("../output/Perspectives.Sync.DeltaInTransaction/index.js"),
    import("../output/Perspectives.Representation.TypeIdentifiers/index.js"),
    import("../output/Data.Maybe/index.js"),
    import("../output/Data.Set/index.js"),
    import("../output/Simple.JSON/index.js"),
    import("../output/Effect.Aff/index.js"),
    import("../output/Data.Either/index.js")
  ]);
  const run = action => new Promise((resolve, reject) => {
    aff.runAff(result => () => {
      if (result instanceof either.Left) reject(result.value0);
      else resolve(result.value0);
    })(action)();
  });
  const create = () => run(transaction.createTransaction(
    types.ENR.create("model://example.invalid#History$User")
  )(true));
  const makeDelta = (version, users = ["recipient-a"]) => ({
    users,
    delta: {
      author: "author",
      encryptedDelta: JSON.stringify({
        resourceKey: "def:#external#ConnectedToAMQPBroker",
        resourceVersion: version,
        deltaType: "SetProperty",
        values: [version % 2 === 0 ? "false" : "true"]
      }),
      signature: new maybe.Just(`signature-${version}`)
    }
  });
  return { transaction, deltaEq, maybe, set, json, aff, either, run, create, makeDelta };
}

test("indexed delta membership preserves structural equality, recipients, order and transaction lifecycle", async t => {
  const { transaction, deltaEq, maybe, set, json, either, create, makeDelta } = await transactionDeltaFixture(t);
  const base = await create();
  const first = makeDelta(1, ["recipient-a", "recipient-b"]);
  const withSigned = fields => ({ ...first, delta: { ...first.delta, ...fields } });
  const variants = [
    first,
    { ...first, users: ["recipient-b", "recipient-a"] },
    { ...first, users: ["recipient-a"] },
    { ...first, users: ["recipient-a", "recipient-b", "recipient-a"] },
    withSigned({ author: "another-author" }),
    withSigned({ encryptedDelta: `${first.delta.encryptedDelta} ` }),
    withSigned({ signature: maybe.Nothing.value }),
    withSigned({ signature: new maybe.Just("") }),
    { ...first, users: [] }
  ];
  const attempts = [
    ...variants,
    ...variants.map(({ users, delta }) => ({ users: [...users], delta: { ...delta } })),
    ...[...variants].reverse()
  ];
  const expected = [];
  let actual = base;
  for (const delta of attempts) {
    if (!expected.some(previous => deltaEq.eqDeltaInTransaction.eq(previous)(delta))) expected.push(delta);
    actual = transaction.appendTransactionDelta(delta)(actual);
  }
  assert.deepEqual(actual.deltas, expected);
  assert.equal(set.size(actual.deltaMembership), expected.length);
  assert.deepEqual(base.deltas, []);
  const encoded = JSON.parse(json.writeJSON(transaction.writeForeignTransaction)(actual));
  assert.equal(Object.hasOwn(encoded, "deltaMembership"), false);
  const restored = json.readJSON(transaction.readForeignTransaction)(
    JSON.stringify({ ...encoded, publicKeys: [] })
  );
  assert.ok(restored instanceof either.Right);
  assert.deepEqual(transaction.appendTransactionDelta(first)(restored.value0).deltas, expected);
  const cleared = transaction.clearTransactionDeltas(actual);
  assert.deepEqual(transaction.appendTransactionDelta(first)(cleared).deltas, [first]);
  const independent = await create();
  assert.deepEqual(transaction.appendTransactionDelta(first)(independent).deltas, [first]);
});

test("manager-sized history replay retains all delta-recipient pairs without repeated array searches", async t => {
  const { transaction, set, create, makeDelta } = await transactionDeltaFixture(t);
  const history = Array.from({ length: 1086 + 5 }, (_, i) => makeDelta(i).delta);
  let result = await create();
  const started = performance.now();
  for (let perspective = 0; perspective < 4; perspective++) {
    for (let context = 0; context < 14; context++) {
      for (const delta of history) {
        result = transaction.appendTransactionDelta({ users: [`recipient-${context}`], delta })(result);
      }
    }
  }
  const elapsedMs = performance.now() - started;
  assert.equal(result.deltas.length, 15274);
  assert.equal(set.size(result.deltaMembership), 15274);
  assert.deepEqual(result.deltas[0], { users: ["recipient-0"], delta: history[0] });
  assert.deepEqual(result.deltas.at(-1), { users: ["recipient-13"], delta: history.at(-1) });
  t.diagnostic(`61,096 delta-add attempts, 15,274 retained entries: ${elapsedMs.toFixed(1)} ms`);
});

test("stored history bypasses persistence while new and incoming deltas retain their storage rules", async t => {
  const { maybe, run, create, makeDelta } = await transactionDeltaFixture(t);
  const [deltas, core, avar, map] = await Promise.all([
    import("../output/Perspectives.Deltas/index.js"),
    import("../output/Perspectives.CoreTypes/index.js"),
    import("../output/Effect.AVar/index.js"),
    import("../output/Data.Map/index.js")
  ]);
  const unavailableStorage = new Error("storage-unavailable");
  let storageAccesses = 0;
  const state = avar.new({
    logConfig: { defaultLevel: core.Silent.value, topicLevels: map.empty },
    logColor: maybe.Nothing.value,
    // Fail database-name resolution without disabling runtime state needed by logging.
    get systemIdentifier() {
      storageAccesses++;
      throw unavailableStorage;
    }
  })();
  const transaction = avar.new(await create())();
  const incoming = avar.new({ ...await create(), isExecutingIncomingDeltas: true })();
  const delta = makeDelta(1, []);
  const record = {
    _id: "stored-delta", _rev: new maybe.Just("1-revision"),
    resourceKey: "external#ConnectedToAMQPBroker", resourceVersion: 1,
    author: delta.delta.author, deltaId: new maybe.Just("stored-delta-id"),
    signedDelta: delta.delta, deltaType: "SetProperty",
    applied: true, disposition: new maybe.Just("Applied"), contextKey: maybe.Nothing.value
  };
  const execute = (scope, action) => run(core.runMonadPerspectives(action(scope))(state));
  await execute(transaction, deltas.addStoredDelta([])(record));
  assert.equal(storageAccesses, 0);
  await assert.rejects(execute(transaction, deltas.addDelta(delta)),
    error => error === unavailableStorage);
  assert.equal(storageAccesses, 1);
  await execute(incoming, deltas.addDelta(delta));
  assert.equal(storageAccesses, 1);
});

function captureReport(run, publish = true) {
  const messages = [];
  const originalCode = process.exitCode;
  const originalSend = process.send;
  const originalProfile = process.env.PDR_PERF_PROFILE;
  process.send = (report, callback) => {
    messages.push(structuredClone(report));
    callback?.();
  };
  mock.method(process, "exit", () => {});
  try {
    collector.configureScenarios(["scenario"])();
    run();
    if (publish) collector.publishResults(true)();
    return messages.at(-1);
  } finally {
    process.exitCode = originalCode;
    if (originalSend === undefined) delete process.send;
    else process.send = originalSend;
    mock.restoreAll();
    if (originalProfile === undefined) delete process.env.PDR_PERF_PROFILE;
    else process.env.PDR_PERF_PROFILE = originalProfile;
  }
}

function normalisationCapture(t, enabled = true) {
  const previous = globalThis.pdrMeasureNormalisation;
  globalThis.pdrMeasureNormalisation = true;
  profiler.resetNormalisationMeasurements();
  globalThis.pdrMeasureNormalisation = enabled;
  t.after(() => {
    if (previous === undefined) delete globalThis.pdrMeasureNormalisation;
    else globalThis.pdrMeasureNormalisation = previous;
  });
  const output = t.mock.method(globalThis.console, "info", () => {});
  const prefix = "[PDR NORMALISATION] ";
  return () => output.mock.calls
    .map(({ arguments: [line] }) => line)
    .filter(line => line.startsWith(prefix))
    .map(line => JSON.parse(line.slice(prefix.length)));
}

test("disabled normalisation measurements do not read the clock or emit reports", t => {
  const reports = normalisationCapture(t, undefined);
  globalThis.pdrMeasureNormalisation = undefined;
  t.mock.method(globalThis.performance, "now", () => {
    throw new Error("Disabled normalisation measurements must not read the clock");
  });
  assert.equal(profiler.startNormalisation("alice")("role")(false)(), null);
  profiler.finishNormalisation(null)(true)();
  profiler.reportNormalisationMeasurements();
  assert.deepEqual(reports(), []);
});

test("normalisation aggregates isolate PDRs, compilation and start phases without per-call logs", t => {
  const reports = normalisationCapture(t);
  let now = 0;
  t.mock.method(globalThis.performance, "now", () => now);
  profiler.setNormalisationPhase("startup")();
  const first = profiler.startNormalisation("alice")("role")(false)();
  now = 1;
  const overlapping = profiler.startNormalisation("alice")("role")(false)();
  now = 2;
  const bob = profiler.startNormalisation("bob")("role")(false)();
  now = 3;
  const compilation = profiler.startNormalisation("alice")("role")(true)();
  now = 5;
  profiler.finishNormalisation(overlapping)(false)();
  now = 8;
  profiler.finishNormalisation(first)(true)();
  now = 9;
  profiler.finishNormalisation(bob)(true)();
  now = 10;
  profiler.finishNormalisation(compilation)(true)();
  now = 12;
  const repeated = profiler.startNormalisation("alice")("role")(false)();
  profiler.setNormalisationPhase("signup")();
  now = 16;
  profiler.finishNormalisation(repeated)(true)();
  profiler.finishNormalisation(repeated)(true)();
  now = 20;
  const nextPhase = profiler.startNormalisation("alice")("role")(false)();
  now = 23;
  profiler.finishNormalisation(nextPhase)(true)();
  assert.deepEqual(reports(), []);
  profiler.reportNormalisationMeasurements();
  const rows = reports();
  const alice = rows.find(row => row.pdr === "alice" && row.context === "runtime");
  assert.deepEqual(alice.topRoles, [{
    role: "role", requests: 4, succeeded: 3, failed: 1, unfinished: 0,
    elapsedMs: 19, repeatRequests: 2, repeatedElapsedMs: 7
  }]);
  assert.equal(alice.distinctRoles, 1);
  assert.deepEqual(alice.phases.map(row =>
    [row.phase, row.requests, row.repeatRequests, row.repeatedElapsedMs]),
  [["startup", 3, 1, 4], ["signup", 1, 0, 0]]);
  assert.equal(rows.find(row => row.pdr === "bob").repeatRequests, 0);
  assert.equal(rows.find(row => row.context === "compilation").repeatRequests, 0);
});

test("failed normalisation does not warm a later request", t => {
  const reports = normalisationCapture(t);
  const failed = profiler.startNormalisation("alice")("role")(false)();
  profiler.finishNormalisation(failed)(false)();
  const retry = profiler.startNormalisation("alice")("role")(false)();
  profiler.finishNormalisation(retry)(true)();
  profiler.reportNormalisationMeasurements();
  assert.equal(reports()[0].failed, 1);
  assert.equal(reports()[0].succeeded, 1);
  assert.equal(reports()[0].repeatRequests, 0);
});

test("resetting normalisation measurements excludes late completions from the previous collection", t => {
  const reports = normalisationCapture(t);
  const old = profiler.startNormalisation("alice")("role")(false)();
  profiler.resetNormalisationMeasurements();
  const current = profiler.startNormalisation("alice")("role")(false)();
  profiler.finishNormalisation(old)(true)();
  profiler.reportNormalisationMeasurements();
  assert.equal(reports()[0].requests, 1);
  assert.equal(reports()[0].unfinished, 1);
  profiler.finishNormalisation(current)(true)();
  profiler.reportNormalisationMeasurements();
  assert.equal(reports()[1].succeeded, 1);
  assert.equal(reports()[1].repeatRequests, 0);
});

test("compiled normalisation measurement preserves results and errors and omits error details", async t => {
  const reports = normalisationCapture(t);
  const [runtime, aff, either] = await Promise.all([
    import("../output/Perspectives.Performance/index.js"),
    import("../output/Effect.Aff/index.js"),
    import("../output/Data.Either/index.js")
  ]);
  runtime.resetNormalisationMeasurements();
  const measured = runtime.measureNormalisation(aff.monadEffectAff)(aff.monadErrorAff);
  const run = action => new Promise((resolve, reject) => {
    aff.runAff(result => () => {
      if (result instanceof either.Left) reject(result.value0);
      else resolve(result.value0);
    })(action)();
  });
  const failure = new Error("private-normalisation-error");
  for (const enabled of [false, true]) {
    globalThis.pdrMeasureNormalisation = enabled;
    assert.equal(await run(measured("alice")("success-role")(false)(
      aff.monadEffectAff.liftEffect(() => 42)
    )), 42);
    await assert.rejects(run(measured("alice")("failed-role")(false)(
      aff.monadEffectAff.liftEffect(() => { throw failure; })
    )), error => error === failure);
  }
  runtime.reportNormalisationMeasurements();
  const [row] = reports();
  assert.equal(row.requests, 2);
  assert.equal(row.succeeded, 1);
  assert.equal(row.failed, 1);
  assert.equal(row.unfinished, 0);
  assert.ok(!JSON.stringify(row).includes(failure.message));
});

test("statistics use nearest-rank p95 and population standard deviation", () => {
  assert.deepEqual(statistics([4, 1, 3, 2]), {
    count: 4, min: 1, median: 2.5, mean: 2.5, p95: 4, max: 4, standardDeviation: Math.sqrt(1.25)
  });
  assert.equal(statistics(Array.from({ length: 100 }, (_, i) => i + 1)).p95, 95);
  assert.equal(statistics([]).mean, null);
  assert.equal(statistics([7]).standardDeviation, 0);
  for (const value of [NaN, Infinity, -1, undefined]) assert.throws(() => statistics([value]));
});

test("sender timing excludes setup and Bob polling; completion is observed separately", () => {
  const report = captureReport(() => {
    let now = 10;
    mock.method(performance, "now", () => now);
    collector.beginTrial("scenario")();
    now = 1000; // Untimed setup.
    collector.beginAction();
    now = 1020;
    collector.endAction();
    now = 1120; // Includes Bob polling delay.
    collector.endCompletion();
    collector.endTrial("success")(true)();
  });
  assert.deepEqual(report.trials[0], {
    scenario: "scenario", status: "success", succeeded: true,
    senderActionMs: 20, bobCompletionMs: 100, endToEndMs: 120,
    senderCompleted: true, completionObserved: true, profile: null
  });
});

test("semantic failures and timeouts retain elapsed timings without claiming completion", () => {
  for (const status of ["semantic-failure", "completion-timeout"]) {
    const report = captureReport(() => {
      let now = 0;
      mock.method(performance, "now", () => now);
      collector.beginTrial("scenario")();
      collector.beginAction();
      now = 5;
      collector.endAction();
      now = 20;
      collector.endCompletion();
      collector.endTrial(status)(false)();
    });
    assert.equal(report.trials[0].succeeded, false);
    assert.equal(report.trials[0].endToEndMs, 20);
    assert.equal(report.trials[0].completionObserved, status !== "completion-timeout");
  }
});

test("setup and sender exceptions report failures, not invented completed samples", () => {
  const setup = captureReport(() => {
    collector.beginTrial("scenario")();
    collector.endTrial("exception")(false)();
  }).trials[0];
  assert.equal(setup.senderActionMs, null);
  const sender = captureReport(() => {
    let now = 10;
    mock.method(performance, "now", () => now);
    collector.beginTrial("scenario")();
    collector.beginAction();
    now = 15;
    collector.endTrial("exception")(false)();
  }).trials[0];
  assert.equal(sender.senderActionMs, 5);
  assert.equal(sender.senderCompleted, false);
  assert.equal(sender.bobCompletionMs, null);
});

test("profiling is disabled by default and records only counts and spans when opted in", () => {
  assert.equal(profiler.profilingEnabled(), false);
  assert.equal(profiler.startProfile("decrypt")(), null);
  profiler.countEncrypted(profiler.captureSession())("secret")(1)();
  const report = captureReport(() => {
    process.env.PDR_PERF_PROFILE = "1";
    collector.beginTrial("scenario")();
    collector.beginAction();
    const session = profiler.captureSession();
    const token = profiler.startProfile("decrypt")();
    profiler.countEncrypted(session)("é")(2)();
    profiler.countDecrypted(session)("€")(3)(4)();
    profiler.finishProfile(token)(false)();
    collector.endAction();
    collector.endCompletion();
    collector.endTrial("success")(true)();
  });
  assert.deepEqual(report.trials[0].profile.counts, {
    messages: 1, deltas: 3, publicKeys: 4, wrappedKeys: 2, encryptedUtf8Bytes: 2, decryptedUtf8Bytes: 3
  });
  assert.equal(report.trials[0].profile.spans.decrypt.failed, 1);
  assert.equal(profiler.profilingEnabled(), false);
  assert.equal(JSON.stringify(report).includes("secret"), false);
  assert.equal(JSON.stringify(report).includes("€"), false);
});

test("Bob observation freezes unfinished spans and pins late phases away from the next trial", () => {
  const report = captureReport(() => {
    process.env.PDR_PERF_PROFILE = "1";
    collector.beginTrial("scenario")();
    collector.beginAction();
    const oldSession = profiler.captureSession();
    const oldCascade = profiler.startProfileInSession(oldSession)("incomingCascade")();
    const completedExecution = profiler.startProfileInSession(oldSession)("executeTransaction")();
    profiler.finishProfile(completedExecution)(true)();
    collector.endAction();
    collector.endCompletion();
    collector.endTrial("success")(true)();
    assert.equal(profiler.sessionEnabled(oldSession)(), false);

    collector.beginTrial("next-scenario")();
    collector.beginAction();
    profiler.finishProfile(oldCascade)(true)();
    assert.equal(profiler.startProfileInSession(oldSession)("publicStates")(), null);
    profiler.countDecrypted(oldSession)("private payload")(10)(20)();
    collector.endAction();
    collector.endCompletion();
    collector.endTrial("success")(true)();
  });
  assert.deepEqual(report.trials[0].profile.spans.incomingCascade, {
    count: 1, completed: 0, failed: 0, unfinished: 1, totalMs: 0
  });
  assert.equal(report.trials[0].profile.spans.executeTransaction.completed, 1);
  assert.equal(report.trials[0].profile.spans.executeTransaction.unfinished, 0);
  assert.deepEqual(report.trials[1].profile.spans, {});
  assert.equal(report.trials[1].profile.counts.deltas, 0);
  assert.equal(JSON.stringify(report).includes("private payload"), false);
});

test("work after endCompletion but before endTrial cannot change the observation profile", () => {
  const report = captureReport(() => {
    process.env.PDR_PERF_PROFILE = "1";
    collector.beginTrial("scenario")();
    collector.beginAction();
    const session = profiler.captureSession();
    profiler.countEncrypted(session)("é")(2)();
    const cascade = profiler.startProfileInSession(session)("incomingCascade")();
    collector.endAction();
    collector.endCompletion();
    assert.equal(profiler.sessionEnabled(session)(), false);
    assert.equal(profiler.profilingEnabled(), false);
    profiler.countEncrypted(session)("late ciphertext")(10)();
    profiler.countDecrypted(session)("late payload")(30)(40)();
    profiler.finishProfile(cascade)(true)();
    assert.equal(profiler.startProfileInSession(session)("publicStates")(), null);
    collector.endTrial("success")(true)();
  });
  assert.deepEqual(report.trials[0].profile.spans.incomingCascade, {
    count: 1, completed: 0, failed: 0, unfinished: 1, totalMs: 0
  });
  assert.deepEqual(report.trials[0].profile.counts, {
    messages: 1, deltas: 0, publicKeys: 0, wrappedKeys: 2, encryptedUtf8Bytes: 2, decryptedUtf8Bytes: 0
  });
  assert.equal(JSON.stringify(report).includes("late payload"), false);
});

test("aggregation excludes warmup timings, retains failed timings and missing scenarios", () => {
  const success = { scenario: "scenario", succeeded: true, status: "success", senderActionMs: 10, bobCompletionMs: 20, endToEndMs: 30 };
  const failure = { ...success, succeeded: false, status: "semantic-failure", senderActionMs: 50 };
  const missing = normalizeTrials(["scenario"], []);
  const summary = aggregate([
    { warmup: true, trials: [{ ...success, senderActionMs: 1000 }] },
    { warmup: false, trials: [success] },
    { warmup: false, trials: [failure] },
    { warmup: false, trials: missing }
  ])[0];
  assert.equal(summary.trials, 3);
  assert.equal(summary.succeeded, 1);
  assert.equal(summary.failed, 2);
  assert.equal(summary.notRun, 1);
  assert.equal(summary.timings.senderActionMs.count, 2);
  assert.equal(summary.timings.senderActionMs.mean, 30);
  assert.throws(() => normalizeTrials(["scenario"], [success, success]));
  assert.throws(() => normalizeTrials(["scenario"], [{ ...success, scenario: "unknown" }]));
});

test("missing snapshots produce a clear prerequisite failure without invented timing samples", () => {
  const failure = captureReport(() => {
    mock.method(console, "error", () => {});
    collector.publishFailure("missing-snapshots")();
  }, false);
  assert.equal(failure.failureReason, "missing-snapshots");
  assert.equal(failure.suiteCompleted, false);
  assert.deepEqual(failure.trials, []);
});

test("manifest-less worker failures are counted for scenarios learned from another round", () => {
  const normalized = normalizeRounds([
    { warmup: false, succeeded: false, status: "worker-failure", trials: [] },
    { warmup: false, succeeded: true, status: "success", trials: [{
      scenario: "scenario", status: "success", succeeded: true,
      senderActionMs: 10, bobCompletionMs: 20, endToEndMs: 30
    }] }
  ]);
  assert.deepEqual(normalized.expectedScenarios, ["scenario"]);
  assert.equal(normalized.unknownScenarioFailureCount, 0);
  assert.equal(normalized.rounds[0].trials[0].status, "not-run");
  const summary = aggregate(normalized.rounds)[0];
  assert.equal(summary.trials, 2);
  assert.equal(summary.failed, 1);
  assert.equal(summary.succeeded, 1);
});

test("all workers failing before a manifest produce an explicit unknown-scenario failure count", () => {
  const normalized = normalizeRounds([
    { warmup: true, succeeded: false, trials: [] },
    { warmup: false, succeeded: false, trials: [] }
  ]);
  assert.deepEqual(normalized.expectedScenarios, []);
  assert.equal(normalized.unknownScenarioFailureCount, 2);
  assert.deepEqual(aggregate(normalized.rounds), []);
});

test("existing scaffold entrypoints remain uninstrumented and measurements require process isolation", async () => {
  const scaffold = await readFile(new URL("./layertests/Layer3Scaffold.purs", import.meta.url), "utf8");
  assert.match(scaffold, /getSynchronisationResults = getSynchronisationResultsInternal Nothing withTwoPDRsCached true/);
  assert.match(scaffold, /credentials <- requireAMQPAdministratorCredentials cfg\s+getSynchronisationResultsInternal Nothing \(withTwoPDRsCachedNoBus credentials\) false cacheRef cfg/);
  assert.match(scaffold, /executeModelTest = executeModelTestInternal Nothing/);
  assert.match(scaffold, /case cached of\s+Just results -> pure results/);
  assert.match(scaffold, /available <- performanceSnapshotsAvailable cfg\s+unless available \$ throwError/);
  assert.match(scaffold, /Just _ -> pure unit\s+Nothing -> attempt \(snapshotPDR/);
  assert.match(scaffold, /aliceExists <- snapshotExists cfg.snapshotDirAlice/);
  assert.match(scaffold, /bobExists <- snapshotExists cfg.snapshotDirBob/);
  assert.match(scaffold, /hooks.endTrial "exception" false\s+throwError err/);
  assert.match(scaffold, /Performance scenario failed; aborting the measured suite/);
  const runner = await readFile(new URL("../scripts/performance-runner.mjs", import.meta.url), "utf8");
  assert.match(runner, /fork\(/);
  assert.match(runner, /fresh-process-per-round/);
  assert.match(runner, /repetitionUnit: "full-suite"/);
  assert.match(runner, /warmupScope: "external-and-filesystem-caches-only"/);
  assert.match(runner, /withinRoundCachePolicy: "shared-by-ordered-scenarios"/);
  assert.match(runner, /profileBoundary: "bob-result-observation-or-trial-failure-not-receiver-quiescence"/);
  assert.match(runner, /encryptedByteMetric: "utf8-ciphertext-string-not-total-wire-bytes"/);
});
