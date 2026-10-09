// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import { test, mock } from "node:test";
import { statistics, aggregate, normalizeRounds, normalizeTrials } from "../scripts/performance-statistics.mjs";
import * as collector from "./PerformanceMeasurements.js";
import * as profiler from "../src/core/performance.js";

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
  const scaffold = await readFile(new URL("./Layer3Scaffold.purs", import.meta.url), "utf8");
  assert.match(scaffold, /getSynchronisationResults = getSynchronisationResultsInternal Nothing withTwoPDRsCached true/);
  assert.match(scaffold, /getSynchronisationResultsOverAMQP = getSynchronisationResultsInternal Nothing withTwoPDRsCachedNoBus false/);
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
