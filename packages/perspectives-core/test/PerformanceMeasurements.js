// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

const sessionKey = Symbol.for("perspectives.performance.session");
const trials = [];
let current;
let expectedScenarios = [];
let failureReason = null;

export const configureScenarios = scenarios => () => {
  trials.length = 0;
  current = undefined;
  failureReason = null;
  delete globalThis[sessionKey];
  expectedScenarios = scenarios;
  if (typeof process.send === "function") {
    process.send({ type: "perspectives-performance-manifest", expectedScenarios });
  }
};

export const beginTrial = scenario => () => {
  current = {
    scenario, status: "incomplete", succeeded: false,
    senderActionMs: null, bobCompletionMs: null, endToEndMs: null,
    senderCompleted: false, completionObserved: false, profile: null
  };
  if (typeof process.send === "function") {
    process.send({ type: "perspectives-performance-trial-started", scenario });
  }
};

export const beginAction = () => {
  current.actionStart = performance.now();
  if (process.env.PDR_PERF_PROFILE === "1") {
    current.profile = {
      active: true, spans: {},
      counts: { messages: 0, deltas: 0, publicKeys: 0, wrappedKeys: 0, encryptedUtf8Bytes: 0, decryptedUtf8Bytes: 0 }
    };
    globalThis[sessionKey] = current.profile;
  }
};

export const endAction = () => {
  current.senderEnd = performance.now();
  current.senderActionMs = current.senderEnd - current.actionStart;
  current.senderCompleted = true;
};

function freezeProfile() {
  const session = current?.profile;
  if (!session?.active) return;
  session.active = false;
  current.profile = {
    spans: Object.fromEntries(Object.entries(session.spans).map(([label, span]) => [label, { ...span }])),
    counts: { ...session.counts }
  };
  if (globalThis[sessionKey] === session) delete globalThis[sessionKey];
}

export const endCompletion = () => {
  const end = performance.now();
  current.bobCompletionMs = end - current.senderEnd;
  current.endToEndMs = end - current.actionStart;
  current.completionObserved = true;
  freezeProfile();
};

export const endTrial = status => succeeded => () => {
  if (current.actionStart !== undefined && current.senderEnd === undefined) {
    current.senderActionMs = performance.now() - current.actionStart;
  }
  if (current.senderEnd !== undefined && !current.completionObserved) {
    const end = performance.now();
    current.bobCompletionMs = end - current.senderEnd;
    current.endToEndMs = end - current.actionStart;
  }
  // A timeout is an elapsed wait, not an observed successful completion.
  if (status === "completion-timeout") current.completionObserved = false;
  freezeProfile();
  delete current.actionStart;
  delete current.senderEnd;
  current.status = status;
  current.succeeded = succeeded;
  trials.push(current);
  if (typeof process.send === "function") {
    process.send({ type: "perspectives-performance-trial", trial: current });
  }
  current = undefined;
};

export const amqpMode = () => process.env.PDR_PERF_MODE === "amqp";

export const publishFailure = reason => () => {
  failureReason = reason === "missing-snapshots" ? reason : "suite-failure";
  if (failureReason === "missing-snapshots") {
    console.error("Performance measurements require complete existing snapshots for both PDRs; bootstrap is disabled. Prepare snapshots with the existing scaffold before benchmarking.");
  }
  publishResults(false)();
};

export const publishResults = suiteCompleted => () => {
  const report = { type: "perspectives-performance", suiteCompleted, failureReason, expectedScenarios, trials };
  const exitCode = suiteCompleted && trials.length > 0 && trials.every(trial => trial.succeeded) ? 0 : 1;
  // The dedicated worker has finished scaffold teardown. Failed startup can
  // leave runtime fibers alive; do not let those turn a reported failure into
  // a five-minute harness hang. Flush the report before terminating the worker.
  if (typeof process.send === "function") {
    process.send(report, error => process.exit(error ? 1 : exitCode));
  } else {
    process.stdout.write(`${JSON.stringify(report)}\n`, () => process.exit(exitCode));
  }
};
