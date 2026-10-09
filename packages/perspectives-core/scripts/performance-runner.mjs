// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import { fork } from "node:child_process";
import { writeFile } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { aggregate, normalizeRounds, normalizeTrials } from "./performance-statistics.mjs";

const options = { mode: "memory", warmup: 1, repetitions: 5, timeoutMs: 300000, profile: false, output: "performance-report.json" };
const names = { "--mode": "mode", "--warmup": "warmup", "--repetitions": "repetitions", "--timeout-ms": "timeoutMs", "--output": "output" };
try {
  const args = process.argv.slice(2);
  while (args.length) {
    const flag = args.shift();
    if (flag === "--profile") { options.profile = true; continue; }
    const name = names[flag];
    if (!name || !args.length) throw new Error(`Invalid option: ${flag}`);
    options[name] = ["warmup", "repetitions", "timeoutMs"].includes(name) ? Number(args.shift()) : args.shift();
  }
  if (!["memory", "amqp"].includes(options.mode) ||
      !Number.isSafeInteger(options.warmup) || options.warmup < 0 ||
      !Number.isSafeInteger(options.repetitions) || options.repetitions < 1 ||
      !Number.isSafeInteger(options.timeoutMs) || options.timeoutMs < 1) {
    throw new Error("Invalid mode or trial counts");
  }
} catch (error) {
  console.error(error.message);
  process.exit(2);
}

function runRound(index, warmup) {
  return new Promise(resolve => {
    let result;
    let expectedScenarios = [];
    const completedTrials = [];
    let activeScenario;
    let status = "worker-failure";
    let settled = false;
    const child = fork(fileURLToPath(new URL("../dist/test.performance.node.js", import.meta.url)), [], {
      env: { ...process.env, PDR_PERF_MODE: options.mode, PDR_PERF_PROFILE: options.profile ? "1" : "0" },
      // Runtime logs are separate from the machine-readable JSON report.
      stdio: ["ignore", "ignore", "inherit", "ipc"]
    });
    const finish = (exitCode, signal) => {
      if (settled) return;
      settled = true;
      clearTimeout(timer);
      let trials = [];
      try {
        const receivedTrials = result?.trials ?? completedTrials;
        trials = normalizeTrials(expectedScenarios, receivedTrials);
        if (activeScenario && !receivedTrials.some(trial => trial.scenario === activeScenario)) {
          trials = trials.map(trial => trial.scenario === activeScenario
            ? { ...trial, status: "aborted" }
            : trial);
        }
      } catch {
        status = "invalid-worker-report";
      }
      const succeeded = status !== "timeout" && status !== "invalid-worker-report" &&
        exitCode === 0 && result?.suiteCompleted === true &&
        expectedScenarios.length > 0 && trials.length === expectedScenarios.length &&
        trials.every(trial => trial.succeeded);
      resolve({ index, warmup, status: succeeded ? "success" : status, failureReason: result?.failureReason ?? null, succeeded, exitCode, signal, trials });
    };
    const timer = setTimeout(() => {
      status = "timeout";
      child.kill("SIGKILL");
    }, options.timeoutMs);
    child.on("message", message => {
      if (message?.type === "perspectives-performance-manifest") expectedScenarios = message.expectedScenarios;
      if (message?.type === "perspectives-performance-trial-started") activeScenario = message.scenario;
      if (message?.type === "perspectives-performance-trial") {
        completedTrials.push(message.trial);
        activeScenario = undefined;
      }
      if (message?.type === "perspectives-performance") {
        result = message;
        expectedScenarios = message.expectedScenarios;
        status = message.failureReason ?? (message.suiteCompleted ? "semantic-failure" : "suite-failure");
      }
    });
    child.on("error", () => finish(null, null));
    child.on("exit", finish);
  });
}

const rounds = [];
for (let index = 0; index < options.warmup + options.repetitions; index++) {
  console.error(`Performance ${options.mode}: round ${index + 1}/${options.warmup + options.repetitions}`);
  rounds.push(await runRound(index, index < options.warmup));
}
const normalized = normalizeRounds(rounds);
const report = {
  schemaVersion: 1,
  suite: "Destructive synchronisation tests",
  generatedAt: new Date().toISOString(),
  mode: options.mode,
  warmup: options.warmup,
  repetitions: options.repetitions,
  profiling: options.profile,
  profileBoundary: "bob-result-observation-or-trial-failure-not-receiver-quiescence",
  profileParticipants: "combined-incoming-work-in-both-pdrs",
  profileSpanTotals: "completed-spans-only-unfinished-counted-separately",
  encryptedByteMetric: "utf8-ciphertext-string-not-total-wire-bytes",
  timeoutMs: options.timeoutMs,
  clock: "performance.now",
  units: "milliseconds",
  isolation: "fresh-process-per-round",
  repetitionUnit: "full-suite",
  warmupScope: "external-and-filesystem-caches-only",
  withinRoundCachePolicy: "shared-by-ordered-scenarios",
  pollingIntervalMs: 100,
  pollingMaxAttempts: 100,
  nodeVersion: process.version,
  succeeded: rounds.every(round => round.succeeded),
  expectedScenarios: normalized.expectedScenarios,
  unknownScenarioFailureCount: normalized.unknownScenarioFailureCount,
  rounds: normalized.rounds,
  scenarios: aggregate(normalized.rounds)
};
const json = `${JSON.stringify(report, null, 2)}\n`;
await writeFile(options.output, json, "utf8");
process.stdout.write(json);
process.exitCode = report.succeeded ? 0 : 1;
