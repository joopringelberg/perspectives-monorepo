// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

export function statistics(values) {
  if (values.some(value => !Number.isFinite(value) || value < 0)) {
    throw new TypeError("Timings must be finite, non-negative numbers");
  }
  if (!values.length) return { count: 0, min: null, median: null, mean: null, p95: null, max: null, standardDeviation: null };
  const sorted = [...values].sort((a, b) => a - b);
  const mean = sorted.reduce((sum, value) => sum + value, 0) / sorted.length;
  const middle = Math.floor(sorted.length / 2);
  return {
    count: sorted.length,
    min: sorted[0],
    median: sorted.length % 2 ? sorted[middle] : (sorted[middle - 1] + sorted[middle]) / 2,
    mean,
    p95: sorted[Math.ceil(sorted.length * 0.95) - 1],
    max: sorted.at(-1),
    standardDeviation: Math.sqrt(sorted.reduce((sum, value) => sum + (value - mean) ** 2, 0) / sorted.length)
  };
}

export function aggregate(rounds) {
  const trials = rounds.filter(round => !round.warmup).flatMap(round => round.trials);
  return [...new Set(trials.map(trial => trial.scenario))].map(scenario => {
    const selected = trials.filter(trial => trial.scenario === scenario);
    const timings = Object.fromEntries(["senderActionMs", "bobCompletionMs", "endToEndMs"].map(field =>
      [field, statistics(selected.map(trial => trial[field]).filter(value => value !== null))]));
    return {
      scenario,
      trials: selected.length,
      succeeded: selected.filter(trial => trial.succeeded).length,
      failed: selected.filter(trial => !trial.succeeded).length,
      notRun: selected.filter(trial => trial.status === "not-run").length,
      timings
    };
  });
}

export function normalizeTrials(expectedScenarios, trials) {
  if (new Set(expectedScenarios).size !== expectedScenarios.length ||
      new Set(trials.map(trial => trial.scenario)).size !== trials.length ||
      trials.some(trial => !expectedScenarios.includes(trial.scenario))) {
    throw new Error("Worker returned inconsistent scenarios");
  }
  return expectedScenarios.map(scenario => trials.find(trial => trial.scenario === scenario) ?? {
    scenario, status: "not-run", succeeded: false,
    senderActionMs: null, bobCompletionMs: null, endToEndMs: null,
    senderCompleted: false, completionObserved: false, profile: null
  });
}

export function normalizeRounds(rounds) {
  const expectedScenarios = [...new Set(rounds.flatMap(round => round.trials.map(trial => trial.scenario)))];
  return {
    expectedScenarios,
    unknownScenarioFailureCount: expectedScenarios.length ? 0 : rounds.filter(round => !round.succeeded).length,
    rounds: expectedScenarios.length
      ? rounds.map(round => ({ ...round, trials: normalizeTrials(expectedScenarios, round.trials) }))
      : rounds
  };
}
