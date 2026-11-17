// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

const sessionKey = Symbol.for("perspectives.performance.session");
const allowedLabels = new Set(["decrypt", "executeTransaction", "incomingCascade", "publicStates"]);

let nextTimingSpan = 0;

export const timingEnabled = () => globalThis.pdrTraceTimings === true;

function emitTiming(event, stage, id, monotonicMs, details = {}) {
  console.info("[PDR TIMING] " + JSON.stringify({
    event, stage, id, at: new Date().toISOString(), monotonicMs, ...details
  }));
}

export function traceTimingImpl(stage, id) {
  if (timingEnabled()) emitTiming("event", stage, id, globalThis.performance.now());
}

export const startTiming = stage => id => () => {
  if (!timingEnabled()) return null;
  const token = { stage, id, span: ++nextTimingSpan, start: globalThis.performance.now(), finished: false };
  emitTiming("start", stage, id, token.start, { span: token.span });
  return token;
};

export const finishTiming = token => succeeded => () => {
  if (!token || token.finished) return;
  token.finished = true;
  if (!timingEnabled()) return;
  const end = globalThis.performance.now();
  emitTiming("end", token.stage, token.id, end, {
    span: token.span, elapsedMs: end - token.start, succeeded
  });
};

let normalisationMeasurements = new Map();
let normalisationPhase = "unlabelled";

export const normalisationEnabled = () => globalThis.pdrMeasureNormalisation === true;

export const resetNormalisationMeasurements = () => {
  if (!normalisationEnabled()) return;
  normalisationMeasurements = new Map();
  normalisationPhase = "unlabelled";
};

export const setNormalisationPhase = phase => () => {
  if (normalisationEnabled()) normalisationPhase = phase;
};

function normalisationStats(roles, role) {
  let stats = roles.get(role);
  if (!stats) {
    stats = {
      requests: 0, succeeded: 0, failed: 0, unfinished: 0,
      elapsedMs: 0, repeatRequests: 0, repeatedElapsedMs: 0
    };
    roles.set(role, stats);
  }
  const repeated = stats.succeeded > 0;
  stats.requests++;
  stats.unfinished++;
  stats.repeatRequests += repeated ? 1 : 0;
  return { stats, repeated };
}

export const startNormalisation = pdr => role => compiling => () => {
  if (!normalisationEnabled()) return null;
  let contexts = normalisationMeasurements.get(pdr);
  if (!contexts) normalisationMeasurements.set(pdr, contexts = new Map());
  const context = compiling ? "compilation" : "runtime";
  let bucket = contexts.get(context);
  if (!bucket) contexts.set(context, bucket = { roles: new Map(), phases: new Map() });
  let phaseRoles = bucket.phases.get(normalisationPhase);
  if (!phaseRoles) bucket.phases.set(normalisationPhase, phaseRoles = new Map());
  return {
    entries: [normalisationStats(bucket.roles, role), normalisationStats(phaseRoles, role)],
    start: globalThis.performance.now(),
    finished: false
  };
};

export const finishNormalisation = token => succeeded => () => {
  if (!token || token.finished) return;
  token.finished = true;
  if (!normalisationEnabled()) return;
  const elapsedMs = globalThis.performance.now() - token.start;
  for (const { stats, repeated } of token.entries) {
    stats.unfinished--;
    stats.succeeded += succeeded ? 1 : 0;
    stats.failed += succeeded ? 0 : 1;
    stats.elapsedMs += elapsedMs;
    if (repeated && succeeded) stats.repeatedElapsedMs += elapsedMs;
  }
};

function summariseNormalisations(roles) {
  const summary = {
    requests: 0, distinctRoles: roles.size, succeeded: 0, failed: 0, unfinished: 0,
    elapsedMs: 0, repeatRequests: 0, repeatedElapsedMs: 0
  };
  for (const stats of roles.values()) {
    for (const key of Object.keys(stats)) summary[key] += stats[key];
  }
  return summary;
}

export const reportNormalisationMeasurements = () => {
  if (!normalisationEnabled()) return;
  for (const [pdr, contexts] of normalisationMeasurements) {
    for (const [context, bucket] of contexts) {
      console.info("[PDR NORMALISATION] " + JSON.stringify({
        pdr, context, ...summariseNormalisations(bucket.roles),
        phases: Array.from(bucket.phases, ([phase, roles]) =>
          ({ phase, ...summariseNormalisations(roles) })),
        topRoles: Array.from(bucket.roles, ([role, stats]) => ({ role, ...stats }))
          .sort((left, right) => right.repeatedElapsedMs - left.repeatedElapsedMs)
          .slice(0, 10)
      }));
    }
  }
};

export const profilingEnabled = () => Boolean(globalThis[sessionKey]?.active);
export const captureSession = () => globalThis[sessionKey]?.active ? globalThis[sessionKey] : null;
export const sessionEnabled = session => () => Boolean(session?.active);

export const startProfileInSession = session => label => () => {
  if (!session?.active || !allowedLabels.has(label)) return null;
  const span = session.spans[label] ??= { count: 0, completed: 0, failed: 0, unfinished: 0, totalMs: 0 };
  span.count++;
  span.unfinished++;
  return { session, label, start: performance.now(), finished: false };
};

export const startProfile = label => () => startProfileInSession(captureSession())(label)();

export const finishProfile = token => succeeded => () => {
  if (!token || token.finished || !token.session.active) return;
  token.finished = true;
  const span = token.session.spans[token.label];
  span.completed++;
  span.unfinished--;
  span.failed += succeeded ? 0 : 1;
  span.totalMs += performance.now() - token.start;
};

export const countEncrypted = session => ciphertext => wrappedKeys => () => {
  if (!session?.active) return;
  session.counts.messages++;
  session.counts.wrappedKeys += wrappedKeys;
  session.counts.encryptedUtf8Bytes += new TextEncoder().encode(ciphertext).byteLength;
};

export const countDecrypted = session => payload => deltas => publicKeys => () => {
  if (!session?.active) return;
  session.counts.deltas += deltas;
  session.counts.publicKeys += publicKeys;
  session.counts.decryptedUtf8Bytes += new TextEncoder().encode(payload).byteLength;
};
