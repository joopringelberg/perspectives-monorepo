// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

const sessionKey = Symbol.for("perspectives.performance.session");
const allowedLabels = new Set(["decrypt", "executeTransaction", "incomingCascade", "publicStates"]);

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
