// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Node.js-compatible stub for idb-keyval (https://www.npmjs.com/package/idb-keyval).
//
// This file is used by the Node.js build (rollup.node.config.js) and by the
// spago test runner.  It replaces the browser IndexedDB-backed implementation
// with a plain in-memory Map that is optionally persisted to a JSON file.
//
// The file path for persistence can be overridden by setting the environment
// variable PERSPECTIVES_KEYVAL_STORE.  Defaults to ./idb-keyval-store.json in
// the current working directory.  Set to "" to disable persistence (memory-only).

import { readFileSync, writeFileSync, existsSync } from 'fs';

const STORE_FILE = process.env.PERSPECTIVES_KEYVAL_STORE !== undefined
  ? process.env.PERSPECTIVES_KEYVAL_STORE
  : './idb-keyval-store.json';

function load() {
  if (STORE_FILE && existsSync(STORE_FILE)) {
    try {
      return new Map(Object.entries(JSON.parse(readFileSync(STORE_FILE, 'utf8'))));
    } catch (e) {
      // Non-fatal: a corrupted or empty store file is treated as a fresh store.
      // This can happen if the process was killed while writing.
      console.warn('[idb-keyval.node] Could not load store file, starting fresh:', e.message);
      return new Map();
    }
  }
  return new Map();
}

const store = load();

function save() {
  if (STORE_FILE) {
    try {
      writeFileSync(STORE_FILE, JSON.stringify(Object.fromEntries(store)), 'utf8');
    } catch (e) {
      // Non-fatal: if we cannot persist, continue in-memory.
      console.warn('[idb-keyval.node] Could not persist store:', e.message);
    }
  }
}

// These exports match the idb-keyval npm package API so that the compiled
// output/IDBKeyVal/foreign.js (which does `import * as idbKeyval from 'idb-keyval'`
// and re-exports idbKeyval.get / idbKeyval.set) works when the alias redirects
// 'idb-keyval' to this file.
export function get(key) {
  return Promise.resolve(store.has(key) ? store.get(key) : undefined);
}

export function set(key, value) {
  store.set(key, value);
  save();
  return Promise.resolve();
}

export function clear() {
  store.clear();
  save();
  return Promise.resolve();
}
