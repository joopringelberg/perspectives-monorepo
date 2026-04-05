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

// EffectFn1 String (Promise (Nullable Foreign))
// The idb-keyval `get` function is called as an EffectFn1 from PureScript.
// It must return an Effect that produces a Promise.
export const getValueByKeyImpl = function(key) {
  return function() {
    return Promise.resolve(store.has(key) ? store.get(key) : null);
  };
};

// EffectFn2 String Foreign Unit
// The idb-keyval `set` function is called as an EffectFn2 from PureScript.
// It must return an Effect that produces Unit.
export const setKeyValueImpl = function(key, value) {
  return function() {
    store.set(key, value);
    save();
  };
};

// Effect Unit
export function clear() {
  store.clear();
  save();
}
