// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import assert from "node:assert/strict";
import { test } from "node:test";
import * as history from "../output/Perspectives.Sync.PropertyHistory/index.js";
import { Just, Nothing } from "../output/Data.Maybe/index.js";
import { Left, Right } from "../output/Data.Either/index.js";
import { perspectivesUsersPublicKey } from "../output/Perspectives.ModelDependencies/index.js";

const key = "role#model://test.example#History$Value";

function delta(version, operation = "SetProperty", values = [String(version)], author = "author") {
  return {
    author,
    signature: new Just("original-signature"),
    encryptedDelta: JSON.stringify({
      deltaFormatVersion: 2, resourceKey: key, resourceVersion: version,
      deltaType: operation, id: "role", roleType: "model://test.example#History$Role@0.0",
      property: "model://test.example#History$Value@0.0",
      subject: "model://test.example#History$Writer@0.0", subjectKind: "ENR", values
    })
  };
}

function record(signed, applied = true) {
  const payload = JSON.parse(signed.encryptedDelta);
  return {
    _id: `${payload.resourceVersion}-${signed.author}-${payload.historyCheckpoint ? "checkpoint" : "mutation"}`,
    _rev: Nothing.value, resourceKey: key, resourceVersion: payload.resourceVersion,
    author: signed.author, signedDelta: signed, deltaType: payload.deltaType,
    applied, deltaId: Nothing.value, disposition: Nothing.value, contextKey: Nothing.value
  };
}

function right(result) {
  assert.ok(result instanceof Right, result instanceof Left ? result.value0 : "Expected Right");
  return result.value0;
}

function compact(records) {
  const result = right(history.compactPropertyHistory(records));
  if (!(result instanceof Just)) return records;
  const { json, version } = result.value0;
  const baseline = record({ author: "zz-discloser", signature: new Just("disclosure-signature"), encryptedDelta: json });
  return [baseline, ...records.filter(entry => entry.resourceVersion > version)];
}

test("1,088 successive property changes retain a baseline plus exactly ten recent versions", () => {
  let retained = [];
  for (let version = 1; version <= 1088; version++) {
    retained = compact([...retained, record(delta(version))]);
    assert.ok(retained.length <= 11);
  }
  assert.equal(retained.length, 11);
  assert.deepEqual(retained.map(entry => entry.resourceVersion), Array.from({ length: 11 }, (_, i) => 1078 + i));
  assert.deepEqual(right(history.propertyHistory(retained)).values, ["1088"]);
  const marker = right(history.readCheckpoint(retained[0].signedDelta.encryptedDelta)).value0;
  assert.equal(marker.winner.signature.value0, "original-signature");
  right(history.validateCheckpoint(retained[0].signedDelta));
});

test("numeric versions and relative additions, removals and deletion reconstruct without obsolete values", () => {
  let retained = [];
  for (let version = 1; version <= 30; version++) {
    const operation = version === 20 ? "DeleteProperty" : version === 30 ? "RemoveProperty" : "AddProperty";
    retained = compact([...retained, record(delta(version, operation, [version === 30 ? "21" : String(version)]))]);
  }
  assert.deepEqual(right(history.propertyHistory(retained)).values, Array.from({ length: 8 }, (_, i) => String(i + 22)));
});

test("a boundary conflict compares the original winner, not the discloser, and removes the losing effect", () => {
  let retained = [];
  for (let version = 1; version <= 11; version++) {
    retained = compact([...retained, record(delta(version, "AddProperty", [String(version)], "a"))]);
  }
  retained.push(record(delta(1, "AddProperty", ["replacement"], "m")));
  const values = right(history.propertyHistory(retained)).values;
  assert.ok(!values.includes("1"));
  assert.deepEqual(values, ["replacement", ...Array.from({ length: 10 }, (_, i) => String(i + 2))]);
});

test("an interrupted pruning pass preserves the newest baseline and does not compact backwards", () => {
  const original = Array.from({ length: 12 }, (_, i) => record(delta(i + 1)));
  const retained = compact(original);
  const interrupted = [original[0], ...retained];
  assert.ok(right(history.compactPropertyHistory(interrupted)) instanceof Nothing);
  assert.deepEqual(right(history.propertyHistory(interrupted)).values, ["12"]);
});

test("invalid or nested baselines and substituted identity-key or resource metadata are rejected", () => {
  const valid = compact(Array.from({ length: 11 }, (_, i) => record(delta(i + 1))))[0].signedDelta;
  const payload = JSON.parse(valid.encryptedDelta);
  const identityWinner = JSON.parse(payload.historyCheckpoint.winner.encryptedDelta);
  const invalid = [
    { ...payload, deltaFormatVersion: 2 },
    { ...payload, resourceKey: "another-resource" },
    { ...payload, resourceVersion: 99 },
    { ...payload, roleType: "another-role" },
    {
      ...payload, property: perspectivesUsersPublicKey,
      historyCheckpoint: {
        ...payload.historyCheckpoint,
        winner: {
          ...payload.historyCheckpoint.winner,
          encryptedDelta: JSON.stringify({ ...identityWinner, property: perspectivesUsersPublicKey })
        }
      }
    },
    {
      ...payload,
      historyCheckpoint: {
        ...payload.historyCheckpoint,
        winner: { ...valid, signature: valid.signature.value0 }
      }
    }
  ];
  for (const body of invalid) {
    assert.ok(history.validateCheckpoint({ ...valid, encryptedDelta: JSON.stringify(body) }) instanceof Left);
  }
});
