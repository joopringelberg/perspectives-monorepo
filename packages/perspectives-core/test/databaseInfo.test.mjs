// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import assert from "node:assert/strict";
import { test } from "node:test";
import { readDatabaseInfo } from "../src/core/persistence/databaseInfo.js";

test("returns local database info without HTTP", async () => {
  const info = { db_name: "local", doc_count: 3 };
  assert.equal(await readDatabaseInfo({ adapter: "memory", info: async () => info }), info);
});

for (const adapter of ["http", "https"])
{
  test(`returns successful ${adapter} database info`, async () => {
    const database = {
      adapter,
      fetch: async path => {
        assert.equal(path, "");
        return new globalThis.Response(JSON.stringify({ db_name: "remote", doc_count: 4 }));
      }
    };
    assert.deepEqual(await readDatabaseInfo(database), { db_name: "remote", doc_count: 4 });
  });
}

for (const [status, error] of [[401, "unauthorized"], [403, "forbidden"], [404, "not_found"], [500, "internal_server_error"]])
{
  test(`preserves HTTP ${status} instead of returning an invalid DatabaseInfo`, async () => {
    await assert.rejects(readDatabaseInfo({
      adapter: "http",
      fetch: async () => new globalThis.Response(JSON.stringify({ error, reason: "CouchDB rejected the request" }), { status })
    }), rejection => {
      assert.equal(rejection.status, status);
      assert.equal(rejection.name, error);
      assert.equal(rejection.message, "CouchDB rejected the request");
      return true;
    });
  });
}

test("propagates network errors unchanged", async () => {
  const error = new TypeError("Network failure");
  await assert.rejects(readDatabaseInfo({
    adapter: "http",
    fetch: async () => { throw error; }
  }), rejection => rejection === error);
});
