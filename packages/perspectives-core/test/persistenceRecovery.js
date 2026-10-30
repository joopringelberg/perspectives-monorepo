// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

export function recoveringDatabase()
{
  let writable = false;
  return {
    database: {
      adapter: "memory",
      info: async () => ({ db_name: "stub", doc_count: 0 }),
      put(doc, _options, callback)
      {
        if (writable)
        {
          callback(null, { ok: true, id: doc._id, rev: "1-saved" });
        }
        else
        {
          callback({ status: 503, name: "unavailable", message: "Simulated write failure", error: true });
        }
      }
    },
    allowWrites: () => { writable = true; }
  };
}
