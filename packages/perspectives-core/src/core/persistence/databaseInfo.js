// SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

export async function readDatabaseInfo(database)
{
  if (database.adapter !== "http" && database.adapter !== "https")
  {
    return database.info();
  }

  // PouchDB's remote info() does not check the HTTP status before returning JSON.
  const response = await database.fetch("");
  const body = await response.json();
  if (!response.ok)
  {
    throw {
      status: response.status,
      name: body.error || "database_info",
      message: body.reason || response.statusText,
      error: body.error || true
    };
  }
  return body;
}

export function databaseInfoImpl(database)
{
  return function(onError, onSuccess)
  {
    readDatabaseInfo(database).then(onSuccess, err => onError(new Error(JSON.stringify({
      status: err.status,
      name: err.name || err.constructor.name,
      message: err.message || err.reason || "Database info request failed",
      error: err.error || err.message || true
    }))));
    return function(_cancelError, _cancelerError, cancelerSuccess)
    {
      cancelerSuccess();
    };
  };
}
