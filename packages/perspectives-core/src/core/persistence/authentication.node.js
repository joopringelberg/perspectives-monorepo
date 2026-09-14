// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

export function isUnauthorized (error)
{
  var unauthorizedRegex = new RegExp(/unauthorized/i);
  return (
    (error.message && null != error.message.match(unauthorizedRegex)) ||
    (error.error && null != error.error.match(unauthorizedRegex)) ||
    (error.name && null != error.name.match(unauthorizedRegex)) ||
    (error.status == 401)
  );
}

function hasSameAuthority(firstUrl, secondUrl)
{
  try
  {
    return new URL(firstUrl).origin === new URL(secondUrl).origin;
  }
  catch (_error)
  {
    return false;
  }
}

export function invalidateDatabaseConnectors (credentialUrl)
{
  return function (couchdbUrl)
  {
    return function (databases)
    {
      var result = {...databases};
      var credentialAppliesToConfiguredServer = couchdbUrl !== "" && hasSameAuthority(credentialUrl, couchdbUrl);

      Object.keys(result).forEach(function (databaseName)
      {
        var isEndpoint = /^https?/.test(databaseName);
        if ((isEndpoint && hasSameAuthority(credentialUrl, databaseName)) || (!isEndpoint && credentialAppliesToConfiguredServer))
        {
          delete result[databaseName];
        }
      });

      return result;
    };
  };
}