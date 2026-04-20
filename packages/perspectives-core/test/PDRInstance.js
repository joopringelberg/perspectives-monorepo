// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import { set as idbSet } from 'idb-keyval';
import { readFileSync } from 'fs';

// Reads the keypair from accounts/orn2j1nh3q_test3_keypair.json, imports both keys
// via SubtleCrypto (ECDSA P-384), and stores them in the IDB stub under
// <guid>_privateKey and <guid>_publicKey.
//
// The file content is URL-encoded JSON; the keys match the convention used by
// authenticate.purs: takeGuid(perspectivesUser) + "_privateKey" / "_publicKey".
//
// Returns an Effect (Promise Unit) suitable for PureScript's Control.Promise.toAffE.
export function loadKeypairImpl(guid) {
  return function () {
    return (async () => {
      const raw = readFileSync('accounts/orn2j1nh3q_test3_keypair.json', 'utf8');
      const { privateKey: privateJwk, publicKey: publicJwk } = JSON.parse(decodeURIComponent(raw.trim()));

      const privateKey = await crypto.subtle.importKey(
        'jwk',
        privateJwk,
        { name: 'ECDSA', namedCurve: 'P-384' },
        true,
        ['sign']
      );

      const publicKey = await crypto.subtle.importKey(
        'jwk',
        publicJwk,
        { name: 'ECDSA', namedCurve: 'P-384' },
        true,
        ['verify']
      );

      await idbSet(guid + '_privateKey', privateKey);
      await idbSet(guid + '_publicKey', publicKey);
    })();
  };
}
