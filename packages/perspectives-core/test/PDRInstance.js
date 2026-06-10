// Returns an Effect (Promise Unit) suitable for PureScript's Control.Promise.toAffE.
// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import { set as idbSet } from 'idb-keyval';

// Generates a fresh ECDSA P-384 keypair and stores both keys in the IDB stub
// under <guid>_privateKey and <guid>_publicKey.
//
// Generating fresh keys each test run guarantees that sign + verify use a
// matching pair, without any dependency on a stored keypair file.
export function loadKeypairImpl(guid) {
  return function () {
    return (async () => {
      const keyPair = await crypto.subtle.generateKey(
        { name: 'ECDSA', namedCurve: 'P-384' },
        true,
        ['sign', 'verify']
      );
      await idbSet(guid + '_privateKey', keyPair.privateKey);
      await idbSet(guid + '_publicKey', keyPair.publicKey);
    })();
  };
}