// Returns an Effect (Promise Unit) suitable for PureScript's Control.Promise.toAffE.
// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later

import { get as idbGet, set as idbSet } from 'idb-keyval';
import { existsSync, mkdirSync } from 'fs';
import { writeFile, readFile } from 'fs/promises';
import PouchDB from 'pouchdb-core';
import PouchDBMemoryAdapter from 'pouchdb-adapter-memory';

// Ensure the memory adapter is registered (idempotent — safe to call multiple times).
PouchDB.plugin(PouchDBMemoryAdapter);

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

// ---------------------------------------------------------------------------
// PDR SNAPSHOT / RESTORE
// ---------------------------------------------------------------------------

// Strips revision metadata that would cause conflicts when inserting into a
// fresh PouchDB database. Mirrors the copyableDoc helper in persistenceAPI.node.js.
function stripRevision(doc) {
  const { _rev, _revisions, _conflicts, _deleted_conflicts, _local_seq, ...rest } = doc;
  return rest;
}

// Snapshot the four in-memory PouchDB databases belonging to a PDR instance
// and the user's ECDSA keypair to `snapshotDir`.
//
// Files written:
//   <snapshotDir>/<systemIdentifier>_entities.json
//   <snapshotDir>/<systemIdentifier>_post.json
//   <snapshotDir>/<systemIdentifier>_models.json
//   <snapshotDir>/<systemIdentifier>_invertedqueries.json
//   <snapshotDir>/keypair.json
//   <snapshotDir>/snapshot.complete   (written last as an atomic "done" marker)
//
// PureScript type: String -> String -> String -> Effect (Promise Unit)
//   args: systemIdentifier, perspectivesUser, snapshotDir
export function snapshotPDRImpl(systemIdentifier) {
  return function (perspectivesUser) {
    return function (snapshotDir) {
      return function () {
        return (async () => {
          mkdirSync(snapshotDir, { recursive: true });

          // Dump each database.
          const suffixes = ['_entities', '_post', '_models', '_invertedqueries'];
          for (const suffix of suffixes) {
            const dbName = systemIdentifier + suffix;
            const db = new PouchDB(dbName, { adapter: 'memory' });
            const result = await db.allDocs({ include_docs: true, attachments: true, binary: false });
            const docs = result.rows
              .map(row => row.doc)
              .filter(doc => doc && !doc._deleted);
            await writeFile(
              `${snapshotDir}/${dbName}.json`,
              JSON.stringify(docs, null, 2),
              'utf8'
            );
          }

          // Export the ECDSA keypair as JWK so it survives process restarts.
          const privateKey = await idbGet(perspectivesUser + '_privateKey');
          const publicKey = await idbGet(perspectivesUser + '_publicKey');
          if (privateKey && publicKey) {
            const privateJwk = await crypto.subtle.exportKey('jwk', privateKey);
            const publicJwk = await crypto.subtle.exportKey('jwk', publicKey);
            await writeFile(
              `${snapshotDir}/keypair.json`,
              JSON.stringify({ privateJwk, publicJwk }, null, 2),
              'utf8'
            );
            // Write sentinel last so that a partial snapshot is never treated as complete.
            // Only written when the keypair was also saved successfully.
            await writeFile(`${snapshotDir}/snapshot.complete`, 'done', 'utf8');
          } else {
            console.warn('[snapshotPDR] Keypair not found in idb-keyval store; snapshot not saved.');
          }
        })();
      };
    };
  };
}

// Restore the four in-memory PouchDB databases and the ECDSA keypair from a
// snapshot previously created by snapshotPDRImpl.
//
// The databases must NOT have been written to between process start and this
// call. `bulkDocs` with stripped `_rev` values inserts every document as a
// fresh revision-1 entry.  Design documents are restored and their view
// indexes are rebuilt automatically by PouchDB on first query.
//
// PureScript type: String -> String -> String -> Effect (Promise Unit)
//   args: systemIdentifier, perspectivesUser, snapshotDir
export function restoreSnapshotImpl(systemIdentifier) {
  return function (perspectivesUser) {
    return function (snapshotDir) {
      return function () {
        return (async () => {
          // Restore databases.
          const suffixes = ['_entities', '_post', '_models', '_invertedqueries'];
          for (const suffix of suffixes) {
            const dbName = systemIdentifier + suffix;
            const db = new PouchDB(dbName, { adapter: 'memory' });
            const docsJson = await readFile(`${snapshotDir}/${dbName}.json`, 'utf8');
            const docs = JSON.parse(docsJson);
            if (docs.length > 0) {
              // Strip revision fields: inserting without _rev creates fresh rev-1 entries.
              const cleanDocs = docs.map(stripRevision);
              await db.bulkDocs(cleanDocs);
            }
          }

          // Restore keypair.
          const keypairJson = JSON.parse(
            await readFile(`${snapshotDir}/keypair.json`, 'utf8')
          );
          const { privateJwk, publicJwk } = keypairJson;
          const privateKey = await crypto.subtle.importKey(
            'jwk', privateJwk,
            { name: 'ECDSA', namedCurve: 'P-384' },
            true, ['sign']
          );
          const publicKey = await crypto.subtle.importKey(
            'jwk', publicJwk,
            { name: 'ECDSA', namedCurve: 'P-384' },
            true, ['verify']
          );
          await idbSet(perspectivesUser + '_privateKey', privateKey);
          await idbSet(perspectivesUser + '_publicKey', publicKey);
        })();
      };
    };
  };
}

// Returns true iff a completed snapshot exists at `snapshotDir`.
// Checks for the sentinel file written last by snapshotPDRImpl.
//
// PureScript type: String -> Effect Boolean
export function snapshotExistsImpl(snapshotDir) {
  return function () {
    return existsSync(`${snapshotDir}/snapshot.complete`);
  };
}