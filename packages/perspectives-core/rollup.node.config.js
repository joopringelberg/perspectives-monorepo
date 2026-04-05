// SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
// SPDX-License-Identifier: GPL-3.0-or-later
//
// Rollup configuration for the Node.js build of perspectives-core (PDR-N).
//
// This configuration produces dist/perspectives-core.node.js, which can be used:
//   - by the spago test runner (spago test --main Test.Main)
//   - by a Tauri sidecar or Electron main-process module
//   - by any Node.js host that wants to embed the PDR
//
// Key differences from rollup.config.js (the browser / PDR-B build):
//   1. persistenceAPI.js  → persistenceAPI.node.js  (pouchdb instead of pouchdb-browser)
//   2. idb-keyval.js      → idb-keyval.node.js      (file-backed Map stub)
//   3. preferBuiltins: true  (use Node.js built-ins as-is)
//   4. Output file: dist/perspectives-core.node.js
//
// To add this build to the normal workflow, add to package.json:
//   "build:node": "pnpm exec spago build && rollup -c rollup.node.config.js"
//
// AffJax note:
//   The PureScript source imports Affjax.Web.  For a real Node.js deployment that
//   needs HTTP, replace affjax-web with affjax-node in spago.yaml and change the
//   five source files that import "Affjax.Web as AJ" to "Affjax.Node as AJ".
//   For testing purposes (Layer 1 and 2 as described in nodejs-testing-architecture.md)
//   the HTTP layer is not exercised, so this is not required immediately.

import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import alias from '@rollup/plugin-alias';
import url from '@rollup/plugin-url';
import replace from '@rollup/plugin-replace';
import { promises as fs } from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default async function() {
  const packageJson = JSON.parse(await fs.readFile(new URL('./package.json', import.meta.url)));

  return {
    input: './output/Main/index.js',
    output: {
      file: './dist/perspectives-core.node.js',
      format: 'es',
      sourcemap: true,
      name: 'perspectivesCoreNode',
    },
    plugins: [
      alias({
        entries: [
          // Redirect the browser PouchDB FFI to the Node.js version.
          {
            find: /persistenceAPI\.js$/,
            replacement: path.join(__dirname, 'src/core/persistence/persistenceAPI.node.js'),
          },
          // Redirect the browser idb-keyval FFI to the Node.js file-backed stub.
          {
            find: /idb-keyval\.js$/,
            replacement: path.join(__dirname, 'src/core/idb-keyval.node.js'),
          },
        ],
      }),
      resolve({ preferBuiltins: true }),
      commonjs(),
      json(),
      url({
        include: ['**/*.arc'],
        limit: 0,
        fileName: '[name][extname]',
      }),
      replace({
        preventAssignment: true,
        __PDRVersion__: JSON.stringify(packageJson.version ? packageJson.version : 'no version'),
        __MYCONTEXTS__: JSON.stringify('https://mycontexts.com/'),
      }),
    ],
    // Keep eventsource external (same as the browser build).
    // In Node.js the eventsource package is available as a CommonJS module.
    external: ['eventsource'],
  };
}
