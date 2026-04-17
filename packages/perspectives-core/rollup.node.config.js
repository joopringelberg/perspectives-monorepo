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
//   3. Affjax.Web/index.js → Affjax.Node/index.js   (xhr2-backed HTTP instead of XHR browser)
//   4. preferBuiltins: true  (use Node.js built-ins as-is)
//   5. Output file: dist/perspectives-core.node.js
//
// Prerequisites (run once after adding these to spago.yaml / package.json):
//   pnpm install                   # installs xhr2 (required by affjax-node FFI)
//   pnpm exec spago build          # compiles both affjax-web and affjax-node to output/
//   rollup -c rollup.node.config.js
//
// AffJax implementation note:
//   The five PureScript source files that import "Affjax.Web as AJ" do NOT need to
//   change.  This config aliases the compiled output/Affjax.Web/index.js module to
//   output/Affjax.Node/index.js at bundle time.  The Affjax.Node module is a drop-in
//   replacement: it provides the same request/printError/Request/Response/Error
//   surface but uses the xhr2 npm package instead of the browser XHR API.
//   Both affjax-node and affjax-web are declared as spago dependencies so that spago
//   compiles both; only one is included in each Rollup output.

import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import alias from '@rollup/plugin-alias';
import url from '@rollup/plugin-url';
import replace from '@rollup/plugin-replace';
import sourcemaps from 'rollup-plugin-sourcemaps';
import { promises as fs, readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import path from 'path';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Allow overriding input/output via environment variables so this config can
// also be used to bundle the test entry point:
//   ROLLUP_INPUT=./output/Test.Main/index.js \
//   ROLLUP_OUTPUT=./dist/test.node.js \
//   rollup -c rollup.node.config.js
const inputFile  = process.env.ROLLUP_INPUT  ?? './output/Main/index.js';
const outputFile = process.env.ROLLUP_OUTPUT ?? './dist/perspectives-core.node.js';
// When building the test bundle, PureScript only *exports* main() — it does not
// call it.  Appending `main();` via outro ensures Node.js actually runs the tests
// when the bundle is loaded with `node dist/test.node.js`.
const isTestBuild = !!process.env.ROLLUP_INPUT;

export default async function() {
  const packageJson = JSON.parse(await fs.readFile(new URL('./package.json', import.meta.url)));

  return {
    input: inputFile,
    output: {
      file: outputFile,
      format: 'es',
      sourcemap: true,
      name: 'perspectivesCoreNode',
      outro: isTestBuild ? 'main();' : '',
    },
    plugins: [
      // Read existing source maps from spago-compiled output/**/*.js files and attach
      // them to the module before any other plugin processes it.  Without this, Rollup
      // stops the source-map chain at the intermediate .js file and PureScript (.purs)
      // breakpoints cannot be resolved in the VS Code debugger.
      sourcemaps(),
      alias({
        entries: [
          // Redirect pouchdb-browser to pouchdb-core (memory adapter, no native LevelDB).
          // The compiled output/Perspectives.Persistence.API/foreign.js imports
          // `pouchdb-browser` as a bare specifier; pouchdb-core is a drop-in that
          // exposes the same PouchDB constructor and is plugin-extended in
          // persistenceAPI.node.js with the memory + http adapters.
          {
            find: 'pouchdb-browser',
            replacement: 'pouchdb-core',
          },
          // Redirect the browser idb-keyval FFI to the Node.js file-backed stub.
          // The FFI is copied to output/*/foreign.js by spago, but the idb-keyval
          // import inside it is a bare npm specifier that we can intercept here.
          {
            find: 'idb-keyval',
            replacement: path.join(__dirname, 'src/core/idb-keyval.node.js'),
          },
          // Redirect affjax-web compiled output to affjax-node compiled output.
          // The five PureScript files that import Affjax.Web as AJ do not need to change;
          // Rollup transparently substitutes Affjax.Node (xhr2-backed) at bundle time.
          // The Affjax.Node module provides the same API surface: request, printError,
          // Request, Response, Error.
          {
            find: /^.*[/\\]Affjax\.Web[/\\]index\.js$/,
            replacement: path.join(__dirname, 'output/Affjax.Node/index.js'),
          },
        ],
      }),
      // Redirect output/Perspectives.Persistence.API/foreign.js to the Node.js-compatible
      // persistence module so that pouchdb-adapter-memory and pouchdb-adapter-http are
      // registered.  Two hooks provide belt-and-suspenders reliability:
      //   resolveId – changes the module ID so source maps point to persistenceAPI.node.js
      //   load      – fallback that intercepts by absolute path if resolveId is bypassed
      {
        name: 'persistence-api-node',
        resolveId(source, importer) {
          if (
            source === './foreign.js' &&
            importer &&
            importer.includes('Perspectives.Persistence.API')
          ) {
            return path.join(__dirname, 'src/core/persistence/persistenceAPI.node.js');
          }
          return null;
        },
        load(id) {
          if (id.includes('Perspectives.Persistence.API') && id.endsWith('foreign.js')) {
            return readFileSync(
              path.join(__dirname, 'src/core/persistence/persistenceAPI.node.js'),
              'utf8'
            );
          }
          return null;
        },
      },
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
    external: ['eventsource'],
  };
}
