/* eslint-env node */
/* global URL */

import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import typescript from '@rollup/plugin-typescript';
import copy from 'rollup-plugin-copy';
import del from 'rollup-plugin-delete';
import replace from '@rollup/plugin-replace';
import fs from 'fs';
import path from 'path';
import { ensureBuildMeta } from '../mycontexts/src/buildMeta.js';

// Resolve paths to sibling package (mycontexts)
const mycontextsDir = path.resolve(path.dirname(new URL(import.meta.url).pathname), '../mycontexts');
const thepackage = JSON.parse(fs.readFileSync(path.join(mycontextsDir, 'package.json'), 'utf8'));
const { buildId } = await ensureBuildMeta();

export default {
  input: 'src/perspectives-proxy.ts',
  output: {
    file: 'dist/perspectives-proxy.js',
    format: 'es',
    sourcemap: true
  },
  external: [
    'perspectives-core'
  ],
  plugins: [
    del({ targets: 'dist/*' }),
    resolve(),
    commonjs(),
    // Run replacements before TypeScript so TS sees concrete literals
    replace({
      preventAssignment: true,
      values: {
        __MYCONTEXTS_VERSION__: JSON.stringify(thepackage.version),
        __BUILD_ID__: JSON.stringify(buildId),
      }
    }),
    typescript({
      tsconfig: './tsconfig.json', // Ensure Rollup uses the correct tsconfig file
      declaration: true,
      declarationDir: 'dist'
    }),
    copy({
      targets: [
        { src: 'src/perspectivesshape.d.ts', dest: 'dist' }
      ]
    })
  ]
};