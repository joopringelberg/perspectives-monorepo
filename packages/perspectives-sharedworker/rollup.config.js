// filepath: /Users/joopringelberg/Code/perspectives-monorepo/packages/perspectives-sharedworker/rollup.config.js
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import babel from '@rollup/plugin-babel';
import del from 'rollup-plugin-delete';

export default {
  input: 'src/perspectives-sharedworker.js',
  output: {
    file: 'dist/perspectives-sharedworker.js',
    format: 'es',
    name: 'PerspectivesSharedWorker',
    sourcemap: true
  },
  plugins: [
    del({ targets: 'dist/*' }),
    resolve(),
    commonjs(),
    json(),
    babel({
      babelHelpers: 'bundled',
      presets: ['@babel/preset-env']
    })
  ]
};