// filepath: /Users/joopringelberg/Code/perspectives-monorepo/packages/perspectives-sharedworker/rollup.config.js
import babel from '@rollup/plugin-babel';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import cleaner from 'rollup-plugin-cleaner';

export default {
  input: 'src/perspectives-sharedworker.js',
  output: {
    file: 'dist/perspectives-sharedworker.js',
    format: 'es',
    sourcemap: true
  },
  plugins: [
    cleaner({
      targets: ['./dist/']
    }),
    resolve(),
    commonjs(),
    json(),
    babel({
      babelHelpers: 'bundled',
      exclude: 'node_modules/**',
      presets: ['@babel/preset-env']
    })
  ],
  watch: {
    include: 'src/**'
  }
};