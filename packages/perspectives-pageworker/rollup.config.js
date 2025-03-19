// filepath: /Users/joopringelberg/Code/perspectives-monorepo/packages/perspectives-pageworker/rollup.config.js
import babel from '@rollup/plugin-babel';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import json from '@rollup/plugin-json';
import cleaner from 'rollup-plugin-cleaner';
import copy from 'rollup-plugin-copy';

export default {
  input: 'src/perspectives-pageworker.js',
  output: {
    file: 'dist/perspectives-pageworker.js',
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
    }),
    copy({
      targets: [
        { src: 'src/perspectives-pageworker.d.ts', dest: 'dist' }
      ]
    })
  ],
  watch: {
    include: 'src/**'
  }
};