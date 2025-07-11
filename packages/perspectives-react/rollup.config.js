import typescript from '@rollup/plugin-typescript';
import resolve from '@rollup/plugin-node-resolve';
import commonjs from '@rollup/plugin-commonjs';
import replace from '@rollup/plugin-replace';
import postcss from 'rollup-plugin-postcss';
import json from '@rollup/plugin-json';
import del from 'rollup-plugin-delete';
import dts from 'rollup-plugin-dts';
import copy from 'rollup-plugin-copy';
import { visualizer } from 'rollup-plugin-visualizer';

export default [
  {
    input: 'src/perspectives-react-components.ts', // Update this to your entry file
    output: {
      dir: 'dist',
      format: 'es',
      sourcemap: true,
      entryFileNames: '[name].js',
      chunkFileNames: '[name]-[hash].js',
      assetFileNames: '[name]-[hash][extname]'
    },
    plugins: [
      // 1. Cleanup first
      del({ targets: 'dist/*' }),

      // 4. Code replacements
      replace({
        preventAssignment: true,
        values: {
          __PPSTORAGEURL__: JSON.stringify('https://mycontexts.com/ppsfs/uploadfile'),
          __PPSTORAGELIMIT__: '10',
          __PPWARNINGLEVEL__: '5',
          __MyContextsContainer__: JSON.stringify('root')
        }
      }),

      // 2. Resolution plugins
      resolve(),

      // 3. Transform plugins
      commonjs({
        requireReturnsDefault: 'preferred',
        transformMixedEsModules: true,
      }),

      // 5. Typescript compilation
      typescript({
        tsconfig: './tsconfig.json',
        declaration: true,
        declarationDir: 'dist/types',
        rootDir: 'src'
      }),

      // 6. Asset processing
      postcss({
        extract: false,
        minimize: true,
        sourceMap: true
      }),
      json(),
      
      // 7. Final file operations
      copy({
        targets: [
          { src: 'src/roledata.d.ts', dest: 'dist/types' },
          { src: 'src/components.css', dest: 'dist/types' },
          { src: 'src/highlight.css', dest: 'dist/types', rename: 'highlight.css' }
        ]
      }),
      // visualizer({
      //   filename: 'bundle-analysis.html',
      //   open: true
      // })  
    ],
    external: [
      'react',
      'react-dom',
      'react-bootstrap',
      '@primer/octicons-react',
      'prop-types',
      'perspectives-proxy',
      'pouchdb-browser',
      'i18next',
      'regenerator-runtime',
      '@chatscope/chat-ui-kit-styles/dist/default/styles.min.css'
    ]
  },
  {
    input: 'dist/types/perspectives-react-components.d.ts',
    output: [{ file: 'dist/perspectives-react-components.d.ts', format: 'es' }],
    plugins: [
      dts(),
      postcss({
        extract: true, // Extract CSS to a separate file
        minimize: true, // Minimize the CSS
        sourceMap: true, // Generate source maps for the CSS
        extensions: ['.css', '.scss'], // Handle both CSS and SCSS files
      }),
      typescript({
        tsconfig: './tsconfig.json',
        declaration: true,
        declarationDir: 'dist/types',
        rootDir: 'src'
      })
    ],
    external: [
      /\.css$/,  // Match any CSS file
      '@chatscope/chat-ui-kit-styles/dist/default/styles.min.css'
    ]  }
];