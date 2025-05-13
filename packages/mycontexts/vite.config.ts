import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'
import tsconfigPaths from 'vite-tsconfig-paths' 
import commonjs from 'vite-plugin-commonjs'
import { visualizer } from 'rollup-plugin-visualizer'
import copy from 'rollup-plugin-copy'
import del from 'rollup-plugin-delete'
import { resolve } from 'path';
import fs from 'fs';
import postcss from 'rollup-plugin-postcss';
import json from '@rollup/plugin-json';import { default as thepackage } from './package.json'

// Get path to monorepo root
const monorepoRoot = resolve(__dirname, '../..');

const pageDispatcherVersion = "1";

// https://vite.dev/config/
export default defineConfig({
  base: "/www/",
  server: {
    port: 5177,
    https: {
      key: fs.readFileSync(resolve(monorepoRoot, 'certificates/key.pem')),
      cert: fs.readFileSync(resolve(monorepoRoot, 'certificates/cert.pem')),
    },
    fs: {
      allow: [
        '..', // Allow serving files from one level up to the project root
        '/Users/joopringelberg/Code/perspectives-monorepo/node_modules',
        '/Users/joopringelberg/Code/perspectives-monorepo/packages/perspectives-core/output'
      ] 
    },
    headers: {
      // Critical headers for service worker development
      'Service-Worker-Allowed': '/',
      'Cache-Control': 'no-store, no-cache, must-revalidate, proxy-revalidate, max-age=0',
      'Pragma': 'no-cache',
      'Surrogate-Control': 'no-store',
      'Expires': '0'
    }
  },
  plugins: [
    react(),
    commonjs(),
    tsconfigPaths()
  ],
  resolve: {
    alias: {
      'perspectives-core': resolve( __dirname, '../perspectives-core/dist/perspectives-core.js'),
      'perspectives-sharedworker': resolve( __dirname, '../perspectives-sharedworker/dist/perspectives-sharedworker.js'),
      '@perspectives/core': resolve( __dirname, '../perspectives-core/src'),
      '@perspectives/proxy': resolve( __dirname, '../perspectives-proxy/src'),
      '@perspectives/react': resolve( __dirname, '../perspectives-react/src')
    }
  },
  optimizeDeps: {
    include: ['react', 'react-dom']
  },
  build: {
    target: 'es2023',
    sourcemap: true,
    rollupOptions: {
      input: {
        main: './index.html',
        manage: './manage.html'
      },
      plugins: [
        del({ targets: 'dist/*' }), // Add this line to clear the dist directory
        // Add this plugin to copy assets
        copy({
          targets: [
            { 
              src: 'src/perspectives-pagedispatcher.js', 
              dest: 'public',
              rename: (name) => `perspectives-pagedispatcher${pageDispatcherVersion}.js` 
            },
            {
              src: 'src/notification-worker.js',
              dest: 'public',
            },
            { 
              src: 'public/**/*', 
              dest: 'dist' 
            },
          ]
        }),
        // visualizer({
        //   filename: './dist/stats.html',
        //   open: true
        // })
      ]
    }
  },
  define: {
    __MYCONTEXTS_VERSION__: JSON.stringify(thepackage.version),
    __STARTPAGE__: JSON.stringify("pub:https://perspectives.domains/cw_j4qovsczpm/#bxjprzq9q6$External"),
    __MyContextsContainer__: JSON.stringify("root"),
    __PAGEDISPATCHER_VERSION__: pageDispatcherVersion
  }
})
