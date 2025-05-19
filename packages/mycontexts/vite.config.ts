import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'
import tsconfigPaths from 'vite-tsconfig-paths' 
import commonjs from 'vite-plugin-commonjs'
import { visualizer } from 'rollup-plugin-visualizer'
import copy from 'rollup-plugin-copy'
import del from 'rollup-plugin-delete'
import { resolve } from 'path';
import fs from 'fs';
import glob from 'fast-glob';

import { default as thepackage } from './package.json'

// Get path to monorepo root
const monorepoRoot = resolve(__dirname, '../..');

const pageDispatcherVersion = "1";

const build = JSON.parse(fs.readFileSync("./build.json", { encoding: "utf-8" })).build;


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
        // A custom plugin to generate the service worker with file list
        {
          name: 'generate-service-worker',
          writeBundle: async () => {
            // Read the service worker template
            let swContent = fs.readFileSync('./src/perspectives-serviceworker.js', 'utf8');
            
            // Get all files in dist directory (recursively)

            const files = await glob('**/*', { 
              cwd: resolve(__dirname, 'dist'),
              onlyFiles: true 
            });
            
            // Create the file array as a string
            const fileListStr = files
              .map(file => `"/${file}"`)
              .join(',\n  ');
            
            // Replace placeholder if it exists, or insert after declaration
            if (swContent.includes('const appFiles = [')) {
              swContent = swContent.replace(
                /const appFiles = \[\s*[\s\S]*?\];/m,
                `const appFiles = [\n  ${fileListStr}\n];`
              );
            } else {
              // Add it after the cacheName declaration
              swContent = swContent.replace(
                /(const cacheName = .*)/,
                `$1\n\nconst appFiles = [\n  ${fileListStr}\n];`
              );
            }
            
            // Replace version placeholders
            swContent = swContent.replace(/__MYCONTEXTS_VERSION__/g, JSON.stringify(thepackage.version));
            swContent = swContent.replace(/__BUILD__/g, JSON.stringify(build));
            
            // Write the final service worker to dist
            fs.writeFileSync(`./dist/perspectives-serviceworker${build}.js`, swContent);
            console.log('Service worker generated with', files.length, 'files in cache list');
          }
        }
      ]
    }
  },
  define: {
    __MYCONTEXTS_VERSION__: JSON.stringify(thepackage.version),
    __STARTPAGE__: JSON.stringify("pub:https://perspectives.domains/cw_j4qovsczpm/#bxjprzq9q6$External"),
    __MyContextsContainer__: JSON.stringify("root"),
    __PAGEDISPATCHER_VERSION__: pageDispatcherVersion,
    __BUILD__: JSON.stringify(build),
  }
})
