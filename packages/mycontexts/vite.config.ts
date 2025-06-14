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

// This is the web root where the app will be served from
// For development, this is usually "/www/"
// For production, it will be "/".
const {build, webroot} = JSON.parse(fs.readFileSync("./build.json", { encoding: "utf-8" }))

// https://vite.dev/config/
export default defineConfig({
  base: webroot,
  server: {
    port: 5177,
    host: '0.0.0.0',
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
      'Expires': '0',
      'Access-Control-Allow-Origin': '*',
      'Content-Security-Policy': "default-src * 'self' 'unsafe-inline' 'unsafe-eval' data: gap: https://ssl.gstatic.com; style-src * 'self' 'unsafe-inline'; script-src * 'self' 'unsafe-inline' 'unsafe-eval';"
    },
  },
  plugins: [
    react(),
    commonjs(),
    tsconfigPaths(),
    
    // Add this plugin to fix the @vite/client loading issue
    {
      name: 'fix-vite-client-path',
      configureServer(server) {
        // Early middleware to handle /www/@vite/client requests
        server.middlewares.use((req, res, next) => {
          // If requesting vite client through /www/ path, adjust the URL
          if (req.url?.startsWith('/www/@vite')) {
            console.log(`Redirecting ${req.url} to ${req.url.replace('/www/', '/')}`);
            req.url = req.url.replace('/www/', '/');
          }
          next();
        });
      }
    },
    {
      name: 'adjust-vite-client-html',
      transformIndexHtml(html) {
        if (webroot === '/www/') {
          // Fix paths in development mode HTML
          return html.replace(
            /<script type="module" src="\/@vite\/client"><\/script>/,
            `<script type="module" src="${webroot}@vite/client"></script>`
          );
        }
        return html;
      }
    }
  ],
  resolve: {
    alias: {
      'perspectives-core': resolve( __dirname, '../perspectives-core/dist/perspectives-core.js'),
      'perspectives-sharedworker': resolve( __dirname, '../perspectives-sharedworker/dist/perspectives-sharedworker.js'),
      'perspectives-pageworker': resolve(__dirname, '../perspectives-pageworker/dist/perspectives-pageworker.js'),
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
          buildStart: async () => {
            console.log(`Generating development service worker (webroot: ${webroot})...`);
            
            // Read the service worker template
            let swContent = fs.readFileSync('./src/perspectives-serviceworker.js', 'utf8');
            
            // For development, include basic files with correct paths
            const devFiles = [
              `${webroot}index.html`,
              `${webroot}manage.html`,
              `${webroot}assets/main.js`,
              `${webroot}assets/main.css`
            ];
            
            // Create the file array as a string
            const fileListStr = devFiles.map(file => `"${file}"`).join(',\n  ');
            
            // Update file list in service worker
            if (swContent.includes('const appFiles = [')) {
              swContent = swContent.replace(
                /const appFiles = \[\s*[\s\S]*?\];/m,
                `const appFiles = [\n  ${fileListStr}\n];`
              );
            } else {
              // Add it if it doesn't exist
              swContent = swContent.replace(
                /(const cacheName = .*)/,
                `$1\n\nconst appFiles = [\n  ${fileListStr}\n];`
              );
            }
            
            // Replace version placeholders
            swContent = swContent.replace(/__MYCONTEXTS_VERSION__/g, JSON.stringify(thepackage.version));
            swContent = swContent.replace(/__BUILD__/g, JSON.stringify(build));
            
            // Write to public directory for development
            fs.writeFileSync('./public/perspectives-serviceworker.js', swContent);
            console.log('Development service worker generated at ./public/perspectives-serviceworker.js');
          },
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
            fs.writeFileSync('./dist/perspectives-serviceworker.js', swContent);
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
