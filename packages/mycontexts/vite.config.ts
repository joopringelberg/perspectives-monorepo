import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'
import tsconfigPaths from 'vite-tsconfig-paths' 
import commonjs from '@rollup/plugin-commonjs'
import { visualizer } from 'rollup-plugin-visualizer'
import copy from 'rollup-plugin-copy'
import del from 'rollup-plugin-delete'
import { resolve } from 'path';
import fs from 'fs';
import glob from 'fast-glob';
import { nodeResolve } from '@rollup/plugin-node-resolve';

import { default as thepackage } from './package.json'

// Get path to monorepo root
const monorepoRoot = resolve(__dirname, '../..');

const pageDispatcherVersion = "1";

// https://vite.dev/config/
export default defineConfig(({ mode }) => {
  // For development, always use root base
  const isDev = mode === 'development';
  
  // Production base is determined by build.json
  const buildPath = process.env.BUILD_PATH || '/';
  const devBase = '/';
  
  const { build } = JSON.parse(fs.readFileSync("./build.json", { encoding: "utf-8" }));
  
  return {
    base: isDev ? devBase : buildPath,
    server: {
      port: 5177,
      host: '0.0.0.0',
      https: {
        key: fs.readFileSync(resolve(monorepoRoot, 'certificates/key.pem')),
        cert: fs.readFileSync(resolve(monorepoRoot, 'certificates/cert.pem')),
      },
      fs: {
        allow: [
          '..', 
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
      // First plugin should be the CJS fix 
      {
        name: 'fix-cjs-exports',
        transform(code, id) {
          // List of problematic CommonJS modules
          const cjsModules = [
            'invariant',
            'warning', 
            'classnames', 
            'events', 
            'react-transition-group',
            'prop-types-extra',
            'prop-types',
            'uncontrollable',
            'highlight.js',
            'markdown-it-link-attributes'
          ];
          
          if (cjsModules.some(mod => id.includes(mod)) && 
              !code.includes('export default') && 
              code.includes('module.exports')) {
            return `${code}\nexport default module.exports;`;
          }
          return null;
        }
      },
      
      // React ESM compat fix
      {
        name: 'react-esm-compat',
        transform(code, id) {
          if (id.includes('node_modules/react/') && !code.includes('export default') && code.includes('export {')) {
            return `${code}\nexport default React;`;
          }
          if (id.includes('react/jsx-dev-runtime') && !code.includes('export const jsxDEV')) {
            return `${code}\nexport const jsxDEV = (type, props, key, isStaticChildren, source, self) => jsx(type, props, key);`;
          }
          return null;
        }
      },
      
      // Node resolve
      nodeResolve({
        extensions: ['.mjs', '.js', '.jsx', '.ts', '.tsx', '.json'],
        browser: true
      }),
      
      // React
      react(),
      
      // CommonJS with careful inclusion/exclusion
      commonjs({
        requireReturnsDefault: 'preferred',
        transformMixedEsModules: true,
        exclude: [
          /node_modules\/react(\/|$)/,
          /node_modules\/react-dom(\/|$)/
        ],
        include: [
          /node_modules\/(invariant|warning|classnames|events|react-transition-group|prop-types|prop-types-extra|uncontrollable|highlight\.js|markdown-it-link-attributes)\/.*/
        ]
      }),
      
      // Other plugins
      tsconfigPaths(),
      {
        name: 'generate-service-worker',
        buildStart: async () => {
          console.log(`Generating development service worker (webroot: ${buildPath})...`);
          
          // Read the service worker template
          let swContent = fs.readFileSync('./src/perspectives-serviceworker.js', 'utf8');
          
          // For development, include basic files with correct paths
          const devFiles = [
            `${buildPath}index.html`,
            `${buildPath}manage.html`,
            `${buildPath}assets/main.js`,
            `${buildPath}assets/main.css`
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
          // visualizer({
          //   filename: './dist/stats.html',
          //   open: true
          // })
          // A custom plugin to generate the service worker with file list
          {
            name: 'generate-service-worker',
            buildStart: async () => {
              console.log(`Generating development service worker (webroot: ${buildPath})...`);
              
              // Read the service worker template
              let swContent = fs.readFileSync('./src/perspectives-serviceworker.js', 'utf8');
              
              // For development, include basic files with correct paths
              const devFiles = [
                `${buildPath}index.html`,
                `${buildPath}manage.html`,
                `${buildPath}assets/main.js`,
                `${buildPath}assets/main.css`
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
  }
});
