import { defineConfig } from 'vite'
// import react from '@vitejs/plugin-react-swc'
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
  const devBase = '/';
  
  const { build, buildPath } = JSON.parse(fs.readFileSync("./build.json", { encoding: "utf-8" }));

  console.log(`Building in ${mode} mode, isDev: ${isDev}, buildPath: ${buildPath}`);
  
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
    ],
    resolve: {
      alias: {
        'perspectives-core': resolve( __dirname, '../perspectives-core/dist/perspectives-core.js'),
        'perspectives-sharedworker': resolve( __dirname, '../perspectives-sharedworker/dist/perspectives-sharedworker.js'),
        'perspectives-pageworker': resolve(__dirname, '../perspectives-pageworker/dist/perspectives-pageworker.js'),
        '@perspectives/core': resolve( __dirname, '../perspectives-core/src'),
        '@perspectives/proxy': resolve( __dirname, '../perspectives-proxy/src'),
        '@perspectives/react': resolve( __dirname, '../perspectives-react/src'),
        
        // Force a single React instance across all packages
        'react': resolve(__dirname, 'node_modules/react'),
        'react-dom': resolve(__dirname, 'node_modules/react-dom'),
        'react/jsx-runtime': resolve(__dirname, 'node_modules/react/jsx-runtime'),
        'react/jsx-dev-runtime': resolve(__dirname, 'node_modules/react/jsx-dev-runtime'),
        'react-is': resolve(monorepoRoot, 'node_modules/.pnpm/react-is@16.13.1/node_modules/react-is'),
      },
      dedupe: ['react', 'react-dom', 'react-is']
    },
    optimizeDeps: {
      include: [
        'react', 
        'react-dom', 
        'react/jsx-runtime',
        'react/jsx-dev-runtime',
      ],
      exclude: [        
        'react/cjs/react.production.min.js',
        'react-dom/cjs/react-dom.production.min.js'
      ],
      esbuildOptions: {
        mainFields: ['module', 'main'],
        resolveExtensions: ['.mjs', '.js', '.ts', '.jsx', '.tsx', '.json'],
        format: 'esm'
      }
    },
    build: {
      target: 'es2023',
      sourcemap: true,
      rollupOptions: {
        output: {
          entryFileNames: 'assets/[name]-[hash].js',
          chunkFileNames: 'assets/[name]-[hash].js',
          assetFileNames: 'assets/[name]-[hash].[ext]',
        },
        input: {
          main: './index.html',
          manage: './manage.html',
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
