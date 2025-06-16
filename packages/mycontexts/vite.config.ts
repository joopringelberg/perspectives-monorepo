import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react-swc'
import tsconfigPaths from 'vite-tsconfig-paths' 
import { visualizer } from 'rollup-plugin-visualizer'
import copy from 'rollup-plugin-copy'
import del from 'rollup-plugin-delete'
import { resolve } from 'path';
import fs from 'fs';
import glob from 'fast-glob';
import rollupCommonjs from '@rollup/plugin-commonjs';

import { default as thepackage } from './package.json'

// Get path to monorepo root
const monorepoRoot = resolve(__dirname, '../..');

const pageDispatcherVersion = "1";

// This is the web root where the app will be served from
// For development, this is usually "/www/"
// For production, it will be "/".
const {build, webroot} = JSON.parse(fs.readFileSync("./build.json", { encoding: "utf-8" }))

// Determine if we're in development mode
const isDevMode = process.env.NODE_ENV !== 'production';

// https://vite.dev/config/
export default defineConfig({
  // Use root path for dev server, configured path for production
  base: isDevMode ? '/' : webroot,
  server: {
    port: 5177,
    host: '0.0.0.0',
    https: {
      key: fs.readFileSync(resolve(monorepoRoot, 'certificates/key.pem')),
      cert: fs.readFileSync(resolve(monorepoRoot, 'certificates/cert.pem')),
    },
    fs: {
      // Make sure strict is false to allow serving from outside project root
      strict: false,
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
    {
      name: 'handle-react-esm',
      enforce: 'pre',
      load(id) {
        // Handle specific React modules
        if (id.includes('react') && id.includes('node_modules')) {
          // Let React's ESM work properly
          return null;
        }
      }
    },
    react({
      jsxRuntime: 'automatic',
      jsxImportSource: 'react',
      development: process.env.NODE_ENV !== 'production'
    }),
    tsconfigPaths(),    
    {
      name: 'inject-app-base-path',
      transformIndexHtml(html) {
        // Inject the real base path as a global variable
        const injectScript = `
        <script>
          window.__APP_BASE_PATH__ = "${webroot}";
        </script>`;
        
        return html.replace('</head>', `${injectScript}</head>`);
      }
    },
    {
      name: 'fix-commonjs-exports',
      transform(code, id) {
        const commonjsModules = [
          'invariant',
          'classnames',
          'lodash',
          'react-transition-group', 
          'warning',
          'uncontrollable',
          'events',
          'spark-md5',
          'vuvuzela',
        ];    
    // Rest of your existing code remains the same
    const isTargetModule = commonjsModules.some(module => 
      id.includes(`/node_modules/${module}/`) || 
      id.includes(`/node_modules/.pnpm/${module}@`)
    );

    if (isTargetModule) {
      // Only modify if it doesn't already have an export default
      if (!code.includes('export default') && !code.includes('exports.default =')) {
        console.log(`Adding default export to CommonJS module: ${id}`);
        
        // Add appropriate export depending on module structure
        if (code.includes('module.exports =')) {
          return {
            code: `${code}\nexport default module.exports;`,
            map: null
          };
        } else if (code.includes('exports.')) {
          return {
            code: `${code}\nexport default exports;`,
            map: null
          };
        }
      }
    }
    
    return null;
      }
    },
    {
      name: 'monitor-esm-compatibility',
      transform(code, id, options) {
        // Only monitor node_modules
        if (id.includes('node_modules') && !id.includes('?v=')) {
          // Check for potential ESM/CommonJS issues
          const hasModuleExports = code.includes('module.exports');
          const hasDefaultExport = code.includes('export default') || code.includes('exports.default =');
          
          if (hasModuleExports && !hasDefaultExport) {
            console.log(`[ESM Compat Warning] ${id.split('/node_modules/')[1]} might need default export`);
          }
        }
        return null;
      }
    },
    rollupCommonjs({
      requireReturnsDefault: 'preferred',
      transformMixedEsModules: true,
      include: [],
      exclude: [/@paralleldrive\/cuid2/, /react/, /react-dom/]
    }),
  ],
  resolve: {
    alias: {
      // Keep your local package aliases
      'perspectives-core': resolve(__dirname, '../perspectives-core/dist/perspectives-core.js'),
      'perspectives-sharedworker': resolve(__dirname, '../perspectives-sharedworker/dist/perspectives-sharedworker.js'),
      'perspectives-pageworker': resolve(__dirname, '../perspectives-pageworker/dist/perspectives-pageworker.js'),
      '@perspectives/core': resolve(__dirname, '../perspectives-core/src'),
      '@perspectives/proxy': resolve(__dirname, '../perspectives-proxy/src'),
      '@perspectives/react': resolve(__dirname, '../perspectives-react/src'),
      'react/jsx-dev-runtime': isDevMode ? resolve(__dirname, 'src/jsx-dev-runtime-shim.js') : 'react/jsx-dev-runtime',
      'react-dom/client': 'react-dom/client.js',
    }
  },
  optimizeDeps: {
    include: [
      'react', 
      'react-dom',
      'react/jsx-runtime',
      'react/jsx-dev-runtime'],
    exclude: [
      'invariant',
      'classnames'
      // Add other problematic packages
    ],
    esbuildOptions: {
      // Node.js global to browser globalThis
      define: {
        global: 'globalThis'
      },
      // Enable JSX in .js files
    loader: {
      '.js': 'jsx',
      '.ts': 'tsx',
      '.jsx': 'jsx',
      '.tsx': 'tsx'
    },
    format: 'esm'  // Add this line
    }
  },
  build: {
    target: 'es2023',
    sourcemap: true,
    commonjsOptions: {
      transformMixedEsModules: true,
      requireReturnsDefault: 'auto',
      defaultIsModuleExports: true
    },
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
