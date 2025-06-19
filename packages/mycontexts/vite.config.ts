import { defineConfig } from 'vite'
// import react from '@vitejs/plugin-react-swc'
import tsconfigPaths from 'vite-tsconfig-paths' 
import commonjs from '@rollup/plugin-commonjs'
import react from '@vitejs/plugin-react'
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
            'markdown-it-link-attributes',
            'spark-md5',
            'vuvuzela',
            '@paralleldrive/cuid2'
          ];
          
          if (cjsModules.some(mod => id.includes(mod))) {
            // Log for debugging
            
            // Check if it's already an ESM module with a default export
            if (code.includes('export default')) {
              return null;
            }
            
            // Different patterns of CommonJS modules
            if (code.includes('module.exports')) {
              console.log(`Transforming CJS module with 'module.exports': ${id}`);
              // Create a virtual CommonJS environment
              return `
// ESM bridge for CommonJS module
const exports = {};
const module = { exports };

// Original module code
${code}

// Re-export as default
export default module.exports;
`;
            } 
            // Some modules use direct exports
            else if (code.includes('exports.') || code.includes('exports[')) {
              console.log(`Transforming CJS module with 'export default exports': ${id}`);
              return `
// ESM bridge for CommonJS module
const exports = {};

// Original module code
${code}

// Re-export as default
export default exports;
`;
            }
            // Some modules might have a global object they're attaching to
            else if (id.includes('vuvuzela') && !code.includes('export')) {
              // For vuvuzela specifically, which might have a different structure
              console.log(`Transforming vuvuzela with 'export default vuvuzela': ${id}`);
              return `
// Original module code
${code}

// Export the global object
export default (typeof vuvuzela !== 'undefined' ? vuvuzela : {});
`;
            }
          }
          return null;
        }
      },
      
      // React ESM compat fix
      {
        name: 'react-esm-compat',
        enforce: 'pre',
        transform(code, id) {
          // Map of specific modules that need replacement
          const moduleReplacements = {
            'react-dom/client.js': `
import * as ReactDOM from 'react-dom';

export const createRoot = ReactDOM.createRoot;
export const hydrateRoot = ReactDOM.hydrateRoot;
export default {
  createRoot: ReactDOM.createRoot,
  hydrateRoot: ReactDOM.hydrateRoot
};`,
            'react-dom/server.js': `
import * as ReactDOMServer from './cjs/react-dom-server.browser.${mode !== 'production' ? 'development' : 'production.min'}.js';
export * from './cjs/react-dom-server.browser.${mode !== 'production' ? 'development' : 'production.min'}.js';
export default ReactDOMServer;`
          };
          
          // Check for exact module matches first
          for (const [modulePath, replacement] of Object.entries(moduleReplacements)) {
            if (id.includes(modulePath)) {
              console.log(`Replacing module: ${modulePath}`);
              return replacement;
            }
          }
          
          // Then handle the pattern-based transformations
          
          // React main module
          if (id.includes('node_modules/react/index.js')) {
            console.log('Replacing React entry module with ESM version');
            
            // For dev builds
            if (mode !== 'production') {
              return `
import * as React from './cjs/react.development.js';
export * from './cjs/react.development.js';
export default React;
`;
            } else {
              // For production builds
              return `
import * as React from './cjs/react.production.min.js';
export * from './cjs/react.production.min.js';
export default React;
`;
            }
          }
          
          // React DOM main module
          if (id.includes('node_modules/react-dom/index.js')) {
            console.log('Fixing React DOM main module');
            
            // For dev builds
            if (mode !== 'production') {
              return `
import * as ReactDOM from './cjs/react-dom.development.js';
export * from './cjs/react-dom.development.js';
export default ReactDOM;
`;
            } else {
              // For production builds
              return `
import * as ReactDOM from './cjs/react-dom.production.min.js';
export * from './cjs/react-dom.production.min.js';
export default ReactDOM;
`;
            }
          }
          
          // Handle React DOM development version
          if (id.includes('react-dom/cjs/react-dom.development.js')) {
            console.log('Converting React DOM development file to ESM');
            
            return `
// ESM bridge for CommonJS module
const exports = {};
const module = { exports };

${code}

// Export all properties from the exports object
export const __SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED = exports.__SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED;
export const createPortal = exports.createPortal;
export const createRoot = exports.createRoot;
export const findDOMNode = exports.findDOMNode;
export const flushSync = exports.flushSync;
export const hydrate = exports.hydrate;
export const hydrateRoot = exports.hydrateRoot;
export const render = exports.render;
export const unmountComponentAtNode = exports.unmountComponentAtNode;
export const unstable_batchedUpdates = exports.unstable_batchedUpdates;
export const unstable_renderSubtreeIntoContainer = exports.unstable_renderSubtreeIntoContainer;
export const version = exports.version;

// Default export for import ReactDOM from 'react-dom'
export default exports;
`;
          }
          
          // For React's development file
          if (id.includes('react/cjs/react.development.js')) {
            console.log('Converting React development file to ESM');
            
            // Add ESM export handling
            return `
// ESM bridge for CommonJS module
const exports = {};
const module = { exports };

${code}

// Re-export all exports as named exports
export const Children = exports.Children;
export const Component = exports.Component;
export const Fragment = exports.Fragment;
export const Profiler = exports.Profiler;
export const PureComponent = exports.PureComponent;
export const StrictMode = exports.StrictMode;
export const Suspense = exports.Suspense;
export const __SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED = exports.__SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED;
export const cloneElement = exports.cloneElement;
export const createContext = exports.createContext;
export const createElement = exports.createElement;
export const createFactory = exports.createFactory;
export const createRef = exports.createRef;
export const forwardRef = exports.forwardRef;
export const isValidElement = exports.isValidElement;
export const lazy = exports.lazy;
export const memo = exports.memo;
export const startTransition = exports.startTransition;
export const useCallback = exports.useCallback;
export const useContext = exports.useContext;
export const useDebugValue = exports.useDebugValue;
export const useDeferredValue = exports.useDeferredValue;
export const useEffect = exports.useEffect;
export const useId = exports.useId;
export const useImperativeHandle = exports.useImperativeHandle;
export const useInsertionEffect = exports.useInsertionEffect;
export const useLayoutEffect = exports.useLayoutEffect;
export const useMemo = exports.useMemo;
export const useReducer = exports.useReducer;
export const useRef = exports.useRef;
export const useState = exports.useState;
export const useSyncExternalStore = exports.useSyncExternalStore;
export const useTransition = exports.useTransition;
export const version = exports.version;

// Default export for import React from 'react'
export default exports;
`;
          }
          
          // For React's production file
          if (id.includes('react/cjs/react.production.min.js')) {
            console.log('Converting React production file to ESM');
            
            // Same approach for production file
            return `
// ESM bridge for CommonJS module
const exports = {};
const module = { exports };

${code}

// Re-export all exports as named exports
export const Children = exports.Children;
export const Component = exports.Component;
export const Fragment = exports.Fragment;
export const Profiler = exports.Profiler;
export const PureComponent = exports.PureComponent;
export const StrictMode = exports.StrictMode;
export const Suspense = exports.Suspense;
export const __SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED = exports.__SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED;
export const cloneElement = exports.cloneElement;
export const createContext = exports.createContext;
export const createElement = exports.createElement;
export const createFactory = exports.createFactory;
export const createRef = exports.createRef;
export const forwardRef = exports.forwardRef;
export const isValidElement = exports.isValidElement;
export const lazy = exports.lazy;
export const memo = exports.memo;
export const startTransition = exports.startTransition;
export const useCallback = exports.useCallback;
export const useContext = exports.useContext;
export const useDebugValue = exports.useDebugValue;
export const useDeferredValue = exports.useDeferredValue;
export const useEffect = exports.useEffect;
export const useId = exports.useId;
export const useImperativeHandle = exports.useImperativeHandle;
export const useInsertionEffect = exports.useInsertionEffect;
export const useLayoutEffect = exports.useLayoutEffect;
export const useMemo = exports.useMemo;
export const useReducer = exports.useReducer;
export const useRef = exports.useRef;
export const useState = exports.useState;
export const useSyncExternalStore = exports.useSyncExternalStore;
export const useTransition = exports.useTransition;
export const version = exports.version;

// Default export for import React from 'react'
export default exports;
`;
          }
          
          // For JSX runtime
          if (id.includes('react/jsx-runtime')) {
            console.log('Patching jsx-runtime module');
            
            // Your existing jsx-runtime fix
            return `
import { createElement, Fragment as _Fragment } from 'react';

export const Fragment = _Fragment;

// Create jsx and jsxs functions based on createElement
export function jsx(type, props, key) {
  const propsCopy = { ...props };
  if (key !== undefined) {
    propsCopy.key = key;
  }
  return createElement(type, propsCopy);
}

export function jsxs(type, props, key) {
  return jsx(type, props, key);
}

// Also export as named imports for different transpiler patterns
export { jsx as _jsx, jsxs as _jsxs };
`;
          }
          
          return null;
        }
      },
      
      // Add this after your fix-cjs-exports plugin
      {
        name: 'handle-cjs-require',
        enforce: 'pre',
        transform(code, id) {
          // Process any file from @paralleldrive/cuid2 that uses require
          if (id.includes('@paralleldrive/cuid2') && 
              code.includes('require(') && 
              !code.includes('typeof require')) {
            
            console.log(`Adding require implementation to: ${id}`);
            
            // Extract the relative paths being required
            const requirePaths: string[] = [];
            const regex = /require\(['"]([^'"]+)['"]\)/g;
            let match;
            
            while ((match = regex.exec(code)) !== null) {
              requirePaths.push(match[1]);
            }
            
            // Create ESM imports for each required module
            const imports = requirePaths.map(path => {
              // Convert relative path to ESM import - create a safe variable name
              // Replace all non-alphanumeric characters with underscores
              const safeVarName = path.replace(/[@/\.-]/g, '_').replace(/\+/g, '_plus_');
              return `import * as _${safeVarName} from '${path}';`;
            }).join('\n');
            
            // Create the require function
            const requireFunc = `
// ESM bridge for CommonJS require
function require(path) {
  ${requirePaths.map(path => {
    const safeVarName = path.replace(/[@/\.-]/g, '_').replace(/\+/g, '_plus_');
    return `if (path === '${path}') return _${safeVarName};`;
  }).join('\n  ')}
  throw new Error('Unsupported require path: ' + path);
}`;
            
            // Also create CommonJS environment objects
            const cjsSetup = `
// Create CommonJS environment variables
const exports = {};
const module = { exports };`;
            
            // Assemble the final code
            return `
// ESM imports
${imports}

${requireFunc}

${cjsSetup}

// Original module
${code}

// Export using dynamic approach that avoids redeclarations
export default module.exports;
const _exportNames = ['createId', 'init', 'getConstants', 'isCuid'];
_exportNames.forEach(name => {
  if (name !== 'default' && name in module.exports) {
    Object.defineProperty(exports, name, {
      enumerable: true,
      get: function() { return module.exports[name]; }
    });
  }
});
`;
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
      react({
        jsxImportSource: 'react',
        babel: {
          plugins: [
            ['@babel/plugin-transform-react-jsx', {
              runtime: 'automatic'
            }]
          ]
        }
      }),  
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
