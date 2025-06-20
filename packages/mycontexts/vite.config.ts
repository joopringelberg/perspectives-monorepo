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
      {
        name: 'fix-react-direct-import',
        enforce: 'pre',
        load(id) {
          // Handle direct loading of React's index.js
          if (id.endsWith('node_modules/react/index.js') || 
              id.includes('node_modules/.pnpm/react@18.3.1/node_modules/react/index.js')) {
            console.log(`Direct load of React's index.js: ${id}`);
            
            // For production, we want to load the production version directly
          return `
// ESM version of React's entry point
export * from './cjs/react.production.min.js';
import React from './cjs/react.production.min.js';
export default React;
`;
          }

          // Handle direct loading of React DOM's index.js
          if (id.endsWith('node_modules/react-dom/index.js') || 
              id.includes('node_modules/.pnpm/react-dom@18.3.1/node_modules/react-dom/index.js')) {
            console.log(`Direct load of React DOM's index.js: ${id}`);
            
            // For production, load the production version directly
            return `
// ESM version of React DOM's entry point
export * from './cjs/react-dom.production.min.js';
import ReactDOM from './cjs/react-dom.production.min.js';
export default ReactDOM;
`;
          }
          
          return null;
        }
      },
      {
        name: 'fix-react-production-imports',
        enforce: 'pre',
        resolveId(source, importer) {
          // Handle the case with ?commonjs-proxy suffix
          if (importer && importer.includes('?commonjs-proxy') && source.startsWith('./cjs/')) {
            const originalImporter = importer.split('?')[0];
            const reactDir = originalImporter.substring(0, originalImporter.lastIndexOf('/'));
            const fileName = source.substring(source.lastIndexOf('/') + 1);
            const absolutePath = resolve(reactDir, 'cjs', fileName);
            
            console.log(`[commonjs-proxy] Redirecting ${source} from ${importer} to ${absolutePath}`);
            return { id: absolutePath, moduleSideEffects: true };
          }
          
          // Original cases
          if (source === './cjs/react.production.min.js' && importer && importer.includes('node_modules/react/index.js')) {
            const reactDir = importer.substring(0, importer.lastIndexOf('/'));
            const absolutePath = resolve(reactDir, 'cjs/react.production.min.js');
            
            console.log(`Redirecting ${source} to ${absolutePath}`);
            return { id: absolutePath, moduleSideEffects: true };
          }
          
          return null;
        },
        
        // Add a transform hook that will handle both the normal files and ?commonjs-proxy versions
        transform(code, id) {
          // Remove ?commonjs-proxy suffix for checking
          const cleanId = id.split('?')[0];
          
          // React's production minified file 
          if (cleanId.includes('react/cjs/react.production.min.js')) {
            console.log(`Converting React production minified file to ESM: ${id}`);
            
            // If already transformed (has export default), skip
            if (code.includes('export default')) {
              console.log(`Skipping already transformed React file: ${id}`);
              return null;
            }
            
            // Transform the production file carefully
            return `
// ESM bridge for CommonJS module - production
const exports = {};
const module = { exports };

${code}

// Re-export all named exports from the exports object
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

// Default export
export default exports;
`;
    }

    // React DOM's production minified file
    if (cleanId.includes('react-dom/cjs/react-dom.production.min.js')) {
      console.log(`Converting React DOM production minified file to ESM: ${id}`);
      
      // If already transformed, skip
      if (code.includes('export default')) {
        console.log(`Skipping already transformed React DOM file: ${id}`);
        return null;
      }
      
      return `
// ESM bridge for CommonJS module - production
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
          
          return null;
        }
      },
      {
        name: 'fix-react-is',
        enforce: 'pre',
        
        // Add a load hook to handle direct loading of react-is files
        load(id) {
          // Handle direct loading of react-is files with relative paths
          if (id === './cjs/react-is.production.min.js' || id === './cjs/react-is.development.js') {
            console.log(`Direct load of ${id} for react-is`);
            
            // Find the actual path in the pnpm structure
            const reactIsPath = resolve(monorepoRoot, 'node_modules/.pnpm/react-is@16.13.1/node_modules/react-is');
            const filePath = resolve(reactIsPath, id);
            
            try {
              const content = fs.readFileSync(filePath, 'utf-8');
              console.log(`Successfully loaded ${filePath}`);
              return content;
            } catch (e) {
              console.error(`Failed to read ${filePath}, creating virtual module`);
              
              // Create a minimal implementation as fallback
              return `
// Virtual implementation of react-is
'use strict';

Object.defineProperty(exports, '__esModule', { value: true });

// Type symbols
const REACT_ELEMENT_TYPE = Symbol.for('react.element');
const REACT_PORTAL_TYPE = Symbol.for('react.portal');
const REACT_FRAGMENT_TYPE = Symbol.for('react.fragment');
const REACT_STRICT_MODE_TYPE = Symbol.for('react.strict_mode');
const REACT_PROFILER_TYPE = Symbol.for('react.profiler');
const REACT_PROVIDER_TYPE = Symbol.for('react.provider');
const REACT_CONTEXT_TYPE = Symbol.for('react.context');
const REACT_FORWARD_REF_TYPE = Symbol.for('react.forward_ref');
const REACT_SUSPENSE_TYPE = Symbol.for('react.suspense');
const REACT_MEMO_TYPE = Symbol.for('react.memo');
const REACT_LAZY_TYPE = Symbol.for('react.lazy');

function typeOf(object) {
  if (typeof object === 'object' && object !== null) {
    const $$typeof = object.$$typeof;
    switch ($$typeof) {
      case REACT_ELEMENT_TYPE:
        return REACT_ELEMENT_TYPE;
      case REACT_PORTAL_TYPE:
        return REACT_PORTAL_TYPE;
      case REACT_FRAGMENT_TYPE:
        return REACT_FRAGMENT_TYPE;
      case REACT_STRICT_MODE_TYPE:
        return REACT_STRICT_MODE_TYPE;
      case REACT_PROFILER_TYPE:
        return REACT_PROFILER_TYPE;
      case REACT_PROVIDER_TYPE:
        return REACT_PROVIDER_TYPE;
      case REACT_CONTEXT_TYPE:
        return REACT_CONTEXT_TYPE;
      case REACT_FORWARD_REF_TYPE:
        return REACT_FORWARD_REF_TYPE;
      case REACT_SUSPENSE_TYPE:
        return REACT_SUSPENSE_TYPE;
      case REACT_MEMO_TYPE:
        return REACT_MEMO_TYPE;
      case REACT_LAZY_TYPE:
        return REACT_LAZY_TYPE;
    }
  }
  return undefined;
}

function isValidElementType(type) {
  return typeof type === 'string' || typeof type === 'function' || 
         type === REACT_FRAGMENT_TYPE || type === REACT_PROFILER_TYPE ||
         type === REACT_STRICT_MODE_TYPE || type === REACT_SUSPENSE_TYPE ||
         (typeof type === 'object' && type !== null && 
          (type.$$typeof === REACT_LAZY_TYPE || 
           type.$$typeof === REACT_MEMO_TYPE || 
           type.$$typeof === REACT_PROVIDER_TYPE || 
           type.$$typeof === REACT_CONTEXT_TYPE || 
           type.$$typeof === REACT_FORWARD_REF_TYPE));
}

// Exports
exports.typeOf = typeOf;
exports.isValidElementType = isValidElementType;
exports.isElement = obj => typeOf(obj) === REACT_ELEMENT_TYPE;
exports.isFragment = obj => typeOf(obj) === REACT_FRAGMENT_TYPE;
exports.isLazy = obj => typeOf(obj) === REACT_LAZY_TYPE;
exports.isMemo = obj => typeOf(obj) === REACT_MEMO_TYPE;
exports.isPortal = obj => typeOf(obj) === REACT_PORTAL_TYPE;
exports.isProfiler = obj => typeOf(obj) === REACT_PROFILER_TYPE;
exports.isStrictMode = obj => typeOf(obj) === REACT_STRICT_MODE_TYPE;
exports.isSuspense = obj => typeOf(obj) === REACT_SUSPENSE_TYPE;

// Constant values
exports.AsyncMode = Symbol.for('react.async_mode');
exports.ConcurrentMode = Symbol.for('react.concurrent_mode');
exports.ContextConsumer = REACT_CONTEXT_TYPE;
exports.ContextProvider = REACT_PROVIDER_TYPE;
exports.Element = REACT_ELEMENT_TYPE;
exports.ForwardRef = REACT_FORWARD_REF_TYPE;
exports.Fragment = REACT_FRAGMENT_TYPE;
exports.Lazy = REACT_LAZY_TYPE;
exports.Memo = REACT_MEMO_TYPE;
exports.Portal = REACT_PORTAL_TYPE;
exports.Profiler = REACT_PROFILER_TYPE;
exports.StrictMode = REACT_STRICT_MODE_TYPE;
exports.Suspense = REACT_SUSPENSE_TYPE;

// Remaining functions
exports.isAsyncMode = obj => false; // Deprecated
exports.isConcurrentMode = obj => false; // Deprecated
exports.isContextConsumer = obj => typeOf(obj) === REACT_CONTEXT_TYPE;
exports.isContextProvider = obj => typeOf(obj) === REACT_PROVIDER_TYPE;
exports.isForwardRef = obj => typeOf(obj) === REACT_FORWARD_REF_TYPE;
`;
        }
      }
      return null;
        },
        
        resolveId(source, importer) {
          // Handle ?commonjs-proxy suffix for react-is
          if (importer && importer.includes('react-is') && importer.includes('?commonjs-proxy') && 
              source.startsWith('./cjs/')) {
            const reactIsPath = resolve(monorepoRoot, 'node_modules/.pnpm/react-is@16.13.1/node_modules/react-is');
            const filePath = resolve(reactIsPath, source);
            console.log(`[commonjs-proxy] Redirecting ${source} from ${importer} to ${filePath}`);
            return filePath;
          }
          
          // Handle normal imports from react-is
          if (importer && importer.includes('react-is') && source.startsWith('./cjs/')) {
            const reactIsDir = importer.substring(0, importer.lastIndexOf('/'));
            const absolutePath = resolve(reactIsDir, source);
            console.log(`Redirecting ${source} from ${importer} to ${absolutePath}`);
            return absolutePath;
          }
          
          return null;
        },
        
        transform(code, id) {
          // Handle transform for main react-is index file
          if (id.includes('node_modules/react-is/index.js')) {
            console.log('Fixing react-is/index.js module');
            
            // Create a direct ESM implementation instead of relying on CJS imports
            return `// react-is ESM implementation
// Direct ESM implementation of react-is
const REACT_ELEMENT_TYPE = Symbol.for('react.element');
const REACT_PORTAL_TYPE = Symbol.for('react.portal');
const REACT_FRAGMENT_TYPE = Symbol.for('react.fragment');
const REACT_STRICT_MODE_TYPE = Symbol.for('react.strict_mode');
const REACT_PROFILER_TYPE = Symbol.for('react.profiler');
const REACT_PROVIDER_TYPE = Symbol.for('react.provider');
const REACT_CONTEXT_TYPE = Symbol.for('react.context');
const REACT_FORWARD_REF_TYPE = Symbol.for('react.forward_ref');
const REACT_SUSPENSE_TYPE = Symbol.for('react.suspense');
const REACT_MEMO_TYPE = Symbol.for('react.memo');
const REACT_LAZY_TYPE = Symbol.for('react.lazy');

// Type checking function
function typeOf(object) {
  if (typeof object === 'object' && object !== null) {
    const $$typeof = object.$$typeof;
    switch ($$typeof) {
      case REACT_ELEMENT_TYPE: return REACT_ELEMENT_TYPE;
      case REACT_PORTAL_TYPE: return REACT_PORTAL_TYPE;
      case REACT_FRAGMENT_TYPE: return REACT_FRAGMENT_TYPE;
      case REACT_STRICT_MODE_TYPE: return REACT_STRICT_MODE_TYPE;
      case REACT_PROFILER_TYPE: return REACT_PROFILER_TYPE;
      case REACT_PROVIDER_TYPE: return REACT_PROVIDER_TYPE;
      case REACT_CONTEXT_TYPE: return REACT_CONTEXT_TYPE;
      case REACT_FORWARD_REF_TYPE: return REACT_FORWARD_REF_TYPE;
      case REACT_SUSPENSE_TYPE: return REACT_SUSPENSE_TYPE;
      case REACT_MEMO_TYPE: return REACT_MEMO_TYPE;
      case REACT_LAZY_TYPE: return REACT_LAZY_TYPE;
    }
  }
  return undefined;
}

// Element type validation
function isValidElementType_(type) {
  return typeof type === 'string' || typeof type === 'function' || 
         type === REACT_FRAGMENT_TYPE || type === REACT_PROFILER_TYPE ||
         type === REACT_STRICT_MODE_TYPE || type === REACT_SUSPENSE_TYPE ||
         (typeof type === 'object' && type !== null && 
          (type.$$typeof === REACT_LAZY_TYPE || 
           type.$$typeof === REACT_MEMO_TYPE || 
           type.$$typeof === REACT_PROVIDER_TYPE || 
           type.$$typeof === REACT_CONTEXT_TYPE || 
           type.$$typeof === REACT_FORWARD_REF_TYPE));
}

// Named exports for all the type checking functions
export const AsyncMode = Symbol.for('react.async_mode');
export const ConcurrentMode = Symbol.for('react.concurrent_mode');
export const ContextConsumer = REACT_CONTEXT_TYPE;
export const ContextProvider = REACT_PROVIDER_TYPE;
export const Element = REACT_ELEMENT_TYPE;
export const ForwardRef = REACT_FORWARD_REF_TYPE;
export const Fragment = REACT_FRAGMENT_TYPE;
export const Lazy = REACT_LAZY_TYPE;
export const Memo = REACT_MEMO_TYPE;
export const Portal = REACT_PORTAL_TYPE;
export const Profiler = REACT_PROFILER_TYPE;
export const StrictMode = REACT_STRICT_MODE_TYPE;
export const Suspense = REACT_SUSPENSE_TYPE;

export { typeOf };
export const isValidElementType = isValidElementType_;
export const isElement = obj => typeOf(obj) === REACT_ELEMENT_TYPE;
export const isFragment = obj => typeOf(obj) === REACT_FRAGMENT_TYPE;
export const isLazy = obj => typeOf(obj) === REACT_LAZY_TYPE;
export const isMemo = obj => typeOf(obj) === REACT_MEMO_TYPE;
export const isPortal = obj => typeOf(obj) === REACT_PORTAL_TYPE;
export const isProfiler = obj => typeOf(obj) === REACT_PROFILER_TYPE;
export const isStrictMode = obj => typeOf(obj) === REACT_STRICT_MODE_TYPE;
export const isSuspense = obj => typeOf(obj) === REACT_SUSPENSE_TYPE;
export const isAsyncMode = () => false; // Deprecated
export const isConcurrentMode = () => false; // Deprecated
export const isContextConsumer = obj => typeOf(obj) === REACT_CONTEXT_TYPE;
export const isContextProvider = obj => typeOf(obj) === REACT_PROVIDER_TYPE;
export const isForwardRef = obj => typeOf(obj) === REACT_FORWARD_REF_TYPE;

// Create ReactIs object for default export
const ReactIs = {
  typeOf,
  isValidElementType,
  isElement: isElement,
  isFragment,
  isLazy,
  isMemo,
  isPortal,
  isProfiler,
  isStrictMode,
  isSuspense,
  isAsyncMode,
  isConcurrentMode,
  isContextConsumer,
  isContextProvider,
  isForwardRef,
  AsyncMode,
  ConcurrentMode,
  ContextConsumer,
  ContextProvider,
  Element,
  ForwardRef,
  Fragment,
  Lazy,
  Memo,
  Portal,
  Profiler,
  StrictMode,
  Suspense
};

// Default export
export default ReactIs;
`;
            }
            
            return null;
          }
      },      
      {
        name: 'fix-object-assign',
        enforce: 'pre',
        transform(code, id) {
          // Handle object-assign module
          if (id.includes('object-assign/index.js')) {
            console.log('Fixing object-assign module');
            
            // Create a direct ESM implementation
            return `
// Direct ESM implementation of object-assign
'use strict';

/* eslint-disable no-unused-vars */
var getOwnPropertySymbols = Object.getOwnPropertySymbols;
var hasOwnProperty = Object.prototype.hasOwnProperty;
var propIsEnumerable = Object.prototype.propertyIsEnumerable;

function toObject(val) {
  if (val === null || val === undefined) {
    throw new TypeError('Object.assign cannot be called with null or undefined');
  }
  return Object(val);
}

function shouldUseNative() {
  try {
    if (!Object.assign) {
      return false;
    }
    // Detect buggy property enumeration order in older V8 versions
    var test1 = new String('abc');
    test1[5] = 'de';
    if (Object.getOwnPropertyNames(test1)[0] === '5') {
      return false;
    }
    // More V8 tests
    var test2 = {};
    for (var i = 0; i < 10; i++) {
      test2['_' + String.fromCharCode(i)] = i;
    }
    var order2 = Object.getOwnPropertyNames(test2).map(function (n) {
      return test2[n];
    });
    if (order2.join('') !== '0123456789') {
      return false;
    }
    // Use a custom check pattern
    var test3 = {};
    'abcdefghijklmnopqrst'.split('').forEach(function (letter) {
      test3[letter] = letter;
    });
    return Object.keys(Object.assign({}, test3)).join('') === 'abcdefghijklmnopqrst';
  } catch (err) {
    return false;
  }
}

// Default implementation
function objectAssign(target, ...sources) {
  var to = toObject(target);
  
  for (var s = 0; s < sources.length; s++) {
    var from = sources[s];
    
    if (from === null || from === undefined) {
      continue;
    }
    
    var keys = Object.keys(Object(from));
    
    for (var k = 0; k < keys.length; k++) {
      var key = keys[k];
      if (hasOwnProperty.call(from, key)) {
        to[key] = from[key];
      }
    }
  }
  
  return to;
}

// Export using native implementation if available, otherwise use polyfill
const assignFn = shouldUseNative() ? Object.assign : objectAssign;

// Export default and named export
export default assignFn;
export const assign = assignFn;
`;
          }
          return null;
        },
        
        // Add resolveId in case someone imports object-assign with ?commonjs-proxy
        resolveId(source, importer) {
          if (importer && importer.includes('object-assign') && importer.includes('?commonjs-proxy')) {
            console.log(`Handling object-assign import: ${source} from ${importer}`);
            return importer.split('?')[0]; // Return the path without the query string
          }
          return null;
        }
      },
      {
        name: 'fix-prop-types',
        enforce: 'pre',
        transform(code, id) {
          if (id.includes('node_modules/prop-types/index.js')) {
            console.log('Fixing prop-types module exports');
            
            return `
// Direct ESM implementation of prop-types

// Factory for prop types
function createPropType(validate) {
  function checkType(isRequired, props, propName, componentName, location, propFullName) {
    const componentNameSafe = componentName || 'ANONYMOUS';
    const propFullNameSafe = propFullName || propName;
    
    if (props[propName] == null) {
      if (isRequired) {
        return new Error(
          'Required ' + location + ' \`' + propFullNameSafe + '\` was not specified in \`' +
          componentNameSafe + '\`.'
        );
      }
      return null;
    } else {
      return validate(props, propName, componentNameSafe, location, propFullNameSafe);
    }
  }
  
  const chainedCheckType = checkType.bind(null, false);
  chainedCheckType.isRequired = checkType.bind(null, true);
  return chainedCheckType;
}

// Create primitive type validators
const PropTypesObj = {
  array: createPropType(() => null),
  bool: createPropType(() => null),
  func: createPropType(() => null),
  number: createPropType(() => null),
  object: createPropType(() => null),
  string: createPropType(() => null),
  symbol: createPropType(() => null),
  any: createPropType(() => null),
  arrayOf: () => createPropType(() => null),
  element: createPropType(() => null),
  elementType: createPropType(() => null),
  instanceOf: () => createPropType(() => null),
  node: createPropType(() => null),
  objectOf: () => createPropType(() => null),
  oneOf: () => createPropType(() => null),
  oneOfType: () => createPropType(() => null),
  shape: () => createPropType(() => null),
  exact: () => createPropType(() => null),
};

// Special exports
PropTypesObj.checkPropTypes = function() { /* Simple implementation */ };
PropTypesObj.PropTypes = PropTypesObj;  // This enables both PropTypes.string and PropTypes.PropTypes.string
PropTypesObj.resetWarningCache = () => {};

// Export named validators
export const array = PropTypesObj.array;
export const bool = PropTypesObj.bool;
export const func = PropTypesObj.func;
export const number = PropTypesObj.number;
export const object = PropTypesObj.object;
export const string = PropTypesObj.string;
export const symbol = PropTypesObj.symbol;
export const any = PropTypesObj.any;
export const arrayOf = PropTypesObj.arrayOf;
export const element = PropTypesObj.element;
export const elementType = PropTypesObj.elementType;
export const instanceOf = PropTypesObj.instanceOf;
export const node = PropTypesObj.node;
export const objectOf = PropTypesObj.objectOf;
export const oneOf = PropTypesObj.oneOf;
export const oneOfType = PropTypesObj.oneOfType;
export const shape = PropTypesObj.shape;
export const exact = PropTypesObj.exact;
export const checkPropTypes = PropTypesObj.checkPropTypes;
export const resetWarningCache = PropTypesObj.resetWarningCache;

// IMPORTANT: Also export PropTypes as a named export (capital P)
export const PropTypes = PropTypesObj;

// Default export
export default PropTypesObj;
`;
          }
          
          return null;
        },
        resolveId(source, importer) {
          if (importer && importer.includes('prop-types') && source.startsWith('./')) {
            // Handle relative imports within prop-types
            const propTypesDir = importer.substring(0, importer.lastIndexOf('/'));
            const absolutePath = resolve(propTypesDir, source);
            console.log(`Redirecting ${source} from ${importer} to ${absolutePath}`);
            return absolutePath;
          }
          return null;
        }
      },
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
    // Add debug information to see which files are being processed
    if (id.includes('react') || id.includes('react-dom')) {
      console.log(`Processing React module in ESM compat: ${id}, mode: ${mode}`);
    }
    
    // Skip processing if the file already has ESM exports
    if (code.includes('export default') || code.includes('export const')) {
      return null;
    }
    
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
        console.log(`Replacing module: ${modulePath} (mode: ${mode})`);
        return replacement;
      }
    }
    
    // For JSX runtime
    if (id.includes('react/jsx-runtime')) {
      console.log('Patching jsx-runtime module');
      
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
{
  name: 'fix-scheduler-module',
  enforce: 'pre',
  load(id) {
    // Handle scheduler module loading
    if (id.endsWith('scheduler/index.js') || id.includes('node_modules/scheduler/index.js')) {
      console.log(`Direct load of scheduler's index.js: ${id}`);
      
      return `
// ESM version of scheduler entry point
export * from './cjs/scheduler.production.min.js';
import * as Scheduler from './cjs/scheduler.production.min.js';
export default Scheduler;
`;
    }
    return null;
  },
  
  transform(code, id) {
    if (id.includes('scheduler/cjs/scheduler.production.min.js')) {
      console.log(`Converting scheduler production file to ESM: ${id}`);
      
      return `
// ESM bridge for CommonJS module - scheduler
const exports = {};
const module = { exports };

${code}

// Export all properties from the exports object
export const unstable_ImmediatePriority = exports.unstable_ImmediatePriority;
export const unstable_UserBlockingPriority = exports.unstable_UserBlockingPriority;
export const unstable_NormalPriority = exports.unstable_NormalPriority;
export const unstable_LowPriority = exports.unstable_LowPriority;
export const unstable_IdlePriority = exports.unstable_IdlePriority;
export const unstable_runWithPriority = exports.unstable_runWithPriority;
export const unstable_next = exports.unstable_next;
export const unstable_scheduleCallback = exports.unstable_scheduleCallback;
export const unstable_cancelCallback = exports.unstable_cancelCallback;
export const unstable_wrapCallback = exports.unstable_wrapCallback;
export const unstable_getCurrentPriorityLevel = exports.unstable_getCurrentPriorityLevel;
export const unstable_shouldYield = exports.unstable_shouldYield;
export const unstable_requestPaint = exports.unstable_requestPaint;
export const unstable_continueExecution = exports.unstable_continueExecution;
export const unstable_pauseExecution = exports.unstable_pauseExecution;
export const unstable_getFirstCallbackNode = exports.unstable_getFirstCallbackNode;
export const unstable_Profiling = exports.unstable_Profiling;

// Default export
export default exports;
`;
    }
    
    return null;
  }
},
// Add this to your plugins array
{
  name: 'fix-node-polyfills',
  enforce: 'pre',
  resolveId(source, importer) {
    // Redirect Node.js built-in modules
    if (source === 'util' && importer) {
      console.log(`Redirecting Node.js util module import from ${importer}`);
      // Return a virtual module
      return 'virtual:node-util';
    }
    return null;
  },
  
  load(id) {
    // Provide minimal implementations for Node.js modules
    if (id === 'virtual:node-util') {
      console.log('Providing virtual Node.js util module');
      return `
// Virtual Node.js util module
export const types = {
  isDate: (obj) => Object.prototype.toString.call(obj) === '[object Date]',
  isRegExp: (obj) => Object.prototype.toString.call(obj) === '[object RegExp]',
  isArray: Array.isArray,
  isObject: (obj) => obj !== null && typeof obj === 'object',
  isPrimitive: (val) => val === null || (typeof val !== 'object' && typeof val !== 'function'),
  isBuffer: () => false,
  isFunction: (val) => typeof val === 'function',
  isNumber: (val) => typeof val === 'number',
  isString: (val) => typeof val === 'string',
  isSymbol: (val) => typeof val === 'symbol',
  isUndefined: (val) => val === undefined
};

export default {
  types,
  // Add other util methods as needed
  format: (fmt, ...args) => {
    let i = 0;
    return fmt.replace(/%([a-z%])/g, (match, format) => {
      if (match === '%%') return '%';
      if (i >= args.length) return match;
      const arg = args[i++];
      
      switch (format) {
        case 's': return String(arg);
        case 'd': return Number(arg);
        case 'j': return JSON.stringify(arg);
        default: return match;
      }
    });
  },
  inherits: (ctor, superCtor) => {
    ctor.super_ = superCtor;
    ctor.prototype = Object.create(superCtor.prototype, {
      constructor: {
        value: ctor,
        enumerable: false,
        writable: true,
        configurable: true
      }
    });
  }
};
`;
    }
    
    return null;
  }
},
  {
    name: 'add-require-polyfill',
    transformIndexHtml(html) {
      return html.replace(
        /<head>/,
        `<head>
  <script>
    // Polyfill require for direct script usage
    (function() {
      if (typeof window.require === 'undefined') {
        const modules = {
          'react': window.React,
          'react-dom': window.ReactDOM,
          'scheduler': window.Scheduler || {},
          'util': { 
            types: {
              isDate: (obj) => Object.prototype.toString.call(obj) === '[object Date]',
              isRegExp: (obj) => Object.prototype.toString.call(obj) === '[object RegExp]',
              // Add other type checks as needed
            }
          }
        };
        
        window.require = function(id) {
          if (!modules[id]) {
            console.warn('Module not available in require polyfill:', id);
            return {};
          }
          return modules[id];
        };
      }
    })();
  </script>`
      );
    }
  },
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
        'scheduler-global': resolve(__dirname, 'src/scheduler-global.js'),
      },
      dedupe: ['react', 'react-dom', 'react-is', 'scheduler']
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
          // Fix the manual chunks configuration
          manualChunks: (id) => {
            // Group React and related packages together
            if (id.includes('node_modules/react/') || 
                id.includes('node_modules/react-dom/') ||
                id.includes('node_modules/scheduler/')) {
              return 'vendor-react';
            }
            
            // Group utils together
            if (id.includes('node_modules/prop-types/') || 
                id.includes('node_modules/react-is/') ||
                id.includes('node_modules/object-assign/')) {
              return 'vendor-utils';
            }
            
            // Return undefined for other modules (will be chunked automatically)
            return undefined;
          },
          entryFileNames: 'assets/[name]-[hash].js',
          chunkFileNames: 'assets/[name]-[hash].js',
          assetFileNames: 'assets/[name]-[hash].[ext]',
          intro: `
            // Expose React and ReactDOM globally for legacy scripts
            window.React = await import('react');
            window.ReactDOM = await import('react-dom');
            
            // Import the scheduler module from the bundle
            // Instead of trying to load it from a path
            import('/assets/scheduler-[hash].js')
              .then(module => {
                window.Scheduler = module.default || module;
              })
              .catch(error => {
                console.error('Failed to load scheduler:', error);
                // Fallback implementation
                window.Scheduler = {
                  unstable_ImmediatePriority: 1,
                  // ...rest of the implementation
                };
              });
          `
        },
        input: {
          main: './index.html',
          manage: './manage.html',
          scheduler: './src/scheduler-global.js'
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
