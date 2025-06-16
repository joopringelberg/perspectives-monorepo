// Import React's original JSX runtime
import * as ReactJSXRuntime from 'react/jsx-runtime';

// Re-export jsx functions from jsx-runtime
export const jsx = ReactJSXRuntime.jsx;
export const jsxs = ReactJSXRuntime.jsxs;
export const Fragment = ReactJSXRuntime.Fragment;

// Create the missing jsxDEV function
export function jsxDEV(type, props, key, isStaticChildren, source, self) {
  return jsx(type, props, key);
}

// Default export for CommonJS compatibility
export default {
  jsx,
  jsxs,
  Fragment,
  jsxDEV
};