import js from '@eslint/js';
import tseslint from 'typescript-eslint';

export default [
  js.configs.recommended,
  ...tseslint.configs.recommended,
  {
    files: ['**/*.js', '**/*.mjs', '.lintstagedrc.js', 'commitlint.config.js'],
    ignores: ['**/node_modules/**', '**/dist/**'],
    languageOptions: {
      ecmaVersion: 'latest',
      sourceType: 'module',
    },
    rules: {
      // You can customize rules here
      'no-unused-vars': 'warn',
    },
  },
  // Service Worker specific configuration
  {
    files: ['**/perspectives-serviceworker.js', '**/service-worker.js', '**/*serviceworker*.js'],
    languageOptions: {
      globals: {
        self: 'readonly',
        caches: 'readonly',
        Response: 'readonly',
        console: 'readonly',
        fetch: 'readonly',
        BroadcastChannel: 'readonly',
        URL: 'readonly',
        clients: 'readonly',
        Promise: 'readonly'
      }
    },
    rules: {
      'no-useless-escape': 'warn', // Downgrade to warning
    }
  }
];