import globals from "globals";
import pluginJs from "@eslint/js";
import tseslint from "typescript-eslint";
import pluginReact from "eslint-plugin-react";


/** @type {import('eslint').Linter.Config[]} */
export default [
  { 
    ignores: ["dist/**", "public/**"] 
  },
  { files: ["**/*.{js,mjs,cjs,ts,jsx,tsx}"] },
  { languageOptions: { globals: globals.browser } },
  {
    files: ["src/buildMeta.js", "src/generateManifest.js", "src/syncPublicAssets.js"],
    languageOptions: {
      globals: {
        ...globals.node,
        console: "readonly",
      },
    },
  },
  {
    files: ["src/perspectives-serviceworker.js"],
    languageOptions: {
      globals: {
        ...globals.serviceworker,
        clients: "readonly",
        BroadcastChannel: "readonly",
        console: "readonly",
      },
    },
  },
  pluginJs.configs.recommended,
  ...tseslint.configs.recommended,
  pluginReact.configs.flat.recommended,
];