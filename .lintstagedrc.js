export default {
  // Lint TS and JS source and test files under packages on commit
  "packages/**/src/**/*.{ts,tsx,js,jsx}": [
    "eslint --fix --config ./eslint.config.js",
  ],
  "packages/**/test/**/*.{ts,tsx,js,jsx}": [
    "eslint --fix --config ./eslint.config.js",
  ],
};