Running testscripts for the PDR
======================

For a high-level discussion of the Node.js/testing architecture, including recommendations for mobile, see:
[`docsources/nodejs-testing-architecture.md`](../docsources/nodejs-testing-architecture.md)

---

## Quick start: running tests with `spago test`

The test suite is launched by `spago test --main Test.Main`.  Most test suites inside
`test/Main.purs` are commented out; uncomment the ones you want to run before invoking
the command.

Pure PureScript suites (no database, no network) — these run without any extra setup:

```bash
cd packages/perspectives-core
pnpm exec spago test --main Test.Main
```

---

## Node.js-compatible PouchDB

The production build uses `pouchdb-browser` (backed by IndexedDB), which does **not**
work in Node.js.  A ready-made Node.js-compatible persistence module is provided:

```
src/core/persistence/persistenceAPI.node.js
```

It uses the `pouchdb` package (LevelDB backend) and removes all browser-only APIs.

### Install the Node.js PouchDB package

```bash
pnpm add --save-dev pouchdb
```

For tests that require no persistence across runs, add the in-memory adapter:

```bash
pnpm add --save-dev pouchdb-adapter-memory
```

### Point the bundler at the Node.js file

When building the test bundle (e.g. with rollup or esbuild), add an alias so that
imports of `persistenceAPI.js` resolve to `persistenceAPI.node.js`:

```js
// rollup alias example
alias({
  entries: [
    { find: './persistenceAPI.js', replacement: './persistenceAPI.node.js' }
  ]
})
```

---

## AffJax (HTTP client)

The production build depends on `affjax-web`, which uses the browser XHR2 API.  For
Node.js tests that need to make real HTTP requests (e.g. loading models from
`perspectives.domains`), switch the spago dependency to `affjax-node`:

```yaml
# spago.yaml — add for Node.js test builds, remove affjax-web
dependencies:
  - affjax-node   # instead of affjax-web
```

If you need cookie support (e.g. for a CouchDB session), use the `xhr2-cookies` fork of
`affjax-web` as described in the original note below.

---

## SSL / Certificate handling

### Disable TLS verification (quick, insecure)

```bash
export NODE_TLS_REJECT_UNAUTHORIZED="0"
```

### Trust the local mkcert CA (recommended)

```bash
export NODE_EXTRA_CA_CERTS="$(mkcert -CAROOT)/rootCA.pem"
```

Without this, connecting to a self-signed endpoint throws:

```
"request to https://perspectives.domains/models_perspectives_domains/ failed,
 reason: unable to verify the first certificate",
 "errno":"UNABLE_TO_VERIFY_LEAF_SIGNATURE"
```

---

## Legacy note: XHR cookies

*(Retained for historical reference.)*

An early fork of Affjax replaced the cookies-agnostic `xhr2` package with `xhr2-cookies`
to support cookie-based CouchDB authentication.  If you need cookies in Node.js tests,
use that fork and edit the two lines in `persistenceAPI.js`:

```js
// var PouchDB = require('pouchdb-browser').default;
var PouchDB = require('pouchdb');
```

For new test code, prefer `persistenceAPI.node.js` (which handles PouchDB switching
correctly) and use token-based CouchDB authentication instead of cookies.

