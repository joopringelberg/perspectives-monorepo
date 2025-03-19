# perspectives-pageworker
The library `perspectives-pageWorker` runs the Perspectives Distributed Runtime in an InPlace page. This page is said to *host the PDR*. We use it on browsers that do not support SharedWorkers (viz Safari).

The pageworker connects to the core (run in the same javascript process) via an InternalChannel instance created by the PDR itself. It connects to the client through the [Channel Messaging API](https://developer.mozilla.org/en-US/docs/Web/API/Channel_Messaging_API), where this client runs in a browser window tab.

## Installation
Install with npm:

```
$ npm install perspectives-workers
```

## Usage
The pageworker is set up by a function in the perspectives-proxy package, `configurePDRproxy`, when applied to the argument "sharedWorkerChannel". This function is called by the `inplace` client.


## Build
Create `dist/perspectives-sharedworker.js` by evaluating on the command line:

```
$ npm run build
```
This is equivalent to:
```
$ npx webpack
```
## Watch
Have Webpack watch the sources and update `dist/perspectivespageworker.js` by evaluating on the command line:

```
$ npm run watch
```
This is equivalent to:
```
$ npx webpack --watch
```

## Dependency management
See [Publishing a new version](https://github.com/joopringelberg/perspectives-core/blob/master/technical%20readme.md#publishing-a-new-version) in the Perspectives Core (PDR) project.

