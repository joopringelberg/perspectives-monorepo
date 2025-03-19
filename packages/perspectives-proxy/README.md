# perspectives-proxy
Exposes perspectives-core either through TCP or a channel based on coroutines for apps with integrated core. This package is used exclusively in:
* [perspectives-core](https://github.com/joopringelberg/perspectives-core)
* [perspectives-react](https://github.com/joopringelberg/perspectives-react)

## Installation
Install with npm:

```
$ npm install perspectives-proxy
```

## Usage
The purescript module `perspectivesAPI` (part of perspectives-core) uses the exported function `createRequestEmitterImpl` to set up a coroutine `Producer` for requests. This channels requests from a GUI that runs in the same process to the core. Applying this function resolves the promise InternalChannelPromise. This promise, in turn, is used on creating a proxy for the PDR.

The javascript package perspectives-react imports perspectives-proxy and uses the `PDRproxy` promise.

The package perspectives-react-integrated-client evaluates the statement `configurePDRproxy( "internalChannel" )` to fulfil the promise `PDRproxy`. This promise, in turn, is imported by the modules in the package perspectives-react, used in the client package. This client runs in Electron and runs the PDR in the renderer process (i.e. on the web page).

In contrast, the package inplace (MyContexts) evaluates the statement `configurePDRproxy( "sharedWorkerChannel" )` to fulfil the promise `PDRproxy`. This client runs in the browser and deploys the PDR in a service worker.

We have Webpack treat all `perspectives-proxy` dependencies as `externals`, so only MyContexts bundles it. 

## Module form
We now have WebPack bundle the source file and make it an UMD module. This works, as it creates only a single instance that is shared. 
An experiment using export statements and making the source the package's main value failed.

## Build
Create `dist/perspectives-proxy.js` by evaluating on the command line:

```
$ npm run build
```
This is equivalent to:
```
$ npx webpack
```
## Watch
Have Webpack watch the sources and update `dist/perspectives-proxy.js` by evaluating on the command line:

```
$ npm run watch
```
This is equivalent to:
```
$ npx webpack --watch
```
