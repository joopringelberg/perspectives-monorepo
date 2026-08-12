# MyContexts

Some short hints for developing this project.

### The public directory
The AppImages are only in the public directory.
* *package.json* contains the script **manifest**. It runs generateManifest.js and this computes the manifest. The manifest is written immediately to the *public* directory. The script also refreshes an untracked build id in *.build-meta.json*.
* *build.json* is tracked configuration and currently only stores the web root in *buildPath*.
* Local build ids are readable by default: unless you set *BUILD_ID* explicitly, the tooling uses *BUILD_LABEL* (or the current git branch), plus timestamp and git sha.
* *vite.config.js* constructs *perspectives-serviceworker.js* from a template and adds the files generated in the *dist* directory to it. It then writes the result to the *public* directory and adds the build id to it. **Note**: this is what happens on production builds. For development, Vite constructs the same file in the *public* directory - but _only on starting the server_!

### What Is Tracked In `public/`
Treat `public/` as a mixed directory: some files are source assets, others are generated output.

Tracked source assets:
* `AppImages/`
* `vite.svg`
* any hand-maintained translations or static files added directly to `public/`

Generated and therefore untracked:
* `public/perspectives-serviceworker.js`
* `public/perspectives.webmanifest`
* `.build-meta.json`
* `public/assets/perspectives-sharedworker.js`
* `public/assets/perspectives-sharedworker.js.map`

Shared worker sync:
* `pnpm run sharedworker` in `mycontexts` builds the sibling package `perspectives-sharedworker` and copies `dist/perspectives-sharedworker.js` plus its sourcemap into `public/assets/`.
* `dev`, `build`, `build:root` and `build:www` all run that sync step automatically, so `public/assets/` is no longer a hand-maintained copy for the shared worker.

Publish flow:
* `publish:www` always runs `build:www` first.
* `publish:root` always runs `build:root` first.
* This ensures every publish uses a fresh build with the right base path, manifest, service worker and synced shared worker assets.