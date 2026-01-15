# MyContexts

Some short hints for developing this project.

### The public directory
The AppImages are only in the public directory.
* *package.json* contains the script **prebuild**. It runs generateManifest.js and this computes the manifest. The manifest is written immediately to the *public* directory. Also, this script updates the build number in *build.json*.
* *vite.config.js* constructs *perspectives-serviceworker.js* from a template and adds the files generated in the *dist* directory to it. It then writes the result to the *public* directory and adds the build number to it. **Note**: this is what happens on production builds. For development, Vite constructs the same file in the *public* directory - but _only on starting the server_!