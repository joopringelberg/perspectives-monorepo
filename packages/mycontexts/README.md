# MyContexts

Some short hints for developing this project.

### The public directory
The AppImages are only in the public directory.
* *package.json* contains the script **prebuild**. It runs generateManifest.js and this computes the manifest. The manifest is written immediately to the *public* directory. Also, this script updates the build number in *build.json*.
* *vite.config.js* copies *notification-worker.js* to the *public* directory. 
* *vite.config.js* also copies *perspectives-pagedispatcher.js* to the *public* directory, but adds a version number to its name. This was done to mitigate the caching problems caused by Safari during development. This number is also used in the project *perspectives-pageworker*: it loads the page dispatcher using that version number. __To increase the number, edit the vite.config.js file__. NOTICE: you'll have to remove older versions of the page dispatcher manually from the public directory.
* Finally, *vite.config.js* constructs *perspectives-serviceworker.js* from a template and adds the files generated in the *dist* directory to it. It then writes the result to the *public* directory and adds the build number to it.