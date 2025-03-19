# purescript-lru-cache
A Purescript wrapper of [lru-cache](https://github.com/isaacs/node-lru-cache).

These are fixed size caches (either in terms of their number of items or in terms of memory size).
The name derives from the eviction policy: elements used least recently are 
evicted from the cache when a new item is added to a full cache.

This library does not support:
   * keys other than String;
   * the fetch method;
   * reading cache options;
   * the thisp object as context for the forEach function.

Documentation will be published with this module on pursuit.

## Publish new package version:
1. In package.json: increase the package number
5. Commit
6. Create tag
7. Push tag
