# serialisable-nonempty-arrays
A wrapper for Data.Array.NonEmpty with a Decode and Encode instance. Also with a ReadForeign and WriteForeign instance.

## Dependency management
See [Publishing a new version](https://github.com/joopringelberg/perspectives-core/blob/master/technical%20readme.md#publishing-a-new-version) in the Perspectives Core (PDR) project.

## Publish new package version:
1. In spago.yaml: update the version of perspectives-utilities at `ref`
2. outcomment the `path` section
3. incomment the `git` and `ref` sections
4. In package.json: increase the package number
5. Commit
6. Create tag
7. Push tag
8. In spago.yaml: switch back to the local version.