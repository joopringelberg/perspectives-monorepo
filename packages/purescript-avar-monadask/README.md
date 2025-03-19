# purescript-avar-monadask

AVar in MonadAsk provides shared memory for parallel asynchronous processes.

The module `Control.Monad.AvarMonadAsk` provides an interface similar to `Control.Monad.State.Class`:

*  get
*  gets
*  put
*  modify

However, the state is kept in an AVar that is accessed through the `ask` of MonadAsk. Monads that are member of class `MonadAff` can access this AVar with `get` or `gets`, set its content with `put` and change it using `modify`.

This work was inspired partly by [this discussion](https://github.com/slamdata/purescript-halogen/issues/386) on the interplay between StateT and Aff.

## Publish new package version:
4. In package.json: increase the package number
5. Commit
6. Create tag
7. Push tag
