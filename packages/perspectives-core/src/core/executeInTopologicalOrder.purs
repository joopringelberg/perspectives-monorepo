module Perspectives.ExecuteInTopologicalOrder where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.Writer (WriterT, lift, runWriterT, tell)
import Data.Array (cons, difference, elemIndex, filter, foldM, length, null)
import Data.Either (Either)
import Data.Maybe (isJust)
import Data.Tuple (Tuple(..))
import Perspectives.Parsing.Messages (PerspectivesError(..), MultiplePerspectivesErrors)

--------------------------------------------------------------------------------------------
-- TOPOLOGICAL SORTING
-- A topological sort (see e.g. https://www.interviewcake.com/concept/java/topological-sort) of graphs.
-- There is a module Graph in Pursuit, but we cannot use it because our package set is too old.
--------------------------------------------------------------------------------------------

type ToSort = Array
type Sorted = Array
type Skipped = Array

-- | The action can return any result type.
-- | The end result is the sorted list of action results.
executeInTopologicalOrder
  :: forall m item label result
   . MonadThrow MultiplePerspectivesErrors m
  => Eq label
  => Show label
  => (item -> label)
  -> (item -> Array label)
  -> ToSort item
  -> (item -> m result)
  -> m (Sorted result)
executeInTopologicalOrder getLabel getDependencies toSort action = evalStateT (executeInTopologicalOrder' toSort [] []) (getLabel <$> toSort)
  where
  executeInTopologicalOrder'
    :: ToSort item
    -> Sorted result
    -> Array label
    -> (StateT (ToSort label) m) (Sorted result)
  executeInTopologicalOrder' toSort' sortedResults sortedLabels = do
    Tuple (Tuple sortedResults' sortedLabels') skipped <- runWriterT (foldM executeInTopologicalOrder'' (Tuple sortedResults sortedLabels) toSort')
    if null skipped
    -- If no items are left to sort, we're done.
    then pure sortedResults'
    else if length sortedLabels == length sortedLabels'
    -- But if our effort did not increase the number of sorted items (that is, sortedItems'), stop.
    then throwError [ (Custom ("Cannot topologically sort these items (are you sure all relevant items are included?): " <> show (getLabel <$> skipped))) ]
    -- Otherwise, rinse and repeat by trying to sort the skipped items!
    else executeInTopologicalOrder' skipped sortedResults' sortedLabels'

  executeInTopologicalOrder''
    :: Tuple (Sorted result) (Array label)
    -> item
    -> WriterT (Skipped item) (StateT (ToSort label) m) (Tuple (Sorted result) (Array label))
  executeInTopologicalOrder'' sorted@(Tuple sortedResults sortedLabels) item = do
    allItems <- get
    if zeroInDegrees allItems sortedLabels
    -- This item has no other dependencies than those that have been handled before, so perform the action on it and tack it onto the sorted items.
    then do
      result <- lift $ lift $ action item
      pure $ Tuple (cons result sortedResults) (cons (getLabel item) sortedLabels)
    -- Some of the dependencies of this item have not been handled before. Push it to the skipped items and return the original sorted items.
    -- Notice that if an item has a dependency that IS NOT in the original set of items to sort, it will never be handled!
    else do
      tell [ item ]
      pure sorted
    where
    -- All of the dependencies are already sorted.
    -- INSTEAD, make sure that the item is not dependent on items that have not yet been sorted!
    zeroInDegrees :: Array label -> Array label -> Boolean
    zeroInDegrees allItems handledLabels = null $ (filter (isJust <<< flip elemIndex allItems) (getDependencies item)) `difference` handledLabels

-- | Sort the items topologically, using a function that gets an items dependencies, depending on item equality.
sortTopologically
  :: forall m item
   . MonadThrow MultiplePerspectivesErrors m
  => Eq item
  => Show item
  => (item -> Array item)
  -> ToSort item
  -> m (Array item)
sortTopologically getDependencies toSort = executeInTopologicalOrder identity getDependencies toSort (pure <<< identity)

-- | Sort the items topologically, using a function that gets an items dependencies and a function that 
-- | returns a label from an item. Labels should be comparable using eq.
sortTopologically_
  :: forall m item label
   . MonadThrow MultiplePerspectivesErrors m
  => Eq label
  => Show label
  => (item -> label)
  -> (item -> Array label)
  -> ToSort item
  -> m (Array item)
sortTopologically_ getLabel getDependencies toSort = executeInTopologicalOrder getLabel getDependencies toSort (pure <<< identity)

-- | As sortTopologically_, but returns an Either for error handling. Useful in non-monadic contexts.
sortTopologicallyEither
  :: forall item label
   . Eq label
  => Show label
  => (item -> label)
  -> (item -> Array label)
  -> Array item
  -> Either MultiplePerspectivesErrors (Array item)
sortTopologicallyEither = sortTopologically_