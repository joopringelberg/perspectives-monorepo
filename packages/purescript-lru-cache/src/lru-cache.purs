-- | A Purescript wrapper for Least Recently Used Caches in Javascript, by 
-- | https://github.com/isaacs
-- | These are fixed size caches (either in terms of their number of items or in terms of memory size).
-- | The name derives from the eviction policy: elements used least recently are 
-- | evicted from the cache when a new item is added to a full cache.
-- | 
-- | This library does not support:
-- |    * keys other than String;
-- |    * the fetch method;
-- |    * reading cache options;
-- |    * the thisp object as context for the forEach function.
-- | 
-- | See https://github.com/isaacs/node-lru-cache

module LRUCache
  ( Cache
  , CreateOptions
  , DisposeFunction
  , Key
  , Reason
  , SetOptions
  , SizeCalculation
  , calculatedSize
  , clear
  , defaultCreateOptions
  , defaultGetOptions
  , defaultPeekOptions
  , delete
  , dump
  , entries
  , find
  , forEach
  , get
  , getRemainingTTL
  , has
  , keys
  , load
  , logDisposal
  , makeDisposeFunctionFromEffect
  , newCache
  , peek
  , pop
  , purgeStale
  , rentries
  , rforEach
  , rkeys
  , rvalues
  , set
  , size
  , values
  ) where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import JS.Iterable (Iterable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, EffectFn5, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4, runEffectFn5)
import Foreign (Foreign)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

-------------------------------------------------------------------------------
---- CACHE
-------------------------------------------------------------------------------
-- | The type of Caches holding a particular type of element.
-- | Keys to the Cache must be Strings (lru-cache implements object keys, too, 
-- | but we do not support them in this library).)
foreign import data Cache ∷ Type → Type

type role Cache nominal

foreign import newCache_ :: forall a. Fn1 Foreign (Cache a)

-- | Create a new cache.
newCache :: forall a. CreateOptions a -> Cache a
newCache options =
  let
    (opts :: CreateOptions') = options
      { sizeCalculation = unsafeCoerce $ uncurry2_ <$> options.sizeCalculation
      , dispose = unsafeCoerce $ uncurry3_ <$> options.dispose
      , disposeAfter = unsafeCoerce $ uncurry2_ <$> options.disposeAfter
      }
  in
    runFn1 newCache_ (write opts)

-------------------------------------------------------------------------------
---- OPTIONS
-------------------------------------------------------------------------------
-- | CreateOptions to create a Cache with. See https://github.com/isaacs/node-lru-cache#options
-- | for details.
type CreateOptions a =
  { max :: Maybe Int
  , maxSize :: Maybe Int
  , sizeCalculation :: Maybe (SizeCalculation a)
  -- fetchMethod is not supported
  , dispose :: Maybe (DisposeFunction a)
  , disposeAfter :: Maybe (DisposeFunction a)
  , noDisposeOnSet :: Maybe Boolean
  , ttl :: Maybe Int
  , noUpdateTTL :: Maybe Boolean
  , ttlResolution :: Maybe Int
  , ttlAutopurge :: Maybe Boolean
  , allowStale :: Maybe Boolean
  , updateAgeOnGet :: Maybe Boolean
  , updateAgeOnHas :: Maybe Boolean
  }

-- | At least one of 'max', 'ttl', or 'maxSize' is required, to prevent
-- | unsafe unbounded storage.
-- | In most cases, it's best to specify a max for performance, so all
-- | the required memory allocation is done up-front.
defaultCreateOptions :: forall a. CreateOptions a
defaultCreateOptions =
  { max: Just 2500
  , maxSize: Nothing
  , sizeCalculation: Nothing
  , dispose: Nothing
  , disposeAfter: Nothing
  , noDisposeOnSet: Nothing
  , ttl: Nothing
  , noUpdateTTL: Nothing
  , ttlResolution: Nothing
  , ttlAutopurge: Nothing
  , allowStale: Nothing
  , updateAgeOnGet: Nothing
  , updateAgeOnHas: Nothing
  }

-- | Function to calculate the size of items.
type SizeCalculation a = a -> Key -> Int

type Reason = String
type DisposeFunction a = a -> Key -> Reason -> Unit

makeDisposeFunctionFromEffect :: forall a. (a -> Key -> Reason -> Effect Unit) -> DisposeFunction a
makeDisposeFunctionFromEffect f a b c = runEffect (f a b c)

runEffect :: forall r. Effect r -> r
runEffect = runEffect_

foreign import runEffect_ :: forall r. Effect r -> r

foreign import uncurry2_ :: forall a b c. (a -> b -> c) -> EffectFn2 a b c
foreign import uncurry3_ :: forall a b c d. (a -> b -> c -> d) -> EffectFn3 a b c d

-- 
type CreateOptions' =
  { max :: Maybe Int
  , maxSize :: Maybe Int
  , sizeCalculation :: Maybe Foreign
  -- fetchMethod is not supported
  , dispose :: Maybe Foreign
  , disposeAfter :: Maybe Foreign
  , noDisposeOnSet :: Maybe Boolean
  , ttl :: Maybe Int
  , noUpdateTTL :: Maybe Boolean
  , ttlResolution :: Maybe Int
  , ttlAutopurge :: Maybe Boolean
  , allowStale :: Maybe Boolean
  , updateAgeOnGet :: Maybe Boolean
  , updateAgeOnHas :: Maybe Boolean
  }

type Key = String

-------------------------------------------------------------------------------
---- LOGGING DISPOSAL
-------------------------------------------------------------------------------
foreign import logDisposal :: forall a. DisposeFunction a

-------------------------------------------------------------------------------
---- SET
-------------------------------------------------------------------------------
-- | Add a value to the cache.
-- | Optional options object may contain ttl and sizeCalculation as described above, which default to the settings on the cache object.
-- | Options object my also include size, which will prevent calling the sizeCalculation function and just use the specified number if it is a positive integer, and noDisposeOnSet which will prevent calling a dispose function in the case of overwrites.
-- | Will update the recency of the entry.
set :: forall a. Key -> a -> Maybe (SetOptions a) -> Cache a -> Effect (Cache a)
set key a mOptions cache = case mOptions of
  Nothing -> runEffectFn3 set__ key a cache
  Just options -> runEffectFn4 set_ key a cache options

foreign import set_ :: forall a. EffectFn4 Key a (Cache a) (SetOptions a) (Cache a)
foreign import set__ :: forall a. EffectFn3 Key a (Cache a) (Cache a)

-- | Options to be provided to the set function.
type SetOptions a =
  { size :: Maybe Int
  , sizeCalculation :: Maybe (SizeCalculation a)
  , ttl :: Maybe Int
  , noDisposeOnSet :: Boolean
  }

-------------------------------------------------------------------------------
---- GET
-------------------------------------------------------------------------------
-- | Return a value from the cache.
-- | Will update the recency of the cache entry found.
get :: forall a. Key -> GetOptions -> Cache a -> Effect (Maybe a)
get key options cache = runEffectFn5 get_ key (unsafeCoerce (write options)) Nothing Just cache

foreign import get_ :: forall a m. EffectFn5 Key Foreign m (a -> m) (Cache a) m

type GetOptions = { updateAgeOnGet :: Maybe Boolean, allowStale :: Maybe Boolean }

defaultGetOptions :: GetOptions
defaultGetOptions = { updateAgeOnGet: Nothing, allowStale: Nothing }

-------------------------------------------------------------------------------
---- SIZE
-------------------------------------------------------------------------------
-- | The total number of items held in the cache at the current moment.
size :: forall a. Cache a -> Effect Int
size c = runEffectFn1 size_ c

foreign import size_ :: forall a. EffectFn1 (Cache a) Int

-------------------------------------------------------------------------------
---- CALCULATEDSIZE
-------------------------------------------------------------------------------
-- | The total size of items in cache when using size tracking.
calculatedSize :: forall a. Cache a -> Effect Int
calculatedSize c = runEffectFn1 calculatedSize_ c

foreign import calculatedSize_ :: forall a. EffectFn1 (Cache a) Int

-------------------------------------------------------------------------------
---- PEEK
-------------------------------------------------------------------------------
-- | Like get() but doesn't update recency or delete stale items.
-- | Returns Nothing if the item is stale, unless allowStale is set either on the cache or in the options object.
peek :: forall a. Key -> PeekOptions -> Cache a -> Effect (Maybe a)
peek key options cache = runEffectFn5 peek_ key (unsafeCoerce (write options)) Nothing Just cache

foreign import peek_ :: forall a m. EffectFn5 Key Foreign m (a -> m) (Cache a) m

type PeekOptions = { allowStale :: Maybe Boolean }

defaultPeekOptions :: PeekOptions
defaultPeekOptions = { allowStale: Nothing }

-------------------------------------------------------------------------------
---- HAS
-------------------------------------------------------------------------------

-- | Check if a key is in the cache, without updating the recency of use. Age is updated if updateAgeOnHas is set to true in either the options or the constructor.
-- | Will return false if the item is stale, even though it is technically in the cache.
has :: forall a. Key -> { updateAgeOnHas :: Boolean } -> Cache a -> Effect Boolean
has key options cache = runEffectFn3 has_ key options cache

foreign import has_ :: forall a. EffectFn3 Key { updateAgeOnHas :: Boolean } (Cache a) Boolean

-------------------------------------------------------------------------------
---- DELETE
-------------------------------------------------------------------------------
-- | Deletes a key out of the cache.
-- | Returns true if the key was deleted, false otherwise.
delete :: forall a. Key -> Cache a -> Effect Boolean
delete key cache = runEffectFn2 delete_ key cache

foreign import delete_ :: forall a. EffectFn2 Key (Cache a) Boolean

-------------------------------------------------------------------------------
---- CLEAR
-------------------------------------------------------------------------------
-- | Clear the cache entirely, throwing away all values.
clear :: forall a. Cache a -> Effect Unit
clear cache = runEffectFn2 clear_ unit cache

foreign import clear_ :: forall a. EffectFn2 Unit (Cache a) Unit

-------------------------------------------------------------------------------
---- KEYS
-------------------------------------------------------------------------------
-- | Return a generator yielding the keys in the cache, in order from most recently used to least recently used.
keys :: forall a. Cache a -> Effect (Iterable String)
keys cache = runEffectFn1 keys_ cache

foreign import keys_ :: forall a. EffectFn1 (Cache a) (Iterable String)

-------------------------------------------------------------------------------
---- RKEYS
-------------------------------------------------------------------------------
-- | Return a generator yielding the keys in the cache, in order from least recently used to most recently used.
rkeys :: forall a. Cache a -> Effect (Iterable String)
rkeys cache = runEffectFn1 rkeys_ cache

foreign import rkeys_ :: forall a. EffectFn1 (Cache a) (Iterable String)

-------------------------------------------------------------------------------
---- VALUES
-------------------------------------------------------------------------------
-- | Return a generator yielding the values in the cache, in order from most recently used to least recently used.
values :: forall a. Cache a -> Effect (Iterable a)
values cache = runEffectFn1 values_ cache

foreign import values_ :: forall a. EffectFn1 (Cache a) (Iterable a)

-------------------------------------------------------------------------------
---- RVALUES
-------------------------------------------------------------------------------
-- | Return a generator yielding the values in the cache, in order from least recently used to most recently used.
rvalues :: forall a. Cache a -> Effect (Iterable a)
rvalues cache = runEffectFn1 rvalues_ cache

foreign import rvalues_ :: forall a. EffectFn1 (Cache a) (Iterable a)

-------------------------------------------------------------------------------
---- ENTRIES
-------------------------------------------------------------------------------
-- | Return a generator yielding [key, value] pairs, in order from most recently used to least recently used.
entries :: forall a. Cache a -> Effect (Iterable (Tuple Key a))
entries cache = runEffectFn2 entries_ Tuple cache

foreign import entries_ :: forall a x y. EffectFn2 (x -> y -> Tuple x y) (Cache a) (Iterable (Tuple Key a))

-------------------------------------------------------------------------------
---- RENTRIES
-------------------------------------------------------------------------------
-- | Return a generator yielding [key, value] pairs, in order from most recently used to least recently used.
rentries :: forall a. Cache a -> Effect (Iterable (Tuple Key a))
rentries cache = runEffectFn2 rentries_ Tuple cache

foreign import rentries_ :: forall a x y. EffectFn2 (x -> y -> Tuple x y) (Cache a) (Iterable (Tuple Key a))

-------------------------------------------------------------------------------
---- FIND
-------------------------------------------------------------------------------
-- | Find a value for which the supplied fn method returns a truthy value, similar to Array.find().
-- | fn is called as fn(value, key, cache).
find :: forall a. FindFunction a -> Maybe GetOptions -> Cache a -> Effect (Maybe a)
find criterium mOptions cache = case mOptions of
  Nothing -> runEffectFn5 find_ (uncurry3_ criterium) defaultGetOptions Nothing Just cache
  Just options -> runEffectFn5 find_ (uncurry3_ criterium) options Nothing Just cache

type FindFunction a = (a -> Key -> Cache a -> Boolean)

foreign import find_
  :: forall a m
   . EffectFn5
       (EffectFn3 a Key (Cache a) Boolean)
       GetOptions
       m
       (a -> m)
       (Cache a)
       (Maybe a)

-------------------------------------------------------------------------------
---- DUMP
-------------------------------------------------------------------------------
-- | The javascript function returns an array of [key, entry] objects which can be passed to cache.load()
-- | The Purescript version returns an array of Foreign values.
-- | Use this value with the `load` function to create a new Cache.
dump :: forall a. Cache a -> Effect (Array Foreign)
dump cache = runEffectFn1 dump_ cache

foreign import dump_ :: forall a. EffectFn1 (Cache a) (Array Foreign)

-------------------------------------------------------------------------------
---- LOAD
-------------------------------------------------------------------------------
-- | Reset the cache and load in the items in entries in the order listed. Note that the shape of the resulting cache may be different if the same options are not used in both caches.
load :: forall a. Array Foreign -> Cache a -> Effect (Cache a)
load keysAndVals cache = runEffectFn2 load_ keysAndVals cache

foreign import load_ :: forall a. EffectFn2 (Array Foreign) (Cache a) (Cache a)

-------------------------------------------------------------------------------
---- PURGESTALE
-------------------------------------------------------------------------------
-- | Delete any stale entries. Returns true if anything was removed, false otherwise.
purgeStale :: forall a. Cache a -> Effect Boolean
purgeStale cache = runEffectFn1 purgeStale_ cache

foreign import purgeStale_ :: forall a. EffectFn1 (Cache a) Boolean

-------------------------------------------------------------------------------
---- REMAININGTTL
-------------------------------------------------------------------------------
-- | Return the number of ms left in the item's TTL. 
-- | If item is not in cache, returns Just 0. 
-- | Returns Nothing if item is in cache without a defined TTL.
getRemainingTTL :: forall a. Key -> Cache a -> Effect (Maybe Int)
getRemainingTTL key cache = runEffectFn4 getRemainingTTL_ key Nothing Just cache

foreign import getRemainingTTL_ :: forall a m. EffectFn4 Key m (a -> m) (Cache a) (Maybe Int)

-------------------------------------------------------------------------------
---- FOREACH
-------------------------------------------------------------------------------
-- | Call the fn function with each set of fn(value, key, cache) in the LRU cache, from most recent to least recently used.
-- | Does not affect recency of use.
-- | NOTE that this function is of limited usefullness in Purescript. The function that is applied to all cache members and keys
-- | can only sort an effect (in Javascript): it is not possible to create a side effect in a Purescript Monad, which is why 
-- | we do not allow functions in arbitrary monads that are member of MonadEffect.
forEach :: forall a. (a -> Key -> Cache a -> Effect Unit) -> Cache a -> Effect Unit
forEach fn cache = runEffectFn3 forEach_ (unsafeCoerce uncurry3_ fn) unit cache

foreign import forEach_ :: forall a. EffectFn3 Foreign Unit (Cache a) Unit

-------------------------------------------------------------------------------
---- RFOREACH
-------------------------------------------------------------------------------
-- | Same as cache.forEach(fn), but in order from least recently used to most recently used.
rforEach :: forall a. (a -> Key -> Cache a -> Effect Unit) -> Cache a -> Effect Unit
rforEach fn cache = runEffectFn3 rforEach_ (unsafeCoerce uncurry3_ fn) unit cache

foreign import rforEach_ :: forall a. EffectFn3 Foreign Unit (Cache a) Unit

-------------------------------------------------------------------------------
---- POP
-------------------------------------------------------------------------------
-- | Evict the least recently used item, returning its value.
-- | Returns undefined if cache is empty.
pop :: forall a. Cache a -> Effect (Maybe a)
pop cache = runEffectFn3 pop_ Nothing Just cache

foreign import pop_ :: forall a m. EffectFn3 m (a -> m) (Cache a) m
