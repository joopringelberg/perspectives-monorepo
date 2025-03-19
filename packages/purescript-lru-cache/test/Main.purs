module Test.Main where

import Control.Monad.Free (Free)
import Control.Monad.ST as ST
import Control.Monad.Trans.Class (lift)
import JS.Iterator.ST (next, iterator)
import Data.Maybe (Maybe(..), isJust)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import LRUCache (calculatedSize, clear, defaultCreateOptions, defaultGetOptions, defaultPeekOptions, delete, dump, entries, find, forEach, get, getRemainingTTL, has, keys, load, newCache, peek, purgeStale, rentries, rkeys, set, size, values, rforEach, pop)
import Prelude (Unit, bind, discard, eq, not, pure, show, ($), (<$>), (>>=))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite


theSuite :: Free TestF Unit
theSuite = suite "LRU-basic" do

  test "set and get" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    mVal <- liftEffect $ get "1" defaultGetOptions c1
    assert "Element added with key should be retrieved by the same key" (eq mVal (Just 1))
    mVal2 <- liftEffect $ get "3" defaultGetOptions c1
    assert "get with key that has not been used with set should return nothing" (eq mVal2 Nothing)
  
  test "size" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    s <- liftEffect (size c1)
    assert "Cache should report size 2 after adding two elements." (eq s 2)

  test "calculatedsize" do
    c1 <- pure (newCache defaultCreateOptions {maxSize = Just 1000, sizeCalculation = Just \s _ -> length s})
    _ <- liftEffect $ set "1" "aap" Nothing c1
    _ <- liftEffect $ set "2" "noot" Nothing c1
    s <- liftEffect (calculatedSize c1)
    assert "Cache should report size 7 after adding 'aap' and 'noot." (eq s 7)

  test "peek" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    mVal <- liftEffect $ peek "1" defaultPeekOptions c1
    assert "Element added with key should be retrieved when peeking with the same key" (eq mVal (Just 1))
    mVal2 <- liftEffect $ peek "2" defaultPeekOptions c1
    assert "peek with key that has not been used with set should return nothing" (eq mVal2 Nothing)

  test "has" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    has1 <- liftEffect $ has "1" {updateAgeOnHas: false} c1
    assert "has should return true on element added before" (eq has1 true)
    has2 <- liftEffect $ has "1" {updateAgeOnHas: false} c1
    assert "has should return false on element never added" (eq has2 true) 

  test "delete" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    deletedP <- liftEffect $ delete "1" c1
    assert "Cache should report 'true' after deleting a recently inserted key into a cache that is not full." deletedP
    deletedP' <- liftEffect $ delete "1" c1
    assert "Cache should report 'false' after deleting a key for the second time." (not deletedP')
    s <- liftEffect (size c1)
    assert "Cache should report size 1 after deleting one of two added elements." (eq s 1)

  test "clear" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ clear c1
    s <- liftEffect (size c1)
    assert "Cache should report size 0 after clearing it." (eq s 0)

  test "keys" do
    c1 <- pure (newCache defaultCreateOptions {ttl = Just 1000, updateAgeOnGet = Just true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    keys' <- liftEffect $ keys c1
    {fst, snd, thrd} <- pure $ ST.run do
      -- At this level of indentation, we are in the ST monadic context.
      iter <- iterator keys'
      fst <- next iter
      snd <- next iter
      thrd <- next iter
      pure {fst, snd, thrd}
    (assert "After first retrieving `1` and then `2`, the iterator returned by `keys` should first return `2`." (eq fst (Just "2")))
    (assert "After first retrieving `1` and then `2`, the iterator returned by `keys` should first return `2` and then `1`." (eq snd (Just "1")))
    (assert "After first retrieving `1` and then `2`, the iterator returned by `keys` should first return `2` and then `1` and then nothing." (eq thrd Nothing))

  test "rkeys" do
    c1 <- pure (newCache defaultCreateOptions {ttl = Just 1000, updateAgeOnGet = Just true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    keys' <- liftEffect $ keys c1
    {fst, snd, thrd} <- pure $ ST.run do
      -- At this level of indentation, we are in the ST monadic context.
      iter <- iterator keys'
      fst <- next iter
      snd <- next iter
      thrd <- next iter
      pure {fst, snd, thrd}
    assert "After first retrieving `1` and then `2`, the iterator returned by `rkeys` should first return `1`." (eq fst (Just "1"))
    assert "After first retrieving `1` and then `2`, the iterator returned by `rkeys` should first return `1` and then `2`." (eq snd (Just "2"))
    assert "After first retrieving `1` and then `2`, the iterator returned by `rkeys` should first return `1` and then `2` and then nothing." (eq thrd Nothing)

  test "values" do
    c1 <- pure (newCache defaultCreateOptions {ttl = Just 1000, updateAgeOnGet = Just true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    values' <- liftEffect $ values c1
    {fst, snd, thrd} <- pure $ ST.run do
      -- At this level of indentation, we are in the ST monadic context.
      iter <- iterator values'
      fst <- next iter
      snd <- next iter
      thrd <- next iter
      pure {fst, snd, thrd}
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should first return 2." (eq fst (Just 2))
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should first return 2 and then 1." (eq snd (Just 1))
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should first return 2 and then 1 and then nothing." (eq thrd Nothing)

  test "rvalues" do
    c1 <- pure (newCache defaultCreateOptions {ttl = Just 1000, updateAgeOnGet = Just true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    keys' <- liftEffect $ keys c1
    {fst, snd, thrd} <- pure $ ST.run do
      -- At this level of indentation, we are in the ST monadic context.
      iter <- iterator keys'
      fst <- next iter
      snd <- next iter
      thrd <- next iter
      pure {fst, snd, thrd}
    assert "After first retrieving `1` and then `2`, the iterator returned by `rvalues` should first return 1." (eq fst (Just "1"))
    assert "After first retrieving `1` and then `2`, the iterator returned by `rvalues` should first return 1 and then 2." (eq snd (Just "2"))
    assert "After first retrieving `1` and then `2`, the iterator returned by `rvalues` should first return 1 and then 2 and then nothing." (eq thrd Nothing)

  test "entries" do
    c1 <- pure (newCache defaultCreateOptions {ttl = Just 1000, updateAgeOnGet = Just true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    entries' <- liftEffect $ entries c1
    {fst, snd, thrd} <- pure $ ST.run do
      -- At this level of indentation, we are in the ST monadic context.
      iter <- iterator entries'
      fst <- next iter
      snd <- next iter
      thrd <- next iter
      pure {fst, snd, thrd}
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should first return (Tuple '2' 2)." (eq fst (Just (Tuple "2" 2)))
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should then return 2 and then (Tuple '1' 1)." (eq snd (Just (Tuple "1" 1)))
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should then return nothing." (eq thrd Nothing)

  test "rentries" do
    c1 <- pure (newCache defaultCreateOptions {ttl = Just 1000, updateAgeOnGet = Just true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    _ <- liftEffect $ get "1" defaultGetOptions c1
    _ <- liftEffect $ get "2" defaultGetOptions c1
    rentries' <- liftEffect $ rentries c1
    {fst, snd, thrd} <- pure $ ST.run do
      -- At this level of indentation, we are in the ST monadic context.
      iter <- iterator rentries'
      fst <- next iter
      snd <- next iter
      thrd <- next iter
      pure {fst, snd, thrd}
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should first return (Tuple '1' 1)." (eq fst (Just (Tuple "1" 1)))
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` then return (Tuple '2' 2)." (eq snd (Just (Tuple "2" 2)))
    assert "After first retrieving `1` and then `2`, the iterator returned by `values` should finally return nothing." (eq thrd Nothing)

  test "find" do
    c1 <- pure (newCache defaultCreateOptions {ttl = Just 1000, updateAgeOnGet = Just true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    mResult <- liftEffect $ find (\v _ _ -> eq v 2) Nothing c1
    assert "Should be able to find an item that was inserted in the cache" (eq mResult (Just 2))

  test "dump and load" do
    c1 <- pure (newCache defaultCreateOptions {ttl = Just 1000, updateAgeOnGet = Just true})
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    contents <- liftEffect $ dump c1
    _ <- liftEffect $ clear c1
    c1' <- liftEffect $ load contents c1
    s <- liftEffect $ size c1
    assert "After dumping, clearing and reloading the cache should have the same number of elements" (eq s 2)
  
  test "purgeStale" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    s <- liftEffect (purgeStale c1)
    assert "PurgeStale should return false on a fresh cache." (not s)

  test "getRemainingTTL" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    s <- liftEffect (getRemainingTTL "1" c1)
    assert "remainingTTL should return Nothing for items in a cache without TTL." (eq s Nothing)

    c2 <- pure (newCache defaultCreateOptions {ttl = Just 1000, updateAgeOnGet = Just true})
    _ <- liftEffect $ set "1" 1 Nothing c2
    s' <- liftEffect (getRemainingTTL "1" c2)
    assert "remainingTTL should return Just for items in a cache without TTL." (isJust s')

  test "forEach" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    liftEffect $ forEach (\a _ _ -> logShow a) c1
    assert "The console should show '2\n1'" true

  test "rforEach" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    _ <- liftEffect $ set "2" 2 Nothing c1
    liftEffect $ rforEach (\a _ _ -> logShow a) c1
    assert "The console should show '1\n2'" true
  
  test "pop" do
    c1 <- pure (newCache defaultCreateOptions)
    _ <- liftEffect $ set "1" 1 Nothing c1
    mVal <- liftEffect $ pop c1
    assert "Cache with at least one memeber should be able to pop it" (eq mVal (Just 1))
    mVal2 <- liftEffect $ pop c1
    assert "Cache without members should pop nothing" (eq mVal2 Nothing)
