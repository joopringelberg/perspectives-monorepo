module Test.Parsing.Arc.PhaseThree.SetAffectedContextCalculations where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (filter, length)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.InvertedQuery.Storable (StoredQueries)
import Perspectives.InvertedQueryKey (RunTimeInvertedQueryKey(..), deserializeInvertedQueryKey)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..))
import Perspectives.TypePersistence.LoadArc.FS (loadAndCompileArcFile)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

theSuite :: Free TestF Unit
theSuite = suite "Test.Parsing.Arc.PhaseThree.SetAffectedContextCalculations" do

  test "Constant condition: true"
    ( withStoredQueries \storedQueries ->
        liftAff $ assert "There should be one role-triggered query key on ARole"
          ( ( countBindingKeys (EnumeratedRoleType "model:Test$TestCase1$ARole") (ContextType "model:Test$TestCase1") storedQueries
                + countContextKeys (EnumeratedRoleType "model:Test$TestCase1$ARole") storedQueries
            ) == 1
          )
    )

  test "Constant condition: RoleName >> PropName"
    ( withStoredQueries \storedQueries ->
        liftAff $ assert "There should be two RTContextKey entries on ARole"
          (countContextKeys (EnumeratedRoleType "model:Test$TestCase2$ARole") storedQueries == 2)
    )

  test "Nested context condition: RoleName >> binding >> PropName"
    ( withStoredQueries \storedQueries ->
        liftAff $ assert "There should be a single binding-triggered query key on SubCase1$External"
          (countBindingKeys (EnumeratedRoleType "model:Test$TestCase3$SubCase1$External") (ContextType "model:Test$TestCase3$SubCase1") storedQueries == 1)
    )

  test "Nested context condition: RoleName >> binding >> context >> RoleName >> PropName"
    ( withStoredQueries \storedQueries -> do
        liftAff $ assert "There should be a single RTContextKey entry on SubCase2$SubCaseRole1"
          (countContextKeys (EnumeratedRoleType "model:Test$TestCase4$SubCase2$SubCaseRole1") storedQueries == 1)
        liftAff $ assert "There should be a single binding-triggered query key on SubCase2$External"
          (countBindingKeys (EnumeratedRoleType "model:Test$TestCase4$SubCase2$External") (ContextType "model:Test$TestCase4$SubCase2") storedQueries == 1)
        liftAff $ assert "There should be a single RTContextKey entry on NestedContext"
          (countContextKeys (EnumeratedRoleType "model:Test$TestCase4$NestedContext") storedQueries == 1)
    )

  test "On the external role of the current context: extern >> PropName"
    ( withStoredQueries \storedQueries ->
        liftAff $ assert "There should be a single RTContextKey entry on SubCase3$External"
          (countContextKeys (EnumeratedRoleType "model:Test$TestCase5$SubCase3$External") storedQueries == 1)
    )

  test "On a role of the enclosing context: extern >> binder XX >> context >> RoleName >> PropName"
    ( withStoredQueries \storedQueries ->
        liftAff $ assert "There should be a single RTContextKey entry on AnotherRole"
          (countContextKeys (EnumeratedRoleType "model:Test$TestCase6$AnotherRole") storedQueries == 1)
    )

withStoredQueries :: (StoredQueries -> MonadPerspectives Unit) -> Aff Unit
withStoredQueries assertions =
  runP do
    result <- loadAndCompileArcFile "setAffectedContextCalculations" testDirectory
    case result of
      Left modelErrors -> liftAff $ assert ("There are model errors: " <> show modelErrors) false
      Right (Tuple _ storedQueries) -> assertions storedQueries

countContextKeys :: EnumeratedRoleType -> StoredQueries -> Int
countContextKeys roleType = countMatchingKeys "RTContextKey" case _ of
  RTContextKey { role_origin } -> role_origin == roleType
  _ -> false

countBindingKeys :: EnumeratedRoleType -> ContextType -> StoredQueries -> Int
countBindingKeys roleType contextType storedQueries =
  countMatchingKeys "RTFillerKey"
    ( case _ of
        RTFillerKey { filledRole_origin, filledContext_origin } -> filledRole_origin == roleType && filledContext_origin == contextType
        _ -> false
    )
    storedQueries
    + countMatchingKeys "RTFilledKey"
        ( case _ of
            RTFilledKey { fillerRole_origin, fillerContext_origin } -> fillerRole_origin == roleType && fillerContext_origin == contextType
            _ -> false
        )
        storedQueries

countMatchingKeys :: String -> (RunTimeInvertedQueryKey -> Boolean) -> StoredQueries -> Int
countMatchingKeys expectedType matches = foldl
  ( \n { queryType, keys } ->
      if queryType == expectedType then n + length (filter (\key -> maybe false matches (deserializeInvertedQueryKey queryType key)) keys)
      else n
  )
  0
