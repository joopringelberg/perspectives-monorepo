module Test.Parsing.Arc.PhaseThree.SetAffectedContextCalculations where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (any, filter, length)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..))
import Perspectives.InvertedQuery.Storable (StoredQueries)
import Perspectives.InvertedQueryKey (RunTimeInvertedQueryKey(..), deserializeInvertedQueryKey)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), queryFunction, range)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), StateIdentifier(..))
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
          ( ( countBindingKeys (EnumeratedRoleType "model://test.local#Test$TestCase1$ARole") (ContextType "model://test.local#Test$TestCase1") storedQueries
                + countContextKeys (EnumeratedRoleType "model://test.local#Test$TestCase1$ARole") storedQueries
            ) == 1
          )
    )

  test "Constant condition: RoleName >> PropName"
    ( withStoredQueries \storedQueries ->
        liftAff $ assert "There should be two RTContextKey entries on ARole"
          (countContextKeys (EnumeratedRoleType "model://test.local#Test$TestCase2$ARole") storedQueries == 2)
    )

  test "Nested context condition: RoleName >> binding >> PropName"
    ( withStoredQueries \storedQueries ->
        liftAff $ assert "There should be a single binding-triggered query key on SubCase1$External"
          (countBindingKeys (EnumeratedRoleType "model://test.local#Test$TestCase3$SubCase1$External") (ContextType "model://test.local#Test$TestCase3$SubCase1") storedQueries == 1)
    )

  test "Nested context condition: RoleName >> binding >> context >> RoleName >> PropName"
    ( withStoredQueries \storedQueries -> do
        liftAff $ assert "There should be a single RTContextKey entry on SubCase2$SubCaseRole1"
          (countContextKeys (EnumeratedRoleType "model://test.local#Test$TestCase4$SubCase2$SubCaseRole1") storedQueries == 1)
        liftAff $ assert "There should be a single binding-triggered query key on SubCase2$External"
          (countBindingKeys (EnumeratedRoleType "model://test.local#Test$TestCase4$SubCase2$External") (ContextType "model://test.local#Test$TestCase4$SubCase2") storedQueries == 1)
        liftAff $ assert "There should be a single RTContextKey entry on NestedContext"
          (countContextKeys (EnumeratedRoleType "model://test.local#Test$TestCase4$NestedContext") storedQueries == 1)
    )

  test "On the external role of the current context: extern >> PropName"
    ( withStoredQueries \storedQueries ->
        liftAff $ assert "There should be a single RTContextKey entry on SubCase3$External"
          (countContextKeys (EnumeratedRoleType "model://test.local#Test$TestCase5$SubCase3$External") storedQueries == 1)
    )

  test "On a role of the enclosing context: extern >> binder XX >> context >> RoleName >> PropName"
    ( withStoredQueries \storedQueries ->
        liftAff $ assert "There should be a single RTContextKey entry on AnotherRole"
          (countContextKeys (EnumeratedRoleType "model://test.local#Test$TestCase6$AnotherRole") storedQueries == 1)
    )

  test "A filter criterion through origin returns directly to the state context"
    ( withStoredQueries \storedQueries ->
        liftAff $ assert "The WantedName inverse should reach TestCase7 without applying the Candidates filter"
          (hasOriginPropertyPath storedQueries)
    )

  test "An origin operand is distinct from filter input when their domains are equal"
    ( withStoredQueries \storedQueries -> do
        liftAff $ assert "The origin CandidateName inverse should reach the candidate directly without applying the filter"
          (hasSameDomainOriginPropertyPath storedQueries)
    )

  test "An origin operand bypasses a navigated filter source"
    ( withStoredQueries \storedQueries -> do
        liftAff $ assert "Every origin ModelName inverse should reach TestCase9 rather than its external role"
          (allOriginPropertyPathsReachContext storedQueries)
        liftAff $ assert "Every inverse for the context state should return TestCase9"
          (allStatePathsReachContext storedQueries)
        liftAff $ assert "Origin-dependent criteria should not be embedded as runtime filters"
          (not $ anyStatePathContainsFilter storedQueries)
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

hasOriginPropertyPath :: StoredQueries -> Boolean
hasOriginPropertyPath = any
  ( \{ queryType, keys, query: InvertedQuery { description: ZQ backward _ } } ->
      queryType == "RTPropertyKey"
        && any isWantedNameKey keys
        && case backward of
          Just qfd ->
            range qfd == CDOM (UET testCase7)
              && (queryFunction <$> composition2path qfd) ==
                [ Value2Role (ENP wantedName)
                , DataTypeGetter ContextF
                ]
          _ -> false
  )
  where
  testCase7 = ContextType "model://test.local#Test$TestCase7"
  externalRole = EnumeratedRoleType "model://test.local#Test$TestCase7$External"
  wantedName = EnumeratedPropertyType "model://test.local#Test$TestCase7$External$WantedName"

  isWantedNameKey key = case deserializeInvertedQueryKey "RTPropertyKey" key of
    Just (RTPropertyKey { property, role }) -> property == wantedName && role == externalRole
    _ -> false

composition2path :: QueryFunctionDescription -> Array QueryFunctionDescription
composition2path (BQD _ (BinaryCombinator ComposeF) left right _ _ _) = [ left ] <> composition2path right
composition2path qfd = [ qfd ]

hasSameDomainOriginPropertyPath :: StoredQueries -> Boolean
hasSameDomainOriginPropertyPath = any
  ( \{ queryType, keys, query: InvertedQuery { description: ZQ backward _ } } ->
      queryType == "RTPropertyKey"
        && any isCandidateNameKey keys
        && case backward of
          Just qfd ->
            let
              path = queryFunction <$> composition2path qfd
            in
              any (_ == Value2Role (ENP candidateName)) path
                && not (any isFilterOrSourceNavigation path)
          _ -> false
  )
  where
  candidates = EnumeratedRoleType "model://test.local#Test$TestCase8$Candidates"
  candidateName = EnumeratedPropertyType "model://test.local#Test$TestCase8$Candidates$CandidateName"

  isCandidateNameKey key = case deserializeInvertedQueryKey "RTPropertyKey" key of
    Just (RTPropertyKey { property, role }) -> property == candidateName && role == candidates
    _ -> false

  isFilterOrSourceNavigation FilterF = true
  isFilterOrSourceNavigation (DataTypeGetter ContextF) = true
  isFilterOrSourceNavigation (RolGetter _) = true
  isFilterOrSourceNavigation _ = false

allOriginPropertyPathsReachContext :: StoredQueries -> Boolean
allOriginPropertyPathsReachContext storedQueries =
  let
    matchingQueries = filter
      ( \{ queryType, keys } ->
          queryType == "RTPropertyKey" && any isModelNameKey keys
      )
      storedQueries
  in
    length matchingQueries > 0
      && foldl
        ( \allReachContext { query: InvertedQuery { description: ZQ backward _ } } ->
            allReachContext && maybe false ((_ == CDOM (UET testCase9)) <<< range) backward
        )
        true
        matchingQueries
  where
  testCase9 = ContextType "model://test.local#Test$TestCase9"
  externalRole = EnumeratedRoleType "model://test.local#Test$TestCase9$External"
  modelName = EnumeratedPropertyType "model://test.local#Test$TestCase9$External$ModelName"

  isModelNameKey key = case deserializeInvertedQueryKey "RTPropertyKey" key of
    Just (RTPropertyKey { property, role }) -> property == modelName && role == externalRole
    _ -> false

allStatePathsReachContext :: StoredQueries -> Boolean
allStatePathsReachContext storedQueries =
  let
    state = StateIdentifier "model://test.local#Test$TestCase9$SomeState"
    matchingQueries = queriesForState state storedQueries
  in
    length matchingQueries > 0
      && foldl
        ( \allReachContext { query: InvertedQuery { description: ZQ backward _ } } ->
            allReachContext && maybe false ((_ == CDOM (UET testCase9)) <<< range) backward
        )
        true
        matchingQueries
  where
  testCase9 = ContextType "model://test.local#Test$TestCase9"

queriesForState :: StateIdentifier -> StoredQueries -> StoredQueries
queriesForState state = filter (\{ query: InvertedQuery { states } } -> any (_ == state) states)

anyStatePathContainsFilter :: StoredQueries -> Boolean
anyStatePathContainsFilter =
  any
    ( \{ query: InvertedQuery { description: ZQ backward _ } } ->
        maybe false (any ((_ == FilterF) <<< queryFunction) <<< composition2path) backward
    )
    <<< queriesForState (StateIdentifier "model://test.local#Test$TestCase9$SomeState")

countMatchingKeys :: String -> (RunTimeInvertedQueryKey -> Boolean) -> StoredQueries -> Int
countMatchingKeys expectedType matches = foldl
  ( \n { queryType, keys } ->
      if queryType == expectedType then n + length (filter (\key -> maybe false matches (deserializeInvertedQueryKey queryType key)) keys)
      else n
  )
  0
