module Test.Query.ComparisonOperators where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Writer (runWriterT)
import Data.Array (foldl, uncons)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Effect (Effect)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Query.Interpreter (interpret, toString)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), singletonPath)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Query.UnsafeCompiler (compareRangeValues, compileFunction, compileSequenceFunction)
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, suiteOnly, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: Free TestF Unit
theSuite = suite "Query comparison operators" do
  for_
    [ { name: "overlap", left: [ "a", "b", "c" ], right: [ "b", "d" ], expected: [ "a", "c" ] }
    , { name: "reversed operands", left: [ "b", "d" ], right: [ "a", "b", "c" ], expected: [ "d" ] }
    , { name: "empty left", left: [], right: [ "a" ], expected: [] }
    , { name: "empty right", left: [ "a", "b" ], right: [], expected: [ "a", "b" ] }
    , { name: "both empty", left: [], right: [], expected: [] }
    , { name: "identical operands", left: [ "a", "b" ], right: [ "a", "b" ], expected: [] }
    , { name: "disjoint operands", left: [ "a" ], right: [ "b" ], expected: [ "a" ] }
    ]
    \{ name, left, right, expected } -> test ("Difference: " <> name) do
      result <- runP do
        let query = BQD valueDomain (BinaryCombinator DifferenceF) (valueSet left) (valueSet right) valueDomain False False
        compiled <- compileFunction query
        actual <- fst <$> runWriterT (runArrayT $ compiled "source")
        paths <- fst <$> runWriterT (runArrayT $ interpret query (singletonPath $ V "" (Value "source")))
        pure { actual, interpreted: toString <$> paths }
      equal expected result.actual
      equal expected result.interpreted

  test "numeric equality compares parsed numbers" do
    result <- compareRangeValues (VDOM PNumber Nothing) EqualsF "2.0" "2"
    assert "Expected numeric equality to treat 2.0 and 2 as equal." result

  test "numeric inequality compares parsed numbers" do
    result <- compareRangeValues (VDOM PNumber Nothing) NotEqualsF "2.0" "2"
    assert "Expected numeric inequality to treat 2.0 and 2 as equal." (not result)

  test "numeric ordering compares parsed numbers" do
    result <- compareRangeValues (VDOM PNumber Nothing) LessThanF "1.0" "2"
    assert "Expected numeric ordering to compare parsed numbers." result

  test "string equality still compares string representations" do
    result <- compareRangeValues (VDOM PString Nothing) EqualsF "2.0" "2"
    assert "Expected string equality to preserve string semantics." (not result)

  for_ [ MinimumF, MaximumF ] \operator ->
    for_ [ PDate, PDateTime ] \range ->
      test (show operator <> " compares " <> show range <> " chronologically") do
        results <- runP do
          aggregate <- compileSequenceFunction
            (SQD (VDOM range Nothing) (UnaryCombinator operator) (VDOM range Nothing) True Unknown)
          traverse (map fst <<< runWriterT <<< runArrayT <<< aggregate)
            [ [ "946684800000", "1009843200000", "-315619200000" ]
            , []
            , [ "946684800000" ]
            ]
        equal
          [ [ if operator == MinimumF then "-315619200000.0" else "1009843200000.0" ]
          , []
          , [ "946684800000.0" ]
          ]
          results

valueDomain :: Domain
valueDomain = VDOM PString Nothing

valueSet :: Array String -> QueryFunctionDescription
valueSet values = case uncons values of
  Nothing -> UQD valueDomain FilterF
    (SQD valueDomain (Constant PBool "false") (VDOM PBool Nothing) True True)
    valueDomain
    False
    False
  Just { head, tail } -> foldl
    (\left value -> BQD valueDomain (BinaryCombinator UnionF) left (constant value) valueDomain False False)
    (constant head)
    tail
  where
  constant value = SQD valueDomain (Constant PString value) valueDomain True True
