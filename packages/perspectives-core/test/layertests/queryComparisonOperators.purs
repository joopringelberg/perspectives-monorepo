module Test.Query.ComparisonOperators where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Writer (runWriterT)
import Data.Array (foldl, uncons)
import Data.Foldable (for_)
import Data.List.NonEmpty (singleton)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Effect (Effect)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Query.Interpreter (interpret, toString)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), singletonPath)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext(..))
import Perspectives.Query.UnsafeCompiler (compareRangeValues, compileFunction, compileSequenceFunction)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Duration_(..), Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Test.Perspectives.Utils (runP)
import Test.BindingReplacement as Binding
import Test.Unit (TestF, suite, suiteOnly, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: Free TestF Unit
theSuite = suite "Query comparison operators" do
  for_
    [ { name: "role", domain: RDOM $ ST $ RoleInContext { context: Binding.contextType, role: Binding.visibleType }, source: R Binding.visibleId }
    , { name: "context", domain: CDOM $ ST Binding.contextType, source: C Binding.contextId }
    ]
    \{ name, domain, source } ->
      for_ [ EqualsF, NotEqualsF ] \operator ->
        test ("interpreted " <> name <> " identity comparison: " <> show operator) do
          result <- runP do
            let
              operand = SQD domain (DataTypeGetter IdentityF) domain True True
              query = BQD domain (BinaryCombinator operator) operand operand (VDOM PBool Nothing) True True
            compiled <- compileFunction query
            actual <- fst <$> runWriterT (runArrayT $ compiled (toString $ singletonPath source))
            paths <- fst <$> runWriterT (runArrayT $ interpret query (singletonPath source))
            pure { actual, interpreted: toString <$> paths }
          equal [ if operator == EqualsF then "true" else "false" ] result.actual
          equal result.actual result.interpreted

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
        let query = BQD valueDomain (BinaryCombinator DifferenceF) (valueSet PString left) (valueSet PString right) valueDomain False False
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
    for_
      [ { range: PDate, values: [ "946684800000", "1009843200000", "-315619200000" ], smallest: "-315619200000.0", largest: "1009843200000.0", single: "946684800000.0" }
      , { range: PDateTime, values: [ "946684800000", "1009843200000", "-315619200000" ], smallest: "-315619200000.0", largest: "1009843200000.0", single: "946684800000.0" }
      , { range: PNumber, values: [ "10", "2", "100" ], smallest: "2.0", largest: "100.0", single: "10.0" }
      , { range: PDuration Day_, values: [ "10", "2", "100" ], smallest: "2.0", largest: "100.0", single: "10.0" }
      , { range: PString, values: [ "b", "a", "c" ], smallest: "a", largest: "c", single: "b" }
      ]
      \{ range, values, smallest, largest, single } -> test (show operator <> " agrees across evaluators for " <> show range) do
        results <- runP do
          let
            domain = VDOM range Nothing
            sequence = SQD domain (UnaryCombinator operator) domain True Unknown
          aggregate <- compileSequenceFunction sequence
          traverse
            ( \input -> do
                actual <- fst <$> runWriterT (runArrayT $ aggregate input)
                let query = BQD valueDomain (BinaryCombinator ComposeSequenceF) (valueSet range input) sequence domain True Unknown
                paths <- fst <$> runWriterT (runArrayT $ interpret query (singletonPath $ V "" (Value "source")))
                pure { actual, paths }
            )
            [ values, [], [ single ] ]
        let expected = [ [ if operator == MinimumF then smallest else largest ], [], [ single ] ]
        equal expected $ _.actual <$> results
        equal expected $ (\result -> toString <$> result.paths) <$> results
        for_ results \{ paths } ->
          for_ paths \path ->
            equal (Just $ singleton path.head) path.mainPath

valueDomain :: Domain
valueDomain = VDOM PString Nothing

valueSet :: Range -> Array String -> QueryFunctionDescription
valueSet range values = case uncons values of
  Nothing -> UQD domain FilterF
    (SQD domain (Constant PBool "false") (VDOM PBool Nothing) True True)
    domain
    False
    False
  Just { head, tail } -> foldl
    (\left value -> BQD domain (BinaryCombinator UnionF) left (constant value) domain False False)
    (constant head)
    tail
  where
  domain = VDOM range Nothing
  constant value = SQD domain (Constant range value) domain True True
