module Test.Query.ComparisonOperators where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Perspectives.Query.QueryTypes (Domain(..))
import Perspectives.Query.UnsafeCompiler (compareRangeValues)
import Perspectives.Representation.QueryFunction (FunctionName(..))
import Perspectives.Representation.Range (Range(..))
import Test.Unit (TestF, suiteOnly, test)
import Test.Unit.Assert (assert)

theSuite :: Free TestF Unit
theSuite = suiteOnly "Query comparison operators" do
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
