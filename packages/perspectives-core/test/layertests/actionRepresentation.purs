module Test.ActionRepresentation where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Foldable (all)
import Foreign (MultipleErrors)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.Action (ActionEffect(..), StartMoment(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Repetition (Duration(..))
import Simple.JSON (readJSON, writeJSON)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Representation.Action" do
  test "StartMoment canonical values round-trip" do
    let moments = [ Immediately, After (Second 2.0), OnceSettled ]
    assert "All canonical StartMoment values should round-trip" (all roundTripsStartMoment moments)

  test "StartMoment rejects legacy shapes" do
    let
      nullValue = readJSON "null" :: Either MultipleErrors StartMoment
      rawDuration = readJSON "{\"constructor\":\"Second\",\"n\":2}" :: Either MultipleErrors StartMoment
    assert "Null should not default to Immediately" (isLeft nullValue)
    assert "A raw Duration should not decode as After" (isLeft rawDuration)

  test "ActionEffect canonical shape round-trips" do
    let
      qfd = SQD ContextKind (DataTypeGetter ContextF) ContextKind True True
      effect = ActionEffect { bindings: [ qfd ], stages: [ qfd ], capturedBindingNames: [ "value" ] }
    assert "The canonical ActionEffect should round-trip" $
      case (readJSON (writeJSON effect) :: Either MultipleErrors ActionEffect) of
        Left _ -> false
        Right parsed -> writeJSON parsed == writeJSON effect

  test "ActionEffect rejects legacy shapes" do
    let
      stagesOnly = readJSON "{\"stages\":[]}" :: Either MultipleErrors ActionEffect
      rawQfd = readJSON (writeJSON qfd) :: Either MultipleErrors ActionEffect
      qfd = SQD ContextKind (DataTypeGetter ContextF) ContextKind True True
    assert "An ActionEffect without bindings and captured names should be rejected" (isLeft stagesOnly)
    assert "A raw QueryFunctionDescription should be rejected" (isLeft rawQfd)

isLeft :: forall a b. Either a b -> Boolean
isLeft (Left _) = true
isLeft (Right _) = false

roundTripsStartMoment :: StartMoment -> Boolean
roundTripsStartMoment moment = case (readJSON (writeJSON moment) :: Either MultipleErrors StartMoment) of
  Left _ -> false
  Right parsed -> writeJSON parsed == writeJSON moment
