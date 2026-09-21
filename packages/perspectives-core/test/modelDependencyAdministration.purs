module Test.ModelDependencyAdministration where

import Prelude

import Control.Monad.Free (Free)
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Perspectives.DomeinFile (deriveModelDependencies)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: Free TestF Unit
theSuite = suite "Model dependency administration phase 2" do
  test "dependency metadata preserves explicit imported versions" do
    let
      dependencies = deriveModelDependencies
        [ ModelUri "model://example.org#Persons@2.4" ]
        [ ModelUri "model://example.org#abcpersons@2.4" ]
    case dependencies of
      [ dependency ] -> do
        Assert.equal (ModelUri "model://example.org#abcpersons") dependency.modelId
        Assert.equal (Just "2.4") dependency.declaredRequirement
        Assert.equal (Just (ModelUri "model://example.org#abcpersons@2.4")) dependency.resolvedModel
        Assert.equal (Just "2.4") dependency.resolvedVersion
      _ -> Assert.assert "one direct dependency should yield one dependency record" false

  test "dependency metadata records the resolved release for unversioned imports" do
    let
      dependencies = deriveModelDependencies
        [ ModelUri "model://example.org#Persons" ]
        [ ModelUri "model://example.org#abcpersons@2.6" ]
    case dependencies of
      [ dependency ] -> do
        Assert.equal (ModelUri "model://example.org#abcpersons") dependency.modelId
        Assert.equal Nothing dependency.declaredRequirement
        Assert.equal (Just (ModelUri "model://example.org#abcpersons@2.6")) dependency.resolvedModel
        Assert.equal (Just "2.6") dependency.resolvedVersion
      _ -> Assert.assert "one direct dependency should yield one dependency record" false
