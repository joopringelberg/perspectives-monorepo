module Test.ModelDependencyAdministration where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Perspectives.DomeinFile (deriveModelDependencies)
import Perspectives.Extern.Couchdb (findDependentModelConflicts, findDirectDependencyVersionConflicts, modelDependencyPropertiesForManifest)
import Perspectives.ModelDependencies as DEP
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
        Assert.equal (Just "2.6") dependency.resolvedVersion
      _ -> Assert.assert "one direct dependency should yield one dependency record" false

  test "manifest dependency properties omit ResolvedModel" do
    let
      dependencies = deriveModelDependencies
        [ ModelUri "model://example.org#Persons@2.4" ]
        [ ModelUri "model://example.org#abcpersons@2.4" ]
    case dependencies of
      [ dependency ] ->
        Assert.equal
          [ Tuple DEP.modelDependencyModelId [ "model://example.org#abcpersons" ]
          , Tuple DEP.modelDependencyDeclaredRequirement [ "2.4" ]
          , Tuple DEP.modelDependencyResolvedVersion [ "2.4" ]
          ]
          (modelDependencyPropertiesForManifest dependency)
      _ -> Assert.assert "one direct dependency should yield one dependency record" false

  test "direct dependency conflicts are detected against installed versions" do
    let
      dependencies = deriveModelDependencies
        [ ModelUri "model://example.org#Persons@2.4" ]
        [ ModelUri "model://example.org#abcpersons@2.4" ]
    case dependencies of
      [ dependency ] ->
        Assert.equal
          [ { dependencyModelId: "model://example.org#abcpersons"
            , expectedVersionedModel: "model://example.org#abcpersons@2.4"
            , installedVersionedModel: "model://example.org#abcpersons@2.3"
            }
          ]
          ( findDirectDependencyVersionConflicts
              [ { modelId: "model://example.org#abcpersons"
                , versionedModelUri: "model://example.org#abcpersons@2.3"
                }
              ]
              [ dependency ]
          )
      _ -> Assert.assert "one direct dependency should yield one dependency record" false

  test "dependent conflicts are detected from installed manifest requirements" do
    let
      dependencies = deriveModelDependencies
        [ ModelUri "model://example.org#Persons@2.4" ]
        [ ModelUri "model://example.org#abcpersons@2.4" ]
    case dependencies of
      [ dependency ] ->
        Assert.equal
          [ { dependentVersionedModelUri: "model://example.org#sales@1.0"
            , requiredVersionedModel: "model://example.org#abcpersons@2.4"
            }
          ]
          ( findDependentModelConflicts
              "model://example.org#abcpersons"
              "model://example.org#abcpersons@2.6"
              [ { dependentVersionedModelUri: "model://example.org#sales@1.0"
                , dependency
                }
              ]
          )
      _ -> Assert.assert "one direct dependency should yield one dependency record" false
