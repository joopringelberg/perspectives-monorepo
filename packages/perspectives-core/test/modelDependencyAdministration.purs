module Test.ModelDependencyAdministration where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Perspectives.DomeinFile (deriveModelDependencies)
import Perspectives.Extern.Couchdb (findDependentModelConflicts, findDirectDependencyVersionConflicts, modelDependencyPropertiesForManifest, planDependencyResolution, planDirectDependencyResolutions, renderDependencyResolutionPlan)
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

  test "direct dependency planning distinguishes install keep and update" do
    let
      dependencies = deriveModelDependencies
        [ ModelUri "model://example.org#Persons@2.4"
        , ModelUri "model://example.org#Orders@1.2"
        , ModelUri "model://example.org#Addresses@1.0"
        ]
        [ ModelUri "model://example.org#abcpersons@2.4"
        , ModelUri "model://example.org#ordersstable@1.2"
        , ModelUri "model://example.org#addressesstable@1.0"
        ]
    case dependencies of
      [ personsDependency, ordersDependency, addressesDependency ] ->
        Assert.equal
          [ { dependencyModelId: "model://example.org#abcpersons"
            , requestedVersionedModel: "model://example.org#abcpersons@2.4"
            , installedVersionedModel: Just "model://example.org#abcpersons@2.3"
            , disposition: "update"
            }
          , { dependencyModelId: "model://example.org#ordersstable"
            , requestedVersionedModel: "model://example.org#ordersstable@1.2"
            , installedVersionedModel: Just "model://example.org#ordersstable@1.2"
            , disposition: "keep"
            }
          , { dependencyModelId: "model://example.org#addressesstable"
            , requestedVersionedModel: "model://example.org#addressesstable@1.0"
            , installedVersionedModel: Nothing
            , disposition: "install"
            }
          ]
          ( planDirectDependencyResolutions
              [ { modelId: "model://example.org#abcpersons"
                , versionedModelUri: "model://example.org#abcpersons@2.3"
                }
              , { modelId: "model://example.org#ordersstable"
                , versionedModelUri: "model://example.org#ordersstable@1.2"
                }
              ]
              [ personsDependency, ordersDependency, addressesDependency ]
          )
      _ -> Assert.assert "three direct dependencies should yield three dependency records" false

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

  test "dependency resolution planning combines actions and blockers" do
    let
      dependencies = deriveModelDependencies
        [ ModelUri "model://example.org#Persons@2.4"
        , ModelUri "model://example.org#Orders@1.2"
        ]
        [ ModelUri "model://example.org#abcpersons@2.4"
        , ModelUri "model://example.org#ordersstable@1.2"
        ]
    case dependencies of
      [ personsDependency, ordersDependency ] -> do
        let
          plan = planDependencyResolution
            "model://example.org#sales@1.0"
            [ { modelId: "model://example.org#abcpersons"
              , versionedModelUri: "model://example.org#abcpersons@2.3"
              }
            ]
            [ { dependentVersionedModelUri: "model://example.org#crm@5.0"
              , dependency: ordersDependency { modelId = ModelUri "model://example.org#sales", resolvedVersion = Just "0.9" }
              }
            ]
            [ personsDependency, ordersDependency ]
        Assert.equal
          [ { dependencyModelId: "model://example.org#abcpersons"
            , requestedVersionedModel: "model://example.org#abcpersons@2.4"
            , installedVersionedModel: Just "model://example.org#abcpersons@2.3"
            , disposition: "update"
            }
          , { dependencyModelId: "model://example.org#ordersstable"
            , requestedVersionedModel: "model://example.org#ordersstable@1.2"
            , installedVersionedModel: Nothing
            , disposition: "install"
            }
          ]
          plan.directDependencies
        Assert.equal
          [ { dependencyModelId: "model://example.org#abcpersons"
            , expectedVersionedModel: "model://example.org#abcpersons@2.4"
            , installedVersionedModel: "model://example.org#abcpersons@2.3"
            }
          ]
          plan.directConflicts
        Assert.equal
          [ { dependentVersionedModelUri: "model://example.org#crm@5.0"
            , requiredVersionedModel: "model://example.org#sales@0.9"
            }
          ]
          plan.dependentConflicts
        Assert.assert "rendered plan should mention requested direct dependency planning output"
          (renderDependencyResolutionPlan plan /= "")
      _ -> Assert.assert "two direct dependencies should yield two dependency records" false
