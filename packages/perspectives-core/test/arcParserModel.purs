module Test.Parsing.Arc.Model where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Node.Process (cwd)
import Parsing (ParseError)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE)
import Perspectives.Parsing.Arc.ContextualVariables (makeTypeTimeOnlyRoleTypeStep)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..), VarBinding(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Query.ExpressionCompiler (contextRoleRangeHasNonExternalRole)
import Perspectives.Query.QueryTypes (RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedRoleType(..), RoleType(..))
import Test.Unit (TestF, suiteOnly, test)
import Test.Unit.Assert (assert)

testDirectory :: String
testDirectory = "test"

modelDirectory :: String
modelDirectory = "../perspectives-core/src/model/"

theSuite :: Free TestF Unit
theSuite = suiteOnly "Perspectives.Parsing.Arc.Model" do

  --------------------------------------------------------------------------------
  ---- PARSE SYSTEM MODEL
  --------------------------------------------------------------------------------

  test "Parse system model" do
    procesDir <- liftEffect cwd
    filePath <- pure (Path.concat [ procesDir, modelDirectory, "fillFromTest.arc" ])
    text <- readTextFile UTF8 filePath
    (r :: Either ParseError ContextE) <- runIndentParser text domain
    case r of
      (Left e) -> assert (show e) false 
      (Right _) -> pure unit

  test "TypeTimeOnlyRoleTypeStep keeps calculated role type" do
    let
      binding = makeTypeTimeOnlyRoleTypeStep
        "currentactor"
        (CR $ CalculatedRoleType "model:MyTestDomain$Guest")
        (ContextType "model:MyTestDomain$SomeContext")
        (ArcPosition { column: 1, line: 1 })
    assert "currentactor should keep the calculated role type in type-time bindings"
      case binding of
        VarBinding "currentactor" (Simple (TypeTimeOnlyCalculatedRole _ "model:MyTestDomain$Guest")) -> true
        _ -> false

  test "contextRoleRangeHasNonExternalRole detects non-external role types" do
    let
      externalRange = ST $ RoleInContext
        { context: ContextType "model:MyDomain$SubContext"
        , role: EnumeratedRoleType "model:MyDomain$SubContext$External"
        }
      nonExternalRange = ST $ RoleInContext
        { context: ContextType "model:MyDomain$SubContext"
        , role: EnumeratedRoleType "model:MyDomain$SubContext$SomeRole"
        }
    assert "External-only role range should be accepted"
      (contextRoleRangeHasNonExternalRole externalRange == false)
    assert "Non-external role range should be rejected"
      (contextRoleRangeHasNonExternalRole nonExternalRange)
