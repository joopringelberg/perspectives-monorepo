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
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Test.Unit (TestF, suite, suiteOnly, test)
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
