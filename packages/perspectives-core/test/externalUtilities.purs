module Test.Perspectives.ExternalUtilities where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Effect (Effect)
import Parsing (ParseError)
import Perspectives.Extern.Utilities (applyModelVersions)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

theSuite :: Free TestF Unit
theSuite = suite "External Utilities" do
  test "applies versions to matching domain and import declarations" do
    let
      source = "domain model://perspectives.domains#CouchdbManagement@12.4\n  use cdb for model://perspectives.domains#Couchdb\n  use acc for model://perspectives.domains#BodiesWithAccounts\n\n  -- model://perspectives.domains#Couchdb"
      expected = "domain model://perspectives.domains#CouchdbManagement@12.5\n  use cdb for model://perspectives.domains#Couchdb@4.0\n  use acc for model://perspectives.domains#BodiesWithAccounts\n\n  -- model://perspectives.domains#Couchdb"
    Assert.equal expected $ applyModelVersions " CouchdbManagement = 12.5 ; Couchdb=4.0 " source

  test "updates every matching import and lets the last assignment win" do
    let
      source = "domain model://joopringelberg.nl#RebootUniverse@1.0\n  use cdb for model://perspectives.domains#Couchdb@3.0\n  use legacy for model://example.com#Couchdb"
      expected = "domain model://joopringelberg.nl#RebootUniverse@1.1\n  use cdb for model://perspectives.domains#Couchdb@4.1\n  use legacy for model://example.com#Couchdb@4.1"
    Assert.equal expected $ applyModelVersions "RebootUniverse=1.1; Couchdb=4.0; invalid; Couchdb=4.1" source

  test "parses versioned imports followed by a comment" do
    let
      source = "-- Copyright\n\n-- PDRDEPENDENCY\ndomain model://perspectives.domains#System@7.0\n  use sys for model://perspectives.domains#System\n  use cdb for model://perspectives.domains#Couchdb\n  use ser for model://perspectives.domains#Serialise\n  use sensor for model://perspectives.domains#Sensor\n  use util for model://perspectives.domains#Utilities\n\n  -- model:System is booted in a unique way.\n"
      versionedSource = applyModelVersions "Couchdb=4.0; Serialise=3.0; Sensor=3.0; Utilities=3.0" source
    (result :: Either ParseError ContextE) <- runIndentParser versionedSource domain
    case result of
      Left parseError -> Assert.assert (show parseError) false
      Right _ -> pure unit

main :: Effect Unit
main = runTest theSuite