module Test.ObsoleteRepresentationCleanup where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..), isNothing)
import Data.String (Pattern(..), indexOf)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Representation.Context (ContextKind(..), defaultContext)
import Perspectives.Representation.EnumeratedRole (defaultEnumeratedRole)
import Perspectives.Representation.TypeIdentifiers (RoleKind(..))
import Simple.JSON (writeJSON)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

theSuite :: Free TestF Unit
theSuite = suite "Test.ObsoleteRepresentationCleanup" do
  test "default EnumeratedRole omits obsolete inverted-query members" do
    let
      serialisedRole = writeJSON $ defaultEnumeratedRole "model:Test$Ctx$ARole" "ARole" RoleInContext "model:Test$Ctx" origin
    assert "default EnumeratedRole should not serialise fillerInvertedQueries" (lacks "\"fillerInvertedQueries\"" serialisedRole)
    assert "default EnumeratedRole should not serialise filledInvertedQueries" (lacks "\"filledInvertedQueries\"" serialisedRole)
    assert "default EnumeratedRole should not serialise contextInvertedQueries" (lacks "\"contextInvertedQueries\"" serialisedRole)

  test "default Context omits obsolete role-inverted-query members" do
    let
      serialisedContext = writeJSON $ defaultContext "model:Test$Ctx" "Ctx" Domain Nothing origin Nothing
    assert "default Context should not serialise roleInvertedQueries" (lacks "\"roleInvertedQueries\"" serialisedContext)

origin :: ArcPosition
origin = ArcPosition { line: 0, column: 0 }

lacks :: String -> String -> Boolean
lacks fragment serialised = isNothing $ indexOf (Pattern fragment) serialised
