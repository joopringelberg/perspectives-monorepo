module Test.Error.Pretty where

import Prelude

import Control.Monad.Free (Free)
import Effect.Aff.Class (liftAff)
import Perspectives.Error.Pretty (renderMultiplePerspectivesErrors, renderPerspectivesError)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

position :: ArcPosition
position = ArcPosition { line: 3, column: 7 }

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Error.Pretty" do
  test "renderPerspectivesError humanizes URI-like UnknownRole names" do
    rendered <- runP $ renderPerspectivesError (UnknownRole position "model://example.org#MyModel$MyContext$MyRole")
    liftAff $ assert "UnknownRole should be rendered with the local type name" $
      rendered == "(UnknownRole) The role 'MyRole' is not defined, at: line 3, column 7"

  test "renderMultiplePerspectivesErrors renders each error before joining" do
    rendered <- runP $ renderMultiplePerspectivesErrors
      [ UnknownRole position "model://example.org#MyModel$MyContext$MyRole"
      , Custom "Second error"
      ]
    liftAff $ assert "Multiple errors should be humanized and newline-separated" $
      rendered == "(UnknownRole) The role 'MyRole' is not defined, at: line 3, column 7\nSecond error"
