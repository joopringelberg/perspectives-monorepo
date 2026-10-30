module Test.Conversations.ParserMain where

import Prelude

import Effect (Effect)
import Test.Conversations.Parser (theSuite)
import Test.Conversations.Renderer (theSuite) as Renderer
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  theSuite
  Renderer.theSuite