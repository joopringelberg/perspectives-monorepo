module Test.AMQP.IncomingPost where

import Prelude

import Control.Monad.Free (Free)
import Effect.Aff.Class (liftAff)
import Perspectives.AMQP.IncomingPost (pendingIncomingPostMessage)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.AMQP.IncomingPost" do
  test "formats the Dutch pending-post message" do
    liftAff $ assert
      "Dutch status text should include the count and localized message"
      (pendingIncomingPostMessage "nl" 3 == "Bezig met 3 inkomende berichten")

  test "formats the singular English pending-post message" do
    liftAff $ assert
      "English singular status text should stay grammatical"
      (pendingIncomingPostMessage "en" 1 == "Working on 1 incoming message")

  test "formats the plural English pending-post message" do
    liftAff $ assert
      "English plural status text should include the count"
      (pendingIncomingPostMessage "en" 4 == "Working on 4 incoming messages")

  test "formats singular Dutch progress with a regional locale" do
    liftAff $ assert
      "Dutch singular status text should stay grammatical"
      (pendingIncomingPostMessage "nl-NL" 1 == "Bezig met 1 inkomend bericht")
