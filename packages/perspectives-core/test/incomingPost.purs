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
      (pendingIncomingPostMessage "nl" 3 == "3 nog niet verwerkte gegevensberichten")

  test "formats the singular English pending-post message" do
    liftAff $ assert
      "English singular status text should stay grammatical"
      (pendingIncomingPostMessage "en" 1 == "1 unprocessed data message")

  test "formats the plural English pending-post message" do
    liftAff $ assert
      "English plural status text should include the count"
      (pendingIncomingPostMessage "en" 4 == "4 unprocessed data messages")
