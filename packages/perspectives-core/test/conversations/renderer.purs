module Test.Conversations.Renderer where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Perspectives.Conversations.Parser (ConversationElement(..), parseConversation)
import Perspectives.Conversations.Renderer (ConversationRenderError(..), renderConversationFromContextYaml)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Conversations.Renderer" do
  test "renders a selected context YAML conversation as parseable surface text" do
    result <- liftEffect $ renderConversationFromContextYaml contextYaml "food-guide"
    case result of
      Left renderError -> assert (show renderError) false
      Right rendered -> case parseConversation rendered of
        Left parseError -> assert (show parseError) false
        Right body -> do
          assert "The named element and its reference should survive the YAML roundtrip." $
            case Object.lookup "pasta-guide" body.elements of
              Just (Sequence [ Statement _, Question _, Answer _, Answer _ ]) -> true
              _ -> false
          assert "Separate YAML statements should remain separate surface statements." $
            case body.conversation of
              [ Statement "Welcome."
              , Statement "I will help you choose."
              , Question "What would you like to cook?"
              , Answer { text: "Pasta", sequence: Just [ Ref "pasta-guide" ] }
              , Answer { text: "Soup", sequence: Nothing }
              ] -> true
              _ -> false

  test "reports an unknown conversation identifier" do
    result <- liftEffect $ renderConversationFromContextYaml contextYaml "missing"
    case result of
      Left (ConversationNotFound "missing") -> assert "Expected a missing-conversation error." true
      _ -> assert "Expected a missing-conversation error." false

  test "rejects YAML text outside the current surface grammar" do
    result <- liftEffect $ renderConversationFromContextYaml multilineAnswerYaml "unsupported"
    case result of
      Left (UnrepresentableConversation _) -> assert "Expected an unrepresentable-conversation error." true
      _ -> assert "Expected an unrepresentable-conversation error." false

contextYaml :: String
contextYaml =
  """
schema: perspectives-context-conversations/v1
model: model://example.org#Cooking@1.0
context: model://example.org#Cooking$Kitchen
conversations:
  food-guide:
    elements:
      pasta-guide:
        sequence:
          - statement: Pasta requires a suitable sauce.
          - question: Which pasta?
          - answer: Carbonara
          - answer: Arrabbiata
    conversation:
      - statement: Welcome.
      - statement: I will help you choose.
      - question: What would you like to cook?
      - answer:
          text: Pasta
          sequence:
            - ref: pasta-guide
      - answer: Soup
bindings: {}
"""

multilineAnswerYaml :: String
multilineAnswerYaml =
  """
schema: perspectives-context-conversations/v1
model: model://example.org#Cooking@1.0
context: model://example.org#Cooking$Kitchen
conversations:
  unsupported:
    conversation:
      - question: Choose
      - answer: |
          This answer occupies
          more than one line.
"""