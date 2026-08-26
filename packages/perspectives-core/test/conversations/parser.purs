module Test.Conversations.Parser where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Foreign.Object as Object
import Perspectives.Conversations.Parser (ConversationElement(..), parseConversation)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Conversations.Parser" do
  test "parses the food preparation conversation" do
    case parseConversation foodPreparation of
      Left parseError -> assert (show parseError) false
      Right body -> do
        assert "The root conversation should contain its introduction, question, and three answers." $
          case body.conversation of
            [ Statement introduction
            , Question question
            , Answer { text: "Pasta", sequence: Just [ Ref "pasta-guide" ] }
            , Answer { text: "Risotto", sequence: Just [ Ref "risotto-guide" ] }
            , Answer { text: "Soup", sequence: Just [ Ref "soup-guide" ] }
            ] -> introduction == "Welcome to the Interactive Cooking Guide.\nI will help you find the right recipe."
              && question == "What would you like to cook today?"
            _ -> false
        assert "A terminal answer should have no continuation, while Penne should have a nested sequence." $
          case Object.lookup "pasta-guide" body.elements of
            Just
              ( Sequence
                  [ Statement _
                  , Question _
                  , Answer { text: "Spaghetti Carbonara", sequence: Nothing }
                  , Answer
                      { text: "Penne Arrabbiata"
                      , sequence: Just
                          [ Statement _
                          , Question "How spicy would you like it?"
                          , Answer { text: "Mild - use half a chilli pepper.", sequence: Nothing }
                          , Answer { text: "Hot - use two chilli peppers", sequence: Nothing }
                          ]
                      }
                  , Answer { text: "Lasagna al Forno", sequence: Nothing }
                  ]
              ) -> true
            _ -> false

  test "does not require a question to end in a question mark" do
    case parseConversation "conversation:\n? Choose one\n  - First\n  - Second" of
      Right { conversation: [ Question "Choose one", Answer _, Answer _ ] } -> assert "Expected a question and two answers." true
      _ -> assert "Expected a question and two answers." false

  test "rejects unresolved references" do
    case parseConversation "conversation:\n? Choose one\n  - First\n    ref: missing" of
      Left _ -> assert "An unresolved reference should fail validation." true
      Right _ -> assert "An unresolved reference should fail validation." false

  test "requires two-space answer indentation" do
    case parseConversation "conversation:\n? Choose one\n - First" of
      Left _ -> assert "One-space indentation should be rejected." true
      Right _ -> assert "One-space indentation should be rejected." false

foodPreparation :: String
foodPreparation =
  """
pasta-guide:
Pasta dishes require durum wheat pasta and a suitable sauce.
? Which pasta dish would you like to prepare?
  - Spaghetti Carbonara
  - Penne Arrabbiata
    Penne Arrabbiata is a spicy tomato-based pasta.
    ? How spicy would you like it?
      - Mild - use half a chilli pepper.
      - Hot - use two chilli peppers
  - Lasagna al Forno

risotto-guide:
Risotto requires Arborio or Carnaroli rice and warm stock.
? Which risotto would you like to prepare?
  - Risotto ai Funghi
  - Risotto al Limone
  - Risotto Milanese

soup-guide:
Soups are versatile and can be made from almost any vegetable.
? Which soup would you like to make?
  - Minestrone
  - French Onion Soup
  - Pumpkin Soup

conversation:
Welcome to the Interactive Cooking Guide.
I will help you find the right recipe.
? What would you like to cook today?
  - Pasta
    ref: pasta-guide
  - Risotto
    ref: risotto-guide
  - Soup
    ref: soup-guide
"""