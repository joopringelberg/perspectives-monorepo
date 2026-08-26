-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Conversations.Renderer
  ( ConversationRenderError(..)
  , renderConversationBody
  , renderConversationFromContextYaml
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (message)
import Foreign (F, Foreign, ForeignError(..), fail, readString, tagOf)
import Foreign.Object (Object)
import Foreign.Object as Object
import Perspectives.Conversations.Parser (ConversationBody, ConversationElement(..), parseConversation)
import Purescript.YAML as YAML
import Simple.JSON (read')

data ConversationRenderError
  = YamlParseError String
  | ConversationNotFound String
  | InvalidConversationYaml String
  | UnrepresentableConversation String

derive instance eqConversationRenderError :: Eq ConversationRenderError

instance showConversationRenderError :: Show ConversationRenderError where
  show = case _ of
    YamlParseError yamlMessage -> "Cannot parse context YAML: " <> yamlMessage
    ConversationNotFound conversationId -> "Conversation '" <> conversationId <> "' was not found."
    InvalidConversationYaml yamlMessage -> "Invalid conversation YAML: " <> yamlMessage
    UnrepresentableConversation renderMessage -> "Conversation cannot be represented in the surface grammar: " <> renderMessage

-- | Select a conversation from a context YAML document and render it in the
-- | canonical surface syntax accepted by parseConversation.
renderConversationFromContextYaml :: String -> String -> Effect (Either ConversationRenderError String)
renderConversationFromContextYaml yamlSource conversationId = do
  parsed <- YAML.load yamlSource :: Effect (Either _ Foreign)
  pure do
    document <- case parsed of
      Left yamlError -> Left $ YamlParseError $ message yamlError
      Right foreignValue -> decodeForeign "context document" decodeContextDocument foreignValue
    rawConversation <- case Object.lookup conversationId document of
      Nothing -> Left $ ConversationNotFound conversationId
      Just conversation -> Right conversation
    conversation <- decodeForeign ("conversation '" <> conversationId <> "'") decodeConversationBody rawConversation
    renderConversationBody conversation

-- | Render an already decoded conversation body. The result is reparsed before
-- | it is returned, ensuring it belongs to the current surface grammar.
renderConversationBody :: ConversationBody -> Either ConversationRenderError String
renderConversationBody body = do
  elementSections <- traverse renderNamedElement (Object.toUnfoldable body.elements :: Array (Tuple String ConversationElement))
  root <- renderSection "conversation" body.conversation
  let rendered = String.joinWith "\n\n" $ elementSections <> [ root ]
  case parseConversation rendered of
    Left parseError -> Left $ UnrepresentableConversation $ show parseError
    Right _ -> Right rendered

decodeContextDocument :: Foreign -> F (Object Foreign)
decodeContextDocument foreignValue = do
  document :: { conversations :: Object Foreign } <- read' foreignValue
  pure document.conversations

decodeConversationBody :: Foreign -> F ConversationBody
decodeConversationBody foreignValue = do
  body :: { conversation :: Array Foreign, elements :: Maybe (Object Foreign) } <- read' foreignValue
  conversation <- traverse decodeElement body.conversation
  elements <- traverse decodeElement $ fromMaybe Object.empty body.elements
  pure { conversation, elements }

decodeElement :: Foreign -> F ConversationElement
decodeElement foreignValue = do
  object :: Object Foreign <- read' foreignValue
  case Object.toUnfoldable object :: Array (Tuple String Foreign) of
    [ Tuple "statement" value ] -> Statement <$> readString value
    [ Tuple "question" value ] -> Question <$> readString value
    [ Tuple "answer" value ] -> Answer <$> decodeAnswer value
    [ Tuple "sequence" value ] -> Sequence <$> (read' value >>= traverse decodeElement)
    [ Tuple "ref" value ] -> Ref <$> readString value
    [ Tuple key _ ] -> fail $ ForeignError $ "Unsupported conversation element '" <> key <> "'."
    _ -> fail $ ForeignError "A conversation element must contain exactly one key."

decodeAnswer :: Foreign -> F { text :: String, sequence :: Maybe (Array ConversationElement) }
decodeAnswer foreignValue
  | tagOf foreignValue == "String" = do
      text <- readString foreignValue
      pure { text, sequence: Nothing }
  | otherwise = do
      answer :: { text :: String, sequence :: Maybe (Array Foreign) } <- read' foreignValue
      sequence <- traverse (traverse decodeElement) answer.sequence
      pure { text: answer.text, sequence }

renderNamedElement :: Tuple String ConversationElement -> Either ConversationRenderError String
renderNamedElement (Tuple name element) = case element of
  Sequence sequence -> renderSection name sequence
  _ -> renderSection name [ element ]

renderSection :: String -> Array ConversationElement -> Either ConversationRenderError String
renderSection name sequence = do
  lines <- renderSequence false 0 sequence
  pure $ name <> ":\n" <> String.joinWith "\n" lines

renderSequence :: Boolean -> Int -> Array ConversationElement -> Either ConversationRenderError (Array String)
renderSequence allowReferences indentation sequence = do
  rendered <- traverse (renderElement allowReferences indentation) $ flattenSequences sequence
  pure $ Array.concat $ Array.mapWithIndex addStatementSeparator rendered
  where
  flattened = flattenSequences sequence
  addStatementSeparator index lines =
    if isStatementAt index && isStatementAt (index + 1) then Array.snoc lines "" else lines
  isStatementAt index = case Array.index flattened index of
    Just (Statement _) -> true
    _ -> false

renderElement :: Boolean -> Int -> ConversationElement -> Either ConversationRenderError (Array String)
renderElement allowReferences indentation = case _ of
  Statement text -> renderStatement indentation text
  Question text -> do
    validateSingleLine "question" text
    pure [ spaces indentation <> "? " <> text ]
  Answer { text, sequence } -> do
    validateSingleLine "answer" text
    continuation <- case sequence of
      Nothing -> pure []
      Just elements -> renderSequence true (indentation + 4) elements
    pure $ Array.cons (spaces (indentation + 2) <> "- " <> text) continuation
  Sequence sequence -> renderSequence allowReferences indentation sequence
  Ref reference
    | allowReferences -> pure [ spaces indentation <> "ref: " <> reference ]
    | otherwise -> Left $ UnrepresentableConversation "A reference occurs outside an answer continuation."

renderStatement :: Int -> String -> Either ConversationRenderError (Array String)
renderStatement indentation text = do
  let lines = String.split (Pattern "\n") text
  when (Array.null lines || Array.any String.null lines)
    $ Left
    $ UnrepresentableConversation "A statement contains an empty line."
  traverse renderLine lines
  where
  renderLine line = do
    when (String.trim line /= line)
      $ Left
      $ UnrepresentableConversation "A statement line has leading or trailing whitespace."
    when (startsWithReservedMarker line)
      $ Left
      $ UnrepresentableConversation
      $ "A statement line starts with a reserved marker: '" <> line <> "'."
    pure $ spaces indentation <> line

validateSingleLine :: String -> String -> Either ConversationRenderError Unit
validateSingleLine kind text = do
  when (String.contains (Pattern "\n") text || String.contains (Pattern "\r") text)
    $ Left
    $ UnrepresentableConversation
    $ "A " <> kind <> " contains multiple lines."
  when (String.null text || String.trim text /= text)
    $ Left
    $ UnrepresentableConversation
    $ "A " <> kind <> " is empty or has leading or trailing whitespace."

startsWithReservedMarker :: String -> Boolean
startsWithReservedMarker text =
  hasPrefix "?" || hasPrefix "-" || hasPrefix "ref:"
  where
  hasPrefix prefix = isJust $ String.stripPrefix (Pattern prefix) text

flattenSequences :: Array ConversationElement -> Array ConversationElement
flattenSequences = Array.concatMap case _ of
  Sequence sequence -> flattenSequences sequence
  element -> [ element ]

spaces :: Int -> String
spaces count = String.joinWith "" $ Array.replicate count " "

decodeForeign :: forall a. String -> (Foreign -> F a) -> Foreign -> Either ConversationRenderError a
decodeForeign description decoder foreignValue = case runExcept $ decoder foreignValue of
  Left foreignErrors -> Left $ InvalidConversationYaml $ description <> ": " <> show foreignErrors
  Right value -> Right value