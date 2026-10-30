-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Conversations.Parser
  ( Answer
  , ConversationBody
  , ConversationElement(..)
  , ConversationParseError(..)
  , parseConversation
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.CodePoint.Unicode (isAlpha, isAlphaNum)
import Data.Either (Either(..))
import Data.Foldable (foldl, traverse_)
import Data.List as List
import Data.Maybe (Maybe(..), isJust)
import Data.String (CodePoint, Pattern(..))
import Data.String as String
import Data.String.CodePoints (codePointFromChar, toCodePointArray)
import Foreign.Object (Object)
import Foreign.Object as Object
import Parsing (ParseError, Parser, fail, runParser)
import Parsing.Combinators (many, optionMaybe, try)
import Parsing.String (char, eof, string)
import Parsing.String.Basic (takeWhile)

type Answer =
  { text :: String
  , sequence :: Maybe (Array ConversationElement)
  }

data ConversationElement
  = Statement String
  | Question String
  | Answer Answer
  | Sequence (Array ConversationElement)
  | Ref String

derive instance eqConversationElement :: Eq ConversationElement

type ConversationBody =
  { elements :: Object ConversationElement
  , conversation :: Array ConversationElement
  }

data ConversationParseError
  = LexicalError ParseError
  | StructuralError Int String

derive instance eqConversationParseError :: Eq ConversationParseError

instance showConversationParseError :: Show ConversationParseError where
  show = case _ of
    LexicalError parseError -> show parseError
    StructuralError lineNumber message -> "Line " <> show lineNumber <> ": " <> message

type SourceLine =
  { indentation :: Int
  , lineNumber :: Int
  , text :: String
  }

type Section =
  { body :: Array SourceLine
  , lineNumber :: Int
  , name :: String
  }

type ParsedSequence =
  { elements :: Array ConversationElement
  , rest :: Array SourceLine
  }

parseConversation :: String -> Either ConversationParseError ConversationBody
parseConversation source = do
  lines <- case runParser source sourceLines of
    Left parseError -> Left $ LexicalError parseError
    Right parsedLines -> Right $ Array.mapWithIndex addLineNumber parsedLines
  traverse_ validateSourceLine lines
  sections <- parseSections lines
  validateSectionReferences sections
  assembleBody sections
  where
  addLineNumber index line = line { lineNumber = index + 1 }

sourceLines :: Parser String (Array SourceLine)
sourceLines = Array.fromFoldable <$> many sourceLine <* eof

sourceLine :: Parser String SourceLine
sourceLine = do
  indentation <- List.length <$> many (char ' ')
  text <- takeWhile \codePoint -> codePoint /= newline && codePoint /= carriageReturn
  lineEnding <- optionMaybe $ try (string "\r\n") <|> string "\n" <|> string "\r"
  when (indentation == 0 && String.null text && not (isJust lineEnding)) $
    fail "Expected a source line"
  pure { indentation, lineNumber: 0, text }
  where
  newline = codePointFromChar '\n'
  carriageReturn = codePointFromChar '\r'

parseSections :: Array SourceLine -> Either ConversationParseError (Array Section)
parseSections = go [] <<< dropBlankLines
  where
  go sections lines = case Array.uncons lines of
    Nothing -> Right sections
    Just { head: line, tail } -> case sectionName line of
      Nothing -> structural line "Expected a section header such as 'pasta-guide:' or 'conversation:'."
      Just name -> do
        let split = Array.span (not <<< isSectionHeader) tail
        when (Array.null $ dropBlankLines split.init)
          $ structural line
          $ "Section '" <> name <> "' has no content."
        go (Array.snoc sections { body: split.init, lineNumber: line.lineNumber, name }) (dropBlankLines split.rest)

assembleBody :: Array Section -> Either ConversationParseError ConversationBody
assembleBody sections = do
  result <- foldl addSection (Right { conversation: Nothing, elements: Object.empty }) sections
  conversation <- case result.conversation of
    Nothing -> Left $ StructuralError 1 "The document must contain exactly one 'conversation:' section."
    Just value -> Right value
  pure { conversation, elements: result.elements }
  where
  addSection accumulator section = do
    state <- accumulator
    parsed <- parseCompleteSequence false 0 section.body
    if section.name == "conversation" then case state.conversation of
      Just _ -> Left $ StructuralError section.lineNumber "The document contains more than one 'conversation:' section."
      Nothing -> Right $ state { conversation = Just parsed }
    else if Object.member section.name state.elements then
      Left $ StructuralError section.lineNumber $ "Duplicate section '" <> section.name <> "'."
    else
      Right $ state { elements = Object.insert section.name (Sequence parsed) state.elements }

parseCompleteSequence :: Boolean -> Int -> Array SourceLine -> Either ConversationParseError (Array ConversationElement)
parseCompleteSequence allowReferences indentation lines = do
  parsed <- parseSequence allowReferences indentation (dropBlankLines lines)
  case Array.uncons (dropBlankLines parsed.rest) of
    Nothing -> Right parsed.elements
    Just { head } -> structural head $ "Unexpected indentation; expected " <> show indentation <> " spaces."

parseSequence :: Boolean -> Int -> Array SourceLine -> Either ConversationParseError ParsedSequence
parseSequence allowReferences indentation = go [] <<< dropBlankLines
  where
  go elements lines = case Array.uncons lines of
    Nothing -> Right { elements, rest: [] }
    Just { head: line, tail }
      | line.indentation < indentation -> Right { elements, rest: lines }
      | line.indentation > indentation -> structural line $ "Unexpected indentation; expected " <> show indentation <> " spaces."
      | otherwise -> case questionText line.text of
          Just text -> do
            parsedQuestion <- parseQuestion indentation line text tail
            go (elements <> parsedQuestion.elements) (dropBlankLines parsedQuestion.rest)
          Nothing -> case answerText line.text of
            Just _ -> structural line "An answer must directly follow a question at two spaces deeper indentation."
            Nothing -> case referenceName line.text of
              Just reference
                | allowReferences -> do
                    validateReference line reference
                    go (Array.snoc elements $ Ref reference) (dropBlankLines tail)
                | otherwise -> structural line "A reference is only allowed in the continuation of an answer."
              Nothing -> do
                parsedStatement <- parseStatement indentation lines
                go (Array.snoc elements $ Statement parsedStatement.text) (dropBlankLines parsedStatement.rest)

parseQuestion :: Int -> SourceLine -> String -> Array SourceLine -> Either ConversationParseError ParsedSequence
parseQuestion indentation line text lines = do
  when (String.null $ String.trim text) $ structural line "A question must contain text after '?'."
  parsedAnswers <- parseAnswers (indentation + 2) (dropBlankLines lines)
  when (Array.null parsedAnswers.elements) $
    structural line "A question must be followed by at least one answer indented by two spaces."
  Right
    { elements: Array.cons (Question $ String.trim text) parsedAnswers.elements
    , rest: parsedAnswers.rest
    }

parseAnswers :: Int -> Array SourceLine -> Either ConversationParseError ParsedSequence
parseAnswers indentation = go []
  where
  go answers lines = case Array.uncons (dropBlankLines lines) of
    Nothing -> Right { elements: answers, rest: [] }
    Just { head: line, tail }
      | line.indentation /= indentation -> Right { elements: answers, rest: dropBlankLines lines }
      | otherwise -> case answerText line.text of
          Nothing -> Right { elements: answers, rest: dropBlankLines lines }
          Just text -> do
            when (String.null $ String.trim text) $ structural line "An answer must contain text after '-'."
            let continuationLines = dropBlankLines tail
            continuation <- case Array.uncons continuationLines of
              Just { head } | head.indentation > indentation -> do
                parsed <- parseSequence true (indentation + 2) continuationLines
                when (Array.null parsed.elements) $ structural head "An answer continuation must not be empty."
                Right { sequence: Just parsed.elements, rest: parsed.rest }
              _ -> Right { sequence: Nothing, rest: continuationLines }
            let answer = Answer { text: String.trim text, sequence: continuation.sequence }
            go (Array.snoc answers answer) continuation.rest

parseStatement :: Int -> Array SourceLine -> Either ConversationParseError { rest :: Array SourceLine, text :: String }
parseStatement indentation lines = case Array.uncons lines of
  Nothing -> Left $ StructuralError 1 "Expected a statement."
  Just { head: first, tail } -> do
    when (String.null $ String.trim first.text) $ structural first "A statement must contain text."
    let split = Array.span (isStatementLine indentation) tail
    Right
      { rest: split.rest
      , text: String.joinWith "\n" $ map (String.trim <<< _.text) (Array.cons first split.init)
      }

isStatementLine :: Int -> SourceLine -> Boolean
isStatementLine indentation line =
  line.indentation == indentation
    && not (String.null $ String.trim line.text)
    && questionText line.text == Nothing
    && answerText line.text == Nothing
    && referenceName line.text == Nothing

questionText :: String -> Maybe String
questionText text = String.stripPrefix (Pattern "?") text

answerText :: String -> Maybe String
answerText text = String.stripPrefix (Pattern "-") text

referenceName :: String -> Maybe String
referenceName text = String.stripPrefix (Pattern "ref:") text <#> String.trim

validateReference :: SourceLine -> String -> Either ConversationParseError Unit
validateReference line reference =
  unless (isIdentifier reference) $ structural line $ "Invalid reference identifier '" <> reference <> "'."

validateSectionReferences :: Array Section -> Either ConversationParseError Unit
validateSectionReferences sections = traverse_ validateSection sections
  where
  elementNames = map _.name $ Array.filter (_.name >>> (_ /= "conversation")) sections
  validateSection section = traverse_ validateLine section.body
  validateLine line = case referenceName line.text of
    Nothing -> pure unit
    Just reference -> unless (Array.elem reference elementNames)
      $ structural line
      $ "Unknown conversation section '" <> reference <> "'."

validateSourceLine :: SourceLine -> Either ConversationParseError Unit
validateSourceLine line =
  when (String.contains (Pattern "\t") line.text) $
    structural line "Tabs are not allowed; use two spaces for each indentation level."

sectionName :: SourceLine -> Maybe String
sectionName line
  | line.indentation /= 0 = Nothing
  | otherwise = String.stripSuffix (Pattern ":") line.text >>= \name ->
      if isIdentifier name then Just name else Nothing

isSectionHeader :: SourceLine -> Boolean
isSectionHeader = isJust <<< sectionName

isIdentifier :: String -> Boolean
isIdentifier value = case String.uncons value of
  Nothing -> false
  Just { head, tail } -> isIdentifierStart head && Array.all isIdentifierContinue (toCodePointArray tail)

isIdentifierStart :: CodePoint -> Boolean
isIdentifierStart = isAlpha

isIdentifierContinue :: CodePoint -> Boolean
isIdentifierContinue codePoint =
  isAlphaNum codePoint || codePoint == hyphen || codePoint == underscore
  where
  hyphen = codePointFromChar '-'
  underscore = codePointFromChar '_'

dropBlankLines :: Array SourceLine -> Array SourceLine
dropBlankLines = Array.dropWhile (String.null <<< String.trim <<< _.text)

structural :: forall a. SourceLine -> String -> Either ConversationParseError a
structural line = Left <<< StructuralError line.lineNumber