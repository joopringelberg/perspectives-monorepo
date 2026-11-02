-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-- | Pure name resolution for jump-to-definition.
-- | Definitions are collected from the Readable DomeinFileRecord produced by the compiler
-- | (keys are fully qualified readable names; positions come from the `pos` field).
-- | A name under the cursor is resolved like the compiler does it, approximately:
-- |   * prefixed names (`sys:PerspectivesSystem`) are expanded with the model's namespaces;
-- |   * fully qualified names must match exactly (or are looked up in a dependency);
-- |   * local names (`Thing`, `Ctxt$Thing`) match definitions whose qualified name ends with them;
-- |     candidates are ranked by how many leading name segments they share with the enclosing definition.
module Perspectives.Lsp.Resolve
  ( Definition
  , Resolution(..)
  , nameAt
  , enclosingScope
  , resolve
  , selectionRange
  , localName
  , namesInLine
  , references
  , propertyListObjectAt
  , indexedDeclaration
  ) where

import Prelude

import Data.Array (concat, filter, index, length, mapMaybe, mapWithIndex, null, snoc, takeWhile, zip, (!!))
import Data.Array as Arr
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, indexOf, indexOf', lastIndexOf, split)
import Data.String as Str
import Data.String.CodeUnits (charAt, slice)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Perspectives.Lsp.Diagnostics (LspRange)
import Perspectives.Names (expandNamespaces)

-- | `line` and `column` are 1-based (ArcPosition).
type Definition = { name :: String, kind :: String, line :: Int, column :: Int }

data Resolution
  = Local (Array Definition)
  | External String
  | Unresolved

derive instance Eq Resolution
instance Show Resolution where
  show (Local ds) = "Local " <> show ds
  show (External s) = "External " <> s
  show Unresolved = "Unresolved"

isNameChar :: Char -> Boolean
isNameChar c =
  (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c == '_'
    || c == '$'
    || c == ':'
    || c == '#'
    || c == '/'
    || c == '.'
    || c == '@'

-- | The name under (or immediately before) the 0-based `character` on the given line.
nameAt :: String -> Int -> Maybe String
nameAt lineText character =
  let
    isAt i = case charAt i lineText of
      Just c -> isNameChar c
      Nothing -> false
    pos = if isAt character then Just character else if isAt (character - 1) then Just (character - 1) else Nothing
  in
    case pos of
      Nothing -> Nothing
      Just p ->
        let
          start = goLeft p
          end = goRight p
          raw = slice start (end + 1) lineText
          trimmed = trimEnd raw
        in
          if trimmed == "" then Nothing else Just trimmed
        where
        goLeft i = if isAt (i - 1) then goLeft (i - 1) else i
        goRight i = if isAt (i + 1) then goRight (i + 1) else i
  where
  trimEnd s = case charAt (Str.length s - 1) s of
    Just c | c == '.' || c == ':' -> trimEnd (Str.take (Str.length s - 1) s)
    _ -> s

indentation :: String -> Int
indentation s = Str.length (SCU.takeWhile (_ == ' ') s)

-- | The qualified name of the definition that lexically encloses the 1-based `cursorLine`:
-- | the last definition that starts on that line, or that starts before it and is less indented than the cursor line.
enclosingScope :: Array Definition -> Array String -> Int -> Maybe String
enclosingScope defs = enclosingScopeInSorted (Arr.sortWith (\d -> Tuple d.line d.column) defs)

enclosingScopeInSorted :: Array Definition -> Array String -> Int -> Maybe String
enclosingScopeInSorted sorted lines cursorLine =
  let
    ind = indentation (fromMaybe "" (lines !! (cursorLine - 1)))
    candidates = filter (\d -> d.line == cursorLine || (d.line < cursorLine && d.column - 1 < ind)) sorted
  in
    _.name <$> Arr.last candidates

-- | The part after the last `$` (or `#`).
localName :: String -> String
localName s = case lastIndexOf (Pattern "$") s of
  Just i -> Str.drop (i + 1) s
  Nothing -> case lastIndexOf (Pattern "#") s of
    Just i -> Str.drop (i + 1) s
    Nothing -> s

segments :: String -> Array String
segments = split (Pattern "$")

commonPrefixLength :: Array String -> Array String -> Int
commonPrefixLength a b = length (takeWhile (\(Tuple x y) -> x == y) (zip a b))

endsWith :: String -> String -> Boolean
endsWith suffix s = Str.length s >= Str.length suffix && Str.drop (Str.length s - Str.length suffix) s == suffix

resolve :: Array Definition -> Object String -> Maybe String -> String -> Resolution
resolve defs namespaces mscope name' =
  let
    -- The keyword `external` declares the role whose local name is `External`.
    name = if name' == "external" then "External" else name'
    expanded = if contains (Pattern ":") name && not (contains (Pattern "#") name) then fromMaybe name (expandNamespaces namespaces name) else name
  in
    if contains (Pattern "#") expanded then case filter (\d -> d.name == expanded) defs of
      [] -> External expanded
      ds -> Local ds
    else
      let
        candidates = filter (\d -> endsWith ("$" <> expanded) d.name || endsWith ("#" <> expanded) d.name) defs
        scopeSegments = maybe' [] segments mscope
        scored = (\d -> Tuple (commonPrefixLength (segments d.name) scopeSegments) d) <$> candidates
        best = fromMaybe 0 (maximum ((\(Tuple s _) -> s) <$> scored))
      in
        if null candidates then Unresolved
        else Local ((\(Tuple _ d) -> d) <$> filter (\(Tuple s _) -> s == best) scored)
  where
  maybe' d f m = case m of
    Just x -> f x
    Nothing -> d

-- | The range of the local name of the definition on its declaration line (0-based LSP coordinates).
-- | Falls back to a zero-width range at the declaration position.
selectionRange :: Array String -> Definition -> LspRange
selectionRange lines { name, line, column } =
  let
    lineText = fromMaybe "" (index lines (line - 1))
    ln = localName name
    start = { line: line - 1, character: max 0 (column - 1) }
  in
    case indexOf' (Pattern ln) (max 0 (column - 1)) lineText of
      Just i | ln /= "" -> { start: { line: line - 1, character: i }, end: { line: line - 1, character: i + Str.length ln } }
      _ -> { start, end: start }

-- | All names on a line with their 0-based start column. Text after `--` (a comment) is ignored.
namesInLine :: String -> Array { start :: Int, name :: String }
namesInLine lineText = go 0 []
  where
  code = case indexOf (Pattern "--") lineText of
    Just i -> SCU.take i lineText
    Nothing -> lineText
  n = SCU.length code
  isAt i = case charAt i code of
    Just c -> isNameChar c
    Nothing -> false
  go i acc =
    if i >= n then acc
    else if isAt i && not (isAt (i - 1)) then case nameAt code i of
      Just name -> go (i + 1) (snoc acc { start: i, name })
      Nothing -> go (i + 1) acc
    else go (i + 1) acc

-- | The ranges (0-based) of all names in `lines` that resolve to one of the qualified names in `targets`.
references :: Array Definition -> Object String -> Array String -> Array String -> Array LspRange
references defs namespaces lines targets = concat (mapWithIndex refsOnLine lines)
  where
  sorted = Arr.sortWith (\d -> Tuple d.line d.column) defs
  targetLocals = localName <$> targets
  -- A name can only resolve to a target if their local names agree; this avoids resolving every name.
  mayHit name = Arr.elem (localName (unprefixed (if name == "external" then "External" else name))) targetLocals
  unprefixed s =
    if contains (Pattern "#") s then s
    else case lastIndexOf (Pattern ":") s of
      Just i -> Str.drop (i + 1) s
      Nothing -> s
  refsOnLine i lineText = case filter (\{ name } -> mayHit name) (namesInLine lineText) of
    [] -> []
    names ->
      let
        scope = enclosingScopeInSorted sorted lines (i + 1)
      in
        mapMaybe
          ( \{ start, name } ->
              if hits (resolve defs namespaces scope name) then Just { start: { line: i, character: start }, end: { line: i, character: start + SCU.length name } }
              else Nothing
          )
          names
  hits (Local ds) = Arr.any (\d -> Arr.elem d.name targets) ds
  hits (External q) = Arr.elem q targets
  hits Unresolved = false

-- | When the 0-based `character` on the 0-based `line` is inside the (unclosed) parenthesised list of `props (...)`,
-- | the name of the object of the perspective and the 1-based line on which it is given:
-- |   * `perspective on X` (less indented than the `props` line) gives `X`;
-- |   * `perspective of U` gives the role whose declaration (`thing`, `user`, `context`, `external`) encloses it.
propertyListObjectAt :: Array String -> Int -> Int -> Maybe { name :: String, line :: Int }
propertyListObjectAt lines line character =
  let
    lineText = fromMaybe "" (lines !! line)
    before = SCU.take character (codeOf lineText)
  in
    case lastIndexOf (Pattern "props") before of
      Nothing -> Nothing
      Just i ->
        let
          rest = Str.drop (i + 5) before
          isWordStart = i == 0 || not (maybe' false isNameChar (charAt (i - 1) before))
          afterProps = Str.trim rest
        in
          if isWordStart && Str.take 1 afterProps == "(" && not (contains (Pattern ")") afterProps) then up (indentation lineText) false (line - 1)
          else Nothing
  where
  codeOf t = case indexOf (Pattern "--") t of
    Just i -> SCU.take i t
    Nothing -> t
  maybe' d f m = case m of
    Just x -> f x
    Nothing -> d
  words t = filter (_ /= "") (split (Pattern " ") (Str.trim (codeOf t)))
  -- Walk upwards through ever less indented lines.
  up ind ofSeen i = walk ind ofSeen false i

  walk ind ofSeen masterSeen i
    | i < 0 || ind == 0 = Nothing
    | otherwise =
        let
          t = fromMaybe "" (lines !! i)
          ws = words t
          found name = Just { name, line: i + 1 }
        in
          if Arr.null ws || indentation t >= ind then walk ind ofSeen masterSeen (i - 1)
          -- The role of a master/detail section is given on the line that encloses `master` or `detail`.
          else if masterSeen then Arr.head ws >>= found
          else case ws of
            _ | not ofSeen, Arr.take 2 ws == [ "perspective", "on" ] -> ws !! 2 >>= found
            _ | Arr.take 2 ws == [ "perspective", "of" ] -> walk (indentation t) true false (i - 1)
            _ | ofSeen, Arr.head ws == Just "external" -> found "External"
            _ | ofSeen, Arr.elem (fromMaybe "" (Arr.head ws)) [ "thing", "user", "context" ] -> ws !! 1 >>= found
            -- Screen widgets: `table ["title"] Role` and `form ["title"] Role`.
            _ | not ofSeen, Arr.length ws >= 2, Arr.elem (fromMaybe "" (Arr.head ws)) [ "table", "form" ] -> Arr.last ws >>= found
            _ | not ofSeen, Arr.elem (fromMaybe "" (Arr.head ws)) [ "master", "detail" ] -> walk (indentation t) false true (i - 1)
            _ -> walk (indentation t) ofSeen false (i - 1)

-- | The range (0-based) of the name in the first declaration `indexed <name>` at or after the 0-based line `from`
-- | whose local part (after the prefix, `#` or `$`) equals that of the qualified `indexedName`.
indexedDeclaration :: Array String -> Int -> String -> Maybe LspRange
indexedDeclaration lines from indexedName = go (max 0 from)
  where
  target = localPart indexedName
  localPart s = localName case lastIndexOf (Pattern ":") s of
    Just i | not (contains (Pattern "#") s) -> Str.drop (i + 1) s
    _ -> s
  go i = case lines !! i of
    Nothing -> Nothing
    Just t -> case namesInLine t of
      [ { name: "indexed" }, { start, name } ] | localPart name == target ->
        Just { start: { line: i, character: start }, end: { line: i, character: start + SCU.length name } }
      _ -> go (i + 1)
