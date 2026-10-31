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

-- | Turns PerspectivesErrors into LSP Diagnostics.
-- | We do not parse printed messages. Instead, we use the generic representation of the
-- | PerspectivesError data constructors: the constructor name becomes the diagnostic `code`
-- | and all ArcPosition arguments (also those inside `Maybe ArcPosition`, `PureLetStep` and `LetStep`)
-- | determine the diagnostic range.
module Perspectives.Lsp.Diagnostics
  ( ErrorInfo
  , LspPosition
  , LspRange
  , Diagnostic
  , errorInfo
  , error2Diagnostic
  , warning2Diagnostic
  , arcPosition2LspPosition
  , positions2Range
  , class GenericErrorInfo
  , genericErrorInfo
  , class ArgPositions
  , argPositions
  , class PositionsIn
  , positionsIn
  ) where

import Prelude

import Data.Array (filter, head, last)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, Product(..), Sum(..), from)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Perspectives.Parsing.Arc.Expression.AST (PureLetStep(..))
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Parsing.Arc.Statement.AST (LetStep(..))
import Perspectives.Parsing.Messages (PerspectivesError)
import Type.Proxy (Proxy(..))

type ErrorInfo = { name :: String, positions :: Array ArcPosition }

type LspPosition = { line :: Int, character :: Int }
type LspRange = { start :: LspPosition, end :: LspPosition }

-- | severity: 1 = Error, 2 = Warning (LSP DiagnosticSeverity).
-- | `data.positions` holds all (1-based) ArcPositions found in the error, for clients that want more than the range.
type Diagnostic =
  { range :: LspRange
  , severity :: Int
  , code :: String
  , source :: String
  , message :: String
  , "data" :: { positions :: Array { line :: Int, column :: Int } }
  }

-----------------------------------------------------------
-- GENERIC TRAVERSAL OF THE DATA CONSTRUCTORS
-----------------------------------------------------------
class GenericErrorInfo rep where
  genericErrorInfo :: rep -> ErrorInfo

instance (GenericErrorInfo a, GenericErrorInfo b) => GenericErrorInfo (Sum a b) where
  genericErrorInfo (Inl a) = genericErrorInfo a
  genericErrorInfo (Inr b) = genericErrorInfo b

instance (IsSymbol name, ArgPositions args) => GenericErrorInfo (Constructor name args) where
  genericErrorInfo (Constructor args) = { name: reflectSymbol (Proxy :: Proxy name), positions: argPositions args }

class ArgPositions rep where
  argPositions :: rep -> Array ArcPosition

instance ArgPositions NoArguments where
  argPositions _ = []

instance (ArgPositions a, ArgPositions b) => ArgPositions (Product a b) where
  argPositions (Product a b) = argPositions a <> argPositions b

instance PositionsIn a => ArgPositions (Argument a) where
  argPositions (Argument a) = positionsIn a

class PositionsIn a where
  positionsIn :: a -> Array ArcPosition

instance PositionsIn ArcPosition where
  positionsIn p = [ p ]
else instance PositionsIn (Maybe ArcPosition) where
  positionsIn = maybe [] pure
else instance PositionsIn PureLetStep where
  positionsIn (PureLetStep { start, end }) = [ start, end ]
else instance PositionsIn LetStep where
  positionsIn (LetStep { start, end }) = [ start, end ]
else instance PositionsIn a where
  positionsIn _ = []

errorInfo :: forall e rep. Generic e rep => GenericErrorInfo rep => e -> ErrorInfo
errorInfo = genericErrorInfo <<< from

-----------------------------------------------------------
-- POSITIONS AND RANGES
-----------------------------------------------------------
-- | ArcPositions are 1-based; LSP positions are 0-based.
arcPosition2LspPosition :: ArcPosition -> LspPosition
arcPosition2LspPosition (ArcPosition { line, column }) = { line: max 0 (line - 1), character: max 0 (column - 1) }

-- | No (valid) positions: the start of the document.
-- | One position: a zero-width range (clients expand it to the word at that position).
-- | Two or more: from the first to the last position (if the last is not before the first).
positions2Range :: Array ArcPosition -> LspRange
positions2Range ps = case head valid, last valid of
  Just s, Just e ->
    let
      start = arcPosition2LspPosition s
      end = arcPosition2LspPosition e
    in
      if before end start then { start, end: start } else { start, end }
  _, _ -> { start: zero', end: zero' }
  where
  valid = filter (\(ArcPosition { line }) -> line >= 1) ps
  zero' = { line: 0, character: 0 }
  before a b = a.line < b.line || (a.line == b.line && a.character < b.character)

-----------------------------------------------------------
-- DIAGNOSTICS
-----------------------------------------------------------
error2Diagnostic :: PerspectivesError -> Diagnostic
error2Diagnostic e =
  let
    { name, positions } = errorInfo e
  in
    { range: positions2Range positions
    , severity: 1
    , code: name
    , source: "perspectives-pdr"
    , message: show e
    , "data": { positions: (\(ArcPosition p) -> p) <$> positions }
    }

-- | Compiler warnings carry no position; they are shown at the start of the document.
warning2Diagnostic :: String -> Diagnostic
warning2Diagnostic message =
  { range: positions2Range []
  , severity: 2
  , code: "Warning"
  , source: "perspectives-pdr"
  , message
  , "data": { positions: [] }
  }
