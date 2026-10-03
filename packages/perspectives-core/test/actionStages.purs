-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.
-- END LICENSE

module Test.Parsing.Arc.ActionStages where

import Prelude

import Data.Array (fromFoldable, length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Parsing.String (eof)
import Perspectives.Parsing.Arc (userRoleE)
import Perspectives.Parsing.Arc.AST (ActionE(..), ContextActionE(..), ContextPart(..), RoleE(..), RolePart(..), StateE(..), StateQualifiedPart(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.Statement.AST (LetStep(..), Statements(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Action settlement stages" do
  for_
    [ { name: "context action", prefix: "user Tester\n  " }
    , { name: "role action", prefix: "user Tester\n  perspective on extern\n    " }
    , { name: "calculated-user role action", prefix: "user Tester = me\n  perspective on extern\n    " }
    ]
    \{ name, prefix } -> do
      let
        indent = case name of
          "context action" -> "    "
          _ -> "      "
        immediate = indent <> "Text1 = \"Action1 executed\" for extern\n"
        settled = indent <> "once settled\n" <> indent <> "  Text1 = extern >> Text1 + \"(settled)\" for extern\n"
        nextAction = prefixActionIndent indent <> "action Action2\n" <> indent <> "Text1 = \"Action2 executed\" for extern\n"
        source body = prefix <> "action Action1\n" <> body <> nextAction

      test (name <> " supports once settled without letA") do
        effects <- parseEffects $ source (immediate <> "\n" <> settled)
        case effects of
          [ Let (LetStep { bindings, stages }), Statements next ] -> do
            equal [] bindings
            equal [ 1, 1 ] (map length stages)
            equal 1 (length next)
          _ -> assert "Expected two settlement stages and a separate sibling action" false

      test (name <> " supports multiple settlement stages") do
        effects <- parseEffects $ source (immediate <> settled <> settled)
        case effects of
          [ Let (LetStep { bindings, stages }), _ ] -> do
            equal [] bindings
            equal [ 1, 1, 1 ] (map length stages)
          _ -> assert "Expected three settlement stages" false

      test (name <> " keeps plain assignments as Statements") do
        effects <- parseEffects $ source (immediate <> immediate)
        case effects of
          [ Statements assignments, Statements _ ] -> equal 2 (length assignments)
          _ -> assert "Expected the existing single-stage representation" false

      test (name <> " preserves letA bindings and stages") do
        effects <- parseEffects $ source
          ( indent <> "letA\n" <> indent <> "  x <- 1\n" <> indent <> "in\n"
              <> "  "
              <> immediate
              <> "  "
              <> indent
              <> "once settled\n"
              <> indent
              <> "    Text1 = extern >> Text1 + \"(settled)\" for extern\n"
          )
        case effects of
          [ Let (LetStep { bindings, stages }), _ ] -> do
            equal 1 (length bindings)
            equal [ 1, 1 ] (map length stages)
          _ -> assert "Expected letA bindings and two stages" false

-- The action header is two columns to the left of its body.
prefixActionIndent :: String -> String
prefixActionIndent indent = case indent of
  "    " -> "  "
  _ -> "    "

parseEffects :: String -> Aff (Array Statements)
parseEffects source = do
  parsed <- runIndentParser source (userRoleE <* eof)
  case parsed of
    Left err -> do
      assert (show err) false
      pure []
    Right (RE (RoleE { roleParts })) -> pure $ fromFoldable roleParts >>= case _ of
      SQP parts -> fromFoldable parts >>= actionEffect
      ROLESTATE (StateE { stateParts }) -> fromFoldable stateParts >>= actionEffect
      _ -> []
    _ -> do
      assert "Expected a user role" false
      pure []

actionEffect :: StateQualifiedPart -> Array Statements
actionEffect = case _ of
  CA (ContextActionE { effect }) -> [ effect ]
  AC (ActionE { effect }) -> [ effect ]
  _ -> []