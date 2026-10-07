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

import Data.Array (concat, fromFoldable, head, length)
import Data.Either (Either(..))
import Data.Foldable (any, for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split)
import Data.String.CodeUnits (drop)
import Effect (Effect)
import Effect.Aff (Aff)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Parsing.String (eof)
import Perspectives.Parsing.Arc (domain, userRoleE)
import Perspectives.Parsing.Arc.AST (ActionE(..), AutomaticEffectE(..), ContextActionE(..), ContextE(..), ContextPart(..), RoleE(..), RolePart(..), StateE(..), StateQualifiedPart(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), Operator(..), Step(..), SimpleStep(..), UnaryStep(..))
import Perspectives.Parsing.Arc.Statement.AST (Assignment(..), LetStep(..), Statements(..))
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

  for_ [ "entry", "exit" ] \transition ->
    for_ [ "do", "do for Tester", "do for Tester once settled" ] \header -> do
      let
        prefix = "user Tester\n  on " <> transition <> "\n    " <> header <> "\n"
        immediate = "      Text1 = \"immediate\" for extern\n"
        settled = "      once settled\n        Text1 = \"settled\" for extern\n"
        sibling = "    notify Tester\n      \"Finished\"\n    do for Tester\n      Text1 = \"sibling\" for extern\n"

      test (transition <> " " <> header <> " supports multiple settlement stages and sibling effects") do
        effects <- parseEffects (prefix <> immediate <> settled <> settled <> sibling)
        case effects of
          [ Let (LetStep { bindings, stages }), Statements next ] -> do
            equal [] bindings
            equal [ 1, 1, 1 ] (map length stages)
            equal 1 (length next)
          _ -> assert "Expected staged automatic effect and a separate sibling effect" false

      test (transition <> " " <> header <> " preserves plain assignments") do
        effects <- parseEffects (prefix <> immediate <> immediate <> sibling)
        case effects of
          [ Statements assignments, Statements _ ] -> equal 2 (length assignments)
          _ -> assert "Expected the existing single-stage automatic effect representation" false

  test "repository upload settles before updating build and generating translations" do
    source <- readTextFile UTF8 "src/model/couchdbManagement@12.4.arc"
    parsed <- runIndentParser source (domain <* eof)
    case parsed of
      Left err -> assert (show err) false
      Right root ->
        assert "Expected upload and completion in separate settlement stages" $
          any
            ( case _ of
                Let (LetStep { bindings: [], stages: [ [ ExternalEffect { effectName } ], completion ] }) ->
                  effectName == "p:UploadToRepository" && map propertyName completion == [ "Build", "MustUpload", "GenerateYaml" ]
                _ -> false
            )
            (automaticEffects root)

  for_ [ "src/model/couchdbManagement@12.4.arc", "src/model/couchdbManagement.arc" ] \path ->
    test (path <> " keeps repository admin rights after setting AuthorizedDomain") do
      source <- readTextFile UTF8 path
      roleSource <- case split (Pattern "    user Admin filledBy CouchdbServer$Admin\n") source of
        [ _, rest ] -> case head (split (Pattern "\n      on exit ") rest) of
          Just lifecycle -> pure $
            "user Admin filledBy CouchdbServer$Admin\n"
              <> joinWith "\n" (map (drop 4) (split (Pattern "\n") lifecycle))
              <> "\n"
          _ -> do
            assert "Expected repository admin role exit handler" false
            pure ""
        _ -> do
          assert "Expected repository admin role" false
          pure ""
      parsed <- runIndentParser roleSource (userRoleE <* eof)
      case parsed of
        Left err -> assert (show err) false
        Right (RE (RoleE { roleParts })) -> case fromFoldable roleParts >>= adminLifecycleStates of
          [ StateE { condition, stateParts } ] -> do
            assert ("Admin lifecycle must depend only on binding and database readiness: " <> show condition) $
              case condition of
                Binary
                  ( BinaryStep
                      { operator: LogicalAnd _
                      , left: Unary (Exists _ (Simple (Filler _ _)))
                      , right: Binary
                          ( BinaryStep
                              { operator: Compose _
                              , left: Simple (Context _)
                              , right: Binary
                                  ( BinaryStep
                                      { operator: Compose _
                                      , left: Simple (Extern _)
                                      , right: Simple (ArcIdentifier _ "RepoHasDatabases")
                                      }
                                  )
                              }
                          )
                      }
                  ) -> true
                _ -> false
            equal [ "AuthorizedDomain" ] $
              concatMapPropertyNames (fromFoldable stateParts >>= actionEffect)
          _ -> assert "Expected one repository admin lifecycle state" false
        _ -> assert "Expected repository admin role" false

  test "Big Bang settles server and repositories before running their consumers" do
    source <- readTextFile UTF8 "src/model/rebootUniverse@2.0.arc"
    parsed <- runIndentParser source (domain <* eof)
    case parsed of
      Left err -> assert (show err) false
      Right root -> case bigBangEffects root of
        [ Let (LetStep { stages }) ] ->
          equal [ 1, 1, 1, 3, 27, 1, 3 ] (map length stages)
        _ -> assert "Expected the staged Big Bang context action" false

  test "bespoke database settles owner, creation and publication before signaling completion" do
    source <- readTextFile UTF8 "src/model/repositoryTools@1.0.arc"
    parsed <- runIndentParser source (domain <* eof)
    case parsed of
      Left err -> assert (show err) false
      Right root -> case contextEffects "CreateBigBangsDatabase" root of
        [ Let (LetStep { stages }) ] -> do
          equal [ 4, 1, 1, 1 ] (map length stages)
          case stages of
            [ _, [ PropertyAssignment endorsement ], [ PropertyAssignment publication ], [ PropertyAssignment completion ] ] ->
              equal [ "Endorsed", "Public", "Finished" ]
                [ endorsement.propertyIdentifier, publication.propertyIdentifier, completion.propertyIdentifier ]
            _ -> assert "Expected separate endorsement, publication and completion stages" false
        _ -> assert "Expected the staged bespoke database action" false

  test "browser preparation installs AMQPtestSetup before reporting success" do
    source <- readTextFile UTF8 "src/model/rebootUniverse@2.0.arc"
    parsed <- runIndentParser source (domain <* eof)
    case parsed of
      Left err -> assert (show err) false
      Right root -> case contextEffects "AddExtraModels" root of
        [ Let (LetStep { stages }) ] -> do
          equal [ 15, 1 ] (map length stages)
          assert "AMQPtestSetup must be installed alongside the other reboot inputs" $
            any
              ( case _ of
                  ExternalEffect { effectName, arguments: [ Simple (Variable _ name) ] } ->
                    effectName == "cdb:AddModelToLocalStore" && name == "amqptestsetupmodeluri"
                  _ -> false
              )
              (concat stages)
        _ -> assert "Expected the staged extra-model preparation action" false

bigBangEffects :: ContextE -> Array Statements
bigBangEffects = contextEffects "ExecuteBigBang"

contextEffects :: String -> ContextE -> Array Statements
contextEffects target (ContextE { contextParts }) = fromFoldable contextParts >>= case _ of
  CE child@(ContextE { id, contextParts: parts }) ->
    if id == target then fromFoldable parts >>= case _ of
      RE (RoleE { roleParts }) -> fromFoldable roleParts >>= case _ of
        SQP qualified -> fromFoldable qualified >>= actionEffect
        ROLESTATE (StateE { stateParts }) -> fromFoldable stateParts >>= actionEffect
        _ -> []
      _ -> []
    else contextEffects target child
  _ -> []

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
  AE (AutomaticEffectE { effect }) -> [ effect ]
  _ -> []

automaticEffects :: ContextE -> Array Statements
automaticEffects (ContextE { contextParts }) = fromFoldable contextParts >>= case _ of
  CE child -> automaticEffects child
  CSQP parts -> fromFoldable parts >>= actionEffect
  STATE state -> stateEffects state
  RE (RoleE { roleParts }) -> fromFoldable roleParts >>= case _ of
    SQP parts -> fromFoldable parts >>= actionEffect
    ROLESTATE state -> stateEffects state
    _ -> []
  _ -> []

stateEffects :: StateE -> Array Statements
stateEffects (StateE { stateParts, subStates }) =
  (fromFoldable stateParts >>= actionEffect) <> (fromFoldable subStates >>= stateEffects)

propertyName :: Assignment -> String
propertyName = case _ of
  PropertyAssignment { propertyIdentifier } -> propertyIdentifier
  _ -> ""

adminLifecycleStates :: RolePart -> Array StateE
adminLifecycleStates = case _ of
  ROLESTATE state -> findLifecycle state
  _ -> []
  where
  findLifecycle state@(StateE { stateParts, subStates }) =
    if any hasAdminGrant (fromFoldable stateParts >>= actionEffect) then [ state ]
    else fromFoldable subStates >>= findLifecycle

  hasAdminGrant = case _ of
    Let (LetStep { stages }) -> any
      ( case _ of
          ExternalEffect { effectName } -> effectName == "cdb:MakeAdminOfDb"
          _ -> false
      )
      (concat stages)
    _ -> false

concatMapPropertyNames :: Array Statements -> Array String
concatMapPropertyNames effects = effects >>= case _ of
  Statements assignments -> assignments >>= assignedProperty
  Let (LetStep { stages }) -> concat stages >>= assignedProperty
  where
  assignedProperty assignment = case assignment of
    PropertyAssignment { propertyIdentifier } -> [ propertyIdentifier ]
    _ -> []