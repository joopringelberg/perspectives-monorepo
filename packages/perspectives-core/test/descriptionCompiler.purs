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
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <https://www.gnu.org/licenses/>.
-- END LICENSE

module Test.Query.DescriptionCompiler where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object (lookup)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo', toStableDomeinFile)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.SideCar.PhantomTypedNewtypes (Stable)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

modelUri :: String
modelUri = "model://perspectives.domains#DescriptionCompilerTest"

-- Unlike expression-parser tests, these assertions inspect typed compiler output.
compileModel
  :: String
  -> (Array PerspectivesError -> Aff Unit)
  -> (DomeinFileRecord Stable -> Aff Unit)
  -> Aff Unit
compileModel source onError onSuccess = do
  parsed <- runIndentParser ("domain " <> modelUri <> "\n" <> source) domain
  case parsed of
    Left err -> assert (show err) false
    Right root -> do
      Tuple result state <- runPhaseTwo' (traverseDomain root)
      case result of
        Left errors -> onError errors
        Right (DomeinFile df) -> do
          compiled <- runP $ phaseThree df state.postponedStateQualifiedParts Nil
          case compiled of
            Left errors -> onError errors
            Right (Tuple corrected _) -> onSuccess $ unwrap $ toStableDomeinFile (DomeinFile corrected)

roleQuery :: String -> DomeinFileRecord Stable -> Maybe QueryFunctionDescription
roleQuery name df = case lookup (modelUri <> "$" <> name) df.calculatedRoles of
  Just (CalculatedRole { calculation: Q query }) -> Just query
  _ -> Nothing

propertyQuery :: String -> DomeinFileRecord Stable -> Maybe QueryFunctionDescription
propertyQuery name df = case lookup (modelUri <> "$" <> name) df.calculatedProperties of
  Just (CalculatedProperty { calculation: Q query }) -> Just query
  _ -> Nothing

compiles
  :: String
  -> String
  -> (DomeinFileRecord Stable -> Maybe QueryFunctionDescription)
  -> (QueryFunctionDescription -> Boolean)
  -> Free TestF Unit
compiles title source select matches = test title $
  compileModel source
    (\errors -> assert (show errors) false)
    \df -> case select df of
      Nothing -> assert "Expected a compiled calculation" false
      Just query -> assert ("Unexpected typed query: " <> show query) (matches query)

rejects :: String -> String -> (PerspectivesError -> Boolean) -> Free TestF Unit
rejects title source matches = test title $
  compileModel source
    ( \errors -> do
        assert "Expected at least one compiler error" (not $ null errors)
        for_ errors \err -> assert ("Unexpected error: " <> show err) (matches err)
    )
    (\_ -> assert "Expected the compiler to reject this query" false)

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Query.DescriptionCompiler" do
  compiles "PublicRole retains its asserted role range"
    ("  case Box\n    thing Repository (mandatory)\n    thing ClaimedRepository = publicrole pub:cw_test#repository (" <> modelUri <> "$Box$Repository)")
    (roleQuery "Box$ClaimedRepository")
    case _ of
      SQD _ (PublicRole _ (Just _)) (RDOM (UET (RoleInContext { context: ContextType context, role: EnumeratedRoleType role }))) _ _ ->
        context == modelUri <> "$Box" && role == modelUri <> "$Box$Repository"
      _ -> false

  compiles "Role identifier resolves getter and range"
    "  thing Role = AnotherRole\n  thing AnotherRole (mandatory)"
    (roleQuery "Role")
    case _ of
      SQD _ (RolGetter (ENR (EnumeratedRoleType getter))) (RDOM (UET (RoleInContext { context: ContextType context, role: EnumeratedRoleType role }))) _ _ ->
        getter == modelUri <> "$AnotherRole" && context == modelUri && role == getter
      _ -> false

  rejects "Missing role is rejected"
    "  thing Role = AnotherRole"
    case _ of
      ContextHasNoRole _ _ _ _ -> true
      _ -> false

  compiles "Property identifier resolves getter and Boolean range"
    "  thing Role (mandatory)\n    property Prop1 = Prop2\n    property Prop2 (mandatory, Boolean)"
    (propertyQuery "Role$Prop1")
    case _ of
      SQD _ (PropertyGetter (ENP (EnumeratedPropertyType property))) (VDOM PBool _) _ _ ->
        property == modelUri <> "$Role$Prop2"
      _ -> false

  rejects "Missing property is rejected"
    "  thing Role (mandatory)\n    property Prop1 = Prop2"
    case _ of
      RoleHasNoProperty _ _ _ _ -> true
      _ -> false

  compiles "Numeric literal retains value and range"
    "  thing Role (mandatory)\n    property Prop1 = 1"
    (propertyQuery "Role$Prop1")
    case _ of
      SQD _ (Constant PNumber "1") (VDOM PNumber _) _ _ -> true
      _ -> false

  compiles "Binding compiles to a filler getter followed by property access"
    "  thing Role1 (mandatory) filledBy Role2\n    property Prop1 = binding >> Prop2\n  thing Role2 (mandatory)\n    property Prop2 (mandatory, Boolean)"
    (propertyQuery "Role1$Prop1")
    case _ of
      BQD _ (BinaryCombinator ComposeF) (SQD _ (DataTypeGetter FillerF) _ _ _) (SQD _ (PropertyGetter _) _ _ _) (VDOM PBool _) _ _ -> true
      _ -> false

  compiles "Binding without a filler restriction has an unrestricted role range"
    "  thing Role1 (mandatory)\n  thing Role2 = Role1 >> binding"
    (roleQuery "Role2")
    case _ of
      BQD _ (BinaryCombinator ComposeF) _ (SQD _ (DataTypeGetterWithParameter FillerF "direct") AnyRoleType _ _) AnyRoleType _ _ -> true
      _ -> false

  compiles "Binder resolves both the filled role and its context"
    "  thing Role1 (mandatory) filledBy Role2\n  thing Role2 (mandatory)\n  thing Role3 = Role2 >> binder Role1"
    (roleQuery "Role3")
    case _ of
      BQD _ (BinaryCombinator ComposeF) _ (SQD _ (FilledF (EnumeratedRoleType role) (ContextType context)) _ _ _) _ _ _ ->
        role == modelUri <> "$Role1" && context == modelUri
      _ -> false

  compiles "Context navigation preserves right-associated composition"
    "  thing Role1 (mandatory)\n  thing Role2 = Role1 >> context >> Role1"
    (roleQuery "Role2")
    case _ of
      BQD _ (BinaryCombinator ComposeF) _ (BQD _ (BinaryCombinator ComposeF) (SQD _ (DataTypeGetter ContextF) _ _ _) _ _ _ _) _ _ _ -> true
      _ -> false

  rejects "Context getter cannot be applied to a context"
    "  thing Role1 = context"
    (requiresDomain "role")

  rejects "Binder cannot be applied to a context"
    "  thing Role1 (mandatory)\n  thing Role2 = binder Role1"
    (requiresDomain "role")

  compiles "Extern compiles to the external-role getter"
    "  thing Role1 = extern"
    (roleQuery "Role1")
    case _ of
      SQD _ (DataTypeGetter ExternalRoleF) _ _ _ -> true
      _ -> false

  rejects "Extern cannot be applied to a role"
    "  thing Role1 = Role2 >> extern\n  thing Role2 (mandatory)"
    (requiresDomain "context")

  compiles "Identity is retained as the second composition operand"
    "  thing Guest (mandatory)\n  thing GuestToo = Guest >> this"
    (roleQuery "GuestToo")
    case _ of
      BQD _ (BinaryCombinator ComposeF) _ (SQD _ (DataTypeGetter IdentityF) _ _ _) _ _ _ -> true
      _ -> false

  for_
    [ { title: "Logical not", expression: "not Prop1", function: NotF }
    , { title: "Exists", expression: "exists Prop1", function: ExistsF }
    ]
    \{ title, expression, function } ->
      compiles (title <> " compiles to a Boolean unary query")
        ("  thing Role1 (mandatory)\n    property Prop1 (mandatory, Boolean)\n    property Prop2 = " <> expression)
        (propertyQuery "Role1$Prop2")
        case _ of
          UQD _ (UnaryCombinator actual) _ (VDOM PBool _) _ _ -> actual == function
          _ -> false

  rejects "Logical not rejects a String"
    "  thing Role1 (mandatory)\n    property Prop1 (String)\n    property Prop2 = not Prop1"
    case _ of
      NotABoolean _ -> true
      _ -> false

  compiles "Exists accepts a context and returns Boolean"
    "  thing Role1 (mandatory)\n    property Prop2 = exists context"
    (propertyQuery "Role1$Prop2")
    case _ of
      UQD _ (UnaryCombinator ExistsF) (SQD _ (DataTypeGetter ContextF) (CDOM _) _ _) (VDOM PBool _) _ _ -> true
      _ -> false

  compiles "Available compiles to a Boolean unary query"
    "  thing SomeOtherRole\n  thing Role1 (mandatory) filledBy SomeOtherRole\n    property HasBinding = available binding"
    (propertyQuery "Role1$HasBinding")
    case _ of
      UQD _ (UnaryCombinator AvailableF) _ (VDOM PBool _) _ _ -> true
      _ -> false

  rejects "Available rejects property values"
    "  thing Role1 (mandatory)\n    property Prop1 (String)\n    property Prop2 = available Prop1"
    case _ of
      ValueExpressionNotAllowed _ _ _ -> true
      _ -> false

  compiles "Composition retains the final Boolean range"
    "  thing Role1 (mandatory)\n    property Prop1 (Boolean)\n  thing Role3 (mandatory)\n    property Prop2 = context >> Role1 >> Prop1"
    (propertyQuery "Role3$Prop2")
    case _ of
      BQD _ (BinaryCombinator ComposeF) _ _ (VDOM PBool _) _ _ -> true
      _ -> false

  rejects "Composition rejects a context as binding's argument"
    "  thing Role1 (mandatory)\n    property Prop1 = context >> binding"
    (requiresDomain "role")

  for_ [ { expression: "Prop1 == Prop2", function: EqualsF }, { expression: "Prop1 and Prop2", function: AndF } ]
    \{ expression, function } ->
      compiles (expression <> " compiles to the typed binary operator")
        ("  thing Role1 (mandatory)\n    property Prop1 (mandatory, Boolean)\n    property Prop2 (mandatory, Boolean)\n    property Prop3 = " <> expression)
        (propertyQuery "Role1$Prop3")
        case _ of
          BQD _ (BinaryCombinator actual) _ _ (VDOM PBool _) _ _ -> actual == function
          _ -> false

  rejects "Comparison rejects incompatible value ranges"
    "  thing Role1 (mandatory)\n    property Prop1 (Boolean)\n    property Prop2 (String)\n    property Prop3 = Prop1 == Prop2"
    case _ of
      TypesCannotBeCompared _ _ _ -> true
      _ -> false

  rejects "Logical and rejects Number operands"
    "  thing Role1 (mandatory)\n    property Prop1 (Number)\n    property Prop2 (Number)\n    property Prop3 = Prop1 and Prop2"
    case _ of
      WrongTypeForOperator _ _ _ -> true
      _ -> false

  compiles "Filter composes the source with its compiled predicate"
    "  thing Role1 (mandatory)\n    property Prop1 (Boolean)\n  thing Role2 = filter Role1 with Prop1"
    (roleQuery "Role2")
    case _ of
      BQD _ (BinaryCombinator ComposeF) _ (UQD _ FilterF (SQD _ (PropertyGetter _) (VDOM PBool _) _ _) _ _ _) _ _ _ -> true
      _ -> false

  compiles "SelectFrom composes the source with a narrowed role-type filter"
    ("  thing Role1 (mandatory)\n  thing Role2 = selectFrom Role1 just Role1 in " <> modelUri)
    (roleQuery "Role2")
    case _ of
      BQD _ (BinaryCombinator ComposeF) _ (SQD _ (RoleTypeFilter _) (RDOM narrowed) _ _) (RDOM result) _ _ -> narrowed == result
      _ -> false

  compiles "Count aggregation has a numeric range"
    "  thing Guest (mandatory)\n    property NumberOfGuests = this >>= count"
    (propertyQuery "Guest$NumberOfGuests")
    case _ of
      BQD _ (BinaryCombinator ComposeSequenceF) (SQD _ (DataTypeGetter IdentityF) _ _ _) (SQD _ (UnaryCombinator CountF) (VDOM PNumber _) _ _) (VDOM PNumber _) _ _ -> true
      _ -> false

requiresDomain :: String -> PerspectivesError -> Boolean
requiresDomain expected = case _ of
  DomainTypeRequired actual _ _ _ -> actual == expected
  _ -> false
