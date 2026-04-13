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
-- Full text of this license can be found in the LICENSE directory in the
-- projects root.

-- END LICENSE

-- | Comprehensive unit tests for the ARC parser (non-expression syntax).
-- |
-- | Covers all constructs in the Perspectives Language *except* the query
-- | expression sub-language, which is already tested in
-- | `Test.Parsing.Arc.Expression`.
-- |
-- | The tests exercise parsers from `Perspectives.Parsing.Arc` directly so
-- | they run as pure in-memory computations (Layer 1).

module Test.Parsing.Arc.Syntax where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.List (List(..), filter, find, findIndex, head, length, (:))
import Data.List.NonEmpty (length) as LNE
import Data.Maybe (Maybe(..), isJust, isNothing)
import Parsing (ParseError(..))
import Perspectives.Parsing.Arc (automaticEffectE, contextE, domain, propertyE, thingRoleE, userRoleE, viewE)
import Perspectives.Parsing.Arc.AST (ContextE(..), ContextPart(..), FilledBySpecification(..), PropertyE(..), PropertyFacet(..), PropertyPart(..), PropsOrView(..), RoleE(..), RolePart(..), StateSpecification(..), ViewE(..))
import Perspectives.Parsing.Arc.Identifiers (arcIdentifier)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.Token (reservedIdentifier)
import Perspectives.Representation.Context (ContextKind(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), RoleKind(..))
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerbList(..))
import Perspectives.Representation.Verbs (RoleVerb(..), PropertyVerb(..)) as RV
import Test.Parsing.ArcAstSelectors (actionExists, allPropertyVerbs, ensureAction, ensureContext, ensureOnEntry, ensureOnExit, ensurePerspectiveOf, ensurePerspectiveOn, ensurePropertyVerbsForPropsOrView, ensureRoleVerbs, ensureStateInContext, ensureStateInRole, ensureSubState, ensureUserRole, failure, hasAutomaticAction, isImplicitRoleOnIdentifier, isIndexed, isNotified, isStateWithContext, isStateWithExplicitRole, isStateWithExplicitRole_, perspectiveExists, stateExists, stateParts)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc.Syntax" do

  --------------------------------------------------------------------------------
  ---- ARCIDENTIFIER
  --------------------------------------------------------------------------------
  suite "ArcIdentifier" do

    test "Simple capitalised name" do
      r <- runIndentParser "MyTestDomain" arcIdentifier
      case r of
        Left e -> assert (show e) false
        Right id -> assert "Expected 'MyTestDomain'" (id == "MyTestDomain")

    test "Uncapitalised name fails" do
      (r :: Either ParseError String) <- runIndentParser "myTestDomain" arcIdentifier
      case r of
        Left _ -> pure unit
        Right _ -> assert "Uncapitalised name should not parse as ArcIdentifier" false

    test "Segmented name" do
      r <- runIndentParser "MyTestDomain$Context" arcIdentifier
      case r of
        Left e -> assert (show e) false
        Right id -> assert "Expected 'MyTestDomain$Context'" (id == "MyTestDomain$Context")

    test "Prefixed name" do
      r <- runIndentParser "pre:MyTestDomain" arcIdentifier
      case r of
        Left e -> assert (show e) false
        Right id -> assert "Expected 'pre:MyTestDomain'" (id == "pre:MyTestDomain")

    test "Prefixed segmented name" do
      r <- runIndentParser "pre:MyTestDomain$Context$Role" arcIdentifier
      case r of
        Left e -> assert (show e) false
        Right id -> assert "Expected 'pre:MyTestDomain$Context$Role'" (id == "pre:MyTestDomain$Context$Role")

    test "Fully qualified model URI" do
      r <- runIndentParser "model:MyTestDomain$Context" arcIdentifier
      case r of
        Left e -> assert (show e) false
        Right id -> assert "Expected 'model:MyTestDomain$Context'" (id == "model:MyTestDomain$Context")

  --------------------------------------------------------------------------------
  ---- RESERVEDIDENTIFIER
  --------------------------------------------------------------------------------
  suite "ReservedIdentifier" do

    test "Known reserved word succeeds" do
      r <- runIndentParser "entry" reservedIdentifier
      case r of
        Left e -> assert (show e) false
        Right id -> assert "Expected 'entry'" (id == "entry")

    test "Unknown word fails with descriptive message" do
      (r :: Either ParseError String) <- runIndentParser "cheese" reservedIdentifier
      case r of
        Left (ParseError m _) -> assert (show m) (m == "not a reserved word \"cheese\"(or unexpected end of input), ")
        Right _ -> assert "'cheese' should not parse as a reserved identifier" false

  --------------------------------------------------------------------------------
  ---- DOMAIN (top-level context declarations)
  --------------------------------------------------------------------------------
  suite "Domain" do

    test "Minimal domain declaration" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n" domain
      case r of
        Left e -> assert (show e) false
        Right (ContextE { id, kindOfContext }) -> do
          assert "id should be 'model://perspectives.domains#Test'" (id == "model://perspectives.domains#Test")
          assert "kind should be Domain" (kindOfContext == Domain)

    test "Empty string fails" do
      (r :: Either ParseError ContextE) <- runIndentParser "" domain
      case r of
        Left _ -> pure unit
        Right _ -> assert "Empty input should not parse as domain" false

    test "Case context at top level fails (must be domain)" do
      (r :: Either ParseError ContextE) <- runIndentParser "case MyCase\n" domain
      case r of
        Left _ -> pure unit
        Right _ -> assert "A case at the top level should fail: must use 'domain'" false

    test "Domain with a thing role" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  thing MyRole\n" domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          case find (case _ of
            RE (RoleE { id, kindOfRole }) -> id == "MyRole" && kindOfRole == RoleInContext
            _ -> false) ((\(ContextE c) -> c.contextParts) dom) of
            Nothing -> assert "Expected a thing role 'MyRole'" false
            Just _ -> pure unit

    test "Domain with a user role" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  user MyUser\n" domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          case find (case _ of
            RE (RoleE { id, kindOfRole }) -> id == "MyUser" && kindOfRole == UserRole
            _ -> false) ((\(ContextE c) -> c.contextParts) dom) of
            Nothing -> assert "Expected a user role 'MyUser'" false
            Just _ -> pure unit

    test "Domain with a context role" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  context MyCtxRole\n" domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          case find (case _ of
            RE (RoleE { id, kindOfRole }) -> id == "MyCtxRole" && kindOfRole == ContextRole
            _ -> false) ((\(ContextE c) -> c.contextParts) dom) of
            Nothing -> assert "Expected a context role 'MyCtxRole'" false
            Just _ -> pure unit

    test "Domain with an external role" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  external\n    property ExtProp (String)\n" domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          case find (case _ of
            RE (RoleE { id, kindOfRole }) -> id == "External" && kindOfRole == ExternalRole
            _ -> false) ((\(ContextE c) -> c.contextParts) dom) of
            Nothing -> assert "Expected an external role" false
            Just _ -> pure unit

    test "Domain with nested case" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  case MyCase\n" domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          case find (case _ of
            CE (ContextE { id, kindOfContext }) -> id == "MyCase" && kindOfContext == Case
            _ -> false) ((\(ContextE c) -> c.contextParts) dom) of
            Nothing -> assert "Expected a nested case 'MyCase'" false
            Just _ -> pure unit

    test "Domain with nested party" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  party MyParty\n" domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          case find (case _ of
            CE (ContextE { id, kindOfContext }) -> id == "MyParty" && kindOfContext == Party
            _ -> false) ((\(ContextE c) -> c.contextParts) dom) of
            Nothing -> assert "Expected a nested party 'MyParty'" false
            Just _ -> pure unit

    test "Domain with nested activity" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  activity MyActivity\n" domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          case find (case _ of
            CE (ContextE { id, kindOfContext }) -> id == "MyActivity" && kindOfContext == Activity
            _ -> false) ((\(ContextE c) -> c.contextParts) dom) of
            Nothing -> assert "Expected a nested activity 'MyActivity'" false
            Just _ -> pure unit

    test "Line comment '-- ...' is ignored" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  -- thing CommentedOut\n  thing MyRole\n" domain
      case r of
        Left e -> assert (show e) false
        Right dom -> do
          let parts = (\(ContextE c) -> c.contextParts) dom
          case find (case _ of
            RE (RoleE { id }) -> id == "CommentedOut"
            _ -> false) parts of
            Just _ -> assert "Commented-out role should not appear" false
            Nothing -> pure unit

  --------------------------------------------------------------------------------
  ---- CONTEXT PARTS: use, indexed, aspect
  --------------------------------------------------------------------------------
  suite "Context parts" do

    test "use clause introduces a prefix" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  use sys for model://perspectives.domains#System\n" domain
      case r of
        Left e -> assert (show e) false
        Right (ContextE { contextParts }) ->
          case find (case _ of
            PREFIX "sys" "model://perspectives.domains#System" -> true
            _ -> false) contextParts of
            Nothing -> assert "Expected prefix 'sys' -> 'model://perspectives.domains#System'" false
            Just _ -> pure unit

    test "use clause with invalid model name fails" do
      (r :: Either ParseError ContextE) <- runIndentParser "domain model://perspectives.domains#Test\n  use sys for NotAModel\n" domain
      case r of
        Left _ -> pure unit
        Right _ -> assert "Non-model-URI in use clause should fail" false

    test "indexed clause in context" do
      r <- runIndentParser "domain MyContext\n  indexed IndexedName\n" contextE
      case r of
        Left e -> assert (show e) false
        Right (CE ctxt) -> isIndexed "IndexedName" ctxt
        _ -> failure "Expected a ContextPart that is a ContextE"

    test "aspect clause in context" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  aspect pre:MyAspect\n" domain
      case r of
        Left e -> assert (show e) false
        Right (ContextE { contextParts }) ->
          case find (case _ of
            ContextAspect "pre:MyAspect" _ -> true
            _ -> false) contextParts of
            Nothing -> assert "Expected ContextAspect 'pre:MyAspect'" false
            Just _ -> pure unit

    test "aspect role clause in context" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  aspect user pre:MyAspect$MyUser\n" domain
      case r of
        Left e -> assert (show e) false
        Right (ContextE { contextParts }) ->
          case find (case _ of
            AspectRole "pre:MyAspect$MyUser" UserRole _ -> true
            _ -> false) contextParts of
            Nothing -> assert "Expected AspectRole" false
            Just _ -> pure unit

  --------------------------------------------------------------------------------
  ---- ENUMERATED ROLES
  --------------------------------------------------------------------------------
  suite "Enumerated roles" do

    test "Minimal thing role" do
      r <- runIndentParser "thing MyRole\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { id, kindOfRole })) -> do
          assert "id should be 'MyRole'" (id == "MyRole")
          assert "kind should be RoleInContext" (kindOfRole == RoleInContext)
        _ -> assert "Expected RE RoleE" false

    test "Thing role with mandatory attribute" do
      r <- runIndentParser "thing MyRole (mandatory)\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have MandatoryAttribute true" $
            isJust (findIndex (case _ of
              MandatoryAttribute true -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

    test "Thing role with functional attribute" do
      r <- runIndentParser "thing MyRole (functional)\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have FunctionalAttribute true" $
            isJust (findIndex (case _ of
              FunctionalAttribute true -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

    test "Thing role with relational attribute" do
      r <- runIndentParser "thing MyRole (relational)\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have FunctionalAttribute false (relational)" $
            isJust (findIndex (case _ of
              FunctionalAttribute false -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

    test "Thing role with unlinked attribute" do
      r <- runIndentParser "thing MyRole (unlinked)\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have UnlinkedAttribute" $
            isJust (findIndex (case _ of
              UnlinkedAttribute -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

    test "Thing role with mandatory and relational attributes" do
      r <- runIndentParser "thing MyRole (mandatory, relational)\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) -> do
          assert "Should have MandatoryAttribute true" $
            isJust (findIndex (case _ of 
              MandatoryAttribute true -> true
              _ -> false) roleParts)
          assert "Should have FunctionalAttribute false (relational)" $
            isJust (findIndex (case _ of 
              FunctionalAttribute false -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

  --------------------------------------------------------------------------------
  ---- FILLEDBY CLAUSE
  --------------------------------------------------------------------------------
  suite "FilledBy" do

    test "filledBy single filler" do
      r <- runIndentParser "thing MyRole filledBy Host\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have FilledBySpecifications" $
            isJust (findIndex (case _ of 
              FilledBySpecifications _ -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

    test "filledBy multiple alternatives (comma-separated)" do
      r <- runIndentParser "thing MyRole filledBy (Host, Guest)\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have 2 alternatives" $
            (1 == length (filter (case _ of
              FilledBySpecifications (Alternatives atts) -> LNE.length atts == 2
              _ -> false) roleParts))
        _ -> assert "Expected RE RoleE" false

    test "filledBy combination (plus-separated)" do
      r <- runIndentParser "thing MyRole filledBy (Host + Guest)\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have Combination with 2 members" $
            isJust (findIndex (case _ of
              FilledBySpecifications (Combination atts) -> LNE.length atts == 2
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

    test "filledBy disjunction of conjunctions" do
      r <- runIndentParser "thing MyRole filledBy ((Host + Guest), Member)\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have DisjunctionOfConjunctions" $
            isJust (findIndex (case _ of
              FilledBySpecifications (DisjunctionOfConjunctions _) -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

    test "filledBy filler in a named context" do
      r <- runIndentParser "thing MyRole filledBy Host in MyContext\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have FilledBySpecifications" $
            isJust (findIndex (case _ of 
              FilledBySpecifications _ -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

  --------------------------------------------------------------------------------
  ---- CALCULATED ROLES
  --------------------------------------------------------------------------------
  suite "Calculated roles" do

    test "Calculated thing role" do
      r <- runIndentParser "thing MyRole = context >> SomeRole\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have a Calculation part" $
            isJust (findIndex (case _ of 
              Calculation _ _ -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

    test "Calculated thing role with functional attribute" do
      r <- runIndentParser "thing MyRole (functional) = context >> SomeRole\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have a Calculation part" $
            isJust (findIndex (case _ of 
              Calculation _ true -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

    test "Calculated user role" do
      r <- runIndentParser "domain model://perspectives.domains#Test\n  user GoedeGast = filter Gast with WellBehaved\n    perspective on Gast\n      all roleverbs\n" domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureUserRole "GoedeGast" dom >>= \rl ->
            assert "Should have a Calculation in role parts" $
              isJust (findIndex (case _ of
                Calculation _ _ -> true
                _ -> false) ((\(RoleE r) -> r.roleParts) rl))

  --------------------------------------------------------------------------------
  ---- ROLE ASPECTS
  --------------------------------------------------------------------------------
  suite "Role aspects" do

    test "Role aspect (simple)" do
      r <- runIndentParser "thing MyRole\n  aspect pre:MyAspectRole\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          case head (filter (case _ of 
              RoleAspect _ _ _ -> true
              _ -> false) roleParts) of
            Nothing -> assert "Expected a RoleAspect part" false
            Just (RoleAspect id _ mMapping) -> do
              assert "Aspect id should be 'pre:MyAspectRole'" (id == "pre:MyAspectRole")
              assert "No property mapping expected" (isNothing mMapping)
            _ -> assert "Expected RoleAspect" false
        _ -> assert "Expected RE RoleE" false

    test "Role aspect with property mapping" do
      r <- runIndentParser "thing MyRole\n  aspect pre:MyAspectRole where\n    OldProp is replaced by NewProp\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          case head (filter (case _ of 
              RoleAspect _ _ _ -> true
              _ -> false) roleParts) of
            Nothing -> assert "Expected a RoleAspect part" false
            Just (RoleAspect _ _ mMapping) ->
              assert "Property mapping should be present" (isJust mMapping)
            _ -> assert "Expected RoleAspect" false
        _ -> assert "Expected RE RoleE" false

  --------------------------------------------------------------------------------
  ---- ROLE INDEXED
  --------------------------------------------------------------------------------
  suite "Role indexed" do

    test "Indexed role" do
      r <- runIndentParser "thing MyRole\n  indexed TheRole\n" thingRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE (RoleE { roleParts })) ->
          assert "Should have IndexedRole 'TheRole'" $
            isJust (findIndex (case _ of 
              IndexedRole "TheRole" _ -> true
              _ -> false) roleParts)
        _ -> assert "Expected RE RoleE" false

  --------------------------------------------------------------------------------
  ---- PROPERTIES
  --------------------------------------------------------------------------------
  suite "Properties" do

    test "Enumerated property with String range" do
      r <- runIndentParser "property MyProp (String)\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { id, range })) -> do
          assert "id should be 'MyProp'" (id == "MyProp")
          assert "range should be PString" (range == Just PString)
        _ -> assert "Expected PE PropertyE" false

    test "Enumerated property with Number range" do
      r <- runIndentParser "property MyProp (Number)\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { range })) ->
          assert "range should be PNumber" (range == Just PNumber)
        _ -> assert "Expected PE PropertyE" false

    test "Enumerated property with Boolean range" do
      r <- runIndentParser "property MyProp (Boolean)\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { range })) ->
          assert "range should be PBool" (range == Just PBool)
        _ -> assert "Expected PE PropertyE" false

    test "Enumerated property with DateTime range" do
      r <- runIndentParser "property MyProp (DateTime)\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { range })) ->
          assert "range should be PDateTime" (range == Just PDateTime)
        _ -> assert "Expected PE PropertyE" false

    test "Enumerated property with Email range" do
      r <- runIndentParser "property MyProp (Email)\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { range })) ->
          assert "range should be PEmail" (range == Just PEmail)
        _ -> assert "Expected PE PropertyE" false

    test "Property with mandatory attribute" do
      r <- runIndentParser "property MyProp (mandatory, String)\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyParts })) ->
          assert "Should have MandatoryAttribute' true" $
            isJust (findIndex (case _ of 
              MandatoryAttribute' true -> true
              _ -> false) propertyParts)
        _ -> assert "Expected PE PropertyE" false

    test "Property with relational attribute" do
      r <- runIndentParser "property MyProp (relational, String)\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyParts })) ->
          assert "Should have FunctionalAttribute' false" $
            isJust (findIndex (case _ of 
              FunctionalAttribute' false -> true
              _ -> false) propertyParts)
        _ -> assert "Expected PE PropertyE" false

    test "Property with selfonly attribute" do
      r <- runIndentParser "property MyProp (selfonly, String)\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyParts })) ->
          assert "Should have SelfonlyAttribute" $
            isJust (findIndex (case _ of 
              SelfonlyAttribute -> true
              _ -> false) propertyParts)
        _ -> assert "Expected PE PropertyE" false

    test "Property with authoronly attribute" do
      r <- runIndentParser "property MyProp (authoronly, String)\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyParts })) ->
          assert "Should have AuthoronlyAttribute" $
            isJust (findIndex (case _ of 
              AuthoronlyAttribute -> true
              _ -> false) propertyParts)
        _ -> assert "Expected PE PropertyE" false

    test "Calculated property" do
      r <- runIndentParser "property MyProp = SomeRole >> SomeOtherProp\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyParts })) ->
          assert "Should have Calculation'" $
            isJust (findIndex (case _ of 
              Calculation' _ _ -> true
              _ -> false) propertyParts)
        _ -> assert "Expected PE PropertyE" false

    test "Calculated property with functional attribute" do
      r <- runIndentParser "property MyProp (functional) = SomeRole >> SomeOtherProp\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyParts })) ->
          assert "Should have functional Calculation'" $
            isJust (findIndex (case _ of 
              Calculation' _ true -> true 
              _ -> false) propertyParts)
        _ -> assert "Expected PE PropertyE" false

    test "Property without explicit range" do
      r <- runIndentParser "property MyProp\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { range })) ->
          assert "range should be Nothing when unspecified" (isNothing range)
        _ -> assert "Expected PE PropertyE" false

  --------------------------------------------------------------------------------
  ---- PROPERTY FACETS
  --------------------------------------------------------------------------------
  suite "Property facets" do

    test "minLength facet" do
      r <- runIndentParser "property MyProp (String)\n  minLength = 3\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyFacets })) ->
          assert "Should have MinLength 3" $
            isJust (findIndex (case _ of 
              MinLength 3 -> true
              _ -> false) propertyFacets)
        _ -> assert "Expected PE PropertyE" false

    test "maxLength facet" do
      r <- runIndentParser "property MyProp (String)\n  maxLength = 100\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyFacets })) ->
          assert "Should have MaxLength 100" $
            isJust (findIndex (case _ of 
              MaxLength 100 -> true
              _ -> false) propertyFacets)
        _ -> assert "Expected PE PropertyE" false

    test "enumeration facet" do
      r <- runIndentParser "property MyProp (String)\n  enumeration = (\"red\", \"green\", \"blue\")\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyFacets })) ->
          assert "Should have Enumeration" $
            isJust (findIndex (case _ of 
              Enumeration _ -> true
              _ -> false) propertyFacets)
        _ -> assert "Expected PE PropertyE" false

    test "minInclusive facet on Number property" do
      r <- runIndentParser "property MyProp (Number)\n  minInclusive = 0\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyFacets })) ->
          assert "Should have MinInclusive" $
            isJust (findIndex (case _ of 
              MinInclusive _ -> true
              _ -> false) propertyFacets)
        _ -> assert "Expected PE PropertyE" false

    test "maxInclusive facet on Number property" do
      r <- runIndentParser "property MyProp (Number)\n  maxInclusive = 100\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyFacets })) ->
          assert "Should have MaxInclusive" $
            isJust (findIndex (case _ of 
              MaxInclusive _ -> true
              _ -> false) propertyFacets)
        _ -> assert "Expected PE PropertyE" false

    test "messageProperty facet" do
      r <- runIndentParser "property MyProp (String)\n  messageProperty\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyFacets })) ->
          assert "Should have MessageProperty" $
            isJust (findIndex (case _ of 
              MessageProperty -> true
              _ -> false) propertyFacets)
        _ -> assert "Expected PE PropertyE" false

    test "mediaProperty facet" do
      r <- runIndentParser "property MyProp (String)\n  mediaProperty\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyFacets })) ->
          assert "Should have MediaProperty" $
            isJust (findIndex (case _ of 
              MediaProperty -> true
              _ -> false) propertyFacets)
        _ -> assert "Expected PE PropertyE" false

    test "readableName facet" do
      r <- runIndentParser "property MyProp (String)\n  readableName\n" propertyE
      case r of
        Left e -> assert (show e) false
        Right (PE (PropertyE { propertyFacets })) ->
          assert "Should have ReadableNameProperty" $
            isJust (findIndex (case _ of 
              ReadableNameProperty -> true
              _ -> false) propertyFacets)
        _ -> assert "Expected PE PropertyE" false

  --------------------------------------------------------------------------------
  ---- VIEWS
  --------------------------------------------------------------------------------
  suite "Views" do

    test "View with two property references" do
      r <- runIndentParser "view MyView (P1, P2)\n" viewE
      case r of
        Left e -> assert (show e) false
        Right (VE (ViewE { id, viewParts })) -> do
          assert "id should be 'MyView'" (id == "MyView")
          assert "Should have 'P1'" $
            isJust (findIndex (_ == "P1") viewParts)
          assert "Should have 'P2'" $
            isJust (findIndex (_ == "P2") viewParts)
        _ -> assert "Expected VE ViewE" false

    test "View with one property reference" do
      r <- runIndentParser "view SingleView (OneProp)\n" viewE
      case r of
        Left e -> assert (show e) false
        Right (VE (ViewE { viewParts })) ->
          assert "Should have exactly one property" (length viewParts == 1)
        _ -> assert "Expected VE ViewE" false

  --------------------------------------------------------------------------------
  ---- PERSPECTIVES: perspective on
  --------------------------------------------------------------------------------
  suite "Perspective on" do

    test "perspective on with all roleverbs" do
      r <- runIndentParser
        "domain model://perspectives.domains#Feest\n  user GoedeGast = filter Gast with WellBehaved\n    perspective on Gast >> binding\n      only (Create)\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureUserRole "GoedeGast" dom >>=
            ensureStateInRole (isStateWithContext "model://perspectives.domains#Feest") >>=
              perspectiveExists

    test "perspective on External" do
      r <- runIndentParser "user MyRole\n  perspective on External\n    all roleverbs\n" userRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE rl) ->
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "External" >>= perspectiveExists
        _ -> assert "Expected RE RoleE" false

  --------------------------------------------------------------------------------
  ---- PERSPECTIVES: role verbs
  --------------------------------------------------------------------------------
  suite "Role verbs" do

    test "only (Create, Remove)" do
      r <- runIndentParser
        "user MyRole\n  perspective on MyObject\n    only (Create, Remove)\n"
        userRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE rl) ->
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "MyObject" >>=
              ensureRoleVerbs (Including [RV.Create, RV.Remove])
        _ -> assert "Expected RE RoleE" false

    test "except (Delete)" do
      r <- runIndentParser
        "user MyRole\n  perspective on MyObject\n    except (Delete)\n"
        userRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE rl) ->
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "MyObject" >>=
              ensureRoleVerbs (Excluding [RV.Delete])
        _ -> assert "Expected RE RoleE" false

    test "all roleverbs" do
      r <- runIndentParser
        "user MyRole\n  perspective on MyObject\n    all roleverbs\n"
        userRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE rl) ->
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "MyObject" >>=
              ensureRoleVerbs All
        _ -> assert "Expected RE RoleE" false

    test "no roleverbs" do
      r <- runIndentParser
        "user MyRole\n  perspective on MyObject\n    no roleverbs\n"
        userRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE rl) ->
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "MyObject" >>=
              ensureRoleVerbs None
        _ -> assert "Expected RE RoleE" false

  --------------------------------------------------------------------------------
  ---- PERSPECTIVES: property verbs
  --------------------------------------------------------------------------------
  suite "Property verbs" do

    test "view MyView (Consult)" do
      r <- runIndentParser
        "user MyRole\n  perspective on MyObject\n    view MyView (Consult)\n"
        userRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE rl) ->
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "MyObject" >>=
              ensurePropertyVerbsForPropsOrView [Consult] (View "MyView")
        _ -> assert "Expected RE RoleE" false

    test "all props" do
      r <- runIndentParser
        "user MyRole\n  perspective on MyObject\n    all props\n"
        userRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE rl) ->
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "MyObject" >>=
              ensurePropertyVerbsForPropsOrView allPropertyVerbs AllProperties
        _ -> assert "Expected RE RoleE" false

    test "props (P1) verbs (SetPropertyValue)" do
      r <- runIndentParser
        "user MyRole\n  perspective on MyObject\n    props (P1) verbs (SetPropertyValue)\n"
        userRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE rl) ->
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "MyObject" >>=
              ensurePropertyVerbsForPropsOrView [SetPropertyValue] (Properties ("P1" : Nil))
        _ -> assert "Expected RE RoleE" false

    test "props (P1, P2) verbs (AddPropertyValue, RemovePropertyValue)" do
      r <- runIndentParser
        "user MyRole\n  perspective on MyObject\n    props (P1, P2) verbs (AddPropertyValue, RemovePropertyValue)\n"
        userRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE rl) ->
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "MyObject" >>=
              ensurePropertyVerbsForPropsOrView [RV.AddPropertyValue, RV.RemovePropertyValue] (Properties ("P1" : "P2" : Nil))
        _ -> assert "Expected RE RoleE" false

    test "defaults yields all role verbs and all property verbs" do
      r <- runIndentParser
        "user MyRole\n  perspective on MyObject\n    defaults\n"
        userRoleE
      case r of
        Left e -> assert (show e) false
        Right (RE rl) -> do
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "MyObject" >>=
              ensureRoleVerbs All
          ensureStateInRole (isStateWithContext "model:") rl >>=
            ensurePerspectiveOn "MyObject" >>=
              ensurePropertyVerbsForPropsOrView allPropertyVerbs AllProperties
        _ -> assert "Expected RE RoleE" false

  --------------------------------------------------------------------------------
  ---- PERSPECTIVE OF
  --------------------------------------------------------------------------------
  suite "Perspective of" do

    test "perspective of introduces a subject" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    state SomeState = true\n      perspective on SomeRole\n        perspective of SomeUser\n          all roleverbs\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureStateInContext (ContextState (ContextType "model:MyDomain$MyCase") (Just "SomeState")) >>=
              stateParts >>=
                ensurePerspectiveOf (isImplicitRoleOnIdentifier "SomeUser") >>=
                  ensurePerspectiveOn "SomeRole" >>=
                    perspectiveExists

  --------------------------------------------------------------------------------
  ---- STATES
  --------------------------------------------------------------------------------
  suite "States" do

    test "Context with a named state" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    state SomeState = SomeRole >> SomeProp > 10\n      perspective on SomeRole\n        perspective of SomeUser\n          all roleverbs\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureStateInContext (ContextState (ContextType "model:MyDomain$MyCase") (Just "SomeState")) >>=
              stateExists

    test "State with a substate" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    state ParentState = true\n      state ChildState = true\n        perspective on SomeRole\n          perspective of SomeUser\n            all roleverbs\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureStateInContext (ContextState (ContextType "model:MyDomain$MyCase") (Just "ParentState")) >>=
              ensureSubState (ContextState (ContextType "model:MyDomain$MyCase") (Just "ParentState$ChildState")) >>=
                stateExists

    test "Role state declared with 'in state'" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    user SomeUser\n      in state SomeState\n        perspective on SomeRole\n          all roleverbs\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureUserRole "SomeUser" >>=
              ensureStateInRole (isStateWithExplicitRole_ "model:MyDomain$MyCase$SomeUser" (Just "SomeState")) >>=
                ensurePerspectiveOn "SomeRole" >>=
                  perspectiveExists

  --------------------------------------------------------------------------------
  ---- ON ENTRY / ON EXIT
  --------------------------------------------------------------------------------
  suite "On entry / on exit" do

    test "State with on entry notification" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    state SomeState = true\n      on entry\n        notify SomeUser \"Hello!\"\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureStateInContext (ContextState (ContextType "model:MyDomain$MyCase") (Just "SomeState")) >>=
              stateParts >>=
                ensureOnEntry >>=
                  isNotified "SomeUser"

    test "State with on exit automatic action" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    state SomeState = true\n      on exit\n        do for SomeUser\n          remove SomeRole\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureStateInContext (ContextState (ContextType "model:MyDomain$MyCase") (Just "SomeState")) >>=
              stateParts >>=
                ensureOnExit >>=
                  hasAutomaticAction

    test "Role on entry with notification" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    user Self\n      on entry\n        notify SomeUser \"Hi!\"\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureUserRole "Self" >>=
              ensureStateInRole (isStateWithExplicitRole_ "model:MyDomain$MyCase$Self" Nothing) >>=
                ensureOnEntry >>=
                  isNotified "SomeUser"

    test "Role on entry with automatic assignment" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    user Self\n      on entry\n        do\n          Prop1 = false for ARole\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureUserRole "Self" >>=
              ensureStateInRole (isStateWithExplicitRole_ "model:MyDomain$MyCase$Self" Nothing) >>=
                ensureOnEntry >>=
                  hasAutomaticAction

    test "on exit without subject fails" do
      (r :: Either ParseError ContextE) <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    state SomeState = true\n      on exit\n        do\n          remove SomeRole\n"
        domain
      case r of
        Left (ParseError m _) ->
          assert ("Should fail with 'A subject is required', got: " <> m)
            (m == "A subject is required")
        Right _ -> assert "Should have failed: no subject" false

    test "automaticEffectE without subject fails" do
      r <- runIndentParser "do\n  remove SomeRole\n" automaticEffectE
      case r of
        Left _ -> pure unit
        Right _ -> assert "Should fail without a subject" false

  --------------------------------------------------------------------------------
  ---- AUTOMATIC EFFECT TIMING
  --------------------------------------------------------------------------------
  suite "Automatic effect timing" do

    test "do after 5 Seconds" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    state SomeState = true\n      on entry\n        do for SomeUser after 5 Seconds\n          remove SomeRole\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureStateInContext (ContextState (ContextType "model:MyDomain$MyCase") (Just "SomeState")) >>=
              stateParts >>=
                ensureOnEntry >>=
                  hasAutomaticAction

    test "do every 1 Minutes" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    state SomeState = true\n      on entry\n        do for SomeUser every 1 Minutes\n          remove SomeRole\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureStateInContext (ContextState (ContextType "model:MyDomain$MyCase") (Just "SomeState")) >>=
              stateParts >>=
                ensureOnEntry >>=
                  hasAutomaticAction

    test "do after 2 Hours until 24 Hours every 1 Hours maximally 5 times" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    state SomeState = true\n      on entry\n        do for SomeUser after 2 Hours until 24 Hours every 1 Hours maximally 5 times\n          remove SomeRole\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureStateInContext (ContextState (ContextType "model:MyDomain$MyCase") (Just "SomeState")) >>=
              stateParts >>=
                ensureOnEntry >>=
                  hasAutomaticAction

    test "until without every fails" do
      (r :: Either ParseError ContextE) <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    state SomeState = true\n      on entry\n        do for SomeUser until 5 Hours\n          remove SomeRole\n"
        domain
      case r of
        Left _ -> pure unit
        Right _ -> assert "until without every should fail" false

  --------------------------------------------------------------------------------
  ---- ACTIONS
  --------------------------------------------------------------------------------
  suite "Actions" do

    test "Action with assignment statements" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    user SomeUser\n      in state SomeState\n        perspective on SomeRole\n          action MyAction\n            remove MyRole\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureUserRole "SomeUser" >>=
              ensureStateInRole (isStateWithExplicitRole_ "model:MyDomain$MyCase$SomeUser" (Just "SomeState")) >>=
                ensurePerspectiveOn "SomeRole" >>=
                  ensureAction "MyAction" >>=
                    actionExists

    test "Action with letA" do
      r <- runIndentParser
        "domain model://perspectives.domains#Test\n  case MyCase\n    user SomeUser\n      perspective on SomeRole\n        action LetAction\n          letA\n            a <- SomeRole\n          in\n            Prop = 1 for SomeRole\n"
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureContext "MyCase" dom >>=
            ensureUserRole "SomeUser" >>=
              ensureStateInRole (isStateWithContext "model:MyDomain$MyCase") >>=
                ensurePerspectiveOn "SomeRole" >>=
                  ensureAction "LetAction" >>=
                    actionExists

  --------------------------------------------------------------------------------
  ---- SCREENS: classic
  --------------------------------------------------------------------------------
  suite "Classic screens" do

    test "Screen with tabs" do
      r <- runIndentParser
        ( "domain model://perspectives.domains#Test\n"
            <> "  user MyUser\n"
            <> "    perspective on MyObject\n"
            <> "      all roleverbs\n"
            <> "    screen \"My Screen\"\n"
            <> "      tab \"Tab1\"\n"
            <> "        row\n"
            <> "          table MyObject\n"
        )
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureUserRole "MyUser" dom >>= \(RoleE { roleParts }) ->
            assert "Should have a Screen part" $
              isJust (findIndex (case _ of 
                Screen _ -> true
                _ -> false) roleParts)

    test "Screen with rows" do
      r <- runIndentParser
        ( "domain model://perspectives.domains#Test\n"
            <> "  user MyUser\n"
            <> "    perspective on MyObject\n"
            <> "      all roleverbs\n"
            <> "    screen \"My Screen\"\n"
            <> "      row\n"
            <> "        table MyObject\n"
        )
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureUserRole "MyUser" dom >>= \(RoleE { roleParts }) ->
            assert "Should have a Screen part" $
              isJust (findIndex (case _ of 
                Screen _ -> true
                _ -> false) roleParts)

    test "Screen with columns" do
      r <- runIndentParser
        ( "domain model://perspectives.domains#Test\n"
            <> "  user MyUser\n"
            <> "    perspective on MyObject\n"
            <> "      all roleverbs\n"
            <> "    screen \"My Screen\"\n"
            <> "      column\n"
            <> "        form MyObject\n"
        )
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureUserRole "MyUser" dom >>= \(RoleE { roleParts }) ->
            assert "Should have a Screen part" $
              isJust (findIndex (case _ of 
                Screen _ -> true
                _ -> false) roleParts)

  --------------------------------------------------------------------------------
  ---- SCREENS: who-what-where
  --------------------------------------------------------------------------------
  suite "Who-what-where screens" do

    test "Screen with who, what, where" do
      r <- runIndentParser
        ( "domain model://perspectives.domains#Test\n"
            <> "  user MyUser\n"
            <> "    perspective on MyObject\n"
            <> "      all roleverbs\n"
            <> "    screen\n"
            <> "      who\n"
            <> "        MyUser master\n"
            <> "          detail\n"
            <> "      what\n"
            <> "        MyObject master\n"
            <> "          detail\n"
            <> "      where\n"
            <> "        MyObject master\n"
            <> "          detail\n"
        )
        domain
      case r of
        Left e -> assert (show e) false
        Right dom ->
          ensureUserRole "MyUser" dom >>= \(RoleE { roleParts }) ->
            assert "Should have a Screen part" $
              isJust (findIndex (case _ of 
                Screen _ -> true
                _ -> false) roleParts)

  --------------------------------------------------------------------------------
  ---- COMPLETE DOMAIN EXAMPLE
  --------------------------------------------------------------------------------
  suite "Complete domain example" do

    test "Domain with many constructs parses successfully" do
      r <- runIndentParser
        ( "domain Feest\n"
            <> "  use sys for model://perspectives.domains#\n"
            <> "  aspect pre:MyAspect\n"
            <> "  thing Wens (mandatory)\n"
            <> "    property Naam (mandatory, String)\n"
            <> "    property Bedrag (Number)\n"
            <> "    view ViewOpWens (Naam, Bedrag)\n"
            <> "  user Gast filledBy sys:User\n"
            <> "    indexed TheGuest\n"
            <> "    property WellBehaved (Boolean)\n"
            <> "    perspective on Wens\n"
            <> "      view ViewOpWens (Consult, SetPropertyValue)\n"
            <> "  user GoedeGast = filter Gast with WellBehaved\n"
            <> "    perspective on Wens\n"
            <> "      only (Create, Remove)\n"
            <> "  external\n"
            <> "    property AantalGasten (mandatory, Number)\n"
            <> "  case Celebration\n"
        )
        domain
      case r of
        Left e -> assert (show e) false
        Right _ -> pure unit
