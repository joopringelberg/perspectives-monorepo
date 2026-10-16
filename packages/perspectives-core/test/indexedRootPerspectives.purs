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

module Test.Parsing.Arc.IndexedRootPerspectives where

import Prelude

import Data.Array (filter, length)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), contains, joinWith)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (fromFoldable)
import Perspectives.DomeinCache (storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFileRecord)
import Perspectives.ModelDependencies.Readable (perspectivesUsers, socialEnvironment, socialEnvironmentPersons, theWorld)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseThree.CheckPerspectivesModifiers (calculatesMe)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo')
import Perspectives.Parsing.Arc.Position (arcParserStartPosition)
import Perspectives.PerspectivesState (getWarnings)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Context (ContextKind(..), defaultContext)
import Perspectives.Representation.EnumeratedRole (defaultEnumeratedRole)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(UserRole))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Test.Perspectives.Utils (runP)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Indexed root context perspective warnings" do
  for_ [ "Items", "External", "Selection" ] \object ->
    test ("warns without rejecting compilation for peer properties on " <> object) $
      compileWithWarnings
        ( model true
            [ "  user Peer"
            , "    perspective on " <> object
            , "      props (Name) verbs (Consult)"
            ]
        )
        1

  test "warns for a calculated user that is not provably me" $
    compileWithWarnings
      ( model true
          [ "  user Participants (relational)"
          , "  user Peer = Participants"
          , "    perspective on Items"
          , "      props (Name) verbs (Consult)"
          ]
      )
      1

  test "warns for all properties" $
    compileWithWarnings
      (model true [ "  user Peer", "    perspective on Items", "      all props" ])
      1

  test "does not warn for role verbs alone" $
    compileWithWarnings
      (model true [ "  user Peer", "    perspective on Items", "      all roleverbs" ])
      0

  for_ [ "me", "Self", "filter me with true" ] \calculation ->
    test ("does not warn for a calculated local user: " <> calculation) $
      compileWithWarnings
        ( model true
            [ "  user Self = me"
            , "  user Peer = " <> calculation
            , "    perspective on Items"
            , "      props (Name) verbs (Consult)"
            ]
        )
        0

  test "does not warn for a non-indexed root context" $
    compileWithWarnings
      (model false [ "  user Peer", "    perspective on Items", "      all props" ])
      0

  test "does not warn for an indexed nested context" $
    compileWithWarnings
      ( joinWith "\n"
          [ "domain model://example.org#IndexedRoot"
          , "  case Nested"
          , "    indexed NestedInstance"
          , "    thing Items"
          , "      property Name (String)"
          , "    user Peer"
          , "      perspective on Items"
          , "        props (Name) verbs (Consult)"
          ]
      )
      0

  for_
    [ { name: "me", query: me, local: true }
    , { name: "filter me", query: binary (BinaryCombinator ComposeF) me (UQD dom FilterF other dom True True), local: true }
    , { name: "composition ending in me", query: binary (BinaryCombinator ComposeF) other me, local: true }
    , { name: "union of me", query: binary (BinaryCombinator UnionF) me me, local: true }
    , { name: "navigation away from me", query: binary (BinaryCombinator ComposeF) me other, local: false }
    , { name: "me only in a predicate", query: binary (BinaryCombinator ComposeF) other (UQD dom FilterF me dom True True), local: false }
    , { name: "union including a peer", query: binary (BinaryCombinator UnionF) me other, local: false }
    , { name: "framed me", query: UQD dom WithFrame me dom True True, local: true }
    , { name: "sequence ending in me", query: binary (BinaryCombinator SequenceF) other me, local: true }
    ]
    \{ name, query, local } -> test ("local-user proof: " <> name) do
      actual <- runP $ calculatesMe query
      equal local actual

model :: Boolean -> Array String -> String
model indexed users = joinWith "\n" $
  [ "domain model://example.org#IndexedRoot" ]
    <> (if indexed then [ "  indexed RootInstance" ] else [])
    <>
      [ "  external"
      , "    property Name (String)"
      , "  thing Items"
      , "    property Name (String)"
      , "  thing Selection = Items"
      ]
    <> users

compileWithWarnings :: String -> Int -> Test
compileWithWarnings source expected = do
  parsed <- runIndentParser source domain
  case parsed of
    Left err -> assert (show err) false
    Right root -> do
      Tuple result state <- runPhaseTwo' (traverseDomain root)
      case result of
        Left err -> assert (show err) false
        Right (DomeinFile dfr) -> do
          Tuple compiled warnings <- runP do
            -- `me` has System role types as its range; cache just those types for offline compilation.
            let systemModelName = "model://perspectives.domains#System"
            void $ storeDomeinFileInCache (ModelUri systemModelName) $ DomeinFile
              ( defaultDomeinFileRecord
                  { id = ModelUri systemModelName
                  , namespace = ModelUri systemModelName
                  , contexts = fromFoldable $
                      (\name -> Tuple name (defaultContext name name Case Nothing arcParserStartPosition Nothing))
                        <$> [ socialEnvironment, theWorld ]
                  , enumeratedRoles = fromFoldable
                      [ Tuple socialEnvironmentPersons (defaultEnumeratedRole socialEnvironmentPersons "Persons" UserRole socialEnvironment arcParserStartPosition)
                      , Tuple perspectivesUsers (defaultEnumeratedRole perspectivesUsers "PerspectivesUsers" UserRole theWorld arcParserStartPosition)
                      ]
                  }
              )
            compiled <- phaseThree dfr state.postponedStateQualifiedParts state.screens
            warnings <- getWarnings
            pure $ Tuple compiled warnings
          case compiled of
            Left err -> assert (show err) false
            Right _ -> do
              let relevant = filter (contains (Pattern "PeerPerspectiveOnIndexedRootContext") <<< _.message) warnings
              equal expected (length relevant)
              for_ relevant \warning -> do
                assert "Warning identifies the user role" (contains (Pattern "$Peer") warning.message)
                assert "Warning identifies the indexed root context" (contains (Pattern "model://example.org#IndexedRoot") warning.message)
                assert "Warning explains that synchronization is not recommended" (contains (Pattern "not recommended") warning.message)

dom :: Domain
dom = CDOM $ SUM []

me :: QueryFunctionDescription
me = SQD dom (DataTypeGetter MeF) dom True True

other :: QueryFunctionDescription
other = SQD dom (DataTypeGetter ContextF) dom True True

binary :: QueryFunction -> QueryFunctionDescription -> QueryFunctionDescription -> QueryFunctionDescription
binary function left right = BQD dom function left right dom True True
