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

module Test.Query.PropertyAnchors where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (any, concat, drop, filter, head, length, nub, null)
import Data.Either (Either(..))
import Data.Foldable (all, for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (error)
import Foreign.Object as Object
import Perspectives.ContextAndRole (addRol_gevuldeRollen, defaultContextRecord, defaultRolRecord, removeRol_gevuldeRollen)
import Perspectives.CoreTypes (MonadPerspectives, runMonadPerspectivesQuery, (##=))
import Perspectives.Data.EncodableMap as EM
import Perspectives.DependencyTracking.Dependency (findFilledRoleRequests, registerDependency, toAssumption)
import Perspectives.DomeinCache (storeDomeinFileInCache)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (getFilledRoles)
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..), startsWithFilter)
import Perspectives.InvertedQuery.Storable (StoredQueries)
import Perspectives.InvertedQueryKey (RunTimeInvertedQueryKey(..), deserializeInvertedQueryKey)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo', toStableDomeinFile)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Kinked (AnchoredQueryWithAKink, PropertyAnchor(..), invert, invertWithAnchors)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), RoleInContext(..), domain, makeComposition)
import Perspectives.Query.UnsafeCompiler (compileFunction)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Class.PersistentType (cacheInDomeinFile, getContext, getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Readable)
import Test.Perspectives.Utils (runP)
import Test.Unit (Test, TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Perspective property anchors" do
  test "Filter dependencies are not property anchors, even with the result's role type" do
    withInversions (makeComposition source filterStep) \queries -> do
      assert "The criterion path remains indexed" (not $ null queries)
      for_ queries \q@{ query: ZQ backward _ } -> do
        let propertyAnchors = anchorMarkers q
        assert "Criterion and result have the same role domain" (map domain backward == Just itemDomain)
        equal (Just Dependency) (head propertyAnchors)
        equal [ PerspectiveResult ] (drop 2 propertyAnchors)

  test "Nested filters retain the source property anchor" do
    withInversions (makeComposition source (makeComposition filterStep filterStep)) \queries -> do
      assert "There are nested filter dependencies" (not $ null queries)
      assert "Only the source, not either criterion, anchors properties" $
        any (\q -> anchorMarkers q == [ Dependency, ResultFilter, Dependency, ResultFilter, PerspectiveResult ]) queries

  test "Navigation after a filter moves the property anchor to the final result" do
    let binding = SQD itemDomain (DataTypeGetter FillerF) itemDomain True False
    withInversions (makeComposition source (makeComposition filterStep binding)) \queries -> do
      assert "The final binding result anchors properties" $
        any (\q -> head (anchorMarkers q) == Just PerspectiveResult) queries
      assert "The filtered intermediate role is not a property anchor"
        $ not
        $ any (\q -> head (drop 2 (anchorMarkers q)) == Just PerspectiveResult) queries

  for_
    [ { name: "direct dossier properties", selection: "filter Attendees >> binding with context >> Ambassador >> Approved" }
    , { name: "properties inherited through a filler", selection: "filter Attendees with binding >> context >> Ambassador >> Approved" }
    , { name: "navigation after a filter", selection: "(filter Attendees with binding >> context >> Ambassador >> Approved) >> binding" }
    ]
    \{ name, selection } -> test ("Compile and index " <> name) $
      withCompiledQueries selection \queries -> do
        assert "Ambassador role changes remain indexed" $
          hasContextKey (role "Dossier$Ambassador") queries
        assert "The criterion property remains indexed" $
          hasPropertyKey (property "Dossier$Ambassador$Approved") queries
        assert "The perspective's participant name is still available on dependency queries" $
          any
            ( \entry@{ query: InvertedQuery { statesPerProperty } } ->
                hasContextKey (role "Dossier$Ambassador") [ entry ]
                  && isJust (EM.lookup (ENP (property "Dossier$External$ParticipantName")) statesPerProperty)
            )
            queries

  test "An inherited property is indexed from the reconstructed filtered source" $
    withCompiledQueries "filter Attendees with binding >> Eligible" \queries -> do
      assert "Participant-name updates retain their property index" $
        hasPropertyKey (property "Dossier$External$ParticipantName") queries
      assert "Attendee binding changes retain their index" $
        hasBindingKey (role "Meeting$Attendees") queries
      assert "The source-side query still checks the filter when visibility is gained" $
        any (\{ query } -> startsWithFilter query) queries
      assert "There is also an unfiltered dependency query for visibility loss" $
        any (\{ query } -> not $ startsWithFilter query) queries

  test "Nested filters retain inherited property and source-binding indexes" $
    withCompiledQueries "filter (filter Attendees with binding >> Eligible) with binding >> Eligible" \queries -> do
      assert "The inherited result property is indexed" $
        hasPropertyKey (property "Dossier$External$ParticipantName") queries
      assert "The source binding is indexed" $
        hasBindingKey (role "Meeting$Attendees") queries

  test "A filledBy criterion does not look for participant names on a user" $
    withCompiledQueries "filter Attendees >> binding with context >> Ambassador filledBy this:CurrentPerson" \queries -> do
      assert "The ambassador dependency is still indexed" $
        hasContextKey (role "Dossier$Ambassador") queries

  test "Compiled property paths follow visibility changes while criterion dependencies remain reachable" $
    withCompiledModel "filter Attendees with binding >> Eligible" \dfr queries -> do
      results <- runP do
        void $ storeDomeinFileInCache (ModelUri modelName) (toStableDomeinFile $ DomeinFile dfr)
        void $ cacheEntity meetingId $ PerspectContext defaultContextRecord
          { id = meetingId
          , pspType = ContextType (modelName <> "$Meeting")
          , rolInContext = Object.singleton (unwrap $ role "Meeting$Attendees") [ attendeeId ]
          }
        void $ cacheEntity attendeeId $ PerspectRol defaultRolRecord
          { id = attendeeId
          , context = meetingId
          , pspType = role "Meeting$Attendees"
          , binding = Just dossierId
          }
        for [ false, true, false ] \eligible -> do
          dossier <- addRol_gevuldeRollen
            ( PerspectRol defaultRolRecord
                { id = dossierId
                , context = ContextInstance "property-anchor-dossier"
                , pspType = role "Dossier$External"
                , properties = Object.fromFoldable
                    [ Tuple (unwrap $ property "Dossier$External$Eligible") [ Value $ if eligible then "true" else "false" ]
                    , Tuple (unwrap $ property "Dossier$External$ParticipantName") [ Value "Participant" ]
                    ]
                }
            )
            (ContextType (modelName <> "$Meeting"))
            (role "Meeting$Attendees")
            attendeeId
          void $ cacheEntity dossierId dossier
          names <- evaluatePropertyQueries (property "Dossier$External$ParticipantName") queries
          dependencies <- evaluatePropertyQueries (property "Dossier$External$Eligible") queries
          pure { names, dependencies }
      equal [ [], [ unwrap meetingId ], [] ] (map _.names results)
      equal [ [ unwrap meetingId ], [ unwrap meetingId ], [ unwrap meetingId ] ] (map _.dependencies results)

  test "Binder lookup includes every role and context specialising the requested aspects" $
    withCompiledModel "Attendees >> binding" \dfr _ -> do
      result <- runP do
        void $ storeDomeinFileInCache (ModelUri modelName) (toStableDomeinFile $ DomeinFile dfr)
        let
          baseContext = ContextType (modelName <> "$Meeting")
          childContext = ContextType (modelName <> "$ChildMeeting")
          baseRole = role "Meeting$Attendees"
          firstRole = role "First"
          secondRole = role "Second"
          firstId = RoleInstance "binder-first"
          secondId = RoleInstance "binder-second"
          thirdId = RoleInstance "binder-third"
          aspect = RoleInContext { context: baseContext, role: baseRole }
        Context context <- getContext baseContext
        EnumeratedRole prototype <- getEnumeratedRole baseRole
        cacheInDomeinFile childContext (Context context { id = childContext, contextAspects = [ baseContext ] })
        cacheInDomeinFile firstRole (EnumeratedRole prototype { id = firstRole, roleAspects = [ aspect ] })
        cacheInDomeinFile secondRole (EnumeratedRole prototype { id = secondRole, roleAspects = [ aspect ] })
        let emptyFiller = PerspectRol defaultRolRecord { id = dossierId }
        void $ cacheEntity dossierId emptyFiller
        Tuple empty assumptions <- runMonadPerspectivesQuery dossierId (getFilledRoles baseContext baseRole)
        for_ (unwrap assumptions) \assumption -> registerDependency 42 (toAssumption assumption)
        first <- addRol_gevuldeRollen emptyFiller baseContext firstRole firstId
        second <- addRol_gevuldeRollen first baseContext secondRole secondId
        third <- addRol_gevuldeRollen second childContext secondRole thirdId
        let PerspectRol saved = third
        let
          stale = Object.insert (modelName <> "$MissingContext")
            (Object.singleton (modelName <> "$MissingRole") [ RoleInstance "stale" ])
            saved.filledRoles
        void $ cacheEntity dossierId (PerspectRol saved { filledRoles = stale, roleAliases = Object.empty, contextAliases = Object.empty })
        Tuple both _ <- runMonadPerspectivesQuery dossierId (getFilledRoles baseContext baseRole)
        binder <- compileFunction (SQD itemDomain (FilledF baseRole baseContext) itemDomain False False)
        compiled <- unwrap dossierId ##= binder
        firstRequests <- findFilledRoleRequests dossierId baseContext firstRole
        secondRequests <- findFilledRoleRequests dossierId childContext secondRole
        void $ cacheEntity dossierId (removeRol_gevuldeRollen third baseContext secondRole secondId)
        Tuple remaining _ <- runMonadPerspectivesQuery dossierId (getFilledRoles baseContext baseRole)
        pure { empty, both, compiled, remaining, firstRequests, secondRequests, firstId, secondId, thirdId }
      equal [] result.empty
      assert "Both concrete types are returned even with legacy aliases" $
        all (\id -> any (_ == id) result.both) [ result.firstId, result.secondId, result.thirdId ] && length result.both == 3
      assert "The compiled binder returns the same results" $
        all (\id -> any (_ == unwrap id) result.compiled) [ result.firstId, result.secondId, result.thirdId ] && length result.compiled == 3
      assert "Removing one binding preserves the others" $
        all (\id -> any (_ == id) result.remaining) [ result.firstId, result.thirdId ] && length result.remaining == 2
      equal [ 42 ] result.firstRequests
      equal [ 42 ] result.secondRequests

  for_
    [ { name: "readable names", context: ContextType modelName, role: role "Person", property: property "Dossier$External$ParticipantName" }
    , { name: "stable IDs", context: ContextType (modelName <> "$context-id"), role: role "role-id", property: property "property-id" }
    ]
    \input -> test ("An unreachable property reports its filler restriction with readable type names from " <> input.name) $
      withCompiledModel "Attendees >> binding" \dfr _ -> do
        let
          readableAdt = UET (RoleInContext { context: ContextType modelName, role: role "Person" })
          readableProp = ENP (property "Dossier$External$ParticipantName")
          invalidQuery = SQD
            (RDOM (UET (RoleInContext { context: input.context, role: input.role })))
            (PropertyGetter (ENP input.property))
            (VDOM PString Nothing)
            True
            False
        Tuple result _ <- runP do
          void $ storeDomeinFileInCache (ModelUri modelName) (toStableDomeinFile $ DomeinFile dfr)
          Context context <- getContext (ContextType modelName)
          EnumeratedRole person <- getEnumeratedRole (role "Person")
          EnumeratedProperty participantName <- getEnumeratedProperty (property "Dossier$External$ParticipantName")
          cacheInDomeinFile input.context (Context context { id = input.context })
          cacheInDomeinFile input.role (EnumeratedRole person { id = input.role, context = input.context })
          cacheInDomeinFile input.property (EnumeratedProperty participantName { id = input.property })
          runPhaseTwo' (invert invalidQuery)
        case result of
          Left errors -> equal
            [ Custom $ "expandPropertyQuery: no filler restriction found for ADT " <> show readableAdt <> " and property " <> show readableProp <> "." ]
            errors
          Right _ -> assert "Invalid property access must not be silently skipped" false

withInversions :: QueryFunctionDescription -> (Array AnchoredQueryWithAKink -> Test) -> Test
withInversions query assertions = do
  Tuple result _ <- runP $ runPhaseTwo' do
    anchored <- invertWithAnchors query
    plain <- invert query
    pure $ Tuple anchored plain
  case result of
    Left err -> assert (show err) false
    Right (Tuple anchored plain) -> do
      equal plain (map _.query anchored)
      assertions anchored

anchorMarkers :: AnchoredQueryWithAKink -> Array PropertyAnchor
anchorMarkers = map _.propertyAnchor <<< _.steps

withCompiledQueries :: String -> (StoredQueries -> Test) -> Test
withCompiledQueries selection assertions = withCompiledModel selection \_ queries -> assertions queries

withCompiledModel :: String -> (DomeinFileRecord Readable -> StoredQueries -> Test) -> Test
withCompiledModel selection assertions = do
  parsed <- runIndentParser (model selection) ARC.domain
  case parsed of
    Left err -> assert (show err) false
    Right root -> do
      Tuple result state <- runPhaseTwo' (traverseDomain root)
      case result of
        Left err -> assert (show err) false
        Right (DomeinFile dfr) -> do
          compiled <- runP $ phaseThree dfr state.postponedStateQualifiedParts state.screens
          case compiled of
            Left err -> assert (show err) false
            Right (Tuple dfr' queries) -> assertions dfr' queries

model :: String -> String
model selection = joinWith "\n"
  [ "domain " <> modelName
  , "  use this for " <> modelName
  , "  user Person"
  , "    indexed this:CurrentPerson"
  , "    property Active (Boolean)"
  , "  case Meeting"
  , "    user Viewer"
  , "      perspective on OwnAttendees"
  , "        props (ParticipantName) verbs (Consult)"
  , "    context Attendees (relational) filledBy Dossier"
  , "    thing OwnAttendees = " <> selection
  , "  case Dossier"
  , "    external"
  , "      property ParticipantName (String)"
  , "      property Eligible (Boolean)"
  , "    user Ambassador filledBy Person"
  , "      property Approved (Boolean)"
  ]

modelName :: String
modelName = "model://example.org#PropertyAnchors"

role :: String -> EnumeratedRoleType
role name = EnumeratedRoleType (modelName <> "$" <> name)

property :: String -> EnumeratedPropertyType
property name = EnumeratedPropertyType (modelName <> "$" <> name)

hasPropertyKey :: EnumeratedPropertyType -> StoredQueries -> Boolean
hasPropertyKey expected = any (matchesPropertyKey expected)

matchesPropertyKey :: forall r. EnumeratedPropertyType -> { queryType :: String, keys :: Array String | r } -> Boolean
matchesPropertyKey expected { queryType, keys } =
  any
    ( \key -> case deserializeInvertedQueryKey queryType key of
        Just (RTPropertyKey { property: actual }) -> actual == expected
        _ -> false
    )
    keys

evaluatePropertyQueries :: EnumeratedPropertyType -> StoredQueries -> MonadPerspectives (Array String)
evaluatePropertyQueries expected queries = nub <<< concat <$> for (filter (matchesPropertyKey expected) queries)
  \{ query: InvertedQuery { description: ZQ backward _ } } -> case backward of
    Nothing -> throwError $ error "An indexed property query must have a backwards path."
    Just query -> do
      compiled <- compileFunction query
      unwrap dossierId ##= compiled

meetingId :: ContextInstance
meetingId = ContextInstance "property-anchor-meeting"

attendeeId :: RoleInstance
attendeeId = RoleInstance "property-anchor-attendee"

dossierId :: RoleInstance
dossierId = RoleInstance "property-anchor-dossier-external"

hasBindingKey :: EnumeratedRoleType -> StoredQueries -> Boolean
hasBindingKey expected = any \{ queryType, keys } ->
  any
    ( \key -> case deserializeInvertedQueryKey queryType key of
        Just (RTFilledKey { filledRole_destination }) -> filledRole_destination == expected
        Just (RTFillerKey { filledRole_origin }) -> filledRole_origin == expected
        _ -> false
    )
    keys

hasContextKey :: EnumeratedRoleType -> StoredQueries -> Boolean
hasContextKey expected = any \{ queryType, keys } ->
  any
    ( \key -> case deserializeInvertedQueryKey queryType key of
        Just (RTContextKey { role_origin }) -> role_origin == expected
        _ -> false
    )
    keys

itemDomain :: Domain
itemDomain = RDOM (UET (RoleInContext { context: ContextType modelName, role: role "Items" }))

source :: QueryFunctionDescription
source = SQD (CDOM (UET (ContextType modelName))) (RolGetter (ENR (role "Items"))) itemDomain False False

filterStep :: QueryFunctionDescription
filterStep = UQD itemDomain FilterF
  ( UQD itemDomain (UnaryCombinator ExistsF)
      (SQD itemDomain (FilledF (role "Items") (ContextType modelName)) itemDomain False False)
      (VDOM PBool Nothing)
      True
      True
  )
  itemDomain
  False
  False
