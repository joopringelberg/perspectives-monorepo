-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.Parsing.Arc.ConditionalTabs where

import Prelude

import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)
import Data.Array (fromFoldable, uncons)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.List (mapMaybe) as LIST
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Parsing.String (eof)
import Perspectives.ContextAndRole (defaultContextRecord)
import Perspectives.Data.EncodableMap (values) as EM
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.InstanceRepresentation (PerspectContext(..))
import Perspectives.Parsing.Arc (domain, userRoleE)
import Perspectives.Parsing.Arc.AST (ContextPart(..), FreeFormScreenE(..), MarkDownE(..), RoleE(..), RolePart(..), RowE(..), ScreenE(..), ScreenElement(..), TabE(..), WhatE(..), WhenE(..), WhoWhatWhereScreenE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo')
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..))
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ScreenDefinition (MarkDownDef(..), ScreenDefinition(..), ScreenElementDef(..), TabDef(..), What(..), WhenDef(..)) as DEF
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..))
import Perspectives.TypePersistence.ContextSerialisation (addPerspectives)
import Perspectives.TypePersistence.ScreenContextualisation (contextualiseWhat)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Conditional screen tabs" do
  for_ [ false, true ] \www -> do
    let
      screen body =
        [ "user Tester", "  screen" ]
          <> (if www then [ "    who", "      Tester", "        master", "        detail", "    what" ] else [])
          <> map ((if www then "      " else "    ") <> _) body
          <> (if www then [ "    where" ] else [])
      parse body = parseScreen $ joinWith "\n" (screen body)

    test ((if www then "what" else "classic screen") <> " mixes ordinary and conditional tabs") do
      parsed <- parse
        [ "tab \"Manifest\" default"
        , "  row"
        , "    markdown <Manifest>"
        , "when not Published"
        , "  tab \"Compile\""
        , "    row"
        , "      markdown <Compile>"
        , "  tab \"Upload\" default"
        , "    row"
        , "      markdown <Upload>"
        , "tab \"History\""
        , "  row"
        , "    markdown <History>"
        ]
      case tabsOf parsed of
        [ TabE "Manifest" true _, TabE "Compile" false (Cons (WhenElement (WhenE condition context _)) Nil), TabE "Upload" true (Cons (WhenElement (WhenE otherCondition otherContext _)) Nil), TabE "History" false _ ] -> do
          equal condition otherCondition
          equal context otherContext
        _ -> assert ("Unexpected tabs: " <> show parsed) false

    test ((if www then "what" else "classic screen") <> " starts with nested conditional tabs") do
      parsed <- parse
        [ "when true"
        , "  when false"
        , "    tab \"Compile\" default"
        , "      row"
        , "        markdown <Compile>"
        ]
      case tabsOf parsed of
        [ TabE "Compile" true (Cons (WhenElement (WhenE _ _ (Cons (WhenElement _) Nil))) Nil) ] -> pure unit
        _ -> assert ("Expected both conditions to wrap the tab contents: " <> show parsed) false

    test ((if www then "what" else "classic screen") <> " rejects non-tabs inside a conditional tab block") do
      parsed <- runIndentParser
        (joinWith "\n" $ screen [ "when true", "  row", "    markdown <Not a tab>" ])
        (userRoleE <* eof)
      case parsed of
        Left _ -> pure unit
        Right _ -> assert "A conditional tab block must contain tabs" false

  test "what still accepts conditional master/detail items" do
    parsed <- parseScreen $ joinWith "\n"
      [ "user Tester"
      , "  screen"
      , "    who"
      , "      Tester"
      , "        master"
      , "        detail"
      , "    what"
      , "      when true"
      , "        Items"
      , "          master"
      , "          detail"
      , "    where"
      ]
    case parsed of
      WWW (WhoWhatWhereScreenE { what: TableForms _ }) -> pure unit
      _ -> assert "Expected conditional master/detail items, not tabs" false

  for_ [ "        markdown <Compile> when true", "        markdown <Compile>\n          when true" ] \markdown ->
    test ("markdown retains its own condition: " <> markdown) do
      parsed <- parseScreen $ joinWith "\n"
        [ "user Tester"
        , "  screen"
        , "    tab \"Compile\""
        , "      row"
        , markdown
        ]
      case tabsOf parsed of
        [ TabE _ _ (Cons (RowElement (RowE (Cons (MarkDownElement (MarkDownConstant { condition: Just _ })) Nil))) Nil) ] -> pure unit
        _ -> assert "Expected the condition to remain attached to the markdown" false

  test "conditional tab queries compile through phases two and three" do
    parsed <- runIndentParser
      ( joinWith "\n"
          [ "domain model://example.org#ConditionalTabs"
          , "  external"
          , "    property Published (Boolean)"
          , "  user Tester"
          , "    screen"
          , "      when not (extern >> Published)"
          , "        tab \"Compile\""
          , "          row"
          , "            markdown <Compile>"
          ]
      )
      (domain <* eof)
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
              Right (Tuple compiledDfr _) -> case fromFoldable $ EM.values compiledDfr.screens of
                [ DEF.ScreenDefinition { tabs: Just [ DEF.TabDef { elements: [ DEF.WhenElementD _ ] } ] } ] -> pure unit
                _ -> assert "Expected the compiled tab to retain its condition" false

  for_
    [ { name: "true condition", conditions: [ true ], visible: true }
    , { name: "false condition", conditions: [ false ], visible: false }
    , { name: "nested true conditions", conditions: [ true, true ], visible: true }
    , { name: "false inner condition", conditions: [ true, false ], visible: false }
    , { name: "false outer condition", conditions: [ false, true ], visible: false }
    ]
    \{ name, conditions, visible } -> test ("contextualisation: " <> name) do
      let
        tab = DEF.TabDef { title: "Compile", isDefault: true, elements: [ conditionalElements conditions ] }
      results <- runP $ fst <$> runWriterT
        ( runArrayT $ runReaderT
            (contextualiseWhat $ DEF.FreeFormScreen { tabs: Just [ tab ], rows: Nothing, columns: Nothing })
            { contextType, contextInstance, userRoleInstance: RoleInstance "tester" }
        )
      case results of
        [ DEF.FreeFormScreen { tabs } ] ->
          if visible then case tabs of
            Just [ DEF.TabDef { title: "Compile", isDefault: true } ] -> pure unit
            _ -> assert "Expected a visible tab with its title and default flag" false
          else equal Nothing tabs
        _ -> assert "Expected one contextualised screen" false

      classicResult <- runP do
        void $ cacheEntity contextInstance $ PerspectContext
          (defaultContextRecord { id = contextInstance, pspType = contextType })
        fst <$> runWriterT
          ( addPerspectives
              (DEF.FreeFormScreen { tabs: Just [ tab ], rows: Nothing, columns: Nothing })
              (RoleInstance "tester")
              contextInstance
          )
      case classicResult of
        DEF.FreeFormScreen { tabs } ->
          if visible then case tabs of
            Just [ DEF.TabDef { title: "Compile", isDefault: true } ] -> pure unit
            _ -> assert "Expected the classic screen to keep the visible tab" false
          else equal Nothing tabs
        _ -> assert "Expected a classic screen" false

parseScreen :: String -> Aff ScreenE
parseScreen source = do
  parsed <- runIndentParser source (userRoleE <* eof)
  case parsed of
    Right (RE (RoleE { roleParts })) ->
      case
        fromFoldable $ LIST.mapMaybe
          ( case _ of
              Screen screen -> Just screen
              _ -> Nothing
          )
          roleParts
        of
        [ screen ] -> pure screen
        _ -> throwError $ error $ "Expected one screen: " <> show roleParts
    Left err -> throwError $ error $ show err
    Right other -> throwError $ error $ "Expected one screen: " <> show other

tabsOf :: ScreenE -> Array TabE
tabsOf (ClassicScreen (FreeFormScreenE { tabs: Just tabs })) = fromFoldable tabs
tabsOf (WWW (WhoWhatWhereScreenE { what: FreeFormScreen (FreeFormScreenE { tabs: Just tabs }) })) = fromFoldable tabs
tabsOf _ = []

contextType :: ContextType
contextType = ContextType "model://example.org#ConditionalTabs"

contextInstance :: ContextInstance
contextInstance = ContextInstance "conditional-tabs"

conditionalElements :: Array Boolean -> DEF.ScreenElementDef
conditionalElements conditions = case uncons conditions of
  Nothing -> DEF.MarkDownElementD $ DEF.MarkDownExpressionDef
    { textQuery: booleanQuery true, condition: Nothing, text: Just "Compile" }
  Just { head, tail } -> DEF.WhenElementD $ DEF.WhenDef
    { condition: booleanQuery head, elements: [ conditionalElements tail ] }

booleanQuery :: Boolean -> QueryFunctionDescription
booleanQuery value = SQD (CDOM $ ST contextType) (Constant PBool $ if value then "true" else "false") (VDOM PBool Nothing) True True
