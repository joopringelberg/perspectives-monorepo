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
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-- | Regression tests for model translation (Perspectives.ModelTranslation).
-- |
-- | The pipeline under test:
-- |
-- |   DomeinFile --generateFirstTranslation--> ModelTranslation (qualified keys)
-- |     --writeTranslationYaml--> YAML (local names, nested blocks)
-- |     --parseTranslation_pass1--> ModelTranslation (local names)
-- |     --qualifyModelTranslation--> ModelTranslation (qualified keys)
-- |     --generateTranslationTable--> TranslationTable (flat: qualified name / original text -> translations)
-- |
-- | The Readable <-> Stable key conversion (Normalize.purs) needs a StableIdMapping and the
-- | repository; it is not covered here. In these tests readable and stable names coincide.
-- |
-- | See docsources/model-translation.md.

module Test.ModelTranslation where

import Prelude

import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Free (Free)
import Data.Array (concat, elem, length, null, sort)
import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Foreign.Object (empty, fromFoldable, keys, lookup, singleton, union, values)
import Data.MediaType (MediaType(..))
import Perspectives.DomeinCache (AttachmentFiles)
import Perspectives.Extern.Parsing (combineUploadAttachments)
import Perspectives.CoreTypes (TranslationTable(..), Translations(..))
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.ModelTranslation (augmentModelTranslation, generateFirstTranslation, generateTranslationTable, parseTranslation_pass1, qualifyModelTranslation, translateTypeString, translationOf, writeTranslationYaml)
import Perspectives.ModelTranslation.Representation (ActionsPerStateTranslation(..), ActionsTranslation(..), ContextTranslation(..), ContextsTranslation(..), MarkdownsTranslation(..), ModelTranslation(..), NotificationsTranslation(..), PropertiesTranslation(..), RoleTranslation(..), RolesTranslation(..), TitlesTranslation(..))
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo', toStableDomeinFile)
import Perspectives.PerspectivesState (setCurrentLanguage, setTranslationTable)
import Perspectives.SideCar.PhantomTypedNewtypes (Readable)
import Simple.JSON (readJSON, writeJSON)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

-------------------------------------------------------------------------------
---- HELPERS
-------------------------------------------------------------------------------
ns :: String
ns = "model://example.org#TestTranslation"

q :: String -> String
q local = ns <> "$" <> local

lookupTranslation :: String -> String -> TranslationTable -> Maybe String
lookupTranslation key lang (TranslationTable t) = lookup key t >>= \(Translations ts) -> lookup lang ts

hasKey :: String -> TranslationTable -> Boolean
hasKey key (TranslationTable t) = isJust (lookup key t)

tableKeys :: TranslationTable -> Array String
tableKeys (TranslationTable t) = sort (keys t)

-- | Parse a YAML translation file and turn it into a translation table, as the PDR does
-- | (minus the Readable -> Stable normalisation of keys).
yamlToTable :: String -> Aff TranslationTable
yamlToTable yaml = do
  parsed <- liftEffect $ parseTranslation_pass1 yaml
  case parsed of
    Left e -> throwError (error ("Could not parse YAML: " <> message e <> "\n" <> yaml))
    Right mt -> pure $ generateTranslationTable (qualifyModelTranslation mt)

-- | Write a ModelTranslation to YAML, parse it back and produce the translation table.
roundTrip :: ModelTranslation -> Aff TranslationTable
roundTrip = yamlToTable <<< writeTranslationYaml

emptyRoleTranslation :: RoleTranslation
emptyRoleTranslation = RoleTranslation
  { translations: Translations empty
  , properties: PropertiesTranslation empty
  , actions: ActionsPerStateTranslation empty
  , notifications: NotificationsTranslation empty
  , markdowns: MarkdownsTranslation empty
  , titles: TitlesTranslation empty
  }

actionNames :: RoleTranslation -> Array String
actionNames (RoleTranslation { actions: ActionsPerStateTranslation aps }) = concat (values aps <#> \(ActionsTranslation a) -> keys a)

-------------------------------------------------------------------------------
---- FIXTURES
-------------------------------------------------------------------------------

-- | A hand-written translation file that exercises every block of the YAML format.
-- | Names are local; the block structure qualifies them.
structuralYaml :: String
structuralYaml =
  """model://example.org#TestTranslation:
  version: "1.0"
  contexts:
    Outer:
      translations:
        nl: Buiten
        en: Outer
      indexedName:
        name: model://example.org#TestTranslation$MyOuter
        translations:
          nl: Mijn buiten
      external:
        properties:
          Name:
            translations:
              nl: Naam
      notifications:
        0:
          translations:
            orig: Outer filled
            nl: Buiten gevuld
      users:
        Manager:
          translations:
            nl: Beheerder
          properties:
            Nickname:
              translations:
                nl: Bijnaam
          actions:
            model://example.org#TestTranslation$Outer$Manager:
              MakeThing:
                translations:
                  nl: Maak ding
          titles:
            0:
              translations:
                orig: Outer screen
                nl: Buitenscherm
      things:
        Things:
          translations:
            nl: Dingen
      contextroles:
        Inners:
          translations:
            nl: Binnens
      contexts:
        Inner:
          translations:
            nl: Binnen
          things:
            Item:
              translations:
                nl: Ding
              properties:
                Label:
                  translations:
                    nl: Etiket
  roles:
    Admin:
      translations:
        nl: Hoofdbeheerder
"""

-- | Texts that are not safe as plain (unquoted) YAML scalars.
trickyTexts :: Array String
trickyTexts =
  [ "Note: a colon followed by a space"
  , "Leaving # now"
  , "{Name} has joined"
  , "[bracketed] text"
  , "Say \"hi\" and 'bye'"
  , "Path C:\\temp\\new"
  , "true"
  , "123"
  , "- dash start"
  , "* starred"
  , "Ends with colon:"
  , "Unicode: café ✓"
  , "`backtick` start"
  , "a: b # c"
  ]

trickyMarkdowns :: Array String
trickyMarkdowns =
  [ "### Heading\nA \"quoted\" word: and a \\* escaped star\n\nLast line"
  , "List\n  * indented item\n    * deeper item\nEnd"
  , "Ends with a newline\n"
  , "Backslash sequences C:\\temp\\new and \\n literally"
  ]

trickyModel :: Boolean -> ModelTranslation
trickyModel withTranslations = ModelTranslation
  { namespace: ns
  , version: "1.0"
  , contexts: Nothing
  , roles: Just $ RolesTranslation $ singleton (q "Admin") $ RoleTranslation
      { translations: tr [ Tuple "nl" "Beheerder: hoofd", Tuple "en" "# not a comment" ]
      , properties: PropertiesTranslation $ singleton (q "Admin$Nickname") (tr [ Tuple "nl" "Bijnaam: kort" ])
      , actions: ActionsPerStateTranslation empty
      , notifications: NotificationsTranslation $ fromFoldable (trickyTexts <#> \t -> Tuple t (tr [ Tuple "nl" ("NL " <> t) ]))
      , markdowns: MarkdownsTranslation $ fromFoldable (trickyMarkdowns <#> \t -> Tuple t (tr [ Tuple "nl" t ]))
      , titles: TitlesTranslation $ fromFoldable (trickyTexts <#> \t -> Tuple ("Title " <> t) (tr [ Tuple "nl" ("Titel " <> t) ]))
      }
  }
  where
  tr :: Array (Tuple String String) -> Translations
  tr pairs = if withTranslations then Translations (fromFoldable pairs) else Translations empty

-- | A small model that exercises nested contexts, actions from several perspectives in one state,
-- | notifications on entry and exit, markdown and titled screen elements.
testModelSource :: String
testModelSource =
  """domain model://example.org#TestTranslation
  user Admin
  thing ProjectExternal
  case Outer
    state Filled = exists Things
      on entry
        notify Manager
          "Note: the outer context is filled"
      on exit
        notify Manager
          "Leaving # now"
    external
      property Name (String)
    thing Things (relational)
      property Label (String)
    context Inners (relational) filledBy Inner
    user Manager
      property Nickname (String)
      perspective on Things
        only (Create)
        props (Label) verbs (Consult)
        action MakeThing
          create role Things
      perspective on External
        props (Name) verbs (Consult, SetPropertyValue)
        action ClearName
          Name = "cleared"
      action ContextLevel
        create role Things
      screen "Outer screen"
        tab "First tab"
          row
            markdown <### Heading
                      A "quoted" word: and more.
                      >
          row
            table "Things table" Things
          row
            form "External form" External
    case Inner
      thing Item
        property Label (String)
"""

compileModel :: String -> Aff (DomeinFileRecord Readable)
compileModel source = do
  parsed <- runIndentParser source ARC.domain
  case parsed of
    Left e -> throwError (error ("Parse error: " <> show e))
    Right root -> do
      Tuple result state <- runPhaseTwo' (traverseDomain root)
      case result of
        Left e -> throwError (error ("Phase two error: " <> show e))
        Right (DomeinFile df) -> do
          compiled <- runP $ phaseThree df state.postponedStateQualifiedParts state.screens
          case compiled of
            Left e -> throwError (error ("Phase three error: " <> show e))
            Right (Tuple corrected _) -> pure corrected

firstTranslationOfTestModel :: Aff ModelTranslation
firstTranslationOfTestModel = do
  dfr <- compileModel testModelSource
  pure $ generateFirstTranslation (toStableDomeinFile (DomeinFile dfr { _id = "example_org-TestTranslation@1.0.json" }))

-------------------------------------------------------------------------------
---- SUITE
-------------------------------------------------------------------------------
main :: Effect Unit
main = runTest theSuite

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.ModelTranslation" do

  suite "YAML block structure qualifies local names" do
    test "Every block of the YAML file yields the expected qualified key" do
      table <- yamlToTable structuralYaml
      let
        expected =
          [ Tuple (q "Outer") "Buiten"
          , Tuple (q "MyOuter") "Mijn buiten"
          , Tuple (q "Outer$External$Name") "Naam"
          , Tuple "Outer filled" "Buiten gevuld"
          , Tuple (q "Outer$Manager") "Beheerder"
          , Tuple (q "Outer$Manager$Nickname") "Bijnaam"
          , Tuple (q "Outer$Manager$MakeThing") "Maak ding"
          , Tuple "Outer screen" "Buitenscherm"
          , Tuple (q "Outer$Things") "Dingen"
          , Tuple (q "Outer$Inners") "Binnens"
          , Tuple (q "Outer$Inner") "Binnen"
          , Tuple (q "Outer$Inner$Item") "Ding"
          , Tuple (q "Outer$Inner$Item$Label") "Etiket"
          , Tuple (q "Admin") "Hoofdbeheerder"
          ]
      for_ expected \(Tuple key nl) ->
        equal (Just nl) (lookupTranslation key "nl" table)
      assert "The external role of a context has its own key" (hasKey (q "Outer$External") table)
      equal (Just "Outer filled") (lookupTranslation "Outer filled" "orig" table)

    test "A translation table survives JSON serialisation" do
      table <- yamlToTable structuralYaml
      case readJSON (writeJSON table) of
        Left e -> assert ("Could not read back the table: " <> show e) false
        Right (table' :: TranslationTable) -> equal (tableKeys table) (tableKeys table')

  suite "Generated YAML is lossless" do
    test "Notifications, titles and translations with YAML-significant characters survive a round trip" do
      table <- roundTrip (trickyModel true)
      equal (Just "Beheerder: hoofd") (lookupTranslation (q "Admin") "nl" table)
      equal (Just "# not a comment") (lookupTranslation (q "Admin") "en" table)
      equal (Just "Bijnaam: kort") (lookupTranslation (q "Admin$Nickname") "nl" table)
      for_ trickyTexts \t -> do
        equal (Just t) (lookupTranslation t "orig" table)
        equal (Just ("NL " <> t)) (lookupTranslation t "nl" table)
        equal (Just ("Title " <> t)) (lookupTranslation ("Title " <> t) "orig" table)
        equal (Just ("Titel " <> t)) (lookupTranslation ("Title " <> t) "nl" table)

    test "Markdown texts (and their translations) survive a round trip unchanged" do
      table <- roundTrip (trickyModel true)
      for_ trickyMarkdowns \t -> do
        equal (Just t) (lookupTranslation t "orig" table)
        equal (Just t) (lookupTranslation t "nl" table)

    test "Unfilled languages are written as MISSING placeholders" do
      let yaml = writeTranslationYaml (trickyModel false)
      table <- yamlToTable yaml
      equal (Just "MISSING nl") (lookupTranslation (q "Admin") "nl" table)
      equal (Just "MISSING en") (lookupTranslation (q "Admin") "en" table)

    test "Augmenting a fresh translation with an existing table preserves the modeller's translations" do
      table <- roundTrip (trickyModel true)
      table' <- roundTrip (augmentModelTranslation table (trickyModel false))
      for_ (tableKeys table) \key ->
        equal (lookupTranslation key "nl" table) (lookupTranslation key "nl" table')

  suite "Parsing hand-edited YAML is robust" do
    test "Non-string scalars written by a modeller become strings" do
      table <- yamlToTable
        """model://example.org#TestTranslation:
  version: "1.0"
  roles:
    Admin:
      translations:
        nl: 2024
        en: true
"""
      equal (Just "2024") (lookupTranslation (q "Admin") "nl" table)
      equal (Just "true") (lookupTranslation (q "Admin") "en" table)
      case readJSON (writeJSON table) of
        Left e -> assert ("Could not read back the table: " <> show e) false
        Right (_ :: TranslationTable) -> pure unit

    test "A notification without its original text is skipped instead of crashing" do
      r <- try $ yamlToTable
        """model://example.org#TestTranslation:
  version: "1.0"
  roles:
    Admin:
      notifications:
        0:
          translations:
            nl: Zonder origineel
        1:
          translations:
            orig: With original
            nl: Met origineel
"""
      case r of
        Left e -> assert ("Parsing crashed: " <> message e) false
        Right table -> equal (Just "Met origineel") (lookupTranslation "With original" "nl" table)

    test "An empty or non-mapping file is reported as an error, not a crash" do
      for_ [ "", "just a scalar", "- a\n- list" ] \yaml -> do
        r <- try $ liftEffect $ parseTranslation_pass1 yaml
        case r of
          Left e -> assert ("Parsing crashed on " <> show yaml <> ": " <> message e) false
          Right parsed -> assert ("Expected a parse error for " <> show yaml) (isLeft parsed)

  suite "Actions per state" do
    test "Appending merges the actions of a shared state" do
      let
        a = ActionsPerStateTranslation (singleton "S" (ActionsTranslation (singleton "R$A" (Translations empty))))
        b = ActionsPerStateTranslation (singleton "S" (ActionsTranslation (singleton "R$B" (Translations empty))))
        RoleTranslation r = emptyRoleTranslation
      equal [ "R$A", "R$B" ] (sort $ actionNames (RoleTranslation r { actions = a <> b }))

  suite "Translation generated from a compiled model" do
    test "Only top-level contexts are listed at the top level; nested contexts are nested" do
      ModelTranslation { contexts } <- firstTranslationOfTestModel
      case contexts of
        Nothing -> assert "Expected contexts" false
        Just (ContextsTranslation cs) -> do
          equal [ q "Outer" ] (keys cs)
          case lookup (q "Outer") cs of
            Nothing -> assert "Expected Outer" false
            Just (ContextTranslation { contexts: ContextsTranslation nested }) -> equal [ q "Outer$Inner" ] (keys nested)

    test "Top-level roles include roles whose name merely ends in 'External'" do
      ModelTranslation { roles } <- firstTranslationOfTestModel
      case roles of
        Nothing -> assert "Expected roles" false
        Just (RolesTranslation rs) -> equal [ q "Admin", q "ProjectExternal" ] (sort $ keys rs)

    test "A user role lists the actions of all its perspectives and its context actions" do
      mt <- firstTranslationOfTestModel
      case managerOf mt of
        Nothing -> assert "Expected the Manager role" false
        Just manager -> equal
          [ q "Outer$Manager$ClearName", q "Outer$Manager$ContextLevel", q "Outer$Manager$MakeThing" ]
          (sort $ actionNames manager)

    test "Context notifications on entry and on exit are both listed" do
      mt <- firstTranslationOfTestModel
      case outerOf mt of
        Nothing -> assert "Expected Outer" false
        Just (ContextTranslation { notifications: NotificationsTranslation n }) ->
          equal [ "Leaving # now", "Note: the outer context is filled" ] (sort $ keys n)

    test "Screen markdown and titles are listed for the user role" do
      mt <- firstTranslationOfTestModel
      case managerOf mt of
        Nothing -> assert "Expected the Manager role" false
        Just (RoleTranslation { markdowns: MarkdownsTranslation m, titles: TitlesTranslation t }) -> do
          assert "Expected one markdown text" (length (keys m) == 1)
          for_ [ "Outer screen", "First tab", "Things table", "External form" ] \title ->
            assert ("Expected title " <> title) (elem title (keys t))

    test "The YAML round trip of a compiled model reproduces the same translation table keys" do
      mt <- firstTranslationOfTestModel
      table <- roundTrip mt
      let direct = generateTranslationTable mt
      equal (tableKeys direct) (tableKeys table)
      assert "The table is not empty" (not $ null $ tableKeys table)

  suite "Attachments uploaded with a compiled model" do
    let
      att :: String -> Tuple MediaType (Maybe Foreign)
      att v = Tuple (MediaType "application/json") (Just $ unsafeToForeign v)

      valueOf :: String -> AttachmentFiles -> Maybe String
      valueOf name files = lookup name files >>= \(Tuple _ mf) -> unsafeFromForeign <$> mf

      regenerated = fromFoldable
        [ Tuple "storedQueries.json" (att "new queries")
        , Tuple "stableIdMapping.json" (att "new mapping")
        , Tuple "modelDependencies.json" (att "new dependencies")
        ]
      defaults = singleton "translationtable.json" (att "empty table")

    test "A re-upload keeps the translation table and other attachments, but not stale compilation results" do
      let
        retained = fromFoldable
          [ Tuple "translationtable.json" (att "existing table")
          , Tuple "conversations.json" (att "existing conversations")
          , Tuple "storedQueries.json" (att "stale queries")
          , Tuple "stableIdMapping.json" (att "stale mapping")
          ]
        result = combineUploadAttachments { regenerated, retained, defaults }
      equal (Just "existing table") (valueOf "translationtable.json" result)
      equal (Just "existing conversations") (valueOf "conversations.json" result)
      equal (Just "new queries") (valueOf "storedQueries.json" result)
      equal (Just "new mapping") (valueOf "stableIdMapping.json" result)
      equal (Just "new dependencies") (valueOf "modelDependencies.json" result)

    test "A first upload without a table to keep or copy gets an empty translation table" do
      let result = combineUploadAttachments { regenerated, retained: empty, defaults }
      equal (Just "empty table") (valueOf "translationtable.json" result)
      equal [ "modelDependencies.json", "stableIdMapping.json", "storedQueries.json", "translationtable.json" ] (sort $ keys result)

    test "A table copied from the version this version is based on replaces an empty table" do
      let
        seeded = singleton "translationtable.json" (att "table of previous version")
        retained = singleton "translationtable.json" (att "empty table")
        result = combineUploadAttachments { regenerated, retained: seeded `union` retained, defaults }
      equal (Just "table of previous version") (valueOf "translationtable.json" result)

  suite "Runtime lookup" do
    test "translationOf and translateTypeString" do
      let
        table = TranslationTable $ fromFoldable
          [ Tuple (q "Outer") (Translations $ fromFoldable [ Tuple "nl" "Buiten", Tuple "en" "Outer" ])
          , Tuple (q "Outer$Things") (Translations $ fromFoldable [ Tuple "nl" "MISSING nl" ])
          , Tuple (q "Outer$Report") (Translations $ fromFoldable [ Tuple "en" "Report MISSING items" ])
          ]
      results <- runP do
        setTranslationTable ns table
        setCurrentLanguage "nl"
        a <- translationOf ns (q "Outer")
        b <- translationOf ns (q "Outer$Things")
        c <- translationOf ns (q "Outer$Unknown")
        d <- translateTypeString (q "Outer$External")
        e <- translateTypeString "model://other.org#Other$Thing"
        setCurrentLanguage "en"
        f <- translationOf ns (q "Outer$Report")
        g <- translationOf ns (q "Outer$Inner")
        pure [ a, b, c, d, e, f, g ]
      equal
        [ "Buiten"
        , q "Outer$Things"
        , q "Outer$Unknown"
        , "Buiten"
        , "model://other.org#Other$Thing"
        , "Report MISSING items"
        , q "Outer$Inner"
        ]
        results

  where
  outerOf :: ModelTranslation -> Maybe ContextTranslation
  outerOf (ModelTranslation { contexts }) = contexts >>= \(ContextsTranslation cs) -> lookup (q "Outer") cs

  managerOf :: ModelTranslation -> Maybe RoleTranslation
  managerOf mt = outerOf mt >>= \(ContextTranslation { users: RolesTranslation us }) -> lookup (q "Outer$Manager") us
