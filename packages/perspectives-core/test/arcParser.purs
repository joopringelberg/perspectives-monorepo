module Test.Parsing.Arc where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (Either(..))
import Data.List (List(..), filter, head, length, (:))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Parsing (ParseError(..), Position(..))
import Perspectives.Parsing.Arc (contextRoleE, domain, thingRoleE, userRoleE)
import Perspectives.Parsing.Arc.AST (ContextE, ContextPart(..), FormE(..), FreeFormScreenE(..), RoleE(..), RolePart(..), RowE(..), ScreenE(..), ScreenElement(..), TabE(..), TableE(..), TableFormE(..), TableFormOrWhenE(..), TableFormSectionE(..), WhoWhatWhereScreenE(..), PropsOrView(..))
import Perspectives.Parsing.Arc.Expression.AST (BinaryStep(..), FilledByAttribute(..), Operator(..), SimpleStep(..), Step(..), TypeCombination(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Test.Parsing.ArcAstSelectors (ensureOnEntry, ensurePerspectiveOn, ensurePropertyVerbSetForPropsOrView, ensurePropertyVerbsForPropsOrView, ensureStateInRole, ensureUserRole, failure, isNotified, isStateWithContext, isStateWithExplicitRole_)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

-- | Supplementary phase-one regressions not covered by Arc.Syntax.
theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc regressions" do
  test "An empty role declaration fails" do
    result <- runIndentParser "" thingRoleE
    case result of
      Left _ -> pure unit
      Right _ -> failure "An empty input must not parse as a role"

  test "A context role retains its filler specification" do
    result <- runIndentParser "context Rooms (relational) filledBy Room\n" contextRoleE
    case result of
      Right (RE (RoleE { roleParts })) ->
        assert "Expected one filler specification" $
          length
            ( filter
                ( case _ of
                    FilledBySpecifications (Alternatives fillers) ->
                      map (\(FilledByAttribute identifier _) -> identifier) fillers == pure "Room"
                    _ -> false
                )
                roleParts
            ) == 1
      Left e -> failure (show e)
      _ -> failure "Expected a context role"

  test "A role retains both nested properties" do
    result <- runIndentParser "thing Item\n  property Name (String)\n  property Price (Number)\n" thingRoleE
    case result of
      Right (RE (RoleE { roleParts })) ->
        assert "Expected two property declarations" $
          length
            ( filter
                ( case _ of
                    PE _ -> true
                    _ -> false
                )
                roleParts
            ) == 2
      Left e -> failure (show e)
      _ -> failure "Expected a thing role"

  test "A calculated role retains its nested property" do
    result <- runIndentParser "thing Item = context >> Items\n  property Name (String)\n" thingRoleE
    case result of
      Right (RE (RoleE { roleParts })) ->
        assert "Expected a property on the calculated role" $
          length
            ( filter
                ( case _ of
                    PE _ -> true
                    _ -> false
                )
                roleParts
            ) == 1
      Left e -> failure (show e)
      _ -> failure "Expected a calculated role"

  test "A view without explicit verbs grants universal property verbs" do
    role <- parseUser "user Viewer\n  perspective on Item\n    view Details\n"
    ensureStateInRole (isStateWithContext "") role
      >>= ensurePerspectiveOn "Item"
      >>= ensurePropertyVerbSetForPropsOrView Universal (View "Details")

  test "Separate property clauses retain their own verbs" do
    role <- parseUser "user Editor\n  perspective on Item\n    props (Name) verbs (AddPropertyValue)\n    props (Price) verbs (RemovePropertyValue)\n"
    parts <- ensureStateInRole (isStateWithContext "") role >>= ensurePerspectiveOn "Item"
    ensurePropertyVerbsForPropsOrView [ AddPropertyValue ] (Properties ("Name" : Nil)) parts
    ensurePropertyVerbsForPropsOrView [ RemovePropertyValue ] (Properties ("Price" : Nil)) parts

  test "A perspective without a subject fails" do
    result <- runIndentParser "domain model://perspectives.domains#Test\n  perspective on Item\n    all roleverbs\n" domain
    case result of
      Left (ParseError message _) -> assert message (message == "No subject is in scope. ")
      Right _ -> failure "A perspective requires a subject"

  test "A malformed use clause reports its source position" do
    result <- runIndentParser "domain MyTestDomain\n  use sys for System$System" domain
    case result of
      Left (ParseError _ (Position position)) ->
        assert ("Expected the invalid use clause to fail at line 2, column 28; got " <> show position)
          (position.line == 2 && position.column == 28)
      Right _ -> failure "The use clause requires a model URI"

  test "A role notification accepts an interpolated sentence" do
    ctxt <- parseDomain "domain model://perspectives.domains#Test\n  user Self\n    on entry\n      notify Other \"Hello {Other >> FirstName}!\"\n"
    ensureUserRole "Self" ctxt
      >>= ensureStateInRole (isStateWithExplicitRole_ "model://perspectives.domains#Test$Self" Nothing)
      >>= ensureOnEntry
      >>= isNotified "Other"

  test "Who-what-where master widgets retain fillproperty queries" do
    screen <- parseScreen
      ( "domain model://perspectives.domains#Test\n"
          <> "  user Editor\n"
          <> "    screen\n"
          <> "      who\n"
          <> "        Editor\n"
          <> "          master\n"
          <> "            with props (Value)\n"
          <> "            fillproperty Value from ReferenceValues >> Value\n"
          <> "          detail\n"
          <> "            with props (Value)\n"
          <> "      what\n"
          <> "        Item master\n"
          <> "          detail\n"
          <> "      where\n"
          <> "        Destination master\n"
          <> "          detail\n"
      )
    case screen of
      WWW (WhoWhatWhereScreenE { who: TableFormSectionE _ (PlainTableFormE (TableFormE _ (TableE _ fields) _) : Nil) }) ->
        assert "Expected Value's fillproperty query on the master widget"
          ( map _.propertyName fields.fillPropertyValues == ("Value" : Nil) &&
              map (isReferenceValuesQuery <<< _.valuesQuery) fields.fillPropertyValues == (true : Nil)
          )
      _ -> failure "Expected one who master/detail widget"

  test "Classic forms and tables retain separate fillproperty clauses" do
    screen <- parseScreen
      ( "domain model://perspectives.domains#Test\n"
          <> "  user Editor\n"
          <> "    screen \"Project\"\n"
          <> "      tab \"Overview\" default\n"
          <> "        row\n"
          <> "          form External\n"
          <> "            fillproperty Name from ReferenceValues >> Value\n"
          <> "      tab \"Tasks\"\n"
          <> "        row\n"
          <> "          table Task\n"
          <> "            fillproperty Status from ReferenceValues >> Value\n"
      )
    case screen of
      ClassicScreen
        ( FreeFormScreenE
            { tabs: Just
                ( TabE "Overview" true (RowElement (RowE (FormElement (FormE _ form) : Nil)) : Nil)
                    : TabE "Tasks" false (RowElement (RowE (TableElement (TableE _ table) : Nil)) : Nil)
                    : Nil
                )
            }
        ) -> do
        assert "Expected Name's fillproperty query on the form"
          ( map _.propertyName form.fillPropertyValues == ("Name" : Nil) &&
              map (isReferenceValuesQuery <<< _.valuesQuery) form.fillPropertyValues == (true : Nil)
          )
        assert "Expected Status's fillproperty query on the table"
          ( map _.propertyName table.fillPropertyValues == ("Status" : Nil) &&
              map (isReferenceValuesQuery <<< _.valuesQuery) table.fillPropertyValues == (true : Nil)
          )
      _ -> failure "Expected two tabs containing a form and a table"

parseDomain :: String -> Aff ContextE
parseDomain source = runIndentParser source domain >>= case _ of
  Left e -> failure (show e)
  Right ctxt -> pure ctxt

isReferenceValuesQuery :: Step -> Boolean
isReferenceValuesQuery
  ( Binary
      ( BinaryStep
          { operator: Compose _
          , left: Simple (ArcIdentifier _ "ReferenceValues")
          , right: Simple (ArcIdentifier _ "Value")
          }
      )
  ) = true
isReferenceValuesQuery _ = false

parseUser :: String -> Aff RoleE
parseUser source = runIndentParser source userRoleE >>= case _ of
  Left e -> failure (show e)
  Right (RE role) -> pure role
  _ -> failure "Expected a user role"

parseScreen :: String -> Aff ScreenE
parseScreen source = do
  RoleE { roleParts } <- parseDomain source >>= ensureUserRole "Editor"
  case
    head
      ( filter
          ( case _ of
              Screen _ -> true
              _ -> false
          )
          roleParts
      )
    of
    Just (Screen screen) -> pure screen
    _ -> failure "Expected a screen on Editor"
