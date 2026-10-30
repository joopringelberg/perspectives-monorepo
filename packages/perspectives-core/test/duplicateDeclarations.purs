module Test.Parsing.Arc.DuplicateDeclarations where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Array (head)
import Effect (Effect)
import Effect.Aff (Aff)
import Parsing (ParseError)
import Partial.Unsafe (unsafePartial)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE)
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo, evalPhaseTwo')
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..), RoleType(..))
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

evalPhaseTwo :: forall a. PhaseTwo a -> Aff (Either PerspectivesError a)
evalPhaseTwo = evalPhaseTwo' >=> case _ of
  Left errs -> pure $ Left (unsafePartial fromJust $ head errs)
  Right r -> pure $ Right r

main :: Effect Unit
main = runTest do
  suite "Duplicate declaration checks" do
    test "Declaring the same context twice fails early" do
      (parsed :: Either ParseError ContextE) <- runIndentParser "domain MyTestDomain\n  case MyCase\n  case MyCase" ARC.domain
      case parsed of
        Left e -> assert (show e) false
        Right ctxt ->
          evalPhaseTwo (traverseDomain ctxt) >>=
            case _ of
              Left (DuplicateContextDeclaration _ (ContextType contextId)) ->
                assert "The duplicate context should be detected during phase two."
                  (contextId /= "")
              otherwise ->
                assert ("Expected DuplicateContextDeclaration, got: " <> show otherwise) false

    test "Declaring the same role twice fails early" do
      (parsed :: Either ParseError ContextE) <- runIndentParser "domain MyTestDomain\n  thing MyRole\n  thing MyRole" ARC.domain
      case parsed of
        Left e -> assert (show e) false
        Right ctxt ->
          evalPhaseTwo (traverseDomain ctxt) >>=
            case _ of
              Left (DuplicateRoleDeclaration _ (ENR (EnumeratedRoleType roleId))) ->
                assert "The duplicate role should be detected during phase two."
                  (roleId /= "")
              otherwise ->
                assert ("Expected DuplicateRoleDeclaration, got: " <> show otherwise) false
