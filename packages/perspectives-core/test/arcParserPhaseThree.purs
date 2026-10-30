module Test.Parsing.Arc.PhaseThree where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (elemIndex, filter, head, length, uncons)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (logShow)
import Foreign.Object (keys, lookup)
import Node.Encoding as ENC
import Node.FS.Aff (readTextFile)
import Node.Path as Path
import Parsing (ParseError)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (Namespace)
import Perspectives.Parsing.Arc (domain) as ARC
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo', toStableDomeinFile)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..), queryFunction, range)
import Perspectives.Representation.ADT (ADT(..), equalsOrSpecialises_)
import Perspectives.Representation.Action (effectOfAction)
import Perspectives.Representation.CNF (toConjunctiveNormalForm)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Perspective (Perspective(..), StateSpec(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..)) as QF
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (completeDeclaredType, completeExpandedType, declaredType, expandUnexpandedLeaves, toConjunctiveNormalForm_)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier(..), propertytype2string)
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.Representation.View (View(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Readable)
import Test.Parsing.DomeinFileSelectors (ensureCRole, ensureDescription, ensureERole, ensureEnumeratedProperty, ensurePerspectiveOn, ensurePropertyVerbsInState, ensureState, enumeratedPropertyIsFunctional, exists, failure, haveVerbs, isCalculationOf, objectOfPerspective, stateQuery, ensureOnEntry)
import Test.Perspectives.Utils (runP)
import Test.Unit (Test, TestF, TestSuite, suite, test, testOnly)
import Test.Unit.Assert (assert)

withDomeinFile :: forall a. Namespace -> DomeinFile Readable -> MonadPerspectives a -> MonadPerspectives a
withDomeinFile ns df mpa = do
  void $ storeDomeinFileInCache (ModelUri ns) (toStableDomeinFile df)
  r <- mpa
  removeDomeinFileFromCache (ModelUri ns)
  pure r

findQueryFunction :: QueryFunction -> QueryFunctionDescription -> Maybe QueryFunctionDescription
findQueryFunction target qfd
  | queryFunction qfd == target = Just qfd
  | otherwise =
      case qfd of
        SQD _ _ _ _ _ -> Nothing
        UQD _ _ inner _ _ _ -> findQueryFunction target inner
        BQD _ _ left right _ _ _ ->
          case findQueryFunction target left of
            Just found -> Just found
            Nothing -> findQueryFunction target right
        MQD _ _ arguments _ _ _ -> findInArray arguments
      where
      findInArray arguments = case uncons arguments of
        Nothing -> Nothing
        Just { head: first, tail: rest } ->
          case findQueryFunction target first of
            Just found -> Just found
            Nothing -> findInArray rest

completeTypeNormalisationSuite :: Free TestF Unit
completeTypeNormalisationSuite = test "PhaseThree completeType matches on-the-fly normalisation." do
  (r :: Either ParseError ContextE) <- runIndentParser "domain model://example.org#MyTestDomain\n  thing Base (mandatory)\n  thing Filler2 (mandatory)\n  thing Filler1 (mandatory) filledBy Filler2\n  thing Binder (mandatory) filledBy Filler1\n    aspect model://example.org#MyTestDomain$Base" ARC.domain
  case r of
    Left e -> assert (show e) false
    Right ctxt@(ContextE { id }) -> do
      runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple phaseTwoResult state) ->
        case phaseTwoResult of
          Left e -> assert (show e) false
          Right (DomeinFile dr') ->
            runP (phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
              case _ of
                Left e -> assert (show e) false
                Right (Tuple correctedDFR _) ->
                  runP $ withDomeinFile id (DomeinFile correctedDFR) do
                    for_ (keys correctedDFR.enumeratedRoles) \roleId -> do
                      role@(EnumeratedRole { completeType }) <- getEnumeratedRole (EnumeratedRoleType roleId)
                      declaredExpanded <- expandUnexpandedLeaves (declaredType role)
                      completeExpanded <- completeExpandedType role
                      expandedDnf <- completeDeclaredType role >>= expandUnexpandedLeaves >>= pure <<< toConjunctiveNormalForm
                      normalizedDnf <- completeDeclaredType role >>= toConjunctiveNormalForm_
                      liftAff $ assert
                        ("declaredType and completeExpandedType have the same normalized type for " <> roleId)
                        (toConjunctiveNormalForm declaredExpanded == toConjunctiveNormalForm completeExpanded)
                      liftAff $ assert ("expanding the declared type yields the stored completeType for " <> roleId) (expandedDnf == completeType)
                      liftAff $ assert ("The normalisation helper yields the stored completeType for " <> roleId) (normalizedDnf == completeType)

recursiveFillerComparisonSuite :: Free TestF Unit
recursiveFillerComparisonSuite = test "Recursive filler matching uses the filler role type, not the filler runtime chain." do
  (r :: Either ParseError ContextE) <- runIndentParser "domain model://example.org#MyTestDomain\n  thing Person (mandatory)\n  thing Employee (mandatory)\n  thing Binder (mandatory) filledBy Employee" ARC.domain
  case r of
    Left e -> assert (show e) false
    Right ctxt@(ContextE { id }) -> do
      runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple phaseTwoResult state) ->
        case phaseTwoResult of
          Left e -> assert (show e) false
          Right (DomeinFile dr') ->
            runP (phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
              case _ of
                Left e -> assert (show e) false
                Right (Tuple correctedDFR _) ->
                  runP $ withDomeinFile id (DomeinFile correctedDFR) do
                    employeeRole@(EnumeratedRole { completeType: employeeType }) <- getEnumeratedRole (EnumeratedRoleType "model://example.org#MyTestDomain$Employee")
                    personRole@(EnumeratedRole { completeType: personType }) <- getEnumeratedRole (EnumeratedRoleType "model://example.org#MyTestDomain$Person")
                    employeeRuntimeType <- toConjunctiveNormalForm_ (PROD [ declaredType employeeRole, declaredType personRole ])
                    liftAff $ assert "Employee itself is not a Person for recursive filler matching." (not $ employeeType `equalsOrSpecialises_` personType)
                    liftAff $ assert "An Employee filled by a Person would match Person only via its runtime filler chain." (employeeRuntimeType `equalsOrSpecialises_` personType)

theSuite :: Free TestF Unit
theSuite = suite "Perspectives.Parsing.Arc.PhaseThree" do
  completeTypeNormalisationSuite
  recursiveFillerComparisonSuite

  test "Testing qualifyActionRoles." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  thing Role (mandatory) filledBy YetAnotherRole\n  thing AnotherRole = Role >> binding\n  thing YetAnotherRole (mandatory)\n  user SomeUser\n    perspective on AnotherRole\n      all roleverbs" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr)) -> do
              (runP $ phaseThree dr state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple dr' _)) -> do
                    -- logShow dr'
                    ensureCRole "model://example.org#MyTestDomain$AnotherRole" dr' >>= exists
                    ensureERole "model://example.org#MyTestDomain$SomeUser" dr'
                      >>= \(EnumeratedRole { perspectives }) ->
                        case head perspectives of
                          Nothing -> assert "The perspective on AnotherRole should be present." false
                          Just perspective ->
                            objectOfPerspective perspective >>= \calculation ->
                              liftAff $ assert
                                ("Unexpected role calculation range: " <> show (range calculation))
                                (range calculation == (RDOM (SUM [ UET (RoleInContext { context: ContextType "model://example.org#MyTestDomain", role: EnumeratedRoleType "model://example.org#MyTestDomain$YetAnotherRole" }) ])))

  test "Testing qualifyActionRoles: External." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  user Driver\n    perspective on External\n      all roleverbs" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple correctedDFR _)) -> do
                    -- logShow correctedDFR
                    ensureERole "model://example.org#MyTestDomain$Driver" correctedDFR
                      >>= ensurePerspectiveOn "model://example.org#MyTestDomain$External"
                      >>=
                        exists

  test "Testing qualifyActionRoles: ContextHasNoRole." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  user Gast (mandatory)\n    perspective on SomeRole\n      all roleverbs\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left [ (ContextHasNoRole _ _ _ _) ]) -> assert "" true
                  otherwise -> do
                    logShow otherwise
                    assert "Expected the 'ContextHasNoRole' error" false

  test "Testing qualifyBindings: correct reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  thing Binder (mandatory) filledBy Bound\n  thing Bound (mandatory)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple correctedDFR@{ enumeratedRoles } _)) -> do
                    -- logShow correctedDFR
                    case lookup "model://example.org#MyTestDomain$Binder" enumeratedRoles of
                      Nothing -> assert "The model should have role 'model://example.org#MyTestDomain$Binder'." false
                      (Just (EnumeratedRole { binding })) -> assert
                        ("The binding of 'model://example.org#MyTestDomain$Binder' should be 'model://example.org#MyTestDomain$Bound', got " <> show binding)
                        (binding == (Just $ SUM [ UET (RoleInContext { context: (ContextType "model://example.org#MyTestDomain"), role: (EnumeratedRoleType "model://example.org#MyTestDomain$Bound") }) ]))

  test "Testing qualifyBindings: qualified reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  thing Binder (mandatory) filledBy model://example.org#MyTestDomain$Bound\n  thing Bound (mandatory)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple correctedDFR@{ enumeratedRoles } _)) -> do
                    -- logShow correctedDFR
                    case lookup "model://example.org#MyTestDomain$Binder" enumeratedRoles of
                      Nothing -> assert "The model should have role 'model://example.org#MyTestDomain$Binder'." false
                      (Just (EnumeratedRole { binding })) -> assert
                        ("The binding of 'model://example.org#MyTestDomain$Binder' should be 'model://example.org#MyTestDomain$Bound', got " <> show binding)
                        (binding == (Just $ SUM [ UET (RoleInContext { context: (ContextType "model://example.org#MyTestDomain"), role: (EnumeratedRoleType "model://example.org#MyTestDomain$Bound") }) ]))

  test "Testing qualifyBindings: prefixed reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  use my for model://example.org#MyTestDomain\n  thing Binder (mandatory) filledBy my:Bound\n  thing Bound (mandatory)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple correctedDFR@{ enumeratedRoles } _)) -> do
                    -- logShow correctedDFR
                    case lookup "model://example.org#MyTestDomain$Binder" enumeratedRoles of
                      Nothing -> assert "The model should have role 'model://example.org#MyTestDomain$Binder'." false
                      (Just (EnumeratedRole { binding })) -> assert
                        ("The binding of 'model://example.org#MyTestDomain$Binder' should be 'model://example.org#MyTestDomain$Bound', got " <> show binding)
                        (binding == (Just $ SUM [ UET (RoleInContext { context: (ContextType "model://example.org#MyTestDomain"), role: (EnumeratedRoleType "model://example.org#MyTestDomain$Bound") }) ]))

  test "Testing qualifyBindings: missing reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  thing Binder (mandatory) filledBy Bount\n  thing Bound (mandatory)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left [ (NotWellFormedName _ _) ]) -> assert "" true
                  otherwise -> do
                    assert ("Expected an unresolved binding error, got " <> show otherwise) false

  test "Testing qualifyBindings: two candidates for binding." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  thing Binder (mandatory) filledBy Bound\n  thing Bound (mandatory)\n  case Nested\n    thing Bound (mandatory)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                -- logShow x'
                case _ of
                  (Left [ e@(NotUniquelyIdentifyingPropertyType _ _ _) ]) -> do
                    -- logShow e
                    assert "" true
                  otherwise -> do
                    assert ("Expected an ambiguous binding error, got " <> show otherwise) false

  test "Testing qualifyPropertyReferences: correct reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  thing Feest (mandatory)\n    property Datum (mandatory, DateTime)\n    view ViewOpFeest (Datum)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              (runP $ phaseThree dr' state.postponedStateQualifiedParts Nil) >>=
                case _ of
                  (Left e) -> assert (show e) false
                  (Right (Tuple correctedDFR@{ views } _)) -> do
                    -- logShow correctedDFR
                    case lookup "model://example.org#MyTestDomain$Feest$ViewOpFeest" views of
                      Nothing -> assert "There should be a View 'model://example.org#MyTestDomain$Feest$ViewOpFeest'" false
                      (Just (View { propertyReferences })) -> case head (filter (\pref -> propertytype2string pref == "model://example.org#MyTestDomain$Feest$Datum") propertyReferences) of
                        (Just (ENP (EnumeratedPropertyType "model://example.org#MyTestDomain$Feest$Datum"))) -> assert "" true
                        otherwise -> assert "There should be a Property 'model://example.org#MyTestDomain$Feest$Datum' in ViewOpFeest" false
                    ensureEnumeratedProperty "model://example.org#MyTestDomain$Feest$Datum" correctedDFR >>=
                      enumeratedPropertyIsFunctional true

  test "Testing qualifyPropertyReferences: incorrect reference." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  thing Feest (mandatory)\n    property Datum (mandatory, DateTime)\n    view ViewOpFeest (Datu)\n" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left [ (UnknownProperty _ _ _) ]) -> assert "" true
                otherwise -> assert "The view refers to a non-existing property 'Datu' and that should be detected." false

  -- test "Testing qualifyPropertyReferences: reference to property on binding." do
  --   (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  thing Feest (mandatory) filledBy FeestVoorbereiding\n    view ViewOpFeest (Datum)\n  thing FeestVoorbereiding (mandatory)\n    property Datum (mandatory, DateTime)\n" ARC.domain
  --   case r of
  --     (Left e) -> assert (show e) false
  --     (Right ctxt@(ContextE { id })) -> do
  --       -- logShow ctxt
  --       runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
  --         case r of
  --           (Left e) -> assert (show e) false
  --           (Right (DomeinFile dr')) -> do
  --             -- logShow dr'
  --             x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
  --             case x' of
  --               (Left e) -> assert (show e) false
  --               Right (Tuple correctedDFR@{ views } _) -> do
  --                 -- logShow correctedDFR
  --                 -- Get the references from the view
  --                 case lookup "model://example.org#MyTestDomain$Feest$ViewOpFeest" views of
  --                   Nothing -> assert "There should be a View 'model://example.org#MyTestDomain$Feest$ViewOpFeest'" false
  --                   (Just (View { propertyReferences })) -> case elemIndex (ENP $ EnumeratedPropertyType "Datum") propertyReferences of
  --                     Nothing -> assert "There should be an unqualified PropertyType 'Datum' in 'model://example.org#MyTestDomain$Feest$ViewOpFeest'" true
  --                     (Just pr) -> do
  --                       candidates <- runP $ withDomeinFile "model://example.org#MyTestDomain" (DomeinFile correctedDFR) ((lookForUnqualifiedPropertyType_ "Datum") (ENR $ EnumeratedRoleType "model://example.org#MyTestDomain$Feest"))
  --                       case head candidates of
  --                         Nothing -> assert "We should be able to find the qualified version of 'Datum'" false
  --                         (Just (ENP (EnumeratedPropertyType "model://example.org#MyTestDomain$FeestVoorbereiding$Datum"))) | length candidates == 1 -> assert "" true
  --                         otherwise -> assert "There is only one property with local name 'Datum' defined but we've found more?!" false
  --                       xx <- runP $ withDomeinFile "model://example.org#MyTestDomain" (DomeinFile correctedDFR) (allProperties (ST (EnumeratedRoleType "model://example.org#MyTestDomain$Feest")))
  --                       case head xx of
  --                         Nothing -> assert "geen properties" false
  --                         (Just p) | length xx == 1 -> assert "The properties of 'model://example.org#MyTestDomain$Feest' should include 'model://example.org#MyTestDomain$FeestVoorbereiding$Datum'" (p == ENP (EnumeratedPropertyType "model://example.org#MyTestDomain$FeestVoorbereiding$Datum"))
  --                         otherwise -> assert "There is only one property defined for 'Feest'" false

  --                 case lookup "model://example.org#MyTestDomain$Feest$ViewOpFeest" views of
  --                   Nothing -> assert "There should be a View 'model://example.org#MyTestDomain$Feest$ViewOpFeest'" false
  --                   (Just (View { propertyReferences })) -> case head (filter (\pref -> propertytype2string pref == "model://example.org#MyTestDomain$FeestVoorbereiding$Datum") propertyReferences) of
  --                     (Just (ENP (EnumeratedPropertyType "model://example.org#MyTestDomain$FeestVoorbereiding$Datum"))) -> assert "" true
  --                     otherwise -> assert "There should be a Property 'model://example.org#MyTestDomain$FeestVoorbereiding$Datum' in ViewOpFeest" false

  test "Testing qualifyViewReferences: reference to View on Action." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  thing Feest (mandatory) filledBy FeestVoorbereiding\n    property BigParty = NrOfGuests > 10\n    property NrOfGuests (mandatory, Number)\n    view ViewOpFeest (Datum, BigParty)\n  thing FeestVoorbereiding (mandatory)\n    property Datum (mandatory, DateTime)\n  user Guest (mandatory)\n    perspective on Feest\n      view ViewOpFeest verbs (Consult)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                Right (Tuple dfr _) ->
                  ensureERole "model://example.org#MyTestDomain$Guest" dfr
                    >>= ensurePerspectiveOn "model://example.org#MyTestDomain$Feest"
                    >>= ensurePropertyVerbsInState (SubjectState $ StateIdentifier "model://example.org#MyTestDomain$Guest")
                    >>=
                      Universal `haveVerbs` [ Consult ]

  test "Testing qualifyPropertyReferences: reference to a Calculated Property." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  thing Feest (mandatory) filledBy FeestVoorbereiding\n    property NrOfGuests (mandatory, Number)\n    property BigParty = NrOfGuests > 10\n    view ViewOpFeest (Datum, BigParty)\n  thing FeestVoorbereiding (mandatory)\n    property Datum (mandatory, DateTime)\n  user Guest (mandatory)\n    perspective on Feest\n      view ViewOpFeest verbs (Consult)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (addAllExternalFunctions *> traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                Right (Tuple correctedDFR@{ views } _) -> do
                  -- logShow correctedDFR
                  -- get the action
                  case lookup "model://example.org#MyTestDomain$Feest$ViewOpFeest" views of
                    Nothing -> assert "There should be a View 'model://example.org#MyTestDomain$Feest$ViewOpFeest'." false
                    (Just (View { propertyReferences })) -> case elemIndex (CP (CalculatedPropertyType "model://example.org#MyTestDomain$Feest$BigParty")) propertyReferences of
                      Nothing -> assert "There should be a CalculatedProperty 'BigParty' in the View 'ViewOpFeest'" false
                      otherwise -> assert "" true

  test "A Context with a Computed Role." do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#MyTestDomain\n  use cdb for model://perspectives.domains#Couchdb\n  thing MyRole = callExternal cdb:RoleInstances( \"model://example.org#MyTestDomain$SubContext$Modellen\" ) returns model://example.org#MyTestDomain$SubContext$Modellen\n  case SubContext\n    thing Modellen (mandatory)\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error: " <> show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (addAllExternalFunctions *> traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert ("PhaseThree error:" <> show e) false
                (Right (Tuple correctedDFR@{ calculatedRoles } _)) -> do
                  -- logShow correctedDFR
                  case lookup "model://example.org#MyTestDomain$MyRole" calculatedRoles of
                    Nothing -> assert "There should be a role 'MyRole'" false
                    Just (CalculatedRole { calculation }) ->
                      case calculation of
                        Q c -> do
                          assert ("Unexpected calculation range: " <> show (range c))
                            (range c == (RDOM (ST $ RoleInContext { context: (ContextType "model://example.org#MyTestDomain$SubContext"), role: (EnumeratedRoleType "model://example.org#MyTestDomain$SubContext$Modellen") })))
                          assert "The queryfunction of the calculation should be '(ExternalCoreRoleGetter model://perspectives.domains#Couchdb$RoleInstances)'"
                            case queryFunction c of
                              (ExternalCoreRoleGetter "model://perspectives.domains#Couchdb$RoleInstances") -> true
                              otherwise -> false
                        otherwise -> assert "calculation is not compiled" false

  test "Perspective in state" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#Test\n  user Gast (mandatory)\n    property Voornaam (mandatory, String)\n    view AnotherView (Voornaam)\n    perspective on Party\n      in state model://example.org#Test$Party$LateParty\n        view ViewOnParty verbs (Consult)\n  thing Party (mandatory)\n    state LateParty = Datum > '2019-11-04'\n      perspective of Gast\n        view ViewOnParty verbs (Consult)\n    property Naam (mandatory, String)\n    property Datum (mandatory, DateTime)\n    view ViewOnParty (Naam)\n    view AnotherView (Datum)" ARC.domain
    case r of
      (Left e) -> assert (show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (addAllExternalFunctions *> traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert (show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                (Left e) -> assert (show e) false
                (Right (Tuple correctedDFR _)) -> do
                  -- logShow correctedDFR
                  ensureERole "model://example.org#Test$Gast" correctedDFR
                    >>= ensurePerspectiveOn "model://example.org#Test$Party"
                    >>= ensurePropertyVerbsInState (ObjectState $ StateIdentifier "model://example.org#Test$Party$LateParty")
                    >>=
                      exists

  test "Automatic effect on entry (two assignments)" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop2 (mandatory, Number)\n    perspective on Gast\n      props (Prop2) verbs (SetPropertyValue, DeleteProperty)\n  thing Party (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do for Gast\n          Prop2 = 10 for context >> Gast\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> do
                  -- logShow correctedDFR
                  ensureEnumeratedProperty "model://example.org#Test$Party$Prop1" correctedDFR >>=
                    enumeratedPropertyIsFunctional true
                  ensureState "model://example.org#Test$Party$SomeState" correctedDFR
                    >>= stateQuery
                    >>= ensureDescription
                    >>=
                      \qfd -> do
                        case queryFunction qfd of
                          (BinaryCombinator GreaterThanF) -> pure unit
                          _ -> failure "The condition should have operator '>'"

  test "Automatic effect on entry (delete property)" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop2 (mandatory, Number)\n    perspective on Gast\n      props (Prop2) verbs (SetPropertyValue, DeleteProperty)\n  thing Party (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do for Gast\n          delete property Prop2 from context >> Gast\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> do
                  -- logShow correctedDFR
                  ensureEnumeratedProperty "model://example.org#Test$Party$Prop1" correctedDFR >>=
                    enumeratedPropertyIsFunctional true
                  ensureState "model://example.org#Test$Party$SomeState" correctedDFR
                    >>= stateQuery
                    >>= ensureDescription
                    >>=
                      \qfd -> do
                        case queryFunction qfd of
                          (BinaryCombinator GreaterThanF) -> pure unit
                          _ -> failure "The condition should have operator '>'"

  test "Automatic effect on entry (use of object state)" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop2 (mandatory, Number)\n    perspective on Party\n      props (Prop1) verbs (SetPropertyValue)\n      on entry of object state SomeState\n        do\n          Prop1 = 10\n  thing Party (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> do
                  -- logShow correctedDFR
                  ensureEnumeratedProperty "model://example.org#Test$Party$Prop1" correctedDFR >>=
                    enumeratedPropertyIsFunctional true
                  ensureState "model://example.org#Test$Party$SomeState" correctedDFR
                    >>= stateQuery
                    >>= ensureDescription
                    >>=
                      \qfd -> do
                        -- logShow qfd
                        case queryFunction qfd of
                          (BinaryCombinator GreaterThanF) -> pure unit
                          _ -> failure "The condition should have operator '>'"

  test "Bot Action with move" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry \n        do for Gast\n          move context >> C1 >> binding >> context >> Employee to context >> C2 >> binding >> context\n  context C1 filledBy Company\n  context C2 filledBy Company\n  party Company\n    user Employee\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> do
                  ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
                    >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
                    >>= pure <<< effectOfAction
                    >>= case _ of
                      action -> case findQueryFunction QF.Move action of
                        Just (BQD _ _ rle cte _ _ _) -> do
                          assert ("The moved role query should select Company.Employee: " <> show rle)
                            ( case findQueryFunction (QF.RolGetter $ ENR $ EnumeratedRoleType "model://example.org#Test$Company$Employee") rle of
                                Just _ -> true
                                Nothing -> false
                            )
                          assert ("The destination context should be selected through C2: " <> show cte)
                            ( case findQueryFunction (QF.RolGetter $ ENR $ EnumeratedRoleType "model://example.org#Test$C2") cte of
                                Just _ -> true
                                Nothing -> false
                            )
                        Just unexpected -> assert ("The move query should be binary, got " <> show unexpected) false
                        Nothing -> assert ("The action should contain a move query, got " <> show action) false

  test "Bot Action with move to current context" do
    (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser "domain model://example.org#Test\n  party Company\n    user Employee\n    user Gast (mandatory)\n      property Prop1 (mandatory, Number)\n      state SomeState = Prop1 > 10\n        on entry\n          do\n            move context >> Employee\n" ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r state) ->
          case r of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x' <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x' of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) ->
                  ensureState "model://example.org#Test$Company$Gast$SomeState" correctedDFR
                    >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Company$Gast"))
                    >>= pure <<< effectOfAction
                    >>=
                      ( case _ of
                          action -> case findQueryFunction QF.Move action of
                            Just (BQD _ _ rle cte _ _ _) -> do
                              assert ("The moved role query should select Company.Employee: " <> show rle)
                                ( case findQueryFunction (QF.RolGetter $ ENR $ EnumeratedRoleType "model://example.org#Test$Company$Employee") rle of
                                    Just _ -> true
                                    Nothing -> false
                                )
                              assert ("Unexpected destination context: " <> show cte)
                                ( case cte of
                                    SQD _ (DataTypeGetter ContextF) (CDOM (UET (ContextType "model://example.org#Test$Company"))) _ _ -> true
                                    otherwise -> false
                                )
                            Just unexpected -> assert ("Unexpected move query: " <> show unexpected) false
                            Nothing -> assert ("The action should contain a move query, got " <> show action) false
                          otherwise -> assert "Side effect expected" false
                      )

  domainTest "Move a role from another context into the current context"
    "domain model://example.org#Test\n  context C1 filledBy Company\n  context C2 filledBy Company\n  party Company\n    user Employee\n    user Gast (mandatory)\n      property Prop1 (mandatory, Number)\n      state SomeState = Prop1 > 10\n        on entry\n          do\n            move context >> extern >> binder C1 >> context >> C2 >> binding >> context >> Employee\n"
    \correctedDFR ->
      ensureState "model://example.org#Test$Company$Gast$SomeState" correctedDFR
        >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Company$Gast"))
        >>= pure <<< effectOfAction
        >>= \action ->
          case findQueryFunction QF.Move action of
            Just (BQD _ _ source destination _ _ _) -> do
              assert "The source must select Employee from C2"
                ( case findQueryFunction (RolGetter $ ENR $ EnumeratedRoleType "model://example.org#Test$C2") source of
                    Just _ -> case findQueryFunction (RolGetter $ ENR $ EnumeratedRoleType "model://example.org#Test$Company$Employee") source of
                      Just _ -> true
                      Nothing -> false
                    Nothing -> false
                )
              assert ("Unexpected implicit destination: " <> show destination)
                ( case destination of
                    SQD _ (DataTypeGetter ContextF) (CDOM (UET (ContextType "model://example.org#Test$Company"))) _ _ -> true
                    otherwise -> false
                )
            Just unexpected -> assert ("Unexpected move query: " <> show unexpected) false
            Nothing -> assert ("The action should contain a move query, got " <> show action) false

  domainTest "Bot Action with bind"
    "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    perspective on EreGast\n      all roleverbs\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind context >> Gast to EreGast\n  user EreGast filledBy Gast"
    \correctedDFR ->
      ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
        >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
        >>= pure <<< effectOfAction
        >>= \action ->
          case findQueryFunction (Bind $ EnumeratedRoleType "model://example.org#Test$EreGast") action of
            Just (BQD _ _ bndg _ _ _ _) -> assert ("Unexpected binding for bind action: " <> show bndg)
              (range bndg == RDOM (UET (RoleInContext { context: ContextType "model://example.org#Test", role: EnumeratedRoleType "model://example.org#Test$Gast" })))
            Just unexpected -> assert ("Unexpected bind query: " <> show unexpected) false
            Nothing -> assert ("The action should contain a bind query, got " <> show action) false

  expectError "Bind to role without non-matching possible bindings"
    "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    perspective on EreGast\n      all roleverbs\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind context >> Gast to EreGast\n  user Organisator\n    property Prop2\n  user EreGast filledBy Organisator\n"
    case _ of
      (Left (RoleDoesNotBind _ _ _)) -> assert "ok" true
      otherwise -> do
        logShow otherwise
        assert ("Expected the error RoleDoesNotBind, got " <> show otherwise) false

  expectError "Bind: binding not a role"
    "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind context >> Gast >> Prop1 to EreGast\n  user EreGast\n"
    case _ of
      (Left (NotARoleDomain _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert ("Expected the error NotARoleDomain, got " <> show otherwise) false

  expectError "Bind: roletype not in context"
    "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind context >> Gast to AnotherRole\n  user EreGast\n"
    case _ of
      (Left (ContextHasNoRole _ _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert ("Expected the error ContextHasNoRole, got " <> show otherwise) false

  domainTest "On entry with bind in an explicit context"
    "domain model://example.org#Test\n  case Party\n    user Gast (mandatory)\n      property Prop1 (mandatory, Number)\n      perspective on EreGast\n        all roleverbs\n      state SomeState = Prop1 > 10\n        on entry\n          do\n            bind context >> Gast to EreGast in context\n    user EreGast filledBy Gast\n"
    \correctedDFR ->
      ensureState "model://example.org#Test$Party$Gast$SomeState" correctedDFR
        >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Party$Gast"))
        >>= pure <<< effectOfAction
        >>= \action ->
          case findQueryFunction (Bind $ EnumeratedRoleType "model://example.org#Test$Party$EreGast") action of
            Just (BQD _ _ bndg cte _ _ _) -> do
              assert ("Unexpected binding source: " <> show bndg)
                (range bndg == RDOM (UET (RoleInContext { context: ContextType "model://example.org#Test$Party", role: EnumeratedRoleType "model://example.org#Test$Party$Gast" })))
              assert ("Unexpected explicit context: " <> show cte)
                ( case cte of
                    SQD _ (DataTypeGetter ContextF) (CDOM (UET (ContextType "model://example.org#Test$Party"))) _ _ -> true
                    otherwise -> false
                )
            Just unexpected -> assert ("Unexpected bind query: " <> show unexpected) false
            Nothing -> assert ("The action should contain a bind query, got " <> show action) false

  domainTest "On entry with bind in another context"
    "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    perspective on AParty >> binding >> context >> EreGast\n      all roleverbs\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind context >> Gast to EreGast in context >> AParty >> binding >> context\n  context AParty filledBy Party\n  case Party\n    user EreGast filledBy Gast\n"
    \correctedDFR ->
      ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
        >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
        >>= pure <<< effectOfAction
        >>= \action ->
          case findQueryFunction (Bind $ EnumeratedRoleType "model://example.org#Test$Party$EreGast") action of
            Just (BQD _ _ filler destination _ _ _) -> do
              assert ("Unexpected cross-context filler: " <> show filler)
                (range filler == RDOM (UET (RoleInContext { context: ContextType "model://example.org#Test", role: EnumeratedRoleType "model://example.org#Test$Gast" })))
              assert ("Unexpected cross-context destination: " <> show destination)
                (range destination == CDOM (SUM [ UET (ContextType "model://example.org#Test$Party") ]))
              assert "The destination must navigate through AParty"
                ( case findQueryFunction (RolGetter $ ENR $ EnumeratedRoleType "model://example.org#Test$AParty") destination of
                    Just _ -> true
                    Nothing -> false
                )
              assert "The destination must follow AParty's filler"
                ( case findQueryFunction (DataTypeGetter FillerF) destination of
                    Just _ -> true
                    Nothing -> false
                )
            Just unexpected -> assert ("Unexpected cross-context bind query: " <> show unexpected) false
            Nothing -> assert ("The action should bind Party.EreGast, got " <> show action) false

  expectError "Bind: in-clause does not select a context"
    "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind context >> Gast to EreGast in context >> AParty >> binding\n  context AParty filledBy Party\n  case Party\n    user EreGast filledBy Gast\n"
    case _ of
      (Left (NotAContextDomain _ _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert ("Expected the error NotAContextDomain, got " <> show otherwise) false

  expectError "Bind: in-clause does selects non-functional context"
    "domain model://example.org#Test\n  user Gast (relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind context >> Gast to EreGast in context >> AParty >> binding >> context\n  context AParty (mandatory, relational) filledBy Party\n  case Party\n    user EreGast filledBy Gast\n"
    case _ of
      (Left (NotFunctional _ _ _)) -> assert "ok" true
      otherwise -> do
        logShow otherwise
        assert ("Expected the error NotFunctional, got " <> show otherwise) false

  expectError "Bind: bind to calculated role"
    "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do \n          bind context >> Gast to Organiser in context >> AParty >> binding >> context\n  context AParty filledBy Party\n  case Party\n    user EreGast filledBy Gast\n    user Organiser = EreGast\n"
    case _ of
      (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
      otherwise -> do
        assert ("Expected the error CannotCreateCalculatedRole, got " <> show otherwise) false

  expectError "Bind: bind non-functional to functional role"
    "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    perspective on EreGast\n      all roleverbs\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind context >> Gast to EreGast\n    property Prop1 (mandatory, Number)\n  user EreGast (mandatory) filledBy Gast\n"
    case _ of
      (Left (NotFunctional _ _ _)) -> assert "ok" true
      -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert ("Expected the error NotFunctional, got " <> show otherwise) false

  domainTest "Bot Action with bind_"
    "domain model://example.org#Test\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    perspective on EreGast\n      all roleverbs\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind_ context >> Gast to context >> EreGast\n  user EreGast (mandatory) filledBy Gast\n"
    \correctedDFR -> do
      -- logShow correctedDFR
      ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
        >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
        >>= pure <<< effectOfAction
        >>= \action ->
          case findQueryFunction Bind_ action of
            Just (BQD _ _ bndg binder _ _ _) -> do
              assert ("Unexpected bind_ source: " <> show bndg)
                (range bndg == RDOM (UET (RoleInContext { context: ContextType "model://example.org#Test", role: EnumeratedRoleType "model://example.org#Test$Gast" })))
              assert ("Unexpected bind_ target: " <> show binder)
                (range binder == RDOM (UET (RoleInContext { context: ContextType "model://example.org#Test", role: EnumeratedRoleType "model://example.org#Test$EreGast" })))
            Just unexpected -> assert ("Unexpected bind_ query: " <> show unexpected) false
            Nothing -> assert ("The action should contain a bind_ query, got " <> show action) false

  expectError "Bind_: bind non-functional to functional role"
    "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    perspective on EreGast\n      all roleverbs\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          bind_ context >> Gast to context >> EreGast\n  user EreGast (mandatory) filledBy Gast\n"
    case _ of
      (Left (NotFunctional _ _ _)) -> assert "ok" true
      -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert ("Expected the error NotFunctional, got " <> show otherwise) false

  -- domainTest "Bot Action with unqualified unbind"
  --   "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          unbind Gast\n  user EreGast (mandatory) filledBy Gast\n"
  --   \correctedDFR -> do
  --     -- logShow correctedDFR
  --     ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
  --       >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
  --       >>= pure <<< effectOfAction
  --       >>=
  --         case _ of
  --           (UQD _ qf bndg _ _ _) -> do
  --             assert "The queryfunction should be unbind"
  --               ( case qf of
  --                   Unbind Nothing -> true
  --                   otherwise -> false
  --               )
  --             case bndg of
  --               (SQD _ (RolGetter (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))) _ _ _) ->
  --                 assert "The binding should be a rolgetter for Gast" true
  --               otherwise -> assert "The binding should be a rolgetter" false
  --           otherwise -> assert "Side effect expected" false

  -- domainTest "Bot Action with qualified unbind"
  --   "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          unbind Gast from EreGast\n  user EreGast (mandatory) filledBy Gast\n"
  --   \correctedDFR -> do
  --     ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
  --       >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
  --       >>= pure <<< effectOfAction
  --       >>=
  --         case _ of
  --           (UQD _ qf bndg _ _ _) -> do
  --             assert "The queryfunction should be unbind"
  --               ( case qf of
  --                   Unbind (Just (EnumeratedRoleType "model://example.org#Test$EreGast")) -> true
  --                   otherwise -> false
  --               )
  --             case bndg of
  --               (SQD _ (RolGetter (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))) _ _ _) ->
  --                 assert "The binding should be a rolgetter for Gast" true
  --               otherwise -> assert "The binding should be a rolgetter" false
  --           otherwise -> assert "Side effect expected" false

  -- expectError "Unbind: non-existing binder"
  --   "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          unbind Gast from AnotherRole\n  user EreGast (mandatory) filledBy Gast\n"
  --   case _ of
  --     (Left (UnknownRole _ _)) -> assert "ok" true
  --     -- (Left (CannotCreateCalculatedRole _ _ _)) -> assert "ok" true
  --     otherwise -> do
  --       -- logShow otherwise
  --       assert ("Expected the error UnknownRole, got " <> show otherwise) false

  -- expectError "Unbind: binder does not bind"
  --   "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          unbind Gast from EreGast\n  user EreGast (mandatory) filledBy Organisator\n  user Organisator\n    property Prop2\n"
  --   case _ of
  --     (Left (LocalRoleDoesNotBind _ _ _ _)) -> assert "ok" true
  --     otherwise -> do
  --       -- logShow otherwise
  --       assert ("Expected the error LocalRoleDoesNotBind, got " <> show otherwise) false

  domainTest "Bot Action with delete role"
    "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    perspective on Gast\n      all roleverbs\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          delete role Gast\n"
    \correctedDFR -> do
      ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
        >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
        >>= pure <<< effectOfAction
        >>= case _ of
          action -> case findQueryFunction (DeleteRole $ EnumeratedRoleType "model://example.org#Test$Gast") action of
            Just (UQD _ _ bndg _ _ _) -> case bndg of
              (SQD _ (DataTypeGetter ContextF) _ _ _) -> assert "The default delete target should be its containing context" true
              otherwise -> assert ("Unexpected delete-role binding: " <> show bndg) false
            Just unexpected -> assert ("Unexpected delete-role query: " <> show unexpected) false
            Nothing -> assert ("The action should contain a delete-role query, got " <> show action) false

  domainTest "Bot Action with delete property and default object"
    "domain model://example.org#Test\n  user Gast (mandatory)\n    perspective on Gast\n      props (Prop1) verbs (DeleteProperty)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          delete property Prop1\n"
    \correctedDFR -> do
      ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
        >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
        >>= pure <<< effectOfAction
        >>= case _ of
          action -> case findQueryFunction (QF.DeleteProperty $ EnumeratedPropertyType "model://example.org#Test$Gast$Prop1") action of
            Just (UQD _ _ bndg _ _ _) -> case bndg of
              (SQD _ (DataTypeGetter IdentityF) _ _ _) -> assert "The default delete target should be the action context" true
              otherwise -> assert ("Unexpected default delete target: " <> show bndg) false
            Just unexpected -> assert ("Unexpected delete-property query: " <> show unexpected) false
            Nothing -> assert ("The action should contain a delete-property query, got " <> show action) false

  expectError "Bot Action with delete property, property doesn't exist."
    "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          delete property AnotherProp\n"
    case _ of
      (Left (RoleHasNoEnumeratedProperty _ _ _ _)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert ("Expected the error MissingRoleForPropertyAssignment, got " <> show otherwise) false

  expectError "Bot Action with delete property, property is calculated."
    "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    perspective on Gast\n      props (Prop2) verbs (DeleteProperty)\n    property Prop1 (mandatory, Number)\n    property Prop2 = Prop1\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          delete property Prop2 from context >> Gast\n"
    case _ of
      (Left (CannotModifyCalculatedProperty _ _ _)) -> assert "ok" true
      otherwise -> do
        logShow otherwise
        assert ("Expected the error CannotCreateCalculatedProperty, got " <> show otherwise) false

  domainTest "Bot Action with property assignment and default object"
    "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    perspective on Gast\n      props (Prop1) verbs (AddPropertyValue)\n    property Prop1 (mandatory, Number)\n    property Prop2 = Prop1\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          Prop1 =+ 10 for context >> Gast\n"
    \correctedDFR -> do
      ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
        >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
        >>= pure <<< effectOfAction
        >>= case _ of
          action -> case findQueryFunction (QF.AddPropertyValue $ EnumeratedPropertyType "model://example.org#Test$Gast$Prop1") action of
            Just (BQD _ _ val role _ _ _) -> do
              assert "The value should be a Constant"
                ( case val of
                    (SQD _ (Constant PNumber "10") _ _ _) -> true
                    otherwise -> false
                )
              assert ("Unexpected property-assignment target: " <> show role)
                (range role == RDOM (UET (RoleInContext { context: ContextType "model://example.org#Test", role: EnumeratedRoleType "model://example.org#Test$Gast" })))
            Just unexpected -> assert ("Unexpected property-add query: " <> show unexpected) false
            Nothing -> assert ("The action should contain a property-add query, got " <> show action) false

  expectError "Bot Action with property assignment and wrong value range."
    "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    perspective on Gast\n      props (Prop1) verbs (AddPropertyValue)\n    property Prop1 (mandatory, Number)\n    property Prop2 = Prop1\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          Prop1 =+ true for context >> Gast\n"
    case _ of
      (Left (WrongPropertyRange _ _ PNumber PBool)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert ("Expected the error WrongPropertyRange, got " <> show otherwise) false

  expectError "Bot Action with property assignment and not even a property range."
    "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    perspective on Gast\n      props (Prop1) verbs (AddPropertyValue)\n    property Prop1 (mandatory, Number)\n    property Prop2 = Prop1\n    state SomeState = Gast >> Prop1 > 10\n      on entry\n        do\n          Prop1 =+ context >> Gast for context >> Gast\n"
    case _ of
      (Left (NotAPropertyRange _ _ PNumber)) -> assert "ok" true
      otherwise -> do
        -- logShow otherwise
        assert ("Expected the error NotAPropertyRange, got " <> show otherwise) false

  domainTest "Bot Action with property assignment on another role"
    "domain model://example.org#Test\n  user Gast (mandatory, relational)\n    perspective on Organiser\n      props (Prop2) verbs (AddPropertyValue)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do \n          Prop2 =+ 10 for context >> Organiser\n  user Organiser\n    property Prop2 (mandatory, Number)\n"
    \correctedDFR ->
      ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
        >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
        >>= pure <<< effectOfAction
        >>= case _ of
          action -> case findQueryFunction (QF.AddPropertyValue $ EnumeratedPropertyType "model://example.org#Test$Organiser$Prop2") action of
            Just (BQD _ _ val role _ _ _) -> do
              assert "The value should be a Constant"
                ( case val of
                    (SQD _ (Constant PNumber "10") _ _ _) -> true
                    otherwise -> false
                )
              assert ("Unexpected property-assignment target: " <> show role)
                (range role == RDOM (UET (RoleInContext { context: ContextType "model://example.org#Test", role: EnumeratedRoleType "model://example.org#Test$Organiser" })))
            Just unexpected -> assert ("Unexpected property-add query: " <> show unexpected) false
            Nothing -> assert ("The action should contain a property-add query, got " <> show action) false

  domainTest "Bot Action with callEffect"
    "domain model://example.org#Test\n  use cdb for model://perspectives.domains#Couchdb\n  user Gast (mandatory)\n    property Prop1 (mandatory, Number)\n    state SomeState = Prop1 > 10\n      on entry\n        do\n          callEffect cdb:AddModelToLocalStore( context >> AModel >> Name )\n  thing AModel\n    property Name (mandatory, String)\n"
    \correctedDFR ->
      ensureState "model://example.org#Test$Gast$SomeState" correctedDFR
        >>= ensureOnEntry (ENR (EnumeratedRoleType "model://example.org#Test$Gast"))
        >>= pure <<< effectOfAction
        >>= case _ of
          action -> case findQueryFunction (ExternalEffectFullFunction "model://perspectives.domains#Couchdb$AddModelToLocalStore") action of
            Just (MQD _ _ args _ _ _) -> do
              assert "There should be one argument" (length args == 1)
            Just unexpected -> assert ("Unexpected external effect query: " <> show unexpected) false
            Nothing -> assert ("The action should contain the external effect query, got " <> show action) false

-- fileTestOnly "Read model:System" "perspectivesSysteem.arc"

-- extractEffect :: Maybe SideEffect -> QueryFunctionDescription
-- extractEffect = unsafePartial extractEffect_
--   where
--     extractEffect_ :: Partial => Maybe SideEffect -> QueryFunctionDescription
--     extractEffect_ (Just (EF (UQD _ _ (BQD _ _ _ mqd _ _ _) _ _ _))) = mqd

-- x :: DomeinFileRecord -> MonadPerspectives (Array RoleType)
-- x correctedDFR = withDomeinFile "model://example.org#MyTestDomain"
--   (DomeinFile correctedDFR)
--   ((runTypeLevelToArray (ST (ContextType "model://example.org#MyTestDomain")) (lookForUnqualifiedRoleTypeOfADT "Guest")))

expectError :: TestName -> ModelText -> Criterium -> TestSuite
expectError = expectErrorX test

expectErrorOnly :: TestName -> ModelText -> Criterium -> TestSuite
expectErrorOnly = expectErrorX testOnly

type TestName = String
type ModelText = String
type Criterium = ((Either PerspectivesError (DomeinFileRecord Readable)) -> Aff Unit)

expectErrorX
  :: (String -> Test -> TestSuite)
  -> TestName
  -> ModelText
  -> Criterium
  -> TestSuite
expectErrorX theTest testName modelText resultTester = do
  theTest testName do
    (r :: Either ParseError ContextE) <- runIndentParser modelText ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r' state) ->
          case r' of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x of
                Right (Tuple r _) -> resultTester (Right r)
                Left errs -> for_ errs (resultTester <<< Left)

-- resultTester x

type DomainTester = (DomeinFileRecord Readable -> Aff Unit)

domainTest :: TestName -> ModelText -> DomainTester -> TestSuite
domainTest = domainTestX test

domainTestOnly :: TestName -> ModelText -> DomainTester -> TestSuite
domainTestOnly = domainTestX testOnly

type FileName = String

fileTestOnly :: TestName -> FileName -> TestSuite
fileTestOnly testName fileName =
  testOnly testName do
    modelDirectory <- pure "/Users/joopringelberg/Code/perspectives-core/src/model"
    modelText <- readTextFile ENC.UTF8 (Path.concat [ modelDirectory, fileName ])
    (r :: Either ParseError ContextE) <- runIndentParser modelText ARC.domain
    case r of
      (Left e) -> assert ("Parser error:" <> show e) false
      (Right ctxt@(ContextE { id })) -> do
        -- logShow ctxt
        runPhaseTwo' (traverseDomain ctxt) >>= \(Tuple r' state) ->
          case r' of
            (Left e) -> assert ("PhaseTwo error:" <> show e) false
            (Right (DomeinFile dr')) -> do
              -- logShow dr'
              x <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
              case x of
                Left e -> assert ("PhaseThree error:" <> show e) false
                Right (Tuple correctedDFR _) -> assert "OK" true

domainTestX
  :: (String -> Test -> TestSuite)
  -> TestName
  -> ModelText
  -> DomainTester
  -> TestSuite
domainTestX theTest testName modelText domainTester = theTest testName do
  (r :: Either ParseError ContextE) <- {-pure $ unwrap $-}  runIndentParser modelText ARC.domain
  case r of
    (Left e) -> assert ("Parser error:" <> show e) false
    (Right ctxt@(ContextE { id })) -> do
      -- logShow ctxt
      runPhaseTwo' (addAllExternalFunctions *> traverseDomain ctxt) >>= \(Tuple r' state) ->
        case r' of
          (Left e) -> assert ("PhaseTwo error:" <> show e) false
          (Right (DomeinFile dr')) -> do
            -- logShow dr'
            x <- runP $ phaseThree dr' state.postponedStateQualifiedParts Nil
            case x of
              Left e -> assert ("PhaseThree error:" <> show e) false
              Right (Tuple correctedDFR _) -> domainTester correctedDFR
