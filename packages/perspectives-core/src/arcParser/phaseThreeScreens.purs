-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
module Perspectives.Parsing.Arc.PhaseThree.Screens where

import Control.Monad.State (gets) as State
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, cons, elemIndex, filter, filterA, find, findIndex, foldM, fromFoldable, head, length, nub, null)
import Data.Array.Partial (head) as ARRP
import Data.Foldable (for_)
import Data.List (List) as LIST
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Foreign.Object (Object, keys, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, (###=), (###>))
import Perspectives.Data.EncodableMap (empty, insert) as EM
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (areLastSegmentsOf, concatenateSegments, isTypeUri, qualifyWith, startsWithSegments, typeUri2ModelUri_)
import Perspectives.ModelDependencies (chatAspect)
import Perspectives.Parsing.Arc.AST (ChatE(..), FreeFormScreenE(..), MarkDownE(..), PropertyFacet(..), RoleIdentification(..), WhoWhatWhereScreenE(..))
import Perspectives.Parsing.Arc.AST (ColumnE(..), FormE(..), FreeFormScreenE(..), MarkDownE, PropsOrView(..), RowE(..), ScreenE(..), ScreenElement(..), TabE(..), TableE(..), TableFormE(..), WhatE(..), WidgetCommonFields) as AST
import Perspectives.Parsing.Arc.Expression (endOf, startOf)
import Perspectives.Parsing.Arc.Expression.AST (SimpleStep(..), Step(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, getsDF, lift2, modifyDF, throwError, withDomeinFile)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.ExpressionCompiler (compileExpression, compileStep, qualifyLocalRoleName)
import Perspectives.Query.QueryTypes (Domain(..), domain2roleType, functional, range, roleInContext2Role)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT)
import Perspectives.Representation.Class.PersistentType (getCalculatedRole, getEnumeratedRole, tryGetPerspectType)
import Perspectives.Representation.Class.Property (hasFacet)
import Perspectives.Representation.Class.Role (allProperties, displayName, perspectivesOfRoleType, roleADTOfRoleType, roleTypeIsFunctional)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), addProperty, expandPropSet, expandVerbs, perspectiveSupportsPropertyForVerb, perspectiveSupportsRoleVerbs)
import Perspectives.Representation.ScreenDefinition (ChatDef(..), ColumnDef(..), FormDef(..), MarkDownDef(..), RowDef(..), ScreenDefinition(..), ScreenElementDef(..), ScreenKey(..), ScreenMap, TabDef(..), TableDef(..), TableFormDef(..), What(..), Who(..), WhoWhatWhereScreenDef(..), WidgetCommonFieldsDef)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), optimistic, pessimistic)
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType(..), propertytype2string, roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb, roleVerbList2Verbs)
import Perspectives.Representation.View (View(..))
import Perspectives.Types.ObjectGetters (equalsOrGeneralisesRoleType_, lookForUnqualifiedPropertyType, lookForUnqualifiedPropertyType_)
import Prelude (Unit, bind, discard, eq, flip, map, not, pure, show, unit, ($), (<$>), (<<<), (==), (>>=), (<#>))


handleScreens :: LIST.List AST.ScreenE -> PhaseThree Unit
handleScreens screenEs = do
  df@{id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    handleScreens'
  where
    handleScreens' :: PhaseThree Unit
    handleScreens' = do
      screenDefs <- foldM screenDefinition EM.empty (fromFoldable screenEs)
      modifyDF \dfr -> dfr {screens = screenDefs}

    -- `screenDefMap` is the accumulating map of screens.
    -- This function adds the ScreenDefinition that we construct from
    -- the ScreenE to that map.
    screenDefinition :: ScreenMap -> AST.ScreenE -> PhaseThree ScreenMap
    screenDefinition screenDefMap scrn = case scrn of
      s@(AST.ClassicScreen (FreeFormScreenE {subject})) -> do
        -- Add the ScreenDef for each of these roles.
        -- By construction, the subjects are represented with an
        -- RoleIdentification.ExplicitRole data constructor.
        -- This means that a single Enumerated or Calculated role results.
        -- `collectRoles` will throw an error if it fails, so here we are guaranteed
        -- to have a RoleType.
        -- We use the first role type in the list of roles, because, again by construction, we know the screen 
        -- is defined on just one user role type.
        subjectRoleTypes <- collectRoles subject
        toScreen scrn (unsafePartial ARRP.head subjectRoleTypes)
      s@(AST.WWW (WhoWhatWhereScreenE{subject})) -> do
        subjectRoleTypes <- collectRoles subject
        toScreen scrn (unsafePartial ARRP.head subjectRoleTypes)

      where
        toScreen :: AST.ScreenE -> RoleType -> PhaseThree ScreenMap
        toScreen screen subjectRoleType = case screen of
          (AST.ClassicScreen s) -> freeFormScreen' s
          (AST.WWW s) -> whoWhatWhereScreen s

          where
          
            whoWhatWhereScreen :: WhoWhatWhereScreenE -> PhaseThree ScreenMap
            whoWhatWhereScreen (WhoWhatWhereScreenE {who, what, whereTo, context, subject, start, end}) = do
              who' <- traverse tableForm who
              what' <- case what of 
                AST.TableForms tfs -> TableForms <<< fromFoldable <$> traverse tableForm tfs
                AST.FreeFormScreen scrn' -> do 
                  ScreenDefinition {tabs, rows, columns} <- freeFormScreen scrn'
                  pure $ FreeFormScreen {tabs, rows, columns}
              whereTo' <- traverse tableForm whereTo
              chats <- constructChatDefs
              screenDef <- pure $ ScreenDefinition 
                { title: Nothing
                , tabs: Nothing
                , rows: Nothing
                , columns: Nothing
                , whoWhatWhereScreen: Just $ WhoWhatWhereScreenDef 
                  { who: Who {chats, userRoles: fromFoldable who'}
                  , what: what'
                  , whereto: fromFoldable whereTo'}
                }
              pure $ EM.insert (ScreenKey context subjectRoleType) screenDef screenDefMap
              
            tableForm :: AST.TableFormE -> PhaseThree TableFormDef
            tableForm (AST.TableFormE tableE formE) = do
              table' <- table tableFormWidget tableE
              form' <- form tableFormWidget formE
              pure $ TableFormDef {table: table', form: form'}

            freeFormScreen' :: FreeFormScreenE -> PhaseThree ScreenMap
            freeFormScreen' ffs@(FreeFormScreenE{context}) = do
              screenDef <- freeFormScreen ffs
              pure $ EM.insert (ScreenKey context subjectRoleType) screenDef screenDefMap

            -- This is how we get `subjectRoleType` in scope for `widgetCommonFields`.
            freeFormScreen :: FreeFormScreenE -> PhaseThree ScreenDefinition
            freeFormScreen (AST.FreeFormScreenE {title, tabs, rows, columns, context, start, end}) = do
              (tabs' :: Maybe (LIST.List TabDef)) <- case tabs of
                Nothing -> pure Nothing
                Just ts -> Just <$> traverse tab ts
              (rows' :: Maybe (LIST.List ScreenElementDef)) <- case rows of
                Nothing -> pure Nothing
                Just rs -> Just <$> traverse row rs
              columns' <- case columns of
                Nothing -> pure Nothing
                Just cs -> Just <$> traverse column cs
              pure $ ScreenDefinition
                { title
                , tabs: fromFoldable <$> tabs'
                , rows: fromFoldable <$> rows'
                , columns: fromFoldable <$> columns'
                , whoWhatWhereScreen: Nothing
                }

            constructChatDefs :: PhaseThree (Array ChatDef)
            constructChatDefs = lift2 do 
              -- NOTE that we ignore perspectives that the user role's aspects may have!
              -- These have been added in compile time.
              (perspectives :: Array Perspective) <- perspectivesOfRoleType subjectRoleType
              -- Collect all roles the subject role type has a perspective on.
              (objectRoles :: Array RoleType) <- pure $ perspectives <#> \(Perspective{roleTypes}) -> unsafePartial fromJust $ head roleTypes
              -- Filter the roles that are not a specialization of the chatAspect role type.
              chatRoles <- filterA (equalsOrGeneralisesRoleType_ (ENR $ EnumeratedRoleType chatAspect)) objectRoles
              -- For each of these roles, find the properties with the MessageProperty and MediaProperty facets.
              -- Finally construct a ChatDef for each of those roles, where the chatInstance is Nothing.
              catMaybes <$> for chatRoles \chatRole -> do 
                -- Get the properties of the role.
                allProps <- roleADTOfRoleType chatRole >>= allProperties <<< map roleInContext2Role
                -- Filter the properties to find the MessageProperty and MediaProperty.
                messageProperties <- filterA (flip hasFacet MessageProperty) allProps
                mediaProperties <- filterA (flip hasFacet MediaProperty) allProps
                case head messageProperties, head mediaProperties of
                  Just (ENP messageProperty), Just (ENP mediaProperty) -> 
                    -- Construct a ChatDef for this role.
                    pure $ Just $ ChatDef {chatRole, chatInstance: Nothing, messageProperty, mediaProperty}
                  _, _ -> pure Nothing

              

            tab :: AST.TabE -> PhaseThree TabDef
            tab (AST.TabE tabTitle isDefault screenElements) = do
              screenElementDefs <- traverse screenElementDef screenElements
              pure $ TabDef {title: tabTitle, isDefault, elements: (fromFoldable screenElementDefs)}

            row :: AST.RowE -> PhaseThree ScreenElementDef
            row (AST.RowE screenElements) = do
              screenElementDefs <- traverse screenElementDef screenElements
              pure $ RowElementD $ RowDef (fromFoldable screenElementDefs)

            column :: AST.ColumnE -> PhaseThree ScreenElementDef
            column (AST.ColumnE screenElements) = do
              screenElementDefs <- traverse screenElementDef screenElements
              pure $ ColumnElementD $ ColumnDef (fromFoldable screenElementDefs)

            screenElementDef :: AST.ScreenElement -> PhaseThree ScreenElementDef
            screenElementDef (AST.RowElement rowE) = row rowE
            screenElementDef (AST.ColumnElement colE) = column colE
            screenElementDef (AST.TableElement tableE) = TableElementD <$> table relationalWidget tableE
            screenElementDef (AST.FormElement formE) = FormElementD <$> form functionalWidget formE
            screenElementDef (AST.MarkDownElement markdownE) = MarkDownElementD <$> markdown markdownE
            screenElementDef (AST.ChatElement chatE) = ChatElementD <$> chat chatE

            functionalWidget :: ThreeValuedLogic
            functionalWidget = True

            relationalWidget :: ThreeValuedLogic
            relationalWidget = False

            tableFormWidget :: ThreeValuedLogic
            tableFormWidget = Unknown

            table :: ThreeValuedLogic -> AST.TableE -> PhaseThree TableDef
            table cardinality (AST.TableE fields) = TableDef <$> widgetCommonFields fields cardinality

            form :: ThreeValuedLogic -> AST.FormE -> PhaseThree FormDef
            form cardinality (AST.FormE fields) = FormDef <$> widgetCommonFields fields cardinality

            markdown :: AST.MarkDownE -> PhaseThree MarkDownDef
            markdown (MarkDownConstant { text, condition, context:ctxt}) = do
              text' <- unsafePartial case text of 
                Simple (Value _ _ t) -> pure t
              condition' <- traverse (compileStep (CDOM $ ST ctxt)) condition
              pure $ MarkDownConstantDef {text: text', condition: condition', domain: unsafePartial typeUri2ModelUri_ $ unwrap ctxt} 
            markdown (MarkDownPerspective {widgetFields, condition, start:s, end:e}) = do
              case condition of 
                Nothing -> do 
                  widgetFields' <- widgetCommonFields widgetFields Unknown
                  pure $ MarkDownPerspectiveDef {widgetFields: widgetFields', conditionProperty: Nothing}
                Just conditionProp -> do 
                  (objectRoleType :: RoleType) <- unsafePartial ARRP.head <$> collectRoles widgetFields.perspective
                  mconditionProperty <- lift $ lift (objectRoleType ###> (lookForUnqualifiedPropertyType_ conditionProp))
                  case mconditionProperty of 
                    -- The condition property cannot be recognised as a property of the role with the markdown property.
                    Nothing -> throwError (UnknownMarkDownConditionProperty s e conditionProp objectRoleType)
                    Just conditionProperty -> do 
                      -- add the conditionProperty to the widgetFields!
                      widgetFields' <- widgetCommonFields widgetFields Unknown
                      pure $ MarkDownPerspectiveDef {widgetFields: widgetFields' {propertyVerbs = (flip (unsafePartial addProperty) conditionProperty) <$> widgetFields'.propertyVerbs}, conditionProperty: Just conditionProperty}
            markdown (MarkDownExpression {text, condition, context:ctxt, start:start', end:end'}) = do
              text' <- compileStep (CDOM $ ST ctxt) text
              -- The resulting QueryFunctionDescription should be functional.
              if functional text' `eq` True 
                then do
                  condition' <- traverse (compileStep (CDOM $ ST ctxt)) condition
                  pure $ MarkDownExpressionDef {textQuery: text', condition: condition', text: Nothing}
                else throwError (MarkDownExpressionMustBeFunctional start' end')
            
            chat :: ChatE -> PhaseThree ChatDef
            chat (ChatE {chatRole, messagesProperty, mediaProperty, start:start', end:end'}) = do 
              (chatRoleType :: RoleType) <- unsafePartial ARRP.head <$> collectRoles chatRole
              qualifiedMessageProperty <- qualifyProperty chatRoleType messagesProperty
              qualifiedMediaProperty <- qualifyProperty chatRoleType mediaProperty
              pure $ ChatDef {chatRole: chatRoleType, chatInstance: Nothing, messageProperty: qualifiedMessageProperty, mediaProperty: qualifiedMediaProperty}

              where 
              qualifyProperty ::  RoleType -> String -> PhaseThree EnumeratedPropertyType
              qualifyProperty chatRoleType prop = do 
                candidates <- lift2 (chatRoleType ###= lookForUnqualifiedPropertyType_ prop )
                case head candidates of
                  Nothing -> throwError $ UnknownProperty start' prop (roletype2string chatRoleType)
                  (Just t) | length candidates == 1 -> case t of
                    ENP p -> pure p
                    CP p -> throwError $ PropertyCannotBeCalculated prop start' end'
                  otherwise -> throwError $ NotUniquelyIdentifying start' prop (map propertytype2string candidates)


            widgetCommonFields :: AST.WidgetCommonFields -> ThreeValuedLogic -> PhaseThree WidgetCommonFieldsDef
            widgetCommonFields {title:title', perspective, propsOrView, propertyVerbs, roleVerbs, start:start', end:end'} isFunctionalWidget = do
              -- From a RoleIdentification that represents the object,
              -- find the relevant Perspective.
              -- A ScreenElement can only be defined for a named Enumerated or Calculated Role. This means that `perspective` is constructed with the
              -- RoleIdentification.ExplicitRole data constructor: a single RoleType.
              -- If no role can be found for the given specification, collectRoles throws an error.
              (objectRoleType :: RoleType) <- unsafePartial ARRP.head <$> collectRoles perspective
              -- Check the Cardinality
              (lift2 $ roleTypeIsFunctional objectRoleType) >>= if _
                -- object is functional
                then if optimistic isFunctionalWidget 
                  -- we're ok with either True or Unknown (MarkDownPerspectiveDef).
                  then pure unit
                  -- but not with False.
                  else throwError (WidgetCardinalityMismatch start' end')
                -- object is relational
                else if not $ pessimistic isFunctionalWidget
                  -- we're ok with either True or Unknown (MarkDownPerspectiveDef).
                  then pure unit
                  -- but not with False
                  else throwError (WidgetCardinalityMismatch start' end')
              -- All properties defined on this object role.
              allProps <- lift2 ((roleADTOfRoleType objectRoleType >>= allProperties <<< map roleInContext2Role))
              -- The user must have a perspective on it. This perspective must have that RoleType
              -- in its member roleTypes.
              -- So we fetch the user role, get its Perspectives, and find the one that refers to the objectRoleType.
              perspectives <- lift2 $ perspectivesOfRoleType subjectRoleType
              case find (\(Perspective{roleTypes}) -> isJust $ elemIndex objectRoleType roleTypes) perspectives of
                -- This case is probably that the object and user exist, but the latter
                -- has no perspective on the former!
                Nothing -> throwError (UserHasNoPerspective subjectRoleType objectRoleType start' end')
                Just pspve@(Perspective{id:perspectiveId}) -> do
                  if perspectiveSupportsRoleVerbs pspve (maybe [] roleVerbList2Verbs roleVerbs)
                    then pure unit
                    else throwError (UnauthorizedForRole "Auteur" subjectRoleType objectRoleType (maybe [] roleVerbList2Verbs roleVerbs) (Just start') (Just end'))
                  case propsOrView, propertyVerbs of
                    -- The modeller has provided no restrictions.
                    AST.AllProperties, Universal -> pure
                      { title:title'
                      , perspectiveId
                      , perspective: Nothing
                      , propertyVerbs: Nothing
                      , roleVerbs: maybe Nothing (Just <<< roleVerbList2Verbs) roleVerbs
                      , userRole: subjectRoleType
                      }
                    pOrV, pVerbs -> do
                      (propertyTypes :: ExplicitSet PropertyType) <- unsafePartial collectPropertyTypes pOrV perspective start'
                      checkVerbsAndProps allProps propertyTypes (expandVerbs pVerbs) pspve objectRoleType
                      pure
                        { title:title'
                        , perspectiveId
                        , perspective: Nothing
                        , propertyVerbs: Just $ PropertyVerbs propertyTypes pVerbs
                        , roleVerbs: maybe Nothing (Just <<< roleVerbList2Verbs) roleVerbs
                        , userRole: subjectRoleType
                        }
              where 
                checkVerbsAndProps :: Array PropertyType -> ExplicitSet PropertyType -> Array PropertyVerb -> Perspective -> RoleType -> PhaseThree Unit
                checkVerbsAndProps allProps requiredProps propertyVerbs' perspective' objectRoleType = for_ (expandPropSet allProps requiredProps)
                  \requiredProp -> for propertyVerbs' \requiredVerb ->
                    if perspectiveSupportsPropertyForVerb perspective' requiredProp requiredVerb
                      then pure unit
                      else throwError (UnauthorizedForProperty "Auteur" subjectRoleType objectRoleType requiredProp requiredVerb (Just start') (Just end'))

-- | Qualifies incomplete names and changes RoleType constructor to CalculatedRoleType if necessary.
-- | The role type name (parameter `rt`) is always fully qualified, EXCEPT
-- | for the current subject that holds in the body of `perspective of`.
-- | Result contains no double entries.
-- TODO. Nu ook voor perspective on als een enkele identifier is gebruikt!
collectRoles :: RoleIdentification -> PhaseThree (Array RoleType)
-- A single role type will result from this case, but it may be a calculated role!
collectRoles (ExplicitRole ctxt rt pos) = do
  maximallyQualifiedName <- if isTypeUri (roletype2string rt)
    then pure (roletype2string rt)
    else pure $ concatenateSegments (unwrap ctxt) (roletype2string rt)
  r <- qualifyLocalRoleName pos maximallyQualifiedName
  pure [r]
-- Compile the expression s with respect to context ctxt.
-- This case MUST represent the current object that holds in the body of `perspective on`. Multiple Enumerated role types can result from this case.
collectRoles (ImplicitRole ctxt s) = compileExpression (CDOM (UET ctxt)) s >>= \qfd ->
  case range qfd of
    RDOM adt -> pure $ nub $ map ENR (allLeavesInADT $ roleInContext2Role <$> adt)
    otherwise -> throwError $ NotARoleDomain otherwise (startOf s) (endOf s)

-- We lookup the qualified name of these properties here, for the object of the perspective.
-- The (partial) names for properties used here may be defined outside
-- of the model (due to role filling). So we use functions that rely on the
-- model cache and hence we need the current model to be in that cache, too.
-- Hence the Partial constraint.
collectPropertyTypes :: Partial =>
  AST.PropsOrView ->
  RoleIdentification ->
  ArcPosition ->
  PhaseThree (ExplicitSet PropertyType)
collectPropertyTypes AST.AllProperties _ _ = pure Universal
collectPropertyTypes (AST.Properties ps) object start = do
  roleADT <- roleIdentification2rangeADT object
  PSet <$> for (fromFoldable ps)
    \localPropertyName -> do
      candidates <- lift2 (roleADT ###= lookForUnqualifiedPropertyType localPropertyName)
      case head candidates of
        Nothing -> throwError $ UnknownProperty start localPropertyName (show roleADT)
        (Just t) | length candidates == 1 -> pure t
        _ -> throwError $ NotUniquelyIdentifying start localPropertyName (propertytype2string <$> candidates)

collectPropertyTypes (AST.View view) object start = do
  if isTypeUri view
    then do
      mview <- lift2 $ tryGetPerspectType (ViewType view)
      case mview of
        Just (View {propertyReferences}) -> pure $ PSet propertyReferences
        Nothing -> throwError $ UnknownView start view
    else do
      -- If the RoleIdentification is of a single role type (not an expression), add that role
      -- to the types we get from expanding the role specification as an expression.
      -- This causes a calculated role type to be included along with the types of its range.
      -- It does not matter if that added role is still described as Enumerated while it is actually Calculated.
      roles <- map ENR <$> (allLeavesInADT <$> roleIdentification2rangeADT object)
      roles' <- case object of
        ExplicitRole (ContextType ctxt) r pos -> case r of 
          ENR (EnumeratedRoleType er) -> pure $ nub $ cons (ENR $ EnumeratedRoleType (qualifyWith ctxt er)) roles
          CR (CalculatedRoleType er) -> pure $ nub $ cons (CR $ CalculatedRoleType (qualifyWith ctxt er)) roles
        _ -> pure roles
      (views :: Object View) <- getsDF _.views
      -- As we have postponed handling these parse tree fragments after
      -- handling all others, there can be no forward references.
      -- The property references in Views are, by now, qualified.
      case filter (areLastSegmentsOf view) (keys views) of
        noCandidates | null noCandidates -> throwError $ UnknownView start view
        candidates -> case filter (isViewOfObject roles') candidates of
          noCandidates' | null noCandidates' -> throwError $ NotAViewOfObject start view
          candidates' ->
            case length candidates' of
              1 -> unsafePartial case lookup (unsafePartial ARRP.head candidates') views of
                Just (View {propertyReferences}) -> pure $ PSet propertyReferences
              _ -> throwError $ NotUniquelyIdentifying start view candidates'
  where
    isViewOfObject :: Array RoleType -> String -> Boolean
    -- | "Context" `isLocalNameOf` "model:Perspectives$Context"
    isViewOfObject roles viewName = isJust $ findIndex (\rType -> viewName `startsWithSegments` (roletype2string rType)) roles

roleIdentification2rangeADT :: RoleIdentification -> PhaseThree (ADT EnumeratedRoleType)
roleIdentification2rangeADT roleIdentification = map roleInContext2Role <$> unsafePartial domain2roleType <<< range <$> compileStep
  (CDOM (UET (roleIdentification2Context roleIdentification)))
  (roleIdentification2Step roleIdentification)

-- | Returns the current context for the RoleIdentification.
-- | This is, for the lexical position of the current subject or object (for which the
-- | RoleIdentification was constructed), the current context.
roleIdentification2Context :: RoleIdentification -> ContextType
roleIdentification2Context (ExplicitRole ctxt _ _) = ctxt
roleIdentification2Context (ImplicitRole ctxt _) = ctxt

roleIdentification2displayName :: RoleIdentification -> MonadPerspectives (Maybe String)
roleIdentification2displayName (ImplicitRole _ _) = pure Nothing
roleIdentification2displayName (ExplicitRole _ (ENR rt) _) = getEnumeratedRole rt >>= pure <<< Just <<< displayName
roleIdentification2displayName (ExplicitRole _ (CR rt) _) = getCalculatedRole rt >>= pure <<< Just <<< displayName

roleIdentification2TypeName :: RoleIdentification -> (Maybe RoleType)
roleIdentification2TypeName (ImplicitRole _ _) = Nothing
roleIdentification2TypeName (ExplicitRole _ rt@(ENR _) _) = Just rt
roleIdentification2TypeName (ExplicitRole _ rt@(CR _) _) = Just rt

-- | Returns a Step that represents an expression that should be evaluated
-- | with respect to the current context (the compiled function should be applied
-- | to an instance of the current context).
roleIdentification2Step :: RoleIdentification -> Step
roleIdentification2Step (ExplicitRole ctxt (ENR (EnumeratedRoleType rt)) pos) = Simple $ ArcIdentifier pos rt
roleIdentification2Step (ExplicitRole ctxt (CR (CalculatedRoleType rt)) pos) = Simple $ ArcIdentifier pos rt
roleIdentification2Step (ImplicitRole ctxt stp) = stp
