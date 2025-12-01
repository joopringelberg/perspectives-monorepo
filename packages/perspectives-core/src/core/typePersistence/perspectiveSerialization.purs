-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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
-- along with this program.  If not, see <https:--www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.
-- END LICENSE

-- | This module contains functions to create a JSON structure from a Perspective,
-- | that will be used by the client to build a screen automatically.

module Perspectives.TypePersistence.PerspectiveSerialisation where

import Control.Category (identity)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, concat, cons, difference, elemIndex, filter, filterA, findIndex, foldl, head, intersect, modifyAt, nub, null, uncons, union)
import Data.Maybe (Maybe(..), fromJust, isJust, isNothing, maybe)
import Data.Newtype (unwrap)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Foreign.Object (Object, empty, fromFoldable, insert, isEmpty, keys, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), AssumptionTracking, MonadPerspectives, (##>), (##>>))
import Perspectives.Data.EncodableMap (fromFoldable, lookup) as EM
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Extern.Utilities (formatDateTime)
import Perspectives.Identifiers (isExternalRole, qualifyWith)
import Perspectives.Instances.Me (isMe)
import Perspectives.Instances.ObjectGetters (binding, binding_, context, contextType, getActiveRoleStates, getActiveStates, roleType_)
import Perspectives.Instances.Values (parseNumber)
import Perspectives.ModelDependencies (roleWithId)
import Perspectives.ModelTranslation (translateType)
import Perspectives.Parsing.Arc.AST (PropertyFacet(..))
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription(..), domain, domain2roleType, functional, isContextDomain, makeComposition, mandatory, queryFunction, range, roleInContext2Context, roleInContext2Role, roleRange)
import Perspectives.Query.UnsafeCompiler (context2context, context2role, getDynamicPropertyGetter, getPropertyValues, getPublicUrl)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (EnumeratedRoleType, getEnumeratedRole)
import Perspectives.Representation.Class.Property (class PropertyClass, hasFacet, rangeOfPropertyType)
import Perspectives.Representation.Class.Property (getProperty, isCalculated, functional, mandatory, range, Property(..), constrainingFacets) as PROP
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, allProperties, bindingOfADT, perspectivesOfRoleType, roleKindOfRoleType)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..), expandPropSet, expandPropertyVerbs, expandVerbs)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.ScreenDefinition (WidgetCommonFieldsDef, PropertyRestrictions)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..), pessimistic)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), ContextType(..), PropertyType(..), RoleKind(..), RoleType(..), propertytype2string, roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb(..)) as Verbs
import Perspectives.Representation.Verbs (PropertyVerb(..), RoleVerb(..), roleVerbList2Verbs)
import Perspectives.ResourceIdentifiers (createPublicIdentifier, guid)
import Perspectives.TypePersistence.PerspectiveSerialisation.Data (PropertyFacets, RoleInstanceWithProperties, SerialisedPerspective(..), SerialisedPerspective', SerialisedProperty, ValuesWithVerbs)
import Perspectives.Types.ObjectGetters (getContextAspectSpecialisations)
import Perspectives.Utilities (findM)
import Prelude (append, bind, discard, eq, flip, map, not, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=), (||))
import Simple.JSON (writeJSON)

perspectiveForContextAndUser
  :: RoleInstance
  -> -- The user role instance
  RoleType
  -> -- The user role type
  RoleType
  -> -- An object role type, will be matched against the Perspective's roleTypes member.
  (ContextInstance ~~> SerialisedPerspective)
perspectiveForContextAndUser subject userRoleType objectRoleType = perspectiveForContextAndUser' subject userRoleType objectRoleType >=> pure <<< SerialisedPerspective <<< writeJSON

-- | Get the serialisation of the perspective the user role type has on the object role type,
-- | in a given context instance.
perspectiveForContextAndUser'
  :: RoleInstance
  -> -- The user role instance
  RoleType
  -> -- The user role type
  RoleType
  -> -- An object role type, will be matched against the Perspective's roleTypes member.
  (ContextInstance ~~> SerialisedPerspective')
perspectiveForContextAndUser' subject userRoleType objectRoleType cid = ArrayT do
  contextStates <- map ContextState <$> (runArrayT $ getActiveStates cid)
  subjectStates <- map SubjectState <$> (runArrayT $ getActiveRoleStates subject)
  allPerspectives <- lift $ perspectivesOfRoleType userRoleType
  traverse
    ((serialisePerspective contextStates subjectStates cid userRoleType Nothing Nothing Nothing))
    ( filter
        (isJust <<< elemIndex objectRoleType <<< _.roleTypes <<< unwrap)
        allPerspectives
    )

-- | Get the serialisation of the perspective the user role type has on the object role type,
-- | in a given context instance.
perspectiveForContextAndUserFromId
  :: RoleInstance
  -> -- The user role instance
  WidgetCommonFieldsDef
  -> ContextInstance
  -> AssumptionTracking SerialisedPerspective'
perspectiveForContextAndUserFromId subject { perspectiveId, propertyRestrictions, withoutProperties, roleVerbs, userRole } cid = do
  contextStates <- map ContextState <$> (runArrayT $ getActiveStates cid)
  subjectStates <- map SubjectState <$> (runArrayT $ getActiveRoleStates subject)
  allPerspectives <- lift $ perspectivesOfRoleType userRole
  perspective <- pure $ unsafePartial fromJust $ head
    ( filter
        (\(Perspective { id }) -> id == perspectiveId)
        allPerspectives
    )
  serialisePerspective contextStates subjectStates cid userRole propertyRestrictions withoutProperties roleVerbs perspective

perspectivesForContextAndUser :: RoleInstance -> RoleType -> (ContextInstance ~~> SerialisedPerspective)
perspectivesForContextAndUser subject userRoleType cid = ArrayT do
  perspectives <- runArrayT $ perspectivesForContextAndUser' subject userRoleType cid
  pure $ (SerialisedPerspective <<< writeJSON) <$> perspectives

perspectivesForContextAndUser' :: RoleInstance -> RoleType -> (ContextInstance ~~> SerialisedPerspective')
perspectivesForContextAndUser' subject userRoleType cid = ArrayT do
  contextStates <- map ContextState <$> (runArrayT $ getActiveStates cid)
  subjectStates <- map SubjectState <$> (runArrayT $ getActiveRoleStates subject)
  -- NOTE that we ignore perspectives that the user role's aspects may have!
  -- These have been added in compile time.
  perspectives <- lift $ perspectivesOfRoleType userRoleType
  (traverse (serialisePerspective contextStates subjectStates cid userRoleType Nothing Nothing Nothing) perspectives) >>=
    (filterA sendToClient)
  where
  sendToClient :: SerialisedPerspective' -> AssumptionTracking Boolean
  sendToClient { verbs, roleInstances, properties } = pure $
    if isEmpty roleInstances then (isJust $ elemIndex (show Create) verbs) || (isJust $ elemIndex (show CreateAndFill) verbs)
    else (not $ isEmpty properties) || (not $ isEmpty roleInstances)

settingsPerspective :: RoleInstance -> RoleType -> RoleType -> (ContextInstance ~~> SerialisedPerspective)
settingsPerspective subject userRoleType objectRoleType cid = ArrayT do
  contextStates <- map ContextState <$> (runArrayT $ getActiveStates cid)
  subjectStates <- map SubjectState <$> (runArrayT $ getActiveRoleStates subject)
  allPerspectives <- lift $ perspectivesOfRoleType userRoleType
  traverse
    -- Restrict the properties to those that have the facet SettingProperty.
    ( ( \p@(Perspective { object }) -> do
          (allProps :: Array PropertyType) <- lift $ allProperties (roleInContext2Role <$> (unsafePartial domain2roleType $ range object))
          settingsProperties <- lift $ filterA (flip hasFacet SettingProperty) allProps
          propertyRestrictions <- pure $ EM.fromFoldable (flip Tuple [ Consult, Verbs.SetPropertyValue ] <$> settingsProperties)
          serialisePerspective contextStates subjectStates cid userRoleType (Just propertyRestrictions) (Just $ difference allProps settingsProperties) Nothing p
      )
        >=> pure <<< SerialisedPerspective <<< writeJSON
    )
    ( filter
        (isJust <<< elemIndex objectRoleType <<< _.roleTypes <<< unwrap)
        allPerspectives
    )

serialisePerspective
  :: Array StateSpec
  -> Array StateSpec
  -> ContextInstance
  -> RoleType
  -> Maybe PropertyRestrictions
  -> Maybe (Array PropertyType)
  -> Maybe (Array RoleVerb)
  -> Perspective
  -> AssumptionTracking SerialisedPerspective'
serialisePerspective contextStates subjectStates cid userRoleType propertyRestrictions withoutProperties roleVerbs' p@(Perspective { id, object, isEnumerated, roleTypes, roleVerbs, propertyVerbs, actions }) = do
  -- All properties available on the object of the perspective.
  (allProps :: Array PropertyType) <- lift $ allProperties (roleInContext2Role <$> (unsafePartial domain2roleType $ range object))
  -- All PropertyVerbs (PropertyVerbs (ExplicitSet PropertyType) (ExplicitSet PropertyVerb) available on the object of the perspective, given context- and subject state.
  (availablePropertyVerbs :: Array PropertyVerbs) <- pure $ concat (catMaybes $ (flip EM.lookup propertyVerbs) <$> (contextStates <> subjectStates))
  -- All PropertyTypes available for the object of the perspective, given context- and subject state.
  -- Restrict with the given PropertyVerbs.
  -- Includes the readableNameProperties, if available.
  (availableProperties' :: Array PropertyType) <- do
    bruto <- pure $ nub (concat $ expandPropSet allProps <<< (\(PropertyVerbs props _) -> props) <$> availablePropertyVerbs)
    pure $ difference bruto (maybe [] identity withoutProperties)
  -- pure $ nub $ difference (concat $ expandPropSet allProps <<< (\(PropertyVerbs props _) -> props) <$> availablePropertyVerbs) (maybe [] identity withoutProperties)
  mreadableNameProperty <- computeReadableNameProperty availableProperties' allProps
  -- Either the property available on the type with facet ReadableNameProperty, or the Id property (the raw identifier of the role) as a last resort.
  (identifyingProperty :: PropertyType) <- case mreadableNameProperty of
    Nothing -> pure $ CP $ CalculatedPropertyType roleWithId
    Just readableNameProperty -> pure $ readableNameProperty
  -- Add the identifyingProperty and make sure that it is only once in the list
  availableProperties <-
    if isJust $ elemIndex identifyingProperty availableProperties' then pure availableProperties'
    else pure $ cons identifyingProperty availableProperties'
  -- Role instances with their property values.
  roleInstances <- roleInstancesWithProperties
    allProps
    -- availableProperties now includes the identifyingProperty...
    availableProperties
    -- ... so make sure that verbsPerProperty contains an index for it, too.
    (ensureIdentifyingProperty identifyingProperty (verbsPerProperty availablePropertyVerbs allProps))
    cid
    p
    propertyRestrictions
    (maybe [] identity withoutProperties)
    mreadableNameProperty
  -- Additional properties available on instances given object state.
  additionalPropertiesOnInstances <- pure $ foldl union [] (propertiesInInstance <$> roleInstances)
  -- If no properties are available, we'd like to add roleWithId as property. Otherwise, no table can be built.
  serialisedProps <- lift $ traverse makeSerialisedProperty (availableProperties <> additionalPropertiesOnInstances)
  roleKind <- lift $ traverse roleKindOfRoleType (head roleTypes)
  -- If the binding of the ADT that is the range of the object QueryFunctionDescription, is an external role,
  -- its context type may be created.
  (contextTypesToCreate :: Object String) <-
    ( lift $ maybe [] (allLeavesInADT <<< map roleInContext2Role) <$>
        (bindingOfADT $ unsafePartial domain2roleType (range object))
    )
      >>= pure <<< (filter (isExternalRole <<< unwrap))
      >>= lift <<< traverse getEnumeratedRole
      >>= pure <<< map (_.context <<< unwrap)
      >>= \as -> lift $ ((append as) <<< concat <$> (for as (runArrayT <<< getContextAspectSpecialisations)))
        >>= (\as' -> pure $ nub as')
        >>= (traverse \(ContextType cType) -> Tuple cType <$> translateType cType)
        >>= pure <<< fromFoldable
  cType <- lift (cid ##>> contextType)
  contextIdToAddRoleInstanceTo <- (unsafePartial computeContextFromPerspectiveObject object) >>= (lift <<< context2context) >>= \f -> lift (cid ##> f)
  -- NOTE: there can be more than one roleType.
  (roleType :: String) <- pure (roletype2string <$> unsafePartial fromJust $ head roleTypes)
  (displayName :: String) <- lift $ translateType roleType
  translatedActions <- lift
    ( fromFoldable <$> for (nub $ concat (keys <$> (catMaybes $ (flip EM.lookup actions) <$> (contextStates <> subjectStates))))
        \actionName -> do
          translatedActionName <- translateType actionName
          pure $ Tuple actionName translatedActionName
    )
  pure
    { id
    , displayName
    , isFunctional: pessimistic $ functional object
    , isMandatory: pessimistic $ mandatory object
    , isCalculated: not isEnumerated
    , userRoleType: (roletype2string userRoleType)
    , roleType: Just roleType
    , contextInstance: cid
    , roleKind
    , verbs: show <$> case roleVerbs' of
        -- No restrictions
        Nothing -> concat (roleVerbList2Verbs <$> (catMaybes $ (flip EM.lookup roleVerbs) <$> (contextStates <> subjectStates)))
        -- else combine the verbs from all active states.
        Just verbs -> (intersect verbs $ concat (roleVerbList2Verbs <$> (catMaybes $ (flip EM.lookup roleVerbs) <$> (contextStates <> subjectStates))))
    , properties: fromFoldable ((\r@({ id: propId }) -> Tuple propId r) <$> serialisedProps)
    , actions: translatedActions
    , roleInstances: fromFoldable ((\r@({ roleId }) -> Tuple roleId r) <$> roleInstances)
    , contextTypesToCreate
    , contextType: cType
    , contextIdToAddRoleInstanceTo
    , identifyingProperty: propertytype2string identifyingProperty
    }
  where

  -- The perspective object is by construction a role instance. However, there need not (yet) be one.
  -- We must adapt the object so it becomes a query to construct the context we can create in.
  -- A role can be reached from another role through the filled or filler operation, or from its context.
  -- In the first two cases we have to take the context of the object, so we have to add the `context` step;
  -- in the last case we have to remove the last step.
  computeContextFromPerspectiveObject :: Partial => QueryFunctionDescription -> AssumptionTracking QueryFunctionDescription
  computeContextFromPerspectiveObject qfd = case unsnoc qfd of
    Just { query, lastStep } -> case queryFunction lastStep of
      DataTypeGetterWithParameter FilledByF _ -> pure $ makeComposition qfd (SQD (range qfd) (DataTypeGetter ContextF) (CDOM (roleInContext2Context <$> unsafePartial roleRange qfd)) True True)
      -- If the last step is to get role instances from a context, we have to remove that step. The previous step ends with the context we want.
      DataTypeGetterWithParameter GetRoleInstancesForContextFromDatabaseF _ -> pure query
      DataTypeGetter FillerF -> pure $ makeComposition qfd (SQD (range qfd) (DataTypeGetter ContextF) (CDOM (roleInContext2Context <$> unsafePartial roleRange qfd)) True True)
      DataTypeGetterWithParameter FillerF _ -> pure $ makeComposition qfd (SQD (range qfd) (DataTypeGetter ContextF) (CDOM (roleInContext2Context <$> unsafePartial roleRange qfd)) True True)
      FilledF _ ctxt -> pure $ makeComposition qfd (SQD (range qfd) (DataTypeGetter ContextF) (CDOM $ UET ctxt) True True)
      RolGetter _ -> pure query
    Nothing ->
      if isContextDomain (domain qfd) then pure $ SQD (domain qfd) (DataTypeGetter IdentityF) (domain qfd) True True
      else throwError $ error "Programming error: Cannot compute a context domain from this perspective object."

  unsnoc :: QueryFunctionDescription -> Maybe { query :: QueryFunctionDescription, lastStep :: QueryFunctionDescription }
  unsnoc (BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 _ _ _) | queryFunction qfd2 == (BinaryCombinator ComposeF) =
    case unsnoc qfd2 of
      Just { query, lastStep } -> Just { query: makeComposition qfd1 query, lastStep }
      Nothing -> Nothing
  unsnoc (BQD _ _ qfd1 qfd2 _ _ _) = Just { query: qfd1, lastStep: qfd2 }
  unsnoc _ = Nothing

  ensureIdentifyingProperty :: PropertyType -> Object (Array PropertyVerb) -> Object (Array PropertyVerb)
  ensureIdentifyingProperty identifyingProperty obj = case lookup (propertytype2string identifyingProperty) obj of
    Nothing -> insert (propertytype2string identifyingProperty) [ Consult ] obj
    Just _ -> obj

  computeReadableNameProperty :: Array PropertyType -> Array PropertyType -> AssumptionTracking (Maybe PropertyType)
  computeReadableNameProperty availableProps allProps = do
    readableNameProperties <- lift $ filterA (flip hasFacet ReadableNameProperty) allProps
    case head $ intersect readableNameProperties availableProps of
      -- In effect add a new property to those available.
      Nothing -> pure $ head readableNameProperties
      -- There already was a readableNameProperty available.
      Just readableNameProperty -> pure $ Just readableNameProperty

  maybeAddPropertyVerbs :: Array PropertyVerbs -> Array PropertyVerbs
  maybeAddPropertyVerbs pverbs = if null pverbs then [ PropertyVerbs (PSet [ CP $ CalculatedPropertyType roleWithId ]) (PSet [ Consult ]) ] else pverbs

  nameRegex :: Regex
  nameRegex = unsafeRegex "Name" noFlags

  propertiesInInstance :: RoleInstanceWithProperties -> Array PropertyType
  propertiesInInstance = _.objectStateBasedProperties

  propNames :: RoleInstanceWithProperties -> Array String
  propNames { objectStateBasedProperties } = propertytype2string <$> objectStateBasedProperties

-- Construct a map from PropertyType to Array PropertyVerb.
verbsPerProperty :: Array PropertyVerbs -> Array PropertyType -> Object (Array PropertyVerb)
verbsPerProperty pvArr allProps = verbsPerProperty' pvArr allProps empty

verbsPerProperty' :: Array PropertyVerbs -> Array PropertyType -> Object (Array PropertyVerb) -> Object (Array PropertyVerb)
verbsPerProperty' pvArr allProps verbsPerPropMap = foldl
  ( \vPerPropMap (PropertyVerbs props verbs) ->
      let
        (theseProps :: Array PropertyType) = expandPropSet allProps props
        (theseVerbs :: Array PropertyVerb) = expandVerbs verbs
      in
        foldl
          ( \theMap property ->
              let
                prop = propertytype2string property
              in
                case lookup prop theMap of
                  Nothing -> insert prop theseVerbs theMap
                  Just verbs' -> insert prop (verbs' `union` theseVerbs) theMap
          )
          vPerPropMap
          theseProps
  )
  verbsPerPropMap
  pvArr

-- | Returns all properties available on the roles in the range of the query function description
-- | (the perspective's object).
serialiseProperties :: QueryFunctionDescription -> Array PropertyVerbs -> MonadPerspectives (Array SerialisedProperty)
serialiseProperties object pverbs = do
  allProps <- allProperties (roleInContext2Role <$> (unsafePartial domain2roleType $ range object))
  (x :: Array (Tuple PropertyType (Array PropertyVerb))) <- pure $ concat (expandPropertyVerbs allProps <$> pverbs)
  (y :: Array (Tuple PropertyType (Array PropertyVerb))) <- pure $ case uncons x of
    Just { head, tail } -> foldl add [ head ] tail
    Nothing -> []
  traverse (\(Tuple pt verbs) -> makeSerialisedProperty pt) y
  where
  -- Replace two Tuples with the same PropertyType with a single Tuple, with the union of their PropertyVerbs.
  add
    :: Array (Tuple PropertyType (Array PropertyVerb))
    -> (Tuple PropertyType (Array PropertyVerb))
    -> Array (Tuple PropertyType (Array PropertyVerb))
  add cumulator n@(Tuple prop verbs) = case findIndex (eq prop <<< fst) cumulator of
    Nothing -> cons n cumulator
    Just i -> unsafePartial fromJust $ modifyAt i (\(Tuple _ vs) -> (Tuple prop (union vs verbs))) cumulator

makeSerialisedProperty :: PropertyType -> MonadPerspectives SerialisedProperty
makeSerialisedProperty pt = do
  propType <- PROP.getProperty pt
  case propType of
    (PROP.E prop) -> makeSerialisedProperty' prop
    (PROP.C prop) -> makeSerialisedProperty' prop
  where
  makeSerialisedProperty' :: forall r i. PropertyClass r i => r -> MonadPerspectives SerialisedProperty
  makeSerialisedProperty' propType = do
    isFunctional <- PROP.functional propType
    isMandatory <- PROP.mandatory propType
    isCalculated <- PROP.isCalculated propType
    range <- PROP.range propType
    displayName <- translateType (propertytype2string pt)
    pure
      { id: unwrap $ identifier propType
      , displayName
      , isFunctional
      , isMandatory
      , isCalculated
      , range: show range
      , constrainingFacets: serialisePropertyFacets $ PROP.constrainingFacets propType
      }

  serialisePropertyFacets :: Array PropertyFacet -> PropertyFacets
  serialisePropertyFacets facets = foldl
    ( \pr facet -> case facet of
        MinLength x -> pr { minLength = Just x }
        MaxLength x -> pr { maxLength = Just x }
        Pattern s label -> pr { pattern = Just $ { regex: show s, label } }
        WhiteSpace r -> pr { whiteSpace = Just $ show r }
        Enumeration items -> pr { enumeration = Just items }
        MaxInclusive s -> pr { maxInclusive = Just s }
        MinInclusive s -> pr { minInclusive = Just s }
        MaxExclusive s -> pr { maxExclusive = Just s }
        MinExclusive s -> pr { minExclusive = Just s }
        TotalDigits i -> pr { totalDigits = Just i }
        FractionDigits i -> pr { fractionDigits = Just i }
        MessageProperty -> pr { isMessageProperty = true }
        MediaProperty -> pr { isMediaProperty = true }
        ReadableNameProperty -> pr { isReadableNameProperty = true }
        SettingProperty -> pr { isSettingProperty = true }

    )
    { minLength: Nothing
    , maxLength: Nothing
    , pattern: Nothing
    , whiteSpace: Nothing
    , enumeration: Nothing
    , maxInclusive: Nothing
    , maxExclusive: Nothing
    , minInclusive: Nothing
    , minExclusive: Nothing
    , totalDigits: Nothing
    , fractionDigits: Nothing
    , isMessageProperty: false
    , isMediaProperty: false
    , isReadableNameProperty: false
    , isSettingProperty: false
    }
    facets

-----------------------------------------------------------------------------------------
-- INSTANCES
-----------------------------------------------------------------------------------------
-- | A user may have a perspective on an object role that depends on object state.
roleInstancesWithProperties
  :: Array PropertyType
  -> Array PropertyType
  ->
  -- PropertyVerbs that are applicable given the context- and subject state.
  Object (Array PropertyVerb)
  -> ContextInstance
  -> Perspective
  -> Maybe PropertyRestrictions
  -> Array PropertyType
  -> Maybe PropertyType
  -> AssumptionTracking (Array RoleInstanceWithProperties)
roleInstancesWithProperties allProps contextSubjectStateBasedProps subjectContextStateBasedPropertyVerbs cid (Perspective { object, roleVerbs, propertyVerbs, actions }) propertyRestrictions withoutProperties mreadableNameProperty = do
  (roleGetter :: ContextInstance ~~> RoleInstance) <- lift $ context2role object
  -- These are all instances of the object of the perspective, regardless of their state.
  (roleInstances :: Array RoleInstance) <- runArrayT $ roleGetter cid
  -- Property getters for all properties available given context- and subject state.
  (propertyGetters :: Object Getter) <- fromFoldable <$> for contextSubjectStateBasedProps
    ( \propertyType -> do
        getter <- lift $ getDynamicPropertyGetter (propertytype2string propertyType) (roleInContext2Role <$> unsafePartial roleRange object)
        pure $ Tuple (propertytype2string propertyType) getter
    )
  -- If an instance has no properties at all, we should hide the instance.
  evalStateT
    (catMaybes <$> for roleInstances roleInstanceWithProperties)
    propertyGetters

  where
  roleInstanceWithProperties
    :: RoleInstance
    -> StateT (Object Getter) AssumptionTracking (Maybe RoleInstanceWithProperties)
  roleInstanceWithProperties roleId = do
    (roleStates :: Array StateSpec) <- lift $ map ObjectState <$> (runArrayT $ getActiveRoleStates roleId)
    -- PropertyVerbs based on the states of the RoleInstance.
    (objectStateBasedPropertyVerbs :: Array PropertyVerbs) <- pure $ (concat (catMaybes $ (flip EM.lookup propertyVerbs) <$> roleStates))
    -- RoleVerbs based on the states of the RoleInstance.
    objectStateBasedRoleVerbs <- pure $ show <$> concat (roleVerbList2Verbs <$> (catMaybes $ (flip EM.lookup roleVerbs) <$> roleStates))
    -- PropertyTypes based on the states of the RoleInstance.
    (objectStateBasedProperties :: Array PropertyType) <- pure $ difference (nub $ concat $ expandPropSet allProps <<< (\(PropertyVerbs props _) -> props) <$> objectStateBasedPropertyVerbs) withoutProperties
    -- Add getters that we not yet have
    void $ for objectStateBasedProperties
      ( \propertyType -> do
          getters <- get
          if isNothing (lookup (propertytype2string propertyType) getters) then do
            getter <- lift $ lift $ getDynamicPropertyGetter
              (propertytype2string propertyType)
              (roleInContext2Role <$> (unsafePartial roleRange object))
            put (insert (propertytype2string propertyType) getter getters)
          else pure unit
      )
    -- Get all getters from state.
    (allGetters :: Object Getter) <- get
    -- Add verbs based on state of the role instance to the map of
    -- property to verbs based on context- and subject state.
    (localVerbsPerProperty :: Object (Array PropertyVerb)) <- pure $ verbsPerProperty' objectStateBasedPropertyVerbs allProps subjectContextStateBasedPropertyVerbs
    -- Apply for each property available based on context, subject or object
    -- state, the getter to the role instance.
    -- The first Tuple member is the string representation of the property id.
    (valuesAndVerbs :: Array (Tuple String ValuesWithVerbs)) <- for (objectStateBasedProperties <> contextSubjectStateBasedProps)
      \propertyType -> do
        getter <- pure $ unsafePartial fromJust $ lookup (propertytype2string propertyType) allGetters
        vals <- lift (runArrayT $ getter roleId)
        pure $ Tuple (propertytype2string propertyType)
          { values: unwrap <$> vals
          , propertyVerbs: show <$> difference (unsafePartial fromJust $ lookup (propertytype2string propertyType) localVerbsPerProperty) (maybe [] (maybe [] identity <<< (EM.lookup propertyType)) propertyRestrictions)
          }
    kind <- lift $ lift (roleType_ roleId >>= roleKindOfRoleType <<< ENR)
    filler <- lift $ lift $ binding_ roleId
    isMe <- lift $ lift $ isMe roleId
    publicUrl <- case kind of
      -- Only report a result for ContextRole kinds. 
      ContextRole ->
        ( do
            meRole <- lift $ lift $ binding_ roleId
            case meRole of
              Nothing -> pure Nothing
              -- We'll look for public roles in the (type of the) context of the filler of the ContextRole instance.
              Just erole -> do
                ctxt <- lift $ lift (erole ##>> context)
                murl <- lift $ lift $ getPublicUrl ctxt
                case murl of
                  Nothing -> pure Nothing
                  Just url -> do
                    schemeLess <- lift $ lift $ guid (unwrap erole)
                    pure $ Just (createPublicIdentifier url schemeLess)
        )
      _ -> pure Nothing
    cType <- lift $ lift (cid ##>> contextType)
    translatedActions <- lift $ lift
      ( fromFoldable <$> for (nub $ concat (keys <$> (catMaybes $ (flip EM.lookup actions) <$> roleStates)))
          \actionName -> do
            translatedActionName <- translateType (qualifyWith (unwrap cType) actionName)
            pure $ Tuple actionName translatedActionName
      )
    readableName <- computeReadableName valuesAndVerbs
    pure $ Just
      { roleId: (unwrap roleId)
      , objectStateBasedRoleVerbs
      , propertyValues: fromFoldable valuesAndVerbs
      , actions: translatedActions
      , objectStateBasedProperties
      , filler
      , isMe
      , publicUrl
      , readableName
      }

    where
    computeReadableName :: Array (Tuple String ValuesWithVerbs) -> StateT (Object Getter) AssumptionTracking String
    computeReadableName valuesAndVerbs = case mreadableNameProperty of
      Just readableNameProperty -> case head $ filter (eq (propertytype2string readableNameProperty) <<< fst) valuesAndVerbs of
        Just (Tuple _ { values }) -> case head values of
          Just v -> pure v
          -- The name giving property doesn't yield a value. Fall back on the id (an alternative would be to compute a name on the filler).
          Nothing -> pure $ unwrap roleId
        -- This case should not arise. Compute the id.
        Nothing -> pure $ unwrap roleId
      Nothing -> do
        -- The readableNameProperty is not available. We have to look for a property with the ReadableNameProperty facet on the filler.
        bnds <- lift $ runArrayT $ binding roleId
        case head bnds of
          Nothing -> pure $ unwrap roleId
          Just bnd -> do
            bndType <- lift $ lift $ roleType_ bnd
            lift $ getReadableNameFromTelescope (flip hasFacet ReadableNameProperty) (ST bndType) bnd

getReadableNameFromTelescope :: (PropertyType -> MonadPerspectives Boolean) -> ADT EnumeratedRoleType -> RoleInstance -> AssumptionTracking String
getReadableNameFromTelescope predicate adt rid = do
  allLocalProps <- lift $ allLocallyRepresentedProperties adt
  mnameGiver <- lift $ findM predicate allLocalProps
  case mnameGiver of
    Just pt -> do
      values <- runArrayT $ getPropertyValues pt rid
      case head values of
        Nothing -> pure $ unwrap rid
        Just (Value v) -> do
          r <- lift $ rangeOfPropertyType pt
          case r of
            PDateTime -> do
              epoch <- parseNumber v
              liftEffect $ formatDateTime epoch "nl-NL" "{\"dateStyle\": \"short\", \"timeStyle\": \"short\"}"
            PDate -> do
              epoch <- parseNumber v
              liftEffect $ formatDateTime epoch "nl-NL" "{\"dateStyle\": \"short\"}"
            PTime -> do
              epoch <- parseNumber v
              liftEffect $ formatDateTime epoch "nl-NL" "{\"timeStyle\": \"short\"}"
            _ -> pure v
    Nothing -> do
      bnds <- runArrayT $ binding rid
      case head bnds of
        Nothing -> pure $ unwrap rid
        Just bnd -> do
          bndType <- lift $ roleType_ bnd
          getReadableNameFromTelescope predicate (ST bndType) bnd

-- | The verbs in this type contain both those based on context- and subject state,
-- | and those based on object state.

type PropertyGetter =
  { propertyName :: String -- property id
  , getter :: Getter
  }

type Getter = RoleInstance ~~> Value
