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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE
module Perspectives.Sidecar.NormalizeTypeNames
  ( StableIdMappingForModel
  , WithSideCars
  , class NormalizeTypeNames
  , fqn2tid
  , getinstalledModelCuids
  , normalizeTypeNames
  , normalizeTypes
  ) where

import Prelude

import Control.Monad.Reader (Reader, ask, runReaderT)
import Data.Array (catMaybes, foldM)
import Data.Map (Map, fromFoldable, lookup, insert, singleton) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (fromFoldable, toUnfoldable)
import Foreign.Object as OBJ
import Perspectives.CoreTypes (MonadPerspectives, (##=), (##>))
import Perspectives.Data.EncodableMap (EncodableMap, toUnfoldable, fromFoldable) as EM
import Perspectives.DomeinFile (DomeinFile(..), SeparateInvertedQuery(..), UpstreamAutomaticEffect(..), UpstreamStateNotification(..))
import Perspectives.Identifiers (splitTypeUri)
import Perspectives.Instances.ObjectGetters (binding)
import Perspectives.InvertedQuery (InvertedQuery)
import Perspectives.ModelDependencies (modelURI, modelURIReadable, modelsInUse, versionedModelURI)
import Perspectives.Names (getMySystem)
import Perspectives.Parsing.Arc.PhaseTwoDefs (toStableDomeinFile)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..), traverseQfd)
import Perspectives.Query.UnsafeCompiler (getPropertyValues, getRoleInstances)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.Action (Action, AutomaticAction(..))
import Perspectives.Representation.CNF (traverseDPROD)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Property (Property(..)) as Prop
import Perspectives.Representation.Class.Role (Role(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), InvertedQueryKey(..))
import Perspectives.Representation.Perspective (Perspective(..), StateSpec, stateSpec2StateIdentifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), Value(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.ScreenDefinition (ChatDef(..), ColumnDef(..), FormDef(..), MarkDownDef(..), RowDef(..), ScreenDefinition(..), ScreenElementDef(..), ScreenKey(..), TabDef(..), TableDef(..), TableFormDef(..), What(..), WhereTo(..), Who(..), WhoWhatWhereScreenDef(..), WidgetCommonFieldsDef)
import Perspectives.Representation.Sentence (Sentence(..))
import Perspectives.Representation.State (Notification(..), State(..), StateFulObject(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), StateIdentifier(..), ViewType(..))
import Perspectives.Representation.Verbs (PropertyVerb)
import Perspectives.Representation.View (View(..))
import Perspectives.Sidecar.StableIdMapping (ActionUri(..), ContextUri(..), ModelUri(..), PropertyUri(..), Readable, RoleUri(..), Stable, StableIdMapping, StateUri(..), ViewUri(..), idUriForAction, idUriForContext, idUriForProperty, idUriForRole, idUriForState, idUriForView, loadStableMapping)

normalizeTypes :: DomeinFile Readable -> StableIdMapping -> MonadPerspectives (DomeinFile Stable)
normalizeTypes df@(DomeinFile { namespace, referredModels }) mapping = do
  -- Notice that referredModels from a freshly parsed Arc source will be FQNs with names given by the modeller, rather than underlying cuids.
  cuidMap <- getinstalledModelCuids true -- get versioned cuids because we will read from the repository.
  sidecars <- foldM
    ( \scs (ModelUri referredModel) -> case Map.lookup (ModelUri referredModel) cuidMap of
        Nothing -> pure scs
        Just (domeinFileName :: ModelUri Stable) -> do
          mmapping <- loadStableMapping domeinFileName
          case mmapping of
            Nothing -> pure scs
            Just submapping -> pure $ Map.insert (ModelUri referredModel) submapping scs
    )
    (Map.singleton namespace mapping)
    referredModels
  pure $ toStableDomeinFile $ unwrap $ runReaderT
    (normalizeTypeNames df)
    sidecars

type StableIdMappingForModel = (Map.Map (ModelUri Readable) (ModelUri Stable))

-- A map from ModelUri Readable to ModelUri Stable.
-- Not every installed model need have cuid!
-- The Stable ModelUri will be versioned.
getinstalledModelCuids :: Boolean -> MonadPerspectives StableIdMappingForModel
getinstalledModelCuids versioned = do
  let modelProp = if versioned then versionedModelURI else modelURI
  system <- getMySystem
  -- Instances of ModelInUse are filled with instances of sys:VersionedModelManifest
  modelRoles <- (ContextInstance system) ##= getRoleInstances (ENR $ EnumeratedRoleType modelsInUse)
  x :: Array (Maybe (Tuple (ModelUri Readable) (ModelUri Stable))) <- for modelRoles \ri -> do
    mreadableModelUri <- ri ##> binding >=> getPropertyValues (CP $ CalculatedPropertyType modelURIReadable)
    case mreadableModelUri of
      Nothing -> pure Nothing
      Just (Value readableModelUri) -> do
        mstableModelUri <- ri ##> binding >=> getPropertyValues (CP $ CalculatedPropertyType modelProp)
        case mstableModelUri of
          Nothing -> pure Nothing
          -- The Stable name.
          Just (Value stableModelUri) -> pure $ Just $ Tuple (ModelUri readableModelUri) (ModelUri stableModelUri)
  pure $ Map.fromFoldable $ catMaybes x

-- | This monad supports 'ask'. `ask` will return an object whose keys are Model Ids (MIDs).
type WithSideCars = Reader (Map.Map (ModelUri Readable) StableIdMapping)

class NormalizeTypeNames v ident | v -> ident, ident -> v where
  -- | Replace user readable local names given in the model text by Cuids in the given object.
  normalizeTypeNames :: v -> WithSideCars v
  -- Dit zou van Readable naar Stable moeten zijn.
  fqn2tid :: ident -> WithSideCars ident

instance NormalizeTypeNames (DomeinFile Readable) (ModelUri Readable) where
  fqn2tid df = ModelUri <<< unwrap <$> fqn2tid (ContextType $ unwrap df)
  normalizeTypeNames (DomeinFile df) = do
    contexts' <- fromFoldable <$> for ((toUnfoldable df.contexts) :: Array (Tuple String Context))
      (\(Tuple ct ctxt) -> Tuple <$> unwrap <$> (fqn2tid <<< ContextType) ct <*> normalizeTypeNames ctxt)
    enumeratedRoles' <- fromFoldable <$> for ((toUnfoldable df.enumeratedRoles) :: Array (Tuple String EnumeratedRole))
      (\(Tuple ct rle) -> Tuple <$> unwrap <$> (fqn2tid <<< EnumeratedRoleType) ct <*> normalizeTypeNames rle)
    calculatedRoles' <- fromFoldable <$> for ((toUnfoldable df.calculatedRoles) :: Array (Tuple String CalculatedRole))
      (\(Tuple ct rle) -> Tuple <$> unwrap <$> (fqn2tid <<< CalculatedRoleType) ct <*> normalizeTypeNames rle)
    enumeratedProperties' <- fromFoldable <$> for ((toUnfoldable df.enumeratedProperties) :: Array (Tuple String EnumeratedProperty))
      (\(Tuple ct rle) -> Tuple <$> unwrap <$> (fqn2tid <<< EnumeratedPropertyType) ct <*> normalizeTypeNames rle)
    calculatedProperties' <- fromFoldable <$> for ((toUnfoldable df.calculatedProperties) :: Array (Tuple String CalculatedProperty))
      (\(Tuple ct rle) -> Tuple <$> unwrap <$> (fqn2tid <<< CalculatedPropertyType) ct <*> normalizeTypeNames rle)
    states' <- fromFoldable <$> for ((toUnfoldable df.states) :: Array (Tuple String State))
      (\(Tuple ct st) -> Tuple <$> unwrap <$> (fqn2tid <<< StateIdentifier) ct <*> normalizeTypeNames st)
    views' <- fromFoldable <$> for ((toUnfoldable df.views) :: Array (Tuple String View))
      (\(Tuple ct vw) -> Tuple <$> unwrap <$> (fqn2tid <<< ViewType) ct <*> normalizeTypeNames vw)
    referredModels' <- for df.referredModels fqn2tid
    invertedQueriesInOtherDomains' <- fromFoldable <$> for ((toUnfoldable df.invertedQueriesInOtherDomains) :: Array (Tuple String (Array SeparateInvertedQuery)))
      (\(Tuple ct q) -> Tuple <$> (unwrap <$> ((fqn2tid (ModelUri ct)) :: WithSideCars (ModelUri Readable))) <*> traverse normalize q)
    upstreamStateNotifications' <- fromFoldable <$> for ((toUnfoldable df.upstreamStateNotifications) :: Array (Tuple String (Array UpstreamStateNotification)))
      (\(Tuple ct usn) -> Tuple <$> (unwrap <$> ((fqn2tid (ModelUri ct)) :: WithSideCars (ModelUri Readable))) <*> traverse normalize usn)
    upstreamAutomaticEffects' <- fromFoldable <$> for ((toUnfoldable df.upstreamAutomaticEffects) :: Array (Tuple String (Array UpstreamAutomaticEffect)))
      (\(Tuple ct aae) -> Tuple <$> (unwrap <$> ((fqn2tid (ModelUri ct)) :: WithSideCars (ModelUri Readable))) <*> traverse normalize aae)
    screens' <- EM.fromFoldable <$>
      ( for ((EM.toUnfoldable df.screens) :: Array (Tuple ScreenKey ScreenDefinition)) $
          \(Tuple ct sd) -> Tuple <$> normalize ct <*> normalize sd
      )
    id' <- fqn2tid df.id
    pure $ DomeinFile df
      { contexts = contexts'
      , enumeratedRoles = enumeratedRoles'
      , calculatedRoles = calculatedRoles'
      , enumeratedProperties = enumeratedProperties'
      , calculatedProperties = calculatedProperties'
      , states = states'
      , views = views'
      , referredModels = referredModels'
      , invertedQueriesInOtherDomains = invertedQueriesInOtherDomains'
      , upstreamStateNotifications = upstreamStateNotifications'
      , upstreamAutomaticEffects = upstreamAutomaticEffects'
      , screens = screens'
      , id = id'
      }

instance NormalizeTypeNames Context ContextType where
  fqn2tid (ContextType fqn) = do
    sidecars <- ask
    ContextType <$> case splitTypeUri fqn of
      Nothing -> pure fqn -- not a type uri
      Just { modelUri, localName } -> case Map.lookup ((ModelUri modelUri) :: ModelUri Readable) sidecars of
        Nothing -> pure fqn -- no sidecar for this model
        (Just stableIdMapping) -> case idUriForContext stableIdMapping (ContextUri fqn) of
          Nothing -> pure fqn -- no mapping found
          Just cuid -> pure cuid
  normalizeTypeNames (Context cr) = do
    id' <- fqn2tid cr.id
    contextAspects' <- for cr.contextAspects fqn2tid
    rolInContext' <- for cr.rolInContext fqn2tid
    contextRol' <- for cr.contextRol fqn2tid
    gebruikerRol' <- for cr.gebruikerRol fqn2tid
    nestedContexts' <- for cr.nestedContexts fqn2tid
    context' <- for cr.context fqn2tid
    (roleAliases' :: Array (Tuple String EnumeratedRoleType)) <- for (toUnfoldable cr.roleAliases) \(Tuple (key :: String) (value :: EnumeratedRoleType)) -> Tuple <$> (unwrap <$> (fqn2tid $ EnumeratedRoleType key)) <*> (fqn2tid value)
    pure $ Context
      ( cr
          { id = id'
          , contextAspects = contextAspects'
          , rolInContext = rolInContext'
          , contextRol = contextRol'
          , gebruikerRol = gebruikerRol'
          , nestedContexts = nestedContexts'
          , context = context'
          , roleAliases = fromFoldable roleAliases'
          }
      )

instance NormalizeTypeNames EnumeratedRole EnumeratedRoleType where
  fqn2tid (EnumeratedRoleType fqn) = do
    sidecars <- ask
    EnumeratedRoleType <$> case splitTypeUri fqn of
      Nothing -> pure fqn -- not a type uri
      Just { modelUri, localName } -> case Map.lookup (ModelUri modelUri) sidecars of
        Nothing -> pure fqn -- no sidecar for this model
        (Just stableIdMapping) -> case idUriForRole stableIdMapping (RoleUri fqn) of
          Nothing -> pure fqn -- no mapping found
          Just cuid -> pure cuid
  normalizeTypeNames (EnumeratedRole er) = do
    id' <- fqn2tid er.id
    context' <- fqn2tid er.context
    roleAspects' <- for er.roleAspects normalize
    properties' <- for er.properties fqn2tid
    (propertyAliases' :: Array (Tuple String EnumeratedPropertyType)) <- for (toUnfoldable er.propertyAliases) \(Tuple key value) -> Tuple <$> (unwrap <$> (fqn2tid $ EnumeratedPropertyType key)) <*> (fqn2tid value)
    completeType' <- traverseDPROD normalize er.completeType
    actions' <- EM.fromFoldable <$> for (EM.toUnfoldable er.actions) (\(Tuple ss objActs) -> Tuple ss <$> normalizeActionsForState ss objActs)
    perspectives' <- traverse normalize er.perspectives
    pure $ EnumeratedRole $ er
      { id = id'
      , context = context'
      , roleAspects = roleAspects'
      , properties = properties'
      , propertyAliases = fromFoldable propertyAliases'
      , completeType = completeType'
      , actions = actions'
      , perspectives = perspectives'
      }

instance NormalizeTypeNames CalculatedRole CalculatedRoleType where
  fqn2tid (CalculatedRoleType fqn) = do
    sidecars <- ask
    CalculatedRoleType <$> case splitTypeUri fqn of
      Nothing -> pure fqn -- not a type uri
      Just { modelUri, localName } -> case Map.lookup (ModelUri modelUri) sidecars of
        Nothing -> pure fqn -- no sidecar for this model
        (Just stableIdMapping) -> case idUriForRole stableIdMapping (RoleUri fqn) of
          Nothing -> pure fqn -- no mapping found
          Just cuid -> pure cuid
  normalizeTypeNames (CalculatedRole er) = do
    id' <- fqn2tid er.id
    context' <- fqn2tid er.context
    calculation' <- normalize er.calculation
    actions' <- EM.fromFoldable <$> for (EM.toUnfoldable er.actions) (\(Tuple ss objActs) -> Tuple ss <$> normalizeActionsForState ss objActs)
    perspectives' <- traverse normalize er.perspectives
    pure $ CalculatedRole $ er { id = id', context = context', calculation = calculation', actions = actions', perspectives = perspectives' }

-- Normalize action map keys for a given StateSpec into stable IDs using sidecars.
normalizeActionsForState
  :: StateSpec
  -> OBJ.Object Action
  -> WithSideCars (OBJ.Object Action)
normalizeActionsForState stateSpec obj = do
  sidecars <- ask
  let
    stateFqn = unwrap (stateSpec2StateIdentifier stateSpec)
    pairs = OBJ.toUnfoldable obj :: Array (Tuple String Action)
  folded <- for pairs \(Tuple localName act) -> do
    case splitTypeUri (stateFqn <> "$" <> localName) of
      Nothing -> pure (Tuple localName act)
      Just { modelUri } -> case Map.lookup (ModelUri modelUri) sidecars of
        Nothing -> pure (Tuple localName act)
        Just mapping -> case idUriForAction mapping (ActionUri (stateFqn <> "$" <> localName)) of
          Nothing -> pure (Tuple localName act)
          Just stableKey -> pure (Tuple stableKey act)
  pure $ OBJ.fromFoldable folded

-- Normalize a Perspective record structurally: translate type ids and action keys
instance normalizePerspectiveInst :: Normalize Perspective where
  normalize (Perspective pr) = do
    object' <- normalize pr.object
    roleTypes' <- traverse fqn2tid pr.roleTypes
    -- roleVerbs and propertyVerbs carry no type identifiers in values; keep as-is
    actions' <- EM.fromFoldable <$> for (EM.toUnfoldable pr.actions) (\(Tuple ss objActs) -> Tuple ss <$> normalizeActionsForState ss objActs)
    automaticStates' <- traverse fqn2tid pr.automaticStates
    pure $ Perspective pr
      { object = object'
      , roleTypes = roleTypes'
      , actions = actions'
      , automaticStates = automaticStates'
      }

instance NormalizeTypeNames Role RoleType where
  fqn2tid (ENR rt) = ENR <$> fqn2tid rt
  fqn2tid (CR rt) = CR <$> fqn2tid rt
  normalizeTypeNames (E rt) = E <$> normalizeTypeNames rt
  normalizeTypeNames (C rt) = C <$> normalizeTypeNames rt

instance NormalizeTypeNames EnumeratedProperty EnumeratedPropertyType where
  fqn2tid (EnumeratedPropertyType fqn) = do
    sidecars <- ask
    EnumeratedPropertyType <$> case splitTypeUri fqn of
      Nothing -> pure fqn -- not a type uri
      Just { modelUri, localName } -> case Map.lookup (ModelUri modelUri) sidecars of
        Nothing -> pure fqn -- no sidecar for this model
        (Just stableIdMapping) -> case idUriForProperty stableIdMapping (PropertyUri fqn) of
          Nothing -> pure fqn -- no mapping found
          Just cuid -> pure cuid
  normalizeTypeNames (EnumeratedProperty pt) = do
    id' <- fqn2tid pt.id
    role' <- fqn2tid pt.role
    pure $ EnumeratedProperty $ pt { id = id', role = role' }

instance NormalizeTypeNames CalculatedProperty CalculatedPropertyType where
  fqn2tid (CalculatedPropertyType fqn) = do
    sidecars <- ask
    CalculatedPropertyType <$> case splitTypeUri fqn of
      Nothing -> pure fqn -- not a type uri
      Just { modelUri, localName } -> case Map.lookup (ModelUri modelUri) sidecars of
        Nothing -> pure fqn -- no sidecar for this model
        (Just stableIdMapping) -> case idUriForProperty stableIdMapping (PropertyUri fqn) of
          Nothing -> pure fqn -- no mapping found
          Just cuid -> pure cuid
  normalizeTypeNames (CalculatedProperty pt) = do
    id' <- fqn2tid pt.id
    role' <- fqn2tid pt.role
    calculation' <- normalize pt.calculation
    pure $ CalculatedProperty $ pt { id = id', role = role', calculation = calculation' }

instance NormalizeTypeNames State StateIdentifier where
  fqn2tid (StateIdentifier fqn) = do
    sidecars <- ask
    StateIdentifier <$> case splitTypeUri fqn of
      Nothing -> pure fqn -- not a type uri
      Just { modelUri, localName } -> case Map.lookup (ModelUri modelUri) sidecars of
        Nothing -> pure fqn -- no sidecar for this model
        (Just stableIdMapping) -> case idUriForState stableIdMapping (StateUri fqn) of
          Nothing -> pure fqn -- no mapping found
          Just cuid -> pure cuid
  normalizeTypeNames (State s) = do
    id' <- fqn2tid s.id
    stateFulObject' <- case s.stateFulObject of
      Cnt ctype -> Cnt <$> fqn2tid ctype
      Orole rtype -> Orole <$> fqn2tid rtype
      Srole rtype -> Srole <$> fqn2tid rtype
    query' <- normalize s.query
    object' <- traverse normalize s.object
    subStates' <- for s.subStates fqn2tid
    notifyOnEntry' <- EM.fromFoldable <$> for (EM.toUnfoldable s.notifyOnEntry) \(Tuple rtype n) -> Tuple <$> fqn2tid rtype <*> normalize n
    notifyOnExit' <- EM.fromFoldable <$> for (EM.toUnfoldable s.notifyOnExit) \(Tuple rtype n) -> Tuple <$> fqn2tid rtype <*> normalize n
    automaticOnEntry' <- EM.fromFoldable <$> for (EM.toUnfoldable s.automaticOnEntry) \(Tuple rtype n) -> Tuple <$> fqn2tid rtype <*> normalize n
    automaticOnExit' <- EM.fromFoldable <$> for (EM.toUnfoldable s.automaticOnExit) \(Tuple rtype n) -> Tuple <$> fqn2tid rtype <*> normalize n
    pure $ State s { id = id', stateFulObject = stateFulObject', query = query', object = object', subStates = subStates', notifyOnEntry = notifyOnEntry', notifyOnExit = notifyOnExit', automaticOnEntry = automaticOnEntry', automaticOnExit = automaticOnExit' }

instance NormalizeTypeNames Prop.Property PropertyType where
  fqn2tid (ENP fqn) = ENP <$> fqn2tid fqn
  fqn2tid (CP fqn) = CP <$> fqn2tid fqn
  normalizeTypeNames (Prop.E pt) = Prop.E <$> normalizeTypeNames pt
  normalizeTypeNames (Prop.C pt) = Prop.C <$> normalizeTypeNames pt

instance NormalizeTypeNames View ViewType where
  fqn2tid (ViewType fqn) = do
    sidecars <- ask
    ViewType <$> case splitTypeUri fqn of
      Nothing -> pure fqn -- not a type uri
      Just { modelUri, localName } -> case Map.lookup (ModelUri modelUri) sidecars of
        Nothing -> pure fqn -- no sidecar for this model
        (Just stableIdMapping) -> case idUriForView stableIdMapping (ViewUri fqn) of
          Nothing -> pure fqn -- no mapping found
          Just cuid -> pure cuid
  normalizeTypeNames (View vw) = do
    id' <- fqn2tid vw.id
    role' <- fqn2tid vw.role
    propertyReferences' <- for vw.propertyReferences fqn2tid
    pure $ View $ vw { id = id', role = role', propertyReferences = propertyReferences' }

class Normalize v where
  -- | Replace user readable local names given in the model text by Cuids in the given object.
  normalize :: v -> WithSideCars v

-- Instances for structural normalization ------------------------------------

instance normalizeModelUri :: Normalize (ModelUri Stable) where
  normalize (ModelUri fqn) = ModelUri <<< unwrap <$> fqn2tid (ContextType fqn)

instance normalizeRoleInContext :: Normalize RoleInContext where
  normalize (RoleInContext { role, context }) =
    (\role' context' -> RoleInContext { role: role', context: context' })
      <$> fqn2tid role
      <*> fqn2tid context

instance normalizeCalculation :: Normalize Calculation where
  normalize s@(S _ _) = pure s
  normalize (Q qfd) = Q <$> normalize qfd

instance normalizeQfdInst :: Normalize QueryFunctionDescription where
  normalize qfd = traverseQfd nQfd qfd
    where
    nQfd :: QueryFunctionDescription -> WithSideCars QueryFunctionDescription
    nQfd (SQD dom qf ran fun man) = do
      dom' <- normalizeDomain dom
      qf' <- normalizeQueryFunction qf
      ran' <- normalizeDomain ran
      pure $ SQD dom' qf' ran' fun man
    nQfd (UQD dom qf subQfd ran fun man) = do
      dom' <- normalizeDomain dom
      qf' <- normalizeQueryFunction qf
      subQfd' <- normalize subQfd
      ran' <- normalizeDomain ran
      pure $ UQD dom' qf' subQfd' ran' fun man
    nQfd (BQD dom qf subQfd1 subQfd2 ran fun man) = do
      dom' <- normalizeDomain dom
      qf' <- normalizeQueryFunction qf
      subQfd1' <- normalize subQfd1
      subQfd2' <- normalize subQfd2
      ran' <- normalizeDomain ran
      pure $ BQD dom' qf' subQfd1' subQfd2' ran' fun man
    nQfd (MQD dom qf subQfds ran fun man) = do
      dom' <- normalizeDomain dom
      qf' <- normalizeQueryFunction qf
      subQfds' <- for subQfds normalize
      ran' <- normalizeDomain ran
      pure $ MQD dom' qf' subQfds' ran' fun man

    normalizeDomain :: Domain -> WithSideCars Domain
    normalizeDomain (RDOM (d :: ADT RoleInContext)) = RDOM <$> (traverse normalize d)
    normalizeDomain (CDOM (d :: ADT ContextType)) = CDOM <$> (traverse fqn2tid d)
    normalizeDomain (VDOM r (mp :: Maybe PropertyType)) = VDOM <$> pure r <*> (traverse fqn2tid mp)
    normalizeDomain d = pure d

    normalizeQueryFunction :: QueryFunction -> WithSideCars QueryFunction
    normalizeQueryFunction (PropertyGetter pt) = PropertyGetter <$> fqn2tid pt
    normalizeQueryFunction (Value2Role pt) = Value2Role <$> fqn2tid pt
    normalizeQueryFunction (RolGetter pt) = RolGetter <$> fqn2tid pt
    normalizeQueryFunction (RoleTypeConstant pt) = RoleTypeConstant <$> fqn2tid pt
    normalizeQueryFunction (ContextTypeConstant pt) = ContextTypeConstant <$> fqn2tid pt
    normalizeQueryFunction (CreateContext ct rt) = CreateContext <$> fqn2tid ct <*> fqn2tid rt
    normalizeQueryFunction (CreateRootContext ct) = CreateRootContext <$> fqn2tid ct
    normalizeQueryFunction (CreateContext_ ct) = CreateContext_ <$> fqn2tid ct
    normalizeQueryFunction (CreateRole rt) = CreateRole <$> fqn2tid rt
    normalizeQueryFunction (Bind rt) = Bind <$> fqn2tid rt
    normalizeQueryFunction (Unbind rt) = Unbind <$> traverse fqn2tid rt
    normalizeQueryFunction (DeleteRole rt) = DeleteRole <$> fqn2tid rt
    normalizeQueryFunction (DeleteContext rt) = DeleteContext <$> fqn2tid rt
    normalizeQueryFunction (DeleteProperty pt) = DeleteProperty <$> fqn2tid pt
    normalizeQueryFunction (AddPropertyValue pt) = AddPropertyValue <$> fqn2tid pt
    normalizeQueryFunction (RemovePropertyValue pt) = RemovePropertyValue <$> fqn2tid pt
    normalizeQueryFunction (SetPropertyValue pt) = SetPropertyValue <$> fqn2tid pt
    normalizeQueryFunction (CreateFileF s pt) = CreateFileF s <$> fqn2tid pt
    normalizeQueryFunction (FilledF rt ct) = FilledF <$> fqn2tid rt <*> fqn2tid ct
    normalizeQueryFunction f = pure f

instance normalizeNotificationInst :: Normalize Notification where
  normalize (ContextNotification facets@{ sentence, domain }) = do
    parts' <- traverse normalize (unwrap sentence).parts
    domain' <- normalize domain
    let sentence' = Sentence (unwrap sentence) { parts = parts' }
    pure $ ContextNotification facets { sentence = sentence', domain = domain' }
  normalize (RoleNotification facets@{ currentContextCalculation, sentence, domain }) = do
    parts' <- traverse normalize (unwrap sentence).parts
    currentContextCalculation' <- normalize currentContextCalculation
    domain' <- normalize domain
    let sentence' = Sentence (unwrap sentence) { parts = parts' }
    pure $ RoleNotification facets { currentContextCalculation = currentContextCalculation', sentence = sentence', domain = domain' }

instance normalizeAutomaticActionInst :: Normalize AutomaticAction where
  normalize (ContextAction facets@{ effect }) = do
    effect' <- normalize effect
    pure $ ContextAction facets { effect = effect' }
  normalize (RoleAction facets@{ effect, currentContextCalculation }) = do
    effect' <- normalize effect
    currentContextCalculation' <- normalize currentContextCalculation
    pure $ RoleAction facets { effect = effect', currentContextCalculation = currentContextCalculation' }

instance normalizeSeparateInvertedQueryInst :: Normalize SeparateInvertedQuery where
  normalize (RoleInvertedQuery enumeratedRoleType typeName invertedQuery) = do
    enumeratedRoleType' <- fqn2tid enumeratedRoleType
    ContextType typeName' <- fqn2tid (ContextType typeName)
    invertedQuery' <- normalize invertedQuery
    pure $ RoleInvertedQuery enumeratedRoleType' typeName' invertedQuery'
  normalize (ContextInvertedQuery contextType roleType invertedQuery) = do
    contextType' <- fqn2tid contextType
    EnumeratedRoleType roleType' <- fqn2tid (EnumeratedRoleType roleType)
    invertedQuery' <- normalize invertedQuery
    pure $ ContextInvertedQuery contextType' roleType' invertedQuery'
  normalize (FillerInvertedQuery invertedQueryKeys typeName invertedQuery) = do
    invertedQueryKeys' <- for invertedQueryKeys normalize
    EnumeratedRoleType typeName' <- fqn2tid (EnumeratedRoleType typeName)
    invertedQuery' <- normalize invertedQuery
    pure $ FillerInvertedQuery invertedQueryKeys' typeName' invertedQuery'
  normalize (FilledInvertedQuery invertedQueryKeys typeName invertedQuery) = do
    invertedQueryKeys' <- for invertedQueryKeys normalize
    EnumeratedRoleType typeName' <- fqn2tid (EnumeratedRoleType typeName)
    invertedQuery' <- normalize invertedQuery
    pure $ FilledInvertedQuery invertedQueryKeys' typeName' invertedQuery'
  normalize (OnPropertyDelta invertedQueryKeys typeName invertedQuery) = do
    invertedQueryKeys' <- for invertedQueryKeys fqn2tid
    EnumeratedPropertyType typeName' <- fqn2tid (EnumeratedPropertyType typeName)
    invertedQuery' <- normalize invertedQuery
    pure $ OnPropertyDelta invertedQueryKeys' typeName' invertedQuery'

instance normalizeInvertedQueryInst :: Normalize InvertedQuery where
  normalize invertedQuery = pure invertedQuery

instance normalizeInvertedQueryKeyInst :: Normalize InvertedQueryKey where
  normalize (InvertedQueryKey contextType1 contextType2 enumeratedRoleType) =
    InvertedQueryKey <$> fqn2tid contextType1 <*> fqn2tid contextType2 <*> fqn2tid enumeratedRoleType

instance normalizeUpstreamStateNotificationInst :: Normalize UpstreamStateNotification where
  normalize (UpstreamStateNotification usn) = do
    stateId' <- fqn2tid usn.stateId
    notification' <- normalize usn.notification
    qualifiedUsers' <- for usn.qualifiedUsers fqn2tid
    pure $ UpstreamStateNotification usn { stateId = stateId', notification = notification' }

instance normalizeUpstreamAutomaticEffectInst :: Normalize UpstreamAutomaticEffect where
  normalize (UpstreamAutomaticEffect aae) = do
    stateId' <- fqn2tid aae.stateId
    automaticAction' <- normalize aae.automaticAction
    qualifiedUsers' <- for aae.qualifiedUsers fqn2tid
    pure $ UpstreamAutomaticEffect aae { stateId = stateId', automaticAction = automaticAction', qualifiedUsers = qualifiedUsers' }

-- ScreenDefinition normalization -------------------------------------------------

instance normalizeScreenDefinitionInst :: Normalize ScreenDefinition where
  normalize (ScreenDefinition sd) = do
    tabs' <- traverse (traverse normalize) sd.tabs
    rows' <- traverse (traverse normalize) sd.rows
    columns' <- traverse (traverse normalize) sd.columns
    wwww' <- traverse normalize sd.whoWhatWhereScreen
    pure $ ScreenDefinition sd { tabs = tabs', rows = rows', columns = columns', whoWhatWhereScreen = wwww' }

instance normalizeTabDefInst :: Normalize TabDef where
  normalize (TabDef r) = do
    elements' <- traverse normalize r.elements
    pure $ TabDef r { elements = elements' }

instance normalizeScreenElementDefInst :: Normalize ScreenElementDef where
  normalize (RowElementD r) = RowElementD <$> normalize r
  normalize (ColumnElementD c) = ColumnElementD <$> normalize c
  normalize (TableElementD t) = TableElementD <$> normalize t
  normalize (FormElementD f) = FormElementD <$> normalize f
  normalize (MarkDownElementD m) = MarkDownElementD <$> normalize m
  normalize (ChatElementD c) = ChatElementD <$> normalize c

instance normalizeRowDefInst :: Normalize RowDef where
  normalize (RowDef xs) = RowDef <$> traverse normalize xs

instance normalizeColumnDefInst :: Normalize ColumnDef where
  normalize (ColumnDef xs) = ColumnDef <$> traverse normalize xs

instance normalizeTableDefInst :: Normalize TableDef where
  normalize (TableDef r) = do
    markdown' <- traverse normalize r.markdown
    widgetCommonFields' <- normalizeWidgetCommonFields r.widgetCommonFields
    pure $ TableDef r { markdown = markdown', widgetCommonFields = widgetCommonFields' }

instance normalizeFormDefInst :: Normalize FormDef where
  normalize (FormDef r) = do
    markdown' <- traverse normalize r.markdown
    widgetCommonFields' <- normalizeWidgetCommonFields r.widgetCommonFields
    pure $ FormDef r { markdown = markdown', widgetCommonFields = widgetCommonFields' }

instance normalizeMarkDownDefInst :: Normalize MarkDownDef where
  normalize (MarkDownConstantDef r) = do
    condition' <- traverse normalize r.condition
    pure $ MarkDownConstantDef r { condition = condition' }
  normalize (MarkDownPerspectiveDef r) = do
    widgetFields' <- normalizeWidgetCommonFields r.widgetFields
    conditionProperty' <- traverse fqn2tid r.conditionProperty
    pure $ MarkDownPerspectiveDef r { widgetFields = widgetFields', conditionProperty = conditionProperty' }
  normalize (MarkDownExpressionDef r) = do
    textQuery' <- normalize r.textQuery
    condition' <- traverse normalize r.condition
    pure $ MarkDownExpressionDef r { textQuery = textQuery', condition = condition' }

instance normalizeChatDefInst :: Normalize ChatDef where
  normalize (ChatDef fields) = do
    chatRole' <- fqn2tid fields.chatRole
    messageProperty' <- fqn2tid fields.messageProperty
    mediaProperty' <- fqn2tid fields.mediaProperty
    pure $ ChatDef fields { chatRole = chatRole', messageProperty = messageProperty', mediaProperty = mediaProperty' }

instance normalizeWhoWhatWhereScreenDefInst :: Normalize WhoWhatWhereScreenDef where
  normalize (WhoWhatWhereScreenDef r) = do
    who' <- normalize r.who
    what' <- normalize r.what
    whereto' <- normalize r.whereto
    pure $ WhoWhatWhereScreenDef { who: who', what: what', whereto: whereto' }

instance normalizeTableFormDefInst :: Normalize TableFormDef where
  normalize (TableFormDef r) = do
    markdown' <- traverse normalize r.markdown
    table' <- normalize r.table
    form' <- normalize r.form
    pure $ TableFormDef { markdown: markdown', table: table', form: form' }

instance normalizeWhatInst :: Normalize What where
  normalize (TableForms r) = do
    markdown' <- traverse normalize r.markdown
    tableForms' <- traverse normalize r.tableForms
    pure $ TableForms { markdown: markdown', tableForms: tableForms' }
  normalize (FreeFormScreen r) = do
    tabs' <- traverse (traverse normalize) r.tabs
    rows' <- traverse (traverse normalize) r.rows
    columns' <- traverse (traverse normalize) r.columns
    pure $ FreeFormScreen r { tabs = tabs', rows = rows', columns = columns' }

instance normalizeWhoInst :: Normalize Who where
  normalize (Who r) = do
    markdown' <- traverse normalize r.markdown
    chats' <- traverse normalize r.chats
    userRoles' <- traverse normalize r.userRoles
    pure $ Who { markdown: markdown', chats: chats', userRoles: userRoles' }

instance normalizeWhereToInst :: Normalize WhereTo where
  normalize (WhereTo r) = do
    markdown' <- traverse normalize r.markdown
    contextRoles' <- traverse normalize r.contextRoles
    pure $ WhereTo { markdown: markdown', contextRoles: contextRoles' }

instance normalizeScreenKeyInst :: Normalize ScreenKey where
  normalize (ScreenKey ct rt) = ScreenKey <$> fqn2tid ct <*> fqn2tid rt

-- Widget and restrictions normalization
-- PropertyRestrictions normalization via specialized instance
instance normalizePropertyRestrictionsInst :: Normalize (EM.EncodableMap PropertyType (Array PropertyVerb)) where
  normalize em = do
    let pairs = EM.toUnfoldable em :: Array (Tuple PropertyType (Array PropertyVerb))
    EM.fromFoldable <$> for pairs (\(Tuple k v) -> Tuple <$> fqn2tid k <*> pure v)

-- Helper for widget fields (cannot have an instance for a type synonym record)
normalizeWidgetCommonFields :: WidgetCommonFieldsDef -> WithSideCars WidgetCommonFieldsDef
normalizeWidgetCommonFields w = do
  propertyRestrictions' <- traverse normalize w.propertyRestrictions
  withoutProperties' <- traverse (traverse fqn2tid) w.withoutProperties
  userRole' <- fqn2tid w.userRole
  pure $ w { propertyRestrictions = propertyRestrictions', withoutProperties = withoutProperties', userRole = userRole' }
