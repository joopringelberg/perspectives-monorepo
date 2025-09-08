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
  ( WithSideCars
  , class NormalizeTypeNames
  , normalizeTypeNames
  , fqn2tid
  , normalizeTypes
  )
  where

import Prelude

import Control.Monad.Reader (Reader, ask, runReaderT)
import Data.Array (catMaybes, foldM)
import Data.Map (Map, fromFoldable, lookup, insert, singleton) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (fromFoldable, toUnfoldable)
import Perspectives.CoreTypes (MonadPerspectives, (##=), (##>))
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (splitTypeUri)
import Perspectives.Instances.ObjectGetters (getProperty)
import Perspectives.ModelDependencies (modelURI, modelsInUse, versionedDomeinFileName)
import Perspectives.Names (getMySystem)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), RoleInContext(..), traverseQfd)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.CNF (traverseDPROD)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Property (Property(..)) as Prop
import Perspectives.Representation.Class.Role (Role(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), Value(..))
import Perspectives.Representation.QueryFunction (QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), DomeinFileId(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.Sidecar.StableIdMapping (ContextUri(..), ModelUri(..), PropertyUri(..), Readable, RoleUri(..), Stable, StableIdMapping, idUriForContext, idUriForProperty, idUriForRole, loadStableMapping)

normalizeTypes :: DomeinFile -> StableIdMapping -> MonadPerspectives DomeinFile
normalizeTypes df@(DomeinFile {namespace, referredModels}) mapping = do
  -- Notice that referredModels from a freshly parsed Arc source will be FQNs with names given by the modeller, rather than underlying cuids.
  cuidMap <- getinstalledModelCuids
  sidecars <- foldM (\scs (DomeinFileId referredModel) -> case Map.lookup (ModelUri referredModel) cuidMap of
      Nothing -> pure scs
      Just (cuid :: ModelUri Stable) -> do 
        mmapping <- loadStableMapping cuid
        case mmapping of 
          Nothing -> pure scs
          Just submapping -> pure $ Map.insert (ModelUri referredModel) submapping scs)
    (Map.singleton (ModelUri namespace) mapping)
    referredModels
  pure $ unwrap $ runReaderT
    (normalizeTypeNames df)
    sidecars

  where 

    -- A map from DomeinFileName without cuid to DomeinFileId with cuid. 
    -- Not every installed model need have cuid!
    getinstalledModelCuids :: MonadPerspectives (Map.Map (ModelUri Readable)(ModelUri Stable))
    getinstalledModelCuids = do
      system <- getMySystem
      modelRoles <- (ContextInstance system) ##= getRoleInstances (ENR $ EnumeratedRoleType modelsInUse)
      x :: Array (Maybe (Tuple (ModelUri Readable) (ModelUri Stable))) <- for modelRoles \ri -> do
        mcuid <- ri ##> getProperty (EnumeratedPropertyType versionedDomeinFileName)
        case mcuid of 
          Nothing -> pure Nothing
          Just _ -> do 
            mdfid <- ri ##> getProperty (EnumeratedPropertyType modelURI)
            case mdfid of 
              Nothing -> pure Nothing
              Just (Value dfid) -> pure $ Just $ Tuple (ModelUri dfid) (ModelUri dfid) -- LET OP! ZE KUNNEN NIET BEIDE DFID ZIJN!!
      pure $ Map.fromFoldable $ catMaybes x


-- | This monad supports 'ask'. `ask` will return an object whose keys are Model Ids (MIDs).
type WithSideCars = Reader (Map.Map (ModelUri Readable) StableIdMapping)

class NormalizeTypeNames v ident | v -> ident, ident -> v where
  -- | Replace user readable local names given in the model text by Cuids in the given object.
  normalizeTypeNames :: v -> WithSideCars v
  -- Dit zou van Readable naar Stable moeten zijn.
  fqn2tid :: ident -> WithSideCars ident

instance NormalizeTypeNames DomeinFile DomeinFileId where
  fqn2tid df = DomeinFileId <<< unwrap <$> fqn2tid (ContextType $ unwrap df)
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
    pure $ DomeinFile df
      { contexts = contexts'
      , enumeratedRoles = enumeratedRoles'
      , calculatedRoles = calculatedRoles'
      , enumeratedProperties = enumeratedProperties'
      , calculatedProperties = calculatedProperties'
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
    roleAliases' <- for cr.roleAliases fqn2tid
    pure $ Context (cr
      { id = id'
      , contextAspects = contextAspects'
      , rolInContext = rolInContext'
      , contextRol = contextRol'
      , gebruikerRol = gebruikerRol'
      , nestedContexts = nestedContexts'
      , context = context'
      , roleAliases = roleAliases'
      })

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
    roleAspects' <- for er.roleAspects fqn2tidRoleInContext
    properties' <- for er.properties fqn2tid
    propertyAliases' <- for er.propertyAliases fqn2tid
    completeType' <- traverseDPROD fqn2tidRoleInContext er.completeType
    pure $ EnumeratedRole $ er 
      { id = id' 
      , context = context'
      , roleAspects = roleAspects'
      , properties = properties'
      , propertyAliases = propertyAliases'
      , completeType = completeType'
      }
fqn2tidRoleInContext :: RoleInContext -> WithSideCars RoleInContext
fqn2tidRoleInContext (RoleInContext {role, context}) = (\role' context' -> RoleInContext { role: role', context: context' }) <$> fqn2tid role <*> fqn2tid context

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
    calculation' <- normalizeCalculationTypeNames er.calculation
    pure $ CalculatedRole $ er { id = id', context = context', calculation = calculation' }

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
    calculation' <- normalizeCalculationTypeNames pt.calculation
    pure $ CalculatedProperty $ pt { id = id', role = role', calculation = calculation' }

instance NormalizeTypeNames Prop.Property PropertyType where
  fqn2tid (ENP fqn) = ENP <$> fqn2tid fqn
  fqn2tid (CP fqn) = CP <$> fqn2tid fqn
  normalizeTypeNames (Prop.E pt) = Prop.E <$> normalizeTypeNames pt
  normalizeTypeNames (Prop.C pt) = Prop.C <$> normalizeTypeNames pt

normalizeCalculationTypeNames :: Calculation -> WithSideCars Calculation
normalizeCalculationTypeNames s@(S _ _) = pure s
normalizeCalculationTypeNames (Q qfd) = Q <$> normalizeQfd qfd

normalizeQfd :: QueryFunctionDescription -> WithSideCars QueryFunctionDescription
normalizeQfd qfd = traverseQfd nQfd qfd
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
      subQfd' <- normalizeQfd subQfd
      ran' <- normalizeDomain ran
      pure $ UQD dom' qf' subQfd' ran' fun man
    nQfd (BQD dom qf subQfd1 subQfd2 ran fun man) = do
      dom' <- normalizeDomain dom
      qf' <- normalizeQueryFunction qf
      subQfd1' <- normalizeQfd subQfd1
      subQfd2' <- normalizeQfd subQfd2
      ran' <- normalizeDomain ran
      pure $ BQD dom' qf' subQfd1' subQfd2' ran' fun man
    nQfd (MQD dom qf subQfds ran fun man) = do
      dom' <- normalizeDomain dom
      qf' <- normalizeQueryFunction qf
      subQfds' <- for subQfds normalizeQfd 
      ran' <- normalizeDomain ran
      pure $ MQD dom' qf' subQfds' ran' fun man

    normalizeDomain :: Domain -> WithSideCars Domain
    normalizeDomain (RDOM (d :: ADT RoleInContext)) = RDOM <$> (traverse fqn2tidRoleInContext d)
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