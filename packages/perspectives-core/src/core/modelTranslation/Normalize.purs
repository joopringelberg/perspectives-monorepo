-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2025 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.ModelTranslation.Normalize
  ( fromReadableModelTranslation
  , toReadableModelTranslation
  ) where

import Prelude

import Control.Monad.Reader (Reader, ask, runReader)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Array (mapMaybe, uncons)
import Data.String as S
import Data.String.CodeUnits as CU
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as OBJ
import Perspectives.ModelTranslation.Representation (ModelTranslation(..), ContextsTranslation(..), ContextTranslation(..), RolesTranslation(..), RoleTranslation(..), PropertiesTranslation(..), ActionsPerStateTranslation(..), ActionsTranslation)
import Perspectives.Sidecar.StableIdMapping (StableIdMapping, ContextUri(..), RoleUri(..), PropertyUri(..), StateUri(..), idUriForContext, idUriForRole, idUriForProperty, idUriForState)
import Perspectives.Identifiers (deconstructBuitenRol, isExternalRole)

-- | Transform a ModelTranslation whose keys are readable FQNs into one keyed by
-- | stable identifiers (cuid-composed) using a StableIdMapping.
-- | This mirrors the intent of normalizeTypes for DomeinFile structures.
fromReadableModelTranslation :: StableIdMapping -> ModelTranslation -> ModelTranslation
fromReadableModelTranslation mapping mt = runReader (normalizeModelTranslation mt) mapping

-- | Attempt to invert the normalization, turning stable ids back into readable FQNs using the StableIdMapping.
-- | If a stable id cannot be resolved, it is left unchanged. External roles are reconstructed by
-- | resolving their context id and appending $External.
toReadableModelTranslation :: StableIdMapping -> ModelTranslation -> ModelTranslation
toReadableModelTranslation mapping (ModelTranslation rec) =
  let
    rev = buildReverse mapping
    contexts' = toReadableContexts rev <$> rec.contexts
    roles' = toReadableRoles rev <$> rec.roles
    namespace' = deriveReadableNamespace mapping rec.namespace
  in
    ModelTranslation rec { contexts = contexts', roles = roles', namespace = namespace' }

-- Derive a readable namespace. If the existing namespace already looks readable (contains ':#'), keep it.
-- Otherwise, attempt to find any context FQN in the mapping and take its namespace part (segments before last '$').
deriveReadableNamespace :: StableIdMapping -> String -> String
deriveReadableNamespace m current =
  if looksReadable current then current
  else
    case uncons (OBJ.toUnfoldable m.contextCuids :: Array (Tuple String String)) of
      Just { head: Tuple fqn _ } -> takeNamespace fqn
      Nothing -> current
  where
  looksReadable s = S.contains (S.Pattern ":") s && S.contains (S.Pattern "#") s
  takeNamespace f =
    case lastDollar f of
      Nothing -> f
      Just ix -> S.take ix f
  lastDollar s =
    let len = S.length s in go (len - 1)
    where
    go i | i < 0 = Nothing
    go i = case CU.charAt i s of
      Just '$' -> Just i
      _ -> go (i - 1)

-- Reverse maps built by applying forward idUri* functions to every known FQN.
type ReverseMaps =
  { contexts :: Object String
  , roles :: Object String
  , properties :: Object String
  , states :: Object String
  }

buildReverse :: StableIdMapping -> ReverseMaps
buildReverse m =
  { contexts: mk ContextUri idUriForContext m.contextCuids
  , roles: mk RoleUri idUriForRole m.roleCuids
  , properties: mk PropertyUri idUriForProperty m.propertyCuids
  , states: mk StateUri idUriForState m.stateCuids
  }
  where
  mk :: forall a. (String -> a) -> (StableIdMapping -> a -> Maybe String) -> Object String -> Object String
  mk wrap forward obj =
    OBJ.fromFoldable $ mapMaybe
      ( \(Tuple fqn _cuid) -> case forward m (wrap fqn) of
          Just sid -> Just (Tuple sid fqn)
          Nothing -> Nothing
      )
      (OBJ.toUnfoldable obj :: Array (Tuple String String))

toReadableContexts :: ReverseMaps -> ContextsTranslation -> ContextsTranslation
toReadableContexts rev (ContextsTranslation obj) =
  ContextsTranslation $ remapKeysPure (lookupContext rev) (map (toReadableContext rev) obj)

toReadableContext :: ReverseMaps -> ContextTranslation -> ContextTranslation
toReadableContext rev (ContextTranslation rec) =
  let
    external' = toReadableRole rev rec.external
    users' = toReadableRoles rev rec.users
    things' = toReadableRoles rev rec.things
    contextroles' = toReadableRoles rev rec.contextroles
    contexts' = toReadableContexts rev rec.contexts
  in
    ContextTranslation rec { external = external', users = users', things = things', contextroles = contextroles', contexts = contexts' }

toReadableRoles :: ReverseMaps -> RolesTranslation -> RolesTranslation
toReadableRoles rev (RolesTranslation obj) =
  RolesTranslation $ remapKeysPure (lookupRole rev) (map (toReadableRole rev) obj)

toReadableRole :: ReverseMaps -> RoleTranslation -> RoleTranslation
toReadableRole rev (RoleTranslation rec) =
  let
    properties' = toReadableProperties rev rec.properties
    actions' = toReadableActionsPerState rev rec.actions
  in
    RoleTranslation rec { properties = properties', actions = actions' }

toReadableProperties :: ReverseMaps -> PropertiesTranslation -> PropertiesTranslation
toReadableProperties rev (PropertiesTranslation obj) =
  PropertiesTranslation $ remapKeysPure (lookupProperty rev) obj

toReadableActionsPerState :: ReverseMaps -> ActionsPerStateTranslation -> ActionsPerStateTranslation
toReadableActionsPerState rev (ActionsPerStateTranslation obj) =
  ActionsPerStateTranslation $ remapKeysPure (lookupState rev) obj

lookupContext :: ReverseMaps -> String -> String
lookupContext rev key = fromMaybe key (OBJ.lookup key rev.contexts)

lookupRole :: ReverseMaps -> String -> String
lookupRole rev key =
  case OBJ.lookup key rev.roles of
    Just v -> v
    Nothing ->
      if isExternalRole key then
        let
          ctxStable = deconstructBuitenRol key
        in
          case OBJ.lookup ctxStable rev.contexts of
            Just ctxFqn -> ctxFqn <> "$External"
            Nothing -> key
      else key

lookupProperty :: ReverseMaps -> String -> String
lookupProperty rev key = fromMaybe key (OBJ.lookup key rev.properties)

lookupState :: ReverseMaps -> String -> String
lookupState rev key = fromMaybe key (OBJ.lookup key rev.states)

remapKeysPure :: forall a. (String -> String) -> Object a -> Object a
remapKeysPure fk obj =
  OBJ.fromFoldable ((OBJ.toUnfoldable obj :: Array (Tuple String a)) <#> \(Tuple k v) -> Tuple (fk k) v)

-- Reader environment is the StableIdMapping we resolve against.
type Env = StableIdMapping

normalizeModelTranslation :: ModelTranslation -> Reader Env ModelTranslation
normalizeModelTranslation mt@(ModelTranslation rec) = do
  contexts' <- traverse normalizeContexts rec.contexts
  roles' <- traverse normalizeRoles rec.roles
  pure $ ModelTranslation rec { contexts = contexts', roles = roles' }

normalizeContexts :: ContextsTranslation -> Reader Env ContextsTranslation
normalizeContexts (ContextsTranslation obj) = do
  obj' <- remapKeys normalizeContextKey (normalizeContextTranslation obj)
  pure $ ContextsTranslation obj'
  where
  normalizeContextTranslation :: Object ContextTranslation -> Reader Env (Object ContextTranslation)
  normalizeContextTranslation = traverseOBJ normalizeContext

normalizeContext :: ContextTranslation -> Reader Env ContextTranslation
normalizeContext (ContextTranslation rec) = do
  external' <- normalizeRole rec.external
  users' <- normalizeRoles rec.users
  things' <- normalizeRoles rec.things
  contextroles' <- normalizeRoles rec.contextroles
  contexts' <- normalizeContexts rec.contexts
  pure $ ContextTranslation rec { external = external', users = users', things = things', contextroles = contextroles', contexts = contexts' }

normalizeRoles :: RolesTranslation -> Reader Env RolesTranslation
normalizeRoles (RolesTranslation obj) = do
  obj' <- remapKeys normalizeRoleKey (traverseOBJ normalizeRole obj)
  pure $ RolesTranslation obj'

normalizeRole :: RoleTranslation -> Reader Env RoleTranslation
normalizeRole (RoleTranslation rec) = do
  properties' <- normalizeProperties rec.properties
  actions' <- normalizeActionsPerState rec.actions
  pure $ RoleTranslation rec { properties = properties', actions = actions' }

normalizeProperties :: PropertiesTranslation -> Reader Env PropertiesTranslation
normalizeProperties (PropertiesTranslation obj) = do
  obj' <- remapKeys normalizePropertyKey (pure obj)
  pure $ PropertiesTranslation obj'

normalizeActionsPerState :: ActionsPerStateTranslation -> Reader Env ActionsPerStateTranslation
normalizeActionsPerState (ActionsPerStateTranslation obj) = do
  -- Each key is a state identifier in readable form. We attempt to normalize state part before the '$'
  obj' <- remapKeys normalizeStateKey (traverseOBJ normalizeActions obj)
  pure $ ActionsPerStateTranslation obj'

normalizeActions :: ActionsTranslation -> Reader Env ActionsTranslation
normalizeActions at = pure at -- Action names themselves are not type identifiers; keys already handled at state level.

-- Key normalization helpers --------------------------------------------------

normalizeContextKey :: String -> Reader Env String
normalizeContextKey k = do
  env <- ask
  pure $ fromMaybe k (idUriForContext env (ContextUri k))

normalizeRoleKey :: String -> Reader Env String
normalizeRoleKey k = do
  env <- ask
  if isExternalRole k then do
    -- External role identifiers are derived from their context; map context then append suffix.
    let ctxFqn = deconstructBuitenRol k
    let externalSuffix = "$External"
    let base = fromMaybe ctxFqn (idUriForContext env (ContextUri ctxFqn))
    pure (base <> externalSuffix)
  else
    pure $ fromMaybe k (idUriForRole env (RoleUri k))

normalizePropertyKey :: String -> Reader Env String
normalizePropertyKey k = do
  env <- ask
  pure $ fromMaybe k (idUriForProperty env (PropertyUri k))

normalizeStateKey :: String -> Reader Env String
normalizeStateKey k = do
  env <- ask
  -- Keys for ActionsPerStateTranslation are state identifiers; normalize fully.
  pure $ fromMaybe k (idUriForState env (StateUri k))

-- Generic object traversal & remapping ---------------------------------------

-- Traverse all values in an Object
traverseOBJ :: forall a b. (a -> Reader Env b) -> Object a -> Reader Env (Object b)
traverseOBJ f obj = do
  pairs <- traverse (\(Tuple k v) -> Tuple k <$> f v) (OBJ.toUnfoldable obj :: Array (Tuple String a))
  pure (OBJ.fromFoldable pairs)

-- Remap keys of an object using key normalization; then apply value transformation.
remapKeys :: forall a. (String -> Reader Env String) -> Reader Env (Object a) -> Reader Env (Object a)
remapKeys fk mvObj = do
  obj <- mvObj
  pairs <- traverse
    ( \(Tuple k v) -> do
        k' <- fk k
        pure (Tuple k' v)
    )
    (OBJ.toUnfoldable obj :: Array (Tuple String a))
  pure (OBJ.fromFoldable pairs)
