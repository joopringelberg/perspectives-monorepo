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

import Data.Array (foldl)
import Data.List (foldl) as LIST
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as OBJ
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap (EncodableMap, values)
import Perspectives.Identifiers (typeUri2typeNameSpace)
import Perspectives.ModelTranslation.Representation (ModelTranslation(..), ContextsTranslation(..), ContextTranslation(..), RolesTranslation(..), RoleTranslation(..), PropertiesTranslation(..), ActionsPerStateTranslation(..), ActionsTranslation(..))
import Perspectives.Representation.Action (Action(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedRoleType(..))
import Perspectives.Representation.Class.PersistentType (tryGetPerspectType)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (Perspective(..), StateSpec)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), EnumeratedPropertyType(..), StateIdentifier(..))
import Perspectives.Sidecar.ToReadable (toReadable)
import Perspectives.Sidecar.ToStable (toStable)

-- | Transform a ModelTranslation whose keys are readable FQNs into one keyed by
-- | stable identifiers (cuid-composed) using a StableIdMapping.
-- | This mirrors the intent of normalizeTypes for DomeinFile structures.
fromReadableModelTranslation :: ModelTranslation -> MonadPerspectives ModelTranslation
fromReadableModelTranslation mt = normalizeModelTranslation mt

-- | Transform a ModelTranslation whose keys are Stable identifiers into one keyed by
-- | readable FQNs using ToReadable.
toReadableModelTranslation :: ModelTranslation -> MonadPerspectives ModelTranslation
toReadableModelTranslation mt = toReadableModel mt

-- Derive a readable namespace. If the existing namespace already looks readable (contains ':#'), keep it.
-- Otherwise, attempt to find any context FQN in the mapping and take its namespace part (segments before last '$').
-- Convert a ModelTranslation whose keys are Stable identifiers into one keyed by
-- readable FQNs using ToReadable. Keeps namespace as-is.
toReadableModel :: ModelTranslation -> MonadPerspectives ModelTranslation
toReadableModel (ModelTranslation rec) = do
  contexts' <- traverse toReadableContexts rec.contexts
  roles' <- traverse toReadableRoles rec.roles
  pure $ ModelTranslation rec { contexts = contexts', roles = roles' }

-- Reverse maps built by applying forward idUri* functions to every known FQN.
toReadableContexts :: ContextsTranslation -> MonadPerspectives ContextsTranslation
toReadableContexts (ContextsTranslation obj) = do
  obj' <- remapKeys toReadableContextKey (traverseOBJ toReadableContext obj)
  pure $ ContextsTranslation obj'

toReadableContext :: ContextTranslation -> MonadPerspectives ContextTranslation
toReadableContext (ContextTranslation rec) = do
  external' <- toReadableRole rec.external
  users' <- toReadableRoles rec.users
  things' <- toReadableRoles rec.things
  contextroles' <- toReadableRoles rec.contextroles
  contexts' <- toReadableContexts rec.contexts
  pure $ ContextTranslation rec { external = external', users = users', things = things', contextroles = contextroles', contexts = contexts' }

toReadableRoles :: RolesTranslation -> MonadPerspectives RolesTranslation
toReadableRoles (RolesTranslation obj) = do
  obj' <- remapKeys toReadableRoleKey (traverseOBJ toReadableRole obj)
  pure $ RolesTranslation obj'

toReadableRole :: RoleTranslation -> MonadPerspectives RoleTranslation
toReadableRole (RoleTranslation rec) = do
  properties' <- toReadableProperties rec.properties
  actions' <- toReadableActionsPerState rec.actions
  pure $ RoleTranslation rec { properties = properties', actions = actions' }

toReadableProperties :: PropertiesTranslation -> MonadPerspectives PropertiesTranslation
toReadableProperties (PropertiesTranslation obj) = do
  obj' <- remapKeys toReadablePropertyKey (pure obj)
  pure $ PropertiesTranslation obj'

toReadableActionsPerState :: ActionsPerStateTranslation -> MonadPerspectives ActionsPerStateTranslation
toReadableActionsPerState (ActionsPerStateTranslation obj) = do
  -- Remap nested action tables first, then state keys
  obj' <- traverseOBJ toReadableActions obj
  obj'' <- remapKeys toReadableStateKey (pure obj')
  pure $ ActionsPerStateTranslation obj''

toReadableActions :: ActionsTranslation -> MonadPerspectives ActionsTranslation
toReadableActions (ActionsTranslation at) = do
  at' <- remapKeys toReadableActionKey (pure at)
  pure $ ActionsTranslation at'

-- (no longer needed) remapKeysPure was used by the old sidecar-based approach.

-- Reader environment no longer required for readable conversion.

normalizeModelTranslation :: ModelTranslation -> MonadPerspectives ModelTranslation
normalizeModelTranslation mt@(ModelTranslation rec) = do
  contexts' <- traverse normalizeContexts rec.contexts
  roles' <- traverse normalizeRoles rec.roles
  pure $ ModelTranslation rec { contexts = contexts', roles = roles' }

normalizeContexts :: ContextsTranslation -> MonadPerspectives ContextsTranslation
normalizeContexts (ContextsTranslation obj) = do
  obj' <- remapKeys normalizeContextKey (normalizeContextTranslation obj)
  pure $ ContextsTranslation obj'
  where
  normalizeContextTranslation :: Object ContextTranslation -> MonadPerspectives (Object ContextTranslation)
  normalizeContextTranslation = traverseOBJ normalizeContext

normalizeContext :: ContextTranslation -> MonadPerspectives ContextTranslation
normalizeContext (ContextTranslation rec) = do
  external' <- normalizeRole rec.external
  users' <- normalizeRoles rec.users
  things' <- normalizeRoles rec.things
  contextroles' <- normalizeRoles rec.contextroles
  contexts' <- normalizeContexts rec.contexts
  pure $ ContextTranslation rec { external = external', users = users', things = things', contextroles = contextroles', contexts = contexts' }

normalizeRoles :: RolesTranslation -> MonadPerspectives RolesTranslation
normalizeRoles (RolesTranslation obj) = do
  obj' <- remapKeys normalizeRoleKey (traverseOBJ normalizeRole obj)
  pure $ RolesTranslation obj'

normalizeRole :: RoleTranslation -> MonadPerspectives RoleTranslation
normalizeRole (RoleTranslation rec) = do
  properties' <- normalizeProperties rec.properties
  actions' <- normalizeActionsPerState rec.actions
  pure $ RoleTranslation rec { properties = properties', actions = actions' }

normalizeProperties :: PropertiesTranslation -> MonadPerspectives PropertiesTranslation
normalizeProperties (PropertiesTranslation obj) = do
  obj' <- remapKeys normalizePropertyKey (pure obj)
  pure $ PropertiesTranslation obj'

normalizeActionsPerState :: ActionsPerStateTranslation -> MonadPerspectives ActionsPerStateTranslation
normalizeActionsPerState (ActionsPerStateTranslation obj) = do
  -- Each key is a state identifier in readable form. We attempt to normalize state part before the '$'
  obj' <- remapKeys normalizeStateKey (traverseOBJ normalizeActions obj)
  pure $ ActionsPerStateTranslation obj'

normalizeActions :: ActionsTranslation -> MonadPerspectives ActionsTranslation
normalizeActions (ActionsTranslation at) = do
  at' <- remapKeys normalizeActionKey (pure at)
  pure $ ActionsTranslation at'

-- Key normalization helpers --------------------------------------------------

normalizeContextKey :: String -> MonadPerspectives String
normalizeContextKey k = toStable (ContextType k) >>= \(ContextType stableK) -> pure stableK

-- Readable key conversion helpers ------------------------------------------

toReadableContextKey :: String -> MonadPerspectives String
toReadableContextKey k = toReadable (ContextType k) >>= \(ContextType readableK) -> pure readableK

toReadableRoleKey :: String -> MonadPerspectives String
toReadableRoleKey k = do
  -- Try enumerated, then calculated
  EnumeratedRoleType k' <- toReadable (EnumeratedRoleType k)
  if k == k' then do
    CalculatedRoleType k'' <- toReadable (CalculatedRoleType k)
    pure k''
  else pure k'

toReadablePropertyKey :: String -> MonadPerspectives String
toReadablePropertyKey k = do
  -- Try enumerated, then calculated
  EnumeratedPropertyType k' <- toReadable (EnumeratedPropertyType k)
  if k == k' then do
    CalculatedPropertyType k'' <- toReadable (CalculatedPropertyType k)
    pure k''
  else pure k'

toReadableStateKey :: String -> MonadPerspectives String
toReadableStateKey k = toReadable (StateIdentifier k) >>= \(StateIdentifier readableK) -> pure readableK

toReadableActionKey :: String -> MonadPerspectives String
toReadableActionKey k = do
  -- Given a stable action id, attempt to find its readable name by scanning
  -- the role and its perspectives’ actions.
  case typeUri2typeNameSpace k of
    Nothing -> pure k
    Just roleTid -> do
      mErole <- tryGetPerspectType (EnumeratedRoleType roleTid)
      case mErole of
        Just (EnumeratedRole rec) -> findInRole rec
        Nothing -> do
          mCrole <- tryGetPerspectType (CalculatedRoleType roleTid)
          case mCrole of
            Just (CalculatedRole rec) -> findInRole rec
            Nothing -> pure k
  where
  findInRole :: forall l. { actions :: EncodableMap StateSpec (OBJ.Object Action), perspectives :: Array Perspective | l } -> MonadPerspectives String
  findInRole { actions, perspectives } = do
    mReadable <- pure $ LIST.foldl
      ( \m accObj -> case m of
          Just r -> Just r
          Nothing -> findReadableInActions accObj
      )
      Nothing
      (values actions)
    case mReadable of
      Just readK -> pure readK
      Nothing -> do
        mReadable' <- pure $ findReadableInPerspectives perspectives
        case mReadable' of
          Just readK -> pure readK
          Nothing -> pure k

  findReadableInPerspectives :: Array Perspective -> Maybe String
  findReadableInPerspectives perspectives = LIST.foldl
    ( \m (Perspective { actions: pActions }) ->
        case m of
          Just r -> Just r
          Nothing -> LIST.foldl
            ( \m' actionObj -> case m' of
                Just r -> Just r
                Nothing -> findReadableInActions actionObj
            )
            m
            (values pActions)
    )
    Nothing
    perspectives

  findReadableInActions :: OBJ.Object Action -> Maybe String
  findReadableInActions actionsObj =
    foldl
      ( \m (Action { id, readable }) ->
          case m of
            Just r -> Just r
            Nothing -> if unwrap id == k then Just readable else Nothing
      )
      Nothing
      (OBJ.values actionsObj :: Array Action)

normalizeRoleKey :: String -> MonadPerspectives String
normalizeRoleKey k = do
  EnumeratedRoleType stableK <- toStable (EnumeratedRoleType k)
  if k == stableK then do
    -- Try calculated role
    CalculatedRoleType stableK' <- toStable (CalculatedRoleType k)
    pure stableK'
  else pure stableK

normalizePropertyKey :: String -> MonadPerspectives String
normalizePropertyKey k = do
  -- We have either an enumerated or calculated property.
  EnumeratedPropertyType k' <- toStable (EnumeratedPropertyType k)
  if k == k' then do
    CalculatedPropertyType stableK <- toStable (CalculatedPropertyType k)
    pure stableK
  else pure k'

normalizeStateKey :: String -> MonadPerspectives String
normalizeStateKey k = toStable (StateIdentifier k) >>= \(StateIdentifier stableK) -> pure stableK

normalizeActionKey :: String -> MonadPerspectives String
normalizeActionKey k = do
  -- An action is either part of a perspective, or part of a role.
  -- Its identifier is built from the user role identifier and local name that is unique within the set of actions for that role
  -- Retrieve the role, check the context actions and then the actions in the perspectives.
  -- Actions are collected in a Foreign Object. Collect the values (Action objects) and compare their readable 
  -- members with k.
  -- The actual action structure contains its Stable identifier.
  case typeUri2typeNameSpace k of
    Nothing -> pure k
    Just roleFqn -> do
      mErole <- tryGetPerspectType (EnumeratedRoleType roleFqn)
      case mErole of
        Just (EnumeratedRole { actions, perspectives }) -> findInRole { actions, perspectives }
        Nothing -> do
          mCrole <- tryGetPerspectType (CalculatedRoleType roleFqn)
          case mCrole of
            Just (CalculatedRole { actions, perspectives }) -> findInRole { actions, perspectives }
            Nothing -> pure k
  where
  findInRole :: { actions :: EncodableMap StateSpec (OBJ.Object Action), perspectives :: Array Perspective } -> MonadPerspectives String
  findInRole { actions, perspectives } = do
    mStable <- pure $ LIST.foldl
      ( \mStableId' actionObj ->
          case mStableId' of
            Just sid -> Just sid
            Nothing -> findInActions actionObj
      )
      Nothing
      (values actions)
    case mStable of
      Just stableK -> pure stableK
      Nothing -> do
        mStable' <- pure $ findInPerspectives perspectives
        case mStable' of
          Just stableK -> pure stableK
          Nothing -> pure k

  findInPerspectives :: Array Perspective -> (Maybe String)
  findInPerspectives perspectives = foldl
    ( \mStableId (Perspective { actions: pActions }) ->
        case mStableId of
          Just sid -> Just sid
          Nothing -> LIST.foldl
            ( \mStableId' actionObj ->
                case mStableId' of
                  Just sid -> Just sid
                  Nothing -> findInActions actionObj
            )
            mStableId
            (values pActions)
    )
    Nothing
    perspectives

  findInActions :: OBJ.Object Action -> (Maybe String)
  findInActions actionsObj = do
    foldl
      ( \mStableId (Action { id, readable }) ->
          case mStableId of
            Just sid -> Just sid
            Nothing ->
              if readable == k then
                Just $ unwrap id
              else
                Nothing
      )
      Nothing
      (OBJ.values actionsObj :: Array Action)

-- Generic object traversal & remapping ---------------------------------------

-- Traverse all values in an Object
traverseOBJ :: forall a b. (a -> MonadPerspectives b) -> Object a -> MonadPerspectives (Object b)
traverseOBJ f obj = do
  pairs <- traverse (\(Tuple k v) -> Tuple k <$> f v) (OBJ.toUnfoldable obj :: Array (Tuple String a))
  pure (OBJ.fromFoldable pairs)

-- Remap keys of an object using key normalization; then apply value transformation.
remapKeys :: forall a. (String -> MonadPerspectives String) -> MonadPerspectives (Object a) -> MonadPerspectives (Object a)
remapKeys fk mvObj = do
  obj <- mvObj
  pairs <- traverse
    ( \(Tuple k v) -> do
        k' <- fk k
        pure (Tuple k' v)
    )
    (OBJ.toUnfoldable obj :: Array (Tuple String a))
  pure (OBJ.fromFoldable pairs)
