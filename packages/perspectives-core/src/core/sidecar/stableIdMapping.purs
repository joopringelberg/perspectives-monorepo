-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2025 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Sidecar.StableIdMapping
  ( ContextKeySnapshot
  , module Perspectives.SideCar.PhantomTypedNewtypes
  , PropertyKeySnapshot
  , ViewKeySnapshot
  , StateKeySnapshot
  , RoleKeySnapshot
  , ActionKeySnapshot
  , StableIdMapping
  , emptyStableIdMapping
  , idUriForContext
  , idUriForProperty
  , idUriForRole
  , idUriForView
  , idUriForState
  , idUriForAction
  , loadStableMapping
  , lookupContextCuid
  , lookupViewCuid
  , lookupStateCuid
  , lookupPropertyCuid
  , lookupRoleCuid
  , lookupActionCuid
  ) where

import Prelude (bind, pure, ($), (<>), (==))

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object, empty)
import Foreign.Object as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Identifiers (modelUri2ModelUrl, typeUri2typeNameSpace_)
import Perspectives.Persistence.API (fromBlob, getAttachment)
import Perspectives.SideCar.PhantomTypedNewtypes (ActionUri(..), ContextUri(..), ModelUri(..), PropertyUri(..), Readable, RoleUri(..), Stable, StateUri(..), ViewUri(..))
import Simple.JSON (readJSON)

-- A compact, forward-compatible skeleton mapping sidecar.
-- Keep this module dependency-light to avoid import cycles.
-- Consumers can serialise with Simple.JSON.writeJSON locally.

-- Lightweight snapshots of the last-seen canonical keys used for heuristic matching.
-- These mirror the feature sets used by the heuristics without importing heavy modules.
type ContextKeySnapshot =
  { fqn :: String
  , declaringContextFqn :: String
  , roles :: Array String
  , properties :: Array String
  , aspects :: Array String
  }

type RoleKeySnapshot =
  { fqn :: String
  , declaringContextFqn :: String
  , properties :: Array String
  , bindingTypes :: Array String
  , aspects :: Array String
  }

type PropertyKeySnapshot =
  { fqn :: String
  , valueType :: String
  , facets :: Array String
  , aspects :: Array String
  , declaringRoleFqn :: String
  }

type ViewKeySnapshot =
  { fqn :: String
  , declaringRoleFqn :: String
  , properties :: Array String
  }

type StateKeySnapshot =
  { fqn :: String
  , queryHash :: String
  }

-- Actions live under a role and state; we snapshot only the FQN for now.
-- If needed, we can extend with effect hashes later.
type ActionKeySnapshot =
  { fqn :: String
  , declaringRoleFqn :: String
  , declaringStateFqn :: String
  }

type StableIdMapping =
  { version :: Int
  , contexts :: Object String
  , roles :: Object String
  , properties :: Object String
  , views :: Object String
  , states :: Object String
  , actions :: Object String
  -- Baseline canonical keys for heuristics on the next compile.
  , contextKeys :: Object ContextKeySnapshot
  , roleKeys :: Object RoleKeySnapshot
  , propertyKeys :: Object PropertyKeySnapshot
  , viewKeys :: Object ViewKeySnapshot
  , stateKeys :: Object StateKeySnapshot
  , actionKeys :: Object ActionKeySnapshot
  -- Stable ids per kind (FQN -> CUID). Canonical entries should have these.
  , contextCuids :: Object String
  , roleCuids :: Object String
  , propertyCuids :: Object String
  , viewCuids :: Object String
  , stateCuids :: Object String
  , actionCuids :: Object String
  , modelIdentifier :: ModelUri Stable
  }

emptyStableIdMapping :: StableIdMapping
emptyStableIdMapping =
  { version: 4
  , contexts: empty
  , roles: empty
  , properties: empty
  , views: empty
  , states: empty
  , actions: empty
  , contextKeys: empty
  , roleKeys: empty
  , propertyKeys: empty
  , viewKeys: empty
  , stateKeys: empty
  , actionKeys: empty
  , contextCuids: empty
  , roleCuids: empty
  , propertyCuids: empty
  , viewCuids: empty
  , stateCuids: empty
  , actionCuids: empty
  , modelIdentifier: ModelUri ""
  }

-- Resolve a CUID for an FQN:
-- 1) Try direct (legacy keys can still exist in *Cuids).
-- 2) Resolve via alias map to canonical, then lookup in *Cuids.
lookupContextCuid :: StableIdMapping -> ContextUri Readable -> Maybe String
lookupContextCuid m (ContextUri fqn) =
  case OBJ.lookup fqn m.contextCuids of
    Just v -> Just v
    Nothing -> case OBJ.lookup fqn m.contexts of
      Just canonical -> OBJ.lookup canonical m.contextCuids
      Nothing -> Nothing

lookupRoleCuid :: StableIdMapping -> RoleUri Readable -> Maybe String
lookupRoleCuid m (RoleUri fqn) =
  case OBJ.lookup fqn m.roleCuids of
    Just v -> Just v
    Nothing -> case OBJ.lookup fqn m.roles of
      Just canonical -> OBJ.lookup canonical m.roleCuids
      Nothing -> Nothing

lookupPropertyCuid :: StableIdMapping -> PropertyUri Readable -> Maybe String
lookupPropertyCuid m (PropertyUri fqn) =
  case OBJ.lookup fqn m.propertyCuids of
    Just v -> Just v
    Nothing -> case OBJ.lookup fqn m.properties of
      Just canonical -> OBJ.lookup canonical m.propertyCuids
      Nothing -> Nothing

lookupViewCuid :: StableIdMapping -> ViewUri Readable -> Maybe String
lookupViewCuid m (ViewUri fqn) =
  case OBJ.lookup fqn m.viewCuids of
    Just v -> Just v
    Nothing -> case OBJ.lookup fqn m.views of
      Just canonical -> OBJ.lookup canonical m.viewCuids
      Nothing -> Nothing

lookupStateCuid :: StableIdMapping -> StateUri Readable -> Maybe String
lookupStateCuid m (StateUri fqn) =
  case OBJ.lookup fqn m.stateCuids of
    Just v -> Just v
    Nothing -> case OBJ.lookup fqn m.states of
      Just canonical -> OBJ.lookup canonical m.stateCuids
      Nothing -> Nothing

lookupActionCuid :: StableIdMapping -> ActionUri Readable -> Maybe String
lookupActionCuid m (ActionUri fqn) =
  case OBJ.lookup fqn m.actionCuids of
    Just v -> Just v
    Nothing -> case OBJ.lookup fqn m.actions of
      Just canonical -> OBJ.lookup canonical m.actionCuids
      Nothing -> Nothing

loadStableMapping :: ModelUri Stable -> MonadPerspectives (Maybe StableIdMapping)
loadStableMapping (ModelUri domeinFileName) = do
  let split = unsafePartial modelUri2ModelUrl domeinFileName
  -- Retrieve existing sidecar (if any) from repository
  mBlob <- getAttachment split.repositoryUrl split.documentName "stableIdMapping.json"
  case mBlob of
    Nothing -> pure Nothing
    Just blob -> do
      txt <- liftAff $ fromBlob blob
      pure $ case readJSON txt of
        Right (m :: StableIdMapping) -> Just m
        _ -> Nothing

idUriForContext
  :: StableIdMapping
  -> ContextUri Readable
  -> Maybe String
idUriForContext m cid@(ContextUri ctxFqn) = do
  let (namespaceCtxFqn :: String) = typeUri2typeNameSpace_ ctxFqn
  -- if the ctxFqn is the root context (no namespace), then return the model identifier directly -
  -- but only if it is indeed a context in the model. Otherwise we might be dealing with a changed model name.
  if namespaceCtxFqn == ctxFqn then case lookupContextCuid m cid of
    Just v -> Just $ unwrap m.modelIdentifier
    Nothing -> Nothing
  else
    do
      (namespaceTid :: String) <- idUriForContext m (ContextUri namespaceCtxFqn)
      (localTid :: String) <- lookupContextCuid m cid
      pure $ namespaceTid <> "$" <> localTid

idUriForRole :: StableIdMapping -> RoleUri Readable -> Maybe String
idUriForRole m rid@(RoleUri roleFqn) = do
  let ctxFqn = typeUri2typeNameSpace_ roleFqn
  ctxTid <- idUriForContext m (ContextUri ctxFqn)
  rolTid <- lookupRoleCuid m rid
  pure (ctxTid <> "$" <> rolTid)

idUriForProperty :: StableIdMapping -> PropertyUri Readable -> Maybe String
idUriForProperty m (PropertyUri propFqn) = do
  let roleFqn = typeUri2typeNameSpace_ propFqn
  rolTid <- idUriForRole m (RoleUri roleFqn)
  propTid <- lookupPropertyCuid m (PropertyUri propFqn)
  pure (rolTid <> "$" <> propTid)

-- Views are declared under a role; build their identifier by role tid + view cuid.
idUriForView :: StableIdMapping -> ViewUri Readable -> Maybe String
idUriForView m (ViewUri viewFqn) = do
  let roleFqn = typeUri2typeNameSpace_ viewFqn
  rolTid <- idUriForRole m (RoleUri roleFqn)
  viewTid <- lookupViewCuid m (ViewUri viewFqn)
  pure (rolTid <> "$" <> viewTid)

-- States can be declared under a context or a role. Try role first; fall back to context.
idUriForState :: StableIdMapping -> StateUri Readable -> Maybe String
idUriForState m (StateUri stateFqn) =
  -- The simplest case is the root state of a context of role. In that case, we can exchange the fqn 
  -- for the ContextUri Stable or RoleUri Stable respectively, augmented with the cuid for the state itself.
  let
    nsFqn = typeUri2typeNameSpace_ stateFqn
    -- If stateFqn is the root state of a role, then the stable identifier is simply the stable identifier of that role.
    -- We know it is the root state of a role if we can find a role with exactly the same fqn.
    tryRole = idUriForRole m (RoleUri stateFqn)
    -- tryRole = do
    --   rolTid <- idUriForRole m (RoleUri stateFqn)
    --   -- Als er een rolTid is, dan is stateFqn een root state. Dat is in dat geval tevens de stable identifier van deze state!
    --   stTid <- lookupStateCuid m (StateUri stateFqn)
    --   pure (rolTid <> "$" <> stTid)
    -- Similar reasoning for context root states.
    tryContext = idUriForContext m (ContextUri stateFqn)
  -- tryContext = do
  --   ctxTid <- idUriForContext m (ContextUri stateFqn)
  --   stTid <- lookupStateCuid m (StateUri stateFqn)
  --   pure (ctxTid <> "$" <> stTid)
  in
    case tryRole, tryContext of
      Just v, _ -> Just v
      _, Just v -> Just v
      Nothing, Nothing -> do
        stTid <- lookupStateCuid m (StateUri stateFqn)
        nameSpaceTid <- idUriForState m (StateUri nsFqn)
        pure (nameSpaceTid <> "$" <> stTid)

-- Actions are declared under a role, partitioned by a state. Build ID by role tid + state cuid + action cuid.
-- The readable FQN uses $ separators: <roleFqn>$<stateLocal>$<actionLocal> (or nested state path). We rely on cuid lookups.
idUriForAction :: StableIdMapping -> ActionUri Readable -> Maybe String
idUriForAction m (ActionUri actionFqn) = do
  -- The namespace of an action is the parent state FQN; its namespace is the role FQN.
  -- We'll derive role tid via the parent role FQN, then append state cuid and action cuid.
  let stateFqn = typeUri2typeNameSpace_ actionFqn
  stateTid <- idUriForState m (StateUri stateFqn)
  actTid <- lookupActionCuid m (ActionUri actionFqn)
  pure (stateTid <> "$" <> actTid)

