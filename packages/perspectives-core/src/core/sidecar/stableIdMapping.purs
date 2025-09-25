-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2025 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Sidecar.StableIdMapping
  ( ContextKeySnapshot
  , DomeinFileIdF(..)
  , ModelUri(..)
  , PropertyKeySnapshot
  , ViewKeySnapshot
  , StateKeySnapshot
  , Readable
  , RoleKeySnapshot
  , Stable
  , StableIdMapping
  , emptyStableIdMapping
  , idUriForContext
  , idUriForProperty
  , idUriForRole
  , idUriForView
  , idUriForState
  , loadStableMapping
  , lookupContextCuid
  , lookupViewCuid
  , lookupStateCuid
  , lookupPropertyCuid
  , lookupRoleCuid
  , PropertyUri(..)
  , RoleUri(..)
  , ContextUri(..)
  , ViewUri(..)
  , StateUri(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object, empty)
import Foreign.Object as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Identifiers (modelUri2ModelUrl, typeUri2typeNameSpace_)
import Perspectives.Persistence.API (fromBlob, getAttachment)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON)

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

type StableIdMapping =
  { version :: Int
  , contexts :: Object String
  , roles :: Object String
  , properties :: Object String
  , views :: Object String
  , states :: Object String
  -- Baseline canonical keys for heuristics on the next compile.
  , contextKeys :: Object ContextKeySnapshot
  , roleKeys :: Object RoleKeySnapshot
  , propertyKeys :: Object PropertyKeySnapshot
  , viewKeys :: Object ViewKeySnapshot
  , stateKeys :: Object StateKeySnapshot
  -- Stable ids per kind (FQN -> CUID). Canonical entries should have these.
  , contextCuids :: Object String
  , roleCuids :: Object String
  , propertyCuids :: Object String
  , viewCuids :: Object String
  , stateCuids :: Object String
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
  , contextKeys: empty
  , roleKeys: empty
  , propertyKeys: empty
  , viewKeys: empty
  , stateKeys: empty
  , contextCuids: empty
  , roleCuids: empty
  , propertyCuids: empty
  , viewCuids: empty
  , stateCuids: empty
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

-- Phantom tag to distinguish shapes at the type level
foreign import data Readable :: Type
foreign import data Stable :: Type

newtype ModelUri :: Type -> Type
-- URIs tagged with their flavor (phantom parameter)
newtype ModelUri f = ModelUri String

derive instance newtypeModelUri :: Newtype (ModelUri f) _
derive newtype instance eqModelUri :: Eq (ModelUri f)
derive newtype instance ordModelUri :: Ord (ModelUri f)
derive newtype instance showModelUri :: Show (ModelUri f)
derive newtype instance ReadForeign (ModelUri f)
derive newtype instance WriteForeign (ModelUri f)

newtype ContextUri :: forall k. k -> Type
newtype ContextUri f = ContextUri String

derive instance newtypeContextUri :: Newtype (ContextUri f) _
derive newtype instance eqContextUri :: Eq (ContextUri f)
derive newtype instance ordContextUri :: Ord (ContextUri f)
derive newtype instance showContextUri :: Show (ContextUri f)
derive newtype instance ReadForeign (ContextUri f)
derive newtype instance WriteForeign (ContextUri f)

newtype RoleUri :: forall k. k -> Type
newtype RoleUri f = RoleUri String

derive instance newtypeRoleUri :: Newtype (RoleUri f) _
derive newtype instance eqRoleUri :: Eq (RoleUri f)
derive newtype instance ordRoleUri :: Ord (RoleUri f)
derive newtype instance showRoleUri :: Show (RoleUri f)
derive newtype instance ReadForeign (RoleUri f)
derive newtype instance WriteForeign (RoleUri f)

newtype PropertyUri :: forall k. k -> Type
newtype PropertyUri f = PropertyUri String

derive instance newtypePropertyUri :: Newtype (PropertyUri f) _
derive newtype instance eqPropertyUri :: Eq (PropertyUri f)
derive newtype instance ordPropertyUri :: Ord (PropertyUri f)
derive newtype instance showPropertyUri :: Show (PropertyUri f)
derive newtype instance ReadForeign (PropertyUri f)
derive newtype instance WriteForeign (PropertyUri f)

newtype ViewUri :: forall k. k -> Type
newtype ViewUri f = ViewUri String

derive instance newtypeViewUri :: Newtype (ViewUri f) _
derive newtype instance eqViewUri :: Eq (ViewUri f)
derive newtype instance ordViewUri :: Ord (ViewUri f)
derive newtype instance showViewUri :: Show (ViewUri f)
derive newtype instance ReadForeign (ViewUri f)
derive newtype instance WriteForeign (ViewUri f)

newtype StateUri :: forall k. k -> Type
newtype StateUri f = StateUri String

derive instance newtypeStateUri :: Newtype (StateUri f) _
derive newtype instance eqStateUri :: Eq (StateUri f)
derive newtype instance ordStateUri :: Ord (StateUri f)
derive newtype instance showStateUri :: Show (StateUri f)
derive newtype instance ReadForeign (StateUri f)
derive newtype instance WriteForeign (StateUri f)

-- Typed DomeinFileId (you can migrate fields to DomeinFileIdF Stable stepwise)
newtype DomeinFileIdF :: forall k. k -> Type
newtype DomeinFileIdF f = DomeinFileIdF String

derive instance newtypeDomeinFileIdF :: Newtype (DomeinFileIdF f) _
derive newtype instance eqDomeinFileIdF :: Eq (DomeinFileIdF f)
derive newtype instance ordDomeinFileIdF :: Ord (DomeinFileIdF f)
derive newtype instance showDomeinFileIdF :: Show (DomeinFileIdF f)
derive newtype instance ReadForeign (DomeinFileIdF f)
derive newtype instance WriteForeign (DomeinFileIdF f)
