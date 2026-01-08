-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2025 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Sidecar.StableIdMapping
  ( ActionKeySnapshot
  , ContextIndividualKeySnapshot
  , ContextKeySnapshot
  , PropertyKeySnapshot
  , RoleIndividualKeySnapshot
  , RoleKeySnapshot
  , StableIdMapping
  , StableIdMappingWithoutIndividuals
  , StateKeySnapshot
  , ViewKeySnapshot
  , emptyStableIdMapping
  , fromLocalModels
  , fromRepository
  , idUriForAction
  , idUriForContext
  , idUriForProperty
  , idUriForRole
  , idUriForState
  , idUriForView
  , loadStableMapping
  , loadStableMapping_
  , lookupActionCuid
  , lookupContextCuid
  , lookupContextIndividualId
  , lookupPropertyCuid
  , lookupRoleCuid
  , lookupRoleIndividualId
  , lookupStateCuid
  , lookupViewCuid
  , module Perspectives.SideCar.PhantomTypedNewtypes
  ) where

import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object, empty)
import Foreign.Object as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (modelUri2ModelUrl, typeUri2LocalName_, typeUri2typeNameSpace_)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (fromBlob, getAttachment)
import Perspectives.Persistent (modelDatabaseName)
import Perspectives.SideCar.PhantomTypedNewtypes (ActionUri(..), ContextUri(..), ModelUri(..), PropertyUri(..), Readable, RoleUri(..), Stable, StateUri(..), ViewUri(..))
import Prelude (bind, pure, ($), (<>), (==), (>>=), (*>))
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
-- New v5 action snapshot: actions live under a role; store local name and QFD hash
type ActionKeySnapshot =
  { fqn :: String
  , declaringRoleFqn :: String
  , localName :: String
  , qfdHash :: String
  }

-- Legacy v4 action snapshot (state-based). Used for upgrade on load.
type ActionKeySnapshotV4 =
  { fqn :: String
  , declaringRoleFqn :: String
  , declaringStateFqn :: String
  }

-- Indexed individuals snapshots: capture the parent scope and the readable name
type ContextIndividualKeySnapshot =
  { contextFqn :: String
  , name :: String
  }

type RoleIndividualKeySnapshot =
  { roleFqn :: String
  , name :: String
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
  -- Indexed individual key snapshots (parent scope + name)
  , contextIndividualKeys :: Object ContextIndividualKeySnapshot
  , roleIndividualKeys :: Object RoleIndividualKeySnapshot
  -- Stable ids per kind (FQN -> CUID). Canonical entries should have these.
  , contextCuids :: Object String
  , roleCuids :: Object String
  , propertyCuids :: Object String
  , viewCuids :: Object String
  , stateCuids :: Object String
  , actionCuids :: Object String
  -- Indexed individuals within this model (readable name -> stable instance id)
  , contextIndividuals :: Object String
  , roleIndividuals :: Object String
  , modelIdentifier :: ModelUri Stable
  }

emptyStableIdMapping :: StableIdMapping
emptyStableIdMapping =
  { version: 5
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
  , contextIndividualKeys: empty
  , roleIndividualKeys: empty
  , contextCuids: empty
  , roleCuids: empty
  , propertyCuids: empty
  , viewCuids: empty
  , stateCuids: empty
  , actionCuids: empty
  , contextIndividuals: empty
  , roleIndividuals: empty
  , modelIdentifier: ModelUri ""
  }

-- Backward-compatible shape (e.g. existing stored sidecars without individuals maps)
type StableIdMappingWithoutIndividuals =
  { version :: Int
  , contexts :: Object String
  , roles :: Object String
  , properties :: Object String
  , views :: Object String
  , states :: Object String
  , actions :: Object String
  , contextKeys :: Object ContextKeySnapshot
  , roleKeys :: Object RoleKeySnapshot
  , propertyKeys :: Object PropertyKeySnapshot
  , viewKeys :: Object ViewKeySnapshot
  , stateKeys :: Object StateKeySnapshot
  , actionKeys :: Object ActionKeySnapshot
  -- Older sidecars won't have individual key snapshots
  , contextCuids :: Object String
  , roleCuids :: Object String
  , propertyCuids :: Object String
  , viewCuids :: Object String
  , stateCuids :: Object String
  , actionCuids :: Object String
  , modelIdentifier :: ModelUri Stable
  }

upgradeWithoutIndividuals :: StableIdMappingWithoutIndividuals -> StableIdMapping
upgradeWithoutIndividuals m =
  { version: m.version
  , contexts: m.contexts
  , roles: m.roles
  , properties: m.properties
  , views: m.views
  , states: m.states
  , actions: m.actions
  , contextKeys: m.contextKeys
  , roleKeys: m.roleKeys
  , propertyKeys: m.propertyKeys
  , viewKeys: m.viewKeys
  , stateKeys: m.stateKeys
  , actionKeys: m.actionKeys
  , contextIndividualKeys: empty
  , roleIndividualKeys: empty
  , contextCuids: m.contextCuids
  , roleCuids: m.roleCuids
  , propertyCuids: m.propertyCuids
  , viewCuids: m.viewCuids
  , stateCuids: m.stateCuids
  , actionCuids: m.actionCuids
  , contextIndividuals: empty
  , roleIndividuals: empty
  , modelIdentifier: m.modelIdentifier
  }

-- Strict v4 (pre-localName/qfdHash) mapping used for upgrade
type StableIdMappingV4 =
  { version :: Int
  , contexts :: Object String
  , roles :: Object String
  , properties :: Object String
  , views :: Object String
  , states :: Object String
  , actions :: Object String
  , contextKeys :: Object ContextKeySnapshot
  , roleKeys :: Object RoleKeySnapshot
  , propertyKeys :: Object PropertyKeySnapshot
  , viewKeys :: Object ViewKeySnapshot
  , stateKeys :: Object StateKeySnapshot
  , actionKeys :: Object ActionKeySnapshotV4
  , contextIndividualKeys :: Object ContextIndividualKeySnapshot
  , roleIndividualKeys :: Object RoleIndividualKeySnapshot
  , contextCuids :: Object String
  , roleCuids :: Object String
  , propertyCuids :: Object String
  , viewCuids :: Object String
  , stateCuids :: Object String
  , actionCuids :: Object String
  , contextIndividuals :: Object String
  , roleIndividuals :: Object String
  , modelIdentifier :: ModelUri Stable
  }

upgradeV4 :: StableIdMappingV4 -> StableIdMapping
upgradeV4 m =
  let
    -- convert legacy action snapshots to v5
    toV5 :: ActionKeySnapshotV4 -> ActionKeySnapshot
    toV5 s =
      let
        ln = typeUri2LocalName_ s.fqn
      in
        { fqn: s.fqn
        , declaringRoleFqn: s.declaringRoleFqn
        , localName: ln
        , qfdHash: "" -- unknown from legacy sidecar
        }
  in
    { version: 5
    , contexts: m.contexts
    , roles: m.roles
    , properties: m.properties
    , views: m.views
    , states: m.states
    , actions: m.actions
    , contextKeys: m.contextKeys
    , roleKeys: m.roleKeys
    , propertyKeys: m.propertyKeys
    , viewKeys: m.viewKeys
    , stateKeys: m.stateKeys
    , actionKeys: OBJ.fromFoldable do
        k <- OBJ.keys m.actionKeys
        case OBJ.lookup k m.actionKeys of
          Nothing -> []
          Just s -> [ Tuple k (toV5 s) ]
    , contextIndividualKeys: m.contextIndividualKeys
    , roleIndividualKeys: m.roleIndividualKeys
    , contextCuids: m.contextCuids
    , roleCuids: m.roleCuids
    , propertyCuids: m.propertyCuids
    , viewCuids: m.viewCuids
    , stateCuids: m.stateCuids
    , actionCuids: m.actionCuids
    , contextIndividuals: m.contextIndividuals
    , roleIndividuals: m.roleIndividuals
    , modelIdentifier: m.modelIdentifier
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

fromRepository :: Boolean
fromRepository = true

fromLocalModels :: Boolean
fromLocalModels = false

-- | Load the stable ID mapping sidecar for a given model (if any), from the Repository or from the local models.
-- | When taken from the repository, ModelUri should include a version number.
loadStableMapping :: ModelUri Stable -> Boolean -> MonadPerspectives (Maybe StableIdMapping)
loadStableMapping (ModelUri domeinFileName) fromRepo = do
  let split = unsafePartial modelUri2ModelUrl domeinFileName
  source <- if fromRepo then pure split.repositoryUrl else modelDatabaseName
  mBlob <- getAttachment source split.documentName "stableIdMapping.json"
  case mBlob of
    Nothing -> pure Nothing
    Just blob -> do
      txt <- liftAff $ fromBlob blob
      pure $ case readJSON txt of
        Right (m :: StableIdMapping) -> Just m
        _ -> case readJSON txt of
          Right (m0 :: StableIdMappingWithoutIndividuals) -> Just (upgradeWithoutIndividuals m0)
          _ -> case readJSON txt of
            Right (mv4 :: StableIdMappingV4) -> Just (upgradeV4 mv4)
            _ -> Nothing

loadStableMapping_ :: String -> String -> MonadPerspectives (Maybe StableIdMapping)
loadStableMapping_ database documentName = do
  getAttachment database documentName "stableIdMapping.json" >>= case _ of
    Just f -> do
      x <- liftAff $ fromBlob f
      case readJSON x of
        Left e -> logPerspectivesError (Custom $ "loadStableMapping_" <> show e) *> pure Nothing
        Right sq -> pure $ Just sq
    Nothing -> pure Nothing

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

-- Actions are declared under a role. Build ID by role tid + action cuid.
-- The readable FQN uses $ separators: <roleFqn>$<actionLocal>. We rely on cuid lookups.
idUriForAction :: StableIdMapping -> ActionUri Readable -> Maybe String
idUriForAction m (ActionUri actionFqn) = do
  -- The namespace of an action is the parent role FQN in v5.
  let roleFqn = typeUri2typeNameSpace_ actionFqn
  rolTid <- idUriForRole m (RoleUri roleFqn)
  actTid <- lookupActionCuid m (ActionUri actionFqn)
  pure (rolTid <> "$" <> actTid)

-- Lookup helpers for indexed individuals
lookupContextIndividualId :: StableIdMapping -> String -> Maybe String
lookupContextIndividualId m ident = OBJ.lookup ident m.contextIndividuals

lookupRoleIndividualId :: StableIdMapping -> String -> Maybe String
lookupRoleIndividualId m ident = OBJ.lookup ident m.roleIndividuals

