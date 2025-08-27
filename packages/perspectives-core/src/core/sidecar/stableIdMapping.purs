-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2025 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Sidecar.StableIdMapping
  ( StableIdMapping
  , ContextKeySnapshot, RoleKeySnapshot, PropertyKeySnapshot
  , emptyStableIdMapping
  , lookupContextCuid
  , lookupRoleCuid
  , lookupPropertyCuid
  , loadStableMapping
  , idUriForContext
  , idUriForRole
  , idUriForProperty
  ) where

import Prelude

import Data.Array (foldl, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String as Str
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Foreign.Object (Object, empty)
import Foreign.Object as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Identifiers (DomeinFileName, modelUri2ModelUrl, typeUri2typeNameSpace_)
import Perspectives.Persistence.API (fromBlob, getAttachment)
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

type StableIdMapping =
  { version :: Int
  , contexts :: Object String
  , roles :: Object String
  , properties :: Object String
  -- Baseline canonical keys for heuristics on the next compile.
  , contextKeys :: Object ContextKeySnapshot
  , roleKeys :: Object RoleKeySnapshot
  , propertyKeys :: Object PropertyKeySnapshot
  -- Stable ids per kind (FQN -> CUID). Canonical entries should have these.
  , contextCuids :: Object String
  , roleCuids :: Object String
  , propertyCuids :: Object String
  }

emptyStableIdMapping :: StableIdMapping
emptyStableIdMapping =
  { version: 2
  , contexts: empty
  , roles: empty
  , properties: empty
  , contextKeys: empty
  , roleKeys: empty
  , propertyKeys: empty
  , contextCuids: empty
  , roleCuids: empty
  , propertyCuids: empty
  }

-- Resolve a CUID for an FQN:
-- 1) Try direct (legacy keys can still exist in *Cuids).
-- 2) Resolve via alias map to canonical, then lookup in *Cuids.
lookupContextCuid :: StableIdMapping -> String -> Maybe String
lookupContextCuid m fqn =
  case OBJ.lookup fqn m.contextCuids of
    Just v -> Just v
    Nothing -> case OBJ.lookup fqn m.contexts of
      Just canonical -> OBJ.lookup canonical m.contextCuids
      Nothing -> Nothing

lookupRoleCuid :: StableIdMapping -> String -> Maybe String
lookupRoleCuid m fqn =
  case OBJ.lookup fqn m.roleCuids of
    Just v -> Just v
    Nothing -> case OBJ.lookup fqn m.roles of
      Just canonical -> OBJ.lookup canonical m.roleCuids
      Nothing -> Nothing

lookupPropertyCuid :: StableIdMapping -> String -> Maybe String
lookupPropertyCuid m fqn =
  case OBJ.lookup fqn m.propertyCuids of
    Just v -> Just v
    Nothing -> case OBJ.lookup fqn m.properties of
      Just canonical -> OBJ.lookup canonical m.propertyCuids
      Nothing -> Nothing

loadStableMapping :: DomeinFileName -> MonadPerspectives (Maybe StableIdMapping)
loadStableMapping domeinFileName = do
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
  :: String                 -- modelUri: "model://domain#mid"
  -> StableIdMapping
  -> String                 -- context FQN (canonical)
  -> Maybe String
idUriForContext modelUri m ctxFqn =
  let
    parts = split (Pattern "$") ctxFqn
  in case uncons parts of
    Nothing -> Nothing
    Just {head:base, tail:rest} ->
      -- Build ancestor FQNs: base$seg1, base$seg1$seg2, ..., base$...$segN
      let
        Tuple _ pathFqns =
          foldl
            (\(Tuple cur acc) seg ->
               let next = cur <> "$" <> seg
               in Tuple next (acc <> [ next ])
            )
            (Tuple base [])
            rest
      in do
        tids <- traverse (lookupContextCuid m) pathFqns
        pure $ modelUri <> "$" <> Str.joinWith "$" tids

idUriForRole :: String -> StableIdMapping -> String -> Maybe String
idUriForRole modelUri m roleFqn = do
  let ctxFqn = typeUri2typeNameSpace_ roleFqn
  ctxTid <- lookupContextCuid m ctxFqn
  rolTid <- lookupRoleCuid m roleFqn
  pure (modelUri <> "$" <> ctxTid <> "$" <> rolTid)

idUriForProperty :: String -> StableIdMapping -> String -> Maybe String
idUriForProperty modelUri m propFqn = do
  let roleFqn = typeUri2typeNameSpace_ propFqn
  let ctxFqn  = typeUri2typeNameSpace_ roleFqn
  ctxTid <- lookupContextCuid m ctxFqn
  rolTid <- lookupRoleCuid m roleFqn
  propTid <- lookupPropertyCuid m propFqn
  pure (modelUri <> "$" <> ctxTid <> "$" <> rolTid <> "$" <> propTid)
