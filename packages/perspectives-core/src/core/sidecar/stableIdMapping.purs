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
  ) where

import Foreign.Object (Object, empty)
import Foreign.Object as OBJ
import Data.Maybe (Maybe(..))

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
