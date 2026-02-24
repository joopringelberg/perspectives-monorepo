-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

-- | Data migration from the old format (deltas embedded in PerspectContext/PerspectRol)
-- | to the new format (deltas stored in the dedicated DeltaStore).
-- |
-- | This module reads all entity documents from the entities database,
-- | extracts the old embedded SignedDelta fields, and stores them **as-is** in the
-- | DeltaStore — preserving original authors and signatures.
-- |
-- | The old `encryptedDelta` strings do not contain `resourceKey`/`resourceVersion`.
-- | The DeltaStoreRecord wrapper carries those fields at the record level for
-- | indexing and range queries. Consequently, gap detection will not work for these
-- | migrated deltas; this is acceptable because the current state is assumed correct.
-- |
-- | Sentinel deltas (from `defaultContextRecord` / `defaultRolRecord`) are skipped.
-- |
-- | The old delta fields (universeContextDelta, universeRoleDelta, contextDelta,
-- | bindingDelta, propertyDeltas) are effectively removed from the documents when
-- | they are re-saved, since the current PerspectContext/PerspectRol types no
-- | longer include those fields.
-- |
-- | See design/deterministic-delta-ordering.md for the full design.
module Perspectives.DataUpgrade.DeltasMigration
  ( migrateDeltasToStore
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (length, mapWithIndex)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), snd)
import Effect.Class.Console (log)
import Foreign (Foreign)
import Foreign.Object as F
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Persistence.API (documentsInDatabase, includeDocs)
import Perspectives.Persistence.DeltaStore (DeltaStoreRecord(..), deltaStoreDocId, storeDelta)
import Perspectives.Persistence.ResourceVersionStore (setResourceVersion)
import Perspectives.Persistent (entitiesDatabaseName, saveEntiteit_)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Simple.JSON (class ReadForeign, readJSON', read')

-----------------------------------------------------------
-- OLD DOCUMENT TYPES
-----------------------------------------------------------
-- | These types match the pre-migration document format which included
-- | embedded delta fields. Using Simple.JSON's read', extra fields
-- | in the JSON are ignored, so we only list the fields we need.

-- | Old context document format, including the embedded UniverseContextDelta.
type OldContextDoc =
  { id :: ContextInstance
  , universeContextDelta :: SignedDelta
  }

-- | Old role document format, including all embedded delta fields.
type OldRolDoc =
  { id :: RoleInstance
  , pspType :: EnumeratedRoleType
  , context :: ContextInstance
  , universeRoleDelta :: SignedDelta
  , contextDelta :: SignedDelta
  , bindingDelta :: Maybe SignedDelta
  , propertyDeltas :: F.Object (F.Object SignedDelta)
  }

-----------------------------------------------------------
-- DELTA TYPE EXTRACTION
-----------------------------------------------------------

-- | Minimal record for extracting just the deltaType from an encryptedDelta string.
type DeltaTypeOnly = { deltaType :: String }

-- | Try to extract the deltaType string from an encryptedDelta JSON string.
-- | Returns "unknown" if parsing fails (e.g. for sentinel values).
extractDeltaType :: String -> String
extractDeltaType encryptedDelta = case runExcept (readJSON' encryptedDelta) of
  Right (r :: DeltaTypeOnly) -> r.deltaType
  Left _ -> "unknown"

-----------------------------------------------------------
-- SENTINEL DETECTION
-----------------------------------------------------------

-- | Check whether a SignedDelta is a sentinel/default value rather than a real delta.
-- | Old default records had empty author and placeholder encryptedDelta strings:
-- |   - "UniverseContextDelta from defaultContextRecord"
-- |   - "UniverseRoleDelta from defaultRolRecord"
-- |   - "ContextDelta from defaultRolRecord"
isSentinel :: SignedDelta -> Boolean
isSentinel (SignedDelta { author, encryptedDelta }) =
  unwrap author == ""
    || encryptedDelta == "UniverseContextDelta from defaultContextRecord"
    || encryptedDelta == "UniverseRoleDelta from defaultRolRecord"
    || encryptedDelta == "ContextDelta from defaultRolRecord"

-----------------------------------------------------------
-- MIGRATION ENTRY POINT
-----------------------------------------------------------

-- | Migrate all entities from the old format (deltas embedded in resources)
-- | to the new format (deltas stored in DeltaStore with resource versioning).
-- |
-- | This function:
-- | 1. Reads all documents from the entities database
-- | 2. Extracts old SignedDelta fields and stores them as-is in the DeltaStore
-- | 3. Assigns resourceKey/resourceVersion at the DeltaStoreRecord level only
-- | 4. Initialises ResourceVersionStore with the highest version per resource-key
-- | 5. Re-saves each entity to strip old delta fields from the document
-- |
-- | Runs in MonadPerspectives (no transaction needed since we don't re-sign).
migrateDeltasToStore :: MonadPerspectives Unit
migrateDeltasToStore = do
  dbName <- entitiesDatabaseName
  { rows } <- documentsInDatabase dbName includeDocs
  log ("DeltasMigration: processing " <> show (length rows) <> " entity documents")
  for_ rows \{ doc } -> case doc of
    Nothing -> pure unit
    Just foreignDoc -> migrateDocument foreignDoc

-- | Try to interpret a Foreign document as an old context or old role document,
-- | extract its embedded deltas, store them in the DeltaStore, and re-save the
-- | entity to strip old fields.
migrateDocument :: Foreign -> MonadPerspectives Unit
migrateDocument foreignDoc = do
  -- Try as an old context (has universeContextDelta).
  case runExcept (read' foreignDoc) of
    Right (oldCtx :: OldContextDoc) | not (isSentinel oldCtx.universeContextDelta) -> do
      migrateContextDeltas oldCtx
      -- Re-save using current type to strip old fields
      case runExcept (read' foreignDoc) of
        Right (ctx :: PerspectContext) -> void $ saveEntiteit_ (unwrap ctx).id ctx
        Left _ -> pure unit
    _ ->
      -- Try as an old role (has universeRoleDelta).
      case runExcept (read' foreignDoc) of
        Right (oldRol :: OldRolDoc) | not (isSentinel oldRol.universeRoleDelta) -> do
          migrateRolDeltas oldRol
          -- Re-save using current type to strip old fields
          case runExcept (read' foreignDoc) of
            Right (rol :: PerspectRol) -> void $ saveEntiteit_ (unwrap rol).id rol
            Left _ -> pure unit
        _ -> pure unit -- Not a context or role document, or has only sentinel deltas. Skip.

-----------------------------------------------------------
-- CONTEXT MIGRATION
-----------------------------------------------------------

-- | Store the old UniverseContextDelta for a context in the DeltaStore.
migrateContextDeltas :: OldContextDoc -> MonadPerspectives Unit
migrateContextDeltas { id: contextId, universeContextDelta } = do
  let resourceKey = unwrap contextId
  let (SignedDelta { author, encryptedDelta }) = universeContextDelta
  let deltaType = extractDeltaType encryptedDelta
  storeDelta $ DeltaStoreRecord
    { _id: deltaStoreDocId resourceKey 0 author
    , _rev: Nothing
    , resourceKey
    , resourceVersion: 0
    , author
    , signedDelta: universeContextDelta
    , deltaType
    , applied: true
    }
  setResourceVersion resourceKey 0

-----------------------------------------------------------
-- ROLE MIGRATION
-----------------------------------------------------------

-- | Store all old deltas for a role in the DeltaStore.
-- | Version assignment per resource-key:
-- |   roleId:             universeRoleDelta=0, contextDelta=1
-- |   roleId#binding:     bindingDelta=0
-- |   roleId#<propType>:  each propertyDelta=0 (or sequential if multiple per property)
migrateRolDeltas :: OldRolDoc -> MonadPerspectives Unit
migrateRolDeltas { id: roleId, universeRoleDelta, contextDelta, bindingDelta, propertyDeltas } = do
  let resourceKey = unwrap roleId
  let (SignedDelta urd) = universeRoleDelta

  -- 1. UniverseRoleDelta at version 0
  storeDelta $ DeltaStoreRecord
    { _id: deltaStoreDocId resourceKey 0 urd.author
    , _rev: Nothing
    , resourceKey
    , resourceVersion: 0
    , author: urd.author
    , signedDelta: universeRoleDelta
    , deltaType: extractDeltaType urd.encryptedDelta
    , applied: true
    }

  -- 2. ContextDelta at version 1 (if not a sentinel)
  when (not $ isSentinel contextDelta) do
    let (SignedDelta cd) = contextDelta
    storeDelta $ DeltaStoreRecord
      { _id: deltaStoreDocId resourceKey 1 cd.author
      , _rev: Nothing
      , resourceKey
      , resourceVersion: 1
      , author: cd.author
      , signedDelta: contextDelta
      , deltaType: extractDeltaType cd.encryptedDelta
      , applied: true
      }

  -- Set the resource version for the role (highest assigned = 1 if contextDelta was stored, else 0)
  if not (isSentinel contextDelta) then setResourceVersion resourceKey 1
  else setResourceVersion resourceKey 0

  -- 3. BindingDelta (if present and not sentinel)
  case bindingDelta of
    Nothing -> pure unit
    Just bd | isSentinel bd -> pure unit
    Just bd -> do
      let bindingResourceKey = resourceKey <> "#binding"
      let (SignedDelta bdr) = bd
      storeDelta $ DeltaStoreRecord
        { _id: deltaStoreDocId bindingResourceKey 0 bdr.author
        , _rev: Nothing
        , resourceKey: bindingResourceKey
        , resourceVersion: 0
        , author: bdr.author
        , signedDelta: bd
        , deltaType: extractDeltaType bdr.encryptedDelta
        , applied: true
        }
      setResourceVersion bindingResourceKey 0

  -- 4. PropertyDeltas
  -- Old format: Object (Object SignedDelta) where first key is property type,
  -- second key is the value string. Each entry is a SignedDelta.
  for_ (F.toUnfoldable propertyDeltas :: Array (Tuple String (F.Object SignedDelta)))
    \(Tuple propType innerObj) -> do
      let propResourceKey = resourceKey <> "#" <> propType
      -- Assign sequential versions to multiple deltas for the same property
      void $ migratePropertyDeltas propResourceKey (F.toUnfoldable innerObj :: Array (Tuple String SignedDelta))

-- | Store property deltas for a single property resource-key, assigning sequential versions.
-- | Returns the highest version assigned.
migratePropertyDeltas :: String -> Array (Tuple String SignedDelta) -> MonadPerspectives Int
migratePropertyDeltas propResourceKey deltas = do
  -- Extract just the SignedDeltas (drop the key strings) and assign sequential version numbers.
  let signedDeltas = map snd deltas
  let indexed = indexFromZero signedDeltas
  for_ indexed \{ version, signedDelta: sd } -> do
    when (not $ isSentinel sd) do
      let (SignedDelta { author, encryptedDelta }) = sd
      storeDelta $ DeltaStoreRecord
        { _id: deltaStoreDocId propResourceKey version author
        , _rev: Nothing
        , resourceKey: propResourceKey
        , resourceVersion: version
        , author
        , signedDelta: sd
        , deltaType: extractDeltaType encryptedDelta
        , applied: true
        }
  let maxVersion = if length deltas > 0 then length deltas - 1 else 0
  setResourceVersion propResourceKey maxVersion
  pure maxVersion

-----------------------------------------------------------
-- HELPERS
-----------------------------------------------------------

-- | Pair each element with its 0-based index.
indexFromZero :: forall a. Array a -> Array { version :: Int, signedDelta :: a }
indexFromZero = mapWithIndex (\i a -> { version: i, signedDelta: a })
