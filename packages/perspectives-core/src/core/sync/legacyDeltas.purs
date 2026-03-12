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

-- | Legacy delta types for backwards compatibility with the master branch serialisation format.
-- | The old DeltaRecord has only { subject :: RoleType | f } (no resourceKey, resourceVersion).
-- | The old UniverseRoleDelta has roleInstances :: SerializableNonEmptyArray RoleInstance
-- | instead of roleInstance :: RoleInstance.
-- |
-- | These types are read-only (ReadForeign only). They are converted to the new types
-- | with default values for the missing fields (resourceKey = "", resourceVersion = 0).
-- | Old deltas skip verification and are executed directly.

module Perspectives.Sync.LegacyDeltas
  ( LegacyDeltaRecord
  , LegacyUniverseContextDelta(..)
  , LegacyUniverseRoleDelta(..)
  , LegacyContextDelta(..)
  , LegacyRoleBindingDelta(..)
  , LegacyRolePropertyDelta(..)
  , toUniverseContextDelta
  , toUniverseRoleDelta
  , toContextDelta
  , toRoleBindingDelta
  , toRolePropertyDelta
  , extractLegacyResourceKey
  ) where

import Prelude (($), (<>))

import Control.Monad.Except (runExcept)
import Data.Array.NonEmpty (head) as NEA
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, RoleType)
import Perspectives.SerializableNonEmptyArray (SerializableNonEmptyArray, toNonEmptyArray)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType, RoleBindingDelta(..), RoleBindingDeltaType, RolePropertyDelta(..), RolePropertyDeltaType, UniverseContextDelta(..), UniverseContextDeltaType, UniverseRoleDelta(..), UniverseRoleDeltaType)
import Simple.JSON (class ReadForeign, readJSON')

-----------------------------------------------------------
-- LEGACY DELTARECORD (no resourceKey, no resourceVersion)
-----------------------------------------------------------
type LegacyDeltaRecord f = { subject :: RoleType | f }

-----------------------------------------------------------
-- LEGACY UNIVERSECONTEXTDELTA
-----------------------------------------------------------
newtype LegacyUniverseContextDelta = LegacyUniverseContextDelta
  ( LegacyDeltaRecord
      ( id :: ContextInstance
      , contextType :: ContextType
      , deltaType :: UniverseContextDeltaType
      )
  )

derive instance newtypeLegacyUniverseContextDelta :: Newtype LegacyUniverseContextDelta _
derive newtype instance ReadForeign LegacyUniverseContextDelta

toUniverseContextDelta :: LegacyUniverseContextDelta -> UniverseContextDelta
toUniverseContextDelta (LegacyUniverseContextDelta r) = UniverseContextDelta
  { subject: r.subject
  , resourceKey: ""
  , resourceVersion: 0
  , id: r.id
  , contextType: r.contextType
  , deltaType: r.deltaType
  }

-----------------------------------------------------------
-- LEGACY UNIVERSEROLEDELTA
-- Note: roleInstances (array) instead of roleInstance (single)
-----------------------------------------------------------
newtype LegacyUniverseRoleDelta = LegacyUniverseRoleDelta
  ( LegacyDeltaRecord
      ( id :: ContextInstance
      , contextType :: ContextType
      , roleType :: EnumeratedRoleType
      , authorizedRole :: Maybe RoleType
      , roleInstances :: SerializableNonEmptyArray RoleInstance
      , deltaType :: UniverseRoleDeltaType
      )
  )

derive instance newtypeLegacyUniverseRoleDelta :: Newtype LegacyUniverseRoleDelta _
derive newtype instance ReadForeign LegacyUniverseRoleDelta

-- | Convert a legacy UniverseRoleDelta to the new format.
-- | Takes the first element from roleInstances as the single roleInstance.
toUniverseRoleDelta :: LegacyUniverseRoleDelta -> UniverseRoleDelta
toUniverseRoleDelta (LegacyUniverseRoleDelta r) = UniverseRoleDelta
  { subject: r.subject
  , resourceKey: ""
  , resourceVersion: 0
  , id: r.id
  , contextType: r.contextType
  , roleType: r.roleType
  , authorizedRole: r.authorizedRole
  , roleInstance: firstRoleInstance
  , deltaType: r.deltaType
  }
  where
  firstRoleInstance = NEA.head (toNonEmptyArray r.roleInstances)

-----------------------------------------------------------
-- LEGACY CONTEXTDELTA
-----------------------------------------------------------
newtype LegacyContextDelta = LegacyContextDelta
  ( LegacyDeltaRecord
      ( contextInstance :: ContextInstance
      , contextType :: ContextType
      , roleType :: EnumeratedRoleType
      , roleInstance :: RoleInstance
      , destinationContext :: Maybe ContextInstance
      , destinationContextType :: Maybe ContextType
      , deltaType :: ContextDeltaType
      )
  )

derive instance newtypeLegacyContextDelta :: Newtype LegacyContextDelta _
derive newtype instance ReadForeign LegacyContextDelta

toContextDelta :: LegacyContextDelta -> ContextDelta
toContextDelta (LegacyContextDelta r) = ContextDelta
  { subject: r.subject
  , resourceKey: ""
  , resourceVersion: 0
  , contextInstance: r.contextInstance
  , contextType: r.contextType
  , roleType: r.roleType
  , roleInstance: r.roleInstance
  , destinationContext: r.destinationContext
  , destinationContextType: r.destinationContextType
  , deltaType: r.deltaType
  }

-----------------------------------------------------------
-- LEGACY ROLEBINDINGDELTA
-----------------------------------------------------------
newtype LegacyRoleBindingDelta = LegacyRoleBindingDelta
  ( LegacyDeltaRecord
      ( filled :: RoleInstance
      , filledType :: EnumeratedRoleType
      , filler :: Maybe RoleInstance
      , fillerType :: Maybe EnumeratedRoleType
      , oldFiller :: Maybe RoleInstance
      , oldFillerType :: Maybe EnumeratedRoleType
      , deltaType :: RoleBindingDeltaType
      )
  )

derive instance newtypeLegacyRoleBindingDelta :: Newtype LegacyRoleBindingDelta _
derive newtype instance ReadForeign LegacyRoleBindingDelta

toRoleBindingDelta :: LegacyRoleBindingDelta -> RoleBindingDelta
toRoleBindingDelta (LegacyRoleBindingDelta r) = RoleBindingDelta
  { subject: r.subject
  , resourceKey: ""
  , resourceVersion: 0
  , filled: r.filled
  , filledType: r.filledType
  , filler: r.filler
  , fillerType: r.fillerType
  , oldFiller: r.oldFiller
  , oldFillerType: r.oldFillerType
  , deltaType: r.deltaType
  }

-----------------------------------------------------------
-- LEGACY ROLEPROPERTYDELTA
-----------------------------------------------------------
newtype LegacyRolePropertyDelta = LegacyRolePropertyDelta
  ( LegacyDeltaRecord
      ( id :: RoleInstance
      , roleType :: EnumeratedRoleType
      , property :: EnumeratedPropertyType
      , values :: Array Value
      , deltaType :: RolePropertyDeltaType
      )
  )

derive instance newtypeLegacyRolePropertyDelta :: Newtype LegacyRolePropertyDelta _
derive newtype instance ReadForeign LegacyRolePropertyDelta

toRolePropertyDelta :: LegacyRolePropertyDelta -> RolePropertyDelta
toRolePropertyDelta (LegacyRolePropertyDelta r) = RolePropertyDelta
  { subject: r.subject
  , resourceKey: ""
  , resourceVersion: 0
  , id: r.id
  , roleType: r.roleType
  , property: r.property
  , values: r.values
  , deltaType: r.deltaType
  }

-----------------------------------------------------------
-- EXTRACT RESOURCE KEY FROM LEGACY DELTAS
-----------------------------------------------------------

-- | Try to parse a stringified delta as a legacy format and compute the
-- | resource key using the same conventions as new deltas:
-- |   UniverseContextDelta → unwrap id
-- |   UniverseRoleDelta    → unwrap (head roleInstances)
-- |   ContextDelta         → unwrap roleInstance
-- |   RoleBindingDelta     → unwrap filled <> "#binding"
-- |   RolePropertyDelta    → unwrap id <> "#" <> unwrap property
-- | Returns Nothing if the string cannot be parsed as any legacy delta.
extractLegacyResourceKey :: String -> Maybe String
extractLegacyResourceKey s =
  case runExcept $ readJSON' s of
    Right (LegacyUniverseContextDelta r) -> Just (unwrap r.id)
    Left _ -> case runExcept $ readJSON' s of
      Right (LegacyUniverseRoleDelta r) -> Just (unwrap (NEA.head (toNonEmptyArray r.roleInstances)))
      Left _ -> case runExcept $ readJSON' s of
        Right (LegacyContextDelta r) -> Just (unwrap r.roleInstance)
        Left _ -> case runExcept $ readJSON' s of
          Right (LegacyRoleBindingDelta r) -> Just (unwrap r.filled <> "#binding")
          Left _ -> case runExcept $ readJSON' s of
            Right (LegacyRolePropertyDelta r) -> Just (unwrap r.id <> "#" <> unwrap r.property)
            Left _ -> Nothing

-----------------------------------------------------------
-- HELPER: head for SerializableNonEmptyArray is via Data.Array.NonEmpty.head
