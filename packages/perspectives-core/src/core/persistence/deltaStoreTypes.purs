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

-- | The DeltaStoreRecord type, kept in a separate module to avoid circular
-- | dependencies between Perspectives.Persistence.DeltaStore (which uses
-- | MonadPerspectives for caching) and Perspectives.CoreTypes (which defines
-- | PerspectivesExtraState including the delta cache).
module Perspectives.Persistence.DeltaStoreTypes
  ( DeltaStoreRecord(..)
  ) where

import Data.Maybe (Maybe)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Simple.JSON (class ReadForeign, class WriteForeign)

-- | A record stored in the delta-store PouchDB database.
-- | Document ID: <resourceKey>_<resourceVersion>_<author>
newtype DeltaStoreRecord = DeltaStoreRecord
  { _id :: String
  , _rev :: Maybe String
  , resourceKey :: String
  , resourceVersion :: Int
  , author :: PerspectivesUser
  , signedDelta :: SignedDelta
  , deltaType :: String
  , applied :: Boolean
  }

derive newtype instance WriteForeign DeltaStoreRecord
derive newtype instance ReadForeign DeltaStoreRecord
