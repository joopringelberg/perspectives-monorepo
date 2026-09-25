-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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
--
-- END LICENSE

module Perspectives.Sync.CanonicalJson
  ( canonicalizeJsonString
  , computeDeltaId
  , shortDeltaId
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.String.CodeUnits as CU
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import canonicalizeJsonStringImpl :: EffectFn1 String String
foreign import computeDeltaIdImpl :: EffectFn2 String String (Promise String)

canonicalizeJsonString :: String -> Effect String
canonicalizeJsonString = runEffectFn1 canonicalizeJsonStringImpl

computeDeltaId :: String -> String -> Aff String
computeDeltaId author payload = toAffE $ runEffectFn2 computeDeltaIdImpl author payload

shortDeltaId :: String -> String
shortDeltaId = CU.take 16
