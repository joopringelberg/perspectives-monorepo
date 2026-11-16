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
-- Full text of this license can be found in the LICENSE directory in the
-- projects root.
-- END LICENSE

module Test.Lsp.Members where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Perspectives.Lsp.Server (roleProperties)
import Perspectives.Representation.ADT (ADT(..))
import Test.Perspectives.Utils (runP)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Language-server member provenance" do
  test "stable filler references report readable origins for own and aspect properties" do
    let
      root = "model://example.org#Management$Administrators"
      users = "model://perspectives.domains#System$TheWorld$PerspectivesUsers"
      stableUsers = "model://perspectives.domains#tiodn6tcyc$worldcuid1$userscuid1"
      persons = "model://perspectives.domains#System$SocialEnvironment$Persons"
      identifiable = "model://perspectives.domains#System$Identifiable"
      addressable = "model://perspectives.domains#System$Addressable"
      property owner local = { name: owner <> "$" <> local, kind: "enumeratedProperty", origin: owner }
      view name = pure $ case name of
        _ | name == root -> Just
          { name: root
          , members: [ { name: root <> "$Local", kind: "enumeratedProperty" } ]
          , aspects: []
          , filler: Just (PROD [ ST persons, ST stableUsers ])
          }
        _ | name == persons -> Just
          { name: persons, members: [], aspects: [ addressable ], filler: Nothing }
        _ | name == stableUsers -> Just
          { name: users
          , members: [ { name: users <> "$SharedFileServerKey", kind: "enumeratedProperty" } ]
          , aspects: [ identifiable ]
          , filler: Nothing
          }
        _ | name == identifiable -> Just
          { name: identifiable
          , members: [ { name: identifiable <> "$TransportPublicKey", kind: "enumeratedProperty" } ]
          , aspects: []
          , filler: Nothing
          }
        _ | name == addressable -> Just
          { name: addressable
          , members: [ { name: addressable <> "$Street", kind: "enumeratedProperty" } ]
          , aspects: []
          , filler: Nothing
          }
        _ -> Nothing
    properties <- runP $ roleProperties view [] root
    equal
      [ property root "Local"
      , property addressable "Street"
      , property users "SharedFileServerKey"
      , property identifiable "TransportPublicKey"
      ]
      properties
