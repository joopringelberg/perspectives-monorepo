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
-- Full text of this license can be found in the LICENSE directory in the
-- projects root.

-- END LICENSE

-- | Layer 2 test entry point — integration tests with in-memory PouchDB.
-- |
-- | Prerequisites (before these tests can run):
-- |   1. The Node.js build (`pnpm run build:node`) must be available so that
-- |      `persistenceAPI.node.js` provides a PouchDB instance backed by
-- |      `pouchdb-adapter-memory` rather than IndexedDB.
-- |   2. `pouchdb-adapter-memory` must be installed as a dev dependency.
-- |   3. No real CouchDB or RabbitMQ instance is required.
-- |
-- | Run with:
-- |
-- |   pnpm run test:layer2
-- |
-- | These suites exercise the full persistence layer (create / read / update /
-- | delete context and role instances) but remain self-contained because the
-- | PouchDB adapter keeps all data in memory.
-- |
-- | TODO: Enable suites below once `persistenceAPI.node.js` is wired to
-- |       `pouchdb-adapter-memory`.  See the architecture document at
-- |       docsources/nodejs-testing-architecture.md for the migration steps.

module Test.Layer2 where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)

-- TODO: uncomment once persistenceAPI.node.js uses pouchdb-adapter-memory
-- import Test.LoadArc (theSuite) as LARC
-- import Test.ContextAndRole (theSuite) as CAR
-- import Test.Queries (theSuite) as QR
-- import Test.Sync.HandleTransaction (theSuite) as HTA

main :: Effect Unit
main = runTest do
  -- TODO: add Layer 2 suites here.  None are enabled yet — see module comment.
  pure unit
