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

-- | Layer 3 test entry point — synchronisation tests with a stubbed AMQP transport.
-- |
-- | Prerequisites (before these tests can run):
-- |   1. All Layer 2 prerequisites (in-memory PouchDB via `persistenceAPI.node.js`).
-- |   2. A stub AMQP transport that replaces `@stomp/stompjs` with an in-process
-- |      message bus.  The stub must implement the same interface as the real
-- |      Stomp client and be injectable via the PDR state record.
-- |
-- | Test strategy:
-- |   1. Start two PDR instances (PDR-A and PDR-B) in the same Node.js process,
-- |      each with its own in-memory PouchDB and the stub AMQP transport.
-- |   2. Apply a mutation via PDR-A's API.
-- |   3. Assert that PDR-B receives the expected `TransactionForPeer` and that
-- |      its database reflects the change.
-- |
-- | This covers the full synchronisation path (delta generation, signing,
-- | transmission, application) without requiring a real RabbitMQ broker.
-- |
-- | Run with:
-- |
-- |   pnpm run test:layer3
-- |
-- | Smoke tests for the PDR instance scaffold are included below.
-- | They deliberately use `testOnly` so that they do NOT run automatically in CI:
-- | each test calls `startPDRInstance`, which invokes `setupUser` and downloads
-- | essential Perspectives models from https://perspectives.domains — this requires
-- | network access that is not available in the standard CI environment.
-- | To run these tests manually, pass `--only` to the test runner or select them
-- | from the test menu.
-- |
-- | TODO: Enable sync suites below once the stub AMQP transport is implemented.
-- |       See docsources/nodejs-testing-architecture.md §3 for the design.

module Test.Layer3 where

import Prelude

import Effect (Effect)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Test.PDRInstance (noBus, runInPDR, testPouchdbUser, withPDR, withTwoPDRs)
import Test.Sync.SetPropertyGetProperty (theSuite) as SPG
import Test.Unit (suite, test, testOnly)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "PDRInstance scaffold" do

    -- | Smoke test: start one PDR instance, verify the system identifier matches
    -- | the value we passed in, then shut down.
    test "start a single PDR instance and read its system identifier" do
      let user = testPouchdbUser "alice"
      withPDR user defaultRuntimeOptions noBus \pdr -> do
        sysId <- runInPDR pdr getSystemIdentifier
        assert "system identifier should equal alice_macbook" (sysId == "alice_macbook")

    -- | Smoke test: start two PDR instances in the same process, verify that
    -- | they have distinct system identifiers.
    testOnly "start two PDR instances with distinct identifiers" do
      withTwoPDRs
        (testPouchdbUser "alice")
        defaultRuntimeOptions
        (testPouchdbUser "bob")
        defaultRuntimeOptions
        \pdrA pdrB -> do
          sysA <- runInPDR pdrA getSystemIdentifier
          sysB <- runInPDR pdrB getSystemIdentifier
          assert "PDR-A system identifier should equal alice_macbook" (sysA == "alice_macbook")
          assert "PDR-B system identifier should equal bob_macbook" (sysB == "bob_macbook")
          assert "PDR-A and PDR-B should have distinct identifiers" (sysA /= sysB)
