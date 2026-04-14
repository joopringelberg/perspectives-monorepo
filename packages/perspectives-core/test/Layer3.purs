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
-- | The suites below are currently `suiteSkip`-ped inside their own modules
-- | because they depend on the Layer 2 in-memory PouchDB setup and full
-- | model loading, which are not yet wired.  Uncomment and enable them as
-- | those prerequisites land.
-- | See `design/layer3-sync-tests.md` for the step-by-step enablement guide.

module Test.Layer3 where

import Prelude
import Effect (Effect)
import Test.Unit.Main (runTest)
import Test.Sync.SetPropertyGetProperty (theSuite) as SPG

main :: Effect Unit
main = runTest do
  SPG.theSuite
