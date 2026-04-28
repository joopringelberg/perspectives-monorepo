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
-- Full text of this license can be found in the LICENSE directory in the
-- projects root.

-- END LICENSE

-- | Layer 3 synchronisation test: SetProperty (PDR-A) → GetProperty (PDR-B).
-- |
-- | ## What this test exercises
-- |
-- | ```
-- |  ┌────────────────────────────────────────────────────┐
-- |  │  PDR-A (userA)                                     │
-- |  │   setProperty rolId propType [Value "hello"]       │
-- |  │     └─► delta generation                           │
-- |  │           └─► sign delta                           │
-- |  │                 └─► distributeTransaction           │
-- |  │                       └─► sendTransactieToUserUsingAMQP(userB, tfp)
-- |  │                             └─► StompClient.publish("/topic/userB", ...)
-- |  └─────────────────────────────────────────────────────
-- |         │  InProcessBus routes the message
-- |         ▼
-- |  ┌────────────────────────────────────────────────────┐
-- |  │  PDR-B (userB)                                     │
-- |  │   incomingPost  (running in background fiber)      │
-- |  │     └─► messageProducer emits TransactionForPeer   │
-- |  │           └─► executeTransaction tfp               │
-- |  │                 └─► setProperty applied locally    │
-- |  └─────────────────────────────────────────────────────
-- |
-- |  Test assertion: PDR-B.getProperty rolId propType == Just (Value "hello")
-- | ```
-- |
-- | ## Status
-- |
-- | This suite is currently **skipped** (`suiteSkip`) because:
-- |   1. The in-memory PouchDB setup (Layer 2 prerequisite) is not yet wired.
-- |   2. The `model:System` must be loaded in both PDR instances.
-- |   3. The `connectedToAMQPBroker` flag must be set to `"true"` in both
-- |      PDR instances so that `distributeTransaction` routes via AMQP.
-- |   4. Both instances must share a common role instance identifier.
-- |
-- | Enable once the above prerequisites are in place.
-- | See `design/layer3-sync-tests.md` for the full design and step-by-step
-- | enablement guide.

module Test.Sync.SetPropertyGetProperty where

import Prelude

import Control.Monad.Free (Free)
import Data.Maybe (Maybe(..))
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.CoreTypes ((##>))
import Perspectives.Instances.ObjectGetters (getProperty)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Test.PDRInstance (runInPDR, testPouchdbUser, withTwoPDRs)
import Test.Unit (TestF, suiteSkip, test)
import Test.Unit.Assert (assert)

-- | A role instance identifier that exists in both PDR-A and PDR-B's
-- | databases.  In a real Layer 3 test this would be set up by first
-- | creating the role in PDR-A and letting the creation propagate to PDR-B
-- | via the AMQP stub (or by seeding both databases with the same fixture).
-- |
-- | Replace with a real identifier once the two-PDR database seeding is done.
sharedRoleId :: RoleInstance
sharedRoleId = RoleInstance "model:User$userA$User"

-- | The property under test.  Must exist on the role type above in
-- | `model:System`.
testPropertyType :: EnumeratedPropertyType
testPropertyType = EnumeratedPropertyType "model:System$PerspectivesSystem$User$Channel"

-- | The author role used when running a MonadPerspectivesTransaction.
authoringRole :: RoleType
authoringRole = ENR $ EnumeratedRoleType sysUser

theSuite :: Free TestF Unit
theSuite = suiteSkip "SetProperty (PDR-A) → GetProperty (PDR-B)" do

  -- -------------------------------------------------------------------------
  -- Test 1: Basic round-trip
  -- -------------------------------------------------------------------------
  -- | Mutate a property value in PDR-A and verify that PDR-B receives and
  -- | applies the resulting `TransactionForPeer`.
  -- |
  -- | Prerequisites (must all hold before this test can pass):
  -- |  * Layer 2 in-memory PouchDB is wired.
  -- |  * `model:System` is loaded in both PDR instances.
  -- |  * `sharedRoleId` exists in both databases.
  -- |  * `connectedToAMQPBroker` is `"true"` in both instances.
  -- |  * Both PDR instances run `incomingPost` in background fibers.
  test "property set on PDR-A is visible on PDR-B after synchronisation" do
    withTwoPDRs
      (testPouchdbUser "userA")
      defaultRuntimeOptions
      (testPouchdbUser "userB")
      defaultRuntimeOptions
      \pdrA pdrB -> do

        -- -----------------------------------------------------------------------
        -- Set up: load models and seed shared state in both PDR instances.
        -- -----------------------------------------------------------------------
        -- TODO: set connectedToAMQPBroker = "true" in both PDRs.
        -- TODO: start incomingPost in background fibers for both PDRs.

        -- -----------------------------------------------------------------------
        -- Step 1 — Apply mutation in PDR-A.
        -- -----------------------------------------------------------------------
        runInPDR pdrA do
          runMonadPerspectivesTransaction' false authoringRole do
            setProperty [ sharedRoleId ] testPropertyType Nothing [ Value "hello" ]

        -- -----------------------------------------------------------------------
        -- Step 2 — Wait for the transaction to be delivered and applied in PDR-B.
        -- -----------------------------------------------------------------------
        -- The in-process bus delivers messages synchronously in the same tick,
        -- but `executeTransaction` runs in MonadPerspectives (Aff), so we wait
        -- a short while for the fiber to complete.
        liftAff $ delay (Milliseconds 200.0)

        -- -----------------------------------------------------------------------
        -- Step 3 — Query PDR-B and assert the value arrived.
        -- -----------------------------------------------------------------------
        mval <- runInPDR pdrB (sharedRoleId ##> getProperty testPropertyType)
        liftAff $ assert
          "PDR-B: property value should be 'hello' after synchronisation from PDR-A"
          (mval == Just (Value "hello"))

  -- -------------------------------------------------------------------------
  -- Test 2: Bi-directional synchronisation
  -- -------------------------------------------------------------------------
  -- | Verify that mutations flow in both directions: A→B and B→A.
  test "mutations flow in both directions (A→B and B→A)" do
    withTwoPDRs
      (testPouchdbUser "userA")
      defaultRuntimeOptions
      (testPouchdbUser "userB")
      defaultRuntimeOptions
      \pdrA pdrB -> do

        -- TODO: prerequisite setup (see Test 1 above).

        -- A sets the property.
        runInPDR pdrA do
          runMonadPerspectivesTransaction' false authoringRole do
            setProperty [ sharedRoleId ] testPropertyType Nothing [ Value "from-A" ]

        liftAff $ delay (Milliseconds 200.0)

        mvalB <- runInPDR pdrB (sharedRoleId ##> getProperty testPropertyType)
        liftAff $ assert "B sees A's mutation" (mvalB == Just (Value "from-A"))

        -- B updates the property.
        runInPDR pdrB do
          runMonadPerspectivesTransaction' false authoringRole do
            setProperty [ sharedRoleId ] testPropertyType Nothing [ Value "from-B" ]

        liftAff $ delay (Milliseconds 200.0)

        mvalA <- runInPDR pdrA (sharedRoleId ##> getProperty testPropertyType)
        liftAff $ assert "A sees B's mutation" (mvalA == Just (Value "from-B"))
