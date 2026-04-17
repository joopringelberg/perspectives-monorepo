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

-- | Test scaffold for starting one or more PDR instances inside a single
-- | Node.js process.  Each instance gets its own in-memory PouchDB databases
-- | and its own user identity.
-- |
-- | Intended use
-- | ============
-- |
-- |   -- Start one instance, run a MonadPerspectives action, shut down.
-- |   withPDR (testPouchdbUser "alice") defaultRuntimeOptions \pdr -> do
-- |     result <- liftAff $ runInPDR pdr someQuery
-- |     assert "..." result
-- |
-- |   -- Start two instances (for synchronisation tests).
-- |   withTwoPDRs
-- |     (testPouchdbUser "alice") defaultRuntimeOptions
-- |     (testPouchdbUser "bob")   defaultRuntimeOptions
-- |     \pdrA pdrB -> do
-- |       ...
-- |
-- | Note
-- | ====
-- | Synchronisation between instances is intentionally left out of scope here;
-- | see PR #339 for the stubbed AMQP transport that enables Layer 3 sync tests.
-- | `startPDRInstance` starts only the background fibers required for model
-- | loading, in-memory persistence, indexed-resource creation and referential
-- | integrity: the same set that `createAccount_` uses.

module Test.PDRInstance where

import Prelude

import Control.Monad.AvarMonadAsk (modify)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Error, bracket, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar, empty, new, put)
import Main (forkCreateIndexedResources, forkDatabasePersistence, forkJustInTimeModelLoader, forkReferentialIntegrityFixer)
import Perspectives.Authenticate (getPrivateKey)
import Perspectives.CoreTypes (BrokerService, IndexedResource, IntegrityFix, JustInTimeModelLoad(..), MonadPerspectives, MonadPerspectivesTransaction, PerspectivesState, RepeatingTransaction, RuntimeOptions, TypeFix)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.ModelTranslation (getCurrentLanguageFromIDB)
import Perspectives.Persistence.API (PouchdbUser)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (saveMarkedResources)
import Perspectives.PerspectivesState (newPerspectivesState)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SetupCouchdb (createUserDatabases)
import Perspectives.SetupUser (setupUser)
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- PDR INSTANCE
-----------------------------------------------------------

-- | A running PDR installation with its state AVar and a shutdown action.
-- | Use `runInPDR` / `runTransactionInPDR` to interact with the PDR, and
-- | call `shutdown` (or use `withPDR` / `withTwoPDRs`) to clean up.
type PDRInstance =
  { stateAVar :: AVar PerspectivesState
  -- | Kill all background fibers for this instance.
  , shutdown :: Aff Unit
  }

-- | Convenience constructor for an in-memory (no CouchDB) PouchdbUser.
-- | The `perspectivesUser` is the bare name; `systemIdentifier` gets a
-- | "_macbook" suffix to mimic the convention used elsewhere.
testPouchdbUser :: String -> PouchdbUser
testPouchdbUser userName =
  { systemIdentifier: userName <> "_macbook"
  , perspectivesUser: userName
  , userName: Nothing
  , password: Nothing
  , couchdbUrl: Nothing
  }

-- | Start a new PDR instance for `pouchdbUser`.
-- |
-- | Steps performed:
-- |   1. Create all required AVars.
-- |   2. Construct the initial `PerspectivesState`.
-- |   3. Fork the background service fibers (JIT model loader, database
-- |      persistence, referential integrity fixer, indexed resource creator).
-- |   4. Install external functions, load the private key, create user
-- |      databases, run `setupUser` and persist the initial resources.
-- |
-- | The caller is responsible for calling `shutdown` when the instance is no
-- | longer needed.  Use `withPDR` / `withTwoPDRs` to get automatic cleanup.
startPDRInstance :: PouchdbUser -> RuntimeOptions -> Aff PDRInstance
startPDRInstance pouchdbUser runtimeOptions = do
  -- AVars required by PerspectivesState.
  transactionFlag <- new true
  brokerService <- (empty :: Aff (AVar BrokerService))
  transactionWithTiming <- (empty :: Aff (AVar RepeatingTransaction))
  modelToLoad <- (empty :: Aff (AVar JustInTimeModelLoad))
  indexedResourceToCreate <- (empty :: Aff (AVar IndexedResource))
  missingResource <- (empty :: Aff (AVar IntegrityFix))
  typeToBeFixed <- (empty :: Aff (AVar TypeFix))
  userIntegrityChoice <- (empty :: Aff (AVar Boolean))

  -- Build the initial state.
  state <- getCurrentLanguageFromIDB >>= new <<< newPerspectivesState
    pouchdbUser
    transactionFlag
    transactionWithTiming
    modelToLoad
    runtimeOptions
    brokerService
    indexedResourceToCreate
    missingResource
    typeToBeFixed
    userIntegrityChoice

  -- Start background service fibers.
  -- The JIT model loader is stopped gracefully via `put Stop modelToLoad`;
  -- the remaining fibers are killed directly on shutdown.
  void $ forkAff $ forkJustInTimeModelLoader modelToLoad state
  integrityFiber <- forkAff $ forkReferentialIntegrityFixer missingResource state
  persistenceFiber <- forkAff $ forkDatabasePersistence state
  indexedResourceFiber <- forkAff $ forkCreateIndexedResources indexedResourceToCreate state

  -- Set up user databases and install the essential models.
  runPerspectivesWithState
    ( do
        addAllExternalFunctions
        key <- getPrivateKey
        modify \(s@{ runtimeOptions: ro }) -> s { runtimeOptions = ro { privateKey = unsafeCoerce key } }
        getSystemIdentifier >>= createUserDatabases
        setupUser Nothing
        saveMarkedResources
    )
    state

  let
    done :: Error
    done = error "PDRInstance shutdown"

    shutdown :: Aff Unit
    shutdown = do
      -- Signal the JIT loader to stop gracefully.
      put Stop modelToLoad
      -- Kill the remaining daemon fibers.
      killFiber done integrityFiber
      killFiber done persistenceFiber
      killFiber done indexedResourceFiber

  pure { stateAVar: state, shutdown }

-----------------------------------------------------------
-- INTERACTION HELPERS
-----------------------------------------------------------

-- | Run a `MonadPerspectives` action against a PDR instance.
runInPDR :: forall a. PDRInstance -> MonadPerspectives a -> Aff a
runInPDR pdr mp = runPerspectivesWithState mp pdr.stateAVar

-- | Run a `MonadPerspectivesTransaction` against a PDR instance.
-- | Peer sharing is disabled (suitable for isolated single-instance tests).
runTransactionInPDR :: PDRInstance -> MonadPerspectivesTransaction Unit -> Aff Unit
runTransactionInPDR pdr mpt =
  void $ runPerspectivesWithState
    (runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser) mpt)
    pdr.stateAVar

-----------------------------------------------------------
-- BRACKET UTILITIES
-----------------------------------------------------------

-- | Start a PDR instance, run `f`, then shut down — even if `f` throws.
withPDR
  :: forall a
   . PouchdbUser
  -> RuntimeOptions
  -> (PDRInstance -> Aff a)
  -> Aff a
withPDR pouchdbUser runtimeOptions =
  bracket (startPDRInstance pouchdbUser runtimeOptions) _.shutdown

-- | Start two PDR instances, run `f`, then shut down both — even if `f` throws.
-- |
-- | The two instances are started sequentially.  Each has its own separate
-- | in-memory databases and user identity.
withTwoPDRs
  :: forall a
   . PouchdbUser
  -> RuntimeOptions
  -> PouchdbUser
  -> RuntimeOptions
  -> (PDRInstance -> PDRInstance -> Aff a)
  -> Aff a
withTwoPDRs user1 opts1 user2 opts2 f =
  bracket
    (do
      pdr1 <- startPDRInstance user1 opts1
      pdr2 <- startPDRInstance user2 opts2
      pure { pdr1, pdr2 })
    (\{ pdr1, pdr2 } -> do
      pdr1.shutdown
      pdr2.shutdown)
    (\{ pdr1, pdr2 } -> f pdr1 pdr2)
