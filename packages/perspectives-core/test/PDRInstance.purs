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
import Control.Monad.Error.Class (throwError)
import Control.Promise (Promise, toAffE)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds(..), bracket, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar, empty, new, put)
import Effect.Class (liftEffect)
import Foreign.Object (empty) as OBJ
import Main (forkCreateIndexedResources, forkDatabasePersistence, forkJustInTimeModelLoader, forkReferentialIntegrityFixer)
import Perspectives.AMQP.IncomingPost (incomingPost)
import Perspectives.AMQP.Stomp.Stub (InProcessBus, createInProcessBus, makeStompClientFactory)
import Perspectives.ApiTypes (PropertySerialization(..), RolSerialization(..))
import Perspectives.Assignment.RunAction (runActionForObject, runContextAction)
import Perspectives.Assignment.Update (setProperty)
import Perspectives.Authenticate (getPrivateKey)
import Perspectives.CoreTypes (BrokerService, IndexedResource, IntegrityFix, JustInTimeModelLoad(..), MonadPerspectives, MonadPerspectivesTransaction, PerspectivesState, RepeatingTransaction, RuntimeOptions, TypeFix, (##>))
import Perspectives.Extern.Files (getPFileTextValue)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (buitenRol)
import Perspectives.Instances.Builders (createAndAddRoleInstance)
import Perspectives.Instances.ObjectGetters (binding, context, getEnumeratedRoleInstances, getProperty)
import Perspectives.ModelDependencies (connectedToAMQPBroker, sysMe, sysUser)
import Perspectives.ModelTranslation (getCurrentLanguageFromIDB)
import Perspectives.Names (getMySystem, lookupIndexedRole)
import Perspectives.Persistence.API (PouchdbUser)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (saveMarkedResources)
import Perspectives.PerspectivesState (newPerspectivesState, setStompClientFactory)
import Perspectives.Representation.Class.PersistentType (EnumeratedPropertyType(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..), externalRole)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.ResourceIdentifiers (createDefaultIdentifier)
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SetupCouchdb (createUserDatabases)
import Perspectives.SetupUser (setupUser)
import Perspectives.Sync.HandleTransaction (executeTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer)
import Simple.JSON (readJSON)
import Unsafe.Coerce (unsafeCoerce)

-----------------------------------------------------------
-- CRYPTOGRAPHIC KEY LOADING
-----------------------------------------------------------
-- | Reads accounts/orn2j1nh3q_test3_keypair.json, imports both keys via SubtleCrypto
-- | (ECDSA P-384), and stores them in IDB under <guid>_privateKey / <guid>_publicKey.
foreign import loadKeypairImpl :: String -> Effect (Promise Unit)

loadKeypair :: String -> Aff Unit
loadKeypair = loadKeypairImpl >>> toAffE

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
startPDRInstance :: PouchdbUser -> RuntimeOptions -> Maybe InProcessBus -> Aff PDRInstance
startPDRInstance pouchdbUser runtimeOptions bus = do
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

  -- If we have a bus, replace the default real StompClient factory with the in-process stub.
  case bus of
    Just b -> do
      runPerspectivesWithState (setStompClientFactory (makeStompClientFactory b)) state
    Nothing -> pure unit

  -- Start background service fibers.
  -- The JIT model loader is stopped gracefully via `put Stop modelToLoad`;
  -- the remaining fibers are killed directly on shutdown.
  void $ forkAff $ forkJustInTimeModelLoader modelToLoad state
  integrityFiber <- forkAff $ forkReferentialIntegrityFixer missingResource state
  persistenceFiber <- forkAff $ forkDatabasePersistence state
  indexedResourceFiber <- forkAff $ forkCreateIndexedResources indexedResourceToCreate state

  -- Load the keypair from file into IDB before calling createAccount_.
  -- The key names follow authenticate.purs: takeGuid(perspectivesUser) <> "_privateKey" / "_publicKey".
  -- For perspectivesUser "testuser", takeGuid returns "testuser" unchanged (no '#' present).
  loadKeypair pouchdbUser.perspectivesUser

  -- Set up user databases and install the essential models.
  runPerspectivesWithState
    ( do
        addAllExternalFunctions
        key <- getPrivateKey
        modify \(s@{ runtimeOptions: ro }) -> s { runtimeOptions = ro { privateKey = unsafeCoerce key } }
        getSystemIdentifier >>= createUserDatabases
        setupUser Nothing
        saveMarkedResources
        case bus of
          Just b -> runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser)
            ( setProperty
                [ RoleInstance $ createDefaultIdentifier $ buitenRol pouchdbUser.systemIdentifier ]
                (EnumeratedPropertyType connectedToAMQPBroker)
                Nothing
                [ Value "true" ]
            )
          Nothing -> pure unit
    )
    state

  -- If we have a bus, start the incomingPost fiber to receive transactions.
  postFiber <- case bus of
    Just _ -> Just <$> (forkAff $ runPerspectivesWithState incomingPost state)
    Nothing -> pure Nothing

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
      traverse_ (killFiber done) postFiber

  pure { stateAVar: state, shutdown }

noBus :: Maybe InProcessBus
noBus = Nothing

-----------------------------------------------------------
-- INTERACTION HELPERS
-----------------------------------------------------------

-- TODO: een functie met dezelfde naam bestaat in TestUtils.purs (Test.Sync.Utils).
-- Deze versie moet prevaleren.
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

-- | Poll `action` every `interval` until it returns `Just a`, or throw an error
-- | after `maxAttempts` attempts.  Use this to wait for asynchronous PDR
-- | side-effects (state-entry bots, persistence fibers) to complete before
-- | proceeding to the next test step.
pollUntil
  :: forall a
   . Int
  -> Milliseconds
  -> String
  -> Aff (Maybe a)
  -> Aff a
pollUntil maxAttempts interval description action = go maxAttempts
  where
  go 0 = throwError $ error $
    "pollUntil timed out after " <> show maxAttempts <> " attempts waiting for: " <> description
  go n = do
    result <- action
    case result of
      Just a -> pure a
      Nothing -> do
        delay interval
        go (n - 1)

-----------------------------------------------------------
-- BRACKET UTILITIES
-----------------------------------------------------------

-- | Start a PDR instance, run `f`, then shut down — even if `f` throws.
withPDR
  :: forall a
   . PouchdbUser
  -> RuntimeOptions
  -> Maybe InProcessBus
  -> (PDRInstance -> Aff a)
  -> Aff a
withPDR pouchdbUser runtimeOptions mbus =
  bracket (startPDRInstance pouchdbUser runtimeOptions mbus) _.shutdown

-- TODO. Een functie met dezelfde naam bestaat in TestUtils.purs (Test.Sync.Utils). Deze versie moet prevaleren.
-- | Start two PDR instances, run `f`, then shut down both — even if `f` throws.
-- |
-- | The two instances are started sequentially.  Each has its own separate
-- | in-memory databases and user identity.  Both instances are guaranteed to
-- | shut down cleanly even if startup or `f` throws: instance 2 shuts down
-- | before instance 1 (innermost bracket first).
withTwoPDRs
  :: forall a
   . PouchdbUser
  -> RuntimeOptions
  -> PouchdbUser
  -> RuntimeOptions
  -> (PDRInstance -> PDRInstance -> Aff a)
  -> Aff a
withTwoPDRs user1 opts1 user2 opts2 f = do
  bus <- liftEffect createInProcessBus
  withPDR user1 opts1 (Just bus) \pdr1 ->
    withPDR user2 opts2 (Just bus) \pdr2 ->
      f pdr1 pdr2

-----------------------------------------------------------
-- CONNECT TWO PDR INSTANCES VIA INVITATION
-----------------------------------------------------------

-- | Role and property type identifiers for the Invitation mechanism.
-- | Human-readable form; the PDR translates these to stable CUIDs via `toStable`.
outgoingInvitationsType :: String
outgoingInvitationsType = "model://perspectives.domains#System$PerspectivesSystem$OutgoingInvitations"

inviterType :: String
inviterType = "model://perspectives.domains#System$Invitation$Inviter"

inviteeType :: String
inviteeType = "model://perspectives.domains#System$Invitation$Invitee"

invitationMessageProp :: String
invitationMessageProp = "model://perspectives.domains#System$Invitation$External$Message"

serialisedInvitationProp :: String
serialisedInvitationProp = "model://perspectives.domains#System$Invitation$External$SerialisedInvitation"

-- | Connect two PDR instances using the Invitation mechanism from
-- | `model://perspectives.domains#System`.
-- |
-- | Steps performed:
-- |
-- |   PDR1 side:
-- |   1. Run the `CreateInvitation` context action as `PerspectivesSystem$User`
-- |      in PDR1's `PerspectivesSystem` context.  This creates an `Invitation`
-- |      context and binds PDR1's `me` to the `Inviter` role.
-- |   2. Set `Invitation$External$Message` to `"Hello there"`.
-- |   3. Run the `CreateInvitation` object action as `Inviter` on the
-- |      `Invitation$External` role.  This sets `ConfirmationCode`, which
-- |      triggers the state-entry bot to serialise the invitation and store
-- |      it as `Invitation$External$SerialisedInvitation`.
-- |   4. Read the file content of `SerialisedInvitation`.
-- |
-- |   PDR2 side:
-- |   5. Parse the invitation JSON to obtain the embedded `TransactionForPeer`.
-- |   6. Execute that transaction in PDR2; this installs the `Invitation`
-- |      context in PDR2's database and registers PDR1's user as `Inviter`.
-- |   7. Create an `Invitation$Invitee` role instance in the same `Invitation`
-- |      context, filled with PDR2's own `me`.
-- |
-- | Both PDR instances must have been started with `startPDRInstance` (or via
-- | `withTwoPDRs`) so that `setupUser` has already loaded `model:System`.
connectPDRs :: PDRInstance -> PDRInstance -> Aff Unit
connectPDRs pdr1 pdr2 = do

  -- -----------------------------------------------------------------------
  -- PDR1: Step 1 — create the Invitation via context action
  -- -----------------------------------------------------------------------
  mySystem1 <- runInPDR pdr1 getMySystem

  runInPDR pdr1
    $ runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser)
    $
      runContextAction sysUser "CreateInvitation" mySystem1

  -- -----------------------------------------------------------------------
  -- PDR1: Step 2 — obtain the new Invitation context instance
  -- Poll until the state-entry bot has created and stored it.
  -- -----------------------------------------------------------------------
  invCtx <- pollUntil 100 (Milliseconds 100.0)
    "Invitation context to appear in PerspectivesSystem$OutgoingInvitations"
    ( runInPDR pdr1
        ( (ContextInstance mySystem1) ##>
            ( getEnumeratedRoleInstances (EnumeratedRoleType outgoingInvitationsType)
                >=> binding
                >=> context
            )
        )
    )

  let invExternal = externalRole invCtx

  -- -----------------------------------------------------------------------
  -- PDR1: Step 3 — set the Message property on Invitation$External
  -- -----------------------------------------------------------------------
  runInPDR pdr1
    $ runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser)
    $
      setProperty
        [ invExternal ]
        (EnumeratedPropertyType invitationMessageProp)
        Nothing
        [ Value "Hello there" ]

  -- -----------------------------------------------------------------------
  -- PDR1: Step 4 — run the Inviter's CreateInvitation object action
  -- (sets ConfirmationCode → bot creates SerialisedInvitation file)
  -- -----------------------------------------------------------------------
  runInPDR pdr1
    $ runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType inviterType)
    $
      runActionForObject
        (ENR $ EnumeratedRoleType inviterType)
        "CreateInvitation"
        (unwrap invCtx)
        (unwrap invExternal)

  -- -----------------------------------------------------------------------
  -- PDR1: Step 5 — read the SerialisedInvitation file content
  -- Poll until the state-entry bot has set the property and written the file.
  -- -----------------------------------------------------------------------
  invText <- pollUntil 30 (Milliseconds 200.0)
    "SerialisedInvitation property and file content to be set"
    ( do
        mPFileStr <- runInPDR pdr1
          (invExternal ##> getProperty (EnumeratedPropertyType serialisedInvitationProp))
        case mPFileStr of
          Nothing -> pure Nothing
          Just (Value v) -> runInPDR pdr1 $ getPFileTextValue v
    )

  -- -----------------------------------------------------------------------
  -- PDR2: Step 6 — parse invitation JSON and execute the transaction
  -- -----------------------------------------------------------------------
  -- The invitation file is JSON: { message, transaction, confirmation }
  -- where `transaction` is itself a JSON-encoded TransactionForPeer.
  (invData :: { transaction :: String }) <- case readJSON invText of
    Left err -> throwError $ error $
      "connectPDRs: failed to parse invitation JSON: " <> show err
    Right x -> pure x

  (tfp :: TransactionForPeer) <- case readJSON invData.transaction of
    Left err -> throwError $ error $
      "connectPDRs: failed to parse TransactionForPeer: " <> show err
    Right x -> pure x

  runInPDR pdr2
    $ runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser)
    $
      executeTransaction tfp

  -- Poll until the Inviter role is accessible in PDR2, confirming that the
  -- transaction and any state-entry bots have completed.
  void $ pollUntil 30 (Milliseconds 100.0)
    "Inviter role to be accessible in PDR2 after executeTransaction"
    ( runInPDR pdr2
        (invCtx ##> getEnumeratedRoleInstances (EnumeratedRoleType inviterType))
    )

  -- -----------------------------------------------------------------------
  -- PDR2: Step 7 — create Invitee role filled with PDR2's me
  -- -----------------------------------------------------------------------
  mMe2 <- runInPDR pdr2 $ lookupIndexedRole sysMe

  case mMe2 of
    Nothing -> throwError $ error
      "connectPDRs: cannot find indexed role 'me' in PDR2"
    Just me2 ->
      runInPDR pdr2
        $ runMonadPerspectivesTransaction' false (ENR $ EnumeratedRoleType sysUser)
        $ void
        $ createAndAddRoleInstance
            (EnumeratedRoleType inviteeType)
            (unwrap invCtx)
            ( RolSerialization
                { id: Nothing
                , properties: PropertySerialization OBJ.empty
                , binding: Just (unwrap me2)
                }
            )
