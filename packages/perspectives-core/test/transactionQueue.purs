-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Test.TransactionQueue where

import Prelude

import Control.Monad.AvarMonadAsk (gets)
import Control.Monad.Free (Free)
import Control.Monad.Reader (ask)
import Data.Either (isLeft)
import Data.Maybe (Maybe(..), isNothing)
import Data.Map as Map
import Effect (Effect)
import Effect.Aff (forkAff, joinFiber, killFiber, throwError, try)
import Effect.Aff.AVar (empty, put, take, tryRead)
import Effect.Aff.Class (liftAff)
import Effect.AVar as AVar
import Effect.AVar (AVar)
import Effect.Exception (error)
import Foreign (Foreign, unsafeToForeign)
import Perspectives.Api (handleRequest, requestInducesTransaction)
import Perspectives.ApiTypes (RequestRecord, RequestType(..))
import Perspectives.CoreTypes (runMonadPerspectives)
import Perspectives.PerspectivesState (addBinding, lookupVariableBinding, transactionFlag, withTransactionPriority)
import Perspectives.TransactionQueue (TransactionPriority(..), newTransactionQueue, withPriority)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

requestRecord :: RequestType -> Int -> AVar Unit -> RequestRecord
requestRecord kind corrId completed =
  { request: kind
  , subject: ""
  , predicate: ""
  , object: ""
  , reactStateSetter: Just $ unsafeToForeign \(_ :: Foreign) -> void $ AVar.tryPut unit completed
  , corrId
  , contextDescription: unsafeToForeign ""
  , rolDescription: Nothing
  , authoringRole: Nothing
  , onlyOnce: true
  }

theSuite :: Free TestF Unit
theSuite = suite "Transaction admission and API responsiveness" do
  test "classifies mutations without queuing ordinary reads or unsubscribe" do
    equal true (requestInducesTransaction SetProperty)
    equal true (requestInducesTransaction SetPreferredUserRoleType)
    equal true (requestInducesTransaction EvaluateRoleInstance)
    equal true (requestInducesTransaction GetRoleBinders)
    equal true (requestInducesTransaction CheckBinding)
    equal false (requestInducesTransaction GetProperty)
    equal false (requestInducesTransaction GetScreen)
    equal false (requestInducesTransaction Unsubscribe)

  test "a read responds while an earlier GUI mutation waits behind active background work" do
    readWasIndependent <- runP do
      state <- ask
      flag <- transactionFlag
      liftAff do
        entered <- empty
        release <- empty
        mutationDone <- empty
        readDone <- empty
        blocker <- forkAff $ runMonadPerspectives
          (withTransactionPriority BackgroundTransaction $ liftAff (take flag *> put unit entered *> take release *> put true flag))
          state
        take entered
        -- An invalid import exercises the mutation lane without accessing persistence.
        runMonadPerspectives (handleRequest $ requestRecord ImportContexts 1 mutationDone) state
        runMonadPerspectives (handleRequest $ requestRecord GetSystemIdentifier 2 readDone) state
        take readDone
        waiting <- isNothing <$> tryRead mutationDone
        put unit release
        joinFiber blocker
        take mutationDone
        pure waiting
    equal true readWasIndependent

  test "a queued role-binder query neither blocks reads nor registers after unsubscribe" do
    cancelled <- runP do
      state <- ask
      liftAff do
        entered <- empty
        release <- empty
        subscriptionDone <- empty
        readDone <- empty
        blocker <- forkAff $ runMonadPerspectives
          (withTransactionPriority BackgroundTransaction $ liftAff (put unit entered *> take release))
          state
        take entered
        runMonadPerspectives (handleRequest $ requestRecord GetRoleBinders 1 subscriptionDone) state
        runMonadPerspectives (handleRequest $ requestRecord Unsubscribe 1 subscriptionDone) state
        runMonadPerspectives (handleRequest $ requestRecord GetSystemIdentifier 2 readDone) state
        take readDone
        put unit release
        joinFiber blocker
        runMonadPerspectives (withTransactionPriority BackgroundTransaction (pure unit)) state
        pending <- runMonadPerspectives (gets _.pendingApiSubscriptions) state
        noCallback <- isNothing <$> tryRead subscriptionDone
        pure (Map.isEmpty pending && noCallback)
    equal true cancelled

  test "admission preserves the caller's variable bindings" do
    binding <- runP do
      addBinding "currentcontext" [ "example" ]
      withTransactionPriority UserTransaction (lookupVariableBinding "currentcontext")
    equal (Just [ "example" ]) binding

  test "a failed action releases admission" do
    let queue = newTransactionQueue unit
    result <- try $ withPriority queue UserTransaction (throwError (error "expected failure") *> pure unit)
    equal true (isLeft result)
    value <- withPriority queue BackgroundTransaction (pure 42)
    equal 42 value

  test "cancelling a waiting action does not strand the queue" do
    let queue = newTransactionQueue unit
    entered <- empty
    release <- empty
    blocker <- forkAff $ withPriority queue BackgroundTransaction (put unit entered *> take release)
    take entered
    cancelled <- forkAff $ withPriority queue UserTransaction (pure unit)
    killFiber (error "cancel waiter") cancelled
    put unit release
    joinFiber blocker
    value <- withPriority queue BackgroundTransaction (pure 42)
    equal 42 value
