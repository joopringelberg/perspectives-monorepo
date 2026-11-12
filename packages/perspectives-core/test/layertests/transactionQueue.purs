-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Test.TransactionQueue where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.Reader (ask)
import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Aff (forkAff, joinFiber, killFiber, throwError, try)
import Effect.Aff.AVar (empty, put, take, tryRead)
import Effect.Aff.Class (liftAff)
import Effect.AVar as AVar
import Effect.AVar (AVar)
import Effect.Exception (error)
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object as Object
import Perspectives.Api (handleRequest, requestInducesTransaction)
import Perspectives.ApiTypes (RequestRecord, RequestType(..))
import Perspectives.ContextAndRole (defaultRolRecord)
import Perspectives.CoreTypes (runMonadPerspectives)
import Perspectives.DomeinCache (storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFileRecord)
import Perspectives.InstanceRepresentation (PerspectRol(..))
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.PerspectivesState (addBinding, lookupVariableBinding, transactionFlag, transactionNumber, withTransactionPriority)
import Perspectives.Representation.Class.Cacheable (cacheEntity)
import Perspectives.Representation.Context (ContextKind(..), defaultContext)
import Perspectives.Representation.EnumeratedRole (defaultEnumeratedRole)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleKind(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.TransactionQueue (TransactionPriority(..), newTransactionQueue, withPriority)
import Simple.JSON (read)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert, equal)
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
    equal false (requestInducesTransaction GetRoleBinders)
    equal false (requestInducesTransaction CheckBinding)
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

  test "role-binder and binding queries respond without admission or a transaction" do
    runP do
      let
        model = "model://test.example#PureQueries"
        contextType = model <> "$Context"
        roleType = contextType <> "$Role"
        roleId = RoleInstance "def:pure-query-role"
        origin = ArcPosition { line: 0, column: 0 }
      void $ storeDomeinFileInCache (ModelUri model) $ DomeinFile defaultDomeinFileRecord
        { id = ModelUri model
        , namespace = ModelUri model
        , contexts = Object.singleton contextType (defaultContext contextType "Context" Domain Nothing origin Nothing)
        , enumeratedRoles = Object.singleton roleType (defaultEnumeratedRole roleType "Role" RoleInContext contextType origin)
        }
      void $ cacheEntity roleId $ PerspectRol defaultRolRecord
        { id = roleId, pspType = EnumeratedRoleType roleType }
      state <- ask
      flag <- transactionFlag
      liftAff do
        entered <- empty
        release <- empty
        completed <- empty
        blocker <- forkAff $ runMonadPerspectives
          (withTransactionPriority BackgroundTransaction $ liftAff (take flag *> put unit entered *> take release *> put true flag))
          state
        take entered
        for_
          [ { kind: GetRoleBinders, subject: "def:pure-query-role", object: "", expected: [] }
          , { kind: GetRoleBinders, subject: "def:pure-query-role", object: contextType, expected: [] }
          , { kind: CheckBinding, subject: "", object: "def:pure-query-role", expected: [ "true" ] }
          ]
          \query -> do
            response <- empty
            let
              request = (requestRecord query.kind 3 completed)
                { subject = query.subject
                , predicate = roleType
                , object = query.object
                , reactStateSetter = Just $ unsafeToForeign \value -> void $ AVar.tryPut (value :: Foreign) response
                }
            runMonadPerspectives (handleRequest request) state
            result <- take response
            case read result of
              Right (value :: { responseType :: String, result :: Array String }) -> do
                equal "APIresult" value.responseType
                equal query.expected value.result
              Left errors -> assert (show errors) false
        number <- runMonadPerspectives transactionNumber state
        equal 0 number
        put unit release
        joinFiber blocker

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
