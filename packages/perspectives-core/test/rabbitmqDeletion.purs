-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.RabbitMQDeletion where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Array (find)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (bracket)
import Perspectives.AMQP.RabbitMQManagement (deleteQueue, deleteUser, runRabbitState')
import Perspectives.CoreTypes (runMonadPerspectivesQueryToObject)
import Perspectives.Extern.RabbitMQ (deleteAMQPaccount, deleteQueue_, externalFunctions)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..), Value(..))
import Test.Perspectives.Utils (runP)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

foreign import data Server :: Type
foreign import startServer :: Effect (Promise { server :: Server, url :: String })
foreign import stopServer :: Server -> Effect (Promise Unit)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "RabbitMQ account and queue deletion results" do
  for_ [ "DeleteAMQPaccount", "DeleteQueue" ] \name ->
    test (name <> " is registered as a Boolean query, not an effect") do
      case find (\(Tuple identifier _) -> identifier == "model://perspectives.domains#RabbitMQ$" <> name) externalFunctions of
        Nothing -> assert ("Missing registration for " <> name) false
        Just (Tuple _ description) -> do
          equal false description.isEffect
          equal 4 description.nArgs

  for_ [ 204, 200, 401, 403, 404, 500, 0 ] \status ->
    test ("only HTTP 204 confirms deletion, response " <> show status) $
      bracket (toAffE startServer) (\fixture -> toAffE $ stopServer fixture.server) \fixture -> do
        let
          url = fixture.url <> show status <> "/"
          state =
            { virtualHost: "test"
            , brokerServiceUrl: url
            , adminUserName: "test-admin"
            , adminPassword: "test-password"
            }
        deleted <- runP $ runRabbitState' state (deleteUser "test-delete-user")
        equal (status == 204) deleted
        result <- runP $ runMonadPerspectivesQueryToObject (RoleInstance "ignored") $
          deleteAMQPaccount [ url ] [ state.adminUserName ] [ state.adminPassword ] [ "test-delete-user" ]
        equal (Value (if status == 204 then "true" else "false")) result
        queueDeleted <- runP $ runRabbitState' state (deleteQueue "test-delete-queue")
        equal (status == 204) queueDeleted
        queueResult <- runP $ runMonadPerspectivesQueryToObject (RoleInstance "ignored") $
          deleteQueue_ [ url ] [ state.adminUserName ] [ state.adminPassword ] [ "test-delete-queue" ]
        equal (Value (if status == 204 then "true" else "false")) queueResult
