-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.Persistence.Authentication where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Perspectives.Persistence.Authentication (authenticatedPerspectRequest, authenticatedUrlRequest, runningInNode)
import Perspectives.Persistence.Types (runMonadPouchdb)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Persistence authentication request construction" do
  test "uses cookies in browsers and a Basic header only in Node" do
    rq <- runWithCredentials $ authenticatedPerspectRequest authority
    equal Nothing rq.username
    equal Nothing rq.password
    equal true rq.withCredentials
    equal
      (if runningInNode then [ RequestHeader "Authorization" "Basic dGVzdC11c2VyOnRlc3QtcGFzc3dvcmQ=" ] else [])
      rq.headers

  test "derives stored credentials from a full database URL" do
    expected <- runWithCredentials $ authenticatedPerspectRequest authority
    actual <- runWithCredentials $ authenticatedUrlRequest (authority <> "models_test/_security")
    equal expected.headers actual.headers
    equal expected.username actual.username
    equal expected.password actual.password

  test "does not send another authority's credentials" do
    rq <- runWithCredentials $ authenticatedUrlRequest "https://different.example/models_test"
    equal [] rq.headers
    equal Nothing rq.username
    equal Nothing rq.password

  where
  authority = "https://repository.example/"
  runWithCredentials = runMonadPouchdb "test-user" "test-password" (PerspectivesUser "test-user") "test-system" (Just authority)
