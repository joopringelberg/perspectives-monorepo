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

module Test.Layer3ScaffoldTests
  ( scaffoldTests
  ) where

import Prelude

import Data.Array (length, null)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..), (##=))
import Perspectives.Instances.ObjectGetters (binding_, getUnlinkedRoleInstances)
import Perspectives.Logging (ansiMagenta, ansiRed, infoTest)
import Perspectives.ModelDependencies (mySocialEnvironment, socialEnvironmentPersons, sysMe)
import Perspectives.Names (lookupIndexedContext, lookupIndexedRole)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (tryGetPerspectRol)
import Perspectives.PerspectivesState (defaultRuntimeOptions, setTopicLogLevel)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Test.PDRInstance (connectPDRs, noBus, pollUntil, testPouchdbUser, withPDR, withTwoPDRs)
import Test.PDRInstance.Types (runInPDR)
import Test.Unit (TestSuite, suite, test, testOnly)
import Test.Unit.Assert (assert)

scaffoldTests :: TestSuite
scaffoldTests = suite "PDRInstance scaffold" do

  test "start a single PDR instance and read its system identifier" do
    let user = testPouchdbUser "alice"
    withPDR user defaultRuntimeOptions (Just ansiRed) noBus \pdr -> do
      sysId <- runInPDR pdr getSystemIdentifier
      assert "system identifier should equal alice_macbook" (sysId == "alice_macbook")
  
  testOnly "Start a single PDR instance and count the number of Person instances that are created" do
    let user = testPouchdbUser "alice"
    withPDR user defaultRuntimeOptions (Just ansiRed) noBus \pdr -> do
      runInPDR pdr $ setTopicLogLevel TEST Info
      instancesOfMe <- pollUntil 100 (Milliseconds 100.0)
        "Polling for Person instances..." $ do
          runInPDR pdr $ do
            -- Replace the following line with the actual code to count Person instances
            -- Get the indexed context sys:MySocialEnvironment
            mMySocialEnvironment <- lookupIndexedContext mySocialEnvironment
            case mMySocialEnvironment of
              Just mySocialEnvironment -> do
                -- Then get the Persons instance in that context
                persons <- mySocialEnvironment ##= getUnlinkedRoleInstances (EnumeratedRoleType socialEnvironmentPersons)
                infoTest $ "Found persons: " <> (show $ length persons)
                -- pollUntil stops at the first `Just`, so only succeed once there is a Person.
                pure $ if null persons then Nothing else Just persons
              Nothing -> do
                infoTest "No MySocialEnvironment found"
                pure Nothing
      assert "Person count should be 1 initially" ( length instancesOfMe == 1)

  test "start two PDR instances with distinct identifiers" do
    withTwoPDRs
      (testPouchdbUser "alice")
      defaultRuntimeOptions
      (Just ansiRed)
      (testPouchdbUser "bob")
      defaultRuntimeOptions
      (Just ansiMagenta)
      \pdrA pdrB -> do
        sysA <- runInPDR pdrA getSystemIdentifier
        sysB <- runInPDR pdrB getSystemIdentifier
        assert "PDR-A system identifier should equal alice_macbook" (sysA == "alice_macbook")
        assert "PDR-B system identifier should equal bob_macbook" (sysB == "bob_macbook")
        assert "PDR-A and PDR-B should have distinct identifiers" (sysA /= sysB)

  test "start two PDR instances and connect them" do
    withTwoPDRs
      (testPouchdbUser "alice")
      defaultRuntimeOptions
      (Just ansiRed)
      (testPouchdbUser "bob")
      defaultRuntimeOptions
      (Just ansiMagenta)
      \pdrA pdrB -> do
        runInPDR pdrA
          ( do
              setTopicLogLevel TEST Debug
              setTopicLogLevel DELTA Trace
          )
        runInPDR pdrB
          ( do
              setTopicLogLevel BROKER Debug
              setTopicLogLevel SYNC Trace
              setTopicLogLevel TEST Debug
              setTopicLogLevel RESOURCE Trace
          )
        connectPDRs pdrA pdrB
        malice <- runInPDR pdrA do
          muser <- lookupIndexedRole sysMe
          case muser of
            Just user -> binding_ user
            Nothing -> pure Nothing
        mbob <- runInPDR pdrB do
          muser <- lookupIndexedRole sysMe
          case muser of
            Just user -> binding_ user
            Nothing -> pure Nothing
        case malice, mbob of
          Just alice, Just bob -> do
            maliceForBob <- runInPDR pdrB $ tryGetPerspectRol alice
            mbobForAlice <- runInPDR pdrA $ tryGetPerspectRol bob
            case maliceForBob, mbobForAlice of
              Just _, Just _ -> assert "Both PDRs should have each others' Person instance" true
              Just _, Nothing -> assert "Bobs' Person instance should be visible for Alice, too" false
              _, _ -> assert "Both PDRs should have each others' Person instance" false
          _, _ -> assert "Both PDRs should have a `me` instance" false
