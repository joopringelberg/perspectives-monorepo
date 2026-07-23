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

import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (LogLevel(..), LogTopic(..))
import Perspectives.Logging (ansiMagenta, ansiRed)
import Perspectives.ModelDependencies (sysMe)
import Perspectives.Names (lookupIndexedRole)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Persistent (tryGetPerspectRol)
import Perspectives.PerspectivesState (defaultRuntimeOptions, setTopicLogLevel)
import Test.PDRInstance (noBus, runInPDR, testPouchdbUser, withPDR, withTwoPDRs, connectPDRs)
import Test.Unit (TestSuite, suiteSkip, test)
import Test.Unit.Assert (assert)
import Perspectives.Instances.ObjectGetters (binding_)

scaffoldTests :: TestSuite
scaffoldTests = suiteSkip "PDRInstance scaffold" do

  test "start a single PDR instance and read its system identifier" do
    let user = testPouchdbUser "alice"
    withPDR user defaultRuntimeOptions (Just ansiRed) noBus \pdr -> do
      sysId <- runInPDR pdr getSystemIdentifier
      assert "system identifier should equal alice_macbook" (sysId == "alice_macbook")

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
