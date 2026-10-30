-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.RebootBigbangDatabase where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Perspectives.Persistence.CouchdbFunctions (databaseExists, ensureSecurityDocument)
import Perspectives.PerspectivesState (defaultRuntimeOptions)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.Representation.TypeIdentifiers (IndexedContext(..))
import Perspectives.Sidecar.ToStable (toStable)
import Test.PDRInstance (noBus, pollUntil, testPouchdbUser, withPDRCached)
import Test.LocalCouchdbTestSupport (addLocalCouchdbCredentials)
import Test.PDRInstance.Types (runInPDR)
import Test.RebootUniverse (rebootUniverseCompileTestModelConfiguration)
import Test.SinglePDRScaffold (emptyLogConfiguration, executeModelTest, loadModel)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest theSuite

theSuite :: TestSuite
theSuite = suite "Big Bang database creation (local destructive test)" do
  test "reports success after creation and makes the database public" do
    let
      cfg = rebootUniverseCompileTestModelConfiguration
        { testTimeLimit = Milliseconds 15000.0
        , outputSnapshotDirectory = Nothing
        }
    withPDRCached (testPouchdbUser "alice") defaultRuntimeOptions Nothing noBus cfg.snapshotDirectory \pdr -> do
      addLocalCouchdbCredentials pdr
      for_ cfg.testModelLoadMethods (loadModel pdr)
      testApp <- pollUntil 100 (Milliseconds 100.0) "Reboot test app to be installed" $
        runInPDR pdr do
          IndexedContext indexed <- toStable (IndexedContext cfg.indexedTestContext)
          lookupIndexedContext indexed
      for_ [ "Cleanup", "ManageCouchdb", "CreateBigBangsDatabase" ] \name -> do
        result <- executeModelTest pdr testApp
          ("model://joopringelberg.nl#RepositoryTools$" <> name)
          emptyLogConfiguration
          cfg
        case result of
          Left { err } -> assert (name <> ": " <> show err) false
          Right { testSucceeded } -> assert (name <> " should succeed") testSucceeded
      runInPDR pdr do
        exists <- databaseExists "https://perspectives.domains/cw_bigbangsdatabase/"
        liftAff $ assert "The physical Big Bang database must exist" exists
        security <- ensureSecurityDocument "https://perspectives.domains/" "cw_bigbangsdatabase"
        liftAff $ equal (Just []) (unwrap security).members.names
        liftAff $ equal [] (unwrap security).members.roles
