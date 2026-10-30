-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.RebootModelCompilation where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.RebootUniverse (rebootUniverseCompileTestModelConfiguration)
import Test.PublicationRecovery as Publication
import Test.SinglePDRScaffold (getSinglePDRResults)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  theSuite
  Publication.theSuite

theSuite :: TestSuite
theSuite = suite "Reboot model compilation without executing reboot actions" do
  test "compiles RepositoryTools and RebootUniverse against the seed snapshot" do
    results <- getSinglePDRResults $ rebootUniverseCompileTestModelConfiguration
      { tests = []
      , outputSnapshotDirectory = Nothing
      }
    assert "No destructive reboot cases should execute" (null results)
