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

-- | Layer 3 test entry point — tests that require full PDR instances.
-- |
-- | This suite aggregates:
-- |   1. Constructive synchronisation tests (two connected PDR instances)
-- |   2. Model compilation regression tests (full compilation flow)
-- |
-- | Run with:
-- |
-- |   pnpm run test:layer3

module Test.Layer3 where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.SinglePDRScaffold (getSinglePDRResults)
import Test.ConstructiveSynchronisationTest (getSynchronisationResults, synchronisationSuite)
import Test.Layer3ScaffoldTests (scaffoldTests)
import Test.ModelCompilationRegression (getCompilationResults, modelCompilationSuite)
import Test.QueryStepTests (queryStepSuite, queryStepTestModelConfiguration)
import Test.SinglePDRDestructiveTests (singlePDRDestructiveSuite, singlePDRDestructiveTestModelConfiguration)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = launchAff_ do
  synchronisationResults <- getSynchronisationResults
  compilationResults <- getCompilationResults
  queryStepResults <- getSinglePDRResults queryStepTestModelConfiguration
  destructiveResults <- getSinglePDRResults singlePDRDestructiveTestModelConfiguration
  liftEffect $ runTest do
    scaffoldTests
    synchronisationSuite synchronisationResults
    modelCompilationSuite compilationResults
    queryStepSuite queryStepResults
    singlePDRDestructiveSuite destructiveResults
