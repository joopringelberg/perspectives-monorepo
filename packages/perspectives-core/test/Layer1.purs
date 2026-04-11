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

-- | Layer 1 test entry point — pure PureScript unit tests.
-- |
-- | These suites require NO external dependencies: no CouchDB, no RabbitMQ, no
-- | network, no browser. They run via the package.json script:
-- |
-- |   pnpm run test:layer1
-- |
-- | The suites included here are classified as Layer 1 because either:
-- |   (a) they are fully pure (no MonadPerspectives, no IO beyond the file-system),
-- |   (b) they use runP but only exercise in-memory computations (ADT algebra,
-- |       ARC parsing, phase-2/3 transformations, in-memory DomeinFile cache),
-- |       which do not trigger any PouchDB or HTTP call.
-- |
-- | Layer 2 (integration tests with in-memory PouchDB) and Layer 3 (sync tests
-- | with a stubbed AMQP transport) are in Layer2.purs and Layer3.purs respectively.

module Test.Layer1 where

import Prelude

import Effect (Effect)
import Test.Unit.Main (runTest)

-- Pure parser (no MonadPerspectives at all)
import Test.Parsing.Arc.Expression (theSuite) as TPAE

-- Pure ArrayT combinator tests (no MonadPerspectives)
import Test.ArrayT (theSuite) as ARRT

-- ADT algebra tests — use runP but only exercise pure in-memory computations
import Test.Representation.ADT (theSuite) as ADT
import Test.Perspectives.Representation.ADT.DisjunctiveNormalForm (theSuite) as DNF
import Test.Perspectives.Representation.ADT2 (theSuite) as ADT2
import Test.Perspectives.Representation.ADT.SpecialisesADT (theSuite) as SPECADT

-- ARC parser phases 1–3 — use runP / runPhaseTwo' / runPhaseThree' but only
-- perform in-memory AST transformations; no database access
import Test.Parsing.Arc (theSuite) as TPA
import Test.Parsing.Arc.PhaseTwo (theSuite) as TPA2
import Test.Parsing.Arc.PhaseThree (theSuite) as TPA3

-- Query description compiler — uses an in-memory DomeinFile cache; no CouchDB
import Test.Query.DescriptionCompiler (theSuite) as QDC

main :: Effect Unit
main = runTest do
  -- ── Truly pure (no MonadPerspectives / no IO) ──────────────────────────────
  TPAE.theSuite        -- ARC expression parser
  ARRT.theSuite        -- ArrayT combinators

  -- -- ── Pure ADT algebra (runP wraps in-memory computations only) ──────────────
  ADT.theSuite         -- ADT representation
  -- DNF.theSuite         -- Disjunctive/conjunctive normal form
  -- ADT2.theSuite        -- ADT2 functor / foldable / traversable
  -- SPECADT.theSuite     -- equalsOrSpecialises

  -- -- ── ARC parsing phases 1–3 (in-memory; file-system for .arc fixtures) ──────
  -- TPA.theSuite         -- Phase 1 — tokenise + parse
  -- TPA2.theSuite        -- Phase 2 — name resolution / type inference
  -- TPA3.theSuite        -- Phase 3 — inverted query indexing

  -- -- ── Query description compiler (in-memory DomeinFile cache) ────────────────
  -- QDC.theSuite
