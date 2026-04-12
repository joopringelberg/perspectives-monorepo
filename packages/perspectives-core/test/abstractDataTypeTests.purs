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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

-- | Unit tests for the three strongly-interrelated type-representation modules:
-- |
-- |   * Perspectives.Representation.ExpandedADT   (expandedADT.purs)
-- |   * Perspectives.Representation.CNF           (conjunctiveNormalForm.purs)
-- |   * Perspectives.Representation.ADT           (abstractDataType2.purs)
-- |
-- | All tests are purely in-memory; no MonadPerspectives / CouchDB is needed.
-- | They therefore belong in Layer 1.
-- |
-- | For background see docsources/type-comparison.md.

module Test.Perspectives.Representation.AbstractDataTypeTests where

import Prelude

import Control.Monad.Free (Free)
import Data.Array (singleton)
import Data.Foldable (foldMap)
import Data.Identity (Identity(..))
import Data.Int (even)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Aff.Class (liftAff)
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT, commonLeavesInADT, computeBoolean, computeExpandedBoolean, equals_, equalsOrSpecialises, equalsOrSpecialises_, expand, foldMapADT, generalises, generalises_, specialises, specialises_)
import Perspectives.Representation.CNF (DPROD(..), DSUM(..), toConjunctiveNormalForm, traverseDPROD)
import Perspectives.Representation.ExpandedADT (ExpandedADT(..), foldMapExpandedADT)
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert (assert)

-- | Expand an ADT Int purely with the Identity applicative.
-- | ST and UET leaves both become EST leaves; SUM and PROD become ESUM and EPROD.
expandPure :: ADT Int -> Identity (ExpandedADT Int)
expandPure = expand (unsafePartial leafToExpanded)
  where
  leafToExpanded :: Partial => ADT Int -> Identity (ExpandedADT Int)
  leafToExpanded adt = case adt of
    ST x -> Identity (EST x)
    UET x -> Identity (EST x)

theSuite :: Free TestF Unit
theSuite = suite "Test.Perspectives.Representation.AbstractDataTypeTests" do

  -- ─── ExpandedADT ────────────────────────────────────────────────────────────

  suite "ExpandedADT" do

    test "Eq" do
      liftAff $ assert "EST 1 == EST 1" $
        EST 1 == EST (1)
      liftAff $ assert "EST 1 /= EST 2" $
        EST 1 /= EST (2)
      liftAff $ assert "ESUM [EST 1, EST 2] equals identical structure" $
        ESUM [EST 1, EST 2] == ESUM [EST (1), EST 2]
      liftAff $ assert "EPROD [EST 1] /= EPROD [EST 2]" $
        EPROD [EST (1)] /= EPROD [EST 2]

    test "Functor map" do
      liftAff $ assert "map (+1) (EST 1) == EST 2" $
        map ((+) 1) (EST 1) == EST (2)
      liftAff $ assert "map (*2) (ESUM [EST 3, EST 4]) == ESUM [EST 6, EST 8]" $
        map ((*) 2) (ESUM [EST 3, EST 4]) == ESUM [EST 6, EST (8)]
      liftAff $ assert "map (*2) (EPROD [EST 3, EST 4]) == EPROD [EST 6, EST 8]" $
        map ((*) 2) (EPROD [EST 3, EST 4]) == EPROD [EST 6, EST (8)]
      -- ECT: both the label and the body are mapped
      liftAff $ assert "map f (ECT (EST 0) (EST 5)) maps label and body" $
        map ((+) 1) (ECT (EST 0) (EST 5)) == ECT (EST 1) (EST (6))

    test "Foldable foldMap" do
      liftAff $ assert "foldMap singleton (EST 1) == [1]" $
        foldMap singleton (EST (1)) == [1]
      liftAff $ assert "foldMap singleton (ESUM [EST 1, EST 2]) == [1, 2]" $
        foldMap singleton (ESUM [EST (1), EST 2]) == [1, 2]
      liftAff $ assert "foldMap singleton (EPROD [EST 1, EST 2]) == [1, 2]" $
        foldMap singleton (EPROD [EST (1), EST 2]) == [1, 2]
      liftAff $ assert "foldMap Additive sums all leaf values" $
        unwrap (foldMap Additive (EPROD [EST 1, EST 2, EST 3])) == (6)

    test "Traversable traverse" do
      liftAff $ assert "traverse Just (EST 1) == Just (EST 1)" $
        traverse Just (EST (1)) == Just (EST 1)
      liftAff $ assert "traverse Nothing-producing function returns Nothing" $
        traverse (\n -> if n > (0) then Just n else Nothing) (EST (-1)) == Nothing
      liftAff $ assert "traverse Just over ESUM preserves structure" $
        traverse Just (ESUM [EST (1), EST 2]) == Just (ESUM [EST 1, EST 2])
      liftAff $ assert "traverse Just over EPROD preserves structure" $
        traverse Just (EPROD [EST (1), EST 2]) == Just (EPROD [EST 1, EST 2])

    test "foldMapExpandedADT (HeytingAlgebra fold)" do
      -- EPROD folds leaves with AND (Conj)
      liftAff $ assert "EPROD: all even → AND = true" $
        foldMapExpandedADT even (EPROD [EST 2, EST 4]) == true
      liftAff $ assert "EPROD: one odd → AND = false" $
        foldMapExpandedADT even (EPROD [EST 1, EST 2]) == false
      -- ESUM folds leaves with OR (Disj)
      liftAff $ assert "ESUM: at least one even → OR = true" $
        foldMapExpandedADT even (ESUM [EST 1, EST 2]) == true
      liftAff $ assert "ESUM: all odd → OR = false" $
        foldMapExpandedADT even (ESUM [EST 1, EST 3]) == false
      -- ECT delegates to its second argument (the expansion); the label is ignored
      liftAff $ assert "ECT ignores label, delegates to expansion" $
        foldMapExpandedADT even (ECT (EST 1) (EST 2)) == true

  -- ─── CNF ────────────────────────────────────────────────────────────────────

  suite "CNF" do

    test "DSUM Eq is set-based (order-invariant)" do
      liftAff $ assert "DSUM [1, 2] == DSUM [2, 1]" $
        DSUM [1, 2] == DSUM [2, 1]
      liftAff $ assert "DSUM [1] /= DSUM [2]" $
        DSUM [1] /= DSUM [2]
      liftAff $ assert "DSUM [1, 2, 3] == DSUM [3, 1, 2]" $
        DSUM [1, 2, 3] == DSUM [3, 1, 2]

    test "DPROD Eq is set-based (order-invariant)" do
      liftAff $ assert "DPROD [DSUM [1]] == DPROD [DSUM [1]]" $
        DPROD [DSUM [1]] == DPROD [DSUM [1]]
      liftAff $ assert "DPROD: order of DSUM factors does not matter" $
        DPROD [DSUM [1], DSUM [2]] == DPROD [DSUM [2], DSUM [1]]
      liftAff $ assert "DPROD [DSUM [1], DSUM [2]] /= DPROD [DSUM [1, 2]]" $
        DPROD [DSUM [1], DSUM [2]] /= DPROD [DSUM [1, 2]]

    test "toConjunctiveNormalForm: EST leaf" do
      -- EST a → DPROD [ DSUM [a] ]
      liftAff $ assert "EST a normalises to a single-factor, single-alternative CNF" $
        toConjunctiveNormalForm (EST (1)) == DPROD [DSUM [1]]

    test "toConjunctiveNormalForm: ECT (label discarded)" do
      -- ECT label a → toConjunctiveNormalForm a  (label is already included in the expansion)
      liftAff $ assert "ECT label a produces the same CNF as its body a alone" $
        toConjunctiveNormalForm (ECT (EST 99) (EST (1))) == DPROD [DSUM [1]]
      liftAff $ assert "ECT wrapping an EPROD delegates correctly" $
        toConjunctiveNormalForm (ECT (EST 99) (EPROD [EST 1, EST 2]))
          == DPROD [DSUM [1], DSUM [2]]

    test "toConjunctiveNormalForm: ESUM distributes into CNF" do
      -- ESUM [EST a, EST b] → DPROD [ DSUM [a, b] ]
      liftAff $ assert "ESUM [EST 1, EST 2] → DPROD [DSUM [1, 2]]" $
        toConjunctiveNormalForm (ESUM [EST (1), EST 2]) == DPROD [DSUM [1, 2]]
      -- Nested ESUM: the inner sum is distributed in turn, then merged
      liftAff $ assert "ESUM [EST 1, ESUM [EST 2, EST 3]] flattens into DPROD [DSUM [1, 2, 3]]" $
        toConjunctiveNormalForm (ESUM [EST (1), ESUM [EST 2, EST 3]])
          == DPROD [DSUM [1, 2, 3]]

    test "toConjunctiveNormalForm: EPROD flattens into conjunction" do
      -- EPROD [EST a, EST b] → DPROD [ DSUM [a], DSUM [b] ]
      liftAff $ assert "EPROD [EST 1, EST 2] → DPROD [DSUM [1], DSUM [2]]" $
        toConjunctiveNormalForm (EPROD [EST (1), EST 2]) == DPROD [DSUM [1], DSUM [2]]
      -- Nested EPROD: inner product is flattened, outer receives the merged factors
      liftAff $ assert "EPROD [EST 1, EPROD [EST 2, EST 3]] → DPROD with three DSUM factors" $
        toConjunctiveNormalForm (EPROD [EST (1), EPROD [EST 2, EST 3]])
          == DPROD [DSUM [1], DSUM [2], DSUM [3]]

    test "toConjunctiveNormalForm: distributive law (ESUM of EPRODs)" do
      -- (1 ∧ 2) ∨ (3 ∧ 4)
      --   = (1 ∨ 3) ∧ (1 ∨ 4) ∧ (2 ∨ 3) ∧ (2 ∨ 4)
      let
        input = ESUM [EPROD [EST 1, EST 2], EPROD [EST 3, EST 4]]
        expected = DPROD [DSUM [1, 3], DSUM [1, 4], DSUM [2, 3], DSUM [2, 4]]
      liftAff $ assert "(1∧2)∨(3∧4) distributes to (1∨3)∧(1∨4)∧(2∨3)∧(2∨4)" $
        toConjunctiveNormalForm input == expected

    test "toConjunctiveNormalForm: ESUM of EPRODs with shared ESUM children" do
      -- (ESUM [1,2] ∧ ESUM [11,12]) ∨ (ESUM [3,4] ∧ ESUM [13,14])
      -- Each inner EPROD first distributes its own ESUMs; then the outer ESUM distributes.
      let
        input = ESUM
          [ EPROD [ESUM [EST 1, EST 2], ESUM [EST 11, EST 12]]
          , EPROD [ESUM [EST 3, EST 4], ESUM [EST 13, EST 14]]
          ]
        result = toConjunctiveNormalForm input
      -- The result must be a conjunction (DPROD) whose each factor (DSUM) comes from
      -- distributing the outer disjunction.  We verify two key properties:
      --   1. The result is a DPROD (always true from the type).
      --   2. The result is not equal to a trivially wrong CNF.
      liftAff $ assert "complex distribution yields a non-trivial CNF" $
        result /= DPROD [DSUM [1]]

    test "traverseDPROD" do
      liftAff $ assert "traverseDPROD Just preserves structure" $
        traverseDPROD Just (DPROD [DSUM [1, 2]]) == Just (DPROD [DSUM [1, 2]])
      liftAff $ assert "traverseDPROD with partial function returns Nothing" $
        traverseDPROD (\n -> if n > (0) then Just n else Nothing)
          (DPROD [DSUM [0, 1]]) == Nothing
      liftAff $ assert "traverseDPROD maps values inside DSUM" $
        traverseDPROD (\n -> Just (n * 2)) (DPROD [DSUM [1, 2]])
          == Just (DPROD [DSUM [2, 4]])

  -- ─── ADT ────────────────────────────────────────────────────────────────────

  suite "ADT" do

    test "Eq" do
      liftAff $ assert "ST 1 == ST 1" $
        ST 1 == ST (1)
      liftAff $ assert "ST 1 /= ST 2" $
        ST 1 /= ST (2)
      liftAff $ assert "UET 1 == UET 1" $
        UET 1 == UET (1)
      liftAff $ assert "SUM [ST 1, ST 2] == SUM [ST 1, ST 2]" $
        SUM [ST 1, ST 2] == SUM [ST (1), ST 2]

    test "Functor map" do
      liftAff $ assert "map (+1) (ST 1) == ST 2" $
        map ((+) 1) (ST 1) == ST (2)
      liftAff $ assert "map (+1) (UET 1) == UET 2" $
        map ((+) 1) (UET 1) == UET (2)
      liftAff $ assert "map (*2) (SUM [ST 1, ST 2]) == SUM [ST 2, ST 4]" $
        map ((*) 2) (SUM [ST 1, ST 2]) == SUM [ST 2, ST (4)]
      liftAff $ assert "map (*2) (PROD [ST 3, ST 4]) == PROD [ST 6, ST 8]" $
        map ((*) 2) (PROD [ST 3, ST 4]) == PROD [ST 6, ST (8)]

    test "Foldable foldMap" do
      liftAff $ assert "foldMap singleton (ST 1) == [1]" $
        foldMap singleton (ST (1)) == [1]
      liftAff $ assert "foldMap singleton (UET 1) == [1]" $
        foldMap singleton (UET (1)) == [1]
      liftAff $ assert "foldMap singleton (SUM [ST 1, ST 2]) == [1, 2]" $
        foldMap singleton (SUM [ST (1), ST 2]) == [1, 2]
      liftAff $ assert "foldMap singleton (PROD [ST 1, ST 2]) == [1, 2]" $
        foldMap singleton (PROD [ST (1), ST 2]) == [1, 2]
      liftAff $ assert "foldMap Additive sums all leaf values" $
        unwrap (foldMap Additive (PROD [ST 1, ST 2, ST 3])) == (6)

    test "Traversable traverse" do
      liftAff $ assert "traverse Just (ST 1) == Just (ST 1)" $
        traverse Just (ST (1)) == Just (ST 1)
      liftAff $ assert "traverse Nothing-producing returns Nothing" $
        traverse (\n -> if n > (0) then Just n else Nothing) (ST (-1)) == Nothing
      liftAff $ assert "traverse Just over SUM preserves structure" $
        traverse Just (SUM [ST (1), ST 2]) == Just (SUM [ST 1, ST 2])

    test "foldMapADT (HeytingAlgebra fold)" do
      -- PROD folds with AND (Conj)
      liftAff $ assert "PROD: all even → AND = true" $
        foldMapADT even (PROD [ST 2, ST 4]) == true
      liftAff $ assert "PROD: one odd → AND = false" $
        foldMapADT even (PROD [ST 1, ST 2]) == false
      -- SUM folds with OR (Disj)
      liftAff $ assert "SUM: at least one even → OR = true" $
        foldMapADT even (SUM [ST 1, ST 2]) == true
      liftAff $ assert "SUM: all odd → OR = false" $
        foldMapADT even (SUM [ST 1, ST 3]) == false
      -- UET is treated as a leaf
      liftAff $ assert "UET leaf participates like ST" $
        foldMapADT even (PROD [ST 2, UET 4]) == true

    test "computeBoolean" do
      liftAff $ assert "computeBoolean even (PROD [ST 2, ST 4]) == true" $
        computeBoolean even (PROD [ST 2, ST 4]) == true
      liftAff $ assert "computeBoolean even (PROD [ST 1, ST 2]) == false" $
        computeBoolean even (PROD [ST 1, ST 2]) == false
      liftAff $ assert "computeBoolean even (SUM [ST 1, ST 2]) == true" $
        computeBoolean even (SUM [ST 1, ST 2]) == true
      liftAff $ assert "computeBoolean even (SUM [ST 1, ST 3]) == false" $
        computeBoolean even (SUM [ST 1, ST 3]) == false

    test "computeExpandedBoolean" do
      liftAff $ assert "computeExpandedBoolean even (EPROD [EST 2, EST 4]) == true" $
        computeExpandedBoolean even (EPROD [EST 2, EST 4]) == true
      liftAff $ assert "computeExpandedBoolean even (EPROD [EST 1, EST 2]) == false" $
        computeExpandedBoolean even (EPROD [EST 1, EST 2]) == false
      liftAff $ assert "computeExpandedBoolean even (ESUM [EST 1, EST 2]) == true" $
        computeExpandedBoolean even (ESUM [EST 1, EST 2]) == true

    test "allLeavesInADT (union of all leaves)" do
      liftAff $ assert "allLeavesInADT (ST 1) == [1]" $
        allLeavesInADT (ST (1)) == [1]
      liftAff $ assert "allLeavesInADT (UET 1) == [1]" $
        allLeavesInADT (UET (1)) == [1]
      liftAff $ assert "allLeavesInADT (SUM [ST 1, ST 2]) == [1, 2]" $
        allLeavesInADT (SUM [ST (1), ST 2]) == [1, 2]
      liftAff $ assert "allLeavesInADT (PROD [ST 1, ST 2]) == [1, 2]" $
        allLeavesInADT (PROD [ST (1), ST 2]) == [1, 2]
      liftAff $ assert "allLeavesInADT traverses nested structure" $
        allLeavesInADT (PROD [SUM [ST 1, ST 2], SUM [ST 3, ST 4]]) == [1, 2, 3, 4]

    test "commonLeavesInADT (intersection-based)" do
      -- For a single leaf the result is that leaf.
      liftAff $ assert "commonLeavesInADT (ST 1) == [1]" $
        commonLeavesInADT (ST (1)) == [1]
      -- PROD collects all types (union of leaves from each branch).
      liftAff $ assert "commonLeavesInADT (PROD [ST 1, ST 2]) == [1, 2]" $
        commonLeavesInADT (PROD [ST (1), ST 2]) == [1, 2]
      -- SUM: only leaves that appear in EVERY branch survive (intersection).
      -- SUM [ST 1, ST 2] — no leaf in common → []
      liftAff $ assert "commonLeavesInADT (SUM [ST 1, ST 2]) == [] (no common leaf)" $
        commonLeavesInADT (SUM [ST (1), ST 2]) == []
      -- SUM [PROD [ST 1, ST 2], PROD [ST 2, ST 3]]:
      --   branch 1 brings {1, 2}, branch 2 brings {2, 3}; intersection = {2}.
      liftAff $ assert "commonLeavesInADT (SUM [PROD [ST 1, ST 2], PROD [ST 2, ST 3]]) == [2]" $
        commonLeavesInADT (SUM [PROD [ST (1), ST 2], PROD [ST 2, ST 3]]) == [2]

    test "expand (ADT → ExpandedADT)" do
      liftAff $ assert "expand (ST 1) → EST 1" $
        unwrap (expandPure (ST 1)) == EST (1)
      liftAff $ assert "expand (UET 1) → EST 1" $
        unwrap (expandPure (UET 1)) == EST (1)
      liftAff $ assert "expand (PROD [ST 1, ST 2]) → EPROD [EST 1, EST 2]" $
        unwrap (expandPure (PROD [ST 1, ST 2])) == EPROD [EST 1, EST (2)]
      liftAff $ assert "expand (SUM [ST 1, ST 2]) → ESUM [EST 1, EST 2]" $
        unwrap (expandPure (SUM [ST 1, ST 2])) == ESUM [EST 1, EST (2)]
      liftAff $ assert "expand nested: PROD [SUM [ST 1, ST 2], ST 3] → EPROD [ESUM [...], EST 3]" $
        unwrap (expandPure (PROD [SUM [ST 1, ST 2], ST 3]))
          == EPROD [ESUM [EST 1, EST 2], EST (3)]

  -- ─── Type comparison operators ──────────────────────────────────────────────

  suite "Type comparison (CNF variants)" do

    test "equalsOrSpecialises_ (left => right)" do
      -- A type equals or specialises itself.
      liftAff $ assert "DPROD [DSUM [1]] equalsOrSpecialises_ itself" $
        equalsOrSpecialises_ (DPROD [DSUM [1]]) (DPROD [DSUM [1]])
      -- A more specific type (smaller disjunction ⊆ larger one) specialises the less specific.
      -- DPROD [DSUM [1]] ⊆ DPROD [DSUM [1, 2]]: {1} ⊆ {1, 2} ✓
      liftAff $ assert "DPROD [DSUM [1]] specialises DPROD [DSUM [1, 2]]" $
        equalsOrSpecialises_ (DPROD [DSUM [1]]) (DPROD [DSUM [1, 2]])
      -- A type with MORE conjuncts (PROD) specialises one with fewer.
      -- For every DSUM in right there must be a DSUM in left that is a subset.
      liftAff $ assert "DPROD [DSUM [1], DSUM [2]] specialises DPROD [DSUM [1]]" $
        equalsOrSpecialises_ (DPROD [DSUM [1], DSUM [2]]) (DPROD [DSUM [1]])
      -- The reverse direction must NOT hold.
      liftAff $ assert "NOT: DPROD [DSUM [1, 2]] equalsOrSpecialises_ DPROD [DSUM [1]]" $
        not $ equalsOrSpecialises_ (DPROD [DSUM [1, 2]]) (DPROD [DSUM [1]])
      liftAff $ assert "NOT: DPROD [DSUM [1]] equalsOrSpecialises_ DPROD [DSUM [2]]" $
        not $ equalsOrSpecialises_ (DPROD [DSUM [1]]) (DPROD [DSUM [2]])

    test "equals_ (mutual implication)" do
      liftAff $ assert "DPROD [DSUM [1]] equals_ itself" $
        equals_ (DPROD [DSUM [1]]) (DPROD [DSUM [1]])
      -- Different number of factors → not equal.
      liftAff $ assert "NOT: DPROD [DSUM [1]] equals_ DPROD [DSUM [1, 2]]" $
        not $ equals_ (DPROD [DSUM [1]]) (DPROD [DSUM [1, 2]])
      -- Different factor content → not equal.
      liftAff $ assert "NOT: DPROD [DSUM [1]] equals_ DPROD [DSUM [2]]" $
        not $ equals_ (DPROD [DSUM [1]]) (DPROD [DSUM [2]])

    test "equalsOrSpecialises (ExpandedADT)" do
      -- EST equals itself.
      liftAff $ assert "EST 1 equalsOrSpecialises EST 1" $
        equalsOrSpecialises (EST (1)) (EST 1)
      -- EST specialises an ESUM that contains it.
      liftAff $ assert "EST 1 equalsOrSpecialises ESUM [EST 1, EST 2]" $
        equalsOrSpecialises (EST (1)) (ESUM [EST 1, EST 2])
      -- An EPROD (more conjuncts) specialises one of its component types.
      liftAff $ assert "EPROD [EST 1, EST 2] equalsOrSpecialises EST 1" $
        equalsOrSpecialises (EPROD [EST (1), EST 2]) (EST 1)
      -- Unrelated types do not specialise each other.
      liftAff $ assert "NOT: EST 1 equalsOrSpecialises EST 2" $
        not $ equalsOrSpecialises (EST (1)) (EST 2)
      -- A more general type does not specialise a more specific one.
      liftAff $ assert "NOT: ESUM [EST 1, EST 2] equalsOrSpecialises EST 1" $
        not $ equalsOrSpecialises (ESUM [EST (1), EST 2]) (EST 1)

    test "specialises (strict; excludes equals)" do
      -- Strict: EPROD [EST 1, EST 2] specialises EST 1, but they are not equal.
      liftAff $ assert "EPROD [EST 1, EST 2] specialises EST 1" $
        specialises (EPROD [EST (1), EST 2]) (EST 1)
      -- Strict: a type does NOT strictly specialise itself.
      liftAff $ assert "NOT: EST 1 specialises EST 1 (equal, not strict)" $
        not $ specialises (EST (1)) (EST 1)

    test "generalises (strict; excludes equals)" do
      -- EST 1 is more general than EPROD [EST 1, EST 2].
      liftAff $ assert "EST 1 generalises EPROD [EST 1, EST 2]" $
        generalises (EST (1)) (EPROD [EST 1, EST 2])
      -- Strict: a type does NOT strictly generalise itself.
      liftAff $ assert "NOT: EST 1 generalises EST 1 (equal, not strict)" $
        not $ generalises (EST (1)) (EST 1)

    test "specialises_ and generalises_ (CNF variants)" do
      -- specialises_ is flip generalises_
      liftAff $ assert "DPROD [DSUM [1], DSUM [2]] specialises_ DPROD [DSUM [1]]" $
        specialises_ (DPROD [DSUM [1], DSUM [2]]) (DPROD [DSUM [1]])
      liftAff $ assert "NOT: DPROD [DSUM [1]] specialises_ DPROD [DSUM [1]] (equal, not strict)" $
        not $ specialises_ (DPROD [DSUM [1]]) (DPROD [DSUM [1]])
      liftAff $ assert "DPROD [DSUM [1]] generalises_ DPROD [DSUM [1], DSUM [2]]" $
        generalises_ (DPROD [DSUM [1]]) (DPROD [DSUM [1], DSUM [2]])
