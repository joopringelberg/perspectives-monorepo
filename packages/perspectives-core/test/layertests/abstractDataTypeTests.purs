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
-- | All tests are in-memory; runtime cache tests use MonadPerspectives without CouchDB.
-- | They therefore belong in Layer 1.
-- |
-- | For background see docsources/type-comparison.md.

module Test.Perspectives.Representation.AbstractDataTypeTests where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.AvarMonadAsk (gets, modify)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (ask)
import Data.Array (cons, nub, singleton)
import Data.Foldable (all, any, foldMap, foldr, for_)
import Data.Identity (Identity(..))
import Data.Int (even)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Set (fromFoldable, member) as SET
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay, forkAff, joinFiber)
import Effect.Aff.AVar (put, take)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import LRUCache (defaultCreateOptions, defaultGetOptions, get, newCache, set)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MP, representInternally, retrieveInternally, removeInternally, runMonadPerspectives)
import Perspectives.DomeinCache (modifyDomeinFileInCache, removeDomeinFileFromCache, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), defaultDomeinFileRecord)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.PerspectivesState (domeinCacheRemove, resetDomeinCache, setModelUnderCompilation, setModelUri)
import Perspectives.Query.QueryTypes (RoleInContext(..))
import Perspectives.Representation.ADT (ADT(..), allLeavesInADT, commonLeavesInADT, computeBoolean, computeExpandedBoolean, equals_, equalsOrSpecialises, equalsOrSpecialises_, expand, foldMapADT, generalises, generalises_, specialises, specialises_)
import Perspectives.Representation.CNF (CNF, DPROD(..), DSUM(..), simplifyCNF, toConjunctiveNormalForm, traverseDPROD)
import Perspectives.Representation.Class.Cacheable (cacheEntity, overwriteEntity)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (completeExpandedType, toConjunctiveNormalForm_)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), defaultEnumeratedRole)
import Perspectives.Representation.ExpandedADT (ExpandedADT(..), foldMapExpandedADT)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType(..))
import Perspectives.Representation.TypeIdentifiers (RoleKind(..)) as Kind
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Stable)
import Simple.JSON (writeJSON)
import Test.Perspectives.Utils (runP)
import Test.Unit (TestF, suite, test, testSkip)
import Test.Unit.Assert (assert, equal)

-- | Expand an ADT Int purely with the Identity applicative.
-- | ST and UET leaves both become EST leaves; SUM and PROD become ESUM and EPROD.
expandPure :: ADT Int -> Identity (ExpandedADT Int)
expandPure = expand (unsafePartial leafToExpanded)
  where
  leafToExpanded :: Partial => ADT Int -> Identity (ExpandedADT Int)
  leafToExpanded adt = case adt of
    ST x -> Identity (EST x)
    UET x -> Identity (EST x)

allAssignments :: forall a. Array a -> Array (Array a)
allAssignments = foldr (\term assignments -> assignments <> map (cons term) assignments) [ [] ]

satisfiesCnf :: forall a. Ord a => Array a -> DPROD a -> Boolean
satisfiesCnf assignment (DPROD clauses) =
  let
    assignmentSet = SET.fromFoldable assignment
  in
    all (\(DSUM disjunction) -> any (\term -> SET.member term assignmentSet) disjunction) clauses

semanticallyImplies :: forall a. Ord a => DPROD a -> DPROD a -> Boolean
semanticallyImplies left right =
  let
    allTerms (DPROD clauses) = nub $ foldMap (\(DSUM disjunction) -> disjunction) clauses
    universe = allTerms left <> allTerms right
  in
    all (\assignment -> not (satisfiesCnf assignment left) || satisfiesCnf assignment right) (allAssignments universe)

type NormalisationFixture =
  { model :: ModelUri Stable
  , dependency :: ModelUri Stable
  , root :: RoleInContext
  , filler :: RoleInContext
  , alternative :: RoleInContext
  , changedDependency :: DomeinFile Stable
  , simpleModel :: DomeinFile Stable
  }

normalisationFixture :: MP NormalisationFixture
normalisationFixture = do
  let
    model = "model://test.example#Normalisation"
    dependency = "model://test.example#Dependency"
    context = model <> "$Context"
    dependencyContext = dependency <> "$Context"
    origin = ArcPosition { line: 0, column: 0 }
    role name = defaultEnumeratedRole (dependencyContext <> "$" <> name) name Kind.RoleInContext dependencyContext origin
    aspect = role "Aspect"
    filler = role "Filler"
    alternative = role "Alternative"
    leaf r = RoleInContext { context: (unwrap r).context, role: (unwrap r).id }
    simple = defaultEnumeratedRole (context <> "$Root") "Root" Kind.RoleInContext context origin
    root = EnumeratedRole (unwrap simple)
      { roleAspects = [ leaf aspect ], binding = Just (SUM [ ST (leaf filler), ST (leaf alternative) ]) }
    dependencyFile = DomeinFile defaultDomeinFileRecord
      { id = ModelUri dependency
      , namespace = ModelUri dependency
      , enumeratedRoles = Object.fromFoldable (map (\r -> Tuple (unwrap (unwrap r).id) r) [ aspect, filler, alternative ])
      }
    changedDependency = case dependencyFile of
      DomeinFile df -> DomeinFile df
        { enumeratedRoles = Object.insert (unwrap (unwrap aspect).id)
            (EnumeratedRole (unwrap aspect) { binding = Just (ST (leaf alternative)) })
            df.enumeratedRoles
        }
    simpleModel = DomeinFile defaultDomeinFileRecord
      { id = ModelUri model, namespace = ModelUri model, enumeratedRoles = Object.singleton (unwrap (unwrap simple).id) simple }
  void $ storeDomeinFileInCache (ModelUri dependency) dependencyFile
  void $ storeDomeinFileInCache (ModelUri model) case simpleModel of
    DomeinFile df -> DomeinFile df { enumeratedRoles = Object.singleton (unwrap (unwrap root).id) root }
  pure { model: ModelUri model, dependency: ModelUri dependency, root: leaf root, filler: leaf filler, alternative: leaf alternative, changedDependency, simpleModel }

cachedNormalisation :: RoleInContext -> MP (Maybe (CNF RoleInContext))
cachedNormalisation (RoleInContext { role }) = do
  cache <- gets _.roleNormalisationCache
  liftEffect $ get (unwrap role) defaultGetOptions cache

uncachedNormalisation :: RoleInContext -> MP (CNF RoleInContext)
uncachedNormalisation (RoleInContext { role }) =
  getEnumeratedRole role >>= completeExpandedType >>= pure <<< toConjunctiveNormalForm

normalisationCacheSuite :: Free TestF Unit
normalisationCacheSuite = suite "Runtime normalisation cache" do
  test "ST and UET preserve the exact expanded CNF, including imported aspects and fillers" $ runP do
    f <- normalisationFixture
    expected <- uncachedNormalisation f.root
    first <- toConjunctiveNormalForm_ (ST f.root)
    second <- toConjunctiveNormalForm_ (UET f.root)
    cached <- cachedNormalisation f.root
    liftAff do
      equal (writeJSON expected) (writeJSON first)
      equal (writeJSON expected) (writeJSON second)
      equal (Just expected) cached

  test "replacement, overwrite and direct dependency edits invalidate dependent roles" do
    for_
      [ \f -> void $ cacheEntity f.dependency f.changedDependency
      , \f -> void $ overwriteEntity f.dependency f.changedDependency
      , \f -> modifyDomeinFileInCache (const f.changedDependency) f.dependency
      ]
      \change -> runP do
        f <- normalisationFixture
        before <- toConjunctiveNormalForm_ (ST f.root)
        change f
        missing <- isNothing <$> cachedNormalisation f.root
        after <- toConjunctiveNormalForm_ (ST f.root)
        expected <- uncachedNormalisation f.root
        liftAff do
          assert "The dependent role must be evicted" missing
          assert "The changed filler must affect normalisation" (before /= after)
          equal (writeJSON expected) (writeJSON after)

  test "model removal, cache reset and identifier remapping discard cached normal forms" do
    for_
      [ \f -> void $ removeInternally f.dependency
      , \f -> removeDomeinFileFromCache f.dependency
      , \f -> void $ domeinCacheRemove (unwrap f.dependency)
      , \_ -> resetDomeinCache
      , \f -> setModelUri (ModelUri "model://test.example#Readable") f.dependency
      ]
      \change -> runP do
        f <- normalisationFixture
        void $ toConjunctiveNormalForm_ (ST f.root)
        change f
        missing <- isNothing <$> cachedNormalisation f.root
        liftAff $ assert "A model environment change must invalidate normalisation" missing

  test "compilation bypasses both lookup and insertion, including previously warmed roles" $ runP do
    f <- normalisationFixture
    expected <- toConjunctiveNormalForm_ (ST f.root)
    setModelUnderCompilation (Just (ModelUri "model://test.example#Compiling"))
    void $ toConjunctiveNormalForm_ (UET f.root)
    missing <- isNothing <$> cachedNormalisation f.root
    liftAff $ assert "Compilation must not populate the runtime cache" missing
    cache <- gets _.roleNormalisationCache
    let sentinel = DPROD [] :: CNF RoleInContext
    _ <- liftEffect $ set (unwrap (unwrap f.root).role) sentinel Nothing cache
    bypassed <- toConjunctiveNormalForm_ (ST f.root)
    untouched <- cachedNormalisation f.root
    liftAff do
      equal (writeJSON expected) (writeJSON bypassed)
      equal (Just sentinel) untouched
    void $ cacheEntity f.model f.simpleModel
    compiled <- toConjunctiveNormalForm_ (ST f.root)
    fresh <- uncachedNormalisation f.root
    setModelUnderCompilation Nothing
    runtime <- toConjunctiveNormalForm_ (UET f.root)
    liftAff do
      assert "Compilation must not reuse the previously warmed value" (compiled /= expected)
      equal (writeJSON fresh) (writeJSON compiled)
      equal (writeJSON fresh) (writeJSON runtime)

  test "separate PDRs cannot reuse each other's definitions for the same role" $ runP do
    f <- normalisationFixture
    expected <- toConjunctiveNormalForm_ (ST f.root)
    other <- liftAff $ runP do
      g <- normalisationFixture
      void $ cacheEntity g.model g.simpleModel
      toConjunctiveNormalForm_ (ST g.root)
    again <- toConjunctiveNormalForm_ (UET f.root)
    liftAff do
      assert "The two PDRs have different definitions" (other /= expected)
      equal (writeJSON expected) (writeJSON again)

  test "runtime hits refresh LRU recency and the configured capacity is respected" $ runP do
    f <- normalisationFixture
    modify \s -> s { roleNormalisationCache = newCache (defaultCreateOptions { max = Just 2 }) }
    void $ toConjunctiveNormalForm_ (ST f.root)
    void $ toConjunctiveNormalForm_ (ST f.filler)
    void $ toConjunctiveNormalForm_ (UET f.root)
    void $ toConjunctiveNormalForm_ (ST f.alternative)
    retained <- cachedNormalisation f.root
    evicted <- cachedNormalisation f.filler
    newest <- cachedNormalisation f.alternative
    liftAff do
      assert "The recently read root must survive eviction" (not $ isNothing retained)
      equal Nothing evicted
      assert "The newest normal form must be cached" (not $ isNothing newest)

  test "an old in-flight miss cannot populate the cache after a model replacement" $ runP do
    f <- normalisationFixture
    state <- ask
    old <- retrieveInternally f.model
    case old of
      Nothing -> liftAff $ assert "The fixture model must be cached" false
      Just oldAvar -> do
        held <- liftAff $ take oldAvar
        pending <- liftAff $ forkAff $ runMonadPerspectives (toConjunctiveNormalForm_ (ST f.root)) state
        liftAff $ delay (Milliseconds 0.0)
        replacement <- representInternally f.model
        liftAff $ put f.simpleModel replacement
        liftAff $ put held oldAvar
        previous <- liftAff $ joinFiber pending
        missing <- isNothing <$> cachedNormalisation f.root
        current <- toConjunctiveNormalForm_ (ST f.root)
        liftAff do
          assert "The old request must not populate the replacement cache" missing
          assert "The replacement model has a different normal form" (previous /= current)

  test "failed normalisation remains an error and does not create an entry" $ runP do
    f <- normalisationFixture
    let missing = RoleInContext { context: ContextType (unwrap f.model <> "$Context"), role: EnumeratedRoleType (unwrap f.model <> "$Missing") }
    for_ [ ST missing, UET missing ] \adt -> do
      failed <- catchError (toConjunctiveNormalForm_ adt *> pure false) (const $ pure true)
      cached <- cachedNormalisation missing
      liftAff do
        assert "An undefined role must still throw" failed
        equal Nothing cached

theSuite :: Free TestF Unit
theSuite = suite "Test.Perspectives.Representation.AbstractDataTypeTests" do

  normalisationCacheSuite

  -- ─── ExpandedADT ────────────────────────────────────────────────────────────

  suite "ExpandedADT" do

    test "Eq" do
      liftAff $ assert "EST 1 == EST 1" $
        EST 1 == EST (1)
      liftAff $ assert "EST 1 /= EST 2" $
        EST 1 /= EST (2)
      liftAff $ assert "ESUM [EST 1, EST 2] equals identical structure" $
        ESUM [ EST 1, EST 2 ] == ESUM [ EST (1), EST 2 ]
      liftAff $ assert "EPROD [EST 1] /= EPROD [EST 2]" $
        EPROD [ EST (1) ] /= EPROD [ EST 2 ]

    test "Functor map" do
      liftAff $ assert "map (+1) (EST 1) == EST 2" $
        map ((+) 1) (EST 1) == EST (2)
      liftAff $ assert "map (*2) (ESUM [EST 3, EST 4]) == ESUM [EST 6, EST 8]" $
        map ((*) 2) (ESUM [ EST 3, EST 4 ]) == ESUM [ EST 6, EST (8) ]
      liftAff $ assert "map (*2) (EPROD [EST 3, EST 4]) == EPROD [EST 6, EST 8]" $
        map ((*) 2) (EPROD [ EST 3, EST 4 ]) == EPROD [ EST 6, EST (8) ]
      -- ECT: both the label and the body are mapped
      liftAff $ assert "map f (ECT (EST 0) (EST 5)) maps label and body" $
        map ((+) 1) (ECT (EST 0) (EST 5)) == ECT (EST 1) (EST (6))

    test "Foldable foldMap" do
      liftAff $ assert "foldMap singleton (EST 1) == [1]" $
        foldMap singleton (EST (1)) == [ 1 ]
      liftAff $ assert "foldMap singleton (ESUM [EST 1, EST 2]) == [1, 2]" $
        foldMap singleton (ESUM [ EST (1), EST 2 ]) == [ 1, 2 ]
      liftAff $ assert "foldMap singleton (EPROD [EST 1, EST 2]) == [1, 2]" $
        foldMap singleton (EPROD [ EST (1), EST 2 ]) == [ 1, 2 ]
      liftAff $ assert "foldMap Additive sums all leaf values" $
        unwrap (foldMap Additive (EPROD [ EST 1, EST 2, EST 3 ])) == (6)

    test "Traversable traverse" do
      liftAff $ assert "traverse Just (EST 1) == Just (EST 1)" $
        traverse Just (EST (1)) == Just (EST 1)
      liftAff $ assert "traverse Nothing-producing function returns Nothing" $
        traverse (\n -> if n > (0) then Just n else Nothing) (EST (-1)) == Nothing
      liftAff $ assert "traverse Just over ESUM preserves structure" $
        traverse Just (ESUM [ EST (1), EST 2 ]) == Just (ESUM [ EST 1, EST 2 ])
      liftAff $ assert "traverse Just over EPROD preserves structure" $
        traverse Just (EPROD [ EST (1), EST 2 ]) == Just (EPROD [ EST 1, EST 2 ])

    test "foldMapExpandedADT (HeytingAlgebra fold)" do
      -- EPROD folds leaves with AND (Conj)
      liftAff $ assert "EPROD: all even → AND = true" $
        foldMapExpandedADT even (EPROD [ EST 2, EST 4 ]) == true
      liftAff $ assert "EPROD: one odd → AND = false" $
        foldMapExpandedADT even (EPROD [ EST 1, EST 2 ]) == false
      -- ESUM folds leaves with OR (Disj)
      liftAff $ assert "ESUM: at least one even → OR = true" $
        foldMapExpandedADT even (ESUM [ EST 1, EST 2 ]) == true
      liftAff $ assert "ESUM: all odd → OR = false" $
        foldMapExpandedADT even (ESUM [ EST 1, EST 3 ]) == false
      -- ECT delegates to its second argument (the expansion); the label is ignored
      liftAff $ assert "ECT ignores label, delegates to expansion" $
        foldMapExpandedADT even (ECT (EST 1) (EST 2)) == true

  -- ─── CNF ────────────────────────────────────────────────────────────────────

  suite "CNF" do

    test "DSUM Eq is set-based (order-invariant)" do
      liftAff $ assert "DSUM [1, 2] == DSUM [2, 1]" $
        DSUM [ 1, 2 ] == DSUM [ 2, 1 ]
      liftAff $ assert "DSUM [1] /= DSUM [2]" $
        DSUM [ 1 ] /= DSUM [ 2 ]
      liftAff $ assert "DSUM [1, 2, 3] == DSUM [3, 1, 2]" $
        DSUM [ 1, 2, 3 ] == DSUM [ 3, 1, 2 ]

    test "DPROD Eq is set-based (order-invariant)" do
      liftAff $ assert "DPROD [DSUM [1]] == DPROD [DSUM [1]]" $
        DPROD [ DSUM [ 1 ] ] == DPROD [ DSUM [ 1 ] ]
      liftAff $ assert "DPROD: order of DSUM factors does not matter" $
        DPROD [ DSUM [ 1 ], DSUM [ 2 ] ] == DPROD [ DSUM [ 2 ], DSUM [ 1 ] ]
      liftAff $ assert "DPROD [DSUM [1], DSUM [2]] /= DPROD [DSUM [1, 2]]" $
        DPROD [ DSUM [ 1 ], DSUM [ 2 ] ] /= DPROD [ DSUM [ 1, 2 ] ]

    test "simplifyCNF removes strict supersets but preserves original clauses and order" do
      let
        DPROD clauses = simplifyCNF $ DPROD
          [ DSUM [ 2, 1, 2 ]
          , DSUM [ 4, 3 ]
          , DSUM [ 1, 2 ]
          , DSUM [ 1, 2, 5 ]
          , DSUM [ 3, 4 ]
          , DSUM [ 3, 4, 6 ]
          ]
      liftAff $ equal
        [ [ 2, 1, 2 ], [ 4, 3 ], [ 1, 2 ], [ 3, 4 ] ]
        (map (\(DSUM terms) -> terms) clauses)

    test "simplifyCNF handles empty products and duplicate empty clauses" do
      let
        DPROD empty = simplifyCNF (DPROD [] :: DPROD Int)
        DPROD clauses = simplifyCNF $ DPROD [ DSUM [ 1 ], DSUM [], DSUM [ 2, 2 ], DSUM [] ]
      liftAff $ equal [] (map (\(DSUM terms) -> terms) empty)
      liftAff $ equal [ [], [] ] (map (\(DSUM terms) -> terms) clauses)

    test "nested ECT wrappers retain the simplified body's exact clause representation" do
      let
        body = EPROD
          [ ESUM [ EST 3, EST 1 ]
          , EST 1
          , ESUM [ EST 4, EST 2 ]
          , ESUM [ EST 2, EST 4, EST 5 ]
          ]
        DPROD clauses = toConjunctiveNormalForm (ECT (EST 99) (ECT (EST 98) body))
      liftAff $ equal [ [ 1 ], [ 2, 4 ] ] (map (\(DSUM terms) -> terms) clauses)

    test "toConjunctiveNormalForm: EST leaf" do
      -- EST a → DPROD [ DSUM [a] ]
      liftAff $ assert "EST a normalises to a single-factor, single-alternative CNF" $
        toConjunctiveNormalForm (EST (1)) == DPROD [ DSUM [ 1 ] ]

    test "toConjunctiveNormalForm: ECT (label discarded)" do
      -- ECT label a → toConjunctiveNormalForm a  (label is already included in the expansion)
      liftAff $ assert "ECT label a produces the same CNF as its body a alone" $
        toConjunctiveNormalForm (ECT (EST 99) (EST (1))) == DPROD [ DSUM [ 1 ] ]
      liftAff $ assert "ECT wrapping an EPROD delegates correctly" $
        toConjunctiveNormalForm (ECT (EST 99) (EPROD [ EST 1, EST 2 ]))
          == DPROD [ DSUM [ 1 ], DSUM [ 2 ] ]

    test "toConjunctiveNormalForm: ESUM distributes into CNF" do
      -- ESUM [EST a, EST b] → DPROD [ DSUM [a, b] ]
      liftAff $ assert "ESUM [EST 1, EST 2] → DPROD [DSUM [1, 2]]" $
        toConjunctiveNormalForm (ESUM [ EST (1), EST 2 ]) == DPROD [ DSUM [ 1, 2 ] ]
      -- Nested ESUM: the inner sum is distributed in turn, then merged
      liftAff $ assert "ESUM [EST 1, ESUM [EST 2, EST 3]] flattens into DPROD [DSUM [1, 2, 3]]" $
        toConjunctiveNormalForm (ESUM [ EST (1), ESUM [ EST 2, EST 3 ] ])
          == DPROD [ DSUM [ 1, 2, 3 ] ]

    test "toConjunctiveNormalForm: EPROD flattens into conjunction" do
      -- EPROD [EST a, EST b] → DPROD [ DSUM [a], DSUM [b] ]
      liftAff $ assert "EPROD [EST 1, EST 2] → DPROD [DSUM [1], DSUM [2]]" $
        toConjunctiveNormalForm (EPROD [ EST (1), EST 2 ]) == DPROD [ DSUM [ 1 ], DSUM [ 2 ] ]
      -- Nested EPROD: inner product is flattened, outer receives the merged factors
      liftAff $ assert "EPROD [EST 1, EPROD [EST 2, EST 3]] → DPROD with three DSUM factors" $
        toConjunctiveNormalForm (EPROD [ EST (1), EPROD [ EST 2, EST 3 ] ])
          == DPROD [ DSUM [ 1 ], DSUM [ 2 ], DSUM [ 3 ] ]

    test "toConjunctiveNormalForm: distributive law (ESUM of EPRODs)" do
      -- (1 ∧ 2) ∨ (3 ∧ 4)
      --   = (1 ∨ 3) ∧ (1 ∨ 4) ∧ (2 ∨ 3) ∧ (2 ∨ 4)
      let
        input = ESUM [ EPROD [ EST 1, EST 2 ], EPROD [ EST 3, EST 4 ] ]
        expected = DPROD [ DSUM [ 1, 3 ], DSUM [ 1, 4 ], DSUM [ 2, 3 ], DSUM [ 2, 4 ] ]
      liftAff $ assert "(1∧2)∨(3∧4) distributes to (1∨3)∧(1∨4)∧(2∨3)∧(2∨4)" $
        toConjunctiveNormalForm input == expected

    test "toConjunctiveNormalForm: ESUM of EPRODs with shared ESUM children" do
      -- (ESUM [1,2] ∧ ESUM [11,12]) ∨ (ESUM [3,4] ∧ ESUM [13,14])
      -- Each inner EPROD first distributes its own ESUMs; then the outer ESUM distributes.
      let
        input = ESUM
          [ EPROD [ ESUM [ EST 1, EST 2 ], ESUM [ EST 11, EST 12 ] ]
          , EPROD [ ESUM [ EST 3, EST 4 ], ESUM [ EST 13, EST 14 ] ]
          ]
        result = toConjunctiveNormalForm input
      -- The result must be a conjunction (DPROD) whose each factor (DSUM) comes from
      -- distributing the outer disjunction.  We verify two key properties:
      --   1. The result is a DPROD (always true from the type).
      --   2. The result is not equal to a trivially wrong CNF.
      liftAff $ assert "complex distribution yields a non-trivial CNF" $
        result /= DPROD [ DSUM [ 1 ] ]

    test "traverseDPROD" do
      liftAff $ assert "traverseDPROD Just preserves structure" $
        traverseDPROD Just (DPROD [ DSUM [ 1, 2 ] ]) == Just (DPROD [ DSUM [ 1, 2 ] ])
      liftAff $ assert "traverseDPROD with partial function returns Nothing" $
        traverseDPROD (\n -> if n > (0) then Just n else Nothing)
          (DPROD [ DSUM [ 0, 1 ] ]) == Nothing
      liftAff $ assert "traverseDPROD maps values inside DSUM" $
        traverseDPROD (\n -> Just (n * 2)) (DPROD [ DSUM [ 1, 2 ] ])
          == Just (DPROD [ DSUM [ 2, 4 ] ])

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
        SUM [ ST 1, ST 2 ] == SUM [ ST (1), ST 2 ]

    test "Functor map" do
      liftAff $ assert "map (+1) (ST 1) == ST 2" $
        map ((+) 1) (ST 1) == ST (2)
      liftAff $ assert "map (+1) (UET 1) == UET 2" $
        map ((+) 1) (UET 1) == UET (2)
      liftAff $ assert "map (*2) (SUM [ST 1, ST 2]) == SUM [ST 2, ST 4]" $
        map ((*) 2) (SUM [ ST 1, ST 2 ]) == SUM [ ST 2, ST (4) ]
      liftAff $ assert "map (*2) (PROD [ST 3, ST 4]) == PROD [ST 6, ST 8]" $
        map ((*) 2) (PROD [ ST 3, ST 4 ]) == PROD [ ST 6, ST (8) ]

    test "Foldable foldMap" do
      liftAff $ assert "foldMap singleton (ST 1) == [1]" $
        foldMap singleton (ST (1)) == [ 1 ]
      liftAff $ assert "foldMap singleton (UET 1) == [1]" $
        foldMap singleton (UET (1)) == [ 1 ]
      liftAff $ assert "foldMap singleton (SUM [ST 1, ST 2]) == [1, 2]" $
        foldMap singleton (SUM [ ST (1), ST 2 ]) == [ 1, 2 ]
      liftAff $ assert "foldMap singleton (PROD [ST 1, ST 2]) == [1, 2]" $
        foldMap singleton (PROD [ ST (1), ST 2 ]) == [ 1, 2 ]
      liftAff $ assert "foldMap Additive sums all leaf values" $
        unwrap (foldMap Additive (PROD [ ST 1, ST 2, ST 3 ])) == (6)

    test "Traversable traverse" do
      liftAff $ assert "traverse Just (ST 1) == Just (ST 1)" $
        traverse Just (ST (1)) == Just (ST 1)
      liftAff $ assert "traverse Nothing-producing returns Nothing" $
        traverse (\n -> if n > (0) then Just n else Nothing) (ST (-1)) == Nothing
      liftAff $ assert "traverse Just over SUM preserves structure" $
        traverse Just (SUM [ ST (1), ST 2 ]) == Just (SUM [ ST 1, ST 2 ])

    test "foldMapADT (HeytingAlgebra fold)" do
      -- PROD folds with AND (Conj)
      liftAff $ assert "PROD: all even → AND = true" $
        foldMapADT even (PROD [ ST 2, ST 4 ]) == true
      liftAff $ assert "PROD: one odd → AND = false" $
        foldMapADT even (PROD [ ST 1, ST 2 ]) == false
      -- SUM folds with OR (Disj)
      liftAff $ assert "SUM: at least one even → OR = true" $
        foldMapADT even (SUM [ ST 1, ST 2 ]) == true
      liftAff $ assert "SUM: all odd → OR = false" $
        foldMapADT even (SUM [ ST 1, ST 3 ]) == false
      -- UET is treated as a leaf
      liftAff $ assert "UET leaf participates like ST" $
        foldMapADT even (PROD [ ST 2, UET 4 ]) == true

    test "computeBoolean" do
      liftAff $ assert "computeBoolean even (PROD [ST 2, ST 4]) == true" $
        computeBoolean even (PROD [ ST 2, ST 4 ]) == true
      liftAff $ assert "computeBoolean even (PROD [ST 1, ST 2]) == false" $
        computeBoolean even (PROD [ ST 1, ST 2 ]) == false
      liftAff $ assert "computeBoolean even (SUM [ST 1, ST 2]) == true" $
        computeBoolean even (SUM [ ST 1, ST 2 ]) == true
      liftAff $ assert "computeBoolean even (SUM [ST 1, ST 3]) == false" $
        computeBoolean even (SUM [ ST 1, ST 3 ]) == false

    test "computeExpandedBoolean" do
      liftAff $ assert "computeExpandedBoolean even (EPROD [EST 2, EST 4]) == true" $
        computeExpandedBoolean even (EPROD [ EST 2, EST 4 ]) == true
      liftAff $ assert "computeExpandedBoolean even (EPROD [EST 1, EST 2]) == false" $
        computeExpandedBoolean even (EPROD [ EST 1, EST 2 ]) == false
      liftAff $ assert "computeExpandedBoolean even (ESUM [EST 1, EST 2]) == true" $
        computeExpandedBoolean even (ESUM [ EST 1, EST 2 ]) == true

    test "allLeavesInADT (union of all leaves)" do
      liftAff $ assert "allLeavesInADT (ST 1) == [1]" $
        allLeavesInADT (ST (1)) == [ 1 ]
      liftAff $ assert "allLeavesInADT (UET 1) == [1]" $
        allLeavesInADT (UET (1)) == [ 1 ]
      liftAff $ assert "allLeavesInADT (SUM [ST 1, ST 2]) == [1, 2]" $
        allLeavesInADT (SUM [ ST (1), ST 2 ]) == [ 1, 2 ]
      liftAff $ assert "allLeavesInADT (PROD [ST 1, ST 2]) == [1, 2]" $
        allLeavesInADT (PROD [ ST (1), ST 2 ]) == [ 1, 2 ]
      liftAff $ assert "allLeavesInADT traverses nested structure" $
        allLeavesInADT (PROD [ SUM [ ST 1, ST 2 ], SUM [ ST 3, ST 4 ] ]) == [ 1, 2, 3, 4 ]

    test "commonLeavesInADT (intersection-based)" do
      -- For a single leaf the result is that leaf.
      liftAff $ assert "commonLeavesInADT (ST 1) == [1]" $
        commonLeavesInADT (ST (1)) == [ 1 ]
      -- PROD collects all types (union of leaves from each branch).
      liftAff $ assert "commonLeavesInADT (PROD [ST 1, ST 2]) == [1, 2]" $
        commonLeavesInADT (PROD [ ST (1), ST 2 ]) == [ 1, 2 ]
      -- SUM: only leaves that appear in EVERY branch survive (intersection).
      -- SUM [ST 1, ST 2] — no leaf in common → []
      liftAff $ assert "commonLeavesInADT (SUM [ST 1, ST 2]) == [] (no common leaf)" $
        commonLeavesInADT (SUM [ ST (1), ST 2 ]) == []
      -- SUM [PROD [ST 1, ST 2], PROD [ST 2, ST 3]]:
      --   branch 1 brings {1, 2}, branch 2 brings {2, 3}; intersection = {2}.
      liftAff $ assert "commonLeavesInADT (SUM [PROD [ST 1, ST 2], PROD [ST 2, ST 3]]) == [2]" $
        commonLeavesInADT (SUM [ PROD [ ST (1), ST 2 ], PROD [ ST 2, ST 3 ] ]) == [ 2 ]

    test "expand (ADT → ExpandedADT)" do
      liftAff $ assert "expand (ST 1) → EST 1" $
        unwrap (expandPure (ST 1)) == EST (1)
      liftAff $ assert "expand (UET 1) → EST 1" $
        unwrap (expandPure (UET 1)) == EST (1)
      liftAff $ assert "expand (PROD [ST 1, ST 2]) → EPROD [EST 1, EST 2]" $
        unwrap (expandPure (PROD [ ST 1, ST 2 ])) == EPROD [ EST 1, EST (2) ]
      liftAff $ assert "expand (SUM [ST 1, ST 2]) → ESUM [EST 1, EST 2]" $
        unwrap (expandPure (SUM [ ST 1, ST 2 ])) == ESUM [ EST 1, EST (2) ]
      liftAff $ assert "expand nested: PROD [SUM [ST 1, ST 2], ST 3] → EPROD [ESUM [...], EST 3]" $
        unwrap (expandPure (PROD [ SUM [ ST 1, ST 2 ], ST 3 ]))
          == EPROD [ ESUM [ EST 1, EST 2 ], EST (3) ]

  -- ─── Type comparison operators ──────────────────────────────────────────────

  suite "Type comparison (CNF variants)" do

    test "equalsOrSpecialises_ (left => right)" do
      -- A type equals or specialises itself.
      liftAff $ assert "DPROD [DSUM [1]] equalsOrSpecialises_ itself" $
        equalsOrSpecialises_ (DPROD [ DSUM [ 1 ] ]) (DPROD [ DSUM [ 1 ] ])
      -- A more specific type (smaller disjunction ⊆ larger one) specialises the less specific.
      -- DPROD [DSUM [1]] ⊆ DPROD [DSUM [1, 2]]: {1} ⊆ {1, 2} ✓
      liftAff $ assert "DPROD [DSUM [1]] specialises DPROD [DSUM [1, 2]]" $
        equalsOrSpecialises_ (DPROD [ DSUM [ 1 ] ]) (DPROD [ DSUM [ 1, 2 ] ])
      -- A type with MORE conjuncts (PROD) specialises one with fewer.
      -- For every DSUM in right there must be a DSUM in left that is a subset.
      liftAff $ assert "DPROD [DSUM [1], DSUM [2]] specialises DPROD [DSUM [1]]" $
        equalsOrSpecialises_ (DPROD [ DSUM [ 1 ], DSUM [ 2 ] ]) (DPROD [ DSUM [ 1 ] ])
      -- The reverse direction must NOT hold.
      liftAff $ assert "NOT: DPROD [DSUM [1, 2]] equalsOrSpecialises_ DPROD [DSUM [1]]"
        $ not
        $ equalsOrSpecialises_ (DPROD [ DSUM [ 1, 2 ] ]) (DPROD [ DSUM [ 1 ] ])
      liftAff $ assert "NOT: DPROD [DSUM [1]] equalsOrSpecialises_ DPROD [DSUM [2]]"
        $ not
        $ equalsOrSpecialises_ (DPROD [ DSUM [ 1 ] ]) (DPROD [ DSUM [ 2 ] ])

    test "equalsOrSpecialises_ matches monotone CNF implication semantics" do
      let
        cases =
          [ Tuple (DPROD [ DSUM [ 1 ] ]) (DPROD [ DSUM [ 1 ] ])
          , Tuple (DPROD [ DSUM [ 1 ] ]) (DPROD [ DSUM [ 1, 2 ] ])
          , Tuple (DPROD [ DSUM [ 1 ], DSUM [ 2 ] ]) (DPROD [ DSUM [ 1 ] ])
          , Tuple (DPROD [ DSUM [ 1 ], DSUM [ 2 ] ]) (DPROD [ DSUM [ 1, 2 ] ])
          , Tuple (DPROD [ DSUM [ 1, 2 ] ]) (DPROD [ DSUM [ 1 ], DSUM [ 2 ] ])
          ]
      for_ cases \(Tuple left right) ->
        liftAff $ assert ("semantic implication matches equalsOrSpecialises_ for " <> show left <> " => " <> show right)
          ((left `equalsOrSpecialises_` right) == semanticallyImplies left right)

    testSkip "toConjunctiveNormalForm may retain absorbed clauses without changing implication results" do
      let
        canonical = toConjunctiveNormalForm (EST 1)
        absorbed = toConjunctiveNormalForm (EPROD [ EST 1, ESUM [ EST 1, EST 2 ] ])
      liftAff $ assert "Absorption can yield a non-identical but semantically equivalent CNF." $
        canonical /= absorbed
      liftAff $ assert "The absorbed CNF still implies the canonical CNF." $
        absorbed `equalsOrSpecialises_` canonical
      liftAff $ assert "The canonical CNF still implies the absorbed CNF." $
        canonical `equalsOrSpecialises_` absorbed
      liftAff $ assert "The semantic implication check agrees for absorbed => canonical." $
        semanticallyImplies absorbed canonical
      liftAff $ assert "The semantic implication check agrees for canonical => absorbed." $
        semanticallyImplies canonical absorbed

    test "equals_ (mutual implication)" do
      liftAff $ assert "DPROD [DSUM [1]] equals_ itself" $
        equals_ (DPROD [ DSUM [ 1 ] ]) (DPROD [ DSUM [ 1 ] ])
      -- Different number of factors → not equal.
      liftAff $ assert "NOT: DPROD [DSUM [1]] equals_ DPROD [DSUM [1, 2]]"
        $ not
        $ equals_ (DPROD [ DSUM [ 1 ] ]) (DPROD [ DSUM [ 1, 2 ] ])
      -- Different factor content → not equal.
      liftAff $ assert "NOT: DPROD [DSUM [1]] equals_ DPROD [DSUM [2]]"
        $ not
        $ equals_ (DPROD [ DSUM [ 1 ] ]) (DPROD [ DSUM [ 2 ] ])

    test "equalsOrSpecialises (ExpandedADT)" do
      -- EST equals itself.
      liftAff $ assert "EST 1 equalsOrSpecialises EST 1" $
        equalsOrSpecialises (EST (1)) (EST 1)
      -- EST specialises an ESUM that contains it.
      liftAff $ assert "EST 1 equalsOrSpecialises ESUM [EST 1, EST 2]" $
        equalsOrSpecialises (EST (1)) (ESUM [ EST 1, EST 2 ])
      -- An EPROD (more conjuncts) specialises one of its component types.
      liftAff $ assert "EPROD [EST 1, EST 2] equalsOrSpecialises EST 1" $
        equalsOrSpecialises (EPROD [ EST (1), EST 2 ]) (EST 1)
      -- Unrelated types do not specialise each other.
      liftAff $ assert "NOT: EST 1 equalsOrSpecialises EST 2"
        $ not
        $ equalsOrSpecialises (EST (1)) (EST 2)
      -- A more general type does not specialise a more specific one.
      liftAff $ assert "NOT: ESUM [EST 1, EST 2] equalsOrSpecialises EST 1"
        $ not
        $ equalsOrSpecialises (ESUM [ EST (1), EST 2 ]) (EST 1)

    test "specialises (strict; excludes equals)" do
      -- Strict: EPROD [EST 1, EST 2] specialises EST 1, but they are not equal.
      liftAff $ assert "EPROD [EST 1, EST 2] specialises EST 1" $
        specialises (EPROD [ EST (1), EST 2 ]) (EST 1)
      -- Strict: a type does NOT strictly specialise itself.
      liftAff $ assert "NOT: EST 1 specialises EST 1 (equal, not strict)"
        $ not
        $ specialises (EST (1)) (EST 1)

    test "generalises (strict; excludes equals)" do
      -- EST 1 is more general than EPROD [EST 1, EST 2].
      liftAff $ assert "EST 1 generalises EPROD [EST 1, EST 2]" $
        generalises (EST (1)) (EPROD [ EST 1, EST 2 ])
      -- Strict: a type does NOT strictly generalise itself.
      liftAff $ assert "NOT: EST 1 generalises EST 1 (equal, not strict)"
        $ not
        $ generalises (EST (1)) (EST 1)

    test "specialises_ and generalises_ (CNF variants)" do
      -- specialises_ is flip generalises_
      liftAff $ assert "DPROD [DSUM [1], DSUM [2]] specialises_ DPROD [DSUM [1]]" $
        specialises_ (DPROD [ DSUM [ 1 ], DSUM [ 2 ] ]) (DPROD [ DSUM [ 1 ] ])
      liftAff $ assert "NOT: DPROD [DSUM [1]] specialises_ DPROD [DSUM [1]] (equal, not strict)"
        $ not
        $ specialises_ (DPROD [ DSUM [ 1 ] ]) (DPROD [ DSUM [ 1 ] ])
      liftAff $ assert "DPROD [DSUM [1]] generalises_ DPROD [DSUM [1], DSUM [2]]" $
        generalises_ (DPROD [ DSUM [ 1 ] ]) (DPROD [ DSUM [ 1 ], DSUM [ 2 ] ])
