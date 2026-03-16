# Type Comparison in the Perspectives Distributed Runtime

This document gives a comprehensive overview of how types are represented, normalised and compared in the Perspectives Distributed Runtime (PDR). It is intended both as a reference for human developers and as context for automated tooling.

> **Primary source modules**
> | Concern | Module | File |
> |---|---|---|
> | ADT definition & comparison operators | `Perspectives.Representation.ADT` | `src/core/typerepresentation/abstractDataType2.purs` |
> | Expanded ADT definition | `Perspectives.Representation.ExpandedADT` | `src/core/typerepresentation/expandedADT.purs` |
> | CNF definition & normalisation | `Perspectives.Representation.CNF` | `src/core/typerepresentation/conjunctiveNormalForm.purs` |
> | Role type class & CNF entry point | `Perspectives.Representation.Class.Role` | `src/core/typerepresentation/class/role2.purs` |
> | Runtime type of a role instance | `Perspectives.Instances.ObjectGetters` | `src/core/instances/instanceObjectGetters.purs` |
> | Type-level comparison helpers | `Perspectives.Types.ObjectGetters` | `src/core/typerepresentation/typeLevelObjectGetters.purs` |
> | Binding check | `Perspectives.Checking.PerspectivesTypeChecker` | `src/core/typeChecking/perspectivesTypeChecker.purs` |
> | Precomputation of `completeType` | `Perspectives.Parsing.Arc.PhaseThree` | `src/arcParser/arcParserPhaseThree.purs` |

---

## 1. Background: why types are complex

A Perspectives role can be:

* a simple, stand-alone type (`EnumeratedRoleType`);
* a type that **extends aspects** – other roles whose properties and filler restrictions it inherits;
* a type that carries a **filler restriction** – a declaration of which role types are allowed to fill it.

Filler restrictions and aspect inheritance can be arbitrarily nested, so a type is not a flat identifier but a *tree*. That tree is described with an Algebraic Data Type (ADT).

Two ways to combine types produce a **product**:
- adding an **aspect** to a role — the role *is* the intersection of itself and the aspect;
- adding a **filler restriction** — the role is only valid when filled by a role that satisfies the restriction.

A disjunction (sum) appears in filler restrictions:

```
user AccountHolder filledBy ((sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons), sys:TheWorld$Onlookers)
```

Here `,` is a disjunct (OR-alternative), and `+` (a plus-sign between parenthesised groups) introduces another conjunct (AND-requirement).

---

## 2. Data type representations

### 2.1 `ADT a` – the declared type

```purescript
-- Module: Perspectives.Representation.ADT
data ADT a
  = ST a           -- Simple Type: a leaf with no aspects and no filler restriction.
  | UET a          -- UnExpanded Type: a leaf that needs further expansion (has aspects or filler restriction).
  | SUM (Array (ADT a))   -- Disjunction (OR / +)
  | PROD (Array (ADT a))  -- Conjunction (AND / ,)
```

`a` is always a newtype wrapping a type-level identifier, most commonly `RoleInContext` (a pair of context type and role type).

Rules:
* `ST` is used only when the role has **no aspects and no filler restriction**. It is therefore a "closed" leaf.
* `UET` is used when the role has aspects or a filler restriction. It is a "placeholder" that needs to be expanded to yield the full type tree.
* `SUM [] ≡ EMPTY` (identity for conjunction), `PROD [] ≡ EMPTY` (absorbing element).

`ADT` is stored directly in role definitions, e.g. the `binding` field of `EnumeratedRole` stores `Maybe (ADT RoleInContext)`.

### 2.2 `ExpandedADT a` – the fully-expanded type

```purescript
-- Module: Perspectives.Representation.ExpandedADT
data ExpandedADT a
  = EST a                              -- Expanded Simple Type (a leaf)
  | ECT (ExpandedADT a) (ExpandedADT a) -- labelled node: (label, full expansion)
  | ESUM (Array (ExpandedADT a))       -- Expanded disjunction
  | EPROD (Array (ExpandedADT a))      -- Expanded conjunction
```

`ExpandedADT` contains **no `UET` nodes**: every placeholder has been replaced with its full recursive expansion including all aspects and filler restrictions. `ECT label expansion` records the identity of the original role (the label, always an `EST`) alongside its complete subtree. The label appears again inside the expansion, so comparison ignores the `ECT` wrapper and only looks at the second child.

`ExpandedADT` is never stored; it is produced on-the-fly when needed.

### 2.3 `CNF a` / `DPROD a` / `DSUM a` – the canonical form used for comparison

```purescript
-- Module: Perspectives.Representation.CNF
type CNF = DPROD
data DPROD a = DPROD (Array (DSUM a))  -- Conjunction of disjunctions
data DSUM a  = DSUM (Array a)          -- A single disjunction
```

`CNF` stands for *Conjunctive Normal Form*: a **product (AND) of sums (OR)**. For example, the formula `(A ∨ B) ∧ (C ∨ D)` is represented as:

```
DPROD [ DSUM [A, B], DSUM [C, D] ]
```

`CNF` is the only form over which type comparison is defined. It is also **cached**: the `completeType :: CNF RoleInContext` field of `EnumeratedRole` stores the precomputed CNF so that the expensive expansion and normalisation need only happen once, at compile time (Phase 3 of the arc parser).

---

## 3. The normalisation pipeline

Converting a declared `ADT` to a `CNF` happens in a pipeline with two stages.

### Stage 1 — Expand: `ADT → ExpandedADT`

```purescript
-- Perspectives.Representation.Class.Role
expandUnexpandedLeaves :: ADT RoleInContext -> MP (ExpandedADT RoleInContext)
```

This function performs a deep traversal:
* `ST a` → `EST a` (already a leaf, wrap it).
* `UET a` → look up the `EnumeratedRole` for `a`, then:
  * Recursively expand its `binding` (filler restriction) if present.
  * Recursively expand each of its `roleAspects` via `completeExpandedType`.
  * Combine role identity, filler, and aspects into an `ECT` node over an `EPROD`.

The result is a tree in which every node is described purely in terms of `EST`, `ECT`, `ESUM`, `EPROD`.

A variant, `expandAspects`, expands **only aspects** (not fillers), used in limited contexts.

### Stage 2 — Normalise: `ExpandedADT → CNF`

```purescript
-- Perspectives.Representation.CNF
toConjunctiveNormalForm :: ExpandedADT a -> DPROD a
```

Rules:
* `EST a` → `DPROD [ DSUM [a] ]` — a single conjunction factor containing a single alternative.
* `ECT label a` → `toConjunctiveNormalForm a` — the label is discarded; it is already included in the expansion.
* `EPROD as` → normalise each child, then **flatten products**: concatenate all `DSUM` arrays (removing duplicates).
* `ESUM as` → normalise each child, then **distribute**: apply the distributive law `P ∧ (Q ∨ R) ≡ (P ∧ Q) ∨ (P ∧ R)` to merge a disjunction of products into a single product of sums.

This transformation is the standard Boolean CNF conversion applied to the type-lattice.

### The combined shortcut: `toConjunctiveNormalForm_`

```purescript
-- Perspectives.Representation.Class.Role
toConjunctiveNormalForm_ :: ADT RoleInContext -> MP (CNF RoleInContext)
```

For `ST` and `UET` leaves, instead of re-expanding and re-normalising at call time, this function **reads the precomputed `completeType`** field from the stored `EnumeratedRole`. For `SUM` and `PROD` nodes it normalises the children and then applies `distribute` / `flattenProducts`.

> **Important:** `toConjunctiveNormalForm_` relies on the `completeType` field being already populated. This is guaranteed only after Phase 3 of the arc parser has run. Calling it on a freshly constructed `ADT` whose role definitions have not yet been through Phase 3 will silently return whatever default value `completeType` was initialised with (a single-element CNF containing just the role itself).

### How `completeType` is precomputed (Phase 3)

```purescript
-- Perspectives.Parsing.Arc.PhaseThree
computeCompleteEnumeratedType :: EnumeratedRole -> PhaseThree EnumeratedRole
computeCompleteEnumeratedType erole@(EnumeratedRole erec) = do
  normalizedType <- lift $ lift (completeExpandedType erole >>= pure <<< toConjunctiveNormalForm)
  pure (EnumeratedRole erec { completeType = normalizedType })
```

`completeExpandedType` expands the full type tree (aspects *and* filler restriction); the result is then normalised with `toConjunctiveNormalForm` (the `ExpandedADT` version, not the `_` variant). The computed `CNF` is stored back in the role record and persisted to the domain file.

The default initial value of `completeType` for a freshly created role is:

```purescript
DPROD [ DSUM [ RoleInContext { context, role } ] ]
```

i.e. a trivial CNF that represents only the role itself, with no aspects or filler restriction.

---

## 4. Type comparison functions

All comparison is performed on `CNF`. The semantics are **propositional implication**: `left` specialises `right` if and only if `left` logically implies `right`.

### 4.1 Core predicate: `equalsOrSpecialises_`

```purescript
-- Perspectives.Representation.ADT
equalsOrSpecialises_ :: Ord a => Eq a => CNF a -> CNF a -> Boolean
-- left `equalsOrSpecialises_` right   means   left => right
```

**Interpretation:** *`left` is at least as specific as `right`.*

**Algorithm:**

For `left => right` to hold, every conjunct (disjunction) in `right` must be implied by some conjunct in `left`. A conjunct in a CNF `(a₁ ∨ a₂ ∨ …)` is implied by `(b₁ ∨ b₂ ∨ …)` when `{b₁, b₂, …} ⊆ {a₁, a₂, …}` (i.e. `b` offers fewer alternatives, making it a stronger requirement).

In code:

```purescript
equalsOrSpecialises_ (DPROD left) (DPROD right) =
  -- For every disjunction in right …
  all (\(DSUM dRight) ->
    -- … there exists a disjunction in left …
    any (\(DSUM dLeft) ->
      -- … whose terms are a subset of the terms in the right disjunction.
      SET.fromFoldable dLeft `SET.subset` SET.fromFoldable dRight
    ) left
  ) right
```

Arrow notation used in comments throughout the code:

| Expression | Meaning |
|---|---|
| `left -> right` | `left` specialises `right` (left implies right) |
| `left <- right` | `left` generalises `right` (right implies left) |

### 4.2 All comparison operators

| Function | Arguments | Meaning |
|---|---|---|
| `equalsOrSpecialises_` | `CNF a -> CNF a -> Boolean` | `left -> right` (left is ≤ right in the type lattice) |
| `equals_` | `CNF a -> CNF a -> Boolean` | `left ≡ right` (mutual implication, same CNF up to reordering) |
| `specialises_` | `CNF a -> CNF a -> Boolean` | `left -> right` (strict; excludes equals) |
| `equalsOrGeneralises_` | `CNF a -> CNF a -> Boolean` | `left <- right` (flip of `equalsOrSpecialises_`) |
| `generalises_` | `CNF a -> CNF a -> Boolean` | `left <- right` (strict; excludes equals) |
| `equalsOrSpecialises` | `ExpandedADT a -> …` | same as `equalsOrSpecialises_` but on `ExpandedADT` (normalises first) |
| `specialises` | `ExpandedADT a -> …` | same as `specialises_` but on `ExpandedADT` (normalises first) |
| `equalsOrGeneralises` | `ExpandedADT a -> …` | flip of `equalsOrSpecialises` |
| `generalises` | `ExpandedADT a -> …` | `/=` and `equalsOrGeneralises` |

Monadic variants in `Perspectives.Types.ObjectGetters` (module `typeLevelObjectGetters.purs`) convert `RoleType` or `ADT RoleInContext` arguments to CNF before delegating to the functions above:

| Function | Meaning |
|---|---|
| `equalsOrSpecialisesRoleInContext` | `ADT RoleInContext -> ADT RoleInContext -> MP Boolean` |
| `equalsOrGeneralisesRoleInContext` | `ADT RoleInContext -> ADT RoleInContext -> MP Boolean` |
| `equals` | `ADT RoleInContext -> ADT RoleInContext -> MP Boolean` |
| `generalisesRoleType_` | `RoleType -> RoleType -> MP Boolean` |
| `specialisesRoleType_` | `RoleType -> RoleType -> MP Boolean` |
| `equalsOrGeneralisesRoleType_` | `RoleType -> RoleType -> MP Boolean` |
| `equalsOrSpecialisesRoleType_` | `RoleType -> RoleType -> MP Boolean` |

---

## 5. Key functions that use type comparison

### 5.1 `checkBinding` — runtime binding validation

```purescript
-- Perspectives.Checking.PerspectivesTypeChecker
checkBinding :: EnumeratedRoleType -> RoleInstance -> MP Boolean
```

Called when a role instance is about to be bound (filled) by another role instance. The question is: *is the proposed filler compatible with the filler restriction declared on the role to be filled?*

```
filledType  -- the role type being filled (has a declared filler restriction)
filler      -- the role instance proposed as filler
```

Steps:

1. **Collect the filler restriction** of `filledType`, merging its own `binding` with the `binding` fields of all its aspects via `completeDeclaredFillerRestriction`. Result: `Maybe (ADT RoleInContext)`.
2. **Normalise the restriction** to `CNF` with `toConjunctiveNormalForm_`.
3. **Compute the runtime type of the filler** with `completeRuntimeType`. Result: `ADT RoleInContext`.
4. **Normalise the filler type** to `CNF` with `toConjunctiveNormalForm_`.
5. **Compare**: `fillerType \`equalsOrSpecialises_\` restriction` — the filler type must be at least as specific as the restriction.
6. If there is no restriction (`Nothing`), any filler is accepted.

```purescript
checkBinding filledType filler = do
  mrestriction <- getEnumeratedRole filledType
    >>= completeDeclaredFillerRestriction
    >>= traverse toConjunctiveNormalForm_
  fillerType <- completeRuntimeType filler >>= toConjunctiveNormalForm_
  case mrestriction of
    Just restriction -> pure (fillerType `equalsOrSpecialises_` restriction)
    Nothing          -> pure true
```

> **Note:** An earlier implementation (commented out) used `completeExpandedFillerRestriction` which, by first calling the full `expandUnexpandedLeaves` pipeline and then `toConjunctiveNormalForm`, would follow the same normalisation path as `completeType`. The current implementation uses `completeDeclaredFillerRestriction` (which yields the *declared* but *unexpanded* restriction) and then `toConjunctiveNormalForm_`. This means the restriction is normalised via the cached `completeType` of each leaf's role, **not** by re-expanding the declaration from scratch. This can produce different results when the stored `completeType` values are stale or when the filler restriction uses `UET` nodes pointing to roles whose `completeType` was computed under different assumptions than those used when expanding the role itself. This difference is the suspected root cause of the known unexpected result.

### 5.2 `completeDeclaredFillerRestriction` — merging filler restrictions

```purescript
-- Perspectives.Representation.Class.Role
completeDeclaredFillerRestriction :: EnumeratedRole -> MP (Maybe (ADT RoleInContext))
```

Returns the combined filler restriction of a role and all its aspects as an `ADT`. The result is:

* `Nothing` if neither the role nor any of its aspects declares a filler restriction.
* `Just (PROD [roleRestriction, aspect1Restriction, aspect2Restriction, …])` if any restrictions exist.

The product semantics here are correct: an instance must satisfy **all** filler restrictions simultaneously (the role's own and all inherited from aspects).

### 5.3 `completeRuntimeType` — the runtime type of a role instance

```purescript
-- Perspectives.Instances.ObjectGetters
completeRuntimeType :: RoleInstance -> MP (ADT RoleInContext)
```

At runtime, a role instance carries a **chain of fillers** (a role may be filled by another role, which is itself filled, etc.). The complete type of an instance is:

* The declared type (without filler) of the instance's role type.
* Combined (as a product) with the complete runtime type of its filler if it has one.
* If it has no actual filler but a declared filler restriction, that restriction is used instead.

```purescript
completeRuntimeType rid = do
  role <- roleType_ rid >>= getEnumeratedRole
  crt  <- declaredTypeWithoutFiller role        -- own type + aspects (no filler)
  mb   <- binding_ rid                          -- actual filler instance, if any
  case mb of
    Nothing -> do
      mrestrictions <- completeDeclaredFillerRestriction role
      case mrestrictions of
        Nothing           -> pure crt
        Just restrictions -> pure $ PROD [ crt, restrictions ]
    Just b  -> (\adt -> PROD [ crt, adt ]) <$> completeRuntimeType b
```

The returned `ADT` is then passed through `toConjunctiveNormalForm_` before comparison.

### 5.4 `isPerspectiveOnADT` — checking perspective applicability

```purescript
-- Perspectives.Types.ObjectGetters
isPerspectiveOnADT :: Partial => Perspective -> ADT RoleInContext -> MP Boolean
isPerspectiveOnADT p adt = (objectOfPerspective p) `equalsOrGeneralisesRoleInContext` adt
```

A perspective applies to a role instance if the perspective's object type **generalises** (is equal to or more general than) the role's type. This is the reverse of the binding check: a perspective is broad enough to cover the concrete type.

### 5.5 Aspect filler restriction check in the type checker

```purescript
-- Perspectives.Checking.PerspectivesTypeChecker (around line 108)
(lift $ aspectBinding `equalsOrGeneralisesRoleInContext` bnd) >>=
  if _ then pure unit
  else fail $ FillerRestrictionNotAnAspectSubtype …
```

During parse-time type checking, when a role has an aspect, the role's own filler restriction must be at least as specific as the aspect's. In other words, the aspect's restriction must **generalise** the role's restriction.

---

## 6. The type lattice

The type system forms a **lattice** ordered by specialisation:

```
more general
     ↑
     │
   SUM (disjunction of types — any one suffices)
     │
   PROD (conjunction of types — all must be satisfied)
     │
   ST / leaf type
     ↓
more specific
```

A type `T` specialises `U` (`T -> U`) if and only if every model (role instance) of `T` is also a model of `U`. In CNF terms:
- `T -> U` iff for every disjunction *d*<sub>U</sub> in `U`, there exists a disjunction *d*<sub>T</sub> in `T` such that *d*<sub>T</sub> ⊆ *d*<sub>U</sub>.

Intuitively:
- A **PROD** (AND) is more specific than either of its operands: it requires more.
- A **SUM** (OR) is more general than either of its operands: it requires less.
- A leaf type `T` specialises another leaf type `U` only if `T == U` (structural identity of the `RoleInContext` value).

---

## 7. Summary of data flow

```
                Arc source                          Runtime
                ──────────────                      ──────────────
Role declaration (ADT)
       │
       ▼
Phase 3: expandUnexpandedLeaves               binding_ (filler instance)
       │                                             │
       ▼                                             ▼
  ExpandedADT                             completeRuntimeType
       │                                     (produces ADT)
       ▼                                             │
toConjunctiveNormalForm                              │
       │                                             ▼
   CNF (stored as                         toConjunctiveNormalForm_
   completeType field)                       (reads completeType
                         ◄────────────────   for leaf roles)
                                                     │
                                                     ▼
                                              CNF (runtime type)

                                                     │
                       ┌─────────────────────────────┘
                       │
                       ▼
                equalsOrSpecialises_
                (set-subset check on DSUM arrays)
                       │
                       ▼
                    Boolean
```

---

## 8. Known issues and design observations

### 8.1 Two equivalent paths to normalise the filler restriction

`checkBinding` currently normalises the filler restriction using:

```purescript
getEnumeratedRole filledType
  >>= completeDeclaredFillerRestriction   -- yields Maybe (ADT RoleInContext)
  >>= traverse toConjunctiveNormalForm_   -- reads cached completeType per leaf
```

An alternative path that works directly from the expanded declaration tree is:

```purescript
getEnumeratedRole filledType
  >>= completeExpandedFillerRestriction   -- yields Maybe (ExpandedADT RoleInContext)
  <#> map toConjunctiveNormalForm         -- applies ExpandedADT → CNF conversion
-- (completeExpandedFillerRestriction from Perspectives.Representation.Class.Role,
--  toConjunctiveNormalForm from Perspectives.Representation.CNF)
```

**Are the two paths equivalent?**

Yes — *provided every `completeType` value is correct and complete* — both paths produce the same CNF. The reasoning is:

1. `toConjunctiveNormalForm_` on a `UET a` leaf returns `completeType(a)`.
2. `completeType(a)` was written by Phase 3 as `toConjunctiveNormalForm(completeExpandedType(role_a))`.
3. `completeExpandedFillerRestriction` calls `expandUnexpandedLeaves` on each leaf `UET a`, which itself calls `completeExpandedType` for every aspect. The resulting `ExpandedADT` is therefore identical to what Phase 3 computed for each leaf. Applying `toConjunctiveNormalForm` then produces the same CNF as `completeType(a)`.

The paths **can diverge** if `completeType` is stale — for example, if a role's aspects or filler were changed after Phase 3 last ran. In that situation, `toConjunctiveNormalForm_` would return the outdated cached value, while `completeExpandedFillerRestriction` + `toConjunctiveNormalForm` would reflect the current declaration graph.

### 8.2 The `ECT` label in `ExpandedADT` is redundant during comparison

`toConjunctiveNormalForm` for `ECT label a` ignores `label` and processes only `a`. However, `label` (which is an `EST` of the role itself) is also present as the first element of the `EPROD` inside `a` (see `expandUnexpandedLeaves`). The label is therefore not lost; it is simply redundant in the `ECT` wrapper. This is by design.

### 8.3 Equality on `DPROD` and `DSUM` uses set semantics

The `Eq` instance on `DSUM` compares as sets (`SET.fromFoldable left == SET.fromFoldable right`), and `DPROD` similarly. This means the order of elements in the arrays does not matter for equality. However, `equals_` is an additional function (not the `Eq` instance of `CNF`) that also checks that the **number** of conjuncts matches, guarding against the case where one CNF has duplicate conjuncts.

### 8.4 Phase 3 must complete before `toConjunctiveNormalForm_` is reliable

Calling `toConjunctiveNormalForm_` on an `ST` or `UET` leaf before Phase 3 has run will return the *default* `completeType`:

```purescript
DPROD [ DSUM [ RoleInContext { context, role } ] ]
```

This trivial CNF represents only the role itself, without any aspects or filler restriction. Any comparison based on this will miss the full type and may produce incorrect results (typically a false match where a more restrictive check would be expected).
