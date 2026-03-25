# The Query Subsystem

This document gives a technical overview of the query subsystem in the Perspectives Distributed Runtime (PDR). It is intended both as a reference for human developers and as context for automated tooling (e.g. GitHub Copilot).

> **Primary source modules**
> | Concern | Module | File |
> |---|---|---|
> | Expression parsing | `Perspectives.Parsing.Arc.Expression` | `src/arcParser/expressionParser.purs` |
> | Expression AST | `Perspectives.Parsing.Arc.Expression.AST` | `src/arcParser/expressionAST.purs` |
> | AST → typed query description | `Perspectives.Query.ExpressionCompiler` | `src/core/queries/expressionCompiler.purs` |
> | Query description data structures | `Perspectives.Query.QueryTypes` | `src/core/queries/queryFunctionDescription.purs` |
> | Query description → executable code | `Perspectives.Query.UnsafeCompiler` | `src/core/queries/unsafeCompiler.purs` |
> | Runtime interpreter | `Perspectives.Query.Interpreter` | `src/core/queries/queryFunctionInterpreter.purs` |
> | Interpreter data structures | `Perspectives.Query.Interpreter.Dependencies` | `src/core/queries/dependencyPaths.purs` |
> | Compiled getter cache | `Perspectives.ObjectGetterLookup` | `src/core/queries/objectsGetterLookup.purs` |
> | External function registry | `Perspectives.External.HiddenFunctionCache` | `src/core/external/hiddenFunctionCache.purs` |

---

## Contents

1. [Background: Data Model and Calculated Roles](#1-background-data-model-and-calculated-roles)
2. [End-to-end Pipeline](#2-end-to-end-pipeline)
3. [Parsing: Text to AST](#3-parsing-text-to-ast)
4. [Query Types: The Storable Description](#4-query-types-the-storable-description)
5. [Expression Compiler: AST to Query Description](#5-expression-compiler-ast-to-query-description)
6. [Unsafe Compiler: Query Description to Executable Code](#6-unsafe-compiler-query-description-to-executable-code)
7. [Collecting Dependencies while Executing Queries](#7-collecting-dependencies-while-executing-queries)
8. [The Interpreter](#8-the-interpreter)
9. [Extension with Libraries: callExternal and callEffect](#9-extension-with-libraries-callexternal-and-calleffect)
10. [Points of Attention](#10-points-of-attention)

---

## 1. Background: Data Model and Calculated Roles

Perspectives data is structured as **contexts** that contain **roles**. Roles can be *filled* by other roles (a binding or "filler" relationship). All entities are doubly linked: traversal in either direction is possible without a search, with one exception — so-called *unlinked* roles that carry only a pointer from role to context.

**Calculated roles** are roles visible to a context's user role but not physically stored in that context. Their instances are produced by executing a query. A query navigates the graph of contexts and roles, optionally reading property values, and returns a set of results.

Queries also occur in **calculated properties**, **state conditions**, and **perspective filters** — anywhere a derived value is required.

---

## 2. End-to-end Pipeline

```
Arc model text (DSL)
     │
     ▼
[expressionParser.purs]  ── Perspectives.Parsing.Arc.Expression
     │  parses to
     ▼
Step  (AST)              ── Perspectives.Parsing.Arc.Expression.AST
     │
     ▼
[expressionCompiler.purs] ── Perspectives.Query.ExpressionCompiler
     │  type-checks and compiles to
     ▼
QueryFunctionDescription  ── Perspectives.Query.QueryTypes
     │  stored in model (serialisable, distributed)
     │
     ├──► [unsafeCompiler.purs]  ── Perspectives.Query.UnsafeCompiler
     │         compiles to
     │    String ~~> String  (executable PureScript function)
     │         │
     │         ▼
     │    Runtime query execution (returns results directly)
     │
     └──► [queryFunctionInterpreter.purs]  ── Perspectives.Query.Interpreter
               interprets with dependency tracking
          DependencyPath ~~> DependencyPath
               │
               ▼
          InformedAssumption  (feeds inverted query / cache invalidation)
```

The key distinction between the **compiler** path and the **interpreter** path:

* The **unsafe compiler** (`UnsafeCompiler`) produces a regular PureScript closure that can be called repeatedly and at high speed. It is used for normal query execution (e.g. calculating which role instances to show in a client view).
* The **interpreter** (`Interpreter`) executes the same query description step by step while recording which data points were visited. These recorded dependencies are consumed by the **inverted query** mechanism to detect which queries are affected by a data change. (The inverted query mechanism itself is outside the scope of this document.)

---

## 3. Parsing: Text to AST

### Module
`Perspectives.Parsing.Arc.Expression`  
Source: `src/arcParser/expressionParser.purs`

The expression grammar is parsed with the `step` parser (top-level entry point). It returns a value of type `Step`, defined in `Perspectives.Parsing.Arc.Expression.AST` (`expressionAST.purs`).

### The `Step` ADT

```purescript
data Step
  = Simple     SimpleStep
  | Binary     BinaryStep
  | Unary      UnaryStep
  | PureLet    PureLetStep
  | Computation ComputationStep
```

**`SimpleStep`** — leaves of the expression tree:

| Constructor | Description |
|---|---|
| `ArcIdentifier pos name` | An unresolved identifier; may refer to a role, context type, or property |
| `Value pos range literal` | A literal value with its range (type) |
| `Filler pos ctx?` | The filler (binding) of the current role, optionally restricted to a context |
| `Filled pos roleName ctx?` | Roles filled by the current role |
| `Context pos` | The embedding context of the current role |
| `Extern pos` | The external role of the current context |
| `Identity pos` | Pass-through |
| `Variable pos name` | A variable introduced by `letE` |
| `Me pos` | The current user |
| `IndexedName pos` | Looks up the indexed name of an instance |
| `ContextTypeIndividual pos name` | A context-type individual |
| `RoleTypeIndividual pos name` | A role-type individual |
| `TypeOfContext pos` | The type of the current context instance |
| `TypeOfRole pos` | The type of the current role instance |
| `PublicRole pos name` | The public role of a published context |
| `SpecialisesRoleType pos name` | Boolean: does the current role type specialise `name`? |
| `IsInState pos stateName` | Boolean: is the entity in the named state? |
| `Translate pos` | Translate a type identifier to a human-readable string |

**`BinaryStep`** — infix operators connecting two sub-expressions:

| Operator keyword | Meaning |
|---|---|
| `>>` | Composition (pipe): apply right to result of left |
| `filter ... with ...` | Keep only results from left that satisfy right (boolean) |
| `==`, `!=`, `<`, `<=`, `>`, `>=` | Comparison operators |
| `&&`, `\|\|` | Logical AND / OR |
| `+`, `-`, `*`, `/` | Arithmetic |
| `union`, `intersection`, `orElse` | Set operations |
| `binds`, `fills`, `matches` | Role-binding tests |
| `year`, `month`, `week`, `day`, ... | Duration arithmetic on date-time values |

**`UnaryStep`** — prefix operators on one sub-expression:

| Constructor | Meaning |
|---|---|
| `LogicalNot pos step` | Boolean negation |
| `Exists pos step` | Non-empty test |
| `FilledBy pos step` | Is the current role filled by a role from the given set? |
| `Fills pos step` | Does the current role fill any role in the given set? |
| `Available pos step` | Is the entity available (reachable) in the database? |

**`PureLetStep`** — `letE` binding:
```
letE x <- <expr>
     y <- <expr>
in   <expr>
```
Binds variables in a read-only scope; the bound values are available via `Variable`.

**`ComputationStep`** — external function call:
```
callExternal <functionName>(<arg>, ...) returns <type>
```
Used to invoke registered external (JavaScript/PureScript) functions.  
(See [Section 9](#9-extension-with-libraries-callexternal-and-calleffect) for detail.)

### Parser infrastructure

The parser operates on an indented token stream produced by `arcToken.purs`. It uses `purescript-parsing` with the `IndentParser` transformer to handle whitespace-sensitive syntax. Each AST node carries `ArcPosition` (source line and column) for error reporting.

Parsing of `callExternal` is handled by `computationStep`:

```purescript
computationStep :: IP Step
computationStep = do
  functionName <- reserved "callExternal" *> arcIdentifier
  arguments    <- token.symbol "(" *> argsUntilClose
  computedType <- reserved "returns" *> typeExpression
  pure $ Computation $ ComputationStep { functionName, arguments, computedType, ... }
```

`callEffect` and `callDestructiveEffect` appear in the **statement** grammar (not the expression grammar) and are parsed in `statementParser.purs`. They produce `Assignment` values (`ExternalEffect` and `ExternalDestructiveEffect`), not `Step` values.

---

## 4. Query Types: The Storable Description

### Module
`Perspectives.Query.QueryTypes`  
Source: `src/core/queries/queryFunctionDescription.purs`

After type checking and name resolution, the `Step` AST is compiled into a `QueryFunctionDescription` (QFD). This is the form that is stored as part of a model and distributed to peers.

### `QueryFunctionDescription`

```purescript
data QueryFunctionDescription
  = SQD Domain QueryFunction Range ThreeValuedLogic ThreeValuedLogic
  -- Simple: no sub-queries
  | UQD Domain QueryFunction QueryFunctionDescription Range ThreeValuedLogic ThreeValuedLogic
  -- Unary: one sub-query argument
  | BQD Domain QueryFunction QueryFunctionDescription QueryFunctionDescription Range ThreeValuedLogic ThreeValuedLogic
  -- Binary: two sub-query arguments
  | MQD Domain QueryFunction (Array QueryFunctionDescription) Range ThreeValuedLogic ThreeValuedLogic
  -- Multi-argument: array of sub-queries (used for external functions)
```

In every constructor:
- **`Domain`** — the input type of this (sub-)query.
- **`QueryFunction`** — the operation to perform.
- **`Range`** (= `Domain`) — the output type.
- **`ThreeValuedLogic` #1** — is the function *functional* (returns at most one result)?
- **`ThreeValuedLogic` #2** — is the function *mandatory* (always returns a result)?

Cardinality information (`functional`/`mandatory`) is propagated bottom-up during compilation and is used by the compiler and runtime to apply optimisations (e.g. skip `Array` wrapping for functional steps).

### `Domain`

```purescript
data Domain
  = RDOM (ADT RoleInContext)   -- A set of role types in context
  | CDOM (ADT ContextType)     -- A set of context types
  | VDOM RAN.Range (Maybe PropertyType)  -- A value domain with optional property type
  | ContextKind                -- Type-level: any context type
  | RoleKind                   -- Type-level: any role type
  | AnyRoleType                -- Untyped role (used in generic combinators)
```

`Range` is a type alias for `Domain`.

### `RoleInContext`

```purescript
newtype RoleInContext = RoleInContext
  { context :: ContextType
  , role    :: EnumeratedRoleType
  }
```

Because a single role type (via aspect inheritance) may appear in multiple context types, each role in a domain carries its defining context. This makes traversal unambiguous.

### `Calculation`

Calculated roles and properties store their definition as a `Calculation`:

```purescript
data Calculation
  = S Step Boolean              -- Parsed AST (plus functional flag); not yet compiled
  | Q QueryFunctionDescription  -- Compiled query description
```

A model freshly loaded from source holds `S` values. When first needed, the `S` is compiled to `Q` by the expression compiler.

### Serialisation

`QueryFunctionDescription` implements `WriteForeign` / `ReadForeign` (via `Simple.JSON`) using an intermediate flat record type `QFD_`. This allows QFDs to be stored in CouchDB and transmitted to peers.

### Key utility functions

| Function | Purpose |
|---|---|
| `domain`, `range` | Extract domain/range from any QFD constructor |
| `functional`, `mandatory` | Extract cardinality flags |
| `setCardinality` | Replace cardinality flags |
| `replaceDomain`, `replaceRange` | Replace domain/range |
| `makeComposition` | Build a `BQD ... ComposeF ...` from two QFDs |
| `addTermOnRight` | Extend a right-associative composition chain |
| `traverseQfd` | Structural traversal with a transformation function |
| `hasQueryFunction` | Search for a specific `QueryFunction` in the tree |
| `sumOfDomains`, `productOfDomains` | Combine domains (union / intersection of role ADTs) |

---

## 5. Expression Compiler: AST to Query Description

### Module
`Perspectives.Query.ExpressionCompiler`  
Source: `src/core/queries/expressionCompiler.purs`

The expression compiler transforms a `Step` (AST) into a fully typed `QueryFunctionDescription`. This happens in the `PhaseThree` monad, which has access to the complete type model (`DomeinFile`).

### Entry points

```purescript
compileExpression :: Domain -> Step -> FD
-- FD = PhaseThree QueryFunctionDescription

compileStep :: Domain -> Step -> FD
```

`compileExpression` is the public entry point. It qualifies unqualified `returns` type annotations in `ComputationStep`s, then delegates to `compileStep`. The `Domain` argument is the *input* domain: the type of entity the query starts from.

### Compilation of each `Step` variant

**`Simple SimpleStep`** — compiled by `compileSimpleStep`:

| `SimpleStep` | Resulting `QueryFunction` |
|---|---|
| `ArcIdentifier` | Resolves the name against the model; produces `RolGetter`, `PropertyGetter`, or a context getter |
| `Filler` | `DataTypeGetter FillerF` (with optional context restriction via a parameter) |
| `Filled` | `FilledF` (role type, context type) |
| `Context` | `DataTypeGetter ContextF` |
| `Extern` | `DataTypeGetter ExternalRoleF` |
| `Identity` | `DataTypeGetter IdentityF` |
| `Value` | `Constant range value` |
| `Variable` | `VariableLookup varName` |
| `Me` | `DataTypeGetter MeF` |
| `TypeOfContext` | `TypeGetter TypeOfContextF` |
| `TypeOfRole` | `TypeGetter TypeOfRoleF` |

**`Binary BinaryStep`** — compiled by `compileBinaryStep`:

| Operator | `QueryFunction` |
|---|---|
| `>>` | `BinaryCombinator ComposeF` |
| `filter ... with ...` | `FilterF` (unary, with criterion as sub-query) |
| `&&`, `\|\|` | `BinaryCombinator AndF`, `BinaryCombinator OrF` |
| `==`, `!=`, `<`, `<=`, `>`, `>=` | Comparison combinators |
| `+`, `-`, `*`, `/` | Arithmetic combinators |
| `union`, `intersection` | `BinaryCombinator UnionF`, `BinaryCombinator IntersectionF` |
| `orElse` | `BinaryCombinator OrElseF` |
| `binds` | `BinaryCombinator FilledByF` |
| `fills` | `BinaryCombinator FillsF` |

The composition operator `>>` is the most important. It builds a chain of `BQD ... ComposeF ...` nodes; the left sub-query feeds its output as the input for the right sub-query. Domain/range compatibility is checked: the range of the left must match the domain of the right.

**`Unary UnaryStep`** — compiled by `compileUnaryStep`:

| `UnaryStep` | `QueryFunction` |
|---|---|
| `LogicalNot` | `UnaryCombinator NotF` |
| `Exists` | `UnaryCombinator ExistsF` |
| `FilledBy` | `UnaryCombinator FilledByF` |
| `Fills` | `UnaryCombinator FillsF` |
| `Available` | `UnaryCombinator AvailableF` |

**`PureLet PureLetStep`** — compiled by expanding the `letE` bindings into nested `UQD (BindVariable ...)` and `UQD WithFrame` wrappers:

```
letE x <- e1
     y <- e2
in   body

→ UQD ... WithFrame
    (UQD ... (BindVariable "x") e1
      (UQD ... (BindVariable "y") e2
        body))
```

**`Computation ComputationStep`** — compiled in the `ComputationStep` branch of `compileStep`:

The `functionName` is looked up in `HiddenFunctionCache`. The compiler checks that:
- The function is registered.
- The call uses `callExternal` (not `callEffect`) for functions that return values (non-effects).
- The number of arguments matches the registered arity.
- The declared `returns` type is consistent.

The result is an `MQD` with `ExternalCoreRoleGetter` or `ExternalCorePropertyGetter` as the `QueryFunction`, and the compiled argument sub-queries as the `args` array.

### Type environment and error handling

All type lookups (resolving `ArcIdentifier`, checking binding types, etc.) go through the `PhaseThree` monad, which carries:
- A `DomeinFile` (the type model currently being compiled).
- A set of *currently being calculated* identifiers, used to detect circular calculated role/property definitions (via `withCurrentCalculation`).

Type errors throw `PerspectivesError` values that are collected and reported to the user.

---

## 6. Unsafe Compiler: Query Description to Executable Code

### Module
`Perspectives.Query.UnsafeCompiler`  
Source: `src/core/queries/unsafeCompiler.purs`

Given a `QueryFunctionDescription`, the unsafe compiler produces an executable PureScript function of type `String ~~> String` (where `~~>` is the alias for `String -> MonadPerspectivesQuery String`).

### Entry point

```purescript
compileFunction :: QueryFunctionDescription -> MP (String ~~> String)
```

`MP` is `MonadPerspectives` (= `ReaderT (AVar PerspectivesState) Aff`). The function is produced lazily: `compileFunction` itself may do some upfront work (e.g. look up role type definitions) but the resulting closure does the actual work when called.

The type `String ~~> String` is deliberately untyped: at runtime, the same function type is used regardless of whether the input/output represents contexts, roles, or property values. The correct interpretation is maintained by the domain/range metadata in the QFD, and by the call site which applies the correct coercion.

### Compilation of each QFD constructor

#### `SQD` — simple (leaf) functions

| `QueryFunction` | Compiled function |
|---|---|
| `RolGetter (ENR r)` | `getEnumeratedRoleInstances r` (or `externalRole` for external roles) |
| `RolGetter (CR cr)` | Fetches the `CalculatedRole`, compiles its `Calculation`, applies it |
| `PropertyGetter (ENP ep)` | `getProperty ep` (direct enumerated property access) |
| `PropertyGetter (CP cp)` | `getDynamicPropertyGetter` — fetches `CalculatedProperty`, compiles its calculation |
| `DataTypeGetter IdentityF` | Identity function |
| `DataTypeGetter ContextF` | `context` — embedding context of a role |
| `DataTypeGetter ExternalRoleF` | `externalRole` — external role of a context |
| `DataTypeGetter FillerF` | `binding` / recursive filler traversal |
| `DataTypeGetter MeF` | `getPerspectivesUser` — the current user |
| `TypeGetter TypeOfContextF` | `contextType` |
| `TypeGetter TypeOfRoleF` | `roleType` |
| `Constant range value` | Returns the literal `value` regardless of input |
| `VariableLookup varName` | Looks up `varName` in the current variable frame |

#### `UQD` — unary combinators

| `QueryFunction` | Behaviour |
|---|---|
| `BindVariable varName` | Runs sub-query, stores first result as `varName` in state |
| `WithFrame` | Pushes a new variable scope, runs sub-query, pops it |
| `UnaryCombinator ExistsF` | Returns `"true"`/`"false"` based on emptiness of sub-query results |
| `UnaryCombinator NotF` | Logical negation of a boolean sub-query |
| `UnaryCombinator FilledByF` | Tests whether the input role is filled by any role returned by sub-query |
| `UnaryCombinator FillsF` | Tests whether the input role fills any role returned by sub-query |
| `UnaryCombinator AvailableF` | Tests database reachability |
| `FilterF criterium` | Keeps only inputs for which `criterium` returns `"true"` |

#### `BQD` — binary combinators

| `QueryFunction` | Behaviour |
|---|---|
| `BinaryCombinator ComposeF` | Applies right function to each result of left function |
| `BinaryCombinator AndF` | Lazy AND (short-circuits on `"false"`) |
| `BinaryCombinator OrF` | Lazy OR (short-circuits on `"true"`) |
| `BinaryCombinator UnionF` | Set union of results from both sub-queries |
| `BinaryCombinator IntersectionF` | Set intersection |
| `BinaryCombinator OrElseF` | Returns results of left if non-empty, else right |
| `BinaryCombinator FilledByF` | Tests whether any of the left roles is filled by any of the right roles |
| `BinaryCombinator FillsF` | Tests whether any of the left roles fills any of the right roles |
| Comparison / arithmetic combinators | Applies the binary operation to the scalar values |

There is a special optimisation for `ComposeF` where the right operand is a `Constant`: the left is ignored entirely and the constant is returned directly.

#### `MQD` — multi-argument (external functions)

Used exclusively for `callExternal` invocations (see [Section 9](#9-extension-with-libraries-callexternal-and-calleffect)).

### Monad stack

Queries execute in `MonadPerspectivesQuery` (MPQ):

```
MPQ = ArrayT AssumptionTracking
    = ArrayT (WriterT (ArrayWithoutDoubles InformedAssumption) MonadPerspectives)
```

- **`ArrayT`** provides *non-determinism*: a query can return zero, one, or many results. The `ArrayT` transformer makes `MPQ` behave like a list monad.
- **`WriterT (ArrayWithoutDoubles InformedAssumption)`** accumulates *dependency assumptions* during execution. These are consumed by the cache invalidation / inverted query subsystem.
- **`MonadPerspectives`** = `ReaderT (AVar PerspectivesState) Aff` — the base effect monad, providing access to global state and asynchronous effects.

Lifting from `MP` to `MPQ` requires two `lift`s:

```purescript
lift2MPQ :: forall a. MP a -> MPQ a
lift2MPQ = lift <<< lift
```

### Compiled getter cache

`Perspectives.ObjectGetterLookup` (`objectsGetterLookup.purs`) declares two caches:

```purescript
roleGetterCache     :: RoleGetterCache         -- Object (ComputedFunction RoleGetter)
propertyValueGetterCache :: PropertyValueGetterCache  -- Object (ComputedFunction PropertyValueGetter)
```

These are intended to avoid re-compiling the same calculated role or property getter on every invocation. However, see [Section 10.1](#101-compiled-getter-cache-is-non-functional) for a critical issue.

---

## 7. Collecting Dependencies while Executing Queries

As a query executes, the system records which data points were visited. This information feeds the **inverted query** mechanism (outside the scope of this document), which detects which queries need re-evaluation when data changes.

### `InformedAssumption`

Defined in `Perspectives.CoreTypes`:

```purescript
data InformedAssumption
  = RoleAssumption ContextInstance EnumeratedRoleType
    -- "This context instance has (or had) a role of this type"
  | Me ContextInstance
    -- "The 'me' value for this context was accessed"
  | Filler RoleInstance
    -- "This role's filler was accessed"
  | FilledRolesAssumption RoleInstance ContextType EnumeratedRoleType
    -- "This role instance fills a role of this type in that context"
  | Property RoleInstance EnumeratedPropertyType
    -- "This property was read from this role instance"
  | Context RoleInstance
    -- "The context of this role was accessed"
  | External ContextInstance
    -- "The external role of this context was accessed"
  | State ContextInstance
    -- "The state of this context was accessed"
  | RoleState RoleInstance
    -- "The state of this role was accessed"
```

Each assumption precisely identifies a data point. When a delta arrives that modifies that data point, the inverted query system uses assumptions to find all affected queries and marks them for re-evaluation.

### `ArrayWithoutDoubles`

Assumptions are collected via `WriterT`. To avoid storing duplicates (which would cause redundant re-evaluations), the writer uses `ArrayWithoutDoubles`:

```purescript
newtype ArrayWithoutDoubles a = ArrayWithoutDoubles (Array a)

instance Eq a => Semigroup (ArrayWithoutDoubles a) where
  append (ArrayWithoutDoubles a1) (ArrayWithoutDoubles a2) =
    ArrayWithoutDoubles (a1 `union` a2)  -- union eliminates duplicates
```

### How assumptions are generated

The unsafe compiler generates assumptions *implicitly* by calling the low-level instance getters, which tell the writer about each access. For example, `getEnumeratedRoleInstances` records a `RoleAssumption`, and `getProperty` records a `Property` assumption.

The **interpreter** (next section) works differently: it does not call the low-level getters for every step. Instead, it builds explicit `DependencyPath` structures that describe which data points were touched, and then calls `pushAssumptionsForDependencyPath` at the end to convert those paths into `InformedAssumption` values.

---

## 8. The Interpreter

### Module
`Perspectives.Query.Interpreter`  
Source: `src/core/queries/queryFunctionInterpreter.purs`

### Purpose

The interpreter executes a `QueryFunctionDescription` with *explicit dependency tracking*. Unlike the compiled function (which implicitly accumulates assumptions via monad transformer effects), the interpreter builds `DependencyPath` values that explicitly represent the chain of entities traversed.

The interpreter is used primarily by the **inverted query** mechanism: when a delta arrives, the system applies the relevant inverse queries through the interpreter to determine which client subscriptions are affected and which peers must be informed.

### `DependencyPath`

Defined in `Perspectives.Query.Interpreter.Dependencies` (`dependencyPaths.purs`):

```purescript
type DependencyPath =
  { head            :: Dependency
  , mainPath        :: Maybe (NonEmptyList Dependency)
  , supportingPaths :: Array (NonEmptyList Dependency)
  }
```

- **`head`** — the current result (the "front" of the path as the query is being evaluated).
- **`mainPath`** — the linear chain of entities from the current result back to the original input (a `NonEmptyList` of `Dependency`). `Nothing` if only a single entity has been seen so far.
- **`supportingPaths`** — auxiliary paths produced by filter criteria or other branching sub-queries. These represent additional data points that must be monitored for changes, even though they are not on the main data path.

### `Dependency`

```purescript
data Dependency
  = C  ContextInstance
  | R  RoleInstance
  | V  String Value          -- property name + value
  | CT ContextType
  | RT RoleType
  | AnyRoleTypeDependency
```

### Path operations

| Function | Description |
|---|---|
| `singletonPath d` | Create a path with just one node |
| `consOnMainPath d path` | Prepend `d` to the main path; `d` becomes the new head |
| `snocOnMainPath path d` | Append `d` at the source end of the main path |
| `composePaths left right` | Chain two paths: `right.mainPath <> left.mainPath`; supporting paths are unioned |
| `appendPaths p1 p2` | Add `p2`'s paths as supporting paths on `p1` |
| `applyValueFunction f p1 p2` | Combine two value-headed paths with binary function `f` |

The `#>>` operator is an alias for `composePaths`:

```purescript
infixr 9 composePaths as #>>
```

### `interpret` — main entry point

```purescript
interpret :: QueryFunctionDescription -> DependencyPath ~~> DependencyPath
```

The interpreter dispatches on the QFD constructor:

```purescript
interpret qfd = case qfd of
  (UQD ...) -> interpretUQD qfd
  (BQD ...) -> interpretBQD qfd
  (MQD ...) -> interpretMQD qfd
  (SQD ...) -> interpretSQD qfd
```

#### `interpretSQD` — simple query steps

The result depends on the *type* of the current `head` in the input `DependencyPath`:

- **`C cid` (context instance)**: `RolGetter`, `ExternalRoleF`, `TypeOfContextF`, `IndexedContextName`, `GetRoleInstancesForContextFromDatabaseF`.
- **`R rid` (role instance)**: `PropertyGetter` (enumerated or calculated), `ContextF`, `TypeOfRoleF`, `FillerF`, `FilledF`, `IndexedRoleName`.
- **`V _ val` (value)**: `Value2Role` (coerce a value string back to a role identifier).
- **`CT contextType`** (context type): `RoleTypesF`, translation.
- **`RT roleType`** (role type): `SpecialisesRoleTypeF`, translation.

For each step, the interpreter wraps the result in a new `DependencyPath` with the previous head appended to the main path via `consOnMainPath`.

#### `interpretBQD` — binary combinators

- **`ComposeF`**: applies `f1` to the input, then applies `f2` to each result of `f1`, composing the resulting paths.
- **`FilterF`** / **`UnionF`** / **`IntersectionF`** / **`OrElseF`**: the supporting paths of the filter criterion are merged into `supportingPaths`.
- **Comparison and arithmetic operators**: both sub-queries are executed; the resulting values are combined with `applyValueFunction`.

#### `interpretUQD` — unary combinators

- **`BindVariable`**: evaluates the sub-query, stores the first result string in `PerspectivesState` under `varName`.
- **`WithFrame`**: pushes a frame on the variable scope, runs the sub-query, pops it.
- **`ExistsF`**, **`NotF`**, **`FilledByF`**, **`FillsF`**, **`AvailableF`**: test operations, return a boolean-valued `DependencyPath`.
- **`FilterF`**: keeps input paths for which the criterion is `true`; criterion paths become supporting paths.

#### `interpretMQD` — external functions

External functions (`ExternalCoreRoleGetter`, `ExternalCorePropertyGetter`) are supported: the function is retrieved from `HiddenFunctionCache` and called with the compiled argument values. The result updates the `head` of the input `DependencyPath`.

### `pushAssumptionsForDependencyPath`

```purescript
pushAssumptionsForDependencyPath :: Partial => DependencyPath -> AssumptionTracking Unit
```

Walks all paths in a `DependencyPath` and, for each `R rid` node, records a `FilledRolesAssumption` (context type, role type). This converts the interpreter's explicit path representation into the `InformedAssumption` values expected by the inverted query mechanism.

---

## 9. Extension with Libraries: callExternal and callEffect

The system supports two kinds of extension with external (non-Arc) functions: **pure external functions** (accessed via `callExternal`) and **effectful external functions** (accessed via `callEffect` and `callDestructiveEffect`).

### `callExternal` — pure external functions

**Syntax** (in expression context, e.g. calculated role body, state condition):
```
callExternal <functionName>(<arg>, ...) returns <type>
```

**Examples:**
```
callExternal util:SystemParameter( "CurrentVersion" ) returns String
callExternal cdb:RoleInstances( "model://perspectives.domains#MyModel$MyRole" ) returns String
```

**Pipeline:**
1. Parsed as `ComputationStep` in `expressionParser.purs` (by the `computationStep` parser).
2. Compiled to `MQD dom (ExternalCoreRoleGetter functionName) args ...` or `MQD dom (ExternalCorePropertyGetter functionName) args ...` in `expressionCompiler.purs`.
3. Executed in `unsafeCompiler.purs` via `compileFunction (MQD ...)`: retrieves the registered `HiddenFunction` and calls it with the evaluated argument arrays.

**Function registration:**  
External functions must be registered in `HiddenFunctionCache` (a global immutable `Object`). Registration provides:
- The function value itself (typed as `HiddenFunction` = `forall a. a`, to circumvent the PureScript type system).
- The number of additional arguments (beyond the implicit context/role argument).
- Whether it is an effect function (`isEffect :: Boolean`).
- Whether it is functional (returns at most one result).

**Argument protocol:**  
Each argument query is evaluated over the input (context or role instance), producing an `Array String`. The arrays are passed to the external function as positional `Array String` parameters, plus a final `String` parameter which is either the last argument's single value or the input string itself (if the argument count exceeds the defined arity). Up to 6 explicit arguments are supported (hard-coded dispatch in both `UnsafeCompiler` and `Interpreter`).

**Type checking:**  
The expression compiler verifies at compile time that `callExternal` is not used for effect functions (which must use `callEffect`). It also checks argument counts.

### `callEffect` — effectful external functions

**Syntax** (in action/assignment context):
```
callEffect <effectName>(<arg>, ...)
```

**Examples:**
```
callEffect cdb:AddModelToLocalStore( myModel )
callEffect util:SendNotification( subject, body, recipient )
```

`callEffect` appears in the **statement grammar** (`statementParser.purs`), not in the expression grammar. It produces an `Assignment` value (`ExternalEffect`), not a `Step`. Consequently, `callEffect` is compiled and executed differently — through the *action execution* machinery, not the query pipeline.

`callDestructiveEffect` is syntactically identical to `callEffect` but signals that the effect may modify or delete data, which affects transaction ordering and concurrency control.

### Comparison

| Feature | `callExternal` | `callEffect` / `callDestructiveEffect` |
|---|---|---|
| Grammar context | Expression (query) | Statement (action) |
| Returns a value | Yes | No |
| Side effects on Perspectives data | No | Yes |
| Used for | Calculated roles/props, state conditions | Actions, do-blocks |
| Produces in AST | `ComputationStep` | `ExternalEffect` / `ExternalDestructiveEffect` |
| Compiled to | `MQD (ExternalCoreRoleGetter ...)` | Action executor (outside query subsystem) |

---

## 10. Points of Attention

### 10.1 Compiled getter cache is non-functional

**Module:** `Perspectives.ObjectGetterLookup` (`objectsGetterLookup.purs`)  
**Severity:** High

The caches for compiled getter functions are declared as immutable values:

```purescript
roleGetterCache          :: RoleGetterCache          = empty
propertyValueGetterCache :: PropertyValueGetterCache = empty
```

`Foreign.Object` is an immutable persistent data structure. The insert function returns a *new* object; it does not mutate the original. The insertion site in `unsafeCompiler.purs` discards the result:

```purescript
void $ pure $ propertyGetterCacheInsert id (unsafeCoerce getter) functional mandatory
```

And the calculated role getter has an explicit `TODO`:

```purescript
getCalculatedRoleInstances rt@(CalculatedRoleType ident) c = case lookupRoleGetterByName ident of
  (Just g) -> g c
  Nothing -> do
    f <- ...compileFunction...
    -- TODO. Cache the function!
    (unsafeCoerce f) c
```

**Consequence:** Every invocation of a calculated role or calculated property recompiles the query from scratch. For complex queries this may be a performance bottleneck.

**Fix direction:** Replace the plain `Object` values with a mutable reference (e.g. `Ref` or an `AVar` stored in `PerspectivesState`), or use the existing LRU cache infrastructure already used for other caches in `PerspectivesState`.

### 10.2 Unreachable cases in the interpreter

**Module:** `Perspectives.Query.Interpreter`  
**Severity:** Low (compiler warning, not a runtime bug)

The file begins with this comment:

```
-- TODO
--  A case expression contains unreachable cases:
--
--   (MQD dom fun args ran _ _)                 a
--   (BQD _ (BinaryCombinator g) f1 f2 ran _ _) a
--   ...
-- in binding group interpret, getterFromPropertyType, getDynamicPropertyGetter
```

The Purescript compiler reports unreachable patterns in several `case` expressions. This likely arises from the combination of `Partial` constraints and catch-all patterns that are logically unreachable. The code functions correctly at runtime, but the warnings indicate potential dead code or imprecise pattern structure.

### 10.3 Interpreter coverage gaps compared to the compiler

The interpreter currently handles the most common operations but has some gaps compared to the full compiler:

- **`ContextIndividual`** in `interpretSQD`: always returns an empty array (`pure []`). The compiler produces a value for this. This is a known gap: context individuals may not be meaningful at instance level.
- **`AnyRoleType`** domain: only partially handled. Several `interpretSQD` dispatch arms cover only `C`, `R`, `V`, `CT`, and `RT`; an `AnyRoleTypeDependency` head will throw a runtime error.
- **State-related query functions** (`GetActiveStates`, `GetActiveRoleStates`): these are handled in the compiler but not mentioned in the interpreter dispatch. They may fall through to the `otherwise -> throwError` catch-all, causing a runtime error if the interpreter is called on a query that includes state conditions.
- **`MeF`** (current user): handled in the compiler but not explicitly matched in `interpretSQD`. It would fall to the `otherwise` catch-all in the `C cid` branch.

The comment in the TODO block at the top of the file suggests the authors are aware of these gaps.

### 10.4 External function arity limit

**Module:** `Perspectives.Query.UnsafeCompiler` and `Perspectives.Query.Interpreter`  
**Severity:** Low

External functions are limited to 6 explicit additional arguments (plus the implicit input string):

```purescript
_ -> throwError (error "Too many arguments for external core module: maximum is 6")
```

The limit is enforced identically in both the compiler and interpreter via hard-coded `case` expressions. Functions requiring more arguments must be wrapped in a higher-order external function.

### 10.5 Unsafe coercions and runtime type safety

**Module:** `Perspectives.Query.UnsafeCompiler`  
**Severity:** Informational

The unsafe compiler makes extensive use of `unsafeCoerce` to convert between different function types (`String ~~> String`, `ContextInstance ~~> RoleInstance`, `RoleInstance ~~> Value`, etc.). This is by design: the type system cannot express the domain/range relationship at the PureScript level, so type safety is maintained *by construction* in the expression compiler (which validates domains and ranges). A bug in the expression compiler that lets a domain/range mismatch through will not be caught by the PureScript type checker and will produce a runtime error.

### 10.6 Circular calculated definitions

**Module:** `Perspectives.Query.ExpressionCompiler`  
**Severity:** Informational

The expression compiler maintains a set of identifiers currently being compiled (via `withCurrentCalculation`). If the same identifier is encountered recursively, an error is thrown. This prevents infinite loops during compilation but does not prevent queries that would be infinitely recursive at *runtime* (e.g. a query that follows the filler chain without a termination condition). Such runtime loops would need to be detected separately.

### 10.7 Three-valued logic edge cases

**Module:** `Perspectives.Representation.ThreeValuedLogic`  
**Severity:** Low

The cardinality flags `functional` and `mandatory` use three-valued logic (`True`, `False`, `Unknown`). The propagation of `Unknown` through complex expressions (particularly through union, intersection, and filter) has not been formally verified. Incorrect propagation could cause the compiler to misclassify a query's cardinality, leading to subtle behavioural differences (e.g. using `head` on an array that may be empty).
