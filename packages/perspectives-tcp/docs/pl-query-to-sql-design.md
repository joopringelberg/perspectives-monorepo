# Design: PL Query Evaluation Based on SQL

> **Status**: Research document — no implementation.  
> **Context**: `packages/perspectives-tcp`  
> **Related module**: `Perspectives.Representation.QueryFunction` (source: `packages/perspectives-core/src/core/queries/queryFunction.purs`)

---

## Table of Contents

1. [Introduction and Scope](#1-introduction-and-scope)
2. [Assumed Relational Table Structure](#2-assumed-relational-table-structure)
3. [General SQL Translation Principles](#3-general-sql-translation-principles)
4. [QueryFunction Analysis](#4-queryfunction-analysis)
   - 4.1 [Functions Excluded from Analysis](#41-functions-excluded-from-analysis)
   - 4.2 [Category 1 — Straightforwardly Implementable in SQL](#42-category-1--straightforwardly-implementable-in-sql)
   - 4.3 [Category 2 — Cannot be Implemented in SQL](#43-category-2--cannot-be-implemented-in-sql)
   - 4.4 [Category 3 — Implementable with Limitations](#44-category-3--implementable-with-limitations)
   - 4.5 [Specialised Fillers: Problem, Runtime Fix, and Design Decision](#45-specialised-fillers-problem-runtime-fix-and-design-decision)
   - 4.6 [Design Decision: Omitting Filter Steps from Calculated Roles](#46-design-decision-omitting-filter-steps-from-calculated-roles)
5. [Composition: Building SQL Queries and Views from Query Paths](#5-composition-building-sql-queries-and-views-from-query-paths)
6. [Proposed Architecture for Automatic SQL Generation](#6-proposed-architecture-for-automatic-sql-generation)
   - 6.5 [Stable Identifiers and Readable Names](#65-stable-identifiers-and-readable-names)
   - 6.6 [`GenerateTCPConfiguration` — Callable from a Model](#66-generatetcpconfiguration--callable-from-a-model)
7. [Open Questions](#7-open-questions)

---

## 1. Introduction and Scope

The Perspectives Distributed Runtime (PDR) evaluates queries defined as paths over a doubly-linked graph of contexts and roles. This document explores to what extent each step in the Perspectives Query Language (PL) — as defined in `Perspectives.Representation.QueryFunction` — can be translated into an equivalent SQL expression for the relational database maintained by the Transaction Collection Point (TCP).

### Goal

An Onlooker (ultimate filler: `sys:Onlookers`) can hold a perspective on:

- A **Calculated role** — whose instances are produced by evaluating a PDR query.
- A role with a **Calculated property** — whose value is produced by evaluating a PDR query over the role graph.

For reporting purposes, we want to create SQL **views** over the TCP's relational tables that materialise the results of these queries, so that reporting tools (Power BI, Metabase, etc.) can query them without PDR involvement.

### Scope limits (per issue)

The following query functions are **out of scope** for this analysis:

| Excluded category | Reason |
|---|---|
| `callExternal` / `ExternalCoreRoleGetter`, `ExternalCorePropertyGetter`, `ExternalCoreContextGetter`, `ForeignRoleGetter`, `ForeignPropertyGetter` | External function calls into registered PureScript/JS libraries; no SQL equivalent exists |
| Steps returning type values: `TypeOfContextF`, `TypeOfRoleF`, `RoleTypesF`, `SpecialisesRoleTypeF`, `TypeGetter`, `RoleTypeConstant`, `ContextTypeConstant` | Return type metadata, not instance data; not meaningful in a data-level SQL query |
| `FilterF` | Filtering is applied by PDR peers before a mutation is forwarded to the TCP; the TCP receives only pre-filtered data. See [Section 4.6](#46-design-decision-omitting-filter-steps-from-calculated-roles) for the design decision on how this affects Calculated Role expansion. |
| Assignment operators: `CreateContext`, `CreateRole`, `Bind`, `Unbind`, `DeleteRole`, `DeleteContext`, `DeleteProperty`, `Move`, `RemoveRole`, `RemoveContext`, `AddPropertyValue`, `RemovePropertyValue`, `SetPropertyValue`, `CreateFileF`, `ExternalEffectFullFunction`, `ExternalDestructiveFunction`, `ForeignEffectFullFunction`, `BindResultFromCreatingAssignment` | These are action-execution operators, not query operators |
| `TranslateContextType`, `TranslateRoleType` | Translation of type identifiers to human-readable strings; purely presentational |
| `TypeTimeOnlyContextF`, `TypeTimeOnlyEnumeratedRoleF`, `TypeTimeOnlyCalculatedRoleF` | Compile-time-only type references; produce no runtime values |

---

## 2. Assumed Relational Table Structure

The TCP uses the **Smashed Role Chains** schema strategy:

### Context table

Two strategies are being considered for storing context instances:

**Strategy A — One table per context type** *(considered, not adopted)*:
```sql
CREATE TABLE <context_type_name> (
  id         TEXT PRIMARY KEY
  -- No other standard columns; context-level properties (if any) are written
  -- via the external role (see below).
);
```

**Strategy B — A single universal Context table** *(adopted)*:
```sql
CREATE TABLE context (
  id           TEXT PRIMARY KEY,
  context_type TEXT NOT NULL    -- the fully-qualified Perspectives context type URI
);
```

**Design decision**: Strategy B is adopted. Every role table's `context_id` column references this single `context` table. Context type information is stored as data (the `context_type` column) rather than as schema, which simplifies FK references and avoids the need to know the target context type when generating `ContextF` navigation steps.

### Role table (one per role type, including external roles)

External roles are stored as role tables, just like other (enumerated) roles. Each external role instance's `context_id` references the single universal `context` table. There is no separate handling of external roles at the schema level — the only distinction is that an external role type name conventionally ends with `$External`.

```sql
CREATE TABLE <role_type_name> (
  id         TEXT PRIMARY KEY,
  context_id TEXT REFERENCES <context_table>(id) ON DELETE CASCADE,
  filler_id  TEXT REFERENCES <filler_role_table>(id) ON DELETE SET NULL,
  <prop_1>   <sql_type>,   -- one column per Perspectives property in scope
  <prop_2>   <sql_type>,
  ...
);
```

### Key structural properties

| Property | SQL representation |
|---|---|
| Role belongs to a context | `context_id` FK in the role table |
| Role is filled by another role | `filler_id` FK in the role table |
| Context has a role | Reverse lookup: `SELECT * FROM <role_table> WHERE context_id = ?` |
| Role fills another role | Reverse lookup: `SELECT * FROM <filled_role_table> WHERE filler_id = ?` |
| Property value of a role | Column in the role's table |

**Critical difference from PDR**: The PDR's native data structure is doubly-linked — every entity carries pointers in both directions. In the SQL schema, the links are **unidirectional**:

- Role → context: via `context_id` (efficient)
- Role → filler: via `filler_id` (efficient)
- Context → roles: via WHERE clause (requires knowing the role type / table)
- Filled role → filler: via WHERE clause (requires knowing the filled role type / table)

All inter-table relationships are known at query generation time because the PDR's compiled `QueryFunctionDescription` carries full type information at every step.

### Example schema (from existing TCP test data)

```sql
CREATE TABLE bijeenkomsten (   -- context type: Meeting
  id TEXT PRIMARY KEY
);

CREATE TABLE geregistreerden (  -- role type: Registrant
  id         TEXT PRIMARY KEY,
  context_id TEXT REFERENCES bijeenkomsten(id) ON DELETE CASCADE,
  filler_id  TEXT REFERENCES geregistreerden(id) ON DELETE SET NULL,
  voornaam   TEXT,
  achternaam TEXT
);
```

---

## 3. General SQL Translation Principles

### 3.1 Mapping query navigation to SQL patterns

A PDR query is a chain of navigation steps connected by the `>>` composition operator. Each step takes a **set of instances** as input and returns a **set of instances** (or values) as output. In SQL, this maps to:

| PDR composition form | SQL equivalent |
|---|---|
| Single step (leaf) | A `SELECT` statement with a `WHERE` clause or a column projection |
| `step1 >> step2` (ComposeF) | Nested subquery or JOIN; the output of `step1` becomes the input of `step2` |
| `step1 \`union\` step2` | `UNION ALL` (or `UNION` for deduplication) |
| `step1 \`intersection\` step2` | `INTERSECT` |
| `step1 orElse step2` | CTE-based fallback (see Category 3) |
| `letE x <- expr in body` | `WITH x AS (expr) SELECT ... FROM body` |

### 3.2 Navigation direction

Every PDR query step has a known domain (input type) and range (output type), determined at compile time by the expression compiler. This means the SQL translator always knows:

- Which table the input comes from
- Which table the output goes to
- Which FK column connects them

### 3.3 Output form for SQL views

SQL views must produce a flat table. For a perspective on a calculated role, the view returns the role instances. For a perspective on a calculated property, the view returns (role-instance-id, computed-value) pairs.

The `QueryFunctionDescription` tree bottom-up determines whether the output is a set of identifiers (for roles/contexts) or a set of values (for properties).

---

## 4. QueryFunction Analysis

### 4.1 Functions Excluded from Analysis

See [Section 1](#1-introduction-and-scope).

Additionally, the following are excluded because they are query-internal scaffolding with no direct data-access semantics:

| Function | Reason for exclusion |
|---|---|
| `WithFrame` | Variable scoping wrapper; replaced by CTE scope in SQL |
| `BindResultFromCreatingAssignment` | Binds the result of a side-effecting creation; irrelevant for read queries |
| `AssignmentOperator` | Marks assignment operations, not a query step |
| `PublicContext`, `PublicRole` | These refer to published contexts/roles accessible from the internet. In the TCP's local DB they are stored as ordinary rows; no special handling is needed. Treat as `ContextIndividual`/`RoleIndividual`. |

---

### 4.2 Category 1 — Straightforwardly Implementable in SQL

The following query functions have a direct, unambiguous SQL equivalent.

---

#### `DataTypeGetter ContextF`
**PDR semantics**: From a role instance, return its embedding context instance.

**SQL pattern**:
```sql
-- Input: role instance id from <role_table>
SELECT c.*
FROM <context_table> c
WHERE c.id = (SELECT context_id FROM <role_table> WHERE id = :role_id)

-- Or equivalently in a join:
SELECT c.*
FROM <role_table> r
JOIN <context_table> c ON c.id = r.context_id
WHERE r.id = :role_id
```

The role table has a `context_id` FK column, making this a trivial lookup. The context type (and thus the target table) is always known from the domain information in the QFD.

---

#### `DataTypeGetter ExternalRoleF`
**PDR semantics**: From a context instance, return its external role instance. The external role type is `<ContextType>$External`.

**SQL pattern**:
```sql
-- The external role is stored in a dedicated table for <ContextType>$External
SELECT ext.*
FROM <context_type_external_role_table> ext
WHERE ext.context_id = :context_id
```

The external role type is always known: it is `<contextType>$External`. Per the design decision in [Section 2](#2-assumed-relational-table-structure), external roles are stored as dedicated role tables. `ExternalRoleF` maps to a simple SELECT on the external role table filtered by `context_id`.

> **Design decision**: External role properties are stored in the external role's own table (a row in `<contextType>_external`), not inlined into the context table. The universal `context` table holds only `id` and `context_type`. See [Q2](#-q2-external-role-storage-and-context-table-strategy-resolved) for the decision.

---

#### `DataTypeGetter DirectFillerF`
**PDR semantics**: Return the immediate (direct) filler of a role — one hop up the filler chain.

**SQL pattern**:
```sql
SELECT f.*
FROM <filler_role_table> f
WHERE f.id = (SELECT filler_id FROM <filled_role_table> WHERE id = :role_id)
```

The filler's role type (and thus table) is known statically from the QFD's range domain.

---

#### `RolGetter (ENR roleType)` — Enumerated role getter
**PDR semantics**: From a context instance, return all role instances of the given enumerated role type.

**SQL pattern**:
```sql
SELECT r.*
FROM <role_table> r
WHERE r.context_id = :context_id
```

---

#### `PropertyGetter (ENP propertyType)` — Enumerated property getter
**PDR semantics**: From a role instance, return the value of an enumerated (stored) property.

**SQL pattern**:
```sql
SELECT r.<property_column>
FROM <role_table> r
WHERE r.id = :role_id
```

The property maps to a column in the role's table. The column name is determined from the TCP configuration (`TableConfig.columns`).

---

#### `FilledF enumeratedRoleType contextType`
**PDR semantics**: From a filler role instance, return all role instances of the given type that are filled by this role (one hop down the filling chain).

**SQL pattern**:
```sql
SELECT r.*
FROM <filled_role_table> r         -- table for enumeratedRoleType
WHERE r.filler_id = :current_role_id
  -- Optionally constrain to a specific context type:
  AND r.context_id IN (
    SELECT id FROM <context_type_table>
  )
```

The filled role type (and its table) are carried in the `FilledF` constructor; this is a simple reverse FK lookup.

---

#### `GetRoleInstancesForContextFromDatabaseF`
**PDR semantics**: Like `RolGetter` but explicitly database-oriented (bypasses PDR in-memory cache). Used for relational role types.

**SQL pattern**: Identical to `RolGetter`:
```sql
SELECT r.*
FROM <role_table> r
WHERE r.context_id = :context_id
```

---

#### `DataTypeGetter IdentityF`
**PDR semantics**: Pass-through — return the input unchanged.

**SQL**: No operation; the current expression is simply forwarded.

---

#### `Constant range value`
**PDR semantics**: Return the literal string `value` regardless of input.

**SQL pattern**:
```sql
SELECT '<value>'    -- as a scalar expression
-- or in a column list:
SELECT '<value>' AS computed_col, ...
```

---

#### `RoleIndividual roleInstance`
**PDR semantics**: Return the specific role instance identified by `roleInstance` (a constant identifier).

**SQL pattern**:
```sql
SELECT *
FROM <role_table>
WHERE id = '<role_instance_id>'
```

The target table is known from the QFD range domain.

---

#### `ContextIndividual contextInstance`
**PDR semantics**: Return the specific context instance identified by `contextInstance`.

**SQL pattern**:
```sql
SELECT *
FROM <context_table>
WHERE id = '<context_instance_id>'
```

---

#### `BinaryCombinator ComposeF` (the `>>` operator)
**PDR semantics**: Apply the right query to every result of the left query.

**SQL**: The fundamental composition operator. Translates to a JOIN or nested subquery:
```sql
-- step1 >> step2 where step1 returns roles and step2 reads a property:
SELECT r2.<property_column>
FROM <role_table_1> r1
JOIN <role_table_2> r2 ON r2.id = r1.filler_id
WHERE r1.context_id = :context_id
```

For chains with multiple steps, the JOINs accumulate. This is the backbone of all multi-step query translation.

---

#### `BinaryCombinator UnionF`
**PDR semantics**: Set union of the results of two sub-queries.

**SQL**:
```sql
(SELECT id FROM <table_a> WHERE ...)
UNION ALL
(SELECT id FROM <table_b> WHERE ...)
```

Use `UNION` (without `ALL`) if deduplication is required. Use `UNION ALL` for performance when duplicates are acceptable or known not to arise.

---

#### `BinaryCombinator IntersectionF`
**PDR semantics**: Set intersection of the results of two sub-queries.

**SQL**:
```sql
(SELECT id FROM <table_a> WHERE ...)
INTERSECT
(SELECT id FROM <table_b> WHERE ...)
```

Supported in PostgreSQL, MySQL 8.0+, SQLite 3.39+, and MSSQL.

---

#### `BinaryCombinator AndF`, `BinaryCombinator OrF`
**PDR semantics**: Logical AND / OR of two boolean sub-expressions.

**SQL**: Standard `AND` / `OR` operators in `WHERE` or `CASE` expressions.

---

#### Comparison combinators: `EqualsF`, `NotEqualsF`, `LessThanF`, `LessThanEqualF`, `GreaterThanF`, `GreaterThanEqualF`
**PDR semantics**: Compare two values.

**SQL**: `=`, `!=` (or `<>`), `<`, `<=`, `>`, `>=` — direct equivalents.

---

#### Arithmetic combinators: `AddF`, `SubtractF`, `DivideF`, `MultiplyF`
**PDR semantics**: Arithmetic on numeric values.

**SQL**: `+`, `-`, `/`, `*` — direct equivalents.

---

#### `UnaryCombinator ExistsF`
**PDR semantics**: Returns `true` if the sub-query produces at least one result, `false` otherwise.

**SQL**:
```sql
CASE WHEN EXISTS(SELECT 1 FROM ... WHERE ...) THEN 'true' ELSE 'false' END
```

Or in a `WHERE` clause: `EXISTS(SELECT 1 FROM ...)`.

---

#### `UnaryCombinator NotF`
**PDR semantics**: Logical negation of a boolean sub-expression.

**SQL**: `NOT(...)` — direct equivalent.

---

#### `UnaryCombinator CountF`
**PDR semantics**: Return the count of the results of the sub-query. Applied to the complete result array of a preceding step via `BinaryCombinator ComposeSequenceF` (see below).

**SQL**: `COUNT(*)` — direct equivalent. Example:
```sql
SELECT COUNT(*) FROM <role_table> WHERE context_id = :context_id
```

---

#### `UnaryCombinator MinimumF`, `UnaryCombinator MaximumF`
**PDR semantics**: Return the minimum / maximum value from a set. Like `CountF`, these are "sequence functions" applied via `BinaryCombinator ComposeSequenceF`.

**SQL**: `MIN()`, `MAX()` — direct equivalents:
```sql
SELECT MIN(<property_column>) FROM <role_table> WHERE context_id = :context_id
SELECT MAX(<property_column>) FROM <role_table> WHERE context_id = :context_id
```

---

#### `UnaryCombinator AvailableF`
**PDR semantics**: Tests whether the referenced entity is reachable in the database.

**SQL**: Interpreted as "a row with this id exists":
```sql
CASE WHEN EXISTS(SELECT 1 FROM <table> WHERE id = :id) THEN 'true' ELSE 'false' END
```

This is a reasonable interpretation for the TCP: if the row is not in the TCP database, the entity is not available from a reporting perspective.

---

#### `UnaryCombinator FillsF` / `BinaryCombinator FillsF` (single-hop)
**PDR semantics**: Tests whether the current (input) role fills any role in a given set.

**SQL** (single-hop check — direct filler_id reverse lookup):
```sql
-- Does role :rid fill any role in <target_table>?
EXISTS(
  SELECT 1
  FROM <target_role_table>
  WHERE filler_id = :rid
)
```

For the single-hop case this is straightforward. See also [Category 3 — `FillsF` across a full chain](#fillsf-full-chain).

---

#### `BinaryCombinator ComposeSequenceF`
**PDR semantics**: The left sub-query produces a set of results; the right sub-query is a "sequence function" (aggregation) that consumes the complete result set. The right operand is always one of `UnaryCombinator CountF`, `UnaryCombinator MinimumF`, `UnaryCombinator MaximumF`, `UnaryCombinator AddF` (sum), or `UnaryCombinator FirstF`.

**SQL**: The aggregation wraps the left query as a subquery:
```sql
-- count example: how many Participants in this Meeting?
SELECT COUNT(*)
FROM participant
WHERE context_id = :meeting_id

-- minimum example: earliest registration date
SELECT MIN(registered_at)
FROM participant
WHERE context_id = :meeting_id

-- sum example: total amount across all Payment roles in a context
SELECT SUM(amount)
FROM payment_role
WHERE context_id = :context_id
```

**Category 1** for all supported sequence functions. The aggregation function maps directly to standard SQL aggregate functions.

---

#### `BinaryCombinator SequenceF`
**PDR semantics**: Execute both sub-queries sequentially, returning the result of the second. Used primarily when the first sub-query sets up variable bindings (via `BindVariable`) that are used by the second. At compile time, `TypeTimeOnlyContextF`/`TypeTimeOnlyEnumeratedRoleF` bindings are stripped, so this operator mainly survives in queries with runtime variable bindings.

**SQL**: In the absence of runtime variable bindings (the common case in read queries), `SequenceF` degenerates to executing only the second sub-query:
```sql
-- Degenerate case: just execute f2
SELECT ... FROM ...
```

When both sides are non-trivial, a CTE chain handles the sequential binding:
```sql
WITH
  step1 AS (/* f1 */),
  step2 AS (/* f2, optionally referencing step1 */)
SELECT * FROM step2
```

**Category 3**: Implementable when both sides are expressible in SQL; degenerate cases are Category 1.

---

### 4.3 Category 2 — Cannot be Implemented in SQL

#### `DataTypeGetter MeF`
**PDR semantics**: Return the current user (the role instance representing "me" in the current PDR session).

**Decision**: `MeF` is **prohibited in Onlooker queries**. A calculated role or calculated property used in an Onlooker perspective must not contain a `me` step. The PDR's model compiler should detect this at compile time and report a clear error to the modeller, together with guidance on which other query steps are unsupported in Onlooker contexts (see [Section 4.2 exclusions](#41-functions-excluded-from-analysis) and [Q9](#-q9-handling-unsupported-query-steps-resolved)).

**Rationale**: SQL queries are executed against a database without a concept of "currently authenticated Perspectives user". The Onlooker role, by its nature, observes *other* users' data; a reference to `me` in such a perspective would typically be a modelling error. Prohibiting it and providing a clear error message is preferable to attempting a partial workaround.

---

#### `RolGetter (CR calculatedRoleType)` — Calculated role getter (unresolved)
**PDR semantics**: Evaluate a calculated role by recursively executing its definition query.

**Why not directly in SQL**: The definition of a calculated role is itself a `QueryFunctionDescription` stored in the model. Translating a calculated role getter therefore requires recursively translating its definition query into SQL. This is not a fundamental obstacle — the PDR compiler can expand calculated roles into their definitions before SQL generation (analogous to macro expansion). However, if the calculated role definition references further calculated roles, the expansion must be repeated, and circular definitions must be detected.

**Assessment**: The SQL translator should inline (expand) calculated role definitions recursively during query generation. Only leaf `RolGetter (ENR ...)` steps (enumerated roles) need actual SQL table lookups. This puts the burden on the PDR's query-to-SQL compiler, not on the SQL runtime.

---

### 4.4 Category 3 — Implementable with Limitations

#### `DataTypeGetter FillerF`
**PDR semantics**: Traverse the full filler chain, following `filler_id` links recursively until the bottom (ultimate filler) is reached.

**Limitation**: The full filler chain may span multiple role tables (e.g., `Participant` is filled by `Profile`, which is filled by `NationalProfile`). A recursive CTE in SQL cannot span multiple tables.

Note that the fill relation is **never truly self-referential** in Perspectives: a role of type R cannot be filled by another instance of type R, and a role instance cannot fill itself. This means the filler chain is always acyclic and finite. The maximum supported chain depth is **5 hops** (expandable to 6 if required in future).

**SQL for a single-table filler chain** (same role type fills itself — e.g., a tree of organisational units):
```sql
WITH RECURSIVE filler_chain(id, filler_id) AS (
  SELECT id, filler_id
  FROM <role_table>
  WHERE id = :start_role_id

  UNION ALL

  SELECT r.id, r.filler_id
  FROM <role_table> r
  JOIN filler_chain fc ON r.id = fc.filler_id
  WHERE r.filler_id IS NOT NULL
)
SELECT *
FROM filler_chain
WHERE filler_id IS NULL   -- the ultimate filler has no further filler
```

This works when the entire filler chain stays within one table (one role type fills another of the same type).

**When the chain crosses tables**: The PDR model always knows the complete filler chain type statically. The chain is unrolled into a fixed-depth join sequence using the statically-known types at each hop:

```sql
-- Filler chain: Participant → Profile → NationalProfile (depth 2)
SELECT np.*
FROM participant_role p
JOIN profile_role pr      ON pr.id = p.filler_id
JOIN nationalprofile_role np ON np.id = pr.filler_id
WHERE p.id = :participant_id
```

**Decision**: The PDR compiler should unroll filler chains into a fixed-depth JOIN sequence. Maximum supported depth is **5** (may be increased to 6 later). At generation time, if the statically-known chain exceeds this limit, the PDR should emit a warning and the affected view should be flagged as unsupported.

> **Specialised fillers**: See [Section 4.5](#45-specialised-fillers-problem-runtime-fix-and-design-decision) for background on the specialised-filler scenario and the runtime-level fix that makes this fixed-depth approach correct.

---

#### `DataTypeGetterWithParameter FillerF contextType`
**PDR semantics**: Follow the filler chain but return only the filler that resides in a context of the given type.

**SQL**: Similar to `FillerF` but with an additional join to filter by context type:
```sql
-- Unrolled example: find the filler of a Participant that belongs to an Organization context
SELECT f.*
FROM participant_role p
JOIN <filler_role_table> f ON f.id = p.filler_id
JOIN <organization_context_table> ctx ON ctx.id = f.context_id
WHERE p.id = :participant_id
```

The same unrolling limitation as `FillerF` applies.

---

#### `UnaryCombinator FilledByF` / `BinaryCombinator FilledByF` (single-hop vs. full chain)
**PDR semantics**: Tests whether the current role is filled by (directly or indirectly) any role in a given set.

**Single-hop SQL** (straightforward):
```sql
-- Is role :rid filled by any role in <source_set>?
:rid IN (
  SELECT filler_id
  FROM <role_table>
  WHERE filler_id IN (SELECT id FROM <source_set_query>)
)
```

**Full-chain check**: Requires the same recursive CTE approach as `FillerF`. Subject to the same cross-table limitation.

---

#### `FillsF` (full chain)
**PDR semantics**: Tests whether the current role fills any role in a given set, traversing the full chain.

**Single-hop** (Category 1 above) works when only an immediate fill check is needed. For full-chain: recursive CTE subject to the same limitations as `FillerF`.

---

#### `BinaryCombinator OrElseF`
**PDR semantics**: Return the results of the left sub-query if non-empty; otherwise return the results of the right sub-query.

**SQL**: Requires a CTE with conditional fallback:
```sql
WITH left_result AS (
  -- left sub-query
  SELECT * FROM ...
),
right_result AS (
  -- right sub-query
  SELECT * FROM ...
)
SELECT * FROM left_result
UNION ALL
SELECT * FROM right_result
WHERE NOT EXISTS (SELECT 1 FROM left_result)
```

This is valid standard SQL and works across all supported databases. It is somewhat verbose but mechanically derivable from the QFD tree.

---

#### `UnaryCombinator FirstF`
**PDR semantics**: Return only the first result from a set. Like `CountF`, `MinimumF` and `MaximumF`, this is a "sequence function" applied via `BinaryCombinator ComposeSequenceF`.

**SQL**: `LIMIT 1` / `FETCH FIRST 1 ROW ONLY`.

**Limitation**: SQL result order is non-deterministic without an `ORDER BY` clause. Perspectives instances do not have an inherent SQL-level order. If `FirstF` in the PL query refers to the "first instance created" (e.g., insertion order), the TCP tables would need a `created_at` timestamp column or sequential integer to establish an order.

**Recommendation**: When translating `FirstF`, add `ORDER BY id` (which is a CUID and therefore roughly time-ordered) or introduce a `created_at` column. Flag this choice as a semantic approximation.

---

#### `VariableLookup varName`, `BindVariable varName`, `WithFrame`
**PDR semantics**: Variable bindings from `letE` expressions in PL queries. `BindVariable` assigns the result of a sub-query to a name; `WithFrame` creates a new variable scope; `VariableLookup` retrieves the bound value.

**SQL**: Common Table Expressions (CTEs) provide an equivalent mechanism:
```sql
-- letE x <- expr1; y <- expr2 in body
WITH
  x AS (/* expr1 */),
  y AS (/* expr2 */)
/* body */
SELECT ...
```

**Limitation**: PDR `letE` bindings are sequential — the value of `x` may be used in the computation of `y`. SQL CTEs are non-recursive by default and do not share an execution context. However, a CTE can reference a previous CTE in the same `WITH` block, so sequential dependencies are supported.

The main limitation is that if `BindVariable` stores only the *first* result (as the PDR's unsafe compiler does), and the SQL CTE returns multiple rows, additional handling (e.g., `LIMIT 1` on the CTE) is needed to match PDR semantics.

---

#### `DataTypeGetter ModelNameF`
**PDR semantics**: Return the model namespace (a string identifying the model).

**SQL**: The model namespace is known at query generation time (it is a compile-time constant). The SQL translator should emit a string literal:
```sql
SELECT 'model://perspectives.domains#MyModel' AS model_namespace
```

No runtime lookup is needed.

---

#### `DataTypeGetter IndexedContextName`, `DataTypeGetter IndexedRoleName`
**PDR semantics**: Look up the specific instance associated with a symbolic indexed name (e.g., "the world context for user X"). The semantics of indexed names is that each peer resolves the name to their own instance — like the word "home" which means something different for each person.

**Why prohibited in Onlooker queries**: An Onlooker is an impersonal role by definition. It has no identity of its own and therefore no personal indexed name can have any meaningful referent in an Onlooker context. An indexed name in a query that an Onlooker has a perspective on would be a modelling error.

**Decision**: `IndexedContextName` and `IndexedRoleName` are **prohibited in Onlooker queries**, for the same reasons as `MeF`. The PDR model compiler must detect their presence in a calculated role or calculated property that forms part of an Onlooker's perspective, and report a clear error to the modeller (see also [Q9](#-q9-handling-unsupported-query-steps-resolved)).

---

#### `Value2Role propertyType`
**PDR semantics**: Coerce a property value string (which is a role instance identifier stored in a property) back into a role reference, enabling further navigation.

**SQL**: This step effectively joins from one table to another using a property column as a foreign key:
```sql
SELECT target.*
FROM <source_role_table> src
JOIN <target_role_table> target ON target.id = src.<property_column>
WHERE src.id = :role_id
```

**Limitation**: The target role type (and thus target table) must be derivable from the property type. In the PDR model, the property type carries range information that tells the compiler which role type the value refers to. The SQL translator needs access to this type information (from the QFD's `VDOM` range domain plus the `Value2Role` constructor's `PropertyType` argument).

This is feasible as long as the model's type metadata is available to the SQL generator.

---

#### `RegExMatch regExP`
**PDR semantics**: Tests whether a string value matches a regular expression.

**SQL**: Supported, but syntax varies by database:

| Database | SQL syntax |
|---|---|
| PostgreSQL | `value ~ 'pattern'` |
| MySQL / MariaDB | `value REGEXP 'pattern'` |
| SQLite | `value REGEXP 'pattern'` (requires the REGEXP extension; built-in since 3.38) |
| Microsoft SQL Server | No native REGEXP; use `LIKE` for simple patterns, or SQLCLR for full regex |

**Recommendation**: The SQL generator should emit database-specific regex syntax based on the configured Knex `client`. For MSSQL, either restrict to `LIKE`-expressible patterns or flag regex as unsupported.

---

#### `DataTypeGetter IsInStateF` (or `UnaryCombinator IsInStateF`)
**PDR semantics**: Tests whether the current entity (context or role instance) is in a named state. States are computed from conditions defined in the ARC model, not stored as flat values.

**Why prohibited in Onlooker queries**: The PDR never emits state information from one installation to another — state is always re-evaluated locally from the data available to a peer. By extension, the TCP never receives state values as part of the delta stream. Re-evaluating state conditions from raw TCP data would require re-implementing the PDR's state machine in SQL, which is out of scope and error-prone.

**Decision**: `IsInStateF` is **prohibited in Onlooker queries**. Its presence in a calculated role or calculated property that forms part of an Onlooker's perspective is a modelling error. The PDR model compiler must detect and report this (see [Q9](#-q9-handling-unsupported-query-steps-resolved)).

---

### 4.5 Specialised Fillers: Problem, Runtime Fix, and Design Decision

#### The original problem

A role's definition may include a `filledBy` constraint, e.g.:

```arc
role Participant
  filledBy PersonProfile
```

This asserts that `Participant` must be filled by an instance of type `PersonProfile` (or a specialisation thereof). The SQL translator would naturally unroll this as a JOIN to the `person_profile` table.

However, the Perspectives type system allows a filler instance's **effective type set** to include types it is itself filled by. Concretely:

- `PersonProfile` might itself be filled by `RichPersonProfile` (a specialised variant).
- From the type system's perspective, the `RichPersonProfile` instance counts as an instance of `PersonProfile` (because it specialises `PersonProfile` via the filler chain).
- Therefore a `Participant` might in principle be effectively filled by a `RichPersonProfile` instance rather than a plain `PersonProfile` instance.

This would introduce an **extra hop** invisible to the compile-time `filledBy` type, causing the SQL JOIN to point to `person_profile` while the actual filler row lives in `rich_person_profile`.

#### Runtime-level fix

The PDR runtime has been updated to **prevent this situation from arising at data entry time**: when a role is filled, the runtime ensures the filler is never an instance that itself has a filler complying with the filled role's `filledBy` restriction. In other words, the runtime enforces that the immediate filler is always a "leaf" instance at the correct level of the type hierarchy — it does not delegate further down via another filler hop.

This runtime guarantee means the SQL translator can rely on the compile-time `filledBy` type being the actual storage location of the filler, without needing to chase additional hops at query time.

#### Design decision: fixed-depth filler chain

As a direct consequence of the runtime guarantee above, the design adopts the following decision:

> **Properties of a filler role can always be found at a compile-time-known, fixed depth in terms of filler hops.**

The SQL translator unrolls filler chains into a fixed-depth JOIN sequence determined entirely by the statically-known `filledBy` types in the model. No runtime introspection of filler chains is needed. The maximum supported depth remains **5 hops** (extendable to 6).

This eliminates the need for the warning mechanism described in the earlier draft of this section. The specialised-filler scenario is now a historical note rather than an active risk.

---

### 4.6 Design Decision: Omitting Filter Steps from Calculated Roles

#### Background

The Perspectives information-sharing model is built on a **need-to-know principle**: a PDR peer only shares the data that the Perspectives model authorises for the receiving peer. Before forwarding a transaction to the TCP (which acts as an Onlooker), the sending peer therefore already applies all relevant perspective filters — the TCP receives only the data that the Onlooker role is entitled to see.

A **Calculated role** in Perspectives may be defined as a filtered subset of an enumerated role, for example:

```arc
case Bijeenkomst

  user Aanwezigen (relational) filledBy Deelnemer
    property Aanwezig (Boolean)

  user Geregistreerden = filter Aanwezigen with Aanwezig

  user Meekijker = Organizer >> binding >> context >> Meekijker
    perspective on Geregistreerden
      props (FirstName, LastName) verbs (Consult)
```

Here `Geregistreerden` is defined as the subset of `Aanwezigen` instances where `Aanwezig` is `true`.

#### Why re-applying the filter in the TCP would produce wrong results

When the `Organizer` peer sends transactions to the TCP/Meekijker, it only forwards instances of `Aanwezigen` who have `Aanwezig = true` — the `Organizer`'s own PDR has already applied the perspective filter. As a result:

1. The TCP's relational database never receives `Aanwezigen` rows where `Aanwezig = false`.
2. Critically, `Meekijker` does **not** have a perspective on `Aanwezigen` that includes the `Aanwezig` property — so the `Aanwezig` column is not even present in the TCP's role table for `Aanwezigen`.

If the TCP were to translate `Geregistreerden` faithfully as `SELECT * FROM aanwezigen WHERE aanwezig = true`, it would find **no results** for every row. Under the [Closed World Assumption](https://en.wikipedia.org/wiki/Closed-world_assumption), the absence of the `Aanwezig` column is interpreted as `false`, so the filter predicate fails universally.

#### Could filter data arrive from a different peer?

One might ask: could a situation arise where one peer sends the unfiltered `Aanwezigen` data and a different peer sends the `Aanwezig` property values, so that the TCP could reassemble the complete picture and apply the filter itself?

In the Perspectives model, this cannot happen for Onlooker perspectives used for reporting. An Onlooker's perspective is defined at the model level and is consistent across all peers who share data with the TCP. A peer who has the `Aanwezig` property in their perspective would already apply the filter before forwarding transactions; the TCP receives the pre-filtered result, not the raw inputs.

#### Design decision

> **The TCP's SQL view generator must omit `FilterF` steps when expanding a Calculated Role definition.**
>
> When the PDR compiler expands a `RolGetter (CR calculatedRoleType)` into its definition QFD for SQL generation, any `FilterF` node in the definition tree is **dropped**. The role on which the filter was applied (the unfiltered base role) is used directly.
>
> In the example above, `Geregistreerden` is treated as equivalent to `Aanwezigen` for SQL view purposes. The view reads:
>
> ```sql
> -- View for Geregistreerden (filter step dropped):
> SELECT id, context_id, first_name, last_name
> FROM aanwezigen
> WHERE context_id = :context_id
> ```
>
> rather than including a `WHERE aanwezig = true` predicate that would always produce an empty result set.

#### Considerations and caveats

1. **Correctness in the TCP context**: The design decision is correct because the TCP only ever receives already-filtered data. The SQL view accurately reflects the set of instances visible to the Onlooker.

2. **Operational vs. reporting filters**: End users of the TCP database may wish to add their own SQL filters on top of the generated views (e.g., `WHERE first_name LIKE 'A%'`). This is distinct from the Perspectives model-level `FilterF` step and is entirely appropriate at the reporting layer.

3. **Compound calculated roles**: If a calculated role's definition contains nested `FilterF` nodes (e.g., `filter (filter R with P1) with P2`), all `FilterF` nodes are dropped. Only the leaf enumerated role is used as the SQL source.

4. **Modeller awareness**: The PDR's SQL view compiler should log or annotate, in the generated view metadata, which `FilterF` steps were dropped. This makes the transformation transparent and aids debugging.

---

## 5. Composition: Building SQL Queries and Views from Query Paths

### 5.1 The translation model

A PDR query is represented as a `QueryFunctionDescription` (QFD) tree. The tree structure corresponds directly to the SQL structure:

| QFD constructor | SQL translation |
|---|---|
| `SQD domain qf range` | Leaf query — a table scan / column lookup / literal |
| `UQD domain qf subQuery range` | Unary wrapper — typically a scalar test applied to a subquery |
| `BQD domain ComposeF left right range` | JOIN / nested subquery: right is applied to each result of left |
| `BQD domain UnionF left right range` | `UNION ALL` of left and right |
| `BQD domain IntersectionF left right range` | `INTERSECT` of left and right |
| `BQD domain OrElseF left right range` | CTE fallback (see OrElseF above) |
| `BQD domain op left right range` | Binary operation: comparison or arithmetic |
| `MQD ...` | External function call — excluded from SQL translation |

### 5.2 Example: Calculated role

A simple calculated role that retrieves all Participant roles in a Meeting context:

```arc
-- ARC model
role AllParticipants = context >> Role:Participant
```

**QFD**:
```
SQD (RolGetter Participant)    -- domain: CDOM Meeting, range: RDOM Participant
```

**Generated SQL view**:
```sql
CREATE VIEW v_meeting_all_participants AS
SELECT p.*
FROM participant p
-- No join needed; input context is implicit (the consumer provides context_id)
```

A more complete example — a calculated role that navigates via a filler:

```arc
role ParticipantProfiles =
  context >> Role:Participant >> binding
```

**QFD** (schematically):
```
BQD ComposeF
  (BQD ComposeF
    (SQD (RolGetter Participant))
    (SQD (DataTypeGetter DirectFillerF)))
```

**Generated SQL view**:
```sql
CREATE VIEW v_meeting_participant_profiles AS
SELECT pr.*
FROM participant p
JOIN profile pr ON pr.id = p.filler_id
```

### 5.3 Example: Calculated property

```arc
property FullName =             -- Calculated property on Participant
  binding >> Property:Profile$Name
```

**QFD** (schematically):
```
BQD ComposeF
  (SQD (DataTypeGetter DirectFillerF))
  (SQD (PropertyGetter Profile$Name))
```

**Generated SQL view**:
```sql
CREATE VIEW v_participant_full_name AS
SELECT p.id AS participant_id,
       pr.name AS full_name
FROM participant p
LEFT JOIN profile pr ON pr.id = p.filler_id
```

### 5.4 Cross-context navigation

When a query navigates from one context to another via a role binding:

```arc
role RelatedMeetings =
  context >> Role:Participant >> binding >> context >> Role:Meeting
```

**Generated SQL view** (simplified):
```sql
CREATE VIEW v_related_meetings AS
SELECT m.*
FROM participant p
JOIN profile pr     ON pr.id = p.filler_id
JOIN meeting m      ON m.id = pr.context_id  -- navigating "up" to Profile's context
```

This works because every table has `context_id`, enabling upward (role → context) navigation.

---

## 6. Proposed Architecture for Automatic SQL Generation

### 6.1 Division of responsibilities

| Component | Responsibility |
|---|---|
| **PDR** | Has the full model; walks QFD trees; generates SQL view definitions |
| **TCP** | Has the relational DB; receives SQL view definitions; creates and maintains views |

The PDR has all the information needed to translate a `QueryFunctionDescription` into SQL: domain, range, and the specific QueryFunction at each step. The TCP does not need to understand PL queries at all.

### 6.2 Metadata format

**Design decision**: The PDR should emit a **portable query plan** (an intermediate representation closer to relational algebra), not raw SQL text. Raw SQL would couple the PDR to a specific SQL dialect, whereas a query plan allows the TCP to generate database-specific SQL via Knex.

The view configuration is a JSON document containing one entry per SQL view to be created. Each view entry contains:
- A `name` (the SQL view name, derived from a Readable name — see [Section 6.5](#65-stable-identifiers-and-readable-names))
- A `steps` array of relational-algebra operations

**Proposed intermediate representation** (sketch):
```json
{
  "views": [
    {
      "name": "v_approver_profiles",
      "steps": [
        { "op": "scan", "table": "approver", "alias": "a" },
        { "op": "join", "table": "profile", "alias": "pr",
          "on": "pr.id = a.filler_id", "type": "LEFT" },
        { "op": "project", "columns": ["a.id", "pr.name"] }
      ]
    }
  ]
}
```

The TCP's SQL generator would use Knex's query builder to convert these steps into database-portable SQL.

### 6.3 Trigger for view generation

View definitions should be (re-)generated when a new DomeinFile (compiled model) is loaded into the PDR. The TCP starts up and applies the view configurations to the database schema.

**Design decision (Q8)**: Maintaining the TCP and deciding when to regenerate SQL view configurations in response to model updates is an **organisational responsibility**, outside the Perspectives system itself. A model update is a deliberate organisational act. Applying the new SQL configuration to the TCP's database schema — including any required schema migrations for changed role types or properties — is the responsibility of the organisation deploying the TCP. The system provides the tooling (PDR generates the query plan; TCP applies it), but the trigger and governance are external.

### 6.4 Scope: which perspectives to translate

Only perspectives on roles that are **ultimately filled by `sys:Onlookers`** (or a subtype) should generate SQL views. The PDR can determine this at model compilation time by examining the filler chains of all user roles.

The PDR therefore:
1. Collects all Perspectives user roles that are (transitively) filled by `sys:Onlookers`.
2. For each such user role, collects all perspective objects (calculated roles and roles with calculated properties).
3. Expands any `RolGetter (CR calculatedRoleType)` references recursively into their leaf enumerated definitions (see [Q7](#q7-calculated-role-expansion-resolved)).
4. Drops all `FilterF` nodes from the expanded QFDs (see [Section 4.6](#46-design-decision-omitting-filter-steps-from-calculated-roles)).
5. Translates each resulting QFD into a relational-algebra query plan step sequence (using the rules in Section 4).
6. Emits the view configurations (including the Stable→Readable name map — see [Section 6.5](#65-stable-identifiers-and-readable-names)) to the TCP.

### 6.5 Stable Identifiers and Readable Names

The PDR evaluates and translates models that are internally written in terms of **Stable identifiers** — machine-generated CUIDs that uniquely identify context types, role types, and property types across model versions. These are the identifiers present in the compiled `DomeinFile` and in the `QueryFunctionDescription` trees that the SQL translator processes.

However, for the TCP's relational database, table names and column names must be **Readable** — derived from the names as they appear in the ARC model source (e.g., `Aanwezigen`, `FirstName`), not from the internal CUIDs.

**Design decision**: The query plan emitted by the PDR must include a **Stable→Readable name map**: a dictionary that maps each Stable identifier occurring in the plan to its Readable (source-level) name. The TCP uses this map when creating tables, columns, and views.

Notes:
- These are **source-level names**, not translated (internationalised) names. Translation is a presentation concern handled at the GUI layer, not in the reporting database.
- The same Stable identifier always maps to the same Readable name for a given model version, so the map is stable within a model version.
- When a model is updated and a new query plan is emitted, the TCP must re-apply any name changes (which in practice means a schema migration if a type is renamed in the model).

Example sketch of the name map in the query plan:
```json
{
  "nameMap": {
    "cuidAbc123": "Aanwezigen",
    "cuidDef456": "FirstName",
    "cuidGhi789": "Bijeenkomst"
  },
  "views": [ ... ]
}
```

### 6.6 `GenerateTCPConfiguration` — Callable from a Model

The SQL view configuration generation can be triggered from within a Perspectives model, analogously to how translation YAML files are generated. The function `GenerateTCPConfiguration` is an external core function (in module `Perspectives.Extern.Parsing`, implementation in `Perspectives.TCP.Configuration`) that:

1. Accepts a **versioned model URI** as its argument (e.g. `model://perspectives.domains#MyApp@1.2`).
2. Retrieves the corresponding `DomeinFile` from the repository using `modelUri2ModelUrl`.
3. Scans the model for user roles with `kindOfRole == UserRole` that are not `PerspectivesUsers` or `NonPerspectivesUsers` (these are Onlooker roles).
4. For each Onlooker role, collects its perspectives, expands CalculatedRole definitions (dropping `FilterF` nodes per §4.6), and resolves the leaf EnumeratedRoleTypes.
5. For each leaf ENR, emits one SQL table config entry (with all enumerated properties of that role as columns).
6. Adds the single universal `context` table (flagged with `isUniversalContextTable: true`).
7. Returns the resulting TCP configuration as a **JSON string**.

#### JSON output format

```json
{
  "modelUri": "model://perspectives.domains#MyApp@1.2",
  "nameMap": {
    "<stable-role-id>": "<readable-role-qualified-name>",
    "<stable-prop-id>": "<readable-prop-qualified-name>"
  },
  "tables": [
    {
      "name": "context",
      "isUniversalContextTable": true,
      "columns": []
    },
    {
      "name": "<local-role-name>",
      "roleType": "<stable-role-id>",
      "columns": [
        {
          "name": "<local-prop-name>",
          "type": "text|boolean|real|datetime",
          "nullable": true,
          "propertyType": "<stable-prop-id>"
        }
      ]
    }
  ]
}
```

The `tables` array is a drop-in replacement for `TCPConfig.schema.tables`. The operator must merge it with the RabbitMQ broker and database connection settings before starting the TCP process.

The returned string can then be used to create a file (e.g. `TCPConfig`) within a management model, in exactly the same way as the translation YAML is created in `model://perspectives.domains#CouchdbManagement`. Example ARC usage:

```arc
thing TCPConfig
  property FileName = context >> extern >> (ModelURIReadable + "@" + External$Version) + "-tcp-config.json"
    readableName
  property ConfigJson (File)
    pattern = "application/json" "Only .json files for TCP configurations are allowed."

  state GenerateConfig = GenerateConfig and context >> extern >> ArcFeedback matches regexp "^OK"
    on entry
      do for Author
        letA
            config <- callExternal p:GenerateTCPConfiguration( context >> extern >> VersionedModelURI ) returns String
          in
            create file FileName as "application/json" in ConfigJson
              config
```

The function signature in PureScript (in `Perspectives.Extern.Parsing`) is:

```purescript
generateTCPConfiguration :: Array ModelUriString -> (RoleInstance ~~> Value)
```

It is registered in `externalFunctions` as:
```purescript
mkLibFunc1 "model://perspectives.domains#Parsing$GenerateTCPConfiguration" True generateTCPConfiguration
```

The implementation lives in `Perspectives.TCP.Configuration` (module `tcpConfiguration.purs`).

### 6.7 Filler-Chain Properties and SQL Views

Some properties named in an Onlooker perspective are not stored directly on the ENR's table row but reside on a role type reachable by following N `filler_id` FK hops.  A property is identified as a filler-chain property when it appears in the perspective's property set but is **not locally accessible** on the role or its aspects (i.e., it is not returned by `allLocallyOnRoleRepresentedProperties`).

#### Example

`Meekijker` has a perspective on `Geregistreerden` (resolved to the ENR `Aanwezigen`) with properties `FirstName`, `LastName`, and `Aanwezig`.

- `Aanwezig` is declared directly on `Aanwezigen` → direct column.
- `FirstName` and `LastName` are declared on `sys:Identifiable`, an aspect of `PerspectivesUsers` / `NonPerspectivesUsers`.  They are reached via the filler chain: `Aanwezigen ← Deelnemer ← sys:SocialEnvironment$Persons ← PerspectivesUsers`.

The PDR computes this chain at configuration-generation time (purely from the model's `filledBy` restrictions) and records it as `fillerChain` in the column config:

```json
{
  "name": "FirstName",
  "type": "text",
  "nullable": true,
  "propertyType": "<stable-id-of-FirstName>",
  "fillerChain": [
    "<stable-id-of-Deelnemer>",
    "<stable-id-of-Persons>",
    "<stable-id-of-PerspectivesUsers>"
  ]
}
```

The `nameMap` in the PDR output includes readable names for all role types in every filler chain so that TCP can construct the JOIN table names.

#### SQL view generated by TCP

For each role table that has at least one filler-chain column, `SchemaGenerator.applyViews` creates a `CREATE OR REPLACE VIEW "<tableName>_view"` that LEFT JOINs through the hop tables:

```sql
CREATE OR REPLACE VIEW "Aanwezigen_view" AS
SELECT "base"."id", "base"."context_id", "base"."filler_id", "base"."Aanwezig",
       "hop_0_2"."FirstName", "hop_0_2"."LastName"
FROM "Aanwezigen" base
LEFT JOIN "Deelnemer" hop_0_0 ON "base"."filler_id" = hop_0_0."id"
LEFT JOIN "Persons"   hop_0_1 ON "hop_0_0"."filler_id" = hop_0_1."id"
LEFT JOIN "PerspectivesUsers" hop_0_2 ON "hop_0_1"."filler_id" = hop_0_2."id"
```

Reporting tools should query `Aanwezigen_view` rather than the base `Aanwezigen` table when they need filler-chain properties.

#### Union branches

When the binding ADT at a hop is a SUM (union), the PDR picks the **first leaf** of the union. The §Q1 design decision guarantees that the hop count to any named property is the same across all branches of a union, so any single branch correctly represents the depth.

#### Multiple filler-chain columns with different chains

If two properties in the same perspective require different filler chains, the view generator emits a separate JOIN sequence for each unique chain (identified by the chain key = joined stable IDs).  Columns that share the same chain reuse the same JOIN aliases.

---

All open questions have now been resolved. Decisions are summarised below and detailed in the relevant sections of the document.

### ✅ Q1: Filler chain depth *(resolved)*
**Decision**: The maximum supported filler chain depth is **5 hops**, extendable to 6 if needed. The fill relation in Perspectives is never truly self-referential (a role of type R cannot be filled by another instance of R; a role instance cannot fill itself), so chains are always acyclic and finite. The PDR compiler will unroll chains up to the configured maximum and emit a warning if a chain exceeds it. See also [Section 4.5](#45-specialised-fillers-problem-runtime-fix-and-design-decision) for the specialised-filler scenario and why properties can always be found at a fixed, compile-time-known depth.

### ✅ Q2: External role storage and context table strategy *(resolved)*
**Decision (a) — Context table structure**: A **single universal `context` table** with columns `id` (primary key) and `context_type` (the Perspectives context type URI) is used. All role tables' `context_id` columns reference this single table. Per-type context tables are not used.

**Decision (b) — External role storage**: External roles are represented as **role tables**, in exactly the same way as other enumerated roles. External role properties are stored in columns of the external role table; they are not inlined into the context table. An external role instance's `context_id` references the universal `context` table, like any other role.

See [Section 2](#2-assumed-relational-table-structure) for the schema details.

### ✅ Q3: `MeF` in Onlooker queries *(resolved)*
**Decision**: `MeF` is **prohibited** in Onlooker-perspective queries. The PDR model compiler must detect and report this to the modeller. See the [MeF section](#datatypegetter-mef) for details.

### ✅ Q4: Indexed names *(resolved)*
**Decision**: `IndexedContextName` and `IndexedRoleName` are **prohibited in Onlooker queries**. An Onlooker is an impersonal role; indexed names have per-peer semantics (like "home") and have no meaningful referent for an impersonal observer. The PDR model compiler must detect and report their presence (see [Section 4.4 analysis](#datatypegetter-indexedcontextname-datatypegetter-indexedrolename) and [Q9](#-q9-handling-unsupported-query-steps-resolved)).

### ✅ Q5: State as a SQL column *(resolved)*
**Decision**: `IsInStateF` is **prohibited in Onlooker queries**. The PDR never emits state information between installations; state is always re-evaluated locally. The TCP therefore never receives state values in the delta stream and cannot reproduce state evaluation from its data alone. The PDR model compiler must detect and report `IsInStateF` in Onlooker-perspective queries (see [Section 4.4 analysis](#datatypegetter-isinstateF-or-unarycombinator-isinstateF) and [Q9](#-q9-handling-unsupported-query-steps-resolved)).

### ✅ Q6: Intermediate representation format *(resolved)*
**Decision**: The PDR should emit a **portable query plan** (a relational-algebra JSON format), not raw SQL text. This keeps the PDR decoupled from specific SQL dialects and allows the TCP to use Knex's query builder to generate database-appropriate SQL. See [Section 6.2](#62-metadata-format) for the proposed format.

### ✅ Q7: Calculated role expansion *(resolved)*
**Decision**: Calculated role definitions must be **expanded in the PDR** before the query plan is emitted to the TCP. The PDR performs all `RolGetter (CR ...)` inline expansion (recursively, until only enumerated roles remain) and also drops all `FilterF` nodes (see [Section 4.6](#46-design-decision-omitting-filter-steps-from-calculated-roles)). The TCP receives only a leaf-level relational-algebra plan; it never needs to understand calculated roles or perform expansion itself.

### ✅ Q8: View maintenance and model updates *(resolved)*
**Decision**: Maintaining the TCP and deciding when to apply new SQL view configurations is an **organisational responsibility**, outside the Perspectives system. A model update is a deliberate organisational act; regenerating and applying the new query plan to the TCP database schema — including any required migrations — is the responsibility of the organisation deploying the TCP. The Perspectives system provides the tooling (PDR generates the plan; TCP applies it) but does not automate the governance. See [Section 6.3](#63-trigger-for-view-generation) for details.

### ✅ Q9: Handling unsupported query steps *(resolved)*
**Decision**: When the PDR's SQL view compiler encounters a query step that cannot be translated (i.e., a step that is neither Category 1 nor safely ignorable like `FilterF`), it must:

1. **Abandon the view under consideration** — do not emit a partial or incorrect SQL view for the perspective that contains the unsupported step.
2. **Emit a compile-time warning** — report the problematic query step to the modeller with a clear message identifying the affected perspective and the unsupported function.
3. **Continue with remaining views** — process all other perspectives and emit as many valid view definitions as possible.

The result is a **partial query plan**: correct and complete for all translatable perspectives, and absent (with warnings) for those containing unsupported steps. This is preferable to either failing entirely or silently producing incorrect views.

Steps that are unconditionally prohibited in Onlooker queries (and therefore always trigger a warning + view abandonment):
- `DataTypeGetter MeF` (see [Section 4.3](#datatypegetter-mef))
- `DataTypeGetter IndexedContextName`, `DataTypeGetter IndexedRoleName` (see [Section 4.4 analysis](#datatypegetter-indexedcontextname-datatypegetter-indexedrolename))
- `DataTypeGetter IsInStateF` (see [Section 4.4 analysis](#datatypegetter-isinstateF-or-unarycombinator-isinstateF))

Steps that can be safely ignored (dropped without warning):
- `FilterF` — pre-filtered by PDR peers; see [Section 4.6](#46-design-decision-omitting-filter-steps-from-calculated-roles)

---

## Summary Table

| QueryFunction | Category | SQL pattern | Notes |
|---|---|---|---|
| `DataTypeGetter ContextF` | 1 | `JOIN context ON context.id = role.context_id` | |
| `DataTypeGetter ExternalRoleF` | 1 | `SELECT * FROM <ctx>_external WHERE context_id = ?` | External role stored in its own table (see §2 and Q2) |
| `DataTypeGetter DirectFillerF` | 1 | `JOIN filler ON filler.id = role.filler_id` | |
| `DataTypeGetter FillerF` | 3 | Recursive CTE or unrolled JOIN | Multi-table chains require unrolling |
| `DataTypeGetterWithParameter FillerF ctx` | 3 | Unrolled JOIN with context type filter | Same as FillerF |
| `DataTypeGetter IdentityF` | 1 | No-op | |
| `DataTypeGetter ModelNameF` | 3 | String literal constant | Known at generation time |
| `DataTypeGetter IndexedContextName` | 2 | — | Prohibited in Onlooker queries (per-peer semantics; see §4.4, Q4) |
| `DataTypeGetter IndexedRoleName` | 2 | — | Prohibited in Onlooker queries (per-peer semantics; see §4.4, Q4) |
| `DataTypeGetter MeF` | 2 | — | No SQL equivalent; session-dependent |
| `UnaryCombinator CountF` | 1 | `COUNT(*)` | Used as sequence function in ComposeSequenceF |
| `UnaryCombinator MinimumF` | 1 | `MIN()` | Used as sequence function in ComposeSequenceF |
| `UnaryCombinator MaximumF` | 1 | `MAX()` | Used as sequence function in ComposeSequenceF |
| `UnaryCombinator AvailableF` | 1 | `EXISTS(SELECT 1 FROM ... WHERE id = ?)` | |
| `UnaryCombinator FirstF` | 3 | `LIMIT 1` | Non-deterministic without ORDER BY |
| `BinaryCombinator ComposeSequenceF` | 1 | `SELECT AGG(...) FROM (left_query)` | Right side is an aggregation (CountF, MinimumF, MaximumF, AddF, FirstF) |
| `BinaryCombinator SequenceF` | 3 | Sequential CTEs | Mainly for letE-like binding in compound expressions |
| `RolGetter (ENR roleType)` | 1 | `SELECT * FROM <role_table> WHERE context_id = ?` | |
| `RolGetter (CR calculatedRoleType)` | 2* | Expand to definition QFD | *Becomes Category 1 after expansion |
| `PropertyGetter (ENP propertyType)` | 1 | Column projection | |
| `PropertyGetter (CP calculatedPropertyType)` | 2* | Expand to definition QFD | *Becomes Category 1 after expansion |
| `Value2Role propertyType` | 3 | `JOIN target ON target.id = src.<col>` | Target table from model metadata |
| `FilledF enumeratedRoleType contextType` | 1 | `SELECT * FROM <filled_table> WHERE filler_id = ?` | |
| `GetRoleInstancesForContextFromDatabaseF` | 1 | `SELECT * FROM <role_table> WHERE context_id = ?` | Same as RolGetter |
| `Constant range value` | 1 | `SELECT '<value>'` | |
| `RoleIndividual roleInstance` | 1 | `SELECT * FROM <table> WHERE id = '<id>'` | |
| `ContextIndividual contextInstance` | 1 | `SELECT * FROM <table> WHERE id = '<id>'` | |
| `BinaryCombinator ComposeF` | 1 | JOIN or nested subquery | Fundamental composition |
| `BinaryCombinator UnionF` | 1 | `UNION ALL` | |
| `BinaryCombinator IntersectionF` | 1 | `INTERSECT` | |
| `BinaryCombinator OrElseF` | 3 | CTE with fallback | Verbose but standard SQL |
| `BinaryCombinator AndF` | 1 | `AND` | |
| `BinaryCombinator OrF` | 1 | `OR` | |
| `BinaryCombinator FilledByF` | 1/3 | Single-hop: `filler_id IN (...)` / Full-chain: recursive CTE | |
| `BinaryCombinator FillsF` | 1/3 | Single-hop: `EXISTS(... WHERE filler_id = ?)` / Full-chain: recursive CTE | |
| `UnaryCombinator ExistsF` | 1 | `EXISTS(...)` | |
| `UnaryCombinator NotF` | 1 | `NOT(...)` | |
| `UnaryCombinator FilledByF` | 1/3 | Same as Binary FilledByF | |
| `UnaryCombinator FillsF` | 1/3 | Same as Binary FillsF | |
| `BinaryCombinator EqualsF`, `BinaryCombinator NotEqualsF`, `BinaryCombinator LessThanF`, `BinaryCombinator LessThanEqualF`, `BinaryCombinator GreaterThanF`, `BinaryCombinator GreaterThanEqualF` | 1 | `=`, `!=`, `<`, `<=`, `>`, `>=` | |
| `BinaryCombinator AddF`, `BinaryCombinator SubtractF`, `BinaryCombinator DivideF`, `BinaryCombinator MultiplyF` | 1 | `+`, `-`, `/`, `*` | Binary arithmetic |
| `UnaryCombinator AddF` (sum) | 1 | `SUM()` | Used as sequence function in ComposeSequenceF |
| `RegExMatch` | 3 | `~` / `REGEXP` / `LIKE` | DB-specific syntax |
| `UnaryCombinator IsInStateF` | 2 | — | Prohibited in Onlooker queries; state is never emitted between peers (see §4.4, Q5) |
| `BindVariable`, `WithFrame`, `VariableLookup` | 3 | CTE (`WITH x AS (...)`) | Sequential bindings must reference earlier CTEs |
| External functions (`ExternalCoreRoleGetter`, etc.) | excluded | — | callExternal out of scope |
| Type getters (`TypeOfContextF`, etc.) | excluded | — | Type metadata out of scope |
| `FilterF` | excluded (design decision) | — | Dropped when expanding Calculated Roles; pre-filtered by PDR peers. See [§4.6](#46-design-decision-omitting-filter-steps-from-calculated-roles) |
| Assignment operators | excluded | — | Not query steps |
