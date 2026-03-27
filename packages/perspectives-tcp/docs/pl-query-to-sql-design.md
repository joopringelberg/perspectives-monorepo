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
5. [Composition: Building SQL Queries and Views from Query Paths](#5-composition-building-sql-queries-and-views-from-query-paths)
6. [Proposed Architecture for Automatic SQL Generation](#6-proposed-architecture-for-automatic-sql-generation)
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
| `FilterF` | Filtering is applied by PDR peers before a mutation is forwarded to the TCP; the TCP receives only pre-filtered data |
| Assignment operators: `CreateContext`, `CreateRole`, `Bind`, `Unbind`, `DeleteRole`, `DeleteContext`, `DeleteProperty`, `Move`, `RemoveRole`, `RemoveContext`, `AddPropertyValue`, `RemovePropertyValue`, `SetPropertyValue`, `CreateFileF`, `ExternalEffectFullFunction`, `ExternalDestructiveFunction`, `ForeignEffectFullFunction`, `BindResultFromCreatingAssignment` | These are action-execution operators, not query operators |
| `TranslateContextType`, `TranslateRoleType` | Translation of type identifiers to human-readable strings; purely presentational |
| `TypeTimeOnlyContextF`, `TypeTimeOnlyEnumeratedRoleF`, `TypeTimeOnlyCalculatedRoleF` | Compile-time-only type references; produce no runtime values |

---

## 2. Assumed Relational Table Structure

The TCP uses the **Smashed Role Chains** schema strategy:

### Context table (one per context type)

```sql
CREATE TABLE <context_type_name> (
  id         TEXT PRIMARY KEY
  -- No other standard columns; context-level properties (if any) are written
  -- via the external role (see below).
);
```

### Role table (one per role type, including external roles)

```sql
CREATE TABLE <role_type_name> (
  id         TEXT PRIMARY KEY,
  context_id TEXT REFERENCES <owning_context_table>(id) ON DELETE CASCADE,
  filler_id  TEXT REFERENCES <filler_role_table>(id)    ON DELETE SET NULL,
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

The external role type is always known: it is `<contextType>$External`. In the Smashed Role Chains schema the external role has its own table (or its properties are denormalised onto the context table). The TCP already handles this in `handleRolePropertyDelta` where external role property writes use the context table.

> **Note**: If external role properties are written to the context table (as the current TCP implementation does), then `ExternalRoleF` followed by a property step should reach into the context table, not a separate external role table. The SQL translator must know whether the external role is stored separately or inlined into the context table.

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

**Why not in SQL**: SQL queries are executed against a database without a concept of "currently authenticated Perspectives user". The `MeF` function is intrinsically tied to the PDR's session state. In the TCP context, the observer is a configured Onlooker instance; there is no equivalent of a dynamic "me" pointer.

**Possible workaround**: If a perspective query uses `me` to self-reference (e.g., "the Onlooker's own profile"), the specific Onlooker instance ID could be injected as a parameter at SQL generation time by the PDR. This requires the PDR to resolve `MeF` at view-generation time and replace it with a constant (`RoleIndividual`). Whether this is semantically correct depends on whether "me" is stable for a given TCP instance.

> **Open question**: In what Onlooker queries does `MeF` actually appear? If the Onlooker only has perspectives on other users' data (and not on itself), `MeF` may never appear in practice.

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

**SQL for a single-table filler chain**:
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

**When the chain crosses tables**: The PDR model always knows the complete filler chain type statically. If the chain is bounded and known, it can be **unrolled** into a fixed-depth join:

```sql
-- Filler chain: Participant → Profile → NationalProfile (depth 2)
SELECT np.*
FROM participant_role p
JOIN profile_role pr      ON pr.id = p.filler_id
JOIN nationalprofile_role np ON np.id = pr.filler_id
WHERE p.id = :participant_id
```

**Recommendation**: The PDR compiler, having access to the model, can determine the filler chain depth and cross-table path. It should unroll the chain into a fixed-depth JOIN sequence. If the chain depth is variable or unbounded (self-referential role types), this approach breaks down.

> **Open question**: Is there a practical upper bound on filler chain depth in real Perspectives models? In the examples seen so far, chains are typically 1–3 hops. A configurable maximum depth (e.g., 5) with a warning when exceeded would be pragmatic.

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
**PDR semantics**: Look up the specific instance associated with a symbolic indexed name (e.g., "the world context for user X").

**Why not Category 1**: Indexed name resolution is managed by the PDR and is stored in PouchDB, not in the TCP's relational database. A TCP does not have direct access to the indexed name mapping.

**Possible approaches**:
1. **Pre-resolve at generation time**: The PDR resolves the indexed name to a specific instance ID and emits a `ContextIndividual` / `RoleIndividual` constant in the generated SQL. This only works if the indexed name maps to a stable, fixed instance.
2. **Add an indexed-names lookup table to the TCP**: Maintain an additional table `indexed_names(name TEXT PRIMARY KEY, instance_id TEXT)` in the TCP database, populated when the PDR creates indexed contexts/roles. Then:
   ```sql
   SELECT instance_id FROM indexed_names WHERE name = 'MyIndexedName'
   ```
3. **Exclude from TCP queries**: If Onlooker perspectives never use indexed names in their query paths (which is likely for data-access perspectives), this function can simply be excluded from TCP SQL generation.

> **Open question**: Do Onlooker-accessible calculated roles/properties in practice use `IndexedContextName` or `IndexedRoleName`? These are primarily used for "system singletons" (e.g., `usr:MySystem`). If they appear only in state conditions (which are pre-evaluated by the PDR), they may never reach a TCP query.

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

**Limitation**: State is a derived concept. Whether an entity is in a state depends on the evaluation of the state's entry condition (a PL query itself), which may reference data inside or outside the TCP's schema.

**Possible approaches**:
1. **Materialise state as a column**: Add a boolean column to each role/context table for each relevant state. The PDR must send a delta when an entity's state changes (this is not a native delta type today but could be introduced).
2. **Derive state in SQL**: If the state's entry condition is itself translatable to SQL, the SQL generator can inline the condition expression. This is the ideal approach but is limited to states whose conditions are expressible as SQL.
3. **Exclude from TCP queries**: Accept that state-conditional expressions cannot be evaluated in the TCP SQL layer. This is the pragmatic fallback for the initial implementation.

> **Open question**: Which Onlooker perspectives are expected to use `IsInStateF`? This step is most relevant in **state conditions** for actions, not typically in perspective object queries. If it does appear in an Onlooker's calculated role/property definition, the SQL translator should flag it as unsupported and skip or approximate the view.

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

### 6.2 Metadata format (proposed)

The PDR should emit a **view configuration** document, separate from the runtime delta stream. This document is a configuration artefact, not a transaction. One natural format (consistent with the existing TCP config style) is JSON:

```json
{
  "views": [
    {
      "name": "v_approver_profiles",
      "description": "Profiles of all Approvers in each Meeting context",
      "sql": "SELECT p.id AS participant_id, pr.name AS full_name FROM approver a JOIN profile pr ON pr.id = a.filler_id"
    }
  ]
}
```

Alternatively, the PDR could emit a higher-level **query plan** (an intermediate representation closer to relational algebra), from which the TCP generates the SQL using its own Knex-based code. This provides better portability across database backends.

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

View definitions should be (re-)generated when:

1. A new DomeinFile (compiled model) is loaded into the PDR.
2. The TCP starts up and detects that its local view configuration is stale.

The PDR exposes an endpoint (or writes to a file) with the current view configurations. The TCP applies them to the database schema.

### 6.4 Scope: which perspectives to translate

Only perspectives on roles that are **ultimately filled by `sys:Onlookers`** (or a subtype) should generate SQL views. The PDR can determine this at model compilation time by examining the filler chains of all user roles.

The PDR therefore:
1. Collects all Perspectives user roles that are (transitively) filled by `sys:Onlookers`.
2. For each such user role, collects all perspective objects (calculated roles and roles with calculated properties).
3. Translates each relevant QFD into a SQL view definition (using the rules in Section 4).
4. Emits the view configurations to the TCP.

---

## 7. Open Questions

The following questions remain open and should be resolved through further iteration:

### Q1: Filler chain depth
Is there a practical upper bound on filler chain depth in real Perspectives models? A maximum depth of 3–5 hops would allow the SQL translator to unroll filler chains without recursive CTEs. If filler chains are occasionally deeper or unbounded (self-referential roles), a separate strategy (e.g., materialized path or nested set) would be needed.

### Q2: External role storage
Should external roles always have their own dedicated table, or should their properties be denormalised onto the context table? The current TCP implementation writes external role properties to the context table. The SQL translator must know which convention is used (possibly per-table in the config).

### Q3: `MeF` in Onlooker queries
Does `MeF` (the "current user") ever appear in queries for Onlooker perspectives? If yes, how should it be handled: as a runtime parameter, a constant baked into the view at generation time, or an unsupported case?

### Q4: Indexed names
Are indexed context/role names used in Onlooker perspective queries? If so, which of the three strategies (pre-resolution, lookup table, or exclusion — see [Category 3 analysis](#datatypegetter-indexedcontextname-datatypegetter-indexedrolename)) is most appropriate?

### Q5: State as a SQL column
Should state be materialised as a column in the TCP tables? This would require:
(a) The PDR to compute and emit state transitions as deltas (a new delta type).
(b) The TCP to write these state values to a `state_<name>` boolean column.
Is this feasible and desirable?

### Q6: Intermediate representation format
Should the PDR emit raw SQL text or a Knex-portable query plan? Raw SQL is simple to implement but ties the TCP to specific SQL dialect choices made by the PDR. A query plan (e.g., a small relational-algebra JSON format) gives the TCP more control over database-specific SQL generation.

### Q7: Calculated role expansion
When a PDR query refers to a `RolGetter (CR calculatedRoleType)`, the SQL translator must inline the calculated role's definition. At what point in the process should this expansion happen: during PDR compilation (before emitting the SQL config) or during TCP view generation? Pre-expansion by the PDR is cleaner (the TCP receives only leaf-level SQL operations) but may produce larger view definitions for deeply nested calculated roles.

### Q8: View maintenance and model updates
When a model is updated (e.g., a new property is added to a role type), views that reference that role's table may need to be recreated. What is the protocol for notifying the TCP that its views are stale? Should view names be versioned?

### Q9: Handling unsupported query steps
When the SQL translator encounters a query step that cannot be translated (Category 2, or a Category 3 step beyond current scope), should it:
(a) Skip the view entirely and log a warning?
(b) Generate a partial view covering the translatable parts?
(c) Return an error to the model author at compile time?
The answer has implications for which ARC modelling patterns are "Onlooker-compatible" (i.e., fully translatable to SQL).

---

## Summary Table

| QueryFunction | Category | SQL pattern | Notes |
|---|---|---|---|
| `DataTypeGetter ContextF` | 1 | `JOIN context ON context.id = role.context_id` | |
| `DataTypeGetter ExternalRoleF` | 1 | `SELECT * FROM <ctx>_ext WHERE context_id = ?` | Depends on external role storage convention |
| `DataTypeGetter DirectFillerF` | 1 | `JOIN filler ON filler.id = role.filler_id` | |
| `DataTypeGetter FillerF` | 3 | Recursive CTE or unrolled JOIN | Multi-table chains require unrolling |
| `DataTypeGetterWithParameter FillerF ctx` | 3 | Unrolled JOIN with context type filter | Same as FillerF |
| `DataTypeGetter IdentityF` | 1 | No-op | |
| `DataTypeGetter ModelNameF` | 3 | String literal constant | Known at generation time |
| `DataTypeGetter IndexedContextName` | 3 | Pre-resolve or lookup table | Depends on strategy (see Q4) |
| `DataTypeGetter IndexedRoleName` | 3 | Pre-resolve or lookup table | Depends on strategy (see Q4) |
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
| `UnaryCombinator IsInStateF` | 3 | Materialise state or inline condition | Complex; see Q5 |
| `BindVariable`, `WithFrame`, `VariableLookup` | 3 | CTE (`WITH x AS (...)`) | Sequential bindings must reference earlier CTEs |
| External functions (`ExternalCoreRoleGetter`, etc.) | excluded | — | callExternal out of scope |
| Type getters (`TypeOfContextF`, etc.) | excluded | — | Type metadata out of scope |
| `FilterF` | excluded | — | Pre-filtered by PDR peers |
| Assignment operators | excluded | — | Not query steps |
