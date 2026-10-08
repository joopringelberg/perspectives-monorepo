---
applyTo: "**/*.arc"
---

# ARC language reference for model work

Use this reference whenever creating, reviewing, explaining, or debugging a Perspectives model. Treat ARC as an executable, typed domain-specific language, not as prose or ordinary SQL/RBAC configuration. Check the parser and runtime documentation cited at the end when a question depends on implementation details or newer syntax. Do not infer a language rule from syntax coloring alone.

## 1. Model structure and names

An ARC source file declares one model context:

```arc
domain model://example.org#ModelName
  use sys for model://perspectives.domains#System
  use this for model://example.org#ModelName
  case Example
    user Manager = sys:Me
```

Indentation delimits nested declarations and blocks. Preserve the parent/child indentation when adding a declaration. `domain` identifies the model; `case`, `party`, and `activity` introduce other context types. `use prefix for model://...` imports a model, and a prefix qualifies names from that model (`sys:Me`). A fully qualified type name has the form `model://namespace#Model$Context$Role`; local names are resolved in their containing context. Do not confuse a type name with an instance.

Contexts contain role types. At runtime a context instance contains role instances; a role can be bound to a filler role instance. Bindings are navigable in both directions. Context and role types form the network that ARC queries traverse. There is no empty context.

## 2. Contexts, roles, properties, and aspects

Common role declarations:

```arc
context Meetings (relational) filledBy Meeting
user Organizer filledBy sys:TheWorld$PerspectivesUsers
thing AgendaItem (relational)
context CurrentMeeting = filter Meetings with Name == "Planning"
```

`user`, `thing`, and `context` declare role types within a context; `external` declares that context's special `External` role. `public` declares a calculated role backed by a public resource. A role without `=` is enumerated/stored. A role with `= <query>` is calculated: its instances are query results, not separately created role instances. Calculated roles are relational by default; `(functional)` constrains the result to at most one, and `(default)` marks a default user role. Enumerated roles are functional by default (at most one); `(relational)` permits multiple instances. `(mandatory)` expresses a required role.

An indexed context or role declares a name used to find/index its instances. An indexed name is not a unique identity guarantee unless the model/runtime operation enforces uniqueness. The top-level `use` declarations import models; an `aspect` composes types, while `filledBy` describes instance-level binding constraints.

`filledBy` constrains which role types may fill the declared role:

```arc
thing Participant filledBy (sys:TheWorld$PerspectivesUsers + sys:SocialEnvironment$Persons)
thing Vehicle filledBy Car, Bicycle
```

In a filler type, `+` means both component types are required (a product/conjunction); comma-separated alternatives mean either type may fill it (a sum/disjunction). Parenthesize a product when it occurs among alternatives. This is a type constraint, not an instruction to create or bind a filler.

Properties are declared under roles:

```arc
property Name (mandatory, String)
property Remarks (relational, String, authoronly)
property DisplayName = Name
```

Built-in ranges include `Boolean`, `Number`, `String`, `DateTime`, and `Email` (and other ranges supported by the current parser). Properties are functional by default; `(relational)` allows multiple values. `(mandatory)` requires a value. A calculated property uses `= <query>` and is read-only as a derived value. Property facets and supported ranges are parser-defined; inspect `arcParser.purs` rather than guessing a range or facet.

An `aspect` composes type definitions without creating a separate filler binding. A role aspect contributes the aspect role's type constraints/properties to the role; a context aspect contributes the aspect context's roles to the context. Aspects can themselves have aspects. Use `aspect <qualified-name>` on a role and `aspect <qualified-context-name>` in a context declaration. Role aspect property mappings can explicitly map an aspect property to a replacing property; verify the parser's `where ... is replaced by ...` syntax before writing a mapping. Use `filledBy` instead when a real, independently existing filler instance and navigable binding are intended. Aspects are not inheritance of instances and do not create copies of the aspect's instances.

## 3. Queries and expressions

Queries are typed, set-valued traversals of context/role instances and property values. They are used for calculated roles and properties, state conditions, perspectives, filters, and statements. A query can yield zero, one, or many results; use `exists` where a Boolean existence test is intended.

Typical navigation:

```arc
context >> Meetings >> binding
me >> binder Participants
binding >> context
extern >> binder Members
```

`>>` composes query steps left to right. Role identifiers select roles in the current context; `context` selects the containing context, `extern` its external role, `binding` follows a role's filler, and `binder` finds roles filled by the current role. `me` denotes the local user's role in the relevant context. Parenthesize nested expressions to make the query domain and traversal direction explicit.

Common operators include:

- `filter <set-query> with <predicate>` filters the left-hand result set.
- `exists <query>` and `not exists <query>` test non-emptiness and emptiness.
- `and`, `or`, `not`, comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`), and arithmetic operate on compatible values.
- `union`, `intersection`, and `orElse` combine query results with distinct semantics; do not substitute one for another.
- `filledBy`, `fills`, `binds`, `matches`, and type-reflection operations test bindings, values, or types.
- `letE ... in ...` introduces read-only expression bindings; `letA ... in ...` binds query results for an action's staged statements.

ARC is whitespace-sensitive at the block level. Query operator precedence and accepted literal/range syntax are parser-defined; use parentheses rather than assuming precedence from another language. A condition such as `not exists X` uses absence as information (the closed-world aspect of evaluation), so its outcome can depend on when a state is evaluated during a transaction.

## 4. Perspectives are the access model

A perspective relates a user role (the subject) to a role or query of roles (the object) and specifies what that user can see and do. Model every intended access path; a type declaration, binding, aspect, screen, or property does not itself grant permission. Treat perspectives as the model's authorization and visibility policy, similar to RBAC but with query-selected objects, state-dependent rules, and synchronization semantics.

Basic shape:

```arc
user Manager = sys:Me
  perspective on Meetings
    all roleverbs
    props (Name) verbs (Consult, SetPropertyValue)
```

`perspective on <query>` defines the current user role's permissions on the query's result. `perspective of <role>` expresses perspective rules from the specified role. Nested perspectives, `in state` blocks, and perspective `action` declarations refine access over traversed objects and states. Permissions may therefore vary with the current state and with the object reached by a query.

Role verbs govern operations on roles and bindings; property verbs govern operations on property values. `view`/`props` declarations specify property access, while `only`, `except`, and `all roleverbs` constrain role operations. Explicitly grant only the verbs needed. `Consult` is read access; `SetPropertyValue`, `AddPropertyValue`, `RemovePropertyValue`, and `DeleteProperty` concern property values. Exact role verbs and defaults are defined by the parser/runtime; do not invent verbs. A screen is presentation, not an authorization boundary.

### `selfonly` and `authoronly`

These modifiers have different purposes and can apply to a perspective or a property:

- `selfonly` on a self-perspective restricts that perspective to the same role instance (for a multirole user, not every instance of the role type). `selfonly` on a property restricts visibility of its values in the self-perspective. It is useful only when the user role and object/property role have the intended self relationship.
- `authoronly` on a perspective keeps role instances authored by one user private from other authors for synchronization purposes. `authoronly` on a property keeps each author's values private on a shared role instance.

Example property declaration: `property PrivateRemark (String, authoronly)`. These modifiers do not replace ordinary perspective permissions, and they are not interchangeable: use `selfonly` for the user's own role/value and `authoronly` for the author's private role/value. Consider all other perspectives on the same role before claiming data is private. In particular, apply the modifier to the correct object/property and check how the role is synchronized.

## 5. States and automatic actions

States are named, query-conditioned parts of a context or role type:

```arc
state Ready = exists Participants
  on entry
    do
      Status = "ready" for extern
  on exit
    do
      Status = "not ready" for extern
  state HasOrganizer = exists Organizer
    on entry
      do
        ...
```

A state becomes active when its condition is true and exits when it is false. Nested states are evaluated within their parent state's active scope; a nested state is not active if its parent is inactive. Contexts have an always-true root state; role declarations also have a root state. State conditions are queries, not imperative tests. `on entry` and `on exit` blocks run automatically on transitions; actions can create further changes, which can themselves change state conditions.

`do [for <user-role>]` declares an automatic action for a state transition. Without `for`, the current subject/user role is used when the state supplies one. An action can also be declared by `action <Name>` and can be exposed through a perspective. State and perspective scope determine which role/context instances and local names are available; verify scope before reusing an expression in another block.

## 6. Statements, side effects, and transaction ordering

Action bodies contain ordered statements, commonly:

```arc
create role AgendaItem in context
bind me to Organizer in context
Title = "Draft" for AgendaItem
remove filler of Organizer
remove role AgendaItem
```

Other parser-supported statements include:

- `create context <ContextType> [named <query>] [bound to <RoleType>] [in <context-query>]`
- `create role <RoleType> [named <query>] [in <context-query>]`
- `bind <filler-query> to <RoleType> [in <context-query>]` and `bind_ <role-query> to <binder-query>`
- `move <role-query> [to <context-query>]`
- `remove filler of <filled-role-query>`, `remove filler <filler-query> from <filled-role-query>`, and `remove role|context <query>`
- Property assignments such as `<Property> = <query> [for <role-query>]`, and `delete property <Property> [from <role-query>]`
- `callEffect`, `callDestructiveEffect`, `runContextAction`, and `runRoleAction` (see their dedicated sections)

These forms are a quick guide, not a substitute for the statement grammar; `create_ context` and the `remove as filler` forms have distinct signatures. Statement operands are queries and may select multiple targets; check cardinality and authorization rather than assuming a single result.

The source order is not the execution order for all operations. The runtime accumulates changes and runs constructive work/state evaluation through a transaction cascade; destructive assignments (for example unbinding/removing roles or contexts and destructive external effects) are deferred and performed in the destructive phase, after constructive processing, even when written earlier in the action. A mutation can make a state enter or exit; its automatic actions can make more mutations and trigger more states. The runtime repeats state/action processing until it reaches a stable pass. Design for convergence and avoid state/action cycles.

Important qualification: state exits and their `on exit` actions can occur during the cascade before the deferred physical removal. Do not assume an `on exit` action itself runs last. Conditions using `not exists` can also be sensitive to evaluation timing; inspect transaction behavior before relying on a transient intermediate absence.

### `once settled` (not `once delayed`)

`once settled` separates an action into stages:

```arc
letA
  item <- create role Item
in
  Name = "draft" for item

  once settled
    Status = "ready" for item
```

The first stage runs in the current transaction. A later stage is queued as a continuation and runs only after the current logical transaction, including changes and state/action cascades it triggers, has settled. Each continuation waits for the previous stage. The continuation runs as a new transaction; it is not merely a pause within the original transaction. Without `letA`, put `once settled` at the stage boundary and indent the following statements under it. In `do for <role> once settled`, the header delays that `do` stage; a further `once settled` in its body delays the next stage.

An embedded transaction/action invocation is distinct from a settled continuation. `runContextAction <action> for <role> in <context-query>` and `runRoleAction <action> for <role> on <role-query> in <context-query>` invoke another action and await its chain, including its settled stages, before the caller proceeds. They are useful for explicit, depth-first action composition; do not model them as fire-and-forget calls.

The runtime also embeds a sharing transaction for local automatic reactions while processing a non-sharing transaction received from a peer. The peer's deltas are not re-sent; changes made by this installation's own state/action reactions are distributed under the local user's identity. The embedded transaction settles before control returns to the outer transaction.

## 7. External functions and effects

Use the correct calling form:

```arc
Computed = callExternal util:SomeFunction( Input ) returns String
```

`callExternal <qualified-function>(arguments) returns <type>` is an expression query for an external function that returns values and has no Perspectives-data side effect. Its declared result type and argument types/count must match the registered function. It is appropriate in calculated values and conditions; it is not an action statement.

```arc
callEffect util:SomeEffect( Input )
callDestructiveEffect util:SomeDestructiveEffect( Input )
```

`callEffect` and `callDestructiveEffect` are action statements and return no query value. Use `callEffect` for an effectful operation and `callDestructiveEffect` when the effect is destructive; destructive effects are deferred with destructive assignments. External function/effect names are registry entries, often qualified by an imported prefix. The model language cannot declare or implement arbitrary external functions; inspect the registry for valid names and signatures.

## 8. Model-assistance procedure and implementation references

When asked to construct or diagnose a model:

1. Identify the model URI, imports, context nesting, role types, filler constraints, and whether each role/property is stored or calculated.
2. Trace the exact query from its starting context/role through every `>>`, `binding`, and `binder`; state the expected result type and cardinality.
3. Identify the acting user role and the relevant object role, then inspect every applicable perspective, state refinement, verb, and privacy modifier. Do not treat a UI screen as authorization.
4. For state-driven behavior, trace condition changes and all entry/exit actions as a cascade. Account for deferred destructive work and place dependent statements after `once settled` only when they truly require settlement.
5. Verify called external functions/effects, their registration and signatures, and whether they are pure or destructive.
6. Use parser/runtime sources and tests to settle uncertain syntax or semantics. Do not silently “correct” established model syntax based only on this summary.

Relevant in-repository sources:

- `packages/perspectives-core/src/arcParser/arcParser.purs` — model, role, property, perspective, state, and screen grammar.
- `packages/perspectives-core/src/arcParser/expressionParser.purs` and `statementParser.purs` — query and assignment grammar.
- `packages/perspectives-core/src/arcParser/arcAST.purs`, `expressionAST.purs`, and `statementAST.purs` — parsed language forms.
- `packages/perspectives-core/src/arcParser/arcParserPhaseTwo.purs` and `arcParserPhaseThree.purs` — name resolution, type checking, and query compilation.
- `packages/perspectives-core/docsources/query-subsystem.md` — query evaluation and external calls.
- `packages/perspectives-core/docsources/transaction-execution.md` — transaction cascade, destructive scheduling, embedded transactions, and `once settled`.
- `packages/perspectives-core/src/model/` and `packages/language-perspectives-arcII/arc sources/` — working model examples; examples demonstrate usage but do not override parser/runtime behavior.
