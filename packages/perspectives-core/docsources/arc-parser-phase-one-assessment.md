# ARC phase-one parser architecture assessment

This is the architectural proposal for issue #545, based on `master` after
PR #546. It is an assessment only: no parser behavior or AST is changed here.
The maintainer reports that the expanded source-compilation corpus passes
locally; this milestone does not rerun that preparatory task.

## Current responsibilities and flow

| Module | Current responsibility | Interaction |
|---|---|---|
| `src/arcParser/arcParser.purs` | The 2,033-line ARC model grammar: contexts, roles, properties, states, perspectives, actions, and the complete screen/widget grammar. It creates AST nodes and tracks lexical context, subject, object, and state. | Calls expression and statement parsers; uses indentation/state helpers; produces `ContextE` and related declarations. |
| `src/arcParser/expressionParser.purs` | Query-expression terminals, unary and binary operators, literals, `letE`, external calls, and filler-type combinations. It also repairs binary-tree precedence after parsing. | Produces `Step` nodes consumed by declaration parsing, phase three, and the query compiler. |
| `src/arcParser/indentParser.purs` | Defines `IP` and `ArcParserState`, source-position conversion, scoped state helpers, indentation checks, and block-boundary combinators. | Wraps `Parsing.Indent.Monadic`; all three grammar modules rely on its state and line/block semantics. |
| `src/arcParser/statementParser.purs` | Assignment keyword dispatch and individual assignments, property assignment disambiguation, `letA` bindings, statement stages, and `once settled`. | Calls the expression parser for operands; `arcParser.purs` embeds its statements in actions and automatic effects. |

Phase one returns the existing ASTs rather than compiler/runtime values.
Phase two traverses `ContextE`, `RoleE`, and `StateE` into model
representations (`arcParserPhaseTwo.purs:71-139,417-427`). Phase three
compiles `Step` expressions and statement effects through
`ExpressionCompiler` and `StatementCompiler`
(`arcParserPhaseThree.purs:785-787,867`; `core/queries/expressionCompiler.purs:58-60,828-829`;
`core/assignment/statementCompiler.purs:91-112`). That boundary is useful:
it keeps syntax parsing independent of name/type resolution and query/action
compilation.

## Concrete design pressure

* **One declaration module owns several different grammars.**
  `arcParser.purs` combines recursive model declarations (`contextE`,
  `userRoleE`, `thingRoleE`, `contextRoleE`), state/perspective/action syntax
  (`stateE`, `perspectiveOn`, `perspectiveOf`, `inState`, `onEntryE`,
  `onExitE`, `automaticEffectE`, `actionE`), and screens/widgets
  (`screenE` through `markdownE`, lines 1650-2033). Screen parsing is a
  substantial independent grammar, not just a small declaration production.
  The role parsers repeat block construction and subject/object setup; root
  state assembly also occurs separately in `contextE` and `createRoleState`
  (`arcParser.purs:72-135,318-463,616-647`).
* **Dispatch and selection logic is repeated.** Context, role, and state
  productions each inspect one or two reserved words before branching
  (`contextPart`, `rolePart`, and nested `statePart` near lines 150, 580,
  879). `perspectivePart` and state-scoped perspective parsing repeat some of
  the same alternatives (`956-988,1017-1047`). Three screen/property paths
  separately distinguish `view` from `props` and gather related fields
  (`propertyVerbs`, `withoutProperties`, `withPropertiesAndPositions` at
  1400, 1457, 1502; widget consumers at 1813 and 1914). This is concrete
  duplication, although the variants do not all return the same metadata and
  should not be collapsed without preserving those distinctions.
* **Parser choice can backtrack across more than the discriminator.**
  Both `perspectiveOn` and `perspectiveOf` wrap their full productions in
  `try` because they share the `perspective` prefix (`902,928`). The
  expression `with` operator uses `try` plus `notFollowedBy` to coexist with
  screen `with props/view` syntax (`expressionParser.purs:376-383`). Several
  expression alternatives also use `try`, including bracketed type
  expressions and type combinations (`198-205,668-670`). These are valid
  compatibility choices, not proof of pathological runtime behavior; the
  assessment is that their commitment points and error ownership are hard to
  see consistently. `statementParser.purs:44-84,199-212` first classifies an
  assignment as property versus role syntax, then dispatches role keywords;
  its `twoReservedWords` helper is also imported by the much larger
  `arcParser.purs`, making a statement module a shared grammar utility.
* **Block semantics are implicit in a family of helpers.**
  `withEntireBlock` chooses whether a child block exists from indentation and
  line comparisons (`indentParser.purs:316-328`); `entireBlock` and
  `entireBlock1` differ on empty blocks and then call `unlessOutdented`
  (`330-365`); `nestedBlock` and `optionalNestedBlock` add another
  indentation/next-line gate (`369-382`). The comments explicitly permit a
  block parser to succeed without consuming input and describe special
  handling when a child parser fails after reaching an outdent. The exact
  meaning of same-line, blank-line, EOF, and malformed-child cases is
  therefore distributed across the caller and helper. `isIndented` and
  `outdented'` compare columns; `sameOrOutdented'` adds an EOF exception
  (`386-420`). `domain` separately rejects trailing text and tabs
  (`arcParser.purs:287-300`); `containsTab` fails when it finds a tab but
  returns `true` when it does not (`indentParser.purs:445-450`), so callers
  currently rely only on its failure behavior.
* **State mutation and parser backtracking have different owners.**
  The parser transformer stack keeps `ArcParserState` outside the parser
  input state (`indentParser.purs:125-144`). Parser alternatives can rewind
  input without rewinding that outer state. PR #546 addressed this directly
  with `withScopedArcParserState` (`148-161`) and applied it to context,
  state, and protected-label scopes (`177-250`). This is completed
  groundwork to preserve, not a reason to redesign state ownership before
  characterizing its AST consequences.
* **Some assumptions are encoded indirectly.** Expression and statement
  positions are part of their AST nodes; `BinaryStep` also carries the
  `parenthesised` marker (`expressionAST.purs:96`). `IP` converts between
  parser and ARC positions with `unsafeCoerce` (`indentParser.purs` position
  helpers), while `endOf` in the expression parser reconstructs some ends
  from token lengths and comments on its whitespace assumptions
  (`expressionParser.purs:486-537`). These are compatibility-sensitive
  details; parser cleanup must not silently change spans.

## Expression precedence and normalisation

`step_` parses an operand, then recursively parses the right operand, so an
unparenthesised operator chain starts as a right-nested tree
(`expressionParser.purs:61-105`). `operatorPrecedence` assigns the current
levels (`445-482`); `normalizeBinaryStep` rotates an unparenthesised right
child when the left operator has higher precedence, and `normalizeStep`
walks unary, let, and computation children (`108-185`). `step` repeats that
normalisation until structural equality reaches a fixed point (`61-70`).
Parentheses prevent a rotation through the `parenthesised` marker.

This implements precedence, and existing tests verify representative additive
precedence and parenthesised grouping (`test/expressionParser.purs:255-335`).
It is not evidence that the present semantics are wrong. It is also not a
conventional, self-evident operator table: multiplication is level 6 and
division level 5; `and` and `or` share level 2; equal-precedence chains remain
right-nested because rotation is strict. Do not “correct” these choices by
analogy with another language. First characterize mixed-operator
associativity, comparison chains, filter/composition boundaries, and all
parenthesis cases against phase-three query shapes.

The proposed direction is an explicit precedence/associativity parser that
constructs the intended `Step` tree directly, removing the post-parse
fixed-point pass only after tests demonstrate equivalent grouping and spans.
Keep the current precedence data unchanged in the first refactor. If a
language-level precedence change is later desired, treat it as a separate,
documented behavior change with model and compiler tests.

## Proposed architecture and compatibility

Keep the existing AST contracts as the phase-one boundary:

* Preserve constructors and fields in `arcAST.purs`, `expressionAST.purs`,
  and `statementAST.purs`, including source positions, `BinaryStep`'s
  parenthesis marker, and `Statements`/`LetStep` stage shape.
* Preserve existing `ArcParserState` scope meanings. Keep the rollback-safe
  wrapper from #546, and make parser scopes explicit at the grammar sites
  that enter a context, role/perspective, or state. Do not replace it with a
  new state monad absent evidence that the wrappers cannot express the
  language scopes.
* Give indentation parsing one clearly specified block primitive, with
  existing optional/nonempty wrappers retained until their caller semantics
  are covered. Make dedent/EOF and malformed-indented-child behavior explicit
  rather than silently broadening `try`.
* Keep `arcParser.purs` responsible for declarations and their semantic
  context. After grammar behavior is locked down, extract the screen/widget
  grammar behind its existing AST interface if it reduces the module's
  independent responsibilities without changing AST construction.
* Keep expression terminals/type combinations and statement productions in
  their own modules. Centralize only genuinely shared dispatch/selection
  mechanics; do not make a generic grammar framework. After a unique keyword
  or operator discriminator is consumed, report the local parse error rather
  than retry unrelated alternatives. Limit `try` to the ambiguous prefix.

No phase-two or phase-three boundary change is proposed. They consume the
same AST types and remain important validation clients; moving name
resolution, type inference, or expression/statement compilation into phase
one would mix responsibilities and require no demonstrated benefit here.

## Staged implementation and validation

1. **Characterize behavior.** Add AST-shape and source-position tests for
   operator precedence/associativity, parentheses, `filter`/`with`, malformed
   competing keywords, state restoration, and statement/expression `in`
   boundaries. Include indentation variants: same line, nested/outdented
   blocks, blank lines, CRLF, EOF, tabs, and malformed children. Keep all
   existing suites and assertions.
2. **Clarify block and scope mechanics.** Refactor the indentation helpers
   behind explicit empty/nonempty block contracts and deterministic
   dedent/EOF handling. Reuse #546's state scope helper. Validate direct
   parser tests, state-restoration regressions, malformed-input diagnostics,
   and the full Layer 1 suite before moving grammar callers.
3. **Make expression grouping explicit.** Replace right-recursive tree repair
   with precedence-aware parsing only after the characterization tests are
   in place. Preserve operator positions, expression spans, all AST
   constructors, current grouping, and parentheses metadata. Validate the
   full expression suite, phase-two/phase-three AST and query-shape tests,
   and expression-bearing model compilation.
4. **Rationalise statement grammar.** Make assignment discrimination and
   committed keyword failures explicit; keep `letA`, `once settled`, and
   single-stage `Statements` lowering unchanged. Test every assignment
   constructor plus malformed statements, nested let bindings, and stage
   boundaries; validate phase-three statement compilation and action-stage
   tests.
5. **Consolidate declaration/screen grammar and integrate.** Reduce duplicated
   role/state/perspective and screen selector mechanics where their AST
   outputs are demonstrably equivalent. Extract screen parsing only if the
   narrower interface is clearer. Run `pnpm exec spago build`,
   `pnpm run test:layer1`, and the repeatable
   `pnpm run test:compileModelsFromFile` corpus runner; report each model.
   The reported local corpus pass is the baseline, not a substitute for
   validation after parser changes.

At every stage, compare AST/compiled results for representative real model
sources and treat any changed meaning as a regression unless separately
approved and tested. Keep local Spago `path:` dependencies and generated
outputs/caches out of commits.
