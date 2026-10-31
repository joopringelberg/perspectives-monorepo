# ARC phase-one parser architecture and implementation

This document records the assessment and approved staged implementation for
issue #545, based on `master` after PR #546. The implementation preserves the
phase-one AST contract and parser behavior while rationalising block, expression,
assignment, and selector parsing.

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

## Expression precedence and previous normalization

Before implementation, `step_` parsed an operand and recursively parsed the
right operand, so an unparenthesised operator chain started as a right-nested
tree. `operatorPrecedence` assigned the current levels; `normalizeBinaryStep`
rotated an unparenthesised right child when the left operator had higher
precedence, and `step` repeated this until structural equality reached a fixed
point. Parentheses prevented a rotation through the `parenthesised` marker.

This implements precedence, and existing tests verify representative additive
precedence and parenthesised grouping (`test/expressionParser.purs:255-335`).
It is not evidence that the present semantics are wrong. It is also not a
conventional, self-evident operator table: multiplication is level 6 and
division level 5; `and` and `or` share level 2; equal-precedence chains remain
right-nested because rotation is strict. Do not “correct” these choices by
analogy with another language. First characterize mixed-operator
associativity, comparison chains, filter/composition boundaries, and all
parenthesis cases against phase-three query shapes.

Implementation uses precedence-aware parsing to construct the `Step` tree
directly. The precedence table is unchanged; strict precedence comparisons keep
equal-precedence chains right-nested. Characterization tests cover mixed
arithmetic, logical and comparison operators, filter grouping, parentheses,
operator positions, and compatibility-sensitive spans.

## Architecture and compatibility decisions

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

## Implementation results

1. Added characterization for existing operator grouping, parentheses,
   filter boundaries, operator positions, malformed assignment dispatch,
   parser-state restoration, and block behavior (same-line rejection, nested
   and outdented children, blank lines, CRLF, EOF, tabs, and malformed children).
2. `entireBlock` and `entireBlock1` now share a block parser with explicit
   empty/nonempty requirements. Their indentation, dedent, EOF, and recovery
   behavior is retained; the rollback-safe state-scope helper is unchanged.
   The domain parser now calls `ensureNoTabs`, which states the helper's actual
   contract.
3. Replaced fixed-point expression-tree normalization with precedence-aware
   parsing. Existing precedence, equal-precedence right nesting, AST fields,
   parentheses markers, and characterized source spans are preserved.
4. Moved `twoReservedWords` into the token module, where shared lexical
   lookahead belongs. Assignment classification now discriminates `delete`
   and `create` forms explicitly; statement AST and settlement-stage lowering
   remain unchanged.
5. Consolidated view/property selection parsing for perspective verbs and screen
   selectors, retaining property source positions where required. The
   screen/widget grammar remains in `arcParser.purs`: it shares declaration
   state and private helpers, so extraction would not provide a narrower
   AST-preserving interface.

Validation: `spago build` succeeded and `test:layer1` passed with 427 tests
(2 skipped) after the parser rationalisation. The model-source compilation
corpus runner built and bundled successfully, then could not initialize its
PDR: this checkout has no `test/pdr-snapshot/universe/alice` snapshot and no
`mkcert` installation or root CA. The resulting fetch to
`https://perspectives.domains/cw_perspectives_domains/` failed with
`self-signed certificate in certificate chain` (`mkcert: not found`; the
configured `/rootCA.pem` was absent). The corpus therefore remains unvalidated
in this environment and needs a local snapshot or trusted CA setup.
