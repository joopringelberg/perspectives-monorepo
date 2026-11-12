# perspectives-core test suite inventory

This inventory records how the test modules outside the `test:layer1`,
`test:layer2`, and `test:layer3` entry points fit into a regression plan. It
distinguishes compilation from execution: `pnpm exec spago build` compiled the
PureScript project and its test modules successfully in this checkout, but that
does not execute their assertions. The service-dependent runners below were not
run as part of this inventory.

## Current layer coverage 

| Runner | Current coverage | Assessment |
|---|---|---|
| `pnpm run test:layer1` | ARC parsing and compilation, ADT/type operations, query-description compilation, model translation, and focused pure or in-memory regressions. | Keep as the fast default suite. |
| `pnpm run test:layer2` | Currently runs persistence recovery and model-patch suites. `Test.LoadArc` is imported but commented out; context/role, query, and transaction suites are also commented out. | Keep as the persistence integration layer; verify its runtime before calling it green. `persistenceAPI.node.js` already registers the in-memory PouchDB adapter, so the migration TODOs in `Layer2.purs` are stale. |
| `pnpm run test:layer3` | Constructive and destructive synchronization, model-compilation regression, query-step, single-PDR destructive, transaction-execution (compile-from-source configuration), run-action, and scaffold tests. | Keep for full-PDR regressions. A local in-process transport test is distinct from the real-broker tests below. |

`spago build` compiled the test modules too, including modules that are not
reachable from these three runners. A successful build is therefore not
evidence that a suite is included in a runner or that its tests pass.

### Perspective property-anchor regressions

Layer 1 includes `Test.Query.PropertyAnchors`. It checks that filter criteria
remain indexed as dependencies, while property expansion starts only at the
perspective result. The compiler-only markers distinguish result anchors,
result filters, and dependencies even when their role types coincide; they
are not serialized in stored queries.

The suite covers inherited properties, nested filters, navigation after a
filter, and `filledBy` criteria. It also executes compiled property paths across
false-to-true-to-false visibility changes: property-update paths respect the
filter, while criterion dependencies remain reachable when it becomes false.
The model-file compilation corpus includes Stadskamer as the full-model
regression for the original participant-name compilation failure.

## Standalone registered test runners

All PureScript entry points in this table compiled under `spago build`. “Unique”
means the coverage is not already exercised by the three current layer runners.
The focused alias entry modules for `test:modelTranslation`, `test:runAction`,
`test:destructive`, `test:destructiveSync`, and `test:transaction:compile` are
under `test/focussedAliasTests/`; the shared suite modules are also imported by
the layer runners where applicable.

| Command | Coverage beyond layers | Recommendation |
|---|---|---|
| `test:modelTranslation` | No unique coverage; `Test.ModelTranslation` is in Layer 1. | Keep only if the focused command is useful; otherwise remove the duplicate runner. |
| `test:runAction` | No unique coverage; Layer 3 runs the same suite. | Treat as a focused alias, not a separate test-plan item. |
| `test:destructive`, `test:destructiveSync` | No unique coverage; the corresponding single-PDR and destructive-sync suites are in Layer 3. | Keep as optional focused aliases or remove once no longer needed. |
| `test:transaction:compile` | No unique coverage; Layer 3 runs the same transaction suite with the compile-from-source configuration. | Treat as a focused alias or remove. |
| `test:transaction` | Exercises transaction tests with the repository-backed model configuration rather than Layer 3's compile-from-source configuration. | Keep as an explicit model/repository regression; document its model and service prerequisites. |
| `test:amqp` | Real RabbitMQ synchronization, not the in-process transport covered by Layer 3. | Keep in a separate, opt-in live-broker integration group; it requires local routing. |
| `test:brokerServiceSignup` | Broker account/queue signup and cleanup across PDRs. | Keep in the opt-in live-service group; it requires local perspectives.domains routing and RabbitMQ. |
| `test:freshSubscriptions` | Fresh AMQP subscription lifecycle. | Keep in the opt-in live-broker group; do not make it part of the fast layers. |
| `test:modelDependencies` | Dependency installation/resolution against the locally served model repository. | Keep as an opt-in repository integration test. |
| `test:compileModelsFromFile` | Compiles ARC files from disk through the PDR model-loading flow. | Keep; place with model-loading/persistence integration (Layer 2 if its environment supports it, otherwise a dedicated model integration group). |
| `test:stableIds` | Recompilation and persistence of stable-ID mappings; Layer 1's sidecar tests cover a different, lower-level path. | Keep with model persistence integration (Layer 2 or a dedicated model group). |
| `test:snapshotLifecycle` | Snapshot publication and reboot prerequisites. | Keep in a dedicated restart/snapshot integration group; it is not a fast Layer 3 test. For repeated snapshot runs, use fresh processes because in-memory databases survive PDR shutdown. |
| `test:rebootUniverse` | Universe reboot against the local service setup. | Keep as an explicit destructive/environmental test, not a default layer. |
| `test:rebootDatabase` | Big-bang database creation and publication. | Keep as a separately gated destructive/environmental test. |
| `test:rebootUniverseDanger` | Runs the same `Test.RebootUniverse` entry point as `test:rebootUniverse`, without its local-routing guard. | Remove; the guarded command already exists. |
| `test:performance`, `test:performance:amqp` | Timed PDR scenarios, optionally over real AMQP. | Keep outside regression layers as benchmarks; run the AMQP variant only in its explicitly configured environment. |
| `test:performance:scaffold` | Regression tests for measured-trial isolation and failure handling. | Keep as a separate pure harness test, or move into Layer 1 after confirming it remains independent of PDR/service setup. |
| `test:lsp` | Node.js tests for LSP diagnostics and name resolution; independent of the Layer 1–3 suites. | Keep as a focused Node test command and repair the failing fixture noted below. |
| `test:node` | `Test.Main` calls only `Test.Class.Role`; the other imported suites are commented out. | Retire this legacy entry point after moving or explicitly retaining `Test.Class.Role` in the canonical unit runner. |
| `test:recompileModelFromFile` | Tests local repository document preparation, validation, and writes against a local HTTP mock. | Keep; group with unit tests for model publication/recompilation, not the live-repository tests. |

`build:layer3` and `build:performance` only build bundles; they do not execute
tests. `create:universeSnapshot` and `recompile:model` are setup/tool commands,
not test suites, and should not be counted as regression coverage.

## Compiled suites with no active runner

These modules compiled, but are not invoked by Layer 1–3 or by a dedicated
package test command. Add them to a runner only after checking their setup and
avoiding duplicate or service-dependent coverage.

| Module | Coverage | Recommendation |
|---|---|---|
| `Test.Parsing.Arc.DuplicateDeclarations` | Duplicate ARC declaration errors. | Add to Layer 1. |
| `Test.Perspectives.ExternalUtilities` | Pure model-version transformation utilities. | Add to Layer 1. |
| `Test.ModelDependencyAdministration` | Dependency metadata and resolution-plan calculations. | Add to Layer 1; keep database/repository behavior in the separate integration suite. |
| `Test.BindingReplacement` | User-role filler replacement and resulting transaction/delta behavior in an in-memory PDR. | Add to Layer 2 or a focused in-memory runtime group. |
| `Test.PublicationRecovery` | Recovery of published resources after database deletion using a PDR. | Add to Layer 2 when its database setup is available. |
| `Test.RabbitMQDeletion` | Deletion-result semantics exercised against a local HTTP fixture, not a live broker. | Add to a mock-service unit group; it does not need the live-broker suite. |
| `Test.RebootModelCompilation` | Model compilation against a seed snapshot without executing reboot actions. | Add to the snapshot/reboot integration group. |

`Test.Layer2` also leaves `Test.LoadArc` and the context/role, query, and
transaction suites commented out. Decide individually whether to enable them
after the Layer 2 runtime prerequisites are verified; compilation alone is not
enough.

## Native Node.js tests

JavaScript tests do not have a PureScript compile status. The following
standalone tests were run directly with Node:

| Test file | Coverage and disposition | Result |
|---|---|---|
| `test/databaseInfo.test.mjs` | Local/HTTP database-info responses and error propagation. No package script currently runs it; add it to a small Node unit-test command. | 8 passed. |
| `test/transactionQueue.test.mjs` | Scheduler ordering, fairness, cancellation, and queue isolation. Not in a package script; retain as a direct JavaScript regression, noting that Layer 1 separately tests transaction admission through the PDR API. | 6 passed. |
| `test/performanceMeasurements.test.mjs` | Measurement and statistics handling. | 12 passed via `test:performance:collector`. |
| `test/lsp.test.mjs` | LSP diagnostic ranges and name resolution. | 12 passed, 1 failed. The first test passes `null` as the `Step` argument to `NotFunctional`; use a valid `Step` fixture, then keep the suite. |

## Suggested plan

1. Keep Layer 1 as the fast pure/in-memory regression gate, and add the
   currently orphaned pure suites listed above.
2. Keep Layer 2 focused on persistence/model-loading with verified database
   setup; do not claim the commented suites are covered.
3. Keep Layer 3 focused on full-PDR and in-process synchronization.
4. Preserve real RabbitMQ, repository, reboot, and snapshot tests as explicit
   service/environment groups rather than adding them to every regression run.
5. Remove redundant aliases only after confirming nobody relies on them for
   focused debugging. Add a Node unit-test script for the currently unregistered
   database-info and transaction-queue tests, and fix the LSP test fixture.

For command definitions and runner membership, see `package.json` and
`test/Layer1.purs`, `test/Layer2.purs`, `test/Layer3.purs`, and `test/Main.purs`.

## Files with no direct reference from the inventoried suites

The lists below are a static reference check against the suite modules and
standalone tests listed above, including the Layer 2 suites documented as
disabled. A listed file has no direct filename/path reference from those tests;
this is a purge-review list, **not** a recommendation to delete. ARC `use`
dependencies, model URIs resolved at runtime, and files inside snapshots are
not fully resolved by this check. Some candidate fixtures may also be used by
older test modules that are not included in the inventory.

### `test/`

These 16 PureScript test modules are not reached from an inventoried suite or
runner:

```text
test/Channel.purs
test/abstractDataType.purs
test/combinators.purs
test/conversations/parser.purs
test/conversations/parserMain.purs
test/conversations/renderer.purs
test/couchdbExternalFunctions.purs
test/domeinFileEncoding.purs
test/invertFunctionDescription.purs
test/monoid.purs
test/prettyPrint.purs
test/rabbitmq.purs
test/setAffectedContextCalculations.purs
test/transferFile.purs
test/typeLevelObjectGetters.purs
test/sync/SetPropertyGetProperty.purs
```

These 47 non-PureScript test-directory files have no direct reference from the
inventoried suites:

```text
test/actionsTestAddProperty.crl
test/actionsTestBind.crl
test/actionsTestBind_.crl
test/actionsTestDeleteProperty.crl
test/actionsTestRemoveProperty.crl
test/actionsTestSetProperty.crl
test/actionsTestUnbind.crl
test/actionsTestUnbindQualified.crl
test/actionsTestUnbind_.crl
test/actionsTestcase1.crl
test/actionsTestcase2.crl
test/actionsTestcase3.arc
test/actionsTestcase3.crl
test/arcsyntax.arc
test/chatInitiator.crl
test/chatInvitation.crl
test/combinators.arc
test/combinators.crl
test/contextAndRole.crl
test/contextAndRole2.crl
test/contextRoleParser.arc
test/contextRoleParser.crl
test/inversion.arc
test/invertedQueryTest.arc
test/log
test/log.txt
test/logging-instructions.md
test/onContextDelta_context.crl
test/onContextDelta_role.crl
test/onPropertyDelta.crl
test/onRoleDelta_binder.crl
test/onRoleDelta_binding.crl
test/parsetest1.arc
test/perspectivesSysteem.crl
test/publicRoleTypeAssertion.arc
test/queries.crl
test/roleTest.arc
test/runMonadPerspectivesTransaction.arc
test/serialisedAsDeltas.arc
test/setAffectedContextCalculations.arc
test/test instructions.md
test/test1.json
test/test2.arc
test/test2.json
test/testCaseInCouchdb.crl
test/userCor.crl
test/userJoop.crl
```

`test/patches/README.md` is documentation, referenced only by a comment in the
snapshot setup tool; it is not a test input.

`test/domeinFileEncoding.js` is the FFI companion of the unreferenced
`Test.DomeinFile.Encoding` module. The two native test files and their fixtures
are not candidates: they are run by `test:lsp` and
`test:performance:collector`. `test/createUniverseSnapshot.purs` and its JS
companion are setup tooling rather than tests; `createUniverseSnapshot.js`
imports the patch ARC files. The broker-signup test also imports patch files,
and `Test.ModelPatches` uses `test/patches/ScaffoldPatchTest.arc`, so the patch
ARC files are retained as tool/test inputs rather than classified as
unreferenced.

### `src/model/`

These 107 files have no direct file-path or local-model-loader-name reference
from the inventoried test suites:

```text
src/model/BodiesWithAccounts@5.0.arc
src/model/BrokerContract.smcat
src/model/BrokerContract.svg
src/model/BrokerServices.arc
src/model/BrokerServices@6.1.arc
src/model/FirstApp.arc
src/model/JoopsApp.arc
src/model/ModelTestGroups@2.0.arc
src/model/RouteVanDeVraag.arc
src/model/Simple.arc
src/model/TestActionNameInMarkdown.arc
src/model/TestActionPerspectives.arc
src/model/adressbook@1.0.arc
src/model/be-pattern.arc
src/model/birthdayChatGPT.arc
src/model/competition.arc
src/model/couchdb@4.0.arc
src/model/couchdbManagement.arc
src/model/couchdbManagement@12.2.arc
src/model/disconnect.arc
src/model/expressionEvaluator.arc
src/model/files@3.0.arc
src/model/helpLib@1.0.arc
src/model/italie.arc
src/model/json example.arc
src/model/menus.arc
src/model/minimalmodel.arc
src/model/modelManagement.arc
src/model/parsing@3.0.arc
src/model/perspectivesSysteem@6.1.arc
src/model/perspectivesSysteem@6.2.arc
src/model/perspectivesSysteem@6.3.arc
src/model/rebootUniverse@1.0.arc
src/model/sensor@3.0.arc
src/model/serialise@3.0.arc
src/model/shoppinglist overdrachtsbestand.arc
src/model/shoppinglist.arc
src/model/smallestModel.arc
src/model/stadskamers.arc
src/model/test.arc
src/model/testAliases.arc
src/model/testBindingen.arc
src/model/testDeltaOrdering@1.0.arc
src/model/testDoubleBindingRestriction.arc
src/model/testDuration@1.0.arc
src/model/testFields@1.0.arc
src/model/testFilledByRestrictions@1.0.arc
src/model/testFilter.arc
src/model/testMarkDown.arc
src/model/testPersonal.arc
src/model/testPropertyFillFrom.arc
src/model/testPublicRole.arc
src/model/testQueries.arc
src/model/testStateEntryAndExit@1.0.arc
src/model/testSynchronisation@1.0.arc
src/model/testTypeName.arc
src/model/testTypeahead@1.0.arc
src/model/testWhen@1.0.arc
src/model/testcdb.arc
src/model/testcor.arc
src/model/testcor2.arc
src/model/testimported.arc
src/model/testimporter.arc
src/model/testsidecar.arc
src/model/testUpload@1.0.txt
src/model/twoPlayerBoardGame.arc
src/model/utilities@3.0.arc
src/model/voorbeeldOverdracht.arc
src/model/witcoin.arc
src/model/patches/2501/BodiesWithAccounts.arc
src/model/patches/2501/BrokerServices.arc
src/model/patches/2501/couchdbManagement.arc
src/model/patches/2501/disconnect.arc
src/model/patches/2501/hypertext.arc
src/model/patches/2501/introduction.arc
src/model/patches/2504/couchdbManagement.arc
src/model/patches/3061/italie.arc
src/model/patches/3_3_6/perspectivesSysteem@6.3.arc
src/model/translations/model___joopringelberg.nl#Stadskamers@1.0.yaml
src/model/translations/model___joopringelberg.nl#TestDeltaOrdering@1.0.yaml
src/model/translations/model___joopringelberg.nl#TestDuration@1.0.yaml
src/model/translations/model___joopringelberg.nl#TestWhen@1.0.yaml
src/model/translations/model___joopringelberg.nl#njr2ff2ose@1.0.yaml
src/model/translations/model___perspectives.domains#BrokeServices@6.0.yaml
src/model/translations/model___perspectives.domains#BrokerServices@6.1.yaml
src/model/translations/model___perspectives.domains#BrokerServices@7.0.yaml
src/model/translations/model___perspectives.domains#CouchdbManagement@12.0.yaml
src/model/translations/model___perspectives.domains#CouchdbManagement@12.2.yaml
src/model/translations/model___perspectives.domains#CouchdbManagement@12.4.yaml
src/model/translations/model___perspectives.domains#Disconnect@1.1.yaml
src/model/translations/model___perspectives.domains#HelpProject@3.0.yaml
src/model/translations/model___perspectives.domains#HyperContext@1.0.yaml
src/model/translations/model___perspectives.domains#ModelTestGroups@2.0.yaml
src/model/translations/model___perspectives.domains#RepositoryRegistry@1.0.yaml
src/model/translations/model___perspectives.domains#SharedFileServices@2.0.yaml
src/model/translations/model___perspectives.domains#System@6.2.yaml
src/model/translations/model___perspectives.domains#System@6.3.yaml
src/model/translations/model___perspectives.domains#System@7.0.yaml
src/model/translations/model___perspectives.domains#introduction@1.0.yaml
src/model/translations/model___perspectives.domains#kxrm2cilp9@1.1.yaml
src/model/translations/model___perspectives.domains#tiodn6tcyc@6.0.yaml
src/model/translations/model___perspectives.domains#tiodn6tcyc@6.1.yaml
src/model/translations/model___perspectives.domains#zjuzxbqpgc@6.1.yaml
src/model/translations/translation_of_model___perspectives.domains#BodiesWithAccounts@4.0.yaml
src/model/translations/translation_of_model___perspectives.domains#Introduction@2.0.yaml
src/model/translations/translation_of_model___perspectives.domains#SharedFileServices@3.0.yaml
src/model/translations/translation_of_model___perspectives.domains#SimpleChat@1.0.yaml
```

The direct-reference counts are based on the inventory's source paths and
loader calls, not on all runtime dependencies. In particular, do not treat a
model in this list as safe to remove until its ARC `use` dependencies, model
publication/build inputs, and any non-test consumers have been checked.
