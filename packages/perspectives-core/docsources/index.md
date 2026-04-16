# Perspectives Core – Documentation Index

This index provides an overview of the technical documentation in `packages/perspectives-core/docsources` and a cross-reference from PureScript modules to the chapters that discuss them.

---

## Documents

### Architecture & Infrastructure

| Document | Description |
|---|---|
| [Installation Initialization](installation-initialization.md) | What happens when a new Perspectives installation is created — covers both the PDR (PureScript) side and the Perspectives Language (PL) side, including the shared contexts that are bootstrapped on first run. |
| [Model URI resolution](model-uri-resolution.md) | How `ModelUri` values are deterministically mapped to `models_*` and `cw_*` URLs (`modelUri2ModelUrl`, `modelUri2ManifestUrl`), including naming conventions, assumptions, and the reason tests may need an alternate mapping. |
| [PDR → Frontend Messaging](pdr-messaging.md) | The two mechanisms the PDR uses to push messages to the browser frontend: the status-message channel (`setPDRStatus`) and the reactive warning queue, plus how a developer wires up each one. |
| [PDR Client Data Structures](pdr-client-data-structures.md) | All data structures the PDR serialises and sends to its clients (`mycontexts` / `perspectives-react`). Starting from `ScreenDefinition` the document descends into `Perspective`, `Roleinstancewithprops`, and every referenced type, including the developer-tool types `InspectableContext` and `InspectableRole`. |
| [Structured Logging](structured-logging.md) | The runtime-configurable logging layer: `LogLevel`, `LogTopic`, and `LogConfig` types; the `pdrLog` entry point and its pre-bound convenience aliases; `MonadPerspectives` helpers for changing thresholds at runtime; and three JavaScript-callable functions (`setLogLevelForTopic`, `disableLogTopic`, `disableLogging`) that allow reconfiguring the log from the browser DevTools console without restarting. Also includes a migration guide for replacing legacy `log`/`logPerspectivesError` call sites and instructions for adding new topics. |

### Query System

| Document | Description |
|---|---|
| [The Query Subsystem](query-subsystem.md) | End-to-end technical overview of the query pipeline: parsing an ARC expression into an AST, compiling it to a typed `QueryFunctionDescription`, evaluating it at runtime via the interpreter, and registering computed values in the external-function cache. Also covers three-valued logic and assumption tracking. |
| [Query Inversion](query-inversion.md) | The compile-time transformation that turns every forward query into one or more *inverted* queries. Inverted queries allow the runtime to answer "given that data item X changed, which running queries are affected and which users must be notified?" without re-evaluating the world. |

### Transaction & Delta System

| Document | Description |
|---|---|
| [Transaction Execution Process](transaction-execution.md) | How the PDR executes a transaction: the two entry paths (user-initiated vs. peer-received), the multi-phase processing loop (state evaluation → action execution → synchronisation), and the design rationale for the production-rule model. |
| [Delta Ordering and Conflict Resolution](delta-ordering.md) | How the PDR achieves convergence in a distributed system where peers can modify the same resources concurrently. Covers the resource-version scheme, deterministic ordering via `executeDeltaWithVersionTracking`, and the pending-transaction store used to defer out-of-order deltas. |
| [Context Serialisation for a Peer](context-serialisation-for-peer.md) | How a Perspectives context instance is serialised as a set of deltas so that a peer can reconstruct it locally. Central to the Invitation mechanism and to handing a complete context representation to a new installation. |

### Type System & Compiler

| Document | Description |
|---|---|
| [Type Comparison](type-comparison.md) | How types are represented (ADT, ExpandedADT, CNF), normalised, and compared throughout the PDR. Covers `equalsOrSpecialises`, `leastCommonSuperType`, role-class hierarchy, and the role of Phase Three in populating comparison tables. |
| [Stable ID Mapping](stable-id-mapping.md) | How stable type identifiers are preserved across ARC model refactors using a `stableIdMapping.json` sidecar stored alongside the DomeinFile. Covers the alias mechanism, snapshot heuristics, and where in the compilation pipeline the mapping is applied. |

---

## Module Index

The table below lists every PureScript module that is referenced in at least one documentation chapter.  Each entry links back to the chapter(s) where it is discussed.

| Module | Discussed in |
|---|---|
| `Perspectives.AMQP.IncomingPost` | [Context Serialisation for a Peer](context-serialisation-for-peer.md), [Transaction Execution Process](transaction-execution.md) |
| `Perspectives.Assignment.Update` | [Installation Initialization](installation-initialization.md) |
| `Perspectives.Checking.PerspectivesTypeChecker` | [Type Comparison](type-comparison.md) |
| `Perspectives.CollectAffectedContexts` | [Query Inversion](query-inversion.md) |
| `Perspectives.ContextStateCompiler` | [Transaction Execution Process](transaction-execution.md) |
| `Perspectives.CoreTypes` | [The Query Subsystem](query-subsystem.md), [Structured Logging](structured-logging.md) |
| `Perspectives.ErrorLogging` | [PDR → Frontend Messaging](pdr-messaging.md) |
| `Perspectives.Logging` | [Structured Logging](structured-logging.md) |
| `Perspectives.External.HiddenFunctionCache` | [The Query Subsystem](query-subsystem.md) |
| `Perspectives.Identifiers` | [Model URI resolution](model-uri-resolution.md) |
| `Perspectives.Inspector.InspectableResources` | [PDR Client Data Structures](pdr-client-data-structures.md) |
| `Perspectives.Instances.Me` | [Installation Initialization](installation-initialization.md) |
| `Perspectives.Instances.ObjectGetters` | [Type Comparison](type-comparison.md) |
| `Perspectives.Instances.SerialiseAsJson` | [Context Serialisation for a Peer](context-serialisation-for-peer.md) |
| `Perspectives.InvertedQuery` | [Query Inversion](query-inversion.md) |
| `Perspectives.ObjectGetterLookup` | [The Query Subsystem](query-subsystem.md) |
| `Perspectives.Parsing.Arc.Expression` | [The Query Subsystem](query-subsystem.md) |
| `Perspectives.Parsing.Arc.Expression.AST` | [The Query Subsystem](query-subsystem.md) |
| `Perspectives.Parsing.Arc.PhaseThree` | [Type Comparison](type-comparison.md) |
| `Perspectives.Persistence.DeltaStore` | [Delta Ordering and Conflict Resolution](delta-ordering.md) |
| `Perspectives.Persistence.PendingTransactionStore` | [Delta Ordering and Conflict Resolution](delta-ordering.md) |
| `Perspectives.Persistence.ResourceVersionStore` | [Delta Ordering and Conflict Resolution](delta-ordering.md) |
| `Perspectives.PerspectivesState` | [PDR → Frontend Messaging](pdr-messaging.md), [Structured Logging](structured-logging.md) |
| `Perspectives.Query.ExpressionCompiler` | [The Query Subsystem](query-subsystem.md) |
| `Perspectives.Query.Interpreter` | [The Query Subsystem](query-subsystem.md) |
| `Perspectives.Query.Interpreter.Dependencies` | [The Query Subsystem](query-subsystem.md) |
| `Perspectives.Query.Inversion` | [Query Inversion](query-inversion.md) |
| `Perspectives.Query.Kinked` | [Query Inversion](query-inversion.md) |
| `Perspectives.Query.QueryTypes` | [The Query Subsystem](query-subsystem.md) |
| `Perspectives.Query.UnsafeCompiler` | [The Query Subsystem](query-subsystem.md) |
| `Perspectives.ReferentialIntegrity` | [PDR → Frontend Messaging](pdr-messaging.md) |
| `Perspectives.Representation.ADT` | [Type Comparison](type-comparison.md) |
| `Perspectives.Representation.CNF` | [Type Comparison](type-comparison.md) |
| `Perspectives.Representation.Class.Role` | [Type Comparison](type-comparison.md) |
| `Perspectives.Representation.ExpandedADT` | [Type Comparison](type-comparison.md) |
| `Perspectives.Representation.InstanceIdentifiers` | [PDR Client Data Structures](pdr-client-data-structures.md) |
| `Perspectives.Representation.ScreenDefinition` | [PDR Client Data Structures](pdr-client-data-structures.md) |
| `Perspectives.Representation.ThreeValuedLogic` | [The Query Subsystem](query-subsystem.md) |
| `Perspectives.Representation.TypeIdentifiers` | [PDR Client Data Structures](pdr-client-data-structures.md) |
| `Perspectives.Representation.Verbs` | [PDR Client Data Structures](pdr-client-data-structures.md) |
| `Perspectives.RoleStateCompiler` | [Context Serialisation for a Peer](context-serialisation-for-peer.md), [Transaction Execution Process](transaction-execution.md) |
| `Perspectives.RunMonadPerspectivesTransaction` | [Transaction Execution Process](transaction-execution.md) |
| `Perspectives.Sync.HandleTransaction` | [Delta Ordering and Conflict Resolution](delta-ordering.md), [Transaction Execution Process](transaction-execution.md) |
| `Perspectives.Sync.LegacyDeltas` | [Delta Ordering and Conflict Resolution](delta-ordering.md) |
| `Perspectives.Sync.Transaction` | [Transaction Execution Process](transaction-execution.md) |
| `Perspectives.SideCar.PhantomTypedNewtypes` | [Model URI resolution](model-uri-resolution.md) |
| `Perspectives.TypePersistence.PerspectiveSerialisation.Data` | [PDR Client Data Structures](pdr-client-data-structures.md) |
| `Perspectives.Types.ObjectGetters` | [Type Comparison](type-comparison.md) |
| `Perspectives.TypesForDeltas` | [Delta Ordering and Conflict Resolution](delta-ordering.md) |
