# Universe snapshot models

Inventory and patch audit performed on 2026-11-03.

## What the builder does

[`createUniverseSnapshot.purs`](../createUniverseSnapshot.purs) starts a fresh
PDR using the remote repositories, installs models into its **local** model
database, replaces selected ARC sources with files in this directory, recompiles
those models, and saves `test/pdr-snapshot/universe/alice`. It does not publish
models into either remote repository. Recompilation uses readable, unversioned
model URIs; the source files carry the versions listed below.

Run `pnpm run create:universeSnapshot` from `packages/perspectives-core` when
ready to replace the snapshot. The command requires remote serving mode.

## Existing snapshot date

The existing Alice snapshot completed at **2026-10-21 15:40:15 +02:00**
(13:40:15 UTC), according to the modification time of
`test/pdr-snapshot/universe/alice/snapshot.complete`. This marker is written last
by [`snapshotPDRImpl`](../pdrInstance.js). The models export was written at
15:40:14 +02:00.

These are local filesystem timestamps, not an embedded creation timestamp.
Snapshots are Git-ignored, so Git cannot independently establish their creation
date. The directory's birth time (2026-10-16) predates the latest export and is
not the baseline for this audit.

## Complete model inventory

`setupUser` installs System and RepositoryRegistry, recursively loading their
dependencies. `extraModels` explicitly installs another 16 entries, including
RepositoryRegistry again. The existing snapshot contains 25 model documents
(excluding its design document). The current builder additionally includes
UninstallTarget, giving **26 distinct models** for the new snapshot.

Model names below follow `model://<repository>#<name>@<version>`. Source paths
are relative to [`src/model`](../../src/model).

### perspectives.domains (18 models)

| Model | Source | Installation |
|---|---|---|
| System@7.0 | `systemmodels/perspectivesSysteem@7.0.arc` | `setupUser` |
| RepositoryRegistry@1.0 | `systemmodels/repositoryRegistry@1.0.arc` | `setupUser` and `extraModels` |
| BodiesWithAccounts@5.0 | `systemmodels/BodiesWithAccounts@5.0.arc` | Dependency |
| Couchdb@4.0 | `systemmodels/couchdb@4.0.arc` | Dependency |
| CouchdbManagement@12.4 | `systemmodels/couchdbManagement@12.4.arc` | Dependency |
| Files@3.0 | `systemmodels/files@3.0.arc` | Dependency |
| HelpLib@1.0 | `systemmodels/helpLib@1.0.arc` | Dependency |
| Parsing@3.0 | `systemmodels/parsing@3.0.arc` | Dependency |
| Sensor@3.0 | `systemmodels/sensor@3.0.arc` | Dependency |
| Serialise@3.0 | `systemmodels/serialise@3.0.arc` | Dependency |
| Utilities@3.0 | `systemmodels/utilities@3.0.arc` | Dependency |
| RabbitMQ@2.0 | `systemmodels/rabbitMQ@2.0.arc` | `extraModels` |
| BrokerServices@7.0 | `systemmodels/BrokerServices@7.0.arc` | `extraModels` |
| HyperContext@1.0 | `systemmodels/hypertext@1.0.arc` | `extraModels` |
| Introduction@1.0 | `systemmodels/introduction@1.0.arc` | `extraModels` |
| HelpProject@3.0 | `systemmodels/helpProject@3.0.arc` | `extraModels` |
| Disconnect@1.1 | `systemmodels/disconnect@1.1.arc` | `extraModels` |
| SharedFileServices@4.0 | `systemmodels/sharedFileStorage@4.0.arc` | `extraModels` |

### joopringelberg.nl (8 models)

All eight are explicitly listed in `extraModels`.

| Model | Source |
|---|---|
| SynchronisationTestModel@2.0 | `testmodels/testSynchronisation@2.0.arc` |
| TwoPDRDestructiveTests@1.0 | `testmodels/twoPDRDestructiveTests@1.0.arc` |
| StateTestModel@1.0 | `testmodels/testState@1.0.arc` |
| SinglePDRDestructiveTests@2.0 | `testmodels/singlePDRDestructiveTests@2.0.arc` |
| TransactionExecutionTests@1.0 | `testmodels/transactionExecutionTests@1.0.arc` |
| AMQPtestModel@1.0 | `testmodels/AMQPtestModel@1.0.arc` |
| AMQPtestSetup@1.0 | `testmodels/AMQPtestSetup@1.0.arc` |
| UninstallTarget@1.0 | `testmodels/uninstallTarget@1.0.arc` |

RebootUniverse@2.0 and RepositoryTools@1.0 are commented out in the builder and
are compiled separately during reboot from `src/model/testmodels`.
TestModelDependencies@1.0 is also
commented out. BrokerServiceSignupTests@1.0 changed after the baseline but is
not installed by this builder or present in the existing snapshot, so it is
not part of this patch set.

## Models changed since the snapshot

Git history after `2026-10-21T15:40:15+02:00`, restricted to the included source
files, identifies the following eight models. Dates below are commit dates.
Every row is now wired into both `patchModels` and the local recompilation
list, with a matching JavaScript FFI import.

| Model | Post-snapshot commits (date: abbreviated SHA) | Patch action |
|---|---|---|
| System@7.0 | Nov 3: `712952c62`, `f5cf6c50c` | Refresh existing patch |
| CouchdbManagement@12.4 | Oct 28: `bf26e1709` | Refresh existing patch |
| BrokerServices@7.0 | Oct 21: `8afa3aac8`; Oct 27: `2b384354a` | Refresh existing patch |
| AMQPtestModel@1.0 | Oct 27: `2b384354a`, `326577a96`; Oct 28: `bf26e1709`; Nov 2: `4bfa7ffc3` | Refresh existing patch |
| AMQPtestSetup@1.0 | Oct 21: `3569a8fa4`; Oct 28: `bf26e1709` | Existing patch already current |
| TransactionExecutionTests@1.0 | Nov 3: `712952c62`, `a154cead1` | Add patch and enable recompilation |
| TwoPDRDestructiveTests@1.0 | Nov 3: `a154cead1` | Add patch and enable recompilation |
| UninstallTarget@1.0 | Nov 3: `712952c62` | Add patch and enable recompilation |

The Oct 21 changes occurred at 20:45 and 20:50 CEST, **after** the snapshot;
filtering only by calendar day would miss them.

All model patches with a same-named counterpart in `src/model/systemmodels` or
`src/model/testmodels` are kept
byte-for-byte identical to it. RepositoryRegistry@1.0 is already current and
remains disabled in the snapshot patch configuration: it has no post-baseline
source changes. `ScaffoldPatchTest.arc` is a standalone test fixture, has no
counterpart in `src/model`, and is not installed in the universe.

Re-run this audit if model sources, the remote repositories, or `extraModels`
change. Dependency versions above were verified from the existing snapshot's
model documents; a new download can resolve different remote dependencies.
