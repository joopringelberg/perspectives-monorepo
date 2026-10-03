# Rebooting the Perspectives Universe

This runbook describes a local rehearsal followed by a deliberate reboot of the
remote repositories. It rebuilds the repositories, publishes models, and creates
the public pages and broker-service configuration needed by new installations.
It is **not** an ordinary, read-only test run.

The intended sequence is:

1. Read the existing remote universe into a PDR snapshot.
2. Switch to local services and rebuild the universe there.
3. Create the browser installation **BigBang**, install the reboot model, and
   load its extra models.
4. Switch back to remote services and copy all relevant existing databases
   before making destructive changes.
5. Execute the reboot cases against the remote universe, including broker
   signup, and verify the result.

There are currently gaps between that intention and the implementation,
especially backup completeness and broker signup. Read the stop conditions below
before proceeding.

## Sources and prerequisites

Run package commands from `packages/perspectives-core/`. The paths in shell
examples below are relative to that directory.

- [Local development setup](../../../localdevelopment/README.md): configure
  MAMP Apache, local CouchDB, certificates, and all three development domains.
- [Package scripts](../package.json): authoritative commands and routing guards.
- [Snapshot creator](../test/createUniverseSnapshot.purs): snapshot contents and
  remote repository lookups.
- [Reboot runner](../test/rebootUniverseTest.purs): selected cases and execution
  order.
- [Reboot model](../src/model/rebootUniverse@2.0.arc) and
  [RepositoryTools](../src/model/repositoryTools@1.0.arc): actual effects,
  credentials, server configuration, publication, and success conditions.

Have the workspace dependencies and PureScript build tools available, together
with `mkcert`, Bash, `curl`, `jq`, and `file`. Review the configured CouchDB
administrator credentials and broker settings for the intended environment.
The ARC cases contain test credentials and a configured CouchDB port; do not
assume these are suitable for a production server.

Obtain authorization and a maintenance window before changing remote data.
Keep database exports and credentials outside the repository. Do not commit
snapshots containing sensitive data without reviewing their contents.

## 1. Create the seed snapshot from remote repositories

```bash
../../localdevelopment/servefromremote
../../localdevelopment/servefromwhere   # must print remote
pnpm run create:universeSnapshot
```

`create:universeSnapshot` refuses to run unless the routing checker reports
`remote`. The creator starts a fresh PDR for `alice`, installs the base models
and its configured `extraModels`, patches and recompiles CouchdbManagement and
RepositoryRegistry locally, and writes:

```text
test/pdr-snapshot/universe/alice
```

It resolves readable names to stable model identifiers using **both** repository
contexts:

- `pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains`
- `pub:https://joopringelberg.nl/cw_servers_and_repositories/#joopringelberg_nl`

The extra-model list includes models from both namespaces. RepositoryTools and
RebootUniverse are deliberately not fetched at this stage: the reboot runner
compiles them from local ARC sources.

**Checkpoint:** the command completes with `Snapshot written to ...`, and the
snapshot exists. A missing model manifest is a failure to fix before continuing.
This snapshot is the seed for the reboot tests, **not a backup of the remote
CouchDB databases**.

## 2. Rebuild the universe locally

```bash
../../localdevelopment/servefromlocal
../../localdevelopment/servefromwhere   # must print local
pnpm run test:rebootUniverse
```

The routing tools switch `perspectives.domains`, `joopringelberg.nl`, and
`mycontexts.com` together. Confirm that HTTPS repository requests actually reach
the local CouchDB before running the reboot; the routing check examines hosts
configuration, not server health.

`test:rebootUniverse` refuses non-local routing. Its entry point currently uses
`rebootUniverseCompileTestModelConfiguration`, which:

- restores the seed snapshot from step 1;
- compiles `src/model/repositoryTools@1.0.arc` and
  `src/model/rebootUniverse@2.0.arc`;
- executes the enabled cases in `rebootUniverseTests`;
- writes the resulting PDR snapshot to
  `test/pdr-snapshot/universe/aliceAfterReboot`.

The sequence starts with `Cleanup`, then `ManageCouchdb` and
`CreateBigBangsDatabase`, creates the `perspectives.domains` and
`joopringelberg.nl` repositories, and publishes the configured models. This
includes RepositoryTools, RebootUniverse, and the enabled test models in the
`joopringelberg.nl` namespace. The final enabled cases configure the broker
service and publish the public pages and repository overview.

**Destructive scope:** `Cleanup` explicitly deletes
`cw_servers_and_repositories`, `cw_perspectives_domains`,
`models_perspectives_domains`, `cw_joopringelberg_nl`,
`models_joopringelberg_nl`, and `cw_bigbangsdatabase` at the configured
`perspectives.domains` server. Both namespaces are cleaned on that server.
If the namespaces are hosted on separate physical servers, this action does
not clean the other server. Investigate conflicts and existing data rather
than treating the entire reboot sequence as an idempotent command.

**Checkpoint:** every enabled test reports success, and both local repositories
contain their expected manifests, model documents, and attachments. This is a
reconstruction of the configured universe, not a byte-for-byte duplication of
every remote database or every historical model version.

## 3. Prepare BigBang in the browser

Remain in **local** mode.

1. Open MyContexts using the local setup and create an installation named
   **BigBang**. The Node test PDR and its `aliceAfterReboot` snapshot do not
   themselves create this browser installation.
2. In BigBang, manually install
   `model://joopringelberg.nl#RebootUniverse@2.0`.
   Its dependency `model://joopringelberg.nl#RepositoryTools@1.0` comes with it.
   Use the repository's manifest-based installation flow: the readable names
   map to stable CUID-based model URIs.
3. Open **Reboot Universe Tests App**. Create/select an `AddExtraModels` test
   context, fill its `Tester` role with the installation user as needed, and
   execute `RunTest`.

The intended shorthand `model://joopringelberg.nl#RebootUniverse$AddExtraModels`
identifies the test context type; the action on its `Tester` role is `RunTest`.
It looks up manifests in both repositories and installs
RabbitMQ, BrokerServices, HyperContext, Introduction, HelpProject, Disconnect,
RepositoryRegistry, and SharedFileServices using their stable URIs and
`VersionToInstall`. From `joopringelberg.nl` it also installs the six enabled
test models in the snapshot creator's `extraModels` list:
SynchronisationTestModel, TwoPDRDestructiveTests, StateTestModel,
SinglePDRDestructiveTests, TransactionExecutionTests, and AMQPtestModel.
These also use the manifests' stable URIs and `VersionToInstall`, rather than
hard-coded versions.

**Checkpoint:** extra-model installation settles successfully before switching
away from the local repositories. Keep BigBang available for the remote phase.

## 4. Copy the existing remote databases before rebooting them

```bash
../../localdevelopment/servefromremote
../../localdevelopment/servefromwhere   # must print remote
```

Restart/reconnect browser sessions if necessary after changing routing, and
confirm the actual endpoints. No destructive remote case may run until the
backup checkpoint below is satisfied.

### What the three scripts do

| Script | Purpose | Important limitation |
|---|---|---|
| [fetchdocs.sh](../scripts/fetchdocs.sh) | Enumerates live document IDs and downloads document bodies to NDJSON. | Skips `_design/` documents and deleted documents. Attachments are stubs, not bytes. Output files are truncated at startup. |
| [storedocs.sh](../scripts/storedocs.sh) | Creates the destination database if possible and uploads the NDJSON bodies. | Removes revision/conflict fields and **all attachments**. This is a fresh-document import, not revision-preserving replication. Existing IDs can conflict. |
| [copyattachments.sh](../scripts/copyattachments.sh) | Reads the same IDs, downloads each attachment from the source, and uploads it to the destination document. | Requires the source to remain reachable and the destination documents to exist. It streams through temporary files; it does not create an offline attachment archive. |

Run them in the order **fetch -> store -> copy attachments**, separately for
each database. Use distinct export filenames and distinct staging destinations.
Use an explicit local CouchDB address such as `http://127.0.0.1:5984` for the
destination while domain names resolve remotely.

### Do they cover both repositories?

**Yes, as generic per-database transfer tools. No, not automatically.** None of
the scripts hard-codes either domain, discovers repositories, or loops through
all relevant databases. The operator must supply the full database URLs and
repeat the process for both namespaces.

Use this minimum public-data inventory, and confirm it against the actual server:

| Source base URL | Database | Contents |
|---|---|---|
| `https://perspectives.domains` | `cw_servers_and_repositories` | Server and repository registrations |
| `https://perspectives.domains` | `cw_perspectives_domains` | Model manifests and related public instances |
| `https://perspectives.domains` | `models_perspectives_domains` | Compiled models and model attachments |
| `https://perspectives.domains` | `cw_bigbangsdatabase` | Public pages, broker-service page, repository overview |
| `https://joopringelberg.nl` | `cw_servers_and_repositories` | Registrations visible through this host |
| `https://joopringelberg.nl` | `cw_joopringelberg_nl` | Model manifests and related public instances |
| `https://joopringelberg.nl` | `models_joopringelberg_nl` | Compiled test/tool models and attachments |

The two hosts can expose the same physical `cw_servers_and_repositories`
database. Verify the deployment topology; if they are distinct, preserve both.
Do not overwrite one export with the other. Also inventory private/write
databases, security configuration, users, and any other data needed for recovery.
The table is not a claim that these seven URLs constitute a full server backup.

### Transfer example

Run the following for one database, then repeat for every inventory entry.
Set `BACKUP_DIR` to a protected directory outside Git. Supply credentials using
your approved local secret-handling method; the scripts take passwords as
command-line arguments, which can be visible to local process inspection.

```bash
# Example only: choose a NEW, empty staging database for each source.
SOURCE_DB='https://perspectives.domains/models_perspectives_domains'
STAGING_DB='http://127.0.0.1:5984/backup_models_perspectives_domains'
IDS_FILE="$BACKUP_DIR/perspectives-domains-models.ids"
DOCS_FILE="$BACKUP_DIR/perspectives-domains-models.ndjson"

bash scripts/fetchdocs.sh \
  --user "$REMOTE_USER" --pass "$REMOTE_PASS" \
  --url "$SOURCE_DB" --ids "$IDS_FILE" --out "$DOCS_FILE"

bash scripts/storedocs.sh \
  --user "$LOCAL_USER" --pass "$LOCAL_PASS" \
  --url "$STAGING_DB" --in "$DOCS_FILE"

bash scripts/copyattachments.sh \
  --src-url "$SOURCE_DB" --dst-url "$STAGING_DB" \
  --src-user "$REMOTE_USER" --src-pass "$REMOTE_PASS" \
  --dst-user "$LOCAL_USER" --dst-pass "$LOCAL_PASS" \
  --ids "$IDS_FILE"
```

For locally trusted HTTPS, pass `--cacert "$(mkcert -CAROOT)/rootCA.pem"` where
needed. Do not use `--insecure` for production transfers. Run imports serially:
`storedocs.sh` uses a shared `/tmp/doc.json` temporary path.

**Backup checkpoint:** inspect responses and warnings, compare exported IDs with
destination IDs, and compare attachment names, lengths, digests, and downloaded
bytes where necessary. Check model sidecars such as `storedQueries.json`,
`stableIdMapping.json`, and `translationTable.json`. Do not rely on exit status
or the final success-looking messages alone: document fetch/upload HTTP errors
are not consistently fatal, and attachment failures can be logged and skipped.

These scripts do not preserve design documents, `_security`, revision history,
or deleted documents, and their enumeration is not an atomic snapshot under
concurrent writes. Quiesce writes and obtain a separate, verified CouchDB backup
or replication-based recovery copy covering the omitted data. Verify a recovery
procedure before authorizing the remote reboot. NDJSON files alone are
insufficient, because they cannot restore the attachment bytes after the source
has been deleted.

## 5. Execute the remote reboot

Only proceed after the local rehearsal and backup/recovery checkpoints pass.
In BigBang, use the installed reboot app to execute the cases in the order
declared by `rebootUniverseTests` in the
[runner](../test/rebootUniverseTest.purs):

1. `Cleanup` (destructive), `ManageCouchdb`, `CreateBigBangsDatabase`.
2. `CreatePerspectivesDomainsRepository`, `CreateJoopringelbergNlRepository`.
3. Every enabled `AddModel_*` case, in its declared order.
4. `ManageBrokerService`, `Add_public_pages`,
   `CreateRepositoryRegistryPublicPage`.
5. `SignUpToBrokerService`, once the implementation prerequisite below is met.

For each browser case, create/select its test context, fill `Tester` as needed,
run `RunTest`, and wait for `TestSucceeded` before advancing. Review actual HTTP
results as well: some modeled success conditions check local context existence,
not a complete remote-data audit. Stop at the first failure.

### Node alternative and current stop conditions

`pnpm run test:rebootUniverse` is intentionally blocked in remote mode.
`pnpm run test:rebootUniverseDanger` runs the **same** Node entry point without
that routing guard. It is the explicit dangerous alternative, not a dry run and
not an additional test suite. It restores the `alice` seed snapshot, not the
browser BigBang installation. Do not run both approaches blindly against the
same rebuilt repositories.

The intended full workflow is **not currently executable unchanged**:

- `SignUpToBrokerService` is commented out in both the runner's test array and
  the reboot ARC model. Neither Node command executes it. Before claiming a
  complete reboot, implement/restore and validate the ARC case, enable the
  runner entry for Node execution, and recompile/republish/reinstall the model
  for browser execution as appropriate. Confirm that broker connectivity and
  account/contract creation actually work; publishing a broker page is not signup. 
- `AddModel_TestModelDependencies` is explicitly disabled as unfinished. It is
  not part of the currently enabled model-publication sequence.
- `Cleanup` deletes both namespaces' databases through the configured
  `perspectives.domains` server. Confirm that both namespaces are hosted there
  before a remote rerun.

Do not describe a run as including signup merely because all currently enabled
tests pass.

## Final verification and recovery

- Verify both repositories' registrations and manifests through their public
  HTTPS endpoints, including correct stable identifiers and installable versions.
- Download representative compiled models and every required attachment from
  both namespaces; install them from a fresh browser installation.
- Verify the public StartPage, Instructions, BigBangsBrokerService, and
  repository overview in `cw_bigbangsdatabase`.
- Verify real broker signup and communication, not just broker configuration.
- Record the executed case list, results, routing mode, source revision, and
  backup locations without recording passwords.

On failure, stop destructive cases, preserve logs, and use the verified recovery
procedure. Importing documents without attachments or restoring only the PDR
seed snapshot is not a rollback of the remote universe. Do not blindly rerun
`Cleanup` to troubleshoot.
