# Remote CouchDB backup and restore (Ubuntu)

These scripts run **on the server**, not on the Mac over SSH. They target the
Ubuntu 22.04 systemd installation confirmed on the server:

- `couchdb.service`, running as `couchdb:couchdb`;
- CouchDB 3.3.2 under `/opt/couchdb`;
- configuration under `/opt/couchdb/etc`;
- `database_dir = ./data` and `view_index_dir = ./data`;
- local HTTP endpoint `http://127.0.0.1:5984`.

They use Bash entry points and a shared Python 3 standard-library implementation.
No pip packages are needed. Requirements: `bash`, `python3`, `systemctl`, and
`hostname`. Run with `sudo`; do not change ownership of the live CouchDB tree
yourself.

## Deployment

Copy this folder to the server, or update the monorepo checkout there. For example,
from the monorepo root on your Mac (replace `SERVER` with your SSH host):

```bash
scp -r remotedevelopment joop@SERVER:~/remotedevelopment
ssh joop@SERVER
cd ~/remotedevelopment
chmod +x backupcouchdb restorecouchdb
./backupcouchdb --help
./restorecouchdb --help
```

Keep `couchdb_tools.py` beside the two scripts.

## Backup

```bash
sudo ./backupcouchdb
sudo ./backupcouchdb --dest /mnt/backups/couchdb
```

The default destination is `/var/backups/couchdb`. A backup is created as
`couchdb-<UTC timestamp>`. The script:

1. Reads the normal configuration chain in order: `default.ini`, sorted
   `default.d/*.ini`, `local.ini`, sorted `local.d/*.ini`.
2. Checks paths and free space, and obtains an exclusive lock shared with restore.
3. Stops CouchDB through systemd (plan for server downtime).
4. Copies indexes before databases, including hidden `.shards` indexes, then the
   complete configuration directory.
5. Writes `MANIFEST.json` and SHA-256 checksums for **data, indexes, configuration
   and the manifest**, and compares the copy with the stopped source.
6. Renames the temporary `.incomplete-couchdb-*` directory to its final name.
7. Restarts CouchDB if it was initially active, and waits for `/_up`.

If the service was initially stopped, it stays stopped. On failure, incomplete
backups remain clearly marked; they are never accepted by restore. A restart
failure returns a nonzero exit code, even if the backup itself was completed.

Options matching the local Mac scripts:

```bash
sudo ./backupcouchdb --no-restart
sudo ./backupcouchdb --online
```

`--online` avoids downtime, but files can change while being copied. Checksums
verify the resulting copy, **not a coherent source snapshot**. Databases may be
mutually inconsistent; prefer stopped backups. An online copy can fail if files
disappear during copying. `--no-restart` has no effect with `--online`.

The destination must be outside the data, index and configuration directories.
Allow room for the copy plus at least 100 MiB free. Configuration contains
password hashes and possibly other secrets. New backup directories are private
to root. Do not put backups in a web-served directory or commit them to Git.
Protect existing destination directories and encrypt off-server copies.

These are local disk backups, not protection against loss of the server.
Copy completed backups to independent storage through your secure transfer
procedure. Ordinary `scp` as `joop` cannot read root-only backups: create a private
archive accessible only to that account if needed, rather than making backup
directories world-readable. Verify downloaded archives before relying on them.

## Restore

Schedule downtime and ensure a recent independent backup exists first:

```bash
sudo ./restorecouchdb /var/backups/couchdb/couchdb-TIMESTAMP
```

Type `restore` when prompted. For deliberate non-interactive use:

```bash
sudo ./restorecouchdb --yes /var/backups/couchdb/couchdb-TIMESTAMP
```

Before stopping CouchDB, restore checks all backup hashes and the exact file
inventory, compares the CouchDB version and Erlang node name, checks the index
layout, and checks free space on each target filesystem. It then moves the
current directories aside with a `.before-restore-<timestamp>` suffix, copies and
verifies the restored files, sets their ownership to the systemd service account,
and restarts an initially active server.

Copy, verification or startup failures trigger rollback while CouchDB is stopped.
Failed copies are kept with a `.failed` suffix, not deleted. If CouchDB cannot be
stopped safely during rollback, rollback aborts and the script reports an error;
inspect the kept directories before taking manual recovery steps.

Options:

- `--with-config`: replace the **entire** `etc` directory, not just `local.ini`.
  This includes `local.d/10-admins.ini`, default overrides and `vm.args`.
  The saved configuration must point to the current data/index directories and
  retain the current node name. The previous configuration is kept aside.
  Systemd units and environment files outside `etc` are not restored.
- `--no-restart`: leave CouchDB stopped, including after a rollback.
- `--force`: allow a version or node-name mismatch for data restoration. It does
  **not** waive checksums, path checks, index layout or configuration safeguards.
  Compatibility is your responsibility; this is not a migration tool.

The original data remains on disk, so restoring needs **additional** space for
the backup plus a 100 MiB margin per destination filesystem. If a version cannot
be obtained (for example, the server is already stopped), the script warns rather
than claiming compatibility. Confirm the installed version yourself before
restoring.

After restore:

```bash
sudo systemctl status couchdb --no-pager
curl --fail --silent --show-error http://127.0.0.1:5984/_up
sudo journalctl -u couchdb -n 50 --no-pager
```

Check MyContexts and your important databases too. HTTP health alone does not
prove the application works. Keep the old directories until those checks pass;
remove only explicitly identified old directories after reviewing them.

## Overrides and limits

Use explicit environment assignments after `sudo` (sudo usually filters the
calling shell's environment):

```bash
sudo env COUCHDB_ROOT=/opt/couchdb COUCHDB_SERVICE=couchdb.service \
  COUCHDB_TIMEOUT=120 ./backupcouchdb --dest /mnt/backups/couchdb
```

| Variable | Default | Meaning |
| --- | --- | --- |
| `COUCHDB_ROOT` | `/opt/couchdb` | Installation root; base for relative data paths |
| `COUCHDB_ETC` | `$COUCHDB_ROOT/etc` | Complete configuration directory |
| `COUCHDB_SERVICE` | `couchdb.service` | systemd unit |
| `COUCHDB_URL` | `http://127.0.0.1:5984` | Local version/health endpoint |
| `COUCHDB_BACKUP_DIR` | `/var/backups/couchdb` | Default backup destination |
| `COUCHDB_TIMEOUT` | `60` | Seconds to wait for HTTP health after startup |

The service must use `KillMode=control-group`, a non-root static user, and the
normal CouchDB configuration chain. If a nonempty systemd `WorkingDirectory` is
configured, it must match `COUCHDB_ROOT`. Inspect the service locally with
`systemctl cat couchdb` before first use to confirm no custom `-c` configuration
chain or environment overrides alter these assumptions. Do not share unit or
configuration contents without removing secrets.

Only a single-server installation is supported, not Docker, a cluster-wide
snapshot, or replication-based migration. Do not run another backup utility,
manually restart CouchDB, or edit configuration during these operations. The lock
coordinates these scripts only. Symlinks and special files within copied trees
are rejected, and separate data/index directories must not overlap.

Restore accepts **backups made by this Ubuntu implementation**, not the different
manifest/config layout produced by [the Mac scripts](../localdevelopment/README.md).
It backs up CouchDB files and `etc`, not binaries, systemd units, certificates
outside `etc`, Apache, RabbitMQ, or other application services. CouchDB upgrades
or topology changes require their own recovery plan.

## Script tests

From the monorepo root:

```bash
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest discover \
  -s remotedevelopment -p 'test_*.py'
```

Tests use temporary fixture directories and simulated service operations. They
do not touch an installed CouchDB or call systemd. They cover shared/separate
indexes, configuration layering, corruption, rollback, escaped filenames and
the main options. Perform a real backup and a restore rehearsal on a disposable
matching Ubuntu CouchDB instance before relying on these for disaster recovery.
