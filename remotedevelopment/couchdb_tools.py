"""Offline file backups for a single, systemd-managed CouchDB installation."""

import argparse
import configparser
import datetime
import fcntl
import grp
import hashlib
import json
import os
from pathlib import Path
import pwd
import shutil
import signal
import subprocess
import sys
import time
import urllib.error
import urllib.request


class BackupError(Exception):
    pass


def require(condition, message):
    if not condition:
        raise BackupError(message)


def run(*args):
    return subprocess.check_output(args, text=True).strip()


def is_within(path, parent):
    return path == parent or parent in path.parents


def independent(paths):
    for i, first in enumerate(paths):
        for second in paths[i + 1:]:
            require(not is_within(first, second) and not is_within(second, first),
                    f"Directories must not overlap: {first}, {second}")


def config_dirs(root, etc):
    files = [etc / "default.ini", *sorted((etc / "default.d").glob("*.ini")),
             etc / "local.ini", *sorted((etc / "local.d").glob("*.ini"))]
    require((etc / "default.ini").is_file(), f"Missing {etc / 'default.ini'}")
    parser = configparser.ConfigParser(interpolation=None, strict=False)
    parser.read([str(file) for file in files], encoding="utf-8")
    result = []
    for key in ("database_dir", "view_index_dir"):
        require(parser.has_option("couchdb", key), f"No [couchdb] {key} configured")
        value = Path(parser.get("couchdb", key).strip())
        path = (value if value.is_absolute() else root / value).resolve()
        require(path != Path("/") and path != root and path != etc,
                f"Unsafe {key}: {path}")
        result.append(path)
    if result[0] != result[1]:
        independent(result)
    independent([result[0], etc])
    independent([result[1], etc])
    return result


def node_name(etc):
    args = etc / "vm.args"
    require(args.is_file(), f"Missing {args}")
    for line in args.read_text().splitlines():
        parts = line.split()
        if len(parts) == 2 and parts[0] in ("-name", "-sname"):
            return parts[1]
    raise BackupError(f"No Erlang node name found in {args}")


def files_in(root, trees):
    files = []
    for tree in trees:
        base = root / tree
        require(base.is_dir() and not base.is_symlink(), f"Missing directory: {base}")
        for current, dirs, names in os.walk(base):
            for name in dirs + names:
                path = Path(current) / name
                require(not path.is_symlink(), f"Symlinks are not supported: {path}")
                require(path.is_dir() or path.is_file(),
                        f"Special files are not supported: {path}")
            files.extend(Path(current) / name for name in names)
    return sorted(files)


def digest(path):
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(chunk)
    return value.hexdigest()


def inventory(root, trees):
    paths = files_in(root, trees)
    paths.append(root / "MANIFEST.json")
    return {str(path.relative_to(root)): digest(path) for path in sorted(paths)}


def write_checksums(root, sums):
    with (root / "SHA256SUMS").open("w") as stream:
        for name, checksum in sums.items():
            escaped = name.replace("\\", "\\\\").replace("\n", "\\n").replace("\r", "\\r")
            prefix = "\\" if escaped != name else ""
            stream.write(f"{prefix}{checksum}  {escaped}\n")


def read_checksums(root):
    sums = {}
    for line in (root / "SHA256SUMS").read_text().split("\n"):
        if not line:
            continue
        escaped = line.startswith("\\")
        if escaped:
            line = line[1:]
        require(len(line) >= 67 and line[64:66] == "  ",
                "Invalid SHA256SUMS entry")
        checksum, name = line[:64], line[66:]
        require(all(char in "0123456789abcdef" for char in checksum),
                "Invalid SHA256SUMS digest")
        if escaped:
            decoded = ""
            i = 0
            while i < len(name):
                if name[i] == "\\":
                    i += 1
                    require(i < len(name) and name[i] in ("\\", "n", "r"),
                            "Invalid escaped checksum filename")
                    decoded += {"n": "\n", "r": "\r", "\\": "\\"}[name[i]]
                else:
                    decoded += name[i]
                i += 1
            name = decoded
        require(name not in sums, f"Duplicate checksum: {name}")
        sums[name] = checksum
    return sums


def verify(root, trees):
    for name in ("MANIFEST.json", "SHA256SUMS"):
        path = root / name
        require(path.is_file() and not path.is_symlink(), f"Not a regular file: {path}")
    require(inventory(root, trees) == read_checksums(root),
            f"Backup files or checksums differ: {root}")


def space_for(destination, size):
    require(shutil.disk_usage(destination).free >= size + 100 * 1024 * 1024,
            f"Insufficient space at {destination}: need {size // (1024 * 1024)} "
            "MiB plus a 100 MiB margin")


class Installation:
    def __init__(self):
        self.root = Path(os.environ.get("COUCHDB_ROOT", "/opt/couchdb")).resolve()
        self.etc = Path(os.environ.get("COUCHDB_ETC", str(self.root / "etc"))).resolve()
        self.url = os.environ.get("COUCHDB_URL", "http://127.0.0.1:5984").rstrip("/")
        self.service = os.environ.get("COUCHDB_SERVICE", "couchdb.service")
        self.timeout = int(os.environ.get("COUCHDB_TIMEOUT", "60"))
        require(self.timeout > 0, "COUCHDB_TIMEOUT must be positive")
        require(self.service and not self.service.startswith("-"), "Invalid service name")
        properties = run("systemctl", "show", self.service,
                         "--property=LoadState,ActiveState,User,Group,KillMode,WorkingDirectory,ControlGroup")
        self.properties = dict(line.split("=", 1) for line in properties.splitlines())
        require(self.properties["LoadState"] == "loaded", f"Service not loaded: {self.service}")
        require(self.properties["ActiveState"] in ("active", "inactive", "failed"),
                "Service is transitioning; retry after it has settled")
        require(self.properties["KillMode"] == "control-group",
                "CouchDB service must use KillMode=control-group")
        working = self.properties["WorkingDirectory"]
        require(not working or Path(working).resolve() == self.root,
                "COUCHDB_ROOT must match the service WorkingDirectory")
        user = self.properties["User"]
        require(bool(user) and user != "root", "Service must use a non-root CouchDB account")
        account = pwd.getpwnam(user)
        self.uid = account.pw_uid
        self.gid = grp.getgrnam(self.properties["Group"]).gr_gid if self.properties["Group"] else account.pw_gid
        self.data, self.index = config_dirs(self.root, self.etc)
        self.node = node_name(self.etc)
        self.was_active = self.properties["ActiveState"] == "active"
        self.stopped = False

    def request(self, endpoint):
        with urllib.request.urlopen(self.url + endpoint, timeout=2) as response:
            return json.load(response)

    def version(self):
        try:
            return self.request("/")["version"]
        except (urllib.error.URLError, TimeoutError) as error:
            print(f"Warning: cannot obtain CouchDB version: {error}", file=sys.stderr)
            return "unknown"

    def stop(self):
        # Set this first so a partial stop failure still attempts service recovery.
        self.stopped = True
        subprocess.run(["systemctl", "stop", self.service], check=True)
        state = run("systemctl", "show", self.service, "--property=ActiveState", "--value")
        require(state in ("inactive", "failed"), "CouchDB has not stopped")
        require(not run("systemctl", "show", self.service, "--property=ControlGroup", "--value"),
                "CouchDB service still has a control group; refusing to copy files")

    def start(self):
        subprocess.run(["systemctl", "start", self.service], check=True)
        deadline = time.monotonic() + self.timeout
        last_error = None
        while time.monotonic() < deadline:
            try:
                if self.request("/_up").get("status") == "ok":
                    print("CouchDB is up again.")
                    self.stopped = False
                    return
            except (urllib.error.URLError, TimeoutError) as error:
                last_error = error
            time.sleep(1)
        raise BackupError(f"CouchDB did not become healthy within {self.timeout}s: {last_error}")

    def recover_service(self, restart):
        if self.stopped and self.was_active and restart:
            self.start()
        elif self.stopped:
            print(f"CouchDB left stopped. Start it with: sudo systemctl start {self.service}")


def copy_tree(source, target):
    shutil.copytree(source, target, symlinks=True)


def backup(installation, args):
    dest = Path(args.dest).resolve()
    for source in (installation.data, installation.index, installation.etc):
        require(not is_within(dest, source), f"Backup destination must be outside {source}")
    sources = {"data": installation.data, "config": installation.etc}
    if installation.index != installation.data:
        sources["index"] = installation.index
    size = sum(path.stat().st_size for source in sources.values()
               for path in files_in(source.parent, [source.name]))
    dest.mkdir(parents=True, exist_ok=True, mode=0o700)
    space_for(dest, size)
    stamp = datetime.datetime.now(datetime.timezone.utc).strftime("%Y%m%d-%H%M%S-%f")
    target = dest / f"couchdb-{stamp}"
    work = dest / f".incomplete-couchdb-{stamp}"
    work.mkdir(mode=0o700)
    version = installation.version()
    try:
        if not args.online:
            print("Stopping CouchDB...")
            installation.stop()
        else:
            print("Warning: online backups are not mutually consistent and are not "
                  "verified against the source.", file=sys.stderr)
        # Copy secondary indexes before database files, including shared .shards.
        if "index" in sources:
            copy_tree(sources["index"], work / "index")
            copy_tree(sources["data"], work / "data")
        else:
            (work / "data").mkdir()
            entries = sorted(installation.data.iterdir(),
                             key=lambda path: (path.name != ".shards", path.name))
            for entry in entries:
                output = work / "data" / entry.name
                if entry.is_dir():
                    copy_tree(entry, output)
                else:
                    shutil.copy2(entry, output, follow_symlinks=False)
        copy_tree(installation.etc, work / "config")
        manifest = {
            "format": 1, "created": datetime.datetime.now(datetime.timezone.utc).isoformat(),
            "mode": "online" if args.online else "stopped", "version": version,
            "node": installation.node, "host": run("hostname"),
            "database_dir": str(installation.data), "view_index_dir": str(installation.index),
        }
        (work / "MANIFEST.json").write_text(json.dumps(manifest, indent=2) + "\n")
        sums = inventory(work, sources.keys())
        write_checksums(work, sums)
        verify(work, sources.keys())
        if not args.online:
            for name, checksum in sums.items():
                parts = Path(name).parts
                if parts[0] in sources:
                    source = sources[parts[0]].joinpath(*parts[1:])
                    require(digest(source) == checksum, f"Source comparison failed: {source}")
            for tree, source in sources.items():
                expected = {str(path.relative_to(source)) for path in files_in(source.parent, [source.name])}
                copied = {str(path.relative_to(work / tree)) for path in files_in(work, [tree])}
                require(expected == copied, f"Source file inventory changed: {source}")
        work.rename(target)
        print(f"Backup written to {target}")
    finally:
        if work.exists():
            print(f"Backup failed; incomplete files remain in {work}", file=sys.stderr)
        installation.recover_service(not args.no_restart)


def restore(installation, args):
    source = Path(args.backup_dir).resolve()
    require(source.is_dir() and not source.name.startswith(".incomplete-"),
            f"Not a completed backup: {source}")
    require((source / "MANIFEST.json").is_file(),
            "Expected an Ubuntu backup made by remotedevelopment/backupcouchdb")
    trees = ["data", "config"]
    if (source / "index").exists():
        trees.append("index")
    verify(source, trees)
    checksums = read_checksums(source)
    manifest = json.loads((source / "MANIFEST.json").read_text())
    require(manifest["format"] == 1, "Unsupported backup format")
    require(("index" in trees) == (installation.index != installation.data),
            "Backup and installation must use the same shared/separate index layout")
    for label, current, saved in (
        ("node name", installation.node, manifest["node"]),
        ("CouchDB version", installation.version(), manifest["version"]),
    ):
        if current == "unknown" or saved == "unknown":
            print(f"Warning: cannot compare {label}", file=sys.stderr)
        elif current != saved:
            require(args.force, f"Backup {label} {saved} differs from {current}; use --force to override")
            print(f"Warning: overriding {label} mismatch ({saved} / {current})", file=sys.stderr)
    targets = {"data": installation.data}
    if "index" in trees:
        targets["index"] = installation.index
    if args.with_config:
        require(config_dirs(installation.root, source / "config") ==
                [installation.data, installation.index],
                "Backup configuration specifies different data directories")
        require(node_name(source / "config") == installation.node,
                "Backup configuration changes the node name; restore it manually")
        targets["config"] = installation.etc
    for target in (installation.data, installation.index, installation.etc):
        independent([source, target])
    for tree, target in targets.items():
        require(target.parent.is_dir() and target.is_dir(), f"Target directory not found: {target}")
        # Aggregate all copies sharing a filesystem.
        device = target.parent.stat().st_dev
        size = sum(path.stat().st_size for name, directory in targets.items()
                   if directory.parent.stat().st_dev == device
                   for path in files_in(source, [name]))
        space_for(target.parent, size)
    suffix = ".before-restore-" + datetime.datetime.now(datetime.timezone.utc).strftime("%Y%m%d-%H%M%S-%f")
    asides = {tree: Path(str(target) + suffix) for tree, target in targets.items()}
    require(all(not aside.exists() for aside in asides.values()), "Restore aside already exists")
    print(f"Restore {source} ({manifest['created']}, {manifest['mode']}).")
    print("All changes since this backup will be lost from CouchDB.")
    print(f"Previous directories will be kept with suffix {suffix}.")
    if manifest["mode"] != "stopped":
        print("Warning: online backup databases may be mutually inconsistent.", file=sys.stderr)
    if not args.yes:
        require(sys.stdin.isatty(), "Non-interactive restore requires --yes")
        require(input("Type 'restore' to continue: ") == "restore", "Aborted; nothing changed")
    moved = []
    try:
        installation.stop()
        for tree, target in targets.items():
            target.rename(asides[tree])
            moved.append(tree)
            copy_tree(source / tree, target)
            restored = files_in(target.parent, [target.name])
            expected = {
                str(Path(name).relative_to(tree)): checksum
                for name, checksum in checksums.items()
                if Path(name).parts[0] == tree
            }
            actual = {str(path.relative_to(target)): digest(path) for path in restored}
            require(actual == expected, f"Restored file inventory or checksums differ: {target}")
            for current, dirs, names in os.walk(target):
                os.chown(current, installation.uid, installation.gid)
                for name in names:
                    os.chown(Path(current) / name, installation.uid, installation.gid)
        installation.recover_service(not args.no_restart)
    except BaseException:
        if moved:
            print("Restore failed; stopping CouchDB before rollback...", file=sys.stderr)
            installation.stop()
            for tree in reversed(moved):
                target = targets[tree]
                if target.exists():
                    # Keep the failed copy too; never recursively delete live data.
                    target.rename(Path(str(target) + suffix + ".failed"))
                asides[tree].rename(target)
            print("Original files restored; failed copies kept alongside them.", file=sys.stderr)
        installation.recover_service(not args.no_restart)
        raise
    print("Restore completed. Keep the previous files until application checks pass.")


def interrupted(signum, frame):
    raise BackupError(f"Interrupted by signal {signum}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    backup_parser = commands.add_parser("backup", description="Copy CouchDB data, indexes and configuration.")
    backup_parser.add_argument("--dest", default=os.environ.get("COUCHDB_BACKUP_DIR", "/var/backups/couchdb"))
    backup_parser.add_argument("--online", action="store_true", help="Copy without stopping; not a consistent snapshot")
    backup_parser.add_argument("--no-restart", action="store_true", help="Leave CouchDB stopped")
    restore_parser = commands.add_parser("restore", description="Verify and restore an Ubuntu file backup.")
    restore_parser.add_argument("backup_dir")
    restore_parser.add_argument("--with-config", action="store_true", help="Also replace the complete etc directory")
    restore_parser.add_argument("--no-restart", action="store_true")
    restore_parser.add_argument("--yes", action="store_true", help="Skip interactive confirmation")
    restore_parser.add_argument("--force", action="store_true", help="Override version/node mismatches")
    args = parser.parse_args()
    require(os.geteuid() == 0, "Run with sudo (service control and file ownership require root)")
    os.umask(0o077)
    signal.signal(signal.SIGTERM, interrupted)
    signal.signal(signal.SIGINT, interrupted)
    lock_dir = Path("/run/perspectives-couchdb-backup")
    lock_dir.mkdir(mode=0o700, exist_ok=True)
    require(not lock_dir.is_symlink() and lock_dir.stat().st_uid == 0
            and lock_dir.stat().st_mode & 0o077 == 0,
            f"Lock directory must be root-owned and private: {lock_dir}")
    with (lock_dir / "lock").open("a") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as error:
            raise BackupError("Another backup/restore is running") from error
        installation = Installation()
        if args.command == "backup":
            backup(installation, args)
        else:
            restore(installation, args)


if __name__ == "__main__":
    try:
        main()
    except (BackupError, OSError, ValueError, KeyError, configparser.Error,
            subprocess.CalledProcessError) as error:
        print(f"Error: {error}", file=sys.stderr)
        sys.exit(1)
