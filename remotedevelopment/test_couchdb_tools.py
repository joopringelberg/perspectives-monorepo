import importlib.util
import os
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch


spec = importlib.util.spec_from_file_location("tools", Path(__file__).with_name("couchdb_tools.py"))
tools = importlib.util.module_from_spec(spec)
spec.loader.exec_module(tools)


class InstallationFixture:
    def __init__(self, root, separate=False):
        self.root = root
        self.etc = root / "etc"
        self.data = root / "data"
        self.index = root / "index" if separate else self.data
        self.uid = os.getuid()
        self.gid = os.getgid()
        self.node = "couchdb@127.0.0.1"
        self.was_active = True
        self.stopped = False
        self.events = []
        for path in {self.etc, self.data, self.index}:
            path.mkdir(parents=True)
        (self.etc / "default.ini").write_text(
            "[couchdb]\ndatabase_dir=./data\nview_index_dir=./"
            + ("index" if separate else "data") + "\n")
        (self.etc / "local.ini").write_text("[admins]\n")
        (self.etc / "vm.args").write_text("-name couchdb@127.0.0.1\n")
        (self.data / "database.couch").write_text("original data")
        (self.index / ".shards").mkdir()
        (self.index / ".shards" / "view index").write_text("original index")

    def version(self):
        return "3.3.2"

    def stop(self):
        self.events.append("stop")
        self.stopped = True

    def recover_service(self, restart):
        if self.stopped and self.was_active and restart:
            self.events.append("start")
            self.stopped = False


class BackupTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()

    def make_backup(self, installation, **options):
        args = SimpleNamespace(dest=str(self.root / "backups"), online=False, no_restart=False)
        args.__dict__.update(options)
        tools.backup(installation, args)
        return next((self.root / "backups").glob("couchdb-*"))

    def restore(self, installation, backup, **options):
        args = SimpleNamespace(backup_dir=str(backup), with_config=False,
                               yes=True, force=False, no_restart=False)
        args.__dict__.update(options)
        tools.restore(installation, args)

    def test_round_trip_shared_and_separate_indexes_with_config(self):
        for separate in (False, True):
            with self.subTest(separate=separate):
                installation = InstallationFixture(self.root / str(separate), separate)
                backup = self.make_backup(installation)
                (installation.data / "database.couch").write_text("new data")
                (installation.etc / "local.ini").write_text("[admins]\nchanged=yes\n")
                self.restore(installation, backup, with_config=True)
                self.assertEqual((installation.data / "database.couch").read_text(), "original data")
                self.assertEqual((installation.etc / "local.ini").read_text(), "[admins]\n")
                self.assertEqual((installation.index / ".shards" / "view index").read_text(), "original index")
                self.assertEqual(installation.events, ["stop", "start", "stop", "start"])
                asides = list(installation.root.glob("data.before-restore-*"))
                self.assertEqual(len(asides), 1)
                self.assertEqual((asides[0] / "database.couch").read_text(), "new data")
                # Avoid selecting an earlier backup in the next subtest.
                (self.root / "backups").rename(self.root / f"backups-{separate}")

    def test_corruption_and_extra_files_rejected_before_stop(self):
        installation = InstallationFixture(self.root / "couchdb")
        backup = self.make_backup(installation)
        for path in (backup / "data" / "database.couch", backup / "data" / "unexpected"):
            with self.subTest(path=path):
                original = path.read_bytes() if path.exists() else None
                path.write_text("corruption")
                with self.assertRaises(tools.BackupError):
                    self.restore(installation, backup)
                self.assertEqual(installation.events, ["stop", "start"])
                if original is None:
                    path.unlink()
                else:
                    path.write_bytes(original)

    def test_config_checksums_and_symlinks(self):
        installation = InstallationFixture(self.root / "couchdb")
        backup = self.make_backup(installation)
        (backup / "config" / "local.ini").write_text("bad config")
        with self.assertRaises(tools.BackupError):
            self.restore(installation, backup)
        (installation.data / "link").symlink_to(installation.etc / "local.ini")
        with self.assertRaises(tools.BackupError):
            self.make_backup(installation)

    def test_online_and_no_restart_and_inactive_service(self):
        installation = InstallationFixture(self.root / "couchdb")
        self.make_backup(installation, online=True)
        self.assertEqual(installation.events, [])
        self.make_backup(installation, no_restart=True)
        self.assertEqual(installation.events, ["stop"])
        installation.was_active = False
        installation.events.clear()
        self.make_backup(installation)
        self.assertEqual(installation.events, ["stop"])

    def test_copy_failure_rolls_back(self):
        installation = InstallationFixture(self.root / "couchdb")
        backup = self.make_backup(installation)
        (installation.data / "database.couch").write_text("new data")
        with patch.object(tools, "copy_tree", side_effect=OSError("copy failed")):
            with self.assertRaises(OSError):
                self.restore(installation, backup)
        self.assertEqual((installation.data / "database.couch").read_text(), "new data")
        self.assertEqual(installation.events, ["stop", "start", "stop", "stop", "start"])

    def test_failed_start_rolls_back_before_restarting_original(self):
        installation = InstallationFixture(self.root / "couchdb")
        backup = self.make_backup(installation)
        (installation.data / "database.couch").write_text("new data")
        recover = installation.recover_service
        calls = 0

        def fail_once(restart):
            nonlocal calls
            calls += 1
            if calls == 1:
                raise tools.BackupError("health check failed")
            recover(restart)

        installation.recover_service = fail_once
        with self.assertRaises(tools.BackupError):
            self.restore(installation, backup)
        self.assertEqual((installation.data / "database.couch").read_text(), "new data")
        self.assertTrue(list(installation.root.glob("data.before-restore-*.failed")))
        self.assertFalse(installation.stopped)

    def test_changed_backup_during_restore_rolls_back(self):
        installation = InstallationFixture(self.root / "couchdb")
        backup = self.make_backup(installation)
        stop = installation.stop

        def change_source():
            stop()
            (backup / "data" / "database.couch").write_text("changed during restore")

        installation.stop = change_source
        with self.assertRaises(tools.BackupError):
            self.restore(installation, backup)
        self.assertEqual((installation.data / "database.couch").read_text(), "original data")

    def test_escaped_filenames(self):
        installation = InstallationFixture(self.root / "couchdb")
        for name in ("back\\slash", "new\nline", "carriage\rreturn"):
            (installation.data / name).write_text("test")
        backup = self.make_backup(installation)
        tools.verify(backup, ["data", "config"])
        self.restore(installation, backup)

    def test_configuration_chain(self):
        installation = InstallationFixture(self.root / "couchdb")
        (installation.etc / "default.d").mkdir()
        (installation.etc / "local.d").mkdir()
        (installation.etc / "default.d" / "10.ini").write_text("[couchdb]\ndatabase_dir=./wrong\n")
        (installation.etc / "local.ini").write_text("[couchdb]\ndatabase_dir=./also-wrong\n")
        (installation.etc / "local.d" / "10.ini").write_text("[couchdb]\ndatabase_dir=./data\n")
        self.assertEqual(tools.config_dirs(installation.root, installation.etc),
                         [installation.data, installation.data])

    def test_destination_inside_data_rejected(self):
        installation = InstallationFixture(self.root / "couchdb")
        with self.assertRaises(tools.BackupError):
            self.make_backup(installation, dest=str(installation.data / "backups"))
        self.assertEqual(installation.events, [])

    def test_version_mismatch_requires_force(self):
        installation = InstallationFixture(self.root / "couchdb")
        backup = self.make_backup(installation)
        installation.version = lambda: "3.4.0"
        with self.assertRaises(tools.BackupError):
            self.restore(installation, backup)
        self.restore(installation, backup, force=True)

    def test_space_failure_before_stopping(self):
        installation = InstallationFixture(self.root / "couchdb")
        with patch.object(tools.shutil, "disk_usage", return_value=SimpleNamespace(free=1)):
            with self.assertRaises(tools.BackupError):
                self.make_backup(installation)
        self.assertEqual(installation.events, [])

    def test_systemd_stop_checks_cgroup(self):
        installation = object.__new__(tools.Installation)
        installation.service = "couchdb.service"
        installation.stopped = False
        with patch.object(tools.subprocess, "run") as command, \
                patch.object(tools, "run", side_effect=["inactive", ""]):
            installation.stop()
            command.assert_called_once_with(["systemctl", "stop", "couchdb.service"], check=True)
        with patch.object(tools.subprocess, "run"), \
                patch.object(tools, "run", side_effect=["inactive", "/system.slice/couchdb.service"]):
            with self.assertRaises(tools.BackupError):
                installation.stop()

    def test_systemd_start_waits_for_health(self):
        installation = object.__new__(tools.Installation)
        installation.service = "couchdb.service"
        installation.timeout = 60
        installation.stopped = True
        with patch.object(tools.subprocess, "run") as command, \
                patch.object(installation, "request", return_value={"status": "ok"}):
            installation.start()
            command.assert_called_once_with(["systemctl", "start", "couchdb.service"], check=True)
        self.assertFalse(installation.stopped)


if __name__ == "__main__":
    unittest.main()
