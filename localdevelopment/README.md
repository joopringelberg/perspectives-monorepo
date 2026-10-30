# Local Perspectives domains on macOS

This directory contains Apache virtual hosts and commands for switching
`perspectives.domains`, `joopringelberg.nl`, and `mycontexts.com` between local
services and their public DNS destinations.

Local mode resolves all three domains to `127.0.0.1`. MAMP Apache terminates HTTPS
and proxies repository requests to CouchDB at `http://127.0.0.1:5984`.
`mycontexts.com/www/` serves the experimental build in
`packages/mycontexts/dist/`.

## Prerequisites

- macOS
- MAMP installed in `/Applications/MAMP`
- CouchDB listening on `127.0.0.1:5984`
- Homebrew, for installing `mkcert`
- Administrator access, for MAMP configuration and `/etc/hosts`

The configuration expects MAMP Apache. Confirm that it is the running server:

```bash 
ps -axo pid,user,command | grep '[h]ttpd'
```

The process path should start with `/Applications/MAMP/Library/bin/httpd`.

## 1. Create locally trusted certificates

Install `mkcert` and its local certificate authority:

```bash
brew install mkcert
mkcert -install
```

From the repository root, generate one certificate for all three domains:

```bash
mkdir -p localdevelopment/certificates

mkcert \
  -cert-file localdevelopment/certificates/local-domains.pem \
  -key-file localdevelopment/certificates/local-domains-key.pem \
  perspectives.domains \
  '*.perspectives.domains' \
  joopringelberg.nl \
  '*.joopringelberg.nl' \
  mycontexts.com \
  '*.mycontexts.com'
```

The `certificates` directory is ignored by Git. Never commit the private key.

The Apache configurations use stable MAMP paths so they work regardless of
where a developer clones the repository. Link the generated files into MAMP:

```bash
sudo ln -s \
  "$PWD/localdevelopment/certificates/local-domains.pem" \
  /Applications/MAMP/conf/apache/ssl/perspectives-local-domains.pem

sudo ln -s \
  "$PWD/localdevelopment/certificates/local-domains-key.pem" \
  /Applications/MAMP/conf/apache/ssl/perspectives-local-domains-key.pem
```

Run these commands from the repository root. If a destination already exists,
inspect it with `ls -l` before replacing it.

`mkcert -install` adds the local CA to the macOS Keychain until it is removed
with `mkcert -uninstall`.

## 2. Enable the virtual hosts in MAMP

Link the repository configurations into MAMP's automatically included `other`
directory:

```bash
sudo ln -s \
  "$PWD/localdevelopment/apacheconfigs/perspectives.domains.conf" \
  /Applications/MAMP/conf/apache/other/perspectives.domains.conf

sudo ln -s \
  "$PWD/localdevelopment/apacheconfigs/joopringelberg.nl.conf" \
  /Applications/MAMP/conf/apache/other/joopringelberg.nl.conf

sudo ln -s \
  "$PWD/localdevelopment/apacheconfigs/mycontexts.com.conf" \
  /Applications/MAMP/conf/apache/other/mycontexts.com.conf

ln -s \
  "$PWD/packages/mycontexts/dist" \
  /Applications/MAMP/htdocs/mycontexts-www
```

Run these commands from the repository root. The last link gives Apache a
stable document path while keeping the executable files in the package build
directory.

In `/Applications/MAMP/conf/apache/httpd.conf`, enable `mod_rewrite` by
uncommenting this existing line:

```apache
LoadModule rewrite_module modules/mod_rewrite.so
```

Ensure that `headers_module`, `proxy_module`, and `proxy_http_module` are also
enabled. Add these listeners if they are not already present:

```apache
Listen 443
Listen 127.0.0.1:5987
```

Port `5987` is an administrative CouchDB proxy and is deliberately bound only
to loopback.

Validate the configuration:

```bash
/Applications/MAMP/Library/bin/apachectl -t
/Applications/MAMP/Library/bin/apachectl -t -D DUMP_VHOSTS
```

Restart MAMP Apache to activate listener changes:

```bash
sudo /Applications/MAMP/bin/stopApache.sh
sudo /Applications/MAMP/bin/startApache.sh
```

Both repository virtual hosts add `Secure`, `SameSite=None`, and `Partitioned`
to CouchDB session cookies so browser requests from `mycontexts.com` can use
them cross-site. Without `Secure`, browsers reject `SameSite=None` cookies:
`POST /_session` can succeed while subsequent database requests remain
unauthenticated. Validate and reload Apache after changing these cookie headers.

## 3. Build and serve MyContexts

Build the experimental executable with the same `/www/` base path used by the
remote deployment:

```bash
cd packages/mycontexts
pnpm run build:www
```

The build replaces the contents of `packages/mycontexts/dist/`. No copy or
Apache restart is needed because `/Applications/MAMP/htdocs/mycontexts-www` is
a symbolic link to that directory.

Open the local build at:

```text
https://mycontexts.com/www/
```

The root URL `https://mycontexts.com/` redirects temporarily to `/www/`.
Requests under `https://mycontexts.com/models/` are read-only proxies to the
local CouchDB `models` database. Repository replication is still not
configured for local development. RabbitMQ and its two relay services
(`/rbmq/`, `/rbsr`, `/ppsfs`) are configured; see the next section.

## 4. Switch to local services

From the repository root, run:

```bash
./localdevelopment/servefromlocal
```

The command:

- removes older manual mappings for the managed domains;
- adds a marked block mapping all three domains and their `www` names to
  `127.0.0.1`;
- flushes the macOS DNS caches.

It requests administrator authentication because `/etc/hosts` is owned by
root. Running it repeatedly does not create duplicate entries.

Verify local resolution and HTTPS:

```bash
dscacheutil -q host -a name perspectives.domains
/usr/bin/curl https://perspectives.domains/_up
/usr/bin/curl https://joopringelberg.nl/_session
/usr/bin/curl -I https://mycontexts.com/www/
```

The CouchDB health endpoint should return HTTP 200 and a JSON response with
`"status":"ok"`.

Use `/usr/bin/curl` for these checks. MAMP's bundled curl uses a separate CA
bundle and does not automatically trust the CA installed in the macOS Keychain.
It can be used explicitly with:

```bash
/Applications/MAMP/Library/bin/curl \
  --cacert "$(mkcert -CAROOT)/rootCA.pem" \
  https://perspectives.domains/_up
```

### Node.js tests

Node.js does not use the macOS Keychain trust store by default. Supply the
mkcert root CA before starting a Node-based test that accesses the local HTTPS
domains:

```bash
NODE_EXTRA_CA_CERTS="$(mkcert -CAROOT)/rootCA.pem" pnpm run test:rebootUniverse
```

The environment variable is read when Node starts, so setting it from inside a
running test is too late. It augments Node's normal public CA set; it does not
replace it.

Browsers may continue to use an older service-worker cache after rebuilding
MyContexts. For test runs that must use the latest executable, clear the site
data or unregister the service worker for `mycontexts.com` in the browser's
developer tools before reloading.

## 5. RabbitMQ and local relay services

RabbitMQ and its two Node relay services can be run locally instead of relying
on the remote `mycontexts.com` server. This mirrors the production setup
documented in the RabbitMQ install notes (see the wiki page on the UpCloud
server configuration).

### Install and configure RabbitMQ

```bash
brew install rabbitmq
brew services start rabbitmq
rabbitmq-plugins enable rabbitmq_management rabbitmq_web_stomp
```

Create the `mycontexts` virtual host and an administrator account:

```bash
rabbitmqctl add_vhost mycontexts
rabbitmqctl add_user joopring <password>
rabbitmqctl set_permissions -p / joopring ".*" ".*" ".*"
rabbitmqctl set_permissions -p mycontexts joopring ".*" ".*" ".*"
rabbitmqctl set_user_tags joopring administrator
```

The management API listens on `127.0.0.1:15672` by default, matching the
`/rbmq/` proxy in `apacheconfigs/mycontexts.com.conf`.

### Start the relay services

Both relay services now live in this monorepo:

- [`packages/perspectives-rabbitmq-service`](../packages/perspectives-rabbitmq-service)
  registers new RabbitMQ users on behalf of MyContexts clients, and is proxied
  at `/rbsr` (default port `5988`).
- [`packages/perspectives-sharedfilestorage`](../packages/perspectives-sharedfilestorage)
  relays shared media file uploads to Mega.nz, and is proxied at `/ppsfs`
  (default port `15680`).

For each package, copy `startService.example.sh` to `startService.sh` (both
are gitignored) and fill in real credentials, then run it:

```bash
cd packages/perspectives-rabbitmq-service
cp startService.example.sh startService.sh   # fill in RabbitMQ admin credentials
./startService.sh
```

```bash
cd packages/perspectives-sharedfilestorage
cp startService.example.sh startService.sh   # fill in Mega.nz credentials
cp providedkeys.example.json providedkeys.json
./startService.sh
```

Verify with:

```bash
/usr/bin/curl -u joopring:<password> https://mycontexts.com/rbmq/api/overview
/usr/bin/curl -X POST https://mycontexts.com/rbsr -d '{"userName":"aap","password":"noot","queueName":"mies"}'
```

## 6. Check the current mode

The hosts file is the persistent source of truth. Check it from the repository
root with:

```bash
./localdevelopment/servefromwhere
```

The command prints `local` when all six managed hostnames resolve to loopback,
`remote` when none are overridden, and `mixed` when the hosts file contains a
partial or conflicting configuration.

## 7. Switch back to remote services

Run:

```bash
./localdevelopment/servefromremote
```

This removes the marked block and any older manual mappings for the six
managed hostnames, then flushes the macOS DNS caches. It does not stop Apache or
CouchDB; public DNS simply becomes authoritative again.

Verify that the domain no longer resolves to loopback:

```bash
dscacheutil -q host -a name perspectives.domains
```

If a browser retains an old connection or DNS result after switching, close and
reopen the browser before testing again.

Be deliberate when entering remote mode: applications using these domain names
can then reach the production repositories again.

## 8. Back up the local CouchDB files

`backupcouchdb` makes a file-level backup of the local
`Apache CouchDB 3.app` installation, following the
[CouchDB backup guidance](https://docs.couchdb.org/en/stable/maintenance/backups.html).
From the repository root:

```bash
./localdevelopment/backupcouchdb
```

By default the command:

- reads `database_dir` and `view_index_dir` from the app's `default.ini` and
  `~/Library/Preferences/couchdb2-local.ini`;
- quits the CouchDB menu-bar app and stops its detached Erlang VM gracefully
  with `SIGTERM`, waiting until it has exited;
- copies the secondary indexes (`.shards`) before the database files
  (`shards/` and the system databases such as `_users.couch` and `_dbs.couch`);
- copies the app's `etc/` directory and the local ini;
- verifies the copy against the stopped source with SHA-256 checksums;
- restarts CouchDB and waits for `/_up`, also when the backup fails.

Clients such as a running PDR or MyContexts lose their CouchDB connection
while the server is stopped. Use `--online` to copy without stopping CouchDB;
that backup is not compared with the source and its databases are not
guaranteed to be mutually consistent. Other options are `--dest DIR` and
`--no-restart`; see `--help`.

Backups are written to `~/CouchDBBackups/couchdb-<timestamp>/`, unless another
directory is given, and contain `data/` (and `index/` if the index directory is
separate), `config/`, `SHA256SUMS`, and `MANIFEST.txt`. An interrupted backup
remains in a `.incomplete-couchdb-<timestamp>` directory. Backups are readable
only by the current user, because the configuration contains secrets; keep them
outside the repository.

### Restore a backup

```bash
./localdevelopment/restorecouchdb ~/CouchDBBackups/couchdb-<timestamp>
```

The command:

- verifies the backup against its `SHA256SUMS` and refuses incomplete or
  corrupt backups;
- refuses a backup whose Erlang node name or CouchDB version (from
  `MANIFEST.txt`) differs from the current installation, unless `--force` is
  given;
- shows what will be replaced and asks you to type `restore` (`--yes` skips
  this);
- stops CouchDB as `backupcouchdb` does;
- moves the current `database_dir` (and a separate `view_index_dir`) aside to
  `<dir>.before-restore-<timestamp>`, copies the backup into place, and
  verifies the copied files;
- puts the original directories back if anything fails after the move;
- restarts CouchDB and waits for `/_up` (unless `--no-restart`).

All changes made since the backup are lost. The local ini is only restored
with `--with-config` (the current one is kept as
`<ini>.before-restore-<timestamp>`); the app's `etc/` directory is never
restored automatically. Remove the `.before-restore-*` directories once
CouchDB works as expected.

## 9. Recompile a local model into its repository

From this package directory, run:

```sh
pnpm run recompile:model 'model://perspectives.domains#tiodn6tcyc' src/model/system@6.3.arc
```

The first argument is the **unversioned stable ModelUri**, not the readable model
name. The repository is derived from that URI; the version is read from the
local ARC file's `domain` declaration (not its filename). Both must identify the
same repository. The local file and repository database must already exist;
the tool never creates the target repository. The versioned model document may
be new or already exist.

For a protected repository, set `PDR_REPOSITORY_USERNAME` and
`PDR_REPOSITORY_PASSWORD` in the environment. For locally trusted HTTPS
certificates, also set `NODE_EXTRA_CA_CERTS` to the CA certificate path.

The tool uses the same cached Alice PDR snapshot as the model-file compilation
test, creating it on first use. Model dependencies must be available to this
PDR. Existing releases retain their stable IDs and non-compiler attachments
(including translations); an existing release without a valid stable-ID
mapping is rejected. The DomeinFile, stored queries, stable-ID mapping and
model-dependency sidecars are saved in a single revision-checked write, so a
concurrent repository change causes a conflict rather than being overwritten.
New documents receive an empty translation table.

This is an explicit developer recompile/overwrite tool, separate from normal
immutable-release publishing. It changes no Perspectives administration:
manifests, dependencies on manifests, version selection and `Build` are untouched.
Failures are reported on stderr with a nonzero exit code.

Run the focused regression tests with `pnpm run test:recompileModelFromFile`.
They use a loopback HTTP mock and do not modify any live repository.
