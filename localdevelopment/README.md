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
local CouchDB `models` database. RabbitMQ, repository replication, and the
other production-only proxy endpoints are deliberately not configured.

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

## 5. Check the current mode

The hosts file is the persistent source of truth. Check it from the repository
root with:

```bash
./localdevelopment/servefromwhere
```

The command prints `local` when all six managed hostnames resolve to loopback,
`remote` when none are overridden, and `mixed` when the hosts file contains a
partial or conflicting configuration.

## 6. Switch back to remote services

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
