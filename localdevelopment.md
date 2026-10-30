## Local Development

### SSL Certificates
For local development, you need to create self-signed certificates:

1. Create a `certificates` directory in the project root
2. Generate self-signed certificates, e.g. using mkcert.

```
# First create the certificates directory if it doesn't exist
mkdir -p {directory holding your cloned repo}/perspectives-monorepo/certificates

# Navigate to the certificates directory
cd {directory holding your cloned repo}/perspectives-monorepo/certificates

# Create certificates for localhost and other domains you might use
mkcert localhost 127.0.0.1 ::1 perspectives.local *.perspectives.local
```

### Local repository domains

See [Local Perspectives domains on macOS](localdevelopment/README.md) to route
`perspectives.domains` and `joopringelberg.nl` to a local CouchDB, serve a local
MyContexts build through `mycontexts.com`, and switch safely between local and
remote services.