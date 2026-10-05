perspectives-distributed-runtime
======================

### About
The Perspectives Distributed Runtime (PDR) is part of the software created in the course of the [Perspectives Project](https://academy.perspect.it).

The PDR interprets models written in the Perspectives Language. **Patterns of co-operation** can easily be expressed in PL. Examples of co-operation are: buying and selling, renting stuff, a formal meeting, etc.

The PDR has an API for client programs that offer end users screens to interact with each other. Users of such programs exchange information directly, in a Peer-to-Peer fashion, without intermediate servers that store their information.

### Getting started
The PDR is work in progress. It is not yet in a state that it can be used. Hence, we provide no instructions on how to use it right now. However, [here](./technical%20readme.md) is a page with instructions for developers of the PDR.

### Contributing
The PDR is being intensively developed by the core team. We appreciate feedback on the code you can find in this repository

### Dependencies
#### On Purescript packages belonging to the Perspectives Project
* perspectives-couchdb
* purescript-avar-monadask
* purescript-aff-sockets
* perspectives-apitypes
* perspectives-lru-cache
* perspectives-utilities
* serialisable-nonempty-arrays


#### On Node packages belonging to the Perspectives Project
* perspectives-proxy

### Packages belonging to the project but no core dependencies
* perspectives-react
* perspectives-react-integrated-client
* perspectives-documentation
* perspectives-screens
* screenuploader

### License information
This project is available as open source under the terms of the GPL-3.0-or-later license. For accurate information, please check individual files.


## Docs

- Stable ID mapping sidecar: see `docs/stable-id-mapping.md` for how aliases and snapshots are applied between Phase Two and Phase Three.

## Recompile a local model into its repository

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
