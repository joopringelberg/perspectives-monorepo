# Model URI resolution

This document describes how the PDR resolves a model identifier (`ModelUri`) to concrete URLs for:

- the DomeinFile document;
- the repository manifest context;
- the namespace instances store (`cw_*`).

The implementation lives in `Perspectives.Identifiers` (`src/core/identifiers.purs`).

## 1. Input type and shape

`ModelUri` is a phantom-typed newtype (`Perspectives.SideCar.PhantomTypedNewtypes`):

```purescript
newtype ModelUri f = ModelUri String
```

At runtime, mapping functions work on the underlying `String`.

Expected model URI shape:

```text
model://{authority}#{localModelName}
```

Examples:

- `model://perspectives.domains#System@6.3`
- `model://joopringelberg.nl#MyModel`

Validation is done with:

```purescript
newModelPattern = "^model://([^/]+)#([^\\$/]+)$"
```

Captured groups:

1. `authority` (for example `perspectives.domains`);
2. `localModelName` (for example `System@6.3`).

## 2. Deterministic mapping algorithm

For a valid model URI:

1. Split `authority` on `.` into `namespaceParts`.
2. Use the last two parts as:
   - `secondLevel` (for example `perspectives`);
   - `toplevel` (for example `domains`).
3. Build a host base:
   - `https://{secondLevel}.{toplevel}`
4. Build a namespace token:
   - `{namespaceParts joined with "_"}`

So for `perspectives.domains`:

- host base: `https://perspectives.domains`
- namespace token: `perspectives_domains`

For `foo.bar.example.org`:

- host base: `https://example.org`
- namespace token: `foo_bar_example_org`

## 3. `modelUri2ModelUrl`

`modelUri2ModelUrl` returns:

```text
repositoryUrl = https://{secondLevel}.{toplevel}/models_{namespaceToken}
documentName  = {namespaceToken}-{localModelName}.json
```

Example:

- input: `model://perspectives.domains#System@6.3`
- output:
  - `repositoryUrl = https://perspectives.domains/models_perspectives_domains`
  - `documentName  = perspectives_domains-System@6.3.json`

Full DomeinFile URL:

```text
https://perspectives.domains/models_perspectives_domains/perspectives_domains-System@6.3.json
```

## 4. `modelUri2ManifestUrl`

`modelUri2ManifestUrl` returns:

```text
repositoryUrl = https://{secondLevel}.{toplevel}/cw_{namespaceToken}
manifestName  = {namespaceToken}-{localModelName}
```

The key difference from `modelUri2ModelUrl` is:

- repository prefix is `cw_` (not `models_`);
- no `.json` extension on `manifestName`.

## 5. Related helpers

- `modelUri2ModelRepository` returns only `https://.../models_{namespaceToken}`.
- `modelUri2InstancesStore` returns only `https://.../cw_{namespaceToken}`.
- `unversionedModelUri` strips `@version` from a URI when needed for manifest-level lookup.
- `modelUriVersion` extracts the `@version` suffix if present.

## 6. Preconditions and failure mode

These mapping functions are `Partial`: they assume a valid model URI string. Callers typically validate first (`isModelUri`) and then use `unsafePartial`.

In other words: malformed model URIs are treated as programmer/configuration errors, not as recoverable runtime mapping outcomes.

## 7. Why this matters for tests

Production resolution is DNS + naming-convention driven and maps to public CouchDB-style URLs.

For test scenarios that run on an in-memory PouchDB adapter, this deterministic public mapping is often not suitable. In those tests, model loading must be redirected to test-local storage instead of the public `models_*` / `cw_*` URLs.

This redirection requirement is exactly why understanding the deterministic mapping in `Perspectives.Identifiers` is important: all alternative test mappings are deviations from this baseline rule set.
