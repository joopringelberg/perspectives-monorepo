# Public Resource Identifiers

This document describes how public resource identifiers are produced, parsed, and persisted in the PDR.

## Purpose

Public resources are context and role instances that are written for publication and shared through deltas. Their internal shape is the same as local resources; only their identifier scheme differs.

Public identifiers use the pub scheme:

- Remote endpoint form: pub:https://host/path#guid
- Local database form: pub:cw_databaseName#guid

In both forms, the database name follows the cw_ convention.

The local form is intended for tests and in-memory PouchDB usage.

## End-to-end flow

1. ARC parsing:
A public role can declare an expression with at, for example:

public Visitor at (extern >> PublicUrl) = sys:Me

The parser stores this as a PublicUrl role part.

2. Phase Two:
The parsed PublicUrl expression is copied to the enumerated PublicProxy role representation.

3. Phase Three:
The PublicUrl expression is compiled as a query and validated to produce a String domain.

4. Transaction phase:
When distributing transaction deltas for public roles, the runtime computes the publication target URL/database string and builds pub identifiers with createPublicIdentifier.

5. Persistence:
On read/write, parseResourceIdentifier splits pub identifiers into:

- Public url guid for remote publication targets
- Public dbName guid for local publication targets

The resulting database part is passed to PouchDB.

## Parsing rules

The identifier parser now accepts both forms for pub:

- pub:https://...#guid
- pub:cw_localDbName#guid

For rem identifiers, parser behavior is also tolerant of remote endpoint strings that omit a trailing slash before #.

## Offline behavior

Previously, any pub identifier was treated as internet-only.

Now, offline checks are endpoint-aware:

- If pub resolves to an HTTP(S) database endpoint, internet is required.
- If pub resolves to a local database name, internet is not required.

This allows public-resource tests to run against in-memory/local PouchDB databases.

## Validation in ARC expression literals

The publicrole/publiccontext literal parser (pubParser) validates public identifiers using hasPublicResourceShape.

That validation now accepts:

- pub:https://...#guid
- pub:cw_localDbName#guid

## Notes for test setup

- Node test builds of perspectives-core use the memory adapter for non-HTTP database names.
- A local public identifier such as pub:cw_testpublic#myGuid can therefore target an in-memory PouchDB database during tests.

## Related modules

- Perspectives.Parsing.Arc
- Perspectives.Parsing.Arc.Identifiers
- Perspectives.Parsing.Arc.PhaseTwo
- Perspectives.Parsing.Arc.PhaseThree
- Perspectives.ResourceIdentifiers
- Perspectives.ResourceIdentifiers.Parser
- Perspectives.Persistent
- Perspectives.RunMonadPerspectivesTransaction
- Perspectives.StrippedDelta
