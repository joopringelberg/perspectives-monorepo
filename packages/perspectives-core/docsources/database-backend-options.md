# Research: alternative database backends for Perspectives

## Context and scope

This note analyses the backend requirements that are visible in:

- `Perspectives.Couchdb`
- `Perspectives.Couchdb.Revision`
- `Perspectives.Persistence.CouchdbFunctions`
- `Perspectives.Persistence.API`

and the directly related FFI implementation in `src/core/persistence/persistenceAPI.js`.

Goal: identify realistic non-CouchDB backend directions for:

1. local on-device storage in the browser,
2. cloud storage/backup,
3. incremental backup/sync,
4. public/shared data (public read, restricted write).

---

## Backend functionality currently required

From the analysed modules, Perspectives currently depends on these capabilities:

### 1) Document store primitives

- Document CRUD (`get`, `put`, `remove`).
- Revision/version-based concurrency (`_rev`, conflict handling, revision lookup).
- Bulk writes/deletes (`bulkDocs` semantics).
- Range/list queries over document IDs (`allDocs`, `startkey`, `endkey`).

### 2) Attachments

- Binary attachments per document (`putAttachment`, `getAttachment`).
- Attachment metadata preservation.

### 3) Query/index mechanism

- Design-document style views (`map`, optional `reduce`).
- Query by view name with key / keys / no-key variants.
- View index lifecycle operations (cleanup/reset) for recovery/maintenance.

### 4) Replication and incremental copy

- One-shot replication between two stores.
- Continuous replication for backups.
- Incremental resume from a checkpoint (`last_seq`-style concept).

### 5) Change tracking and conflict handling

- Change stream/feed support.
- Conflict inspection and conflict resolution path.
- “Hard reset” recovery behavior (purge + clean save equivalent).

### 6) Administrative and shared/public support

- Remote DB provisioning/existence checks.
- Authentication support for remote endpoints.
- Public/shared database use case:
  - anonymous or URL-based read access,
  - credentialed write access for selected users/roles.

### 7) CouchDB-specific (can be replaced by equivalent mechanisms)

- `_design/*` views.
- `_security`, `_users`, `_replicator` administrative endpoints.
- HTTP ETag-based revision lookup.

These are implementation-specific forms, not conceptual requirements.

---

## Candidate alternatives

### Option A — RxDB in browser + Supabase (PostgreSQL + Storage) in cloud (**most promising**)

### Why this fits

- **Local-first browser runtime**: RxDB runs on IndexedDB and offers document semantics close to current usage.
- **Incremental replication model**: RxDB replication has checkpoint-based pull/push flows that map to periodic backup.
- **Cloud economics and EU hosting**: Supabase offers EU regions and does not force a “many CouchDB databases per customer” model.
- **Attachments/files**: keep metadata in Postgres and file payloads in Supabase Storage (S3-compatible object storage).
- **Shared/public data**: implement with Postgres row-level security and signed/public URLs for read, with authenticated write policies.

### Gaps to bridge

- CouchDB map/reduce views must be translated to:
  - SQL indexes + SQL queries,
  - or materialized views where needed.
- Conflict model changes from `_rev` to custom version/vector/checkpoint strategy in replication endpoint.
- Requires a thin sync API service between RxDB clients and Postgres.

### Option B — ElectricSQL + Postgres

### Why it is interesting

- Strong Postgres foundation and local-first sync story.
- Good fit if the long-term goal is SQL-first querying and fewer ad-hoc view definitions.

### Main caveats

- Heavier migration than Option A for current PouchDB-shaped API surface.
- Attachment and public URL patterns still need separate design.

### Option C — Firestore + Cloud Storage

### Why it can work

- Mature managed service with offline-capable client SDK.
- Straightforward binary file storage via companion storage service.

### Main caveats

- Query model differs strongly from current design-doc view usage.
- Shared/public read + role-based write is possible but policy design and vendor lock-in trade-offs are larger.

---

## Recommended direction

Proceed with **Option A: RxDB + Supabase**.

It preserves a document-oriented, browser-first developer model while moving cloud persistence away from CouchDB SaaS constraints and toward a broadly available EU-hosted relational backend with object storage.

---

## Rough migration plan (incremental)

### Phase 0 — Stabilize abstractions

1. Introduce a backend-agnostic persistence interface in `perspectives-core` for:
   - CRUD,
   - attachments,
   - query operations,
   - replication checkpoint operations.
2. Keep current PouchDB/CouchDB behavior behind a “legacy adapter”.

### Phase 1 — Local backend replacement behind adapter

1. Add an RxDB adapter that implements the same interface for local IndexedDB usage.
2. Keep all higher-level runtime logic unchanged; only swap implementation wiring.
3. Validate parity on:
   - document lifecycle,
   - attachments,
   - query results formerly produced by core views.

### Phase 2 — Cloud backup service

1. Build a small sync service (Supabase Edge Function or separate service) with:
   - checkpointed pull/push APIs,
   - idempotent upsert semantics,
   - conflict detection metadata.
2. Add periodic background sync (same cadence as current backup flow).
3. Add resume-from-checkpoint and retry logic.

### Phase 3 — Shared/public database behavior

1. Model “public role visibility” as explicit publishable rows/documents.
2. Apply RLS policies:
   - read policy for public/signed access,
   - write policy for credentialed roles only.
3. Store attachments in object storage with URL policy aligned to row visibility.

### Phase 4 — Data migration and dual-run

1. Export existing Pouch/Couch data (JSON + attachments).
2. Import into Postgres + object storage with deterministic ID mapping.
3. Run dual-write (or dual-sync) for a validation period.
4. Compare key invariants (counts, hashes, selected query outputs, restore behavior).

### Phase 5 — Cutover

1. Enable new backend by default for new installations.
2. Keep legacy adapter only for migration/rollback window.
3. Remove CouchDB-specific admin flows once no longer needed.

---

## Risks and mitigations

- **View parity risk**: build an explicit mapping catalogue from each current view to SQL query/index definition.
- **Conflict semantics drift**: define deterministic conflict resolution rules before production rollout.
- **Attachment consistency**: use transactional metadata updates and background reconciliation jobs.
- **Operational complexity**: start with backup-only remote sync, then expand to primary-cloud operation.
