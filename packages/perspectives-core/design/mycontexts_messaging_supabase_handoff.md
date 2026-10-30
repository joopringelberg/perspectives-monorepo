# MyContexts / Perspectives — Messaging, Supabase and Storage Architecture Handoff

**Status:** design exploration, not an implementation decision. **Prepared:** 2026-10-28.

## Context and goals

MyContexts is a distributed, local-first application built on the Perspectives Distributed Runtime (PDR), implemented largely in PureScript. Today `perspectives-core` runs in a browser `SharedWorker`; browser pages use `perspectives-proxy` and `perspectives-react`. Each installation stores its own data locally using PouchDB/IndexedDB and exchanges deltas/transactions with peers. RabbitMQ, accessed via STOMP over secure WebSockets, currently acts largely as a durable per-recipient postbox. Mobile browser/PWA suspension interrupts the live STOMP connection. The system should preserve asynchronous delivery without requiring all peers to be online simultaneously. A potential future storage evolution involves RxDB and PostgreSQL/Supabase, but this is not decided.

## Candidate: PGMQ + Supabase Realtime

- **PGMQ** is a PostgreSQL-based durable message queue, supporting reads, visibility timeouts and explicit deletion/archival. It is fundamentally **pull-based**; processing must be idempotent because messages may be redelivered. Confirm exact delivery/ordering guarantees and API constraints before implementation.
- **Supabase** is both an open-source software stack and a hosted SaaS provider. It uses ordinary PostgreSQL as its database and adds Auth, API access, Realtime, Storage and other services. PGMQ can be used outside Supabase, although browser access then requires an appropriate authenticated API.
- **Supabase Realtime** supplies WebSocket notifications to active clients. Prefer evaluating **Broadcast from Database** over Postgres Changes for a scalable notification-only design. Realtime is **not** the durable message transport.
- Design principle: **PGMQ is the postbox; Realtime is the doorbell.** Write the full Perspectives transaction/delta into the queue; publish only a minimal `mail-available` signal on a private recipient-specific topic. A missed signal must never imply missed data.

### Proposed message lifecycle

1. Sender submits a message through an authenticated, authorized API/RPC; server writes to the recipient's durable inbox.
2. In conjunction with the enqueue operation, generate a Realtime notification. **Investigate atomicity** of queue insertion and event generation, including whether a PostgreSQL transaction/outbox pattern is required.
3. An active recipient receives the signal and drains its inbox via HTTPS.
4. After durable local processing, explicitly acknowledge/delete/archive the queue entry. Handle retries, duplicate delivery, poison messages, visibility-timeout expiry and crashes.
5. On **every startup, reconnect, resubscription and resume**, drain the inbox unconditionally, including when no Realtime event was received. Carefully order subscription and initial drain to avoid race windows.
6. An offline or suspended recipient receives no guaranteed Realtime signal; queued data remains available until fetched.

### Authentication, access control and scale

- Use per-installation or per-recipient identity and authorization. A sender must only enqueue to authorized destinations; a recipient must only read/acknowledge its own messages. Do **not** assume exposing PGMQ through `pgmq_public` automatically enforces the intended RLS policy. Audit grants, `SECURITY DEFINER` functions and tenant isolation.
- Private Realtime channels need authorization rules; topic names are not access control.
- **Open scaling question:** a physical PGMQ queue per installation versus shared/partitioned queues plus recipient addressing. The latter is not automatically equivalent to PGMQ's queue-consumer semantics; recipient filtering, contention, ordering, fairness and access control require a concrete design and load tests. Evaluate 10k–100k installations.
- Preserve a transport-neutral messaging interface in Perspectives so RabbitMQ/STOMP and PGMQ can coexist during migration and rollback.

## Browser vs native implications (summary)

- **Active desktop browser / PWA:** one Supabase Realtime WebSocket and PGMQ HTTPS client can live in the existing SharedWorker (subject to library/worker compatibility tests); multiple tabs share the connection.
- **Suspended mobile PWA:** workers and WebSockets cannot be relied upon. Web Push may notify the user or trigger limited service-worker work where supported; always drain PGMQ on resume.
- **Capacitor / mobile Tauri:** foreground Realtime works; wrapping the web app does **not** guarantee persistent background sockets. APNs/FCM can signal the app, but silent/background execution is OS-controlled and not guaranteed, particularly on iOS. The product may need user-visible notifications or deferred synchronization.
- **Desktop Tauri:** while the process is running, background Realtime connectivity is more feasible; still implement reconnect and drain.
- A separate companion handoff describes platform architecture and constraints in more detail.

## File storage and claims

The intended model stores **only JSON claims/metadata and references** in primary structured object storage. Actual media bytes live in file/object storage, retrievable via a URL. Persist a stable provider-neutral `FileRef`, not a signed URL. Resolve to a fresh URL through a storage adapter; for private files, use authenticated fetch or short-lived signed URLs. The access rights of a replicated claim **do not automatically confer access to the referenced file**; design authorization, expiry, sharing, revocation, lifecycle/garbage collection and provider migration. Example interface: `put(blob) -> FileRef`, `getUrl(FileRef) -> URL`, `getBlob(FileRef)`, `delete(FileRef)`.

## Cost model discussed (illustrative; verify current pricing and billing semantics)

The conversation used indicative Supabase Pro pricing of **$25/month**, with **8 GB database disk**, **100 GB object storage**, **500 peak concurrent Realtime connections**, **5 million Realtime messages/month**, and **100k monthly active Auth users** included. Indicative overage rates used were **$0.125/GB-month** PostgreSQL disk, **$0.0213/GB-month** file storage, **$10 per 1,000 additional peak concurrent connections**, and **$2.50 per million additional Realtime messages**. These are planning assumptions, **not a current quote**; check the official pricing, quota enforcement, billing granularity, compute, bandwidth/egress, backup/PITR and any PGMQ/Realtime-specific restrictions.

| Profile | Structured PostgreSQL | File storage | Approx. marginal storage/month/user |
|---|---:|---:|---:|
| Light | 0.1 GB | 0.1 GB | $0.0146 |
| Normal | 0.5 GB | 1 GB | $0.0838 |
| Heavy | 1 GB | 5 GB | $0.2315 |

These are **hypothetical** profiles, not measurements. At 10,000 users, marginal storage alone is roughly $146 / $838 / $2,315 per month, respectively, before included allowances and other charges. A revised normal scenario of **0.2 GB database + 1 GB files** gives ~$0.0463 per user-month. A previous illustrative 10,000-installation case (2,000 peak Realtime connections and 6m notifications/month) estimated ~$42.50/month for Pro + Realtime overages, **excluding database compute, storage, bandwidth and other services**. Realtime message accounting can depend on fan-out, so verify rather than assuming one published notification equals one billable message.

The major cost drivers may be database storage, retained history, file egress, backups, indexes and compute rather than the tiny Realtime wake signals. PGMQ has no separate per-message storage tariff in the earlier discussion, but consumes PostgreSQL resources.

## Lock-in and portability

PostgreSQL data is portable; PGMQ is an extension and can run on suitably configured PostgreSQL elsewhere. Supabase Auth, Realtime, Storage, API/RPC and Edge Functions introduce varying degrees of API and operational coupling, even when self-hostable. Keep interfaces for messaging, auth, storage and replication at the application boundary. A `pg_dump` alone does **not** migrate every aspect of hosted Auth configuration, file objects, secrets, functions and realtime configuration; plan a full migration test.

## Required validation / next engineering tasks

1. Measure real PouchDB/IndexedDB usage: document counts, serialized JSON bytes, revisions, indexes, retained deltas, growth per year and media sizes; build a realistic 5–10 year cohort model.
2. Prototype an authenticated enqueue + notification transaction, and verify event ordering, failure recovery and no lost wakeups.
3. Prototype recipient drain/ack/retry with idempotency keys and recovery from crashes and visibility timeout.
4. Benchmark queue topology, polling load, message rates, authorization isolation and fan-out at target scale.
5. Test Supabase Realtime JS in the existing SharedWorker and across background/resume transitions on desktop and mobile browsers.
6. Compare full operating cost with existing RabbitMQ, including Postgres compute, egress, backup, quotas and operational complexity.
7. Implement storage-provider-neutral file references and access control for claims shared across installations.

## Primary references for verification

- https://supabase.com/docs/guides/queues
- https://supabase.com/docs/guides/realtime
- https://supabase.com/docs/guides/realtime/broadcast
- https://supabase.com/docs/guides/realtime/authorization
- https://supabase.com/docs/guides/storage/serving/downloads
- https://supabase.com/pricing
- https://github.com/tembo-io/pgmq
