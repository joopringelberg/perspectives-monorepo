---
applyTo: "packages/perspectives-tcp/**"
---

# perspectives-tcp – Copilot Instructions

## What This Package Is

`perspectives-tcp` is the **Transaction Collection Point (TCP)** server for the Perspectives project. It is a **Node.js / TypeScript** application that:

1. Subscribes to Perspectives transactions delivered over **RabbitMQ (AMQP via Stomp)**
2. Parses incoming `TransactionForPeer` JSON payloads into typed deltas
3. Maps each delta to SQL INSERT/UPDATE/DELETE operations
4. Persists data to a **relational database** (PostgreSQL / MySQL / SQLite / MSSQL via Knex)

The resulting database can be queried by any standard reporting tool (Power BI, Metabase, Grafana, etc.).

**Language**: TypeScript / Node.js  
**Build tool**: `tsc` (TypeScript compiler)

---

## Directory Structure

```
packages/perspectives-tcp/
├── src/
│   ├── index.ts              # Entry point: wires together AMQPClient → Processor → KnexAdapter
│   ├── tcp.ts                # TCP-related utilities
│   ├── config.ts             # Configuration loading (from config.json)
│   ├── logger.ts             # Structured logger
│   ├── types.ts              # Shared TypeScript types
│   ├── amqp/                 # AMQPClient: subscribe, ack/nack, auto-reconnect
│   ├── transaction/          # DeltaParser: discriminates delta types
│   ├── database/             # KnexAdapter: SQL execution
│   └── schema/               # Schema definitions (Smashed Role Chains strategy)
├── config.example.json       # Example configuration file
├── tsconfig.json
└── package.json
```

---

## Architecture

```
Perspectives peers
      │  TransactionForPeer (JSON / AMQP)
      ▼
 RabbitMQ broker ──► Dead Letter Exchange (rejected messages)
      │
      ▼
 perspectives-tcp
   ├── AMQPClient       – subscribe, ack/nack, auto-reconnect
   ├── DeltaParser      – discriminate delta types
   ├── Processor        – map each delta to INSERT/UPDATE/DELETE
   └── KnexAdapter      – execute SQL
      │
      ▼
 Relational database → Reporting tools
```

### Delta Types (from Perspectives protocol)
- `UniverseContextDelta` — context creation/deletion
- `UniverseRoleDelta` — role instance creation/deletion
- `ContextDelta` — role membership changes
- `RoleBindingDelta` — filler changes
- `RolePropertyDelta` — property value changes

### Schema Strategy: Smashed Role Chains
The preferred schema:
- **One SQL table per observable Perspectives role type** with columns: `id` (PK), `context_id`, `filler_id`, one column per property
- **One SQL table per observable context type** with `id` and `type` columns
- Minimises JOIN operations for reporting queries

---

## Configuration

Copy `config.example.json` to `config.json` and fill in:
- RabbitMQ connection details (host, port, credentials, queue name)
- Database connection (driver, host, port, name, credentials)

---

## Build & Run

```bash
cd packages/perspectives-tcp
pnpm run build    # tsc → compiled JS in dist/
pnpm run start    # node dist/index.js
pnpm run lint     # ESLint
pnpm run test     # Tests
```

---

## Common Pitfalls

1. **Dead Letter Exchange**: configure a DLX in RabbitMQ for messages that fail parsing or DB insertion — the AMQP client will `nack` them.
2. **Schema migrations**: adding new Perspectives properties or role types requires updating the schema and potentially running migrations on the existing database.
3. **Idempotency**: deltas may be redelivered on reconnect. The processor should handle duplicate `INSERT` gracefully (use `INSERT OR IGNORE` / `ON CONFLICT DO NOTHING`).
4. **Config file not committed**: `config.json` contains credentials — never commit it. Only `config.example.json` is in version control.
