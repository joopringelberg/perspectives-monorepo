# perspectives-tcp

**Transaction Collection Point (TCP)** for the Perspectives Distributed Runtime.

The TCP is a Node.js server application that receives Perspectives transactions over RabbitMQ (AMQP) and stores the data in a relational database. The resulting database can be queried with any standard reporting tool (Power BI, Metabase, Grafana, etc.).

---

## Architecture

```
Perspectives peers
      │
      │  TransactionForPeer (JSON / AMQP)
      ▼
 RabbitMQ broker  ──────────────────────────────►  Dead Letter Exchange
      │                                             (rejected messages)
      │
      ▼
 perspectives-tcp
   ├── AMQPClient       – subscribe, ack/nack, auto-reconnect
   ├── DeltaParser      – discriminate UniverseContextDelta / UniverseRoleDelta /
   │                       ContextDelta / RoleBindingDelta / RolePropertyDelta
   ├── Processor        – map each delta to INSERT / UPDATE / DELETE
   └── KnexAdapter      – execute SQL against any supported RDBMS
      │
      ▼
 Relational database  (PostgreSQL / MySQL / SQLite / MSSQL)
      │
      ▼
 Reporting tool  (Power BI, Metabase, …)
```

### Schema strategy – Smashed Role Chains

The preferred schema strategy (as described in the Perspectives issue) is **Smashed Role Chains**:

- One SQL table per observable Perspectives **role type**.
- Each table contains:
  - `id`         – role instance identifier (primary key)
  - `context_id` – reference to the owning context instance
  - `filler_id`  – reference to the role that fills this role (nullable)
  - One column per Perspectives property in scope
- One SQL table per observable **context type** (id + type columns).

This minimises the number of JOIN operations needed in reporting queries.

---

## Requirements

- Node.js ≥ 18
- A RabbitMQ broker (≥ 3.8) with the AMQP 0-9-1 port reachable from the server
- A relational database (PostgreSQL, MySQL/MariaDB, SQLite, or Microsoft SQL Server)

---

## Installation

```bash
# From the monorepo root
pnpm install

# From this package directory
pnpm build
```

Install the database driver that matches your database:

| Database            | Package             |
|---------------------|---------------------|
| PostgreSQL          | `pg`                |
| MySQL / MariaDB     | `mysql2`            |
| SQLite              | `better-sqlite3`    |
| Microsoft SQL Server| `tedious`           |

```bash
# Example: SQLite for local development
pnpm add better-sqlite3
```

---

## Configuration

Copy `config.example.json` to `config.json` and edit it:

```jsonc
{
  "cuid": "YOUR_TCP_CUID",           // identity in the Perspectives Universe

  "rabbitmq": {
    "url": "amqp://user:pass@host:5672/mycontexts",
    "exchange": "amq.topic",         // default – matches the PDR STOMP plugin
    "prefetch": 10
  },

  "database": {
    "client": "better-sqlite3",      // or "pg", "mysql2", "mssql"
    "connection": { "filename": "./tcp-data.db" }
  },

  "verifySignatures": false,         // set true to verify ECDSA-P384 signatures
  "logLevel": "info",

  "schema": {
    "tables": [ /* see config.example.json */ ]
  }
}
```

### Configuration reference

| Field | Required | Description |
|-------|----------|-------------|
| `cuid` | ✓ | CUID that identifies this TCP in the Perspectives Universe. Peers send transactions to the queue with this name. |
| `rabbitmq.url` | ✓ | AMQP connection URL (`amqp[s]://user:pass@host:port/vhost`) |
| `rabbitmq.exchange` | | Topic exchange name. Default: `amq.topic` |
| `rabbitmq.queue` | | Queue name. Default: `cuid` |
| `rabbitmq.routingKey` | | Routing key. Default: `cuid` |
| `rabbitmq.prefetch` | | Consumer prefetch count. Default: `10` |
| `rabbitmq.reconnectDelayMs` | | Reconnect delay in ms. Default: `5000` |
| `database.client` | ✓ | Knex client name (see table above) |
| `database.connection` | ✓ | Knex connection config or connection string |
| `verifySignatures` | | Verify ECDSA-P384 delta signatures. Default: `false` |
| `logLevel` | | `error` \| `warn` \| `info` \| `debug`. Default: `info` |
| `schema.tables` | ✓ | Array of table definitions (see below) |

### Schema table definition

```jsonc
{
  "name": "deelnemers",              // SQL table name
  "roleType": "model://…#Deelnemer", // Perspectives role type URI (or contextType)
  "columns": [
    {
      "name": "naam",
      "type": "text",                // text | integer | boolean | datetime | real
      "propertyType": "model://…#Deelnemer$Naam"  // Perspectives property URI
    }
  ]
}
```

Standard columns (`id`, `context_id`, `filler_id`) are added automatically.

---

## Usage

```bash
# Start the TCP (reads config.json in current directory by default)
perspectives-tcp

# Use a custom config path
perspectives-tcp --config /etc/tcp/stadskamer.json

# Print the SQL CREATE TABLE script and exit (without starting the consumer)
perspectives-tcp --schema --config ./stadskamer.json
```

---

## Delta types processed

| Perspectives delta | Operation |
|-------------------|-----------|
| `UniverseContextDelta (ConstructEmptyContext)` | INSERT into context table |
| `UniverseRoleDelta (ConstructEmptyRole / ConstructExternalRole)` | INSERT into role table |
| `UniverseRoleDelta (RemoveRoleInstance / …)` | DELETE from role table |
| `ContextDelta (AddRoleInstancesToContext)` | UPSERT role row with `context_id` |
| `ContextDelta (MoveRoleInstancesToAnotherContext)` | UPDATE `context_id` |
| `RoleBindingDelta (SetFirstBinding / ReplaceBinding)` | UPDATE `filler_id` |
| `RoleBindingDelta (RemoveBinding)` | SET `filler_id = NULL` |
| `RolePropertyDelta (AddProperty / SetProperty)` | UPDATE column |
| `RolePropertyDelta (RemoveProperty / DeleteProperty)` | SET column to NULL |
| `RolePropertyDelta (UploadFile)` | UPDATE column with file path/URL |

---

## Deployment

### Docker (example)

```dockerfile
FROM node:20-slim
WORKDIR /app
COPY package*.json ./
RUN npm ci --omit=dev
COPY dist/ dist/
CMD ["node", "dist/index.js", "--config", "/config/config.json"]
```

### Environment variables

You may interpolate credentials from environment variables in `config.json`
by passing the config through `envsubst` before startup:

```bash
envsubst < config.template.json > config.json
perspectives-tcp --config config.json
```

---

## Extending

### Custom database adapter

Implement the `DatabaseAdapter` interface in `src/database/adapter.ts` to
support a database not covered by Knex.

### Model-based schema generation (future work)

The current implementation requires the operator to define the schema manually
in `config.json`.  A future extension will auto-derive the schema from a
Perspectives DomeinFile (`.json`) by:

1. Loading the DomeinFile(s) specified in `config.models`.
2. Finding all role types reachable from the configured observer roles.
3. Generating a `schema.tables` configuration using the **Smashed Role Chains** strategy.

---

## License

GPL-3.0-or-later – see the root [LICENSE](../../LICENSE) directory.
