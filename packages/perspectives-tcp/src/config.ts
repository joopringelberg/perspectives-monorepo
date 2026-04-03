// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

import * as fs from 'fs';
import * as path from 'path';

// ---------------------------------------------------------------------------
// Column configuration
// ---------------------------------------------------------------------------

export type ColumnType = 'text' | 'integer' | 'boolean' | 'datetime' | 'real';

export interface ForeignKeyReference {
  /** Name of the referenced table */
  table: string;
  /** Name of the referenced column (usually "id") */
  column: string;
}

export interface ColumnConfig {
  /** SQL column name */
  name: string;
  /** SQL data type */
  type: ColumnType;
  /** Allow NULL values (default: true) */
  nullable?: boolean;
  /** Mark this column as the primary key */
  primaryKey?: boolean;
  /**
   * Perspectives property type URI.
   * When set, values arriving in RolePropertyDeltas with a matching `property`
   * field are stored in this column.
   */
  propertyType?: string;
  /** Foreign key reference (generates a FK constraint in the schema) */
  references?: ForeignKeyReference;
  /**
   * Filler-chain JOIN path.
   *
   * When present, the property is NOT stored directly on this role's table row
   * but must be read by following the `filler_id` FK chain through N role tables.
   * Each element is the stable `EnumeratedRoleType` URI of the role table at
   * that hop; the property column resides in the LAST table in the list.
   *
   * The `SchemaGenerator` uses this to create SQL VIEWs that materialise these
   * joined columns alongside the base table's direct columns.
   *
   * Example: `FirstName` on `Aanwezigen` via 3 hops:
   *   `["...#Deelnemer", "...#Persons", "...#PerspectivesUsers"]`
   */
  fillerChain?: string[];
}

// ---------------------------------------------------------------------------
// Table configuration
// ---------------------------------------------------------------------------

export interface TableConfig {
  /** SQL table name */
  name: string;
  /**
   * Perspectives role type URI.
   * Role instance data (id, context link, filler link, properties) is stored here.
   */
  roleType?: string;
  /**
   * Perspectives context type URI.
   * Context instance data is stored here.
   * @deprecated Prefer `isUniversalContextTable` for the single context table.
   */
  contextType?: string;
  /**
   * When true, this table is the single universal context table.
   * The schema generator will add `id` (PRIMARY KEY) and `context_type` columns
   * automatically.  Role tables reference this table via their `context_id` column.
   * Set by the PDR-generated TCP configuration; the delta processor uses this flag
   * to route UniverseContextDelta events to this table regardless of context type.
   */
  isUniversalContextTable?: boolean;
  /** Column definitions */
  columns: ColumnConfig[];
}

// ---------------------------------------------------------------------------
// Database configuration (passed directly to Knex)
// ---------------------------------------------------------------------------

export interface DatabaseConfig {
  /**
   * Knex client name.
   * Install the matching driver as a (peer) dependency.
   * Examples: 'pg', 'mysql2', 'better-sqlite3', 'mssql'
   */
  client: string;
  /**
   * Knex connection object or connection string.
   * Shape depends on the chosen client; see the Knex documentation.
   */
  connection: Record<string, unknown> | string;
  /** PostgreSQL schema search path (optional) */
  searchPath?: string[];
}

// ---------------------------------------------------------------------------
// RabbitMQ configuration
// ---------------------------------------------------------------------------

export interface RabbitMQConfig {
  /**
   * AMQP connection URL.
   * Format: amqp[s]://[user:password@]host[:port][/vhost]
   * Example: amqp://guest:guest@localhost:5672/mycontexts
   */
  url: string;
  /**
   * Queue name.  Defaults to `cuid` when omitted.
   * This queue is asserted (created if absent) at startup.
   */
  queue?: string;
  /**
   * Topic exchange to bind the queue to.
   * Defaults to 'amq.topic' (the RabbitMQ default topic exchange used by the
   * Perspectives STOMP plugin).
   */
  exchange?: string;
  /**
   * Routing key used when binding the queue to the exchange.
   * Defaults to `cuid` when omitted.
   */
  routingKey?: string;
  /**
   * How many unacknowledged messages the broker will send at once.
   * Defaults to 10.
   */
  prefetch?: number;
  /**
   * Reconnect delay in milliseconds after a lost connection.
   * Defaults to 5000.
   */
  reconnectDelayMs?: number;
}

// ---------------------------------------------------------------------------
// Top-level TCP configuration
// ---------------------------------------------------------------------------

export interface TCPConfig {
  /**
   * CUID that identifies this TCP instance in the Perspectives Universe.
   * Other Perspectives peers will address transactions to this CUID.
   */
  cuid: string;

  /** RabbitMQ broker settings */
  rabbitmq: RabbitMQConfig;

  /** Relational database settings */
  database: DatabaseConfig;

  /**
   * Relational schema definition.
   * Describes the tables and columns that the TCP should maintain.
   * Each table is linked to a Perspectives role type or context type.
   */
  schema: {
    tables: TableConfig[];
    /**
     * Map from stable Perspectives type identifiers to human-readable names.
     * Generated by the PDR (`GenerateTCPConfiguration`) and used by TCP to
     * resolve table and column names when building SQL JOIN views for
     * filler-chain columns.
     *
     * Merge this from the PDR-generated configuration output alongside `tables`.
     */
    nameMap?: Record<string, string>;
  };

  /**
   * When true the TCP verifies ECDSA-P384 signatures on incoming deltas.
   * Verification requires the author's public key to be present in the
   * transaction's `publicKeys` map.  Defaults to false.
   */
  verifySignatures?: boolean;

  /**
   * Minimum log level.
   * Defaults to 'info'.
   */
  logLevel?: 'error' | 'warn' | 'info' | 'debug';
}

// ---------------------------------------------------------------------------
// Loader
// ---------------------------------------------------------------------------

/**
 * Load and parse a JSON configuration file.
 * Throws if the file cannot be read or the JSON is malformed.
 */
export function loadConfig(filePath: string): TCPConfig {
  const resolved = path.resolve(filePath);
  const raw = fs.readFileSync(resolved, 'utf-8');
  return JSON.parse(raw) as TCPConfig;
}

/**
 * Validate the most critical fields of a config object.
 * Throws a descriptive Error when a required field is missing.
 */
export function validateConfig(config: TCPConfig): void {
  if (!config.cuid || config.cuid.trim() === '') {
    throw new Error('config.cuid must be a non-empty string');
  }
  if (!config.rabbitmq?.url) {
    throw new Error('config.rabbitmq.url is required');
  }
  if (!config.database?.client) {
    throw new Error('config.database.client is required');
  }
  if (!config.database?.connection) {
    throw new Error('config.database.connection is required');
  }
  if (!Array.isArray(config.schema?.tables)) {
    throw new Error('config.schema.tables must be an array');
  }
}
