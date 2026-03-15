// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

import { DatabaseAdapter } from '../database/adapter';
import { TableConfig, ColumnConfig } from '../config';
import { logger } from '../logger';

// ---------------------------------------------------------------------------
// Schema generator
// ---------------------------------------------------------------------------

/**
 * Generate and optionally apply the relational database schema derived from
 * the TCP configuration.
 *
 * The generator supports the **Smashed Role Chains** strategy (the preferred
 * approach described in the issue):
 *   - One SQL table per observable Perspectives role type.
 *   - Each table contains:
 *       * `id`         – role instance identifier (primary key)
 *       * `context_id` – reference to the owning context instance
 *       * `filler_id`  – reference to the role that fills this role (nullable)
 *       * One column for every Perspectives property in scope
 *   - One SQL table per observable Perspectives context type.
 *
 * The TCP operator provides the schema as a JSON configuration; future work
 * will auto-derive it from a Perspectives DomeinFile (.json).
 */
export class SchemaGenerator {
  private db: DatabaseAdapter;

  constructor(db: DatabaseAdapter) {
    this.db = db;
  }

  // -------------------------------------------------------------------------
  // Apply schema to the live database
  // -------------------------------------------------------------------------

  /**
   * Create all configured tables in the database (skips tables that already
   * exist – safe to call on every startup).
   */
  async applySchema(tables: TableConfig[]): Promise<void> {
    logger.info('Applying schema to database …');
    const enriched = tables.map(addStandardColumns);
    await this.db.applySchema(enriched);
    logger.info('Schema applied.');
  }

  // -------------------------------------------------------------------------
  // Generate SQL CREATE script
  // -------------------------------------------------------------------------

  /**
   * Return the SQL CREATE TABLE statements as a string.
   * Useful for inspection, version-control, or manual execution.
   */
  async generateSQL(tables: TableConfig[]): Promise<string> {
    const enriched = tables.map(addStandardColumns);
    return this.db.generateSchemaSQL(enriched);
  }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/**
 * Add the three standard columns that every TCP role/context table requires
 * unless they are already explicitly declared in the config:
 *   - `id`         TEXT NOT NULL (primary key)
 *   - `context_id` TEXT (FK to the owning context table, when a context table exists)
 *   - `filler_id`  TEXT (FK to the filler role, when applicable)
 *
 * Standard columns are inserted **before** any user-defined columns.
 */
function addStandardColumns(table: TableConfig): TableConfig {
  const existing = new Set(table.columns.map((c) => c.name));
  const prefix: ColumnConfig[] = [];

  if (!existing.has('id')) {
    prefix.push({ name: 'id', type: 'text', primaryKey: true, nullable: false });
  }

  if (table.roleType && !existing.has('context_id')) {
    prefix.push({ name: 'context_id', type: 'text', nullable: true });
  }

  if (table.roleType && !existing.has('filler_id')) {
    prefix.push({ name: 'filler_id', type: 'text', nullable: true });
  }

  return {
    ...table,
    columns: [...prefix, ...table.columns],
  };
}
