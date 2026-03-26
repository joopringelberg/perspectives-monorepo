// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

import { TableConfig, ColumnConfig } from '../config';

// ---------------------------------------------------------------------------
// Database adapter interface
// ---------------------------------------------------------------------------

/**
 * Minimal set of relational-database operations required by the TCP
 * transaction processor.
 *
 * Implement this interface to support a specific database backend.
 * The provided `KnexAdapter` covers PostgreSQL, MySQL/MariaDB, SQLite and
 * Microsoft SQL Server via the Knex query builder.
 */
export interface DatabaseAdapter {
  /**
   * Apply the schema (create tables if absent).
   * Called once at startup.
   */
  applySchema(tables: TableConfig[]): Promise<void>;

  /**
   * Generate SQL CREATE TABLE statements from the schema configuration.
   * The returned string can be inspected or run manually against any compatible database.
   */
  generateSchemaSQL(tables: TableConfig[]): Promise<string>;

  /**
   * Insert a new row.  If a row with the same primary key already exists,
   * the operation is silently ignored (idempotent).
   *
   * @param table  SQL table name
   * @param data   Column → value map for the new row
   */
  insertRow(table: string, data: Record<string, unknown>): Promise<void>;

  /**
   * Update an existing row identified by its primary-key value.
   * Has no effect if no row with the given id exists.
   *
   * @param table  SQL table name
   * @param id     Primary-key value
   * @param data   Column → value map of fields to update
   */
  updateRow(table: string, id: string, data: Record<string, unknown>): Promise<void>;

  /**
   * Delete a row by its primary-key value.
   * Has no effect if the row does not exist.
   *
   * @param table  SQL table name
   * @param id     Primary-key value
   */
  deleteRow(table: string, id: string): Promise<void>;

  /**
   * Delete all rows whose `context_id` column equals the given context identifier.
   * Used to cascade-delete all role instances when a context is removed.
   * Has no effect if no matching rows exist.
   *
   * @param table      SQL table name
   * @param contextId  Value to match against the `context_id` column
   */
  deleteRowsByContextId(table: string, contextId: string): Promise<void>;

  /**
   * Upsert: insert the row if absent, otherwise update the specified columns.
   *
   * @param table  SQL table name
   * @param id     Primary-key value
   * @param data   Full column → value map (including `id`)
   */
  upsertRow(table: string, id: string, data: Record<string, unknown>): Promise<void>;

  /** Close the database connection. */
  close(): Promise<void>;
}

// ---------------------------------------------------------------------------
// Helper: find the primary-key column name for a table
// ---------------------------------------------------------------------------

export function primaryKeyColumn(table: TableConfig): string {
  const pk = table.columns.find((c) => c.primaryKey === true);
  return pk?.name ?? 'id';
}

// ---------------------------------------------------------------------------
// Helper: find a column by its Perspectives property type
// ---------------------------------------------------------------------------

export function columnForProperty(
  table: TableConfig,
  propertyType: string,
): ColumnConfig | undefined {
  return table.columns.find((c) => c.propertyType === propertyType);
}
