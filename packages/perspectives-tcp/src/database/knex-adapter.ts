// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

import knex, { Knex } from 'knex';
import { DatabaseConfig, TableConfig, ColumnConfig } from '../config';
import { DatabaseAdapter, primaryKeyColumn } from './adapter';
import { logger } from '../logger';

/**
 * Knex-based implementation of `DatabaseAdapter`.
 *
 * Supports any database that Knex supports out of the box:
 *   - PostgreSQL  (`pg`)
 *   - MySQL / MariaDB  (`mysql2`)
 *   - SQLite  (`better-sqlite3`)
 *   - Microsoft SQL Server  (`mssql` / `tedious`)
 *
 * Install the appropriate driver as a dependency in your project; only the
 * driver for the `client` you choose needs to be present.
 */
export class KnexAdapter implements DatabaseAdapter {
  private db: Knex;

  constructor(config: DatabaseConfig) {
    this.db = knex({
      client: config.client,
      connection: config.connection,
      searchPath: config.searchPath,
      useNullAsDefault: true, // required for SQLite
    } as Knex.Config);
  }

  // -------------------------------------------------------------------------
  // Schema
  // -------------------------------------------------------------------------

  async applySchema(tables: TableConfig[]): Promise<void> {
    for (const table of tables) {
      const exists = await this.db.schema.hasTable(table.name);
      if (!exists) {
        logger.info(`Creating table "${table.name}"`);
        await this.db.schema.createTable(table.name, (t) => {
          this.buildTable(t, table);
        });
      } else {
        logger.debug(`Table "${table.name}" already exists – skipping`);
      }
    }
  }

  async generateSchemaSQL(tables: TableConfig[]): Promise<string> {
    const statements: string[] = [];
    for (const table of tables) {
      const sql = this.db.schema
        .createTable(table.name, (t) => {
          this.buildTable(t, table);
        })
        .toString();
      statements.push(sql + ';');
    }
    return statements.join('\n\n');
  }

  private buildTable(builder: Knex.CreateTableBuilder, table: TableConfig): void {
    for (const col of table.columns) {
      const colBuilder = this.addColumn(builder, col);
      if (col.nullable === false) {
        colBuilder.notNullable();
      } else {
        colBuilder.nullable();
      }
      if (col.primaryKey) {
        colBuilder.primary();
      }
    }

    // Add foreign key constraints separately (after all columns are defined)
    for (const col of table.columns) {
      if (col.references) {
        builder
          .foreign(col.name)
          .references(col.references.column)
          .inTable(col.references.table)
          .onDelete('SET NULL');
      }
    }
  }

  private addColumn(
    builder: Knex.CreateTableBuilder,
    col: ColumnConfig,
  ): Knex.ColumnBuilder {
    switch (col.type) {
      case 'integer':
        return builder.integer(col.name);
      case 'boolean':
        return builder.boolean(col.name);
      case 'datetime':
        return builder.datetime(col.name);
      case 'real':
        return builder.float(col.name);
      case 'text':
      default:
        return builder.text(col.name);
    }
  }

  // -------------------------------------------------------------------------
  // DML operations
  // -------------------------------------------------------------------------

  async insertRow(table: string, data: Record<string, unknown>): Promise<void> {
    try {
      await this.db(table).insert(data);
    } catch (err: unknown) {
      // Silently ignore unique-constraint violations (duplicate inserts are idempotent)
      if (isUniqueConstraintError(err)) {
        logger.debug(`Duplicate insert ignored for table "${table}"`);
        return;
      }
      throw err;
    }
  }

  async updateRow(
    table: string,
    id: string,
    data: Record<string, unknown>,
  ): Promise<void> {
    await this.db(table).where({ id }).update(data);
  }

  async deleteRow(table: string, id: string): Promise<void> {
    await this.db(table).where({ id }).delete();
  }

  async upsertRow(
    table: string,
    id: string,
    data: Record<string, unknown>,
  ): Promise<void> {
    const exists = await this.db(table).where({ id }).first();
    if (exists) {
      const updateData: Record<string, unknown> = { ...data };
      delete updateData['id'];
      await this.db(table).where({ id }).update(updateData);
    } else {
      await this.insertRow(table, data);
    }
  }

  async close(): Promise<void> {
    await this.db.destroy();
  }
}

// ---------------------------------------------------------------------------
// Helper – detect unique-constraint errors across database backends
// ---------------------------------------------------------------------------

function isUniqueConstraintError(err: unknown): boolean {
  if (!(err instanceof Error)) return false;
  const message = err.message.toLowerCase();
  return (
    message.includes('unique') ||
    message.includes('duplicate') ||
    // SQLite
    message.includes('unique constraint failed') ||
    // PostgreSQL (error code 23505)
    (err as NodeJS.ErrnoException).code === '23505' ||
    // MySQL (error code 1062)
    (err as NodeJS.ErrnoException).code === 'ER_DUP_ENTRY'
  );
}

export { primaryKeyColumn };
