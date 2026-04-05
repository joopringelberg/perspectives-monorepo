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
    // Pass 1: create all tables without foreign key constraints
    for (const table of tables) {
      const exists = await this.db.schema.hasTable(table.name);
      if (!exists) {
        logger.info(`Creating table "${table.name}"`);
        await this.db.schema.createTable(table.name, (t) => {
          this.buildTable(t, table, false);
        });
      } else {
        logger.debug(`Table "${table.name}" already exists – skipping`);
      }
    }

    // Pass 2: add foreign key constraints now that all tables exist
    for (const table of tables) {
      const fkCols = table.columns.filter((c) => c.references);
      for (const col of fkCols) {
        try {
          await this.db.schema.alterTable(table.name, (t) => {
            t.foreign(col.name)
              .references(col.references!.column)
              .inTable(col.references!.table)
              .onDelete('SET NULL');
          });
        } catch {
          // Constraint may already exist from a previous run
          logger.debug(`FK constraint on "${table.name}"."${col.name}" already exists – skipping`);
        }
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

  private buildTable(builder: Knex.CreateTableBuilder, table: TableConfig, includeForeignKeys = true): void {
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
    if (includeForeignKeys) {
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

  async deleteRowsByContextId(table: string, contextId: string): Promise<void> {
    await this.db(table).where({ context_id: contextId }).delete();
  }

  async updateRowByFillerId(
    table: string,
    fillerId: string,
    data: Record<string, unknown>,
  ): Promise<void> {
    await this.db(table).where({ filler_id: fillerId }).update(data);
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

  async applyViews(viewDefs: Array<{ name: string; sql: string }>): Promise<void> {
    for (const view of viewDefs) {
      logger.info(`Creating view "${view.name}"`);
      try {
        await this.db.raw(view.sql);
      } catch (err) {
        // Log a warning but continue: views are for reporting convenience;
        // failure does not affect the core delta-processing pipeline.
        logger.warn(`Could not create view "${view.name}": ${String(err)}`);
      }
    }
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
