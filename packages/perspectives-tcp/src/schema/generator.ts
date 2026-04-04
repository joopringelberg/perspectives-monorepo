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
 * **SQL views for filler-chain properties**
 *
 * When a `ColumnConfig` has a non-empty `fillerChain` array the property value
 * is not stored in the base table but in a role table reachable by following N
 * `filler_id` FK hops.  For each such table `applyViews` / `generateViewsSQL`
 * creates a `CREATE OR REPLACE VIEW "<tableName>_view"` that LEFT JOINs through
 * the hop tables and exposes all columns (direct + filler-chain) in one place
 * for reporting queries.
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
   *
   * Filler-chain columns (`fillerChain` is set) are **not** stored in the base
   * table; their values are derived via LEFT JOINs in the `<table>_view` views
   * created by `applyViews`.  Passing them to the schema builder would cause
   * duplicate column names when the same property is reachable via multiple
   * union branches.
   */
  async applySchema(tables: TableConfig[]): Promise<void> {
    logger.info('Applying schema to database …');
    const enriched = tables.map((t) => addStandardColumns(stripFillerChainColumns(t)));
    await this.db.applySchema(enriched);
    logger.info('Schema applied.');
  }

  /**
   * Create SQL views for role tables that have filler-chain columns.
   *
   * Each view is named `<tableName>_view` and LEFT JOINs through the
   * filler-chain role tables so that filler-chain properties appear as
   * regular columns alongside the base table's direct columns.
   *
   * Safe to call on every startup (`CREATE OR REPLACE VIEW` is idempotent).
   *
   * @param tables   Table configs (same array passed to `applySchema`)
   * @param nameMap  Stable-ID → readable-name map from the PDR configuration;
   *                 used to resolve hop table names from `fillerChain` IDs.
   */
  async applyViews(
    tables: TableConfig[],
    nameMap: Record<string, string> = {},
  ): Promise<void> {
    const viewDefs = this.buildViewDefs(tables, nameMap);
    if (viewDefs.length > 0) {
      logger.info(`Creating ${viewDefs.length} SQL view(s) …`);
      await this.db.applyViews(viewDefs);
      logger.info('Views applied.');
    }
  }

  // -------------------------------------------------------------------------
  // Generate SQL scripts
  // -------------------------------------------------------------------------

  /**
   * Return the SQL CREATE TABLE statements as a string.
   * Useful for inspection, version-control, or manual execution.
   *
   * Filler-chain columns are excluded (they are view-only).
   */
  async generateSQL(tables: TableConfig[]): Promise<string> {
    const enriched = tables.map((t) => addStandardColumns(stripFillerChainColumns(t)));
    return this.db.generateSchemaSQL(enriched);
  }

  /**
   * Return the SQL CREATE VIEW statements as a string.
   * Includes only tables that have at least one filler-chain column.
   *
   * @param tables   Table configs
   * @param nameMap  Stable-ID → readable-name map
   */
  generateViewsSQL(
    tables: TableConfig[],
    nameMap: Record<string, string> = {},
  ): string {
    return this.buildViewDefs(tables, nameMap)
      .map((v) => v.sql + ';')
      .join('\n\n');
  }

  // -------------------------------------------------------------------------
  // Internal helpers
  // -------------------------------------------------------------------------

  /**
   * Build view definitions for all tables that have at least one filler-chain
   * column.
   */
  private buildViewDefs(
    tables: TableConfig[],
    nameMap: Record<string, string>,
  ): Array<{ name: string; sql: string }> {
    return tables
      .map((t) => this.buildViewDef(addStandardColumns(t), nameMap))
      .filter((v): v is { name: string; sql: string } => v !== null);
  }

  /**
   * Build a single `CREATE OR REPLACE VIEW` definition for a table, or return
   * `null` when the table has no filler-chain columns (no view needed).
   *
   * The generated view:
   *   1. Selects all **direct** columns (those without a `fillerChain`) from
   *      the base table alias `base`.
   *   2. Groups filler-chain columns by their chain key (same chain = same
   *      sequence of LEFT JOINs) to avoid duplicate JOIN clauses.
   *   3. For each unique chain, emits a sequence of
   *      `LEFT JOIN "<hopTable>" hop_<i>_<j> ON prev.filler_id = hop.id`
   *      clauses, then selects the filler-chain columns from the last alias.
   *
   * Table names for each hop are resolved from the `nameMap` (stable ID →
   * readable name → local name).
   */
  private buildViewDef(
    table: TableConfig,
    nameMap: Record<string, string>,
  ): { name: string; sql: string } | null {
    const fillerColumns = table.columns.filter(
      (c) => c.fillerChain && c.fillerChain.length > 0,
    );
    if (fillerColumns.length === 0) return null;

    /** Convert a stable role-type ID to the local SQL table name. */
    const stableToTableName = (stableId: string): string => {
      const readable = nameMap[stableId] ?? stableId;
      const parts = readable.split('$');
      return parts[parts.length - 1] || readable;
    };

    const viewName = `${table.name}_view`;

    // Direct columns from the base table
    const directCols = table.columns
      .filter((c) => !c.fillerChain || c.fillerChain.length === 0)
      .map((c) => `"base"."${c.name}"`)
      .join(', ');

    // Group filler-chain columns by their chain key to share JOIN sequences
    const chainMap = new Map<string, ColumnConfig[]>();
    for (const col of fillerColumns) {
      const key = col.fillerChain!.join(',');
      if (!chainMap.has(key)) chainMap.set(key, []);
      chainMap.get(key)!.push(col);
    }

    const joinClauses: string[] = [];
    // Map from column name to list of select-expression strings (one per chain branch).
    // When multiple chains provide the same column name (union endpoint), we emit
    // COALESCE(<expr0>, <expr1>, …) AS "<name>" in the SELECT list.
    const colExprsMap = new Map<string, string[]>();
    let chainIndex = 0;

    for (const [chainKey, cols] of chainMap.entries()) {
      const chain = chainKey.split(',');
      let prevAlias = 'base';

      for (let hopIndex = 0; hopIndex < chain.length; hopIndex++) {
        const hopTableName = stableToTableName(chain[hopIndex]);
        const alias = `hop_${chainIndex}_${hopIndex}`;
        joinClauses.push(
          `LEFT JOIN "${hopTableName}" ${alias} ON "${prevAlias}"."filler_id" = ${alias}."id"`,
        );
        prevAlias = alias;
      }

      const lastAlias = `hop_${chainIndex}_${chain.length - 1}`;
      for (const col of cols) {
        if (!colExprsMap.has(col.name)) colExprsMap.set(col.name, []);
        colExprsMap.get(col.name)!.push(`"${lastAlias}"."${col.name}"`);
      }
      chainIndex++;
    }

    // Build filler-chain SELECT expressions, using COALESCE for union branches.
    const fillerSelectExprs: string[] = [];
    for (const [colName, exprs] of colExprsMap.entries()) {
      if (exprs.length === 1) {
        fillerSelectExprs.push(exprs[0]);
      } else {
        fillerSelectExprs.push(`COALESCE(${exprs.join(', ')}) AS "${colName}"`);
      }
    }

    const selectParts = [directCols, ...fillerSelectExprs].filter(Boolean);
    const sql = [
      `CREATE OR REPLACE VIEW "${viewName}" AS`,
      `SELECT ${selectParts.join(', ')}`,
      `FROM "${table.name}" base`,
      ...joinClauses,
    ].join('\n');

    return { name: viewName, sql };
  }
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

/**
 * Add the three standard columns that every TCP role/context table requires
 * unless they are already explicitly declared in the config:
 *   - `id`           TEXT NOT NULL (primary key)
 *   - `context_type` TEXT (only for universal context table)
 *   - `context_id`   TEXT (FK to the owning context table, for role tables)
 *   - `filler_id`    TEXT (FK to the filler role, for role tables)
 *
 * Standard columns are inserted **before** any user-defined columns.
 */
function addStandardColumns(table: TableConfig): TableConfig {
  const existing = new Set(table.columns.map((c) => c.name));
  const prefix: ColumnConfig[] = [];

  if (!existing.has('id')) {
    prefix.push({ name: 'id', type: 'text', primaryKey: true, nullable: false });
  }

  // Universal context table: add context_type column
  if (table.isUniversalContextTable && !existing.has('context_type')) {
    // Every context must have a type; nullable: false enforces this invariant.
    prefix.push({ name: 'context_type', type: 'text', nullable: false });
  }

  // Legacy per-type context tables (contextType set, not the universal table)
  if (table.contextType && !table.isUniversalContextTable && !existing.has('context_type')) {
    prefix.push({ name: 'context_type', type: 'text', nullable: true });
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

/**
 * Return a copy of `table` with all filler-chain columns removed.
 *
 * Filler-chain columns (those whose `fillerChain` array is non-empty) are
 * **not** stored in the base table; their values are materialised via
 * LEFT JOINs in the `<table>_view` created by `applyViews`.  Stripping them
 * before calling `addStandardColumns` / `db.applySchema` prevents duplicate
 * column names in the generated `CREATE TABLE` statement when the same
 * property is reachable via multiple union branches.
 */
function stripFillerChainColumns(table: TableConfig): TableConfig {
  return {
    ...table,
    columns: table.columns.filter(
      (c) => !c.fillerChain || c.fillerChain.length === 0,
    ),
  };
}
