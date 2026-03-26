// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

import { createVerify, createPublicKey } from 'crypto';
import { TransactionForPeer, SignedDelta, TaggedDelta } from '../types';
import { TableConfig, ColumnType } from '../config';
import { DatabaseAdapter, columnForProperty } from '../database/adapter';
import { parseDelta } from './deltaParser';
import { logger } from '../logger';

// ---------------------------------------------------------------------------
// Value coercion
// ---------------------------------------------------------------------------

/**
 * Convert a raw string value (as delivered by Perspectives) to the JS type
 * that Knex / the database driver expects for the given column type.
 */
function coerceValue(raw: string | null, colType: ColumnType): unknown {
  if (raw === null || raw === undefined) return null;
  switch (colType) {
    case 'datetime': {
      // Perspectives sends epoch milliseconds as a string
      const n = Number(raw);
      if (!isNaN(n)) return new Date(n);
      // Fall back: let the driver try to parse whatever string was sent
      return raw;
    }
    case 'integer': {
      const n = parseInt(raw, 10);
      return isNaN(n) ? null : n;
    }
    case 'real': {
      const n = parseFloat(raw);
      return isNaN(n) ? null : n;
    }
    case 'boolean':
      return raw === 'true' || raw === '1';
    case 'text':
    default:
      return raw;
  }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

export interface ProcessorOptions {
  /** Whether to verify ECDSA-P384 signatures on incoming deltas */
  verifySignatures: boolean;
  /** Table configuration from the TCPConfig */
  tables: TableConfig[];
}

/**
 * Process a single `TransactionForPeer` JSON object:
 *   1. Optionally verify delta signatures.
 *   2. Parse each delta from its `encryptedDelta` JSON string.
 *   3. Map each delta to one or more database operations.
 */
export async function processTransaction(
  transaction: TransactionForPeer,
  db: DatabaseAdapter,
  options: ProcessorOptions,
): Promise<void> {
  const { deltas, publicKeys, author, timeStamp } = transaction;

  logger.info(
    `Processing transaction from "${author}" at ${timeStamp} (${deltas.length} deltas)`,
  );

  for (let i = 0; i < deltas.length; i++) {
    const signedDelta = deltas[i];
    if (options.verifySignatures) {
      const valid = await verifySignature(signedDelta, publicKeys);
      if (!valid) {
        logger.warn(
          `Signature verification failed for delta by "${signedDelta.author}" – skipping`,
        );
        continue;
      }
    }

    const tagged = parseDelta(signedDelta.encryptedDelta);
    if (!tagged) {
      logger.warn(
        `Could not parse delta from author "${signedDelta.author}" – skipping`,
      );
      continue;
    }

    logger.debug(`  delta[${i}]: ${tagged.kind} from "${signedDelta.author}"`);

    try {
      await dispatchDelta(tagged, db, options.tables);
    } catch (err) {
      logger.error(`Error processing ${tagged.kind} delta:`, err);
      // Continue with remaining deltas rather than aborting the transaction
    }
  }
}

// ---------------------------------------------------------------------------
// Signature verification (Node.js crypto – ECDSA P-384 / SHA-384)
// ---------------------------------------------------------------------------

/**
 * Verify the ECDSA-P384 signature on a `SignedDelta`.
 *
 * The public key is looked up in the `publicKeys` map that is embedded in the
 * `TransactionForPeer`.  If the key is absent or the signature is null, the
 * delta is accepted unconditionally (trusting the sender).
 */
async function verifySignature(
  signedDelta: SignedDelta,
  publicKeys: Record<string, { key: string }>,
): Promise<boolean> {
  if (!signedDelta.signature) {
    // Unsigned delta – accept
    return true;
  }

  const keyInfo = publicKeys[signedDelta.author];
  if (!keyInfo) {
    logger.debug(
      `No public key for author "${signedDelta.author}" – accepting unsigned`,
    );
    return true;
  }

  try {
    const verify = createVerify('SHA384');
    verify.update(signedDelta.encryptedDelta, 'utf-8');
    // The JWK must be converted to a PEM or DER before use with `crypto.createVerify`.
    // For full production use, import via `crypto.subtle.importKey` (Web Crypto).
    // Here we provide a best-effort verification path.
    const pem = jwkToPem(keyInfo.key);
    if (!pem) return true; // Cannot verify – accept
    return verify.verify(pem, signedDelta.signature, 'base64');
  } catch (err) {
    logger.warn(`Signature verification error for "${signedDelta.author}":`, err);
    return false;
  }
}

/**
 * Attempt to convert a serialised JWK string to a PEM string suitable for
 * `crypto.createVerify`.  Returns null when conversion is not possible.
 *
 * This is a simplified implementation. For full P-384 JWK → PEM support,
 * consider using the `jose` or `node-jose` library.
 */
function jwkToPem(jwkString: string): string | null {
  try {
    const jwk = JSON.parse(jwkString) as Record<string, unknown>;
    const keyObject = createPublicKey({ key: jwk, format: 'jwk' });
    return keyObject.export({ type: 'spki', format: 'pem' }) as string;
  } catch {
    return null;
  }
}

// ---------------------------------------------------------------------------
// Delta dispatcher
// ---------------------------------------------------------------------------

async function dispatchDelta(
  tagged: TaggedDelta,
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  switch (tagged.kind) {
    case 'UniverseContextDelta':
      return handleUniverseContextDelta(tagged.delta, db, tables);
    case 'UniverseRoleDelta':
      return handleUniverseRoleDelta(tagged.delta, db, tables);
    case 'ContextDelta':
      return handleContextDelta(tagged.delta, db, tables);
    case 'RoleBindingDelta':
      return handleRoleBindingDelta(tagged.delta, db, tables);
    case 'RolePropertyDelta':
      return handleRolePropertyDelta(tagged.delta, db, tables);
  }
}

// ---------------------------------------------------------------------------
// Individual delta handlers
// ---------------------------------------------------------------------------

async function handleUniverseContextDelta(
  delta: TaggedDelta['delta'],
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  // Narrow to UniverseContextDelta
  if (!('contextType' in delta && 'id' in delta && !('roleInstance' in delta))) {
    logger.debug(`UniverseContextDelta: unexpected delta shape (fields: ${Object.keys(delta).join(', ')}) – skipping`);
    return;
  }
  const d = delta as import('../types').UniverseContextDelta;

  const table = tables.find((t) => t.contextType === d.contextType);
  if (!table) {
    logger.debug(`No table configured for contextType "${d.contextType}" – ignoring`);
    return;
  }

  switch (d.deltaType) {
    case 'ConstructEmptyContext':
      logger.debug(`UniverseContextDelta: INSERT context "${d.id}" into "${table.name}"`);
      await db.insertRow(table.name, { id: d.id, context_type: d.contextType });
      break;
    default:
      logger.debug(`UniverseContextDelta: unhandled deltaType "${d.deltaType}" – ignoring`);
      break;
  }
}

async function handleUniverseRoleDelta(
  delta: TaggedDelta['delta'],
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  if (!('roleType' in delta && 'roleInstance' in delta)) {
    logger.debug(`UniverseRoleDelta: unexpected delta shape (fields: ${Object.keys(delta).join(', ')}) – skipping`);
    return;
  }
  const d = delta as import('../types').UniverseRoleDelta;

  // RemoveExternalRoleInstance cascades: remove all role rows for the context,
  // then remove the context row itself.
  if (d.deltaType === 'RemoveExternalRoleInstance') {
    await handleRemoveExternalRoleInstance(d, db, tables);
    return;
  }

  const table = tables.find((t) => t.roleType === d.roleType);
  if (!table) {
    // External roles never have a dedicated table (their properties are stored
    // in the context table).  Suppress the "not found" log for these types.
    // Note: RemoveExternalRoleInstance is already handled above and never reaches here.
    if (d.deltaType !== 'ConstructExternalRole' && d.deltaType !== 'RemoveUnboundExternalRoleInstance') {
      logger.debug(`No table configured for roleType "${d.roleType}" – ignoring`);
    }
    return;
  }

  switch (d.deltaType) {
    case 'ConstructEmptyRole':
    case 'ConstructExternalRole':
      logger.debug(`UniverseRoleDelta: INSERT role "${d.roleInstance}" into "${table.name}"`);
      await db.insertRow(table.name, {
        id: d.roleInstance,
        context_id: d.id,
      });
      break;

    case 'RemoveRoleInstance':
    case 'RemoveUnboundExternalRoleInstance':
      logger.debug(`UniverseRoleDelta: DELETE role "${d.roleInstance}" from "${table.name}"`);
      await db.deleteRow(table.name, d.roleInstance);
      break;

    default:
      logger.debug(`UniverseRoleDelta: unhandled deltaType "${d.deltaType}" – ignoring`);
      break;
  }
}

/**
 * Handle a RemoveExternalRoleInstance delta by cascade-deleting all role rows
 * that belong to the context, then deleting the context row itself.
 */
async function handleRemoveExternalRoleInstance(
  d: import('../types').UniverseRoleDelta,
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  const contextId = d.id;
  const contextType = d.contextType;

  // Find the context table for this context type (needed for FK cross-reference).
  const contextTable = tables.find((t) => t.contextType === contextType);

  // Collect role tables that belong to this context.  There are two ways a role
  // table can be associated with a context type:
  //
  // 1. Standard roles: roleType starts with "<contextType>$" (the role is
  //    lexically defined inside this context).
  //
  // 2. Aspect roles: the role is lexically defined in another context but
  //    "adopted" by this context.  In that case roleType does NOT start with
  //    "<contextType>$", but the table has a column that carries a foreign-key
  //    reference to the context table (references.table === contextTable.name).
  //
  // Using the FK reference as the authoritative signal covers both cases.
  const contextTypePrefix = contextType + '$';
  const roleTables = tables.filter((t) => {
    if (!t.roleType) return false;
    // Case 1: standard role – roleType prefix match
    if (t.roleType.startsWith(contextTypePrefix)) return true;
    // Case 2: aspect role – a column references the context table
    if (contextTable) {
      return t.columns.some((c) => c.references?.table === contextTable.name);
    }
    return false;
  });

  for (const roleTable of roleTables) {
    logger.debug(
      `RemoveExternalRoleInstance: DELETE all roles with context_id "${contextId}" from "${roleTable.name}"`,
    );
    await db.deleteRowsByContextId(roleTable.name, contextId);
  }

  // Delete the context row itself.
  if (contextTable) {
    logger.debug(
      `RemoveExternalRoleInstance: DELETE context "${contextId}" from "${contextTable.name}"`,
    );
    await db.deleteRow(contextTable.name, contextId);
  } else {
    logger.debug(
      `RemoveExternalRoleInstance: no context table configured for "${contextType}" – skipping context row deletion`,
    );
  }
}

async function handleContextDelta(
  delta: TaggedDelta['delta'],
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  if (!('contextInstance' in delta)) {
    logger.debug(`ContextDelta: unexpected delta shape (fields: ${Object.keys(delta).join(', ')}) – skipping`);
    return;
  }
  const d = delta as import('../types').ContextDelta;

  const table = tables.find((t) => t.roleType === d.roleType);
  if (!table) {
    logger.debug(`No table configured for roleType "${d.roleType}" – ignoring`);
    return;
  }

  switch (d.deltaType) {
    case 'AddRoleInstancesToContext':
      logger.debug(
        `ContextDelta: UPSERT role "${d.roleInstance}" in "${table.name}" (context_id = "${d.contextInstance}")`,
      );
      await db.upsertRow(table.name, d.roleInstance, {
        id: d.roleInstance,
        context_id: d.contextInstance,
      });
      break;

    case 'MoveRoleInstancesToAnotherContext':
      if (d.destinationContext) {
        logger.debug(
          `ContextDelta: UPDATE role "${d.roleInstance}" context_id → "${d.destinationContext}"`,
        );
        await db.updateRow(table.name, d.roleInstance, {
          context_id: d.destinationContext,
        });
      }
      break;

    case 'AddExternalRole':
      // External roles link a context to its external role instance.
      // No separate table operation needed beyond what UniverseRoleDelta provides.
      logger.debug(`ContextDelta: AddExternalRole for "${d.roleInstance}" – no database operation needed`);
      break;

    default:
      logger.debug(`ContextDelta: unhandled deltaType "${d.deltaType}" – ignoring`);
      break;
  }
}

async function handleRoleBindingDelta(
  delta: TaggedDelta['delta'],
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  if (!('filled' in delta)) {
    logger.debug(`RoleBindingDelta: unexpected delta shape (fields: ${Object.keys(delta).join(', ')}) – skipping`);
    return;
  }
  const d = delta as import('../types').RoleBindingDelta;

  const table = tables.find((t) => t.roleType === d.filledType);
  if (!table) {
    logger.debug(`No table configured for roleType "${d.filledType}" – ignoring`);
    return;
  }

  switch (d.deltaType) {
    case 'SetFirstBinding':
    case 'ReplaceBinding':
      logger.debug(
        `RoleBindingDelta: UPDATE filler_id for "${d.filled}" → "${d.filler ?? 'null'}"`,
      );
      await db.updateRow(table.name, d.filled, { filler_id: d.filler ?? null });
      break;

    case 'RemoveBinding':
      logger.debug(`RoleBindingDelta: CLEAR filler_id for "${d.filled}"`);
      await db.updateRow(table.name, d.filled, { filler_id: null });
      break;

    default:
      logger.debug(`RoleBindingDelta: unhandled deltaType "${d.deltaType}" – ignoring`);
      break;
  }
}

async function handleRolePropertyDelta(
  delta: TaggedDelta['delta'],
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  if (!('property' in delta && 'id' in delta)) {
    logger.debug(`RolePropertyDelta: unexpected delta shape (fields: ${Object.keys(delta).join(', ')}) – skipping`);
    return;
  }
  const d = delta as import('../types').RolePropertyDelta;

  // External role properties are stored in the context table, not a separate role table.
  // If the roleType ends with "$External", look up the context table instead.
  const isExternalRole = d.roleType.endsWith('$External');
  let table: TableConfig | undefined;
  if (isExternalRole) {
    const contextType = d.roleType.slice(0, -'$External'.length);
    table = tables.find((t) => t.contextType === contextType);
    if (!table) {
      logger.debug(`No context table configured for external roleType "${d.roleType}" (contextType="${contextType}") – ignoring`);
      return;
    }
  } else {
    table = tables.find((t) => t.roleType === d.roleType);
    if (!table) {
      logger.debug(`No table configured for roleType "${d.roleType}" – ignoring`);
      return;
    }
  }

  const col = columnForProperty(table, d.property);
  if (!col) {
    logger.debug(
      `No column configured for property "${d.property}" in table "${table.name}" – ignoring`,
    );
    return;
  }

  // For External role properties written to the context table, the row id is
  // the context id (without the "$External" suffix).
  const rowId = isExternalRole ? d.id.replace(/\$External$/, '') : d.id;

  switch (d.deltaType) {
    case 'AddProperty':
    case 'SetProperty': {
      const rawValue = d.values.length > 0 ? d.values[0] : null;
      const value = coerceValue(rawValue, col.type);
      logger.debug(
        `RolePropertyDelta: UPDATE ${table.name}.${col.name} for "${rowId}" → ${JSON.stringify(value)}`,
      );
      await db.updateRow(table.name, rowId, { [col.name]: value });
      break;
    }
    case 'RemoveProperty':
    case 'DeleteProperty':
      logger.debug(
        `RolePropertyDelta: SET NULL ${table.name}.${col.name} for "${rowId}"`,
      );
      await db.updateRow(table.name, rowId, { [col.name]: null });
      break;

    case 'UploadFile':
      // File uploads are tracked as a URL/path stored in the property value
      if (d.values.length > 0) {
        logger.debug(
          `RolePropertyDelta: UploadFile – UPDATE ${table.name}.${col.name} for "${rowId}" → ${JSON.stringify(d.values[0])}`,
        );
        await db.updateRow(table.name, rowId, { [col.name]: d.values[0] });
      } else {
        logger.debug(`RolePropertyDelta: UploadFile – no value provided for "${d.id}", ignoring`);
      }
      break;

    default:
      logger.debug(`RolePropertyDelta: unhandled deltaType "${d.deltaType}" – ignoring`);
      break;
  }
}
