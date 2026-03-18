// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

import { createVerify, createPublicKey } from 'crypto';
import { TransactionForPeer, SignedDelta, TaggedDelta } from '../types';
import { TableConfig } from '../config';
import { DatabaseAdapter, columnForProperty } from '../database/adapter';
import { parseDelta } from './deltaParser';
import { logger } from '../logger';

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

  for (const signedDelta of deltas) {
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
  if (!('contextType' in delta && 'id' in delta && !('roleInstance' in delta))) return;
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
  }
}

async function handleUniverseRoleDelta(
  delta: TaggedDelta['delta'],
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  if (!('roleType' in delta && 'roleInstance' in delta)) return;
  const d = delta as import('../types').UniverseRoleDelta;

  const table = tables.find((t) => t.roleType === d.roleType);
  if (!table) {
    logger.debug(`No table configured for roleType "${d.roleType}" – ignoring`);
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
    case 'RemoveExternalRoleInstance':
      logger.debug(`UniverseRoleDelta: DELETE role "${d.roleInstance}" from "${table.name}"`);
      await db.deleteRow(table.name, d.roleInstance);
      break;
  }
}

async function handleContextDelta(
  delta: TaggedDelta['delta'],
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  if (!('contextInstance' in delta)) return;
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
      break;
  }
}

async function handleRoleBindingDelta(
  delta: TaggedDelta['delta'],
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  if (!('filled' in delta)) return;
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
  }
}

async function handleRolePropertyDelta(
  delta: TaggedDelta['delta'],
  db: DatabaseAdapter,
  tables: TableConfig[],
): Promise<void> {
  if (!('property' in delta && 'id' in delta)) return;
  const d = delta as import('../types').RolePropertyDelta;

  const table = tables.find((t) => t.roleType === d.roleType);
  if (!table) {
    logger.debug(`No table configured for roleType "${d.roleType}" – ignoring`);
    return;
  }

  const col = columnForProperty(table, d.property);
  if (!col) {
    logger.debug(
      `No column configured for property "${d.property}" in table "${table.name}" – ignoring`,
    );
    return;
  }

  switch (d.deltaType) {
    case 'AddProperty':
    case 'SetProperty': {
      const value = d.values.length > 0 ? d.values[0] : null;
      logger.debug(
        `RolePropertyDelta: UPDATE ${table.name}.${col.name} for "${d.id}" → ${JSON.stringify(value)}`,
      );
      await db.updateRow(table.name, d.id, { [col.name]: value });
      break;
    }
    case 'RemoveProperty':
    case 'DeleteProperty':
      logger.debug(
        `RolePropertyDelta: SET NULL ${table.name}.${col.name} for "${d.id}"`,
      );
      await db.updateRow(table.name, d.id, { [col.name]: null });
      break;

    case 'UploadFile':
      // File uploads are tracked as a URL/path stored in the property value
      if (d.values.length > 0) {
        await db.updateRow(table.name, d.id, { [col.name]: d.values[0] });
      }
      break;
  }
}
