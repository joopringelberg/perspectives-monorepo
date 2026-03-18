// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

import {
  Delta,
  DeltaKind,
  TaggedDelta,
  ContextDelta,
  RoleBindingDelta,
  RolePropertyDelta,
  UniverseContextDelta,
  UniverseRoleDelta,
} from '../types';

// ---------------------------------------------------------------------------
// Type guards
// ---------------------------------------------------------------------------

type AnyRecord = Record<string, unknown>;

/**
 * Check whether a parsed JSON object looks like a RolePropertyDelta.
 * Discriminating field: `property` (unique to RolePropertyDelta).
 */
function isRolePropertyDelta(obj: AnyRecord): obj is AnyRecord & RolePropertyDelta {
  return typeof obj['property'] === 'string' && Array.isArray(obj['values']);
}

/**
 * Check whether a parsed JSON object looks like a RoleBindingDelta.
 * Discriminating field: `filled` (unique to RoleBindingDelta).
 */
function isRoleBindingDelta(obj: AnyRecord): obj is AnyRecord & RoleBindingDelta {
  return typeof obj['filled'] === 'string' && typeof obj['filledType'] === 'string';
}

/**
 * Check whether a parsed JSON object looks like a ContextDelta.
 * Discriminating field: `contextInstance` (unique to ContextDelta).
 */
function isContextDelta(obj: AnyRecord): obj is AnyRecord & ContextDelta {
  return typeof obj['contextInstance'] === 'string';
}

/**
 * Check whether a parsed JSON object looks like a UniverseRoleDelta.
 * Discriminating field: `roleInstance` present but `contextInstance` absent.
 */
function isUniverseRoleDelta(obj: AnyRecord): obj is AnyRecord & UniverseRoleDelta {
  return typeof obj['roleInstance'] === 'string' && typeof obj['contextInstance'] === 'undefined';
}

/**
 * Check whether a parsed JSON object looks like a UniverseContextDelta.
 * This is the catch-all: id + contextType, no roleInstance.
 */
function isUniverseContextDelta(obj: AnyRecord): obj is AnyRecord & UniverseContextDelta {
  return typeof obj['id'] === 'string' && typeof obj['contextType'] === 'string';
}

// ---------------------------------------------------------------------------
// Parser
// ---------------------------------------------------------------------------

/**
 * Parse the `encryptedDelta` JSON string from a SignedDelta into a typed Delta.
 *
 * Despite the name `encryptedDelta`, the field contains the plain JSON
 * representation of one of the five Perspectives delta types (the ECDSA
 * signature is stored separately in `SignedDelta.signature`).
 *
 * The disambiguation order mirrors the PDR's `executeDelta` function in
 * Perspectives.Sync.HandleTransaction:
 *   1. RolePropertyDelta  (has `property`)
 *   2. RoleBindingDelta   (has `filled`)
 *   3. ContextDelta       (has `contextInstance`)
 *   4. UniverseRoleDelta  (has `roleInstance`)
 *   5. UniverseContextDelta (catch-all)
 *
 * Returns null when the string cannot be parsed or does not match any type.
 */
export function parseDelta(encryptedDelta: string): TaggedDelta | null {
  let obj: Record<string, unknown>;

  try {
    obj = JSON.parse(encryptedDelta) as Record<string, unknown>;
  } catch {
    return null;
  }

  if (typeof obj !== 'object' || obj === null) {
    return null;
  }

  let kind: DeltaKind;
  let delta: Delta;

  if (isRolePropertyDelta(obj)) {
    kind = 'RolePropertyDelta';
    delta = obj;
  } else if (isRoleBindingDelta(obj)) {
    kind = 'RoleBindingDelta';
    delta = obj;
  } else if (isContextDelta(obj)) {
    kind = 'ContextDelta';
    delta = obj;
  } else if (isUniverseRoleDelta(obj)) {
    kind = 'UniverseRoleDelta';
    delta = obj;
  } else if (isUniverseContextDelta(obj)) {
    kind = 'UniverseContextDelta';
    delta = obj;
  } else {
    return null;
  }

  return { kind, delta };
}
