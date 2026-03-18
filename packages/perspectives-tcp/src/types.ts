// BEGIN LICENSE
// Perspectives Distributed Runtime
// SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com)
// SPDX-License-Identifier: GPL-3.0-or-later
// END LICENSE

/**
 * TypeScript types mirroring the PureScript delta types from:
 *   - Perspectives.Sync.TransactionForPeer
 *   - Perspectives.Sync.SignedDelta
 *   - Perspectives.TypesForDeltas
 *
 * Note on `encryptedDelta`: despite the name, this field contains the JSON-serialised
 * delta string (signed with ECDSA, but not encrypted). The field name is historical.
 */

// ---------------------------------------------------------------------------
// Transaction envelope
// ---------------------------------------------------------------------------

export interface TransactionForPeer {
  /** The PerspectivesUser who authored this transaction */
  author: string;
  /** The PerspectivesSystem context instance of the author */
  perspectivesSystem: string;
  /** ISO-8601 timestamp */
  timeStamp: string;
  /** Array of signed deltas */
  deltas: SignedDelta[];
  /** Public-key info keyed by PerspectivesUser string */
  publicKeys: Record<string, PublicKeyInfo>;
}

export interface SignedDelta {
  /** The PerspectivesUser who produced this delta */
  author: string;
  /**
   * JSON string of the actual delta record.
   * Despite the name, the content is the plain JSON representation of one of the
   * five delta types (UniverseContextDelta, UniverseRoleDelta, ContextDelta,
   * RoleBindingDelta or RolePropertyDelta).  The ECDSA signature is in `signature`.
   */
  encryptedDelta: string;
  /** Base64-encoded ECDSA-P384 signature over `encryptedDelta`, or null when unsigned */
  signature: string | null;
}

export interface PublicKeyInfo {
  /** Serialised JWK public key */
  key: string;
  /** The signed deltas that prove ownership of this key */
  deltas: SignedDelta[];
}

// ---------------------------------------------------------------------------
// Shared base record (DeltaRecord in PureScript)
// ---------------------------------------------------------------------------

export interface DeltaRecord {
  /** The user role that authored this delta */
  subject: string;
  /** Resource identifier used for deterministic ordering */
  resourceKey: string;
  /** Monotonically increasing version counter for the resource */
  resourceVersion: number;
}

// ---------------------------------------------------------------------------
// UniverseContextDelta
// ---------------------------------------------------------------------------

export type UniverseContextDeltaType = 'ConstructEmptyContext';

export interface UniverseContextDelta extends DeltaRecord {
  /** Instance identifier of the new (or removed) context */
  id: string;
  /** Fully-qualified type URI of the context */
  contextType: string;
  deltaType: UniverseContextDeltaType;
}

// ---------------------------------------------------------------------------
// UniverseRoleDelta
// ---------------------------------------------------------------------------

export type UniverseRoleDeltaType =
  | 'ConstructEmptyRole'
  | 'ConstructExternalRole'
  | 'RemoveRoleInstance'
  | 'RemoveUnboundExternalRoleInstance'
  | 'RemoveExternalRoleInstance';

export interface UniverseRoleDelta extends DeltaRecord {
  /** Context instance that owns the role */
  id: string;
  /** Fully-qualified type URI of the context */
  contextType: string;
  /** Fully-qualified type URI of the role */
  roleType: string;
  /** Instance identifier of the role */
  roleInstance: string;
  /** Role type that is authorised to create/remove this role (optional) */
  authorizedRole: string | null;
  deltaType: UniverseRoleDeltaType;
}

// ---------------------------------------------------------------------------
// ContextDelta
// ---------------------------------------------------------------------------

export type ContextDeltaType =
  | 'AddRoleInstancesToContext'
  | 'AddExternalRole'
  | 'MoveRoleInstancesToAnotherContext';

export interface ContextDelta extends DeltaRecord {
  /** Context that the role is being added to (or moved from) */
  contextInstance: string;
  /** Fully-qualified type URI of that context */
  contextType: string;
  /** Fully-qualified type URI of the role */
  roleType: string;
  /** Role instance being added or moved */
  roleInstance: string;
  /** Destination context (only present for MoveRoleInstancesToAnotherContext) */
  destinationContext: string | null;
  /** Destination context type (only present for MoveRoleInstancesToAnotherContext) */
  destinationContextType: string | null;
  deltaType: ContextDeltaType;
}

// ---------------------------------------------------------------------------
// RoleBindingDelta
// ---------------------------------------------------------------------------

export type RoleBindingDeltaType = 'SetFirstBinding' | 'RemoveBinding' | 'ReplaceBinding';

export interface RoleBindingDelta extends DeltaRecord {
  /** The role instance that is being filled */
  filled: string;
  /** Fully-qualified type URI of the filled role */
  filledType: string;
  /** The role instance that acts as filler (null for RemoveBinding) */
  filler: string | null;
  /** Fully-qualified type URI of the filler role (null for RemoveBinding) */
  fillerType: string | null;
  /** Previous filler (present for ReplaceBinding) */
  oldFiller: string | null;
  /** Fully-qualified type URI of the previous filler */
  oldFillerType: string | null;
  deltaType: RoleBindingDeltaType;
}

// ---------------------------------------------------------------------------
// RolePropertyDelta
// ---------------------------------------------------------------------------

export type RolePropertyDeltaType =
  | 'AddProperty'
  | 'RemoveProperty'
  | 'DeleteProperty'
  | 'SetProperty'
  | 'UploadFile';

export interface RolePropertyDelta extends DeltaRecord {
  /** Role instance whose property is being changed */
  id: string;
  /** Fully-qualified type URI of the role */
  roleType: string;
  /** Fully-qualified type URI of the property */
  property: string;
  /** New values (empty for RemoveProperty / DeleteProperty) */
  values: string[];
  deltaType: RolePropertyDeltaType;
}

// ---------------------------------------------------------------------------
// Union / discriminated delta
// ---------------------------------------------------------------------------

export type Delta =
  | UniverseContextDelta
  | UniverseRoleDelta
  | ContextDelta
  | RoleBindingDelta
  | RolePropertyDelta;

export type DeltaKind =
  | 'UniverseContextDelta'
  | 'UniverseRoleDelta'
  | 'ContextDelta'
  | 'RoleBindingDelta'
  | 'RolePropertyDelta';

export interface TaggedDelta {
  kind: DeltaKind;
  delta: Delta;
}
