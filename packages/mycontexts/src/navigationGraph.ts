/**
 * navigationGraph.ts
 *
 * Type definitions and utility functions for the server-derived static model context
 * navigation graph.
 *
 * The graph is constructed server-side (in the PDR) from the DomeinFile of the model
 * that defines the current context type. It is retrieved via the `getModelContextGraph`
 * API call and is identical for all users of the same model.
 */

import { ContextType, RoleInstanceT } from "perspectives-proxy";

// ─── Re-export proxy types locally ───────────────────────────────────────────
export type { ModelContextGraph, ModelGraphNode, ModelGraphEdge } from "perspectives-proxy";

// ─── Instance-enriched node (client-side only) ────────────────────────────────

/**
 * An instance record that can be bound to a graph node, representing one context
 * instance that belongs to a given type in the neighbourhood of the current context.
 */
export interface ContextInstance {
  /** The role instance ID that can be used to open the context (via OpenContext event). */
  roleId: RoleInstanceT;
  /** Human-readable name of the context instance. */
  readableName: string;
}

/**
 * Wider-context record: a context that contains the current context as a role.
 * Returned by `getWiderContexts`; enriched with the resolved context type.
 */
export interface WiderContext {
  /** External role instance ID of the wider context. */
  externalRole: RoleInstanceT;
  /** Human-readable name of the wider context. */
  readableName: string;
  /** Resolved context type for matching against DAG nodes. Set after type lookup. */
  contextType?: ContextType;
}

// ─── Utility ─────────────────────────────────────────────────────────────────

/** Extracts the local name from a fully-qualified Perspectives identifier. */
export function localName(qualifiedName: string): string {
  const dollar = qualifiedName.lastIndexOf("$");
  return dollar >= 0 ? qualifiedName.slice(dollar + 1) : qualifiedName;
}
