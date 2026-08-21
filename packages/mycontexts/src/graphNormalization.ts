/**
 * graphNormalization.ts
 *
 * Normalizes the raw inputs (server-derived ModelContextGraph + client-side
 * instance data) into a stable view-model that layout adapters and the React
 * Flow renderer can consume without coupling to Perspectives-specific types.
 *
 * This module has no rendering dependencies; it can be tested in isolation.
 */

import { externalRole } from "perspectives-react";
import {
  ContextType,
  ContextInstanceT,
  RoleInstanceT,
  TableFormDef,
} from "perspectives-proxy";
import type { ModelContextGraph } from "perspectives-proxy";
import type { WiderContext } from "./navigationGraph";

// ─── View-model types ─────────────────────────────────────────────────────────

export type NodeRole = "current" | "upstream" | "downstream" | "other";

export interface InstanceInfo {
  roleId: RoleInstanceT;
  readableName: string;
}

/** A node in the normalized graph, enriched with instance data. */
export interface NormalizedNode {
  /** Stable React / React Flow key: role:typeId:index */
  nodeKey: string;
  /** Fully-qualified Perspectives context type identifier. */
  typeId: ContextType;
  /** Translated type label from the server. */
  typeLabel: string;
  /** Display label: instance name when single-instance, else type label. */
  label: string;
  /** Semantic role relative to the current context. */
  role: NodeRole;
  /** Context instances bound to this node (client-side). */
  instances: InstanceInfo[];
  /** Indexed singleton context instance id (when the DAG node carries one). */
  indexedName?: ContextInstanceT;
  /** Whether the node can be clicked to navigate. */
  isNavigable: boolean;
  /** Whether the node has multiple instances (drives the multi-stack indicator). */
  isMulti: boolean;
}

/** An edge in the normalized graph. */
export interface NormalizedEdge {
  /** Stable React / React Flow key. */
  edgeKey: string;
  /** nodeKey of the source node. */
  sourceKey: string;
  /** nodeKey of the target node. */
  targetKey: string;
  /** Human-readable role label for display on the edge. */
  roleLabel: string;
  /** Perspectives role type id (drives the visibility rule). */
  roleId?: string;
  /** Whether the original type-level edge was a self-referencing type. */
  isSelfLoop: boolean;
  /** Solid (true) vs dotted (false) rendering. */
  isVisible: boolean;
}

export interface NormalizedGraph {
  nodes: NormalizedNode[];
  edges: NormalizedEdge[];
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

function truncate(s: string, maxLen: number): string {
  return s.length > maxLen ? s.slice(0, maxLen - 1) + "…" : s;
}

/**
 * Resolve the navigation role instance id for indexed singleton nodes.
 * Returns undefined when the node carries no indexed context instance.
 */
export function indexedRoleId(
  node: Pick<NormalizedNode, "indexedName">
): RoleInstanceT | undefined {
  if (!node.indexedName) return undefined;
  return externalRole(String(node.indexedName)) as RoleInstanceT;
}

function buildDownstreamInstances(
  contextRoles: TableFormDef[]
): Map<string, InstanceInfo[]> {
  const result = new Map<string, InstanceInfo[]>();
  for (const def of contextRoles) {
    const perspective =
      def.table?.widgetCommonFields?.perspective ??
      def.form?.widgetCommonFields?.perspective;
    if (!perspective || perspective.roleKind !== "ContextRole") continue;
    const typesToCreate = perspective.contextTypesToCreate ?? {};
    const targetTypes = Object.values(typesToCreate).filter(
      (v): v is ContextType => Boolean(v)
    );
    const roleInstances = perspective.roleInstances ?? {};
    const instances: InstanceInfo[] = Object.values(roleInstances).map(
      (ri) => ({ roleId: ri.roleId, readableName: ri.readableName })
    );
    for (const targetType of targetTypes) {
      result.set(targetType, [...(result.get(targetType) ?? []), ...instances]);
    }
  }
  return result;
}

function buildUpstreamInstances(
  widerContexts: WiderContext[]
): Map<string, InstanceInfo[]> {
  const result = new Map<string, InstanceInfo[]>();
  for (const wc of widerContexts) {
    if (!wc.contextType) continue;
    result.set(wc.contextType, [
      ...(result.get(wc.contextType) ?? []),
      { roleId: wc.externalRole, readableName: wc.readableName },
    ]);
  }
  return result;
}

function buildVisibleEdgeRoleTypes(contextRoles: TableFormDef[]): Set<string> {
  const result = new Set<string>();
  for (const def of contextRoles) {
    const perspective =
      def.table?.widgetCommonFields?.perspective ??
      def.form?.widgetCommonFields?.perspective;
    if (!perspective || perspective.roleKind !== "ContextRole") continue;
    result.add(perspective.roleType);
  }
  return result;
}

// ─── Graph-characteristics helpers (for future layout-selector use) ───────────

/**
 * Returns true when the graph contains at least one cycle.
 * Uses a simple DFS reachability check on typeIds.
 */
export function hasCycles(graph: ModelContextGraph): boolean {
  const adj = new Map<string, string[]>();
  for (const e of graph.edges) {
    const from = String(e.from);
    const to = String(e.to);
    if (!adj.has(from)) adj.set(from, []);
    adj.get(from)!.push(to);
  }
  const visited = new Set<string>();
  const inStack = new Set<string>();
  function dfs(node: string): boolean {
    if (inStack.has(node)) return true;
    if (visited.has(node)) return false;
    visited.add(node);
    inStack.add(node);
    for (const nb of adj.get(node) ?? []) {
      if (dfs(nb)) return true;
    }
    inStack.delete(node);
    return false;
  }
  return graph.nodes.some((n) => dfs(String(n.id)));
}

/**
 * Returns the graph edge density: edgeCount / (nodeCount * (nodeCount − 1)).
 * Returns 0 for graphs with fewer than 2 nodes.
 */
export function edgeDensity(graph: ModelContextGraph): number {
  const n = graph.nodes.length;
  if (n < 2) return 0;
  return graph.edges.length / (n * (n - 1));
}

// ─── Public normalizer ────────────────────────────────────────────────────────

/**
 * Converts raw PDR graph + client instance data into the NormalizedGraph
 * view-model. All subsequent processing (layout, rendering) operates only on
 * NormalizedGraph.
 */
export function normalizeGraph(
  graph: ModelContextGraph,
  currentType: ContextType,
  currentTitle: string | undefined,
  contextRoles: TableFormDef[],
  widerContexts: WiderContext[]
): NormalizedGraph {
  if (graph.nodes.length === 0) return { nodes: [], edges: [] };

  const downstreamMap = buildDownstreamInstances(contextRoles);
  const upstreamMap = buildUpstreamInstances(widerContexts);
  const visibleEdgeRoleTypes = buildVisibleEdgeRoleTypes(contextRoles);
  const widerContextTypes = new Set<string>(
    widerContexts
      .map((wc) => wc.contextType)
      .filter((ct): ct is ContextType => Boolean(ct))
      .map((ct) => String(ct))
  );

  const nodeMap = new Map(graph.nodes.map((n) => [n.id, n]));

  // Direct downstream neighbours: current → neighbour.
  const downstreamIds = new Set(
    graph.edges.filter((e) => e.from === currentType).map((e) => e.to)
  );
  // Direct upstream neighbours: neighbour → current.
  const upstreamIds = new Set(
    graph.edges.filter((e) => e.to === currentType).map((e) => e.from)
  );

  const instancesForType = (
    map: Map<string, InstanceInfo[]>,
    typeId: ContextType
  ): InstanceInfo[] => map.get(typeId) ?? map.get(String(typeId)) ?? [];

  // ─── Build nodes ────────────────────────────────────────────────────────────

  const makeKey = (role: NodeRole, id: ContextType, index: number) =>
    `${role}:${String(id)}:${index}`;

  const nodes: NormalizedNode[] = [];
  // Maps typeId → all nodeKeys created for that type.
  const nodeKeysByType = new Map<ContextType, string[]>();

  const pushNode = (
    role: NodeRole,
    typeId: ContextType,
    index: number,
    instances: InstanceInfo[]
  ) => {
    const raw = nodeMap.get(typeId);
    if (!raw) return;
    const nodeKey = makeKey(role, typeId, index);
    const isCurrent = role === "current";
    const isMulti = instances.length > 1;
    const indexedName = (raw as { indexedName?: ContextInstanceT }).indexedName;
    const isIndexed = Boolean(indexedName);
    const isNavigable = !isCurrent && (instances.length > 0 || isIndexed);

    const displayLabel =
      !isCurrent && instances.length === 1 && !isMulti
        ? truncate(instances[0].readableName, 10)
        : truncate(raw.label, 10);

    const node: NormalizedNode = {
      nodeKey,
      typeId,
      typeLabel: raw.label,
      label:
        isCurrent && currentTitle?.trim() ? currentTitle : displayLabel,
      role,
      instances,
      indexedName,
      isNavigable,
      isMulti,
    };
    nodes.push(node);
    nodeKeysByType.set(typeId, [
      ...(nodeKeysByType.get(typeId) ?? []),
      nodeKey,
    ]);
  };

  // Row 0 – current node.
  pushNode("current", currentType, 0, []);

  // Row -1 – upstream nodes (suppress self-referencing type clone here;
  // it appears as a downstream clone instead).
  Array.from(upstreamIds)
    .filter((id) => id !== currentType)
    .map((id) => nodeMap.get(id))
    .filter((n): n is NonNullable<typeof n> => n !== undefined)
    .forEach((n, i) =>
      pushNode("upstream", n.id, i, instancesForType(upstreamMap, n.id))
    );

  // Row +1 – downstream nodes.
  Array.from(downstreamIds)
    .map((id) => nodeMap.get(id))
    .filter((n): n is NonNullable<typeof n> => n !== undefined)
    .forEach((n, i) =>
      pushNode("downstream", n.id, i, instancesForType(downstreamMap, n.id))
    );

  // Row +2 – other nodes (not directly connected to current), dimmed.
  const neighborIds = new Set([...downstreamIds, ...upstreamIds, currentType]);
  graph.nodes
    .filter((n) => !neighborIds.has(n.id))
    .forEach((n, i) => pushNode("other", n.id, i, []));

  // ─── Build edges ────────────────────────────────────────────────────────────

  // Resolve a nodeKey for a given typeId, preferring a specific role.
  const resolveKey = (
    typeId: ContextType,
    preferRole?: NodeRole
  ): string | undefined => {
    const keys = nodeKeysByType.get(typeId);
    if (!keys || keys.length === 0) return undefined;
    if (preferRole) {
      const match = keys.find((k) => k.startsWith(`${preferRole}:`));
      if (match) return match;
    }
    return keys[0];
  };

  const edges: NormalizedEdge[] = [];
  graph.edges.forEach((edge, i) => {
    const isSelfLoop = edge.from === edge.to;

    const sourceKey = isSelfLoop
      ? resolveKey(edge.from, "current")
      : resolveKey(
          edge.from,
          edge.from === currentType ? "current" : undefined
        );
    const targetKey = isSelfLoop
      ? resolveKey(edge.to, "downstream")
      : resolveKey(edge.to, edge.to === currentType ? "current" : undefined);

    if (!sourceKey || !targetKey) return;

    const isVisibleByRole = visibleEdgeRoleTypes.has(String(edge.roleId));
    const isVisibleByWider =
      (edge.from === currentType && widerContextTypes.has(String(edge.to))) ||
      (edge.to === currentType && widerContextTypes.has(String(edge.from)));

    edges.push({
      edgeKey: `edge:${i}:${String(edge.from)}:${String(edge.to)}`,
      sourceKey,
      targetKey,
      roleLabel: edge.roleLabel ?? "",
      roleId: edge.roleId ? String(edge.roleId) : undefined,
      isSelfLoop,
      isVisible: isVisibleByRole || isVisibleByWider,
    });
  });

  return { nodes, edges };
}
