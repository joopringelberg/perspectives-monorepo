/**
 * navigationGraph.ts
 *
 * Renderer-agnostic, model-static navigation graph.
 *
 * The graph is type-level and identical for all users of the same model.
 * It is built client-side from the serialised screen payloads produced by the
 * PDR (WhoWhatWhereScreenDef).  No DomeinFile schema changes are required.
 *
 * A single module-level singleton accumulates edges as the user navigates.
 * Call `mergeScreenIntoGraph` whenever a new screen is loaded to grow the graph.
 * Call `resetNavigationGraph` when the user switches to a completely different
 * model (optional; the graph simply keeps growing otherwise).
 */

import { ContextType, TableFormDef, WhoWhatWhereScreenDef } from "perspectives-proxy";

// ─── Public types ──────────────────────────────────────────────────────────────

export interface GraphNode {
  contextType: ContextType;
  /** Human-readable label derived from the model (e.g. context type local name). */
  label: string;
}

export interface GraphEdge {
  from: ContextType;
  to: ContextType;
  /** Optional human-readable label (role display name or creation label). */
  label?: string;
}

export interface NavigationGraph {
  nodes: Map<string, GraphNode>;
  edges: GraphEdge[];
}

// ─── Module-level singleton ─────────────────────────────────────────────────

let globalGraph: NavigationGraph = {
  nodes: new Map(),
  edges: [],
};

/** Returns the current singleton graph. */
export function getNavigationGraph(): NavigationGraph {
  return globalGraph;
}

/** Replaces the singleton with an empty graph. */
export function resetNavigationGraph(): void {
  globalGraph = { nodes: new Map(), edges: [] };
}

// ─── Internal helpers ─────────────────────────────────────────────────────────

/** Extracts the local name from a fully-qualified Perspectives identifier. */
function localName(qualifiedName: string): string {
  const dollar = qualifiedName.lastIndexOf("$");
  return dollar >= 0 ? qualifiedName.slice(dollar + 1) : qualifiedName;
}

/**
 * Extracts candidate edges from a list of TableFormDefs.
 *
 * For each TableFormDef the embedded Perspective is inspected.  Every entry in
 * `perspective.contextTypesToCreate` gives a (label → ContextType) pair that
 * represents a navigable edge from `sourceType` to that ContextType.
 */
function edgesFromTableForms(
  defs: TableFormDef[],
  sourceType: ContextType
): GraphEdge[] {
  const edges: GraphEdge[] = [];
  for (const def of defs) {
    // A TableFormDef always has both table and form; prefer table.
    const perspective =
      def.table?.widgetCommonFields?.perspective ??
      def.form?.widgetCommonFields?.perspective;
    if (!perspective) continue;

    const toCreate = perspective.contextTypesToCreate;
    if (!toCreate) continue;

    for (const [label, targetType] of Object.entries(toCreate)) {
      if (targetType) {
        edges.push({
          from: sourceType,
          to: targetType as ContextType,
          label,
        });
      }
    }
  }
  return edges;
}

// ─── Public API ───────────────────────────────────────────────────────────────

/**
 * Merges one screen into the global navigation graph.
 *
 * @param screen            The serialised WhoWhatWhereScreenDef for the current context.
 * @param currentContextType The ContextType of the context currently open.
 * @param currentLabel       A human-readable label for the current context type
 *                           (e.g. the screen title or translated type name).
 * @returns A **new** NavigationGraph object so that React equality checks can
 *          detect the change.
 */
export function mergeScreenIntoGraph(
  screen: WhoWhatWhereScreenDef,
  currentContextType: ContextType,
  currentLabel: string
): NavigationGraph {
  const { nodes, edges } = globalGraph;

  // Ensure the current node is present.
  if (!nodes.has(currentContextType as string)) {
    nodes.set(currentContextType as string, {
      contextType: currentContextType,
      label: currentLabel || localName(currentContextType as string),
    });
  }

  // Collect candidate edges from all three screen sections.
  const candidates: GraphEdge[] = [
    ...edgesFromTableForms(screen.whereto.contextRoles, currentContextType),
    ...edgesFromTableForms(screen.who.userRoles, currentContextType),
    ...(screen.what.tag === "TableForms"
      ? edgesFromTableForms(screen.what.elements.tableForms, currentContextType)
      : []),
  ];

  for (const edge of candidates) {
    // Ensure the target node exists.
    if (!nodes.has(edge.to as string)) {
      nodes.set(edge.to as string, {
        contextType: edge.to,
        label: localName(edge.to as string),
      });
    }
    // Avoid duplicate edges (directed).
    if (!edges.some((e) => e.from === edge.from && e.to === edge.to)) {
      edges.push(edge);
    }
  }

  // Return a new object reference so React props equality checks fire.
  globalGraph = { nodes, edges };
  return globalGraph;
}

/**
 * Returns all direct neighbours (both outgoing and incoming edges) of
 * `contextType` in the given graph.
 */
export function getNeighbors(
  graph: NavigationGraph,
  contextType: ContextType
): Set<string> {
  const neighbors = new Set<string>();
  for (const edge of graph.edges) {
    if (edge.from === contextType) neighbors.add(edge.to as string);
    if (edge.to === contextType) neighbors.add(edge.from as string);
  }
  return neighbors;
}

/**
 * Finds a navigable role-instance ID for the given target context type by
 * scanning all three sections of the provided screen for a ContextRole
 * perspective whose `contextTypesToCreate` includes `targetType`.
 *
 * Returns the first `roleId` found, or `undefined` if none is available.
 */
export function findRoleInstanceForType(
  screen: WhoWhatWhereScreenDef,
  targetType: ContextType
): string | undefined {
  const allDefs: TableFormDef[] = [
    ...screen.whereto.contextRoles,
    ...screen.who.userRoles,
    ...(screen.what.tag === "TableForms"
      ? screen.what.elements.tableForms
      : []),
  ];

  for (const def of allDefs) {
    const perspective =
      def.table?.widgetCommonFields?.perspective ??
      def.form?.widgetCommonFields?.perspective;
    if (!perspective) continue;
    if (perspective.roleKind !== "ContextRole") continue;

    const toCreate = perspective.contextTypesToCreate;
    if (!toCreate) continue;

    const matches = Object.values(toCreate).some(
      (ct) => ct === (targetType as string)
    );
    if (!matches) continue;

    const instances = Object.values(perspective.roleInstances) as Array<{ roleId: string }>;
    if (instances.length > 0) {
      return instances[0].roleId;
    }
  }
  return undefined;
}
