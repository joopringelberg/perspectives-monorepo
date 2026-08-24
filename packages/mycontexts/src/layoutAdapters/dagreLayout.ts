/**
 * layoutAdapters/dagreLayout.ts
 *
 * Dagre-based automatic layout adapter.
 *
 * Uses @dagrejs/dagre to compute a ranked graph layout in one of four
 * directions: TB (top-to-bottom, default), LR, BT, or RL.
 *
 * Dagre positions are centre-based; this adapter converts to top-left
 * coordinates as required by React Flow.
 */

// @dagrejs/dagre uses `export =` — use namespace import for TS compatibility.
import * as dagre from "@dagrejs/dagre";
import type { LayoutAdapter, LayoutMap } from "./index";
import type { NormalizedNode, NormalizedEdge } from "../graphNormalization";
import type { NodeRole } from "../graphNormalization";

// ─── Node dimensions – must match NavigationGraphView.tsx ────────────────────

const NODE_SIZE: Record<NodeRole, number> = {
  current: 68,
  upstream: 52,
  downstream: 52,
  other: 36,
};

// ─── Types ────────────────────────────────────────────────────────────────────

export type DagreDirection = "TB" | "LR" | "BT" | "RL";

// ─── Adapter ──────────────────────────────────────────────────────────────────

export class DagreLayout implements LayoutAdapter {
  readonly name: string;
  private readonly direction: DagreDirection;

  constructor(direction: DagreDirection = "TB") {
    this.direction = direction;
    this.name = `dagre-${direction.toLowerCase()}`;
  }

  compute(nodes: NormalizedNode[], edges: NormalizedEdge[]): LayoutMap {
    const g = new dagre.graphlib.Graph();
    g.setDefaultEdgeLabel(() => ({}));
    g.setGraph({
      rankdir: this.direction,
      ranksep: 80,
      nodesep: 60,
      marginx: 20,
      marginy: 20,
    });

    for (const n of nodes) {
      const size = NODE_SIZE[n.role];
      g.setNode(n.nodeKey, { width: size, height: size });
    }

    for (const e of edges) {
      g.setEdge(e.sourceKey, e.targetKey);
    }

    dagre.layout(g);

    const result: LayoutMap = new Map();
    for (const n of nodes) {
      const pos = g.node(n.nodeKey);
      const size = NODE_SIZE[n.role];
      // Dagre gives centre positions; convert to top-left for React Flow.
      result.set(n.nodeKey, {
        nodeKey: n.nodeKey,
        x: pos.x - size / 2,
        y: pos.y - size / 2,
        width: size,
        height: size,
      });
    }

    return result;
  }
}
