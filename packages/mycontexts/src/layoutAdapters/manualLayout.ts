/**
 * layoutAdapters/manualLayout.ts
 *
 * Manual (semantic row-based) layout adapter.
 *
 * Preserves the current mental model:
 *   Row -1  upstream nodes  (nodes that contain the current context as a role)
 *   Row  0  current node
 *   Row +1  downstream nodes (context types reachable from the current context)
 *   Row +2  other nodes      (not directly connected; shown dimmed)
 *
 * Within each row, nodes are evenly spaced horizontally and centred on x = 0.
 * All positions are returned as top-left coordinates (React Flow convention).
 */

import type { LayoutAdapter, LayoutMap } from "./index";
import type { NormalizedNode, NormalizedEdge } from "../graphNormalization";
import type { NodeRole } from "../graphNormalization";

// ─── Spacing constants (pixels) ───────────────────────────────────────────────

const ROW_SPACING = 150;
const COL_SPACING = 110;

// Node diameter per role – must match the values in NavigationGraphView.tsx.
const NODE_SIZE: Record<NodeRole, number> = {
  current: 68,
  upstream: 52,
  downstream: 52,
  other: 36,
};

// ─── Adapter ──────────────────────────────────────────────────────────────────

export class ManualLayout implements LayoutAdapter {
  readonly name = "manual";

  compute(nodes: NormalizedNode[], _edges: NormalizedEdge[]): LayoutMap {
    const result: LayoutMap = new Map();

    const byRole = (role: NodeRole) => nodes.filter((n) => n.role === role);

    const layoutRow = (rowNodes: NormalizedNode[], centreY: number) => {
      rowNodes.forEach((n, i) => {
        // Spread nodes symmetrically around x = 0.
        const centreX =
          rowNodes.length === 1
            ? 0
            : Math.round((i - (rowNodes.length - 1) / 2) * COL_SPACING);
        const size = NODE_SIZE[n.role];
        // Convert centre coords → top-left.
        result.set(n.nodeKey, {
          nodeKey: n.nodeKey,
          x: centreX - size / 2,
          y: centreY - size / 2,
          width: size,
          height: size,
        });
      });
    };

    layoutRow(byRole("upstream"), -ROW_SPACING);
    layoutRow(byRole("current"), 0);
    layoutRow(byRole("downstream"), ROW_SPACING);
    layoutRow(byRole("other"), ROW_SPACING * 2);

    return result;
  }
}
