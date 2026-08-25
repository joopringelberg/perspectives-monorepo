/**
 * layoutSelector.ts
 *
 * Layout policy selector: maps a LayoutPolicy string to a concrete LayoutAdapter.
 *
 * Production default: 'manual' — preserves the semantic row layout (upstream
 * above current, downstream below) that domain users recognise from the
 * previous SVG renderer.
 *
 * Dagre variants are available for layout experimentation without touching
 * the renderer.  A graph-characteristics-based auto-selector is provided but
 * is kept behind an explicit opt-in flag so 'manual' remains the production
 * default.
 */

import { ManualLayout } from "./layoutAdapters/manualLayout";
import { DagreLayout } from "./layoutAdapters/dagreLayout";
import type { LayoutAdapter } from "./layoutAdapters/index";

// ─── Policy type ──────────────────────────────────────────────────────────────

export type LayoutPolicy =
  | "manual"
  | "dagre-tb"
  | "dagre-lr"
  | "dagre-bt"
  | "dagre-rl";

export const DEFAULT_LAYOUT_POLICY: LayoutPolicy = "manual";

// ─── Selector ─────────────────────────────────────────────────────────────────

/**
 * Returns a LayoutAdapter for the given policy.
 * Unrecognised values fall back to the manual adapter.
 */
export function selectLayout(
  policy: LayoutPolicy = DEFAULT_LAYOUT_POLICY
): LayoutAdapter {
  switch (policy) {
    case "dagre-tb":
      return new DagreLayout("TB");
    case "dagre-lr":
      return new DagreLayout("LR");
    case "dagre-bt":
      return new DagreLayout("BT");
    case "dagre-rl":
      return new DagreLayout("RL");
    case "manual":
    default:
      return new ManualLayout();
  }
}

// ─── Graph-characteristics-based auto-selector (dev / experiment flag) ────────

/**
 * Chooses a layout policy based on simple graph characteristics.
 *
 * This function is intentionally **off by default** (`enableAutoSelect = false`).
 * Flip `enableAutoSelect` to `true` only in development / experiment builds
 * to compare layout strategies without changing production behaviour.
 *
 * @param nodeCount   Total number of nodes in the graph.
 * @param hasCycles   Whether the graph contains at least one cycle.
 * @param density     Edge density: edgeCount / (n * (n − 1)).  Range: 0..1.
 * @param enableAutoSelect  Guard flag. Must be `true` to activate.
 */
export function autoSelectLayout(
  nodeCount: number,
  hasCycles: boolean,
  density: number,
  enableAutoSelect = false
): LayoutPolicy {
  if (!enableAutoSelect) return "manual";
  if (hasCycles || density > 0.4) return "dagre-lr";
  if (nodeCount > 10) return "dagre-tb";
  return "manual";
}
