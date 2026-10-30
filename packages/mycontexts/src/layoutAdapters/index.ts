/**
 * layoutAdapters/index.ts
 *
 * Public interface contract for all layout adapters.
 *
 * A LayoutAdapter takes a set of NormalizedNodes and NormalizedEdges and
 * returns a LayoutMap: a mapping from each nodeKey to its final position and
 * dimensions.  Positions use top-left corner coordinates, matching React Flow's
 * node position convention.
 */

import type { NormalizedNode, NormalizedEdge } from "../graphNormalization";

export interface LayoutPosition {
  nodeKey: string;
  /** Left edge of the node in layout units (pixels). Top-left origin. */
  x: number;
  /** Top edge of the node in layout units (pixels). Top-left origin. */
  y: number;
  /** Node width in pixels. */
  width: number;
  /** Node height in pixels. */
  height: number;
}

/** Maps nodeKey → computed layout position. */
export type LayoutMap = Map<string, LayoutPosition>;

export interface LayoutAdapter {
  /** Human-readable name of this adapter (used in debug/experiment output). */
  readonly name: string;
  /**
   * Compute positions for all nodes.
   * @param nodes Normalized nodes (all nodes, including hidden ones; filter
   *   before passing if only a subset should be laid out).
   * @param edges Normalized edges.
   */
  compute(nodes: NormalizedNode[], edges: NormalizedEdge[]): LayoutMap;
}
