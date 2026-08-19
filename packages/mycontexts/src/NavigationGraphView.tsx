/**
 * NavigationGraphView.tsx
 *
 * SVG-based renderer for the model-static navigation graph.
 *
 * Visual encoding:
 *  - Current context node: large, Bootstrap-primary fill (top row).
 *  - Direct neighbours:    medium, Bootstrap-info fill (middle row).
 *  - All other nodes:      small, dimmed (bottom row).
 *
 * Interactions:
 *  - Pan: click-drag on the SVG background.
 *  - Zoom: mouse-wheel.
 *  - Click node: dispatches an `OpenContext` CustomEvent bubbling up from the
 *    host container, reusing the existing www.tsx navigation flow.
 *  - Toggle button: switches between "neighbourhood only" and "full graph".
 */

import * as React from "react";
import { useEffect, useRef, useState, useCallback } from "react";
import { ContextType, WhoWhatWhereScreenDef } from "perspectives-proxy";
import {
  NavigationGraph,
  GraphNode,
  getNavigationGraph,
  getNeighbors,
  mergeScreenIntoGraph,
  findRoleInstanceForType,
} from "./navigationGraph";

// ─── Layout constants ──────────────────────────────────────────────────────────

// Vertical layout: rows spaced along the Y axis.
const ROW_SPACING  = 140;   // vertical distance between rows
const COL_SPACING  = 100;   // horizontal distance between nodes in the same row
const R_CURRENT    = 34;
const R_NEIGHBOR   = 26;
const R_OTHER      = 20;

// ─── Types ────────────────────────────────────────────────────────────────────

interface PositionedNode extends GraphNode {
  x: number;
  y: number;
  r: number;
  role: "current" | "neighbor" | "other";
}

interface Transform {
  tx: number;
  ty: number;
  scale: number;
}

// ─── Layout computation ───────────────────────────────────────────────────────

function computeLayout(
  graph: NavigationGraph,
  currentType: ContextType
): PositionedNode[] {
  const allNodes = Array.from(graph.nodes.values());
  if (allNodes.length === 0) return [];

  const neighbors = getNeighbors(graph, currentType);

  const neighborsArr = allNodes.filter(
    (n) => neighbors.has(n.contextType as string) && n.contextType !== currentType
  );
  const others = allNodes.filter(
    (n) =>
      !neighbors.has(n.contextType as string) &&
      n.contextType !== currentType
  );

  const positioned: PositionedNode[] = [];

  // Row 0 – current node centred at the top.
  const cur = graph.nodes.get(currentType as string);
  if (cur) {
    positioned.push({ ...cur, x: 0, y: 0, r: R_CURRENT, role: "current" });
  }

  // Row 1 – neighbours spread horizontally below the current node.
  const nCount = neighborsArr.length;
  neighborsArr.forEach((n, i) => {
    const x = nCount === 1
      ? 0
      : Math.round((i - (nCount - 1) / 2) * COL_SPACING);
    positioned.push({
      ...n,
      x,
      y: ROW_SPACING,
      r: R_NEIGHBOR,
      role: "neighbor",
    });
  });

  // Row 2 – other nodes spread horizontally below the neighbours row.
  const oCount = others.length;
  others.forEach((n, i) => {
    const x = oCount === 1
      ? 0
      : Math.round((i - (oCount - 1) / 2) * COL_SPACING);
    positioned.push({
      ...n,
      x,
      y: ROW_SPACING * 2,
      r: R_OTHER,
      role: "other",
    });
  });

  return positioned;
}

// ─── Component ────────────────────────────────────────────────────────────────

interface NavigationGraphViewProps {
  currentContextType: ContextType;
  currentContextLabel: string;
  /** Full screen definition for the current context (used for edge extraction
   *  and for finding role-instance IDs on node click). */
  currentScreen: WhoWhatWhereScreenDef | undefined;
  /** Host DOM element; OpenContext events are dispatched on it. */
  hostRef: React.RefObject<HTMLDivElement | null>;
}

export function NavigationGraphView({
  currentContextType,
  currentContextLabel,
  currentScreen,
  hostRef,
}: NavigationGraphViewProps) {

  // ── Graph state ─────────────────────────────────────────────────────────────
  const [graph, setGraph] = useState<NavigationGraph>(getNavigationGraph);
  const [showFullGraph, setShowFullGraph] = useState(false);

  // Merge the current screen into the global graph whenever it changes.
  useEffect(() => {
    if (!currentScreen || !currentContextType) return;
    const updated = mergeScreenIntoGraph(
      currentScreen,
      currentContextType,
      currentContextLabel
    );
    setGraph({ nodes: updated.nodes, edges: updated.edges });
  }, [currentScreen, currentContextType, currentContextLabel]);

  // ── Pan / zoom state ────────────────────────────────────────────────────────
  const [transform, setTransform] = useState<Transform>({ tx: 0, ty: 0, scale: 1 });
  const dragging = useRef(false);
  const dragStart = useRef({ x: 0, y: 0, tx: 0, ty: 0 });

  const onWheel = useCallback((e: React.WheelEvent<SVGSVGElement>) => {
    e.preventDefault();
    setTransform((t) => {
      const factor = e.deltaY < 0 ? 1.1 : 0.9;
      const newScale = Math.min(4, Math.max(0.25, t.scale * factor));
      return { ...t, scale: newScale };
    });
  }, []);

  const onMouseDown = useCallback((e: React.MouseEvent<SVGSVGElement>) => {
    if ((e.target as SVGElement).tagName !== "svg") return;
    dragging.current = true;
    dragStart.current = { x: e.clientX, y: e.clientY, tx: transform.tx, ty: transform.ty };
  }, [transform]);

  const onMouseMove = useCallback((e: React.MouseEvent<SVGSVGElement>) => {
    if (!dragging.current) return;
    const dx = e.clientX - dragStart.current.x;
    const dy = e.clientY - dragStart.current.y;
    setTransform((t) => ({
      ...t,
      tx: dragStart.current.tx + dx,
      ty: dragStart.current.ty + dy,
    }));
  }, []);

  const onMouseUp = useCallback(() => { dragging.current = false; }, []);

  // ── Node click ───────────────────────────────────────────────────────────────
  const handleNodeClick = useCallback(
    (node: PositionedNode) => {
      if (node.role === "current") return; // already here

      if (!currentScreen || !hostRef.current) return;

      const roleInstanceId = findRoleInstanceForType(
        currentScreen,
        node.contextType
      );
      if (!roleInstanceId) return; // no instance available – node not yet navigable

      hostRef.current.dispatchEvent(
        new CustomEvent("OpenContext", { detail: roleInstanceId, bubbles: true })
      );
    },
    [currentScreen, hostRef]
  );

  // ── Render ───────────────────────────────────────────────────────────────────

  if (graph.nodes.size === 0) return null;

  const allPositioned = computeLayout(graph, currentContextType);
  const visibleNodes = showFullGraph
    ? allPositioned
    : allPositioned.filter((n) => n.role !== "other");

  // Show the toggle only when there are "other" nodes.
  const hasOthers = allPositioned.some((n) => n.role === "other");

  // Determine a viewBox that fits all visible nodes.
  const margin = 60;
  const xs = visibleNodes.map((n) => n.x);
  const ys = visibleNodes.map((n) => n.y);
  const minX = (xs.length ? Math.min(...xs) : -COL_SPACING * 2) - margin;
  const maxX = (xs.length ? Math.max(...xs) : COL_SPACING * 2)  + margin;
  const minY = (ys.length ? Math.min(...ys) : 0)                - margin;
  const maxY = (ys.length ? Math.max(...ys) : ROW_SPACING * 2)  + margin;
  const vbW = maxX - minX;
  const vbH = maxY - minY;

  return (
    <div className="navigation-graph-container mt-2" style={{ width: "100%" }}>
      {/* Toggle button */}
      {hasOthers && (
        <div className="d-flex justify-content-end px-2 mb-1">
          <button
            className="btn btn-sm btn-outline-secondary"
            onClick={() => setShowFullGraph((v) => !v)}
            title={showFullGraph ? "Show neighbourhood only" : "Show full graph"}
          >
            <i
              className={`bi ${showFullGraph ? "bi-arrows-collapse" : "bi-arrows-expand"}`}
              aria-hidden="true"
            />
            <span className="ms-1 small">
              {showFullGraph ? "Neighbourhood" : "Full graph"}
            </span>
          </button>
        </div>
      )}

      <svg
        viewBox={`${minX} ${minY} ${vbW} ${vbH}`}
        style={{ width: "100%", height: "420px", cursor: "grab", display: "block" }}
        onWheel={onWheel}
        onMouseDown={onMouseDown}
        onMouseMove={onMouseMove}
        onMouseUp={onMouseUp}
        onMouseLeave={onMouseUp}
        aria-label="Context navigation graph"
        role="img"
      >
        <g transform={`translate(${transform.tx},${transform.ty}) scale(${transform.scale})`}>
          {/* Edges */}
          {graph.edges
            .filter((edge) => {
              const fromVisible = visibleNodes.some(
                (n) => (n.contextType as string) === (edge.from as string)
              );
              const toVisible = visibleNodes.some(
                (n) => (n.contextType as string) === (edge.to as string)
              );
              return fromVisible && toVisible;
            })
            .map((edge, i) => {
              const fromNode = visibleNodes.find(
                (n) => (n.contextType as string) === (edge.from as string)
              );
              const toNode = visibleNodes.find(
                (n) => (n.contextType as string) === (edge.to as string)
              );
              if (!fromNode || !toNode) return null;
              return (
                <line
                  key={i}
                  x1={fromNode.x}
                  y1={fromNode.y}
                  x2={toNode.x}
                  y2={toNode.y}
                  stroke="var(--bs-secondary-color, #6c757d)"
                  strokeWidth={1.5}
                  strokeOpacity={0.5}
                />
              );
            })}

          {/* Nodes */}
          {visibleNodes.map((node) => {
            const isCurrent  = node.role === "current";
            const isNeighbor = node.role === "neighbor";
            const isOther    = node.role === "other";

            const fill    = isCurrent  ? "var(--bs-primary, #0d6efd)"
                          : isNeighbor ? "var(--bs-info, #0dcaf0)"
                          : "var(--bs-secondary, #6c757d)";
            const opacity = isOther ? 0.4 : 1;
            const textFill = isCurrent  ? "#fff"
                           : isNeighbor ? "var(--bs-dark, #212529)"
                           : "#fff";
            const cursor  = isCurrent ? "default" : "pointer";
            const title   = `${node.label}\n${node.contextType as string}`;

            return (
              <g
                key={node.contextType as string}
                transform={`translate(${node.x},${node.y})`}
                style={{ opacity, cursor }}
                onClick={() => handleNodeClick(node)}
                role="button"
                aria-label={node.label}
              >
                <title>{title}</title>
                <circle
                  r={node.r}
                  fill={fill}
                  stroke={isCurrent ? "var(--bs-primary-border-subtle, #9ec5fe)" : "none"}
                  strokeWidth={isCurrent ? 3 : 0}
                />
                <text
                  textAnchor="middle"
                  dominantBaseline="middle"
                  fontSize={isCurrent ? 11 : 9}
                  fontWeight={isCurrent ? "bold" : "normal"}
                  fill={textFill}
                  style={{ pointerEvents: "none", userSelect: "none" }}
                >
                  {truncate(node.label, isCurrent ? 10 : 8)}
                </text>
              </g>
            );
          })}
        </g>
      </svg>
    </div>
  );
}

// ─── Helpers ──────────────────────────────────────────────────────────────────

function truncate(s: string, maxLen: number): string {
  return s.length > maxLen ? s.slice(0, maxLen - 1) + "…" : s;
}
