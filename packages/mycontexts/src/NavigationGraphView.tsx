/**
 * NavigationGraphView.tsx
 *
 * SVG renderer for the server-derived static model context navigation graph.
 *
 * The graph topology comes from the PDR (via getModelContextGraph) and is type-level:
 * nodes are context types, edges are ContextRole relationships.
 *
 * Instance data is overlaid from:
 *  - Downstream (children): context role instances in screenelements.contextRoles.
 *  - Upstream  (parents):  wider contexts returned by getWiderContexts, resolved to
 *    their context types via getContextType.
 *
 * Visual encoding:
 *  - Current node:    large, Bootstrap-primary fill.
 *  - Neighbour nodes: medium, Bootstrap-info fill (downstream) or Bootstrap-success (upstream).
 *  - Other nodes:     small, dimmed secondary fill.
 *
 * Multi-instance behaviour:
 *  - Single instance:  show instance readable name; click opens that context.
 *  - Multiple instances: show type label + stacked-circle icon; click opens a dropdown list.
 *  - No instances:     show type label only; node is non-navigable.
 *
 * Interactions:
 *  - Pan: click-drag on the SVG background.
 *  - Zoom: mouse-wheel.
 *  - Node click: dispatches an OpenContext CustomEvent on hostRef.
 */

import * as React from "react";
import { useEffect, useState, useRef, useCallback } from "react";
import {
  ContextType,
  RoleInstanceT,
  TableFormDef,
} from "perspectives-proxy";
import type { ModelContextGraph } from "perspectives-proxy";
import type { WiderContext } from "./navigationGraph";
import { ListGroup, Overlay, Popover } from "react-bootstrap";

// ─── Layout constants ─────────────────────────────────────────────────────────

const ROW_SPACING = 150;
const COL_SPACING = 110;
const R_CURRENT = 34;
const R_NEIGHBOR = 26;
const R_OTHER = 18;

// ─── Internal types ───────────────────────────────────────────────────────────

interface InstanceInfo {
  roleId: RoleInstanceT;
  readableName: string;
}

interface PositionedNode {
  id: string;
  label: string;
  x: number;
  y: number;
  r: number;
  role: "current" | "downstream" | "upstream" | "other";
  instances: InstanceInfo[];
}

interface Transform {
  tx: number;
  ty: number;
  scale: number;
}

// ─── Helpers ──────────────────────────────────────────────────────────────────

function truncate(s: string, maxLen: number): string {
  return s.length > maxLen ? s.slice(0, maxLen - 1) + "…" : s;
}

/** Compute a vertical radial layout. Returns positioned nodes. */
function computeLayout(
  graph: ModelContextGraph,
  currentType: string,
  downstreamInstances: Map<string, InstanceInfo[]>,
  upstreamInstances: Map<string, InstanceInfo[]>
): PositionedNode[] {
  if (graph.nodes.length === 0) return [];

  const nodeMap = new Map(graph.nodes.map((n) => [n.id, n]));

  // Direct downstream neighbours (current → neighbour edges).
  const downstreamIds = new Set(
    graph.edges.filter((e) => e.from === currentType).map((e) => e.to)
  );
  // Direct upstream neighbours (neighbour → current edges).
  const upstreamIds = new Set(
    graph.edges.filter((e) => e.to === currentType).map((e) => e.from)
  );

  const positioned: PositionedNode[] = [];

  // Row 0 – current node.
  const curNode = nodeMap.get(currentType);
  if (curNode) {
    positioned.push({
      ...curNode,
      x: 0,
      y: 0,
      r: R_CURRENT,
      role: "current",
      instances: [],
    });
  }

  // Row –1 – upstream nodes (above current).
  const upArr = Array.from(upstreamIds)
    .map((id) => nodeMap.get(id))
    .filter((n): n is (typeof n & NonNullable<typeof n>) => n !== undefined);
  upArr.forEach((n, i) => {
    const x =
      upArr.length === 1
        ? 0
        : Math.round((i - (upArr.length - 1) / 2) * COL_SPACING);
    positioned.push({
      ...n,
      x,
      y: -ROW_SPACING,
      r: R_NEIGHBOR,
      role: "upstream",
      instances: upstreamInstances.get(n.id) ?? [],
    });
  });

  // Row +1 – downstream nodes (below current).
  const downArr = Array.from(downstreamIds)
    .map((id) => nodeMap.get(id))
    .filter((n): n is (typeof n & NonNullable<typeof n>) => n !== undefined);
  downArr.forEach((n, i) => {
    const x =
      downArr.length === 1
        ? 0
        : Math.round((i - (downArr.length - 1) / 2) * COL_SPACING);
    positioned.push({
      ...n,
      x,
      y: ROW_SPACING,
      r: R_NEIGHBOR,
      role: "downstream",
      instances: downstreamInstances.get(n.id) ?? [],
    });
  });

  // Row ±2 – other nodes (not directly connected), shown dimmed.
  const neighborIds = new Set([...downstreamIds, ...upstreamIds, currentType]);
  const otherNodes = graph.nodes.filter((n) => !neighborIds.has(n.id));
  const oCount = otherNodes.length;
  otherNodes.forEach((n, i) => {
    const x =
      oCount === 1
        ? 0
        : Math.round((i - (oCount - 1) / 2) * COL_SPACING);
    positioned.push({
      ...n,
      x,
      y: ROW_SPACING * 2,
      r: R_OTHER,
      role: "other",
      instances: [],
    });
  });

  return positioned;
}

/**
 * Build downstream instance map from the Where screenelements.contextRoles.
 * Maps context type id → array of InstanceInfo.
 */
function buildDownstreamInstances(
  contextRoles: TableFormDef[]
): Map<string, InstanceInfo[]> {
  const result = new Map<string, InstanceInfo[]>();

  for (const def of contextRoles) {
    const perspective =
      def.table?.widgetCommonFields?.perspective ??
      def.form?.widgetCommonFields?.perspective;
    if (!perspective) continue;
    if (perspective.roleKind !== "ContextRole") continue;

    // contextTypesToCreate maps label → ContextType for the types reachable via this role.
    const typesToCreate = perspective.contextTypesToCreate ?? {};
    const targetTypes = Object.values(typesToCreate).filter(Boolean) as string[];

    // The existing role instances for this perspective.
    const roleInstances = perspective.roleInstances ?? {};
    const instances: InstanceInfo[] = Object.values(roleInstances).map(
      (ri) => ({ roleId: ri.roleId, readableName: ri.readableName })
    );

    for (const targetType of targetTypes) {
      const existing = result.get(targetType) ?? [];
      result.set(targetType, [...existing, ...instances]);
    }
  }

  return result;
}

/**
 * Build upstream instance map from the resolved WiderContexts.
 * Maps context type id → array of InstanceInfo (using externalRole as roleId for navigation).
 */
function buildUpstreamInstances(
  widerContexts: WiderContext[]
): Map<string, InstanceInfo[]> {
  const result = new Map<string, InstanceInfo[]>();
  for (const wc of widerContexts) {
    if (!wc.contextType) continue;
    const key = wc.contextType as string;
    const existing = result.get(key) ?? [];
    result.set(key, [
      ...existing,
      { roleId: wc.externalRole as RoleInstanceT, readableName: wc.readableName },
    ]);
  }
  return result;
}

// ─── Component ────────────────────────────────────────────────────────────────

export interface NavigationGraphViewProps {
  /** Server-derived type-level context navigation graph. */
  modelGraph: ModelContextGraph | undefined;
  /** The context type of the currently open context (highlighted node). */
  currentContextType: ContextType;
  /** Context role instances from screenelements.contextRoles (downstream population). */
  contextRoles: TableFormDef[];
  /** Wider contexts (upstream) with resolved context types. */
  widerContexts: WiderContext[];
  /** Host DOM element; OpenContext events are dispatched on it. */
  hostRef: React.RefObject<HTMLDivElement | null>;
}

export function NavigationGraphView({
  modelGraph,
  currentContextType,
  contextRoles,
  widerContexts,
  hostRef,
}: NavigationGraphViewProps) {

  const [showFullGraph, setShowFullGraph] = useState(false);
  const [transform, setTransform] = useState<Transform>({ tx: 0, ty: 0, scale: 1 });
  const dragging = useRef(false);
  const dragStart = useRef({ x: 0, y: 0, tx: 0, ty: 0 });

  // Popover state: which node is showing a multi-instance list.
  const [popoverNode, setPopoverNode] = useState<string | null>(null);
  const nodeRefs = useRef<Map<string, SVGGElement>>(new Map());

  // Re-center when graph changes.
  useEffect(() => {
    setTransform({ tx: 0, ty: 0, scale: 1 });
    setPopoverNode(null);
  }, [currentContextType, modelGraph]);

  const onWheel = useCallback((e: React.WheelEvent<SVGSVGElement>) => {
    e.preventDefault();
    setTransform((t) => ({
      ...t,
      scale: Math.min(4, Math.max(0.2, t.scale * (e.deltaY < 0 ? 1.1 : 0.9))),
    }));
  }, []);

  const onMouseDown = useCallback((e: React.MouseEvent<SVGSVGElement>) => {
    if ((e.target as SVGElement).tagName !== "svg") return;
    dragging.current = true;
    dragStart.current = { x: e.clientX, y: e.clientY, tx: transform.tx, ty: transform.ty };
  }, [transform]);

  const onMouseMove = useCallback((e: React.MouseEvent<SVGSVGElement>) => {
    if (!dragging.current) return;
    setTransform((t) => ({
      ...t,
      tx: dragStart.current.tx + (e.clientX - dragStart.current.x),
      ty: dragStart.current.ty + (e.clientY - dragStart.current.y),
    }));
  }, []);

  const onMouseUp = useCallback(() => { dragging.current = false; }, []);

  const handleNodeClick = useCallback(
    (node: PositionedNode) => {
      if (node.role === "current") return;
      if (node.instances.length === 0) return;
      if (node.instances.length === 1) {
        // Navigate directly.
        setPopoverNode(null);
        hostRef.current?.dispatchEvent(
          new CustomEvent("OpenContext", { detail: node.instances[0].roleId, bubbles: true })
        );
      } else {
        // Toggle popover.
        setPopoverNode((prev) => (prev === node.id ? null : node.id));
      }
    },
    [hostRef]
  );

  const navigateTo = useCallback(
    (roleId: RoleInstanceT) => {
      setPopoverNode(null);
      hostRef.current?.dispatchEvent(
        new CustomEvent("OpenContext", { detail: roleId, bubbles: true })
      );
    },
    [hostRef]
  );

  if (!modelGraph || modelGraph.nodes.length === 0) return null;

  const downstreamMap = buildDownstreamInstances(contextRoles);
  const upstreamMap = buildUpstreamInstances(widerContexts);
  const allPositioned = computeLayout(
    modelGraph,
    currentContextType as string,
    downstreamMap,
    upstreamMap
  );

  const hasOthers = allPositioned.some((n) => n.role === "other");
  const visibleNodes = showFullGraph
    ? allPositioned
    : allPositioned.filter((n) => n.role !== "other");

  // Compute viewBox.
  const margin = 70;
  const xs = visibleNodes.map((n) => n.x);
  const ys = visibleNodes.map((n) => n.y);
  const minX = (xs.length ? Math.min(...xs) : -COL_SPACING * 2) - margin;
  const maxX = (xs.length ? Math.max(...xs) : COL_SPACING * 2) + margin;
  const minY = (ys.length ? Math.min(...ys) : -ROW_SPACING) - margin;
  const maxY = (ys.length ? Math.max(...ys) : ROW_SPACING) + margin;
  const vbW = maxX - minX;
  const vbH = maxY - minY;

  return (
    <div className="navigation-graph-container mt-2" style={{ width: "100%" }}>
      {hasOthers && (
        <div className="d-flex justify-content-end px-2 mb-1">
          <button
            className="btn btn-sm btn-outline-secondary"
            onClick={() => setShowFullGraph((v) => !v)}
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
          {modelGraph.edges
            .filter((edge) => {
              const fromV = visibleNodes.some((n) => n.id === edge.from);
              const toV = visibleNodes.some((n) => n.id === edge.to);
              return fromV && toV;
            })
            .map((edge, i) => {
              const fromNode = visibleNodes.find((n) => n.id === edge.from);
              const toNode = visibleNodes.find((n) => n.id === edge.to);
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
                  markerEnd="url(#arrowhead)"
                />
              );
            })}

          {/* Arrow marker */}
          <defs>
            <marker
              id="arrowhead"
              markerWidth="6"
              markerHeight="4"
              refX="6"
              refY="2"
              orient="auto"
            >
              <polygon
                points="0 0, 6 2, 0 4"
                fill="var(--bs-secondary-color, #6c757d)"
                fillOpacity={0.5}
              />
            </marker>
          </defs>

          {/* Nodes */}
          {visibleNodes.map((node) => {
            const isCurrent = node.role === "current";
            const isDown = node.role === "downstream";
            const isUp = node.role === "upstream";
            const isOther = node.role === "other";
            const hasInstances = node.instances.length > 0;
            const isMulti = node.instances.length > 1;

            const fill = isCurrent
              ? "var(--bs-primary, #0d6efd)"
              : isDown
              ? "var(--bs-info, #0dcaf0)"
              : isUp
              ? "var(--bs-success, #198754)"
              : "var(--bs-secondary, #6c757d)";
            const opacity = isOther ? 0.35 : 1;
            const textFill = isCurrent || isUp ? "#fff" : isDown ? "var(--bs-dark, #212529)" : "#fff";
            const cursor = isCurrent || (!hasInstances && !isMulti) ? "default" : "pointer";

            // Display: instance name if single, type label otherwise.
            const displayLabel =
              !isCurrent && hasInstances && !isMulti
                ? truncate(node.instances[0].readableName, 10)
                : truncate(node.label, 10);

            return (
              <g
                key={node.id}
                ref={(el) => {
                  if (el) nodeRefs.current.set(node.id, el);
                  else nodeRefs.current.delete(node.id);
                }}
                transform={`translate(${node.x},${node.y})`}
                style={{ opacity, cursor }}
                onClick={() => handleNodeClick(node)}
                role={hasInstances ? "button" : undefined}
                aria-label={node.label}
              >
                <title>{`${node.label}\n${node.id}`}</title>
                <circle
                  r={node.r}
                  fill={fill}
                  stroke={isCurrent ? "var(--bs-primary-border-subtle, #9ec5fe)" : "none"}
                  strokeWidth={isCurrent ? 3 : 0}
                />
                {/* Multi-instance indicator: second circle slightly offset */}
                {isMulti && (
                  <circle
                    r={node.r * 0.55}
                    cx={node.r * 0.35}
                    cy={node.r * 0.35}
                    fill="none"
                    stroke="#fff"
                    strokeWidth={1.5}
                    strokeOpacity={0.8}
                  />
                )}
                <text
                  textAnchor="middle"
                  dominantBaseline="middle"
                  fontSize={isCurrent ? 11 : 9}
                  fontWeight={isCurrent ? "bold" : "normal"}
                  fill={textFill}
                  style={{ pointerEvents: "none", userSelect: "none" }}
                >
                  {displayLabel}
                </text>
              </g>
            );
          })}
        </g>
      </svg>

      {/* Multi-instance popover (rendered outside SVG for correct Bootstrap positioning) */}
      {popoverNode && (() => {
        const node = allPositioned.find((n) => n.id === popoverNode);
        if (!node || node.instances.length < 2) return null;
        return (
          <div
            className="position-absolute bg-white border rounded shadow-sm p-1"
            style={{ zIndex: 1050, minWidth: 160, top: "50%", left: "50%" }}
            key={popoverNode}
          >
            <div className="fw-bold small px-2 pt-1 pb-1 border-bottom">{node.label}</div>
            <ListGroup variant="flush">
              {node.instances.map((inst) => (
                <ListGroup.Item
                  key={inst.roleId}
                  action
                  className="small py-1 px-2"
                  onClick={() => navigateTo(inst.roleId as RoleInstanceT)}
                >
                  {inst.readableName}
                </ListGroup.Item>
              ))}
            </ListGroup>
          </div>
        );
      })()}
    </div>
  );
}
