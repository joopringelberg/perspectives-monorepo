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
  nodeKey: string;
  id: ContextType;
  typeLabel: string;
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
  currentType: ContextType,
  currentNodeTitle: string | undefined,
  downstreamInstances: Map<string, InstanceInfo[]>,
  upstreamInstances: Map<string, InstanceInfo[]>
): PositionedNode[] {
  if (graph.nodes.length === 0) return [];

  const instancesForNode = (
    map: Map<string, InstanceInfo[]>,
    node: { id: ContextType; label: string }
  ): InstanceInfo[] => map.get(node.id) ?? map.get(node.label) ?? [];

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

  const makeNodeKey = (role: PositionedNode["role"], id: ContextType, index: number) =>
    `${role}:${String(id)}:${index}`;

  // Row 0 – current node.
  const curNode = nodeMap.get(currentType);
  if (curNode) {
    positioned.push({
      nodeKey: makeNodeKey("current", curNode.id, 0),
      ...curNode,
      typeLabel: curNode.label,
      label: currentNodeTitle?.trim() ? currentNodeTitle : curNode.label,
      x: 0,
      y: 0,
      r: R_CURRENT,
      role: "current",
      instances: [],
    });
  }

  // Row –1 – upstream nodes (above current).
  const upArr = Array.from(upstreamIds)
    .filter((id) => id !== currentType)
    .map((id) => nodeMap.get(id))
    .filter((n): n is (typeof n & NonNullable<typeof n>) => n !== undefined);
  upArr.forEach((n, i) => {
    const x =
      upArr.length === 1
        ? 0
        : Math.round((i - (upArr.length - 1) / 2) * COL_SPACING);
    positioned.push({
      nodeKey: makeNodeKey("upstream", n.id, i),
      ...n,
      typeLabel: n.label,
      x,
      y: -ROW_SPACING,
      r: R_NEIGHBOR,
      role: "upstream",
      instances: instancesForNode(upstreamInstances, n),
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
      nodeKey: makeNodeKey("downstream", n.id, i),
      ...n,
      typeLabel: n.label,
      x,
      y: ROW_SPACING,
      r: R_NEIGHBOR,
      role: "downstream",
      instances: instancesForNode(downstreamInstances, n),
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
      nodeKey: makeNodeKey("other", n.id, i),
      ...n,
      typeLabel: n.label,
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
    const targetTypes = Object.values(typesToCreate).filter(
      (value): value is ContextType => Boolean(value)
    );

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
    const existing = result.get(wc.contextType) ?? [];
    result.set(wc.contextType, [
      ...existing,
      { roleId: wc.externalRole, readableName: wc.readableName },
    ]);
  }
  return result;
}

/**
 * Collect role type ids represented in where.contextRoles perspectives.
 * Edges whose roleId is not present here are considered invisible for the current user.
 */
function buildVisibleEdgeRoleTypes(contextRoles: TableFormDef[]): Set<string> {
  const result = new Set<string>();

  for (const def of contextRoles) {
    const perspective =
      def.table?.widgetCommonFields?.perspective ??
      def.form?.widgetCommonFields?.perspective;
    if (!perspective) continue;
    if (perspective.roleKind !== "ContextRole") continue;
    result.add(perspective.roleType);
  }

  return result;
}

// ─── Component ────────────────────────────────────────────────────────────────

export interface NavigationGraphViewProps {
  /** Server-derived type-level context navigation graph. */
  modelGraph: ModelContextGraph | undefined;
  /** The context type of the currently open context (highlighted node). */
  currentContextType: ContextType;
  /** Optional screen title to display as the current node label. */
  currentContextTitle?: string;
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
  currentContextTitle,
  contextRoles,
  widerContexts,
  hostRef,
}: NavigationGraphViewProps) {

  const [showFullGraph, setShowFullGraph] = useState(false);
  const [transform, setTransform] = useState<Transform>({ tx: 0, ty: 0, scale: 1 });
  const dragging = useRef(false);
  const dragStart = useRef({ x: 0, y: 0, tx: 0, ty: 0 });

  // Popover state: which node is showing a multi-instance list.
  const [popoverNode, setPopoverNode] = useState<ContextType | null>(null);
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
      scale: Math.min(4, Math.max(0.2, t.scale * (e.deltaY < 0 ? 1.02 : 0.98))),
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
  const visibleEdgeRoleTypes = buildVisibleEdgeRoleTypes(contextRoles);
  const widerContextTypes = new Set<string>(
    widerContexts
      .map((wc) => wc.contextType)
      .filter((ct): ct is ContextType => Boolean(ct))
      .map((ct) => String(ct))
  );
  const allPositioned = computeLayout(
    modelGraph,
    currentContextType,
    currentContextTitle,
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
    <div
      className="navigation-graph-container mt-2"
      style={{
        width: "100%",
        display: "flex",
        flexDirection: "column",
        height:
          "calc(100dvh - var(--bottom-navbar-height) - var(--top-navbar-height) - var(--who-header-height))",
        overflowY: "auto",
      }}
    >
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
        style={{
          width: "100%",
          height: "100%",
          minHeight: "630px",
          flex: "1 1 auto",
          cursor: "grab",
          display: "block",
        }}
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
              const fromNode =
                edge.from === currentContextType
                  ? visibleNodes.find((n) => n.id === edge.from && n.role === "current") ??
                    visibleNodes.find((n) => n.id === edge.from)
                  : visibleNodes.find((n) => n.id === edge.from);
              const toNode =
                edge.from === currentContextType && edge.to === currentContextType
                  ? visibleNodes.find((n) => n.id === edge.to && n.role === "downstream") ??
                    visibleNodes.find((n) => n.id === edge.to)
                  : visibleNodes.find((n) => n.id === edge.to);
              if (!fromNode || !toNode) return null;
              const isSelfLoop = edge.from === edge.to;
              const isVisibleByRole = visibleEdgeRoleTypes.has(String(edge.roleId));
              const isVisibleByWiderContextEdge =
                (edge.from === currentContextType &&
                  widerContextTypes.has(String(edge.to))) ||
                (edge.to === currentContextType &&
                  widerContextTypes.has(String(edge.from)));
              const isInvisibleEdge = !(isVisibleByRole || isVisibleByWiderContextEdge);
              const strokeDasharray = isInvisibleEdge ? "4 3" : undefined;

              if (isSelfLoop) {
                const midX = (fromNode.x + toNode.x) / 2;
                const midY = (fromNode.y + toNode.y) / 2 - 8;

                return (
                  <React.Fragment key={i}>
                    <line
                      x1={fromNode.x}
                      y1={fromNode.y}
                      x2={toNode.x}
                      y2={toNode.y}
                      stroke="var(--bs-secondary-color, #6c757d)"
                      strokeWidth={1.5}
                      strokeOpacity={0.5}
                      strokeDasharray={strokeDasharray}
                      markerEnd="url(#arrowhead)"
                    />
                    <text
                      x={midX}
                      y={midY}
                      textAnchor="middle"
                      dominantBaseline="middle"
                      fontSize={8}
                      fill="var(--bs-secondary-color, #6c757d)"
                      stroke="var(--bs-body-bg, #fff)"
                      strokeWidth={3}
                      paintOrder="stroke"
                      style={{ pointerEvents: "none", userSelect: "none" }}
                    >
                      {edge.roleLabel}
                    </text>
                  </React.Fragment>
                );
              }

              const midX = (fromNode.x + toNode.x) / 2;
              const midY = (fromNode.y + toNode.y) / 2 - 8;
              return (
                <React.Fragment key={i}>
                  <line
                    x1={fromNode.x}
                    y1={fromNode.y}
                    x2={toNode.x}
                    y2={toNode.y}
                    stroke="var(--bs-secondary-color, #6c757d)"
                    strokeWidth={1.5}
                    strokeOpacity={0.5}
                    strokeDasharray={strokeDasharray}
                    markerEnd="url(#arrowhead)"
                  />
                  <text
                    x={midX}
                    y={midY}
                    textAnchor="middle"
                    dominantBaseline="middle"
                    fontSize={8}
                    fill="var(--bs-secondary-color, #6c757d)"
                    stroke="var(--bs-body-bg, #fff)"
                    strokeWidth={3}
                    paintOrder="stroke"
                    style={{ pointerEvents: "none", userSelect: "none" }}
                  >
                    {edge.roleLabel}
                  </text>
                </React.Fragment>
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
            const multiStroke = "var(--bs-light, #f8f9fa)";

            // Display: instance name if single, type label otherwise.
            const displayLabel =
              !isCurrent && hasInstances && !isMulti
                ? truncate(node.instances[0].readableName, 10)
                : truncate(node.label, 10);

            return (
              <g
                key={node.nodeKey}
                ref={(el) => {
                  if (el) nodeRefs.current.set(node.nodeKey, el);
                  else nodeRefs.current.delete(node.nodeKey);
                }}
                transform={`translate(${node.x},${node.y})`}
                style={{ opacity, cursor }}
                onClick={() => handleNodeClick(node)}
                role={hasInstances ? "button" : undefined}
                aria-label={node.label}
              >
                <title>{node.typeLabel}</title>
                {/* Multi-instance indicator: two stacked disks with a slight southeast offset. */}
                {isMulti && (
                  <circle
                    r={node.r}
                    cx={node.r * 0.22}
                    cy={node.r * 0.22}
                    fill={fill}
                    stroke={multiStroke}
                    strokeWidth={1.5}
                    strokeOpacity={1}
                  />
                )}
                <circle
                  r={node.r}
                  fill={fill}
                  stroke={isMulti ? multiStroke : isCurrent ? "var(--bs-primary-border-subtle, #9ec5fe)" : "none"}
                  strokeWidth={isMulti ? 1.5 : isCurrent ? 3 : 0}
                />
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

      {/* Multi-instance popover anchored to the clicked node. */}
      {popoverNode && (() => {
        const node = allPositioned.find((n) => n.id === popoverNode);
        const targetNode =
          allPositioned.find((n) => n.id === popoverNode && n.role !== "current") ??
          allPositioned.find((n) => n.id === popoverNode);
        const target = targetNode ? nodeRefs.current.get(targetNode.nodeKey) ?? null : null;
        if (!node || node.instances.length < 2 || !target) return null;
        return (
          <Overlay
            key={popoverNode}
            show
            target={target}
            placement="auto"
            popperConfig={{
              strategy: "fixed",
              modifiers: [
                {
                  name: "offset",
                  options: { offset: [0, 8] },
                },
                {
                  name: "flip",
                  options: {
                    fallbackPlacements: ["top", "right", "left", "bottom"],
                    padding: 8,
                  },
                },
                {
                  name: "preventOverflow",
                  options: {
                    boundary: "viewport",
                    padding: 8,
                  },
                },
              ],
            }}
            rootClose
            onHide={() => setPopoverNode(null)}
          >
            {(props) => (
              <Popover
                id={`navigation-graph-popover-${String(popoverNode)}`}
                {...props}
              >
                <Popover.Header as="h3" className="small">
                  {node.label}
                </Popover.Header>
                <Popover.Body className="p-0">
                  <ListGroup variant="flush">
                    {node.instances.map((inst) => (
                      <ListGroup.Item
                        key={inst.roleId}
                        action
                        className="small py-1 px-2"
                        onClick={() => navigateTo(inst.roleId)}
                      >
                        {inst.readableName}
                      </ListGroup.Item>
                    ))}
                  </ListGroup>
                </Popover.Body>
              </Popover>
            )}
          </Overlay>
        );
      })()}
    </div>
  );
}
