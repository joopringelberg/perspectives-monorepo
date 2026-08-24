/**
 * NavigationGraphView.tsx
 *
 * React Flow-based renderer for the server-derived static model context
 * navigation graph.
 *
 * Architecture
 * ────────────
 * 1. graphNormalization.normalizeGraph()
 *    Converts raw PDR graph + client instance data → stable NormalizedGraph
 *    view-model with no rendering dependencies.
 *
 * 2. layoutSelector.selectLayout(policy)
 *    Returns a LayoutAdapter (ManualLayout default, DagreLayout variants).
 *    adapter.compute() returns per-node top-left coordinates + dimensions.
 *
 * 3. React Flow rendering
 *    Custom ContextNode renders circular nodes with Bootstrap CSS-var colours.
 *    Built-in pan/zoom/controls replace the previous hand-rolled SVG handlers.
 *
 * Visual semantics preserved from the previous SVG renderer
 * ──────────────────────────────────────────────────────────
 *  - current/upstream/downstream/other node roles → distinct colours/sizes.
 *  - Multi-instance indicator: stacked-disk offset shadow.
 *  - Indexed singleton: warning-coloured ring.
 *  - Single-instance click:   dispatches OpenContext directly.
 *  - Multi-instance click:    shows instance-list panel below the graph.
 *  - Indexed node click:      navigates immediately via externalRole.
 *  - Non-navigable nodes:     non-clickable (cursor: default).
 *  - Solid / dashed edges:    visibility rule preserved.
 *  - Edge role labels:        rendered via React Flow built-in label support.
 *  - Neighbourhood / full-graph toggle: retained.
 */

import * as React from "react";
import {
  useEffect,
  useLayoutEffect,
  useState,
  useCallback,
  useRef,
} from "react";
import {
  ReactFlow,
  Background,
  Controls,
  Handle,
  Position,
  MarkerType,
  BaseEdge,
  EdgeText,
} from "@xyflow/react";
import type {
  Node,
  Edge,
  EdgeProps,
  NodeProps,
  ReactFlowInstance,
} from "@xyflow/react";
import "@xyflow/react/dist/style.css";

import { ContextType, RoleInstanceT, TableFormDef } from "perspectives-proxy";
import type { ModelContextGraph, WiderContext } from "perspectives-proxy";
import { ListGroup } from "react-bootstrap";
import { i18next } from "perspectives-react";

import {
  normalizeGraph,
  indexedRoleId,
} from "./graphNormalization";
import type { NormalizedNode, NodeRole } from "./graphNormalization";
import { selectLayout } from "./layoutSelector";
import type { LayoutPolicy } from "./layoutSelector";

// ─── Visual constants (must match layoutAdapters/manualLayout.ts) ─────────────

const NODE_SIZE: Record<NodeRole, number> = {
  current: 68,
  upstream: 52,
  downstream: 52,
  user: 44,
  other: 36,
};

const NODE_FILL: Record<NodeRole, string> = {
  current: "var(--bs-primary, #0d6efd)",
  downstream: "var(--bs-info, #0dcaf0)",
  upstream: "var(--bs-success, #198754)",
  user: "var(--bs-success-bg-subtle, #d1e7dd)",
  other: "var(--bs-secondary, #6c757d)",
};

const NODE_TEXT_COLOR: Record<NodeRole, string> = {
  current: "#fff",
  downstream: "var(--bs-dark, #212529)",
  upstream: "#fff",
  user: "var(--bs-dark, #212529)",
  other: "#fff",
};

// ─── Custom node data contract ────────────────────────────────────────────────

interface ContextNodeData extends Record<string, unknown> {
  node: NormalizedNode;
}

interface GraphEdgeData extends Record<string, unknown> {
  laneOffset: number;
}

type CurvedGraphEdge = Edge<GraphEdgeData, "curvedGraph">;

interface PopoverAnchor {
  centerX: number;
  top: number;
  bottom: number;
}

interface PopoverPosition {
  left: number;
  top: number;
}

// ─── Custom circular node component ──────────────────────────────────────────

function ContextNode({ data }: NodeProps) {
  const { node } = data as ContextNodeData;
  const size = NODE_SIZE[node.role];
  const fill = NODE_FILL[node.role];
  const textColor = NODE_TEXT_COLOR[node.role];
  const opacity = node.role === "other" ? 0.35 : 1;
  const cursor = node.isNavigable ? "pointer" : "default";
  const isIndexed = Boolean(node.indexedName);

  // Invisible handles on every side allow edges to follow the direct route
  // between laid-out node centres.
  const handleStyle: React.CSSProperties = {
    opacity: 0,
    width: 0,
    height: 0,
    minWidth: 0,
    minHeight: 0,
    border: 0,
    background: "none",
  };

  // Border semantics:
  //   indexed     → warning ring
  //   current     → primary-border-subtle ring
  //   multi       → light separator to distinguish stacked disk
  //   otherwise   → none
  let border = "none";
  if (isIndexed) {
    border = "2.5px solid var(--bs-warning, #ffc107)";
  } else if (node.role === "current") {
    border = "3px solid var(--bs-primary-border-subtle, #9ec5fe)";
  } else if (node.isMulti) {
    border = "1.5px solid var(--bs-light, #f8f9fa)";
  }

  return (
    <div
      title={node.typeLabel}
      className="nopan"
      style={{
        width: size,
        height: size,
        borderRadius: "50%",
        backgroundColor: fill,
        opacity,
        cursor,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
        position: "relative",
        border,
        boxSizing: "border-box",
      }}
    >
      {/* Multi-instance indicator: offset shadow disk */}
      {node.isMulti && (
        <div
          style={{
            position: "absolute",
            width: size,
            height: size,
            borderRadius: "50%",
            backgroundColor: fill,
            border: "1.5px solid var(--bs-light, #f8f9fa)",
            top: size * 0.11,
            left: size * 0.11,
            zIndex: 0,
            boxSizing: "border-box",
          }}
        />
      )}

      {[Position.Top, Position.Right, Position.Bottom, Position.Left].map(
        (position) => (
          <React.Fragment key={position}>
            <Handle
              id={`target-${position}`}
              type="target"
              position={position}
              style={handleStyle}
              isConnectable={false}
            />
            <Handle
              id={`source-${position}`}
              type="source"
              position={position}
              style={handleStyle}
              isConnectable={false}
            />
          </React.Fragment>
        )
      )}

      <span
        style={{
          fontSize: node.role === "current" ? 11 : 9,
          fontWeight: node.role === "current" ? "bold" : "normal",
          color: textColor,
          pointerEvents: "none",
          userSelect: "none",
          textAlign: "center",
          padding: "0 4px",
          overflow: "hidden",
          zIndex: 1,
          position: "relative",
          lineHeight: 1.2,
          wordBreak: "break-word",
          maxWidth: size - 8,
        }}
      >
        {node.label}
      </span>
    </div>
  );
}

// Declared outside the component to prevent unnecessary re-creation on each
// render (React Flow warns if nodeTypes changes identity across renders).
const NODE_TYPES = { contextNode: ContextNode };

const positionVector: Record<Position, { x: number; y: number }> = {
  [Position.Top]: { x: 0, y: -1 },
  [Position.Right]: { x: 1, y: 0 },
  [Position.Bottom]: { x: 0, y: 1 },
  [Position.Left]: { x: -1, y: 0 },
};

function CurvedGraphEdge({
  sourceX,
  sourceY,
  sourcePosition,
  targetX,
  targetY,
  targetPosition,
  markerEnd,
  style,
  label,
  labelStyle,
  labelShowBg,
  labelBgStyle,
  labelBgPadding,
  labelBgBorderRadius,
  data,
}: EdgeProps<CurvedGraphEdge>) {
  const deltaX = targetX - sourceX;
  const deltaY = targetY - sourceY;
  const distance = Math.hypot(deltaX, deltaY);
  const controlDistance = Math.min(220, Math.max(90, distance * 0.55));
  const perpendicularX = distance === 0 ? 0 : -deltaY / distance;
  const perpendicularY = distance === 0 ? 0 : deltaX / distance;
  const laneOffset = data?.laneOffset ?? 0;
  const sourceVector = positionVector[sourcePosition];
  const targetVector = positionVector[targetPosition];
  const sourceControlX =
    sourceX + sourceVector.x * controlDistance + perpendicularX * laneOffset;
  const sourceControlY =
    sourceY + sourceVector.y * controlDistance + perpendicularY * laneOffset;
  const targetControlX =
    targetX + targetVector.x * controlDistance + perpendicularX * laneOffset;
  const targetControlY =
    targetY + targetVector.y * controlDistance + perpendicularY * laneOffset;
  const path = `M ${sourceX},${sourceY} C ${sourceControlX},${sourceControlY} ${targetControlX},${targetControlY} ${targetX},${targetY}`;
  const labelX =
    (sourceX + 3 * sourceControlX + 3 * targetControlX + targetX) / 8;
  const labelY =
    (sourceY + 3 * sourceControlY + 3 * targetControlY + targetY) / 8;

  return (
    <>
      <BaseEdge path={path} markerEnd={markerEnd} style={style} />
      {label && (
        <EdgeText
          x={labelX}
          y={labelY}
          label={label}
          labelStyle={labelStyle}
          labelShowBg={labelShowBg}
          labelBgStyle={labelBgStyle}
          labelBgPadding={labelBgPadding}
          labelBgBorderRadius={labelBgBorderRadius}
        />
      )}
    </>
  );
}

const EDGE_TYPES = { curvedGraph: CurvedGraphEdge };

// ─── Component props ──────────────────────────────────────────────────────────

export interface NavigationGraphViewProps {
  /** Server-derived type-level context navigation graph from the PDR. */
  modelGraph: ModelContextGraph | undefined;
  /** Context type of the currently open context (highlighted as current node). */
  currentContextType: ContextType;
  /** Optional human-readable title for the current context node label. */
  currentContextTitle?: string;
  /** ContextRole perspectives from screenelements.contextRoles (downstream). */
  contextRoles: TableFormDef[];
  /** Resolved wider (upstream) contexts. */
  widerContexts: WiderContext[];
  /** Host element; OpenContext CustomEvents are dispatched on this ref. */
  hostRef: React.RefObject<HTMLDivElement | null>;
  /**
   * Layout strategy.
   * - 'manual' (default) — semantic row layout; upstream above, downstream below.
   * - 'dagre-tb' / 'dagre-lr' / 'dagre-bt' / 'dagre-rl' — Dagre automatic layout.
   * Dagre variants are primarily for development experimentation.
   */
  layoutPolicy?: LayoutPolicy;
}

// ─── Component ────────────────────────────────────────────────────────────────

export function NavigationGraphView({
  modelGraph,
  currentContextType,
  currentContextTitle,
  contextRoles,
  widerContexts,
  hostRef,
  layoutPolicy = "manual",
}: NavigationGraphViewProps) {
  const [showFullGraph, setShowFullGraph] = useState(false);
  const [showUnavailableConnections, setShowUnavailableConnections] =
    useState(true);
  const [popoverNode, setPopoverNode] = useState<NormalizedNode | null>(null);
  const [popoverAnchor, setPopoverAnchor] = useState<PopoverAnchor | null>(null);
  const [popoverPosition, setPopoverPosition] =
    useState<PopoverPosition | null>(null);
  const canvasRef = useRef<HTMLDivElement>(null);
  const popoverRef = useRef<HTMLDivElement>(null);
  const flowInstanceRef = useRef<ReactFlowInstance | null>(null);
  const resizeFrameRef = useRef<number | null>(null);

  const closePopover = useCallback(() => {
    setPopoverNode(null);
    setPopoverAnchor(null);
  }, []);

  const dispatchOpenContext = useCallback(
    (roleId: RoleInstanceT) => {
      const target: EventTarget | null = hostRef.current ?? document.body;
      target?.dispatchEvent(
        new CustomEvent("OpenContext", { detail: roleId, bubbles: true })
      );
    },
    [hostRef]
  );

  // Reset local UI state when navigating to a different context.
  useEffect(() => {
    setShowFullGraph(false);
    setShowUnavailableConnections(true);
    setPopoverNode(null);
    setPopoverAnchor(null);
  }, [currentContextType, modelGraph]);

  useLayoutEffect(() => {
    const canvas = canvasRef.current;
    const popover = popoverRef.current;
    if (!canvas || !popover || !popoverAnchor) {
      setPopoverPosition(null);
      return;
    }

    const canvasRect = canvas.getBoundingClientRect();
    const popoverRect = popover.getBoundingClientRect();
    const margin = 12;
    const gap = 8;
    const visibleTop = Math.max(margin, -canvasRect.top + margin);
    const visibleBottom = Math.min(
      canvasRect.height - margin,
      window.innerHeight - canvasRect.top - margin
    );
    const below = popoverAnchor.bottom + gap;
    const above = popoverAnchor.top - gap - popoverRect.height;
    const top =
      below + popoverRect.height <= visibleBottom
        ? below
        : Math.max(visibleTop, above);
    const left = Math.min(
      Math.max(margin, popoverAnchor.centerX - popoverRect.width / 2),
      Math.max(margin, canvasRect.width - popoverRect.width - margin)
    );

    setPopoverPosition({ left, top });
  }, [popoverAnchor, popoverNode]);

  useEffect(() => {
    if (!popoverNode) return;

    const handlePointerDown = (event: PointerEvent) => {
      const target = event.target;
      if (!(target instanceof Element)) return;
      if (popoverRef.current?.contains(target)) return;
      if (target.closest(".react-flow__node")) return;
      closePopover();
    };
    const handleKeyDown = (event: KeyboardEvent) => {
      if (event.key === "Escape") closePopover();
    };

    document.addEventListener("pointerdown", handlePointerDown);
    document.addEventListener("keydown", handleKeyDown);
    return () => {
      document.removeEventListener("pointerdown", handlePointerDown);
      document.removeEventListener("keydown", handleKeyDown);
    };
  }, [closePopover, popoverNode]);

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;

    let previousWidth = canvas.clientWidth;
    let previousHeight = canvas.clientHeight;
    const observer = new ResizeObserver(([entry]) => {
      const { width, height } = entry.contentRect;
      if (width === previousWidth && height === previousHeight) return;
      previousWidth = width;
      previousHeight = height;
      closePopover();

      if (resizeFrameRef.current !== null) {
        cancelAnimationFrame(resizeFrameRef.current);
      }
      resizeFrameRef.current = requestAnimationFrame(() => {
        resizeFrameRef.current = null;
        void flowInstanceRef.current?.fitView({ padding: 0.3 });
      });
    });

    observer.observe(canvas);
    return () => {
      observer.disconnect();
      if (resizeFrameRef.current !== null) {
        cancelAnimationFrame(resizeFrameRef.current);
        resizeFrameRef.current = null;
      }
    };
  }, [closePopover]);

  const onFlowInit = useCallback((instance: ReactFlowInstance) => {
    flowInstanceRef.current = instance;
    requestAnimationFrame(() => {
      void instance.fitView({ padding: 0.3 });
    });
  }, []);

  // ─── Navigation callbacks ──────────────────────────────────────────────────

  const handleNodeClick = useCallback(
    (node: NormalizedNode, anchor: PopoverAnchor | null) => {
      if (node.role === "current" || !node.isNavigable) return;

      if (node.instances.length > 1) {
        // Multiple instances take precedence over indexed shortcuts.
        const shouldClose = popoverNode?.nodeKey === node.nodeKey;
        setPopoverNode(shouldClose ? null : node);
        setPopoverAnchor(shouldClose ? null : anchor);
        return;
      }

      setPopoverNode(null);
      setPopoverAnchor(null);

      // Indexed singleton: navigate immediately via the pre-bound context id.
      const rid = indexedRoleId(node);
      if (rid && node.instances.length === 0) {
        dispatchOpenContext(rid);
        return;
      }

      if (node.instances.length === 0) return;

      if (node.instances.length === 1) {
        dispatchOpenContext(node.instances[0].roleId);
      }
    },
    [dispatchOpenContext, popoverNode]
  );

  const navigateTo = useCallback(
    (roleId: RoleInstanceT) => {
      closePopover();
      dispatchOpenContext(roleId);
    },
    [closePopover, dispatchOpenContext]
  );

  const onFlowNodeClick = useCallback(
    (event: React.MouseEvent, rfNode: Node<ContextNodeData>) => {
      const clicked = rfNode.data?.node;
      if (!clicked) return;
      const canvasRect = canvasRef.current?.getBoundingClientRect();
      const nodeRect = event.currentTarget.getBoundingClientRect();
      const anchor = canvasRect
        ? {
            centerX: nodeRect.left - canvasRect.left + nodeRect.width / 2,
            top: nodeRect.top - canvasRect.top,
            bottom: nodeRect.bottom - canvasRect.top,
          }
        : null;
      handleNodeClick(clicked, anchor);
    },
    [handleNodeClick]
  );

  // ─── Early-exit guard ─────────────────────────────────────────────────────

  if (!modelGraph || modelGraph.nodes.length === 0) return null;

  // ─── Graph normalization ──────────────────────────────────────────────────

  const normalized = normalizeGraph(
    modelGraph,
    currentContextType,
    currentContextTitle,
    contextRoles,
    widerContexts
  );

  // ─── Layout computation ───────────────────────────────────────────────────

  const layoutAdapter = selectLayout(layoutPolicy);
  const contextEdges = normalized.edges.filter(
    (edge) => edge.roleKind === "ContextRole"
  );
  const layoutMap = layoutAdapter.compute(normalized.nodes, contextEdges);

  // ─── Neighbourhood / full-graph toggle ────────────────────────────────────

  const hasOthers = normalized.nodes.some((n) => n.role === "other");
  const visibleNodes = showFullGraph
    ? normalized.nodes
    : normalized.nodes.filter((n) => n.role !== "other");
  const visibleKeys = new Set(visibleNodes.map((n) => n.nodeKey));
  const isEdgeInScope = (edge: (typeof normalized.edges)[number]) =>
    visibleKeys.has(edge.sourceKey) && visibleKeys.has(edge.targetKey);
  const hasUnavailableConnections = normalized.edges.some(
    (edge) => isEdgeInScope(edge) && !edge.isVisible
  );

  // ─── React Flow nodes ─────────────────────────────────────────────────────

  const rfNodes: Node<ContextNodeData>[] = visibleNodes.map((n) => {
    const pos = layoutMap.get(n.nodeKey) ?? { x: 0, y: 0, width: 52, height: 52 };
    return {
      id: n.nodeKey,
      position: { x: pos.x, y: pos.y },
      data: { node: n },
      type: "contextNode",
      style: { width: pos.width, height: pos.height },
      draggable: false,
      selectable: false,
    };
  });

  const edgeHandles = (sourceKey: string, targetKey: string) => {
    const source = layoutMap.get(sourceKey);
    const target = layoutMap.get(targetKey);
    if (!source || !target) {
      return {
        sourceHandle: `source-${Position.Bottom}`,
        targetHandle: `target-${Position.Top}`,
      };
    }

    const deltaX = target.x + target.width / 2 - (source.x + source.width / 2);
    const deltaY = target.y + target.height / 2 - (source.y + source.height / 2);
    if (Math.abs(deltaX) > Math.abs(deltaY)) {
      const sourcePosition = deltaX > 0 ? Position.Right : Position.Left;
      const targetPosition = deltaX > 0 ? Position.Left : Position.Right;
      return {
        sourceHandle: `source-${sourcePosition}`,
        targetHandle: `target-${targetPosition}`,
      };
    }

    const sourcePosition = deltaY > 0 ? Position.Bottom : Position.Top;
    const targetPosition = deltaY > 0 ? Position.Top : Position.Bottom;
    return {
      sourceHandle: `source-${sourcePosition}`,
      targetHandle: `target-${targetPosition}`,
    };
  };

  // ─── React Flow edges ─────────────────────────────────────────────────────

  const activeEdges = normalized.edges.filter(
    (edge) =>
      isEdgeInScope(edge) &&
      (showUnavailableConnections || edge.isVisible)
  );
  const edgeGroups = new Map<string, typeof activeEdges>();
  for (const edge of activeEdges) {
    const pair = [edge.sourceKey, edge.targetKey].sort().join("|");
    edgeGroups.set(pair, [...(edgeGroups.get(pair) ?? []), edge]);
  }

  const rfEdges: CurvedGraphEdge[] = activeEdges.map((e) => {
    const pair = [e.sourceKey, e.targetKey].sort().join("|");
    const parallelEdges = edgeGroups.get(pair) ?? [e];
    const laneIndex = parallelEdges.findIndex(
      (edge) => edge.edgeKey === e.edgeKey
    );
    const laneOffset = (laneIndex - (parallelEdges.length - 1) / 2) * 34;
    const handles = edgeHandles(e.sourceKey, e.targetKey);
    const edgeColor =
      e.roleKind === "UserRole"
        ? "var(--bs-success, #198754)"
        : "var(--bs-secondary-color, #6c757d)";
    return {
      id: e.edgeKey,
      source: e.sourceKey,
      target: e.targetKey,
      sourceHandle: handles.sourceHandle,
      targetHandle: handles.targetHandle,
      data: { laneOffset },
      // Only show role label when non-empty.
      label: e.roleLabel || undefined,
      style: {
        stroke: edgeColor,
        strokeWidth: 1.5,
        strokeOpacity: 0.5,
        strokeDasharray: e.isVisible ? undefined : "4 3",
      },
      labelStyle: {
        fontSize: 8,
        fill: edgeColor,
      },
      labelBgStyle: {
        fill: "var(--bs-body-bg, #fff)",
        fillOpacity: 0.8,
      },
      markerEnd: {
        type: MarkerType.Arrow,
        color: edgeColor,
        width: 12,
        height: 12,
      },
      type: "curvedGraph",
      selectable: false,
    };
  });

  // ─── Render ───────────────────────────────────────────────────────────────

  return (
    <div
      className="navigation-graph-container mt-2"
      style={{
        width: "100%",
        display: "flex",
        flexDirection: "column",
        height:
          "calc(100dvh - var(--bottom-navbar-height) - var(--top-navbar-height) - var(--who-header-height))",
        minHeight: "400px",
      }}
    >
      {/* Graph visibility toggles */}
      {(hasOthers || hasUnavailableConnections) && (
        <div className="d-flex justify-content-end gap-2 px-2 mb-1">
          {hasUnavailableConnections && (
            <button
              className="btn btn-sm btn-outline-secondary"
              onClick={() => setShowUnavailableConnections((value) => !value)}
              aria-pressed={showUnavailableConnections}
            >
              <i
                className={`bi ${
                  showUnavailableConnections ? "bi-eye-slash" : "bi-eye"
                }`}
                aria-hidden="true"
              />
              <span className="ms-1 small">
                {showUnavailableConnections
                  ? i18next.t("navigationGraph_hideUnavailable", { ns: "mycontexts" })
                  : i18next.t("navigationGraph_showUnavailable", { ns: "mycontexts" })}
              </span>
            </button>
          )}
          {hasOthers && (
            <button
              className="btn btn-sm btn-outline-secondary"
              onClick={() => setShowFullGraph((v) => !v)}
            >
              <i
                className={`bi ${
                  showFullGraph ? "bi-arrows-collapse" : "bi-arrows-expand"
                }`}
                aria-hidden="true"
              />
              <span className="ms-1 small">
                {showFullGraph
                  ? i18next.t("navigationGraph_neighbourhood", { ns: "mycontexts" })
                  : i18next.t("navigationGraph_fullGraph", { ns: "mycontexts" })}
              </span>
            </button>
          )}
        </div>
      )}

      {/* React Flow canvas */}
      <div
        ref={canvasRef}
        style={{ flex: 1, minHeight: 300, position: "relative" }}
      >
        <ReactFlow
          /* Re-mount (and re-run fitView) whenever the current context or
             layout policy changes. */
          key={`${String(currentContextType)}:${layoutPolicy}`}
          nodes={rfNodes}
          edges={rfEdges}
          nodeTypes={NODE_TYPES}
          edgeTypes={EDGE_TYPES}
          onInit={onFlowInit}
          onNodeClick={onFlowNodeClick}
          fitView
          fitViewOptions={{ padding: 0.3 }}
          nodesDraggable={false}
          nodesConnectable={false}
          elementsSelectable={false}
          panOnDrag
          zoomOnScroll
          style={{ background: "transparent" }}
          aria-label="Context navigation graph"
        >
          <Background />
          <Controls showInteractive={false} />
        </ReactFlow>

        {/* Multi-instance selection panel overlays the canvas so opening it
            does not resize and redraw React Flow. */}
        {popoverNode && (
          <div
            ref={popoverRef}
            className="nowheel nopan border rounded p-2 bg-body shadow-sm"
            style={{
              position: "absolute",
              top: popoverPosition?.top ?? 0,
              left: popoverPosition?.left ?? 0,
              zIndex: 10,
              width: "min(320px, calc(100% - 24px))",
              maxHeight:
                "min(200px, calc(100dvh - var(--top-navbar-height) - 48px))",
              overflowY: "auto",
              visibility: popoverPosition ? "visible" : "hidden",
            }}
          >
            <div className="fw-semibold small mb-1">
              {popoverNode.typeLabel}
            </div>
            <ListGroup variant="flush">
              {popoverNode.instances.map((inst) => (
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
          </div>
        )}
      </div>
    </div>
  );
}
