# Federated Discovery with Perspectives and BYO-LLM

## Purpose
This note captures a distributed discovery architecture where published Perspectives contexts become discoverable items, while LLM usage stays user-sovereign. The design avoids a central LLM gateway and keeps authoritative semantics in Perspectives.

## Core Contract
Each discoverable item is a context instance with:
- A stable CUID identity.
- A type derived from ARC domain model and context type.
- A public projection containing both human-readable and structured attributes.
- A public binding from CUID to URL through a public role.
- Provenance from role occupancy and user-attributed construction history.
- Policy dimensions (language, rights, jurisdiction, trust) modeled as roles and properties.

## Federation Model
Hubs are autonomous and deterministic:
- Authority for locally published items.
- Optional cache and index for peer-sourced items.
- Signed lifecycle exchange: publish, update, unpublish, tombstone.
- Deterministic APIs for schema discovery, plan validation, search execution, and provenance proofs.

Storage posture per hub:
- Immutable event log by CUID and revision.
- Latest-state materialized view.
- Lexical index, vector index, and facet store.
- Provenance and trust evidence store.
- Tombstones as first-class records.

## LLM Positioning
LLM is client-side only (bring your own model/subscription):
- Planner runs in the user client.
- Local, versioned control-plane memory contains schema, model mappings, planner rules, and ranking policy.
- Hubs do not run or proxy LLMs.
- Hubs validate structured plans and execute deterministic retrieval.
- Result presentation is URL-grounded with provenance and trust explanation.

## Physical vs Informational Items
One hub software family can serve both categories, with different retrieval and placement profiles:

Physical goods:
- Geo and logistics constraints are hard early filters.
- Ingestion prefers nearby hubs.
- Query expansion is radius-based from local outward.

Informational products:
- Geography is weak or irrelevant.
- Federation emphasizes global coverage, canonicalization, and deduplication.
- Wider fan-out across trusted peers is typically beneficial.

## Federation Load Balancing
Load balancing is split into three decisions:
- Ingest placement.
- Index placement.
- Query routing.

Typical policy:
- Physical: distance + load + freshness.
- Informational: capacity + topic affinity + stable hashing.
- Background rebalancing migrates responsibility without changing CUID identity.

## Capacity Planning Baseline
Required hub count is estimated by the maximum of ingest, query, and storage constraints, multiplied by a safety factor for skew and failures:

H >= max( ingest_demand / ingest_capacity,
         query_demand / query_capacity,
         storage_demand / storage_capacity ) * safety_factor

With mixed item classes, compute physical and informational demand components separately, then aggregate.

## Key Decisions
- Use one interoperable hub type with profile-driven behavior.
- Keep control-plane authority local and versioned through Perspectives semantics.
- Keep LLM optional, user-provided, and outside the authoritative retrieval path.
- Preserve deterministic validation and execution at hubs for auditability and interoperability.

## Open Design Choices
- Provenance granularity: section-level vs field-level evidence.
- URL update policy: alias retention vs strict canonical replacement.
- Rights vocabulary: external standard vs Perspectives-native taxonomy.
- Planner governance: shared federation profile vs hub-local tuning.
