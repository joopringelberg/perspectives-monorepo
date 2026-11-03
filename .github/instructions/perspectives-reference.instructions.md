---
description: "Use when editing the Perspectives Language Reference Manual, AsciiDoc chapters, ARC syntax examples, or the reference documentation build."
applyTo: "packages/perspectives-reference/**"
---

# perspectives-reference - Copilot Instructions

## What This Package Is

`perspectives-reference` contains the editable **AsciiDoc** sources for the **Perspectives Language Reference Manual**. It is a private workspace package, not part of the PDR runtime. HTML and PDF output are generated locally and are not versioned.

See the [package README](../../packages/perspectives-reference/README.md) for setup and build details.

## Directory Structure

- `overview.adoc`: book entry point, document attributes, and chapter includes.
- Topic `.adoc` files: language syntax and semantics, including contexts, roles, properties, queries, perspectives, states, assignments, actions, and screens.
- Query subtopics such as `literalValues.adoc`, `comparisonOperators.adoc`, and `letExpressions.adoc`: included through `queries.adoc`.
- External-library chapters: utilities, CouchDB, serialisation, parsing, sensors, RabbitMQ, and files.
- `keywords.adoc`: keyword reference.
- `images/`, `adoc-colony.css`, and `extrastyles.css`: assets and styling.
- `scripts/`: build and validation tooling.
- `output/` and `output-pdf/`: ignored, generated HTML and PDF.

## Authoring Guidelines

- Write documentation in English and follow the terminology and AsciiDoc conventions of the surrounding chapter.
- Edit the relevant source chapter, never generated HTML or PDF. Update `overview.adoc` when adding or reorganising chapters; preserve nested includes such as those in `queries.adoc`.
- Preserve existing anchors, cross-references, and relative asset paths. When moving or renaming sections, update affected references.
- Use `perspectivesarc` source blocks for ARC examples. Indentation is meaningful in ARC; follow the two-space nesting convention documented in `conventions.adoc`.
- Keep syntax descriptions, examples, lexical rules, and the keyword reference consistent when a language change affects multiple chapters.
- Confirm implementation-sensitive claims against the parser and runtime in [perspectives-core](../../packages/perspectives-core/), rather than documenting intended but unimplemented behaviour. Consult the [ARC language instructions](arc-language.instructions.md) for model semantics and the [core instructions](perspectives-core.instructions.md) for implementation guidance.
- If an ARC syntax change affects highlighting, also consult the [Highlight.js instructions](perspectives-highlightjs.instructions.md) and [VS Code grammar instructions](language-perspectives-arcII.instructions.md).

## Build and Validation

Run these commands from the monorepo root:

```bash
# Initial Ruby/Bundler dependency setup, when needed
pnpm --filter perspectives-reference setup

# Build HTML and its workspace highlighter dependency
pnpm --filter perspectives-reference... build

# Validate generated pages, local assets, links, and browser highlighting
pnpm --filter perspectives-reference test

# Generate PDF when changing PDF-specific content or layout
pnpm --filter perspectives-reference build:pdf
```

The `...` in the HTML build selector includes the `perspectives-highlightjs` workspace dependency. For documentation changes, build HTML before running the reference tests. Inspect the affected generated pages when changing layout or highlighting.

HTML starts at `output/overview.html`; PDF output is `output-pdf/overview.pdf`. Do not commit generated output.
