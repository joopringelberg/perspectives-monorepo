---

name: PureScript MCP workflow
alwaysApply: true
description: Required workflow for PureScript symbol lookup, source inspection, and usage analysis in perspectives-core
-----------------------------------------------------------------------------------------------------------------------

# PureScript MCP workflow

The PureScript project root is:

`/Users/joopringelberg/Code/perspectives-monorepo/packages/perspectives-core`

## Server initialization

Before using a `pursIde...` tool:

1. Call `get_server_status`.
2. If the PureScript IDE server is not running, call `start_purs_ide_server` with the exact project root above.
3. Never use `/workspace` or invent another project path.
4. Do not restart the IDE server when the status shows that it is already running.

## Quick tool order

For most PureScript questions, use the smallest tool that gives enough evidence:

1. `get_server_status` → verify the IDE is available.
2. `start_purs_ide_server` → start it only if needed.
3. `pursIdeLoad` → load modules when the IDE has just started or after relevant code changes.
4. `pursIdeDefinitionText` → preferred for finding and understanding a declaration.
5. `pursIdeType` → use when only the type signature is needed.
6. `pursIdeUsages` → use when the question is about how a symbol is used.
7. `pursIdeList` → use for module/import inventory when the symbol or module is unknown.
8. `pursIdeRebuild` → use after local edits when the IDE needs a refresh.

Keep the workflow narrow. Do not fetch broad context when one targeted lookup will answer the question.

## General symbol lookup

For questions about PureScript functions, values, data types, constructors, classes, instances, modules, or operators:

1. Prefer the tools supplied by the `purescript-core` MCP server over `grep_search` or filename search.
2. Use only exact tool names exposed by Continue. Never guess tool names.
3. Prefer `pursIdeDefinitionText` when the goal is to find and understand a declaration.
4. Use `pursIdeType` only when type information is needed without the complete source declaration.
5. Do not claim to understand a declaration until its source text has been obtained.
6. If more than one symbol matches, inspect all plausible results before deciding which one is relevant.


## Data types and constructors

When asked to find a constructor belonging to a data type:

1. Use `pursIdeDefinitionText` to retrieve the complete data-type declaration.
2. Identify all constructors directly from the returned source text.
3. Locate the requested constructor within that declaration.
4. Report its arguments and relevant types.
5. If the requested constructor is absent, say so clearly.
6. Search for similarly named types or constructors only when that would help resolve a likely naming error.

When asked about a constructor without a known parent type:

1. Resolve the constructor with the MCP tools.
2. Retrieve its definition text.
3. Identify and report the data type to which it belongs.

## Usages

When asked for usages of a symbol:

1. Call `pursIdeUsages` with the exact:

   * module;
   * namespace;
   * identifier.
2. Read source around the relevant returned locations before explaining how the symbol is used.
3. Do not classify a usage based only on a filename and line number.
4. Prefer representative usages when the result set is large, but mention that the list was abbreviated.

## Following dependencies

When a question depends on following related symbols, do so in a narrow and evidence-driven way:

1. Start from the declaration source returned by `pursIdeDefinitionText`.
2. Identify only the immediate dependencies that are relevant to the question.
3. Resolve each referenced symbol one at a time with `pursIdeDefinitionText` or `pursIdeType`.
4. Use `get_function_call_tree` when the task is specifically about a forward call chain, but keep the depth and node budget small.
5. Do not expand into unrelated modules or broad transitive chains unless the user explicitly asks for that level of detail.
6. Stop once the inspected declarations are sufficient to explain the dependency path clearly and accurately.


## Reliability rules

1. Never invent PureScript declarations, constructors, modules, or usages.
2. Clearly distinguish verified source facts from inference.
3. Prefer semantic MCP tools over textual search for symbol identity.
4. Use textual search only for strings, comments, generated names, or when semantic lookup fails.
5. Keep the retrieved context focused because the active model has a limited context window.
6. Reuse already retrieved source within the current chat instead of fetching it repeatedly.
