---
applyTo: "packages/perspectives-core/**/*.purs"
---

# PureScript code intelligence

When working in `packages/perspectives-core`, always use the tools from the
PureScript MCP server for symbol definitions, usages, types and dependency
analysis.

Do not answer symbol or call-relationship questions from grep or general
code search alone.

## Server initialization

Before using a `pursIde...` tool:

1. Call `get_server_status`.
2. If the PureScript IDE server is not running, first invoke
`start_purs_ide_server` with the `perspectives-core` project directory.

## Very useful tools
1. `pursIdeDefinitionText` → preferred for finding and understanding a declaration.
2. `pursIdeType` → use when only the type signature is needed.
3. `pursIdeUsages` → use when the question is about how a symbol is used.
4. `get_function_call_tree` → builds a bounded forward call tree for a project-defined PureScript function. Only calls to functions defined in the project source are included, and the tree is capped by depth and node count.