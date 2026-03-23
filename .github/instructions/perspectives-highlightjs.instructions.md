---
applyTo: "packages/perspectives-highlightjs/**"
---

# perspectives-highlightjs – Copilot Instructions

## What This Package Is

`perspectives-highlightjs` provides a **Highlight.js language definition** for the Perspectives ARC language. It allows syntax highlighting of ARC code in web pages and React applications.

**Language**: TypeScript / JavaScript  
**Build tool**: Rollup  
**Output**: `dist/` (UMD module)

---

## Directory Structure

```
packages/perspectives-highlightjs/
├── src/                    # ARC language definition for highlight.js
├── test/                   # Tests
├── dist/                   # Build output (UMD)
├── rollup.config.js
├── tsconfig.json
└── package.json
```

---

## Usage

### In an HTML page
```html
<script src="path/to/highlight.js"></script>
<script src="path/to/perspectives-arc.js"></script>
<script>
  hljs.registerLanguage("perspectives-arc", perspectivesarc.default);
  hljs.highlightAll();
</script>
```

### In React (via `perspectives-react`)
`perspectives-react` imports this package and uses it for ARC code display in the `arcViewer.tsx` component.

---

## Build

```bash
cd packages/perspectives-highlightjs
pnpm run build    # Rollup → dist/
pnpm run watch    # Watch mode
```

---

## Common Pitfalls

1. **UMD output** — the dist file is a UMD module that works both as a `<script>` tag global and as a CommonJS/ESM import.
2. **Language definition maintenance** — when new ARC syntax is added (new keywords, operators, etc. in `perspectives-core/src/arcParser/`), update the tokenizer in `src/` to match.
3. **Consumed by `perspectives-react`** — any changes here require rebuilding `perspectives-react` to take effect in the UI.
