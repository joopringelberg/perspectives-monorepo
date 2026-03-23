---
applyTo: "packages/language-perspectives-arcII/**"
---

# language-perspectives-arcII – Copilot Instructions

## What This Package Is

`language-perspectives-arcII` is a **VS Code extension** that provides syntax coloring for the Perspectives ARC language. It is distributed as a `.vsix` file.

**Type**: VS Code language extension (TextMate grammar)  
**Language defined**: `perspectives-arc` (file extension `.arc`)

---

## Directory Structure

```
packages/language-perspectives-arcII/
├── syntaxes/                          # TextMate grammar file(s) for ARC
├── arc sources/                       # Example ARC model files
├── language-configuration.json        # VS Code language configuration (brackets, comments, etc.)
├── package.json                       # Extension manifest
├── settings.json                      # Recommended VS Code settings
├── language-perspectives-arc-0.0.2.vsix  # Packaged extension (binary)
└── README.md
```

---

## How It Works

The extension registers the `perspectives-arc` language in VS Code:
- File extension: `.arc`
- Grammar: TextMate grammar in `syntaxes/`
- Token scopes target **base16 color schemes** for broad theme compatibility

Keywords, types, operators, and comments in ARC are tokenized to appropriate TextMate scopes.

---

## Installing the Extension

Download the `.vsix` file and install via:
- VS Code: `Extensions` view → `...` menu → `Install from VSIX`
- Command palette: `Extensions: Install from VSIX`

---

## Development & Build

Requirements:
- `js-yaml` (local dev dependency)
- `yo` and `vsce` (global tools for scaffolding and packaging)

To package a new `.vsix`:
```bash
cd packages/language-perspectives-arcII
vsce package
```

---

## Updating the Grammar

When the ARC language gains new syntax (new keywords, constructs, etc.):
1. Edit the TextMate grammar file in `syntaxes/`
2. Test by pressing `F5` in VS Code to launch an Extension Development Host
3. Update version in `package.json`
4. Repackage with `vsce package`
5. Commit the new `.vsix` file and the grammar changes

---

## Common Pitfalls

1. **`.vsix` binary in version control** — this is intentional; the packaged extension is committed so users can download it directly from GitHub releases.
2. **`fileTypes` format** — some editors (e.g. Atom) require an array of names without leading dots. The YAML source handles this; ensure compatibility is maintained.
3. **Base16 theme targeting** — token scopes are chosen for base16 themes. Avoid scopes that only work with specific themes.
4. **No automated CI build** — the `.vsix` is built and committed manually. There is no automated build for this package.
