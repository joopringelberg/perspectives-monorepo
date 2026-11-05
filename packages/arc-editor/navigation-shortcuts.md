# ARC editor navigation shortcut inventory

Inventory for Monaco **0.52.2**, based on the imports, providers and commands in
[main.ts](src/main.ts), Monaco's installed keybinding registrations, and a browser
check of the editor's resolved macOS keybindings. This is the standalone ARC
editor, not VS Code: VS Code workbench shortcuts are not automatically available.
The inventory includes the explicitly enabled cursor undo/redo and command palette.

On macOS, Alt means Option. Function keys may require Fn, and the browser or OS
can intercept a shortcut before Monaco receives it. Chords separated by a comma
are pressed in sequence. Unless stated otherwise, the editor must have focus.

## Active cursor movement and selection

| Movement | macOS | Windows / Linux |
|---|---|---|
| Character left/right; visual line up/down | Arrow keys | Arrow keys |
| Additional single-step bindings | Ctrl+B/F (left/right), Ctrl+P/N (up/down) | None |
| First non-whitespace column / start of line (Home behaviour) | Home or Cmd+Left | Home |
| End of line | End or Cmd+Right | End |
| Absolute start/end of line | Ctrl+A / Ctrl+E | No default binding |
| Start/end of document | Cmd+Up / Cmd+Down | Ctrl+Home / Ctrl+End |
| One page up/down | PageUp / PageDown | PageUp / PageDown |
| Extend selection while moving | Shift+Arrows, Shift+Home/End, Cmd+Shift+Left/Right/Up/Down, Shift+PageUp/PageDown; Ctrl+Shift+A/E for absolute line boundaries | Shift+Arrows, Shift+Home/End, Ctrl+Shift+Home/End, Shift+PageUp/PageDown |
| Extend a rectangular selection | Cmd+Shift+Option+Arrows or PageUp/PageDown | Windows: Ctrl+Shift+Alt+Arrows or PageUp/PageDown; Linux: no default binding |
| Select all | Cmd+A | Ctrl+A |
| Undo last cursor movement | Cmd+U | Ctrl+U |
| Redo cursor movement | Cmd+R | Ctrl+R |

Scrolling is separate from moving the cursor:

| Scrolling without moving the cursor | macOS | Windows / Linux |
|---|---|---|
| One line up/down | Ctrl+PageUp / Ctrl+PageDown | Ctrl+Up / Ctrl+Down |
| One page up/down | Cmd+PageUp / Cmd+PageDown | Alt+PageUp / Alt+PageDown |

## Active search, semantic and diagnostic navigation

| Operation | macOS | Windows / Linux |
|---|---|---|
| Open Find | Cmd+F | Ctrl+F |
| Command palette | F1 | F1 |
| Next/previous match | Cmd+G / Cmd+Shift+G, or F3 / Shift+F3 | F3 / Shift+F3 |
| Next/previous match while the Find field has focus | Enter / Shift+Enter | Enter / Shift+Enter |
| Next/previous occurrence of the selection | Cmd+F3 / Cmd+Shift+F3 | Ctrl+F3 / Ctrl+Shift+F3 |
| Use selection as Find text | Cmd+E | No default binding |
| Go to definition | F12; Cmd+F12 is also registered for the web editor | F12; Ctrl+F12 is also registered for the web editor |
| Peek definition | Option+F12 | Windows: Alt+F12; Linux: Ctrl+Shift+F10 |
| Find references (peek) | Shift+F12 | Shift+F12 |
| Next/previous reference while the references peek is open | F4 / Shift+F4; also F12 / Shift+F12 | Same |
| Open selected reference from the results tree | Enter; also Cmd+Down | Enter |
| Switch focus between the reference tree and preview | Cmd+K, F2 | Ctrl+K, F2 |
| Close references peek / Find / diagnostic widget | Escape | Escape |
| Next/previous live diagnostic in the current file | Option+F8 / Option+Shift+F8 | Alt+F8 / Alt+Shift+F8 |
| Next/previous live diagnostic, including other loaded models | F8 / Shift+F8 | F8 / Shift+F8 |
| ARC jump-history back/forward | Option+Left / Option+Right | Alt+Left / Alt+Right |

Definition/reference navigation requires an available PDR language server.
Cmd/Ctrl+click also follows definitions. The definition-to-side chord
Cmd/Ctrl+K, F12 (or Cmd/Ctrl+K, Cmd/Ctrl+F12) is registered, but this editor's
opener reuses its single editor rather than opening a side-by-side editor.

**F8 is already enabled**, indirectly through the hover contribution's diagnostic
navigation import. It uses live Monaco markers, not the saved ArcFeedback text.
Clicking positioned ArcFeedback messages is a separate route to the main source.

### Navigation history is not cursor undo

Cursor undo/redo restores Monaco cursor selections and scroll positions within
the current model. Its history is cleared by text edits or model switches.
Redo uses a custom Cmd/Ctrl+R binding while the editor text has focus; outside
the editor this key retains its browser reload behaviour.

The ARC Back/Forward history records explicit jumps through the editor opener,
member/perspective lists, *Jump to Context of Role*, and positioned feedback.
It does not record every arrow key, mouse click, search match, fold navigation,
or same-file Monaco diagnostic movement. Diagnostic jumps to another loaded
model go through the opener and are recorded. Back/Forward buttons also work when focus is
outside Monaco; their keyboard bindings apply inside the editor.

The custom context-menu actions for role properties, context roles, perspectives,
and enclosing context have **no keyboard shortcuts** assigned.

## Loaded navigation commands without default shortcuts

The folding contribution registers `editor.gotoParentFold`,
`editor.gotoPreviousFold` and `editor.gotoNextFold`, but assigns no default keys.
They can be invoked from the F1 command palette or assigned custom bindings.
Collapse/expand shortcuts are active (Cmd+Option+[ / ] on macOS;
Ctrl+Shift+[ / ] on Windows/Linux), but are folding operations rather than
cursor movement.

## Standard Monaco navigation not enabled here

The editor uses `editor.api` plus selected contributions, not Monaco's full
`editor.main` bundle. These features exist in Monaco but are not loaded:

| Feature | Monaco default macOS shortcut | Monaco default Windows / Linux shortcut | What is missing |
|---|---|---|---|
| Previous/next word, including Shift selection variants | Option+Left/Right | Ctrl+Left/Right | `wordOperations` |
| Previous/next word part (camelCase/subword), including Shift variants | Ctrl+Option+Left/Right | No default binding | `wordPartOperations` |
| Jump to matching bracket | Cmd+Shift+Backslash | Ctrl+Shift+Backslash | `bracketMatching` |
| Go to line/column input | Ctrl+G | Ctrl+G | `standaloneGotoLineQuickAccess` |
| Go to symbol in current document | Cmd+Shift+O | Ctrl+Shift+O | `standaloneGotoSymbolQuickAccess` and an ARC document-symbol provider |
| Expand/shrink structural selection | Ctrl+Cmd+Shift+Right/Left; also Ctrl+Shift+Right/Left | Shift+Alt+Right/Left | `smartSelect`; an ARC selection-range provider would improve syntax-aware results |
| Next/previous highlighted symbol occurrence | F7 / Shift+F7 | F7 / Shift+F7 | `wordHighlighter`; an ARC document-highlight provider is needed for semantic highlighting |
| Add cursor above/below, select next occurrence | Cmd+Option+Up/Down; Cmd+D | Ctrl+Alt+Up/Down; Ctrl+D (Linux add-cursor: Alt+Shift+Up/Down) | `multicursor` |

**Important conflict:** Option+Left/Right currently belongs to ARC Back/Forward
on macOS. Enabling standard word movement requires choosing different history
shortcuts or otherwise resolving that conflict, not just importing the module.

Go to/peek **declaration**, **type definition**, and **implementation** are
registered by the already-loaded symbol-navigation contribution, but are
unavailable for ARC because no corresponding providers are registered.
Monaco's implementation-navigation defaults are Cmd/Ctrl+F12 and
Cmd/Ctrl+Shift+F12 (peek); declaration/type-definition commands have no separate
default keys. Providers would be needed, not merely more imports.

VS Code's workspace-wide Go to File, workspace-symbol search, editor-tab history,
and multi-editor layout are workbench features, not services supplied by this
standalone Monaco setup.
