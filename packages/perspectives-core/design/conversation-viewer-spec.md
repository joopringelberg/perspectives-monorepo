# Conversation Viewer - Interaction and Integration Specification

## 1. Introduction

This document specifies the MyContexts help mode and the conversation viewer that presents model help conversations. Conversation content, model bindings, compilation, and the `conversations.json` runtime artifact are specified in [Model Conversation Documents - YAML Specification](model-conversation-documents-spec.md). The grammar of a conversation body is specified in [Conversational Structure - YAML Specification](conversational-yaml-spec.md).

The viewer is a non-modal, movable window modelled after a chat interface. It shows the conversation history above the currently available human answers. The user can keep the underlying context visible, move the viewer away from the object being discussed, and continue working after closing it.

The first supported help targets are:

- the context-window title in the top navigation bar;
- the header of a TableForm table.

Individual form fields and other screen elements may become targets later. The target protocol must therefore identify semantic model objects rather than particular DOM structures.

---

## 2. Goals and Non-Goals

### 2.1 Goals

- Make model help available by selecting an object in the current context.
- Preserve the role-sensitive lookup defined by the model conversation documents.
- Present statements, questions, and selected answers as a readable conversation history.
- Keep answer options visually and semantically separate from history.
- Allow the viewer to be moved and closed without navigating away from the context.
- Support pointer and keyboard interaction.
- Keep help-target components independent from conversation playback.

### 2.2 Non-goals

- Free-form user text or natural-language generation.
- Persisting conversation progress across browser sessions.
- Editing conversation source material.
- Displaying several conversation viewers simultaneously.
- Defining future toolbar tools beyond reserving a place for them.

---

## 3. Help Mode

The viewer is hidden by default. Selecting the application's help tool enters **help mode** without immediately opening a conversation.

While help mode is active:

- supported targets receive a visible help affordance and a help cursor;
- selecting a target requests its conversation;
- the target's normal click action is suppressed for that selection;
- unsupported content continues to behave normally;
- the application provides a clear way to leave help mode.

Help mode and viewer visibility are separate state. Closing the viewer does not have to leave help mode: the user may close one conversation and select another target. Explicitly toggling the help tool off closes the viewer and removes all target affordances.

The initial implementation supports one global help-mode controller in the MyContexts application shell. It exposes state and an `openHelp` callback to target components through a React context or equivalent typed provider. A document-wide click listener must not infer help targets from CSS selectors or displayed text.

---

## 4. Help Targets

A target activation supplies semantic data and a visual anchor:

```ts
type HelpTarget =
  | {
      kind: "context";
      contextInstance: ContextInstanceT;
      contextType: ContextType;
      userRoleType: RoleType;
      anchorRect: DOMRect;
    }
  | {
      kind: "role";
      contextInstance: ContextInstanceT;
      contextType: ContextType;
      userRoleType: RoleType;
      roleType: RoleType;
      anchorRect: DOMRect;
    };
```

The identifiers passed by the GUI are the identifiers already present in the screen or perspective data. Display labels and translated names must not be parsed to reconstruct types.

### 4.1 Context-window title

In the current MyContexts structure, `WWWComponent` already owns:

- `openContext`, from which the context instance is derived;
- `openContextType`;
- `openContextUserType`.

The `FlippingTitle` rendered in the top navigation bar is the context help target. Outside help mode it retains its existing title/user-role flipping behavior. In help mode, pointer click, Enter, or Space requests context help instead; the flip action is not run for that activation.

The clickable semantics and accessible label must reflect the current mode. In help mode the label describes opening help for the context, rather than showing the user's role.

### 4.2 TableForm header

`TableForms` passes each table definition to `buildTable`. Its `widgetCommonFields.perspective` already contains:

- `contextInstance`;
- `userRoleType`;
- `roleType`;
- `displayName`.

`PerspectiveTable` renders `displayName` in its accordion header. That header is the role help target. The builder and component should accept an optional typed callback, propagated from MyContexts:

```ts
onHelpTarget?: (target: HelpTarget, anchor: HTMLElement) => void;
```

When help mode is inactive, the accordion header continues to open or close the table. When help mode is active, activation prevents the accordion toggle and requests role help. Controls embedded in the header, such as an add button or context menu, remain controls and must not trigger help.

The callback belongs at the rendered header boundary. A wrapper around the complete table is too broad because rows, cells, and embedded controls have their own interactions.

### 4.3 Future targets

A future form-field target extends `HelpTarget` with a property type and any other stable discriminator needed by the PDR API. Adding it must not change the viewer's playback model. Target components only request a conversation; they do not interpret it.

---

## 5. Opening and Resolution

Selecting a target follows this flow:

```text
help target activation
  -> construct semantic HelpTarget
  -> record the triggering element and anchor rectangle
  -> show the viewer in a loading state near the target
  -> call the PDR help-conversation API
  -> receive Maybe ConversationBody
  -> initialize playback or show an unavailable state
```

The PDR request uses the active context instance and a context or role target as defined by the model conversation document specification. The GUI does not read `conversations.json`, determine the active audience, or translate readable identifiers.

Only the most recent request may update the viewer. If the user selects another target before a request completes, the earlier result is ignored or cancelled. Closing the viewer likewise invalidates an outstanding request.

A missing conversation is a normal result. The viewer displays a short, localized "No help is available for this item" message and remains closable. Retrieval or decoding failures display a localized error state without exposing repository details to the end user.

---

## 6. Window Structure

The viewer has three vertically arranged regions:

```text
+--------------------------------------+
| toolbar                     [close]  |
+--------------------------------------+
|                                      |
| scrollable conversation history      |
|                                      |
+--------------------------------------+
| available human answers              |
+--------------------------------------+
```

### 6.1 Toolbar

The toolbar is always visible at the top. Initially it contains:

- a short title identifying the help facility or selected object;
- a small close button at the end.

The empty part of the toolbar is the pointer drag handle. Interactive toolbar controls do not start dragging. Later tools may be added without changing the history or answer regions.

Use a familiar close icon with an accessible name. The toolbar must remain compact and must not be styled as a second content card inside the viewer.

### 6.2 History

History occupies the flexible middle region and scrolls independently. It contains only utterances that have occurred:

- bot statements and questions;
- human answers selected by the user.

Unselected answers never appear in history. Bot and human contributions have distinct alignment and visual treatment comparable to a chat interface, but remain readable model help rather than imitating a social messaging product.

The most recent bot contribution is the final history item above the answer region. When new contributions are appended, history scrolls to reveal that contribution. The user may scroll upward to inspect earlier turns. Automatic scrolling occurs on conversation progression, not continuously while the user is reading older history.

### 6.3 Answer region

The answer region is fixed below history and does not scroll away with old utterances. Each currently available answer is a real button with the complete answer text as its accessible name.

Buttons use a vertical layout so long model text can wrap. Selecting an answer:

1. disables the current answer set against duplicate activation;
2. appends the answer text to history as a human contribution;
3. removes the old answer buttons;
4. advances through the selected answer's continuation;
5. appends the next bot contributions;
6. presents the next answer set, if any.

If the selected answer has no continuation, the conversation is complete. The answer region then shows no choices; it may show a small localized completion status, but closing remains the primary action.

---

## 7. Playback Semantics

The viewer interprets the normalized conversation body returned by the PDR. Source references have already been resolved by the build pipeline.

Playback walks a sequence from its first element:

1. A `statement` is appended to history as a bot contribution, then playback continues.
2. A `question` is appended as a bot contribution.
3. The contiguous `answer` elements following that question become the available answer set.
4. Playback pauses until the user selects one answer.
5. The selected answer text is appended as a human contribution.
6. If that answer contains a nested `sequence`, playback continues at the start of that sequence.
7. If it contains no nested sequence, that branch is complete.

A sequence should therefore not expose an answer unless a question immediately precedes its answer group, as required by the conversation grammar's semantic constraints.

A run is represented independently from the immutable conversation definition:

```ts
type ConversationRun = {
  history: Utterance[];
  answers: RuntimeAnswer[];
  status: "playing" | "complete";
};

type Utterance = {
  speaker: "bot" | "human";
  text: string;
};
```

The runtime interpreter must not mutate the cached conversation body. Opening the same target starts a fresh run. Selecting a different target replaces the current run after the new conversation is resolved.

Localized message values are resolved before or while constructing each displayed utterance. The fallback text from the runtime artifact is used when a translation is unavailable.

---

## 8. Movement and Placement

The viewer uses fixed viewport positioning and is non-modal. Its initial position is derived from the selected target's `anchorRect`:

1. Prefer the side of the target with enough free space.
2. Add a small offset so the viewer does not cover the target.
3. Clamp the complete viewer within the viewport.
4. Keep the top and close button reachable after viewport resize.

Dragging starts with a pointer press on the non-interactive toolbar area. Pointer capture should be used so movement continues when the pointer leaves the toolbar. During drag, update position directly or through suitably local state; do not trigger unrelated screen rerenders.

The viewer cannot be dragged completely off-screen. At minimum, the toolbar and close button remain reachable. On resize or device rotation, its position and maximum dimensions are clamped again.

Each newly selected help target positions the viewer near that target. A user-dragged position is retained while playing the current conversation, including as history grows.

On narrow screens the viewer remains movable but uses responsive maximum width and height. It may initially occupy most of the available width; it must not overlap the fixed top and bottom navigation in a way that makes its toolbar or answer region unreachable.

Keyboard movement should be supported from the toolbar, for example with modified arrow keys, and announced in the toolbar's accessible description. Exact key bindings may follow the application's established accessibility conventions.

---

## 9. Closing and Focus

The viewer closes when:

- the close button is activated;
- Escape is pressed while the viewer is open;
- help mode is turned off;
- the application navigates to another context.

Escape closes only the topmost applicable transient surface. If a menu or other popup inside the viewer is open in a future version, that surface handles Escape first.

Opening records the element that activated the target. Closing returns focus to that element when it still exists and remains applicable. If it no longer exists, focus moves to the context title or another stable application landmark.

Opening the viewer moves focus to its heading or first meaningful content after loading. Answer buttons participate in normal Tab order and are activatable with Enter or Space. The viewer is non-modal: keyboard users may tab back into the underlying application without closing it.

The container should use non-modal dialog semantics, with an accessible name and description, while avoiding a focus trap:

```text
role="dialog"
aria-modal="false"
aria-labelledby=<viewer title id>
```

---

## 10. State Model

The application-level viewer state is conceptually:

```ts
type HelpViewerState =
  | { status: "hidden" }
  | { status: "loading"; target: HelpTarget; position: Point; requestId: number }
  | { status: "unavailable"; target: HelpTarget; position: Point }
  | { status: "error"; target: HelpTarget; position: Point }
  | {
      status: "open";
      target: HelpTarget;
      position: Point;
      run: ConversationRun;
    };
```

Help-mode state is maintained separately:

```ts
type HelpMode = "inactive" | "active";
```

Separating these states permits help mode to remain active after closing a viewer and makes loading, unavailable, and error behavior explicit.

The viewer position and run are transient UI state. They are not written to Perspectives and are reset when the page reloads.

---

## 11. Component Responsibilities

### 11.1 MyContexts application shell

`WWWComponent` or a dedicated provider mounted beside it owns:

- help-mode activation;
- current viewer state;
- calls to the proxy help API;
- request race handling;
- initial placement and viewport clamping;
- viewer rendering above both desktop and mobile screen layouts;
- closing on context navigation.

The viewer should be mounted once near the application root, not inside a table or navigation bar. This prevents clipping by scroll containers and allows fixed positioning above the three main columns.

### 11.2 Target components

`FlippingTitle`, `TableForms`, `buildTable`, and `PerspectiveTable` only:

- read whether help mode is active;
- expose target affordances;
- construct a typed target from identifiers they already own;
- pass the activation element or its rectangle to the controller;
- suppress their normal click behavior for a help activation.

They do not call repository APIs or interpret conversation bodies.

### 11.3 Conversation viewer

A dedicated `ConversationViewer` component owns:

- toolbar and close control;
- drag interaction;
- history rendering and scroll position;
- answer rendering;
- local playback transitions;
- focus behavior within the viewer.

The conversation interpreter should be a pure, separately testable module. React components render its state and dispatch answer selections.

### 11.4 PDR proxy

The proxy exposes the semantic help operation defined by the model conversation document specification. It serializes the target request and returns a typed, localized or localizable conversation body. Attachment names and storage details are not exposed to target components.

---

## 12. Visual and Interaction Constraints

- The viewer has a clear window boundary and restrained elevation over application content.
- Toolbar, history, and answers have stable dimensions so new text does not move the close button.
- Width and height use responsive minimum and maximum constraints.
- Long unbroken words and qualified identifiers wrap rather than overflow.
- History uses `overflow-y: auto`; the entire viewer must not grow beyond the viewport.
- Answer text may span several lines without changing button alignment unpredictably.
- Help-mode highlighting is visible without relying on color alone.
- The viewer's stacking level is above normal content and below application-critical modal dialogs.
- Dragging the viewer does not select toolbar text.
- Motion respects reduced-motion preferences; opening and new-message transitions do not require animation.

---

## 13. Error and Edge Cases

- **No help attachment:** treat every lookup as unavailable without repeatedly warning the user.
- **No binding for target:** show the unavailable state.
- **Invalid conversation body:** log a modeller-facing warning and show the error state.
- **Target changed during load:** only the newest request updates the viewer.
- **Context navigation during load:** close the viewer and discard the result.
- **Target removed during playback:** the conversation may finish, but focus restoration falls back to a stable landmark.
- **Empty answer set after a question:** treat the conversation as malformed, not complete.
- **Repeated answer activation:** accept the first activation only.
- **Viewport becomes smaller:** clamp position and dimensions immediately.

---

## 14. Testing and Acceptance Criteria

### 14.1 Playback tests

- Initial consecutive bot statements are appended in order.
- A question exposes exactly its contiguous answers.
- Selecting an answer appends it once as a human utterance.
- The selected nested sequence supplies the next bot contributions and answers.
- A terminal answer completes the run.
- Reopening a target creates a fresh run without mutating cached content.

### 14.2 Target integration tests

- Context-title activation sends a context target with the active context and user role.
- TableForm-header activation sends the perspective role type already present in the table definition.
- Normal title flipping and accordion toggling still work outside help mode.
- Those normal actions are suppressed for target activation inside help mode.
- Embedded header controls do not open help.

### 14.3 Window tests

- The viewer is hidden initially.
- It opens near the activating target and remains inside the viewport.
- Toolbar dragging changes position and preserves reachability of the close button.
- History scrolls independently while answers remain visible.
- Close button and Escape close the viewer.
- Focus returns to the activating target.
- A context change closes the viewer.

### 14.4 Accessibility tests

- Help targets are keyboard operable and announce their help action in help mode.
- The viewer has a non-modal dialog name.
- Every answer is a keyboard-operable button.
- Bot and human utterances are distinguishable without color alone.
- New conversation content is announced without repeatedly reading the entire history.
- Zoom, narrow viewports, and long translated strings do not hide the toolbar or answer region.

---

## 15. Summary

The help facility has three deliberately separated responsibilities:

```text
screen component identifies a semantic target
  -> PDR resolves a model conversation
  -> movable MyContexts viewer plays a transient conversation run
```

The viewer remains hidden until help mode and target selection require it. It is non-modal, movable, closable by button or Escape, and organized as a fixed toolbar, scrollable chat-like history, and persistent answer region. Current targets are the context title and TableForm header; future targets can reuse the same protocol without changing conversation playback.
