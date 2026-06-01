# Technical reference: GUI styling in MyContexts

This document describes the systematic approach to graphical styling in the
packages `mycontexts` and `perspectives-react`. Its purpose is to clarify the
layered structure, so that future style changes can be applied as much as
possible via Bootstrap variables and Bootstrap conventions rather than ad-hoc
CSS overrides.

---

## 1. Structure of the style layers

The styling is built up in four successive layers, each with a specific
responsibility:

```
Layer 1  Bootstrap base stylesheet (Bootswatch theme, CDN or npm)
Layer 2  Bootstrap CSS variables overridden in accessibility.css (:root)
Layer 3  Bootstrap React components via react-bootstrap (className props)
Layer 4  Package-specific custom stylesheets (www.css, components.css, …)
```

### Layer 1 – Bootstrap base stylesheet

`mycontexts` imports Bootstrap via the Bootswatch **Spacelab** theme:

```typescript
// packages/mycontexts/src/App.tsx
import 'bootswatch/dist/spacelab/bootstrap.min.css';
import 'bootstrap-icons/font/bootstrap-icons.css';
```

This provides Bootstrap 5 plus the Spacelab colour palette (cool grey-blue
tones). Bootstrap Icons are loaded as a separate font package.

A `ThemeManager` (`src/themeManagement.ts`) also exists that can dynamically
load alternative Bootswatch themes or external custom themes at runtime via a
`<link>` element in `document.head`. At present the `ThemeManager` is not
actively used in the production flow; the static import in `App.tsx` is
authoritative.

### Layer 2 – CSS variables in `accessibility.css`

In `packages/mycontexts/src/styles/accessibility.css`, Bootstrap 5 CSS
variables are overridden in the `:root` scope. This is the recommended approach
for changing Bootstrap colours, typography and interaction styles without
modifying the Bootstrap source.

Overridden variables (selection):

| Bootstrap variable | New value | Purpose |
|---|---|---|
| `--bs-body-color` | `#464e57` | Higher contrast ratio for default text |
| `--bs-secondary-color` | `#555555` | Better legibility for secondary text |
| `--bs-form-text-color` | `#6b6b6b` | Legibility of help text in forms |
| `--bs-primary-rgb` / `--bs-primary` | `165, 135, 97` / `#a58761` | Warm copper primary colour |
| `--bs-body-bg` / `--bs-body-bg-rgb` | `#f6f0e2` / `246, 240, 226` | Warm cream/parchment page background |
| `--bs-secondary-bg` / `--bs-secondary-rgb` | `#626b73` | Darker secondary background |
| `--bs-success` / `--bs-success-rgb` | `#007b00` | Darker green for better visibility |
| `--bs-link-color` / `--bs-link-hover-color` | `#7a5230` / `#3d2d20` | Dark warm-brown links with good contrast |
| `--bs-input-placeholder-color` | `#767676` | Placeholder colour meeting WCAG AA |
| `--bs-accordion-btn-bg` | `#b7a38b` | Tan/beige accordion header background |
| `--bs-accordion-btn-color` | `#1b2334` | Dark navy text on accordion headers |
| `--bs-accordion-active-bg` | `#b7a38b` | Same tan background when expanded |
| `--bs-accordion-active-color` | `#1b2334` | Dark navy text when expanded |
| `--bs-code-color` | `#bd2e76` | Darker pink for inline code |

By overriding only Bootstrap variables, all UI parts that share a variable
(buttons, links, tables, …) adapt automatically.

### Layer 3 – Bootstrap React components (`react-bootstrap`)

Both `mycontexts` and `perspectives-react` use the `react-bootstrap` library
(v2.x, Bootstrap 5). React-Bootstrap maps Bootstrap component classes to React
components. Styling is applied via the `className` prop on these components.

### Layer 4 – Custom stylesheets

Both packages contain their own CSS files for aspects that fall outside
Bootstrap defaults.

---

## 2. Using Bootstrap classes via `className`

### 2.1 React-Bootstrap components

The following Bootstrap components are imported and used:

**`packages/mycontexts`** (mainly `www.tsx`, `App.tsx`):

| Component | Use |
|---|---|
| `Container` / `Row` / `Col` | Grid layout of the three-column view (Who / What / Where) |
| `Navbar` | Top and bottom navigation bars |
| `NavDropdown` | Hamburger menu (main menu) in the top bar |
| `Tab` / `Tabs` | Mobile tabs for Who / What / Where sections |
| `Accordion` / `Accordion.Item` | Notifications, clipboard, chat channels |
| `Offcanvas` | Slide-in panels (left: Me, Apps, Settings; bottom: notifications) |
| `Modal` | Inspector dialog, transaction import dialog |
| `Button` | Various action buttons |
| `Form` / `Form.Group` / `Form.Control` | Confirmation code for transaction import |

**`packages/perspectives-react`** (components for PDR screens):

| Component | Use |
|---|---|
| `Table` | Table view of role instances in `perspectivetable.tsx` |
| `Accordion` / `AccordionContext` | Accordion rows for PerspectiveTable |
| `Form` | Form fields in multiple form components |
| `Card` | Error message in `screen.tsx` |
| `Navbar` | Button bar above tables (`tablecontrols.tsx`, `formcontrols.tsx`) |
| `Col` / `Row` / `Container` / `Button` | Layout and buttons throughout components |

### 2.2 Utility classes in `className` props

Bootstrap utility classes are passed directly as strings in the `className` prop.
The most commonly used classes:

**Column backgrounds:**

```tsx
// www.tsx – three columns on desktop, using body background
<Col className='bg-body full-height animated-column'>
<Col className='bg-body animated-column'>
<Col className='bg-body full-height animated-column'>
```

All three columns now use `bg-body` (which resolves to `--bs-body-bg`,
the warm cream colour `#f6f0e2`), giving a uniform background across all
columns.

**Text colours and typography:**

```tsx
<h2 className='text-center text-dark column-heading'>
<i className="bi bi-list fs-2" aria-hidden="true">
<h3 className="text-center pt-5">
```

**Spacing:**

```tsx
<Container fluid className='px-0'>
<Row className='mx-0 px-0'>
<Navbar className="py-0 ps-2">
<Row className='pb-3'>
<div className="text-center p-4 border border-secondary rounded">
```

**Borders and rounding:**

```tsx
<div className="text-center p-4 border border-secondary rounded">
```

**Visually hidden (screen-reader text):**

```tsx
<span className="visually-hidden">{i18next.t("mainMenu")}</span>
```

**Subtle background:**

```tsx
<p className='bg-light-subtle'>Go somewhere</p>
```

**Scroll behaviour (custom class, see §3):**

```tsx
<Tab className='bg-body full-mobile-height px-2 scrollable-content'>
```

---

## 3. Custom stylesheets in `mycontexts`

### 3.1 `src/styles/accessibility.css`

**Purpose:** Overrides Bootstrap CSS variables for better accessibility
(WCAG contrast) and applies the warm copper/parchment colour theme.

Contents:
- `:root` block with modified `--bs-*` variables (see §1, layer 2).
- `.btn-primary` block: dark text (`#1b2334`) on warm copper buttons for
  sufficient contrast.
- `h2.column-heading` / `h3.column-heading`: slight deviation from default
  Bootstrap h4/h5 styles for column headers.
- `.content-section-area:focus`: visible focus outline for keyboard navigation,
  using `var(--bs-link-color)`.
- `.keyboard-nav-instructions`: visually hidden instructions for screen readers.
- `.dropdown-toggle:focus`, `.navbar *:focus`: enlarged focus outline (3px
  white border + coloured box-shadow) on navbar elements.
- `*:focus-visible`: global focus style with `--bs-link-color` for consistency.

### 3.2 `src/styles/www.css`

**Purpose:** Layout and animations for the main screen (WWWComponent).

Contents:

| Class | Description |
|---|---|
| `.full-height` | Height = dvh minus top and bottom navbars (uses `--bottom-navbar-height`, `--top-navbar-height` CSS variables). |
| `.full-mobile-height` | Height for mobile via `--mobile-content-height`. |
| `.full-www-content-height` | Height minus both navbars and the column header (`--who-header-height`). |
| `.chat-height` | Half the available height for the chat window. |
| `.slide-in-from-right` / `.slide-out-to-right` | CSS animations for panel slide-in/out. |
| `.hide-caret` | Hides the Bootstrap dropdown arrow (used on `NavDropdown`). |
| `.scrollable-content` | `overflow-y: auto; overflow-x: hidden` for scrollable columns. |
| `.animated-column` | Smooth width transition when clicking a column (`flex-basis`, `width`, `padding`). |
| `.accordion` | Overrides `--bs-accordion-body-padding-x` to `0.5rem`. |
| `.container` | Overrides `--bs-gutter-x` to `0.5rem`. |
| `.markdown` | Legibility of Markdown blocks; uses accordion padding variables as fallback. |
| `.content-top-aligned` | Flex column with `justify-content: flex-start` for top-aligned content. |
| `.navbar-title` | Truncated title text with `text-overflow: ellipsis` in the top Navbar. |
| `.skip-link` | Accessibility skip link, visually hidden unless focused. |
| `.cursor-pointer` | Hand cursor on hover for clickable elements. |

Bootstrap default overrides via variables or more specific selectors:

```css
/* Dropdown link colour via Bootstrap variables */
html body .dropdown-menu .dropdown-item {
  --bs-dropdown-link-color: var(--bs-primary);
  --bs-dropdown-link-hover-color: var(--bs-info-text);
}

/* Focus ring colour via Bootstrap variable */
:focus {
  outline-color: var(--bs-warning);
}

/* Accordion caret hidden */
.accordion-button:after {
  display: none;
}
```

### 3.3 `src/styles/slidingPanels.css`

**Purpose:** Styles for the `SlidingPanels` pattern (a main panel with a cover
panel that slides in from the right).

| Class | Description |
|---|---|
| `.sliding-panels-container` | `overflow-x: hidden`; flex container at 100% width. |
| `.main-panel` | Fills available space; uses `var(--bs-body-bg)` for background. |
| `.cover-panel` | Absolutely positioned, starts at `right: -100%`, slides to `right: 0` via CSS transition. Z-index 1030 (above the Navbar). Also uses `var(--bs-body-bg)`. |
| `.cover-panel.open` | Activates the slide-in position. |
| `.has-open-panel .main-panel` | `pointer-events: none` so the underlying panel is not clickable. |
| `.sliding-panel-content` | Flex-grow 1 with bottom margin. |

### 3.4 `src/styles/splash.css`

**Purpose:** Basic style for the loading screen (`loadingScreen.tsx`).

Contains a single class `.introductionSplash` with `font-size: large`.

---

## 4. Custom stylesheets in `perspectives-react`

### 4.1 `src/styles/components.css`

**Purpose:** Styles for generic PDR screen components (tables, accordions,
drop areas).

Contents:

| Class / selector | Description |
|---|---|
| `::placeholder` | Higher contrast colour for placeholder text via `--bs-input-placeholder-color`. |
| `.failure:focus` | Red (`var(--bs-danger)`) focus ring for validation errors. |
| `.dropHere` | Cyan (`var(--bs-info)`) ring for active drop zones. |
| `.card:focus, .dropzone:focus` | Coloured outline for focusable cards, using `var(--bs-link-color)`. |
| `.navbarCard` | Reduced vertical padding for card elements in a Navbar. |
| `.widget` | `flex-grow: 1` for widgets that must fill available space. |
| `.disabledIcon` | Reduced opacity + `pointer-events: none` for disabled icons. |
| `.accordion-button::after` | Caret button to right edge (`margin-left: auto`). |
| `.accordion-button > div` | Space between content and caret. |
| `.accordion-button .btn-link:hover/focus` | Removes Bootstrap hover style inside accordion buttons. |
| `.accordion-button:not(.collapsed)` | Uses `var(--bs-accordion-btn-bg)` and `var(--bs-accordion-btn-color)` so the expanded header keeps the same tan/beige styling. |
| `.swiping` | Smooth transform + opacity during swipe gestures. |
| `.swipe-confirm-element` / `.swipe-confirm-buttons` | Flex layout for swipe confirmation options. |
| `.accordion-actions` | Inline flex row for accordion action buttons. |
| `.hide-caret.dropdown-toggle::after` | Hides Bootstrap dropdown arrow (with `!important`). |
| `.card-selected` | Visual highlight for selected cards via `var(--bs-primary)` box-shadow + background. |
| `.public-role` | Uses `var(--bs-info-bg-subtle)` for public role indicators. |
| `.calculated-role` | Uses `var(--bs-warning-bg-subtle)` for calculated role indicators. |
| `.cancelled-peer` | Uses `var(--bs-danger-bg-subtle)` for disconnected peer connections. |

All colour values in this file use Bootstrap CSS variables rather than hardcoded
hex values, so they adapt automatically when the host application overrides
`--bs-*` variables.

### 4.2 `src/styles/highlight.css`

**Purpose:** Colour theme for syntax highlighting of ARC code (via Highlight.js).

This file contains the **Solar Flare** theme (Base16 variant) from Highlight.js.
It contains no Bootstrap variables; it is fully self-contained and defines its
own background and text colours for code blocks. The theme can be replaced by
another Highlight.js theme via the selection script `choose_highlight-theme.sh`
in the `perspectives-react` root.

---

## 5. CSS variables as styling entry points

The current implementation follows a combined approach:

1. **Bootstrap CSS variables** (`--bs-*`) are overridden in `accessibility.css`
   (`:root`) and at component level via `style={}` props.
   This is the **recommended** way to make theme-wide changes to Bootstrap elements.

2. **Custom CSS variables** are defined for layout-specific dimensions that
   Bootstrap does not know about:
   - `--bottom-navbar-height`
   - `--top-navbar-height`
   - `--mobile-content-height`
   - `--who-header-height`
   - `--markdown-padding-y` / `--markdown-padding-x`

   These variables are declared as implicit use (the classes reference them, but
   initial values are set elsewhere via JavaScript or are CSS default values).

3. **No remaining hardcoded hex values** in `slidingPanels.css`,
   `components.css` or `accessibility.css`. All colour values reference
   Bootstrap CSS variables, making the entire UI responsive to theme changes.

---

## 6. Recommendations for style experiments

Based on the analysis above, these are the recommended entry points for style
changes:

### 6.1 Colour changes (theme-wide)

Adjust the `:root` variables in `accessibility.css`:

```css
:root {
  --bs-primary:     #new-colour;
  --bs-primary-rgb: r, g, b;
  --bs-body-bg:     #new-background;
  /* etc. */
}
```

All Bootstrap components that use `--bs-primary` (buttons, navbars,
`bg-primary`, links) adapt automatically.

### 6.2 Switching theme (Bootswatch)

Replace the import in `App.tsx`:

```typescript
// From:
import 'bootswatch/dist/spacelab/bootstrap.min.css';
// To:
import 'bootswatch/dist/flatly/bootstrap.min.css';
```

Or activate the `ThemeManager` to enable runtime theme switching.

### 6.3 Font

Bootstrap 5 uses the native system-font stack by default. To use a different
font, override:

```css
:root {
  --bs-body-font-family: 'Font Name', sans-serif;
  --bs-font-sans-serif: 'Font Name', sans-serif;
}
```

### 6.4 Borders and rounding

Bootstrap 5 uses `--bs-border-color` and `--bs-border-radius`:

```css
:root {
  --bs-border-color: #desired-colour;
  --bs-border-radius: 0; /* sharp corners */
}
```

### 6.5 Replacing hardcoded values with variables

The recommended pattern for any new colour values in custom CSS:

```css
/* Instead of: */
.my-class { box-shadow: 0 0 0 5px #dc3545; }

/* Use: */
.my-class { box-shadow: 0 0 0 5px var(--bs-danger); }
```

This ensures the colour adapts when the Bootstrap theme changes.

---

## 7. File overview

```
packages/mycontexts/
└── src/
    ├── App.tsx                          # Bootstrap theme import (Bootswatch Spacelab)
    ├── themeManagement.ts               # ThemeManager for runtime theme switching
    └── styles/
        ├── accessibility.css            # Bootstrap CSS variables overridden (:root) +
        │                                #   warm copper/parchment colour theme
        ├── www.css                      # Layout and animation styles for WWWComponent
        ├── slidingPanels.css            # Slide-in panels (uses var(--bs-body-bg))
        └── splash.css                   # Loading screen

packages/perspectives-react/
└── src/
    └── styles/
        ├── components.css               # Generic PDR UI styles (tables, drop zones)
        │                                #   All colours via Bootstrap variables
        └── highlight.css                # Highlight.js theme for ARC syntax highlighting
```
