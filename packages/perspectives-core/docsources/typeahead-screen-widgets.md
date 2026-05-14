# Typeahead Screen Widgets

This document describes the three typeahead screen widgets introduced in the Perspectives Language
(ARC) for working with role candidate lists that may be too large for a standard dropdown.

---

## Background

When a user fills a role in MyContexts, the list of candidates is usually small enough to show as a
dropdown. But some applications work with thousands of candidates (e.g. contact lists, product
catalogues). Loading and serialising all those role instances into the browser's memory cache would
be impractical.

The typeahead widgets solve this problem by:

1. **Querying a dedicated PouchDB view** (`defaultViews/filterValueView`) that is keyed on
   `[roleType, contextGuid]` and stores only the light-weight `{filterValue, roleId}` pairs needed
   for the picker UI.
2. **Filtering client-side** as the user types, instead of making a new server round-trip per
   keystroke.

The view is populated for every role instance that carries the `filter:Filter` aspect (specifically
the property `filter:Filter$FilterValue`).

---

## The `filterValueView`

`filterValueView` is a CouchDB/PouchDB map function defined in `setupCouchdb.js` and registered
in `setupUser` / `reSetupUser`. It emits one entry per role document that has a `filterValue`
property:

```
key:   [roleType, contextGuid]
value: { filterValue: string, roleId: RoleInstanceT }
```

The corresponding PureScript constants are declared in `src/core/modelDependencies.purs`:

```purescript
filterAspect        :: String  -- "model://perspectives.domains#Filter$Filter"
filterValueProperty :: String  -- "model://perspectives.domains#Filter$Filter$FilterValue"
```

---

## Four Related Mechanisms

There are four ARC keywords for specifying how a cell or widget lets the user pick a filler role.

| Keyword | Syntactic context | Candidates specified by | Computed with |
|---|---|---|---|
| `fillfrom` | `master…detail` block | Query expression | Standard role-instance cache pipeline |
| `typeaheadfiller` | Stand-alone widget in `what` section | Query via `fillfrom` sub-expression | `fetchCandidatesFromQfd` |
| `typeaheadform` | Stand-alone widget at screen level | Role type (single name) | `fetchFilterValueCandidates` |
| `typeaheadfillfrom` | `master…detail` block | Role type (single name) | `fetchFilterValueCandidates` |

---

## `typeaheadfillfrom`

### Syntax

```arc
RoleName
  master
    typeaheadfillfrom CandidateRoleName
    with props (Prop1, Prop2)
  detail
```

`typeaheadfillfrom` is a direct peer of `fillfrom` inside the `master…detail` block. The role to
be *filled* is inferred from the syntactic context (the role type written immediately before
`master`). Only the *candidates* role type is named explicitly.

`CandidateRoleName` must be a role in the same context that the current user has a perspective on,
and whose instances carry the `filter:Filter` aspect.

### ARC example

```arc
user Manager = sys:Me
  perspective on Deelnemer
    only (Create, Fill, Remove)
    props (LastName) verbs (Consult)
  perspective on Candidates
    all props verbs (Consult)
  screen
    who
      Deelnemer
        master
          typeaheadfillfrom Candidates
          with props (LastName)
        detail

user Deelnemer (relational) filledBy sys:SocialEnvironment$Persons

user Candidates = sys:MySocialEnvironment >> Persons
```

### Compilation pipeline

| Phase | Module | What happens |
|---|---|---|
| Parser | `arcParser.purs` – `tableFormFields` | `typeaheadfillfrom <roleIdentifier>` parsed; stored as `typeAheadFillFromRole :: Maybe String` on `WidgetCommonFields` |
| Phase 3 | `phaseThreeScreens.purs` | `typeAheadFillFromRole` resolved to `typeAheadFillFrom :: Maybe RoleType` via `collectRoles` |
| Screen build | `screenContextualisation.purs` – `contextualiseWidgetCommonFields` | When `typeAheadFillFrom` is set, `fetchFilterValueCandidates` is called to pre-populate `typeAheadFillFromCandidates :: Maybe (Array FilterValueEntry)` |

`typeAheadFillFrom` is also normalised in `normalizeTypeNames.purs`.

### Client data

`WidgetCommonFieldsDefWithoutPerspective` gains two extra fields (both `Maybe` for
backwards-compatibility with older DomeinFiles):

```purescript
typeAheadFillFrom       :: Maybe RoleType
typeAheadFillFromCandidates :: Maybe (Array FilterValueEntry)
```

The corresponding TypeScript additions in `perspectivesshape.d.ts`:

```typescript
export type FilterValueEntry = {
  filterValue: string;
  roleId: RoleInstanceT;
};

// on WidgetCommonFields:
typeAheadFillFromCandidates?: FilterValueEntry[];
```

### React rendering

When `typeAheadFillFromCandidates` is present on a card column's `WidgetCommonFields`, `TableCell`
renders a typeahead text-input with a Bootstrap `Dropdown` instead of the normal static dropdown.
The candidate list is filtered client-side as the user types. Full keyboard navigation
(ArrowDown / ArrowUp / Enter / Escape) is supported. The Bootstrap `Dropdown.Menu` uses
`popperConfig={{ strategy: 'fixed' }}` plus a custom `sameWidth` Popper modifier so the popup
overlays all accordion items and is constrained to the column width.

---

## `typeaheadfiller`

### Syntax

```arc
row
  typeaheadfiller FilledRoleName
    fillfrom CandidatesExpression
```

`typeaheadfiller` is a stand-alone widget for use in `row`, `column`, or `what` sections. It takes
the **role to fill** as its first argument, then a `fillfrom` sub-expression that is a query
returning the candidate role instances.

### Compilation pipeline

| Phase | Module | What happens |
|---|---|---|
| Parser | `arcParser.purs` | `TypeAheadFillerElement` parsed; `fillfrom` sub-expression compiled to a `QueryFunctionDescription` |
| Phase 3 | `phaseThreeScreens.purs` | `TypeAheadFillerDef` constructed |
| Screen build | `screenContextualisation.purs` – `contextualiseTypeAheadFillerDef` | `fetchCandidatesFromQfd` called: the `fillfrom` QFD is decomposed with `unsnocQfd`, the context is navigated, and `defaultViews/filterValueView` is queried |

`fetchCandidatesFromQfd` (defined in `screenContextualisation.purs`) is also re-used by
`addPerspectives` for `TypeAheadFillerDef` elements.

### Client data

```purescript
newtype TypeAheadFillerDef = TypeAheadFillerDef
  { widgetCommonFields :: WidgetCommonFieldsDef
  , candidates         :: Array FilterValueEntry
  }
```

```typescript
export type TypeAheadFillerElementDef = {
  tag: "TypeAheadFillerElementD";
  typeAheadFillerDef: {
    widgetCommonFields: WidgetCommonFields;
    candidates: FilterValueEntry[];
  };
};
```

### React rendering

`roletypeaheadfiller.tsx` — class component with a controlled text-input and Bootstrap
`ListGroup` dropdown. Arrow-key navigation (ArrowDown / ArrowUp / Enter / Escape) is supported.
On selection, `pproxy.bind_()` is called. The existing filler label is displayed on mount.

---

## `typeaheadform`

### Syntax

```arc
what
  row
    typeaheadform "Display Label" CandidateRoleName
```

`typeaheadform` is a stand-alone screen widget that replaces a `form` element when the list of
candidates is large. It takes an optional display label (string literal) and the name of a role
type that the current user has a perspective on. No `fillfrom` expression is needed; candidates
are derived from `objectRoleType` stored in `WidgetCommonFieldsDef`.

### Compilation pipeline

| Phase | Module | What happens |
|---|---|---|
| Parser | `arcParser.purs` | `TypeAheadFormElement` parsed; `objectRoleType` derived from `ExplicitRole` |
| Phase 3 | `phaseThreeScreens.purs` | `TypeAheadFormDef` constructed; `perspective` is always `Nothing` |
| Screen build | `screenContextualisation.purs` – `contextualiseTypeAheadFormDef` | `fetchFilterValueCandidates objectRoleType` called to pre-populate candidates |

`fetchFilterValueCandidates` dispatches on the two possible cases:

- **`ENR rt` (EnumeratedRoleType):** queries `defaultViews/filterValueView` directly with
  `[unwrap rt, contextGuid]`.
- **`CR crType` (CalculatedRoleType):** calls `calculationOfRoleType crType` to obtain the
  `QueryFunctionDescription`, then delegates to `fetchCandidatesFromQfd`.

The function is also used by `addPerspectives TypeAheadFormDef` in `contextSerialization.purs`.

### Client data

```purescript
newtype TypeAheadFormDef = TypeAheadFormDef
  { widgetCommonFields :: WidgetCommonFieldsDef   -- perspective is always Nothing
  , candidates         :: Array FilterValueEntry
  , displayName        :: Maybe String
  }
```

```typescript
export type TypeAheadFormElementDef = {
  tag: "TypeAheadFormElementD";
  typeAheadFormDef: {
    widgetCommonFields: WidgetCommonFields; // perspective always absent
    candidates: FilterValueEntry[];
    displayName?: string;
  };
};
```

`widgetCommonFields.objectRoleType` carries the concrete role type (added in the same release;
`Maybe RoleType` for backwards-compatibility with older DomeinFiles).

### React rendering

`roletypeaheadform.tsx` — class component. After the user selects a candidate, the component
calls `pproxy.getPerspective()` for that single instance and renders the result in a
`PerspectiveBasedForm`. Arrow-key navigation (ArrowDown / ArrowUp / Enter / Escape) is
supported. The `displayName` prop is shown as a label.

---

## Shared helpers — `screenContextualisation.purs`

| Helper | Used by |
|---|---|
| `fetchCandidatesFromQfd` | `contextualiseTypeAheadFillerDef`, `addPerspectives TypeAheadFillerDef` |
| `fetchFilterValueCandidates` | `contextualiseTypeAheadFormDef`, `addPerspectives TypeAheadFormDef`, `contextualiseWidgetCommonFields` (for `typeaheadfillfrom`) |
| `unsnocQfd` | `fetchCandidatesFromQfd` (decomposes a QFD into all-but-last + last step) |

---

## `fillfrom` (for contrast)

`fillfrom` is the *non*-typeahead sibling of `typeaheadfillfrom`. **However**, when more than a fixed number (hard-coded as 20) of candidates are retrieved, it switches to type-ahead behaviour.  It also lives in the
`master…detail` block but takes a full **query expression** rather than a role name:

```arc
master
  fillfrom sys:MySocialEnvironment >> Persons
  with props (LastName)
detail
```

Candidates are fetched through the standard role-instance caching pipeline and delivered to the
client as `perspective.possibleFillers`. There is no `filterValueView` involvement. `fillfrom`
and `typeaheadfillfrom` coexist and serve different use-cases:

| | `fillfrom` | `typeaheadfillfrom` |
|---|---|---|
| Candidate specification | Arbitrary query | Role type name |
| Candidate size | Small–medium (all loaded) | Potentially very large |
| Filtering | Not applicable (full list shown) while less than 20 | Client-side text filter |
| Requires Filter aspect | No | Yes |

---

## Complete ARC Example

```arc
case TestTypeAheadApp
  indexed mm:MyTestTypeAheadApp
  aspect sys:RootContext
  external

  user Manager = sys:Me
    perspective on Friend
      defaults
    action CreateFriend
      create role Friend
    perspective on Candidates
      all props verbs (Consult)
    perspective on Deelnemer
      only (Create, Fill, Remove)
      props (LastName) verbs (Consult)
    perspective on Gast
      only (Create, Fill, Remove)
      props (LastName) verbs (Consult)
    screen
      who
        Deelnemer
          master
            typeaheadfillfrom Candidates   -- typeahead: candidates from FilterValue view
            with props (LastName)
          detail
        Gast
          master
            fillfrom Candidates            -- standard dropdown (all candidates loaded)
            with props (LastName)
          detail
      what
        row
          typeaheadform "Persons" Candidates  -- typeahead form widget
      where

  user Friend filledBy sys:SocialEnvironment$Persons

  user Candidates = sys:MySocialEnvironment >> Persons

  user Deelnemer (relational) filledBy sys:SocialEnvironment$Persons

  user Gast (relational) filledBy sys:SocialEnvironment$Persons
```

---

## Module reference

| PureScript module | Role |
|---|---|
| `Perspectives.Parsing.Arc.AST` | `TypeAheadFillerElement`, `TypeAheadFormElement`; `typeAheadFillFromRole` on `WidgetCommonFields` |
| `Perspectives.Parsing.Arc.Expression.AST` | n/a (expression AST reused) |
| `Perspectives.Parsing.Arc` | Parser rules for `typeaheadfiller`, `typeaheadform`, `typeaheadfillfrom` |
| `Perspectives.Parsing.Arc.PhaseThree.Screens` | `TypeAheadFillerDef`, `TypeAheadFormDef` construction; `collectRoles` for `typeAheadFillFromRole` |
| `Perspectives.Representation.ScreenDefinition` | `TypeAheadFillerDef`, `TypeAheadFormDef`, `FilterValueEntry`; `typeAheadFillFrom` + `typeAheadFillFromCandidates` on `WidgetCommonFieldsDefWithoutPerspective` |
| `Perspectives.TypePersistence.ScreenContextualisation` | `fetchCandidatesFromQfd`, `fetchFilterValueCandidates`, `unsnocQfd`; `contextualiseTypeAheadFillerDef`, `contextualiseTypeAheadFormDef`, `contextualiseWidgetCommonFields` |
| `Perspectives.TypePersistence.ContextSerialization` | `addPerspectives` cases for `TypeAheadFillerDef` and `TypeAheadFormDef` |
| `Perspectives.Parsing.Arc.NormalizeTypeNames` | Normalisation of `typeAheadFillFrom` |
