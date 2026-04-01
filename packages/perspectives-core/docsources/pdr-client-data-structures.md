# PDR Client Data Structures

This document describes the data structures that the Perspectives Distributed Runtime (PDR) sends to its clients (in particular `mycontexts` via `perspectives-react`). The authoritative TypeScript definitions live in `packages/perspectives-proxy/src/perspectivesshape.d.ts`; the corresponding PureScript source types live in `packages/perspectives-core`.

The three most important structures are **`ScreenDefinition`**, **`Perspective`**, and **`Roleinstancewithprops`**. This document takes `ScreenDefinition` as its starting point and descends from there into all referenced types. Types that are not reachable from `ScreenDefinition` are collected in a separate chapter. The developer-tool types `InspectableContext` and `InspectableRole` are described in their own section at the end.

---

## 1. ScreenDefinition

A `ScreenDefinition` is the top-level structure sent by the PDR to describe what a user should see for a given context instance. It is delivered by `PDRproxy.getScreen` (or equivalent), received by the `ScreenReceiver` callback type:

```typescript
export type ScreenReceiver = (screen: ScreenDefinition[]) => void;
```

### TypeScript

```typescript
export type ScreenDefinition = MainScreenElements & {
  title?: string;
  userRole: string; // The translated user role type, e.g. "My Role"
  whoWhatWhereScreen?: WhoWhatWhereScreenDef;
};

export type MainScreenElements = {
  tabs?: TabDef[];
  rows?: ScreenElementDefTagged[];
  columns?: ScreenElementDefTagged[];
};
```

### PureScript counterpart

```purescript
-- module Perspectives.Representation.ScreenDefinition

type MainScreenElements =
  ( tabs    :: Maybe (Array TabDef)
  , rows    :: Maybe (Array ScreenElementDef)
  , columns :: Maybe (Array ScreenElementDef)
  )

type ScreenDefinitionElements f =
  { title             :: Maybe String
  , userRole          :: String
  , whoWhatWhereScreen :: Maybe WhoWhatWhereScreenDef
  | f
  }

newtype ScreenDefinition = ScreenDefinition (ScreenDefinitionElements MainScreenElements)
```

**Source:** `packages/perspectives-core/src/core/typerepresentation/screenDefinition.purs`

### Semantics

A screen may have one of two layouts:

| Field | Purpose |
|---|---|
| `title` | Optional human-readable title shown at the top of the screen. |
| `userRole` | Translated name of the user role for whom the screen was computed. |
| `tabs` | List of tabs, each containing a set of screen elements. Mutually exclusive with `rows`/`columns`. |
| `rows` | Top-level elements laid out as rows. |
| `columns` | Top-level elements laid out as columns. |
| `whoWhatWhereScreen` | Present when the model uses the structured Who/What/Where layout. When present, `tabs`/`rows`/`columns` are normally absent. |

> **Note:** A screen is not itself state-dependent. State dependency is handled by the PDR at serialisation time: unavailable properties, verbs, and actions are omitted, and widgets whose underlying role type is unavailable in the current state are excluded entirely.

---

## 2. TabDef

A `TabDef` represents a single tab in a tabbed screen layout.

### TypeScript

```typescript
export type TabDef = {
  title: string;
  isDefault: boolean;
  elements: ScreenElementDefTagged[];
};
```

### PureScript counterpart

```purescript
newtype TabDef = TabDef
  { title     :: String
  , isDefault :: Boolean
  , elements  :: Array ScreenElementDef
  }
```

| Field | Purpose |
|---|---|
| `title` | Human-readable tab title. |
| `isDefault` | When `true` this tab is shown first. |
| `elements` | The screen elements inside this tab. |

---

## 3. ScreenElementDefTagged / ScreenElementDef

On the wire each screen element is wrapped in a discriminated-union envelope so the client can dispatch on `elementType`:

### TypeScript

```typescript
export type ScreenElementDefTagged = {
  elementType: "RowElementD" | "ColumnElementD" | "TableElementD"
             | "FormElementD" | "MarkDownElementD" | "ChatElementD" | "WhenElementD";
  element: ScreenElementDef;
}

export type ScreenElementDef =
  | RowElementDef
  | ColumnElementDef
  | TableElementDef
  | FormElementDef
  | MarkDownElementDef
  | ChatElementDef
  | WhenElementDef;
```

### PureScript counterpart

```purescript
data ScreenElementDef
  = RowElementD    RowDef
  | ColumnElementD ColumnDef
  | TableElementD  TableDef
  | FormElementD   FormDef
  | MarkDownElementD MarkDownDef
  | ChatElementD   ChatDef
  | WhenElementD   WhenDef
```

The `WriteForeign` instance serialises each constructor into `{ elementType: "<constructor name>", element: <inner value> }`, which the client reads via the `elementType` discriminant.

---

## 4. RowElementDef / ColumnElementDef

Both represent purely structural layout containers.

### TypeScript

```typescript
export type RowElementDef = {
  tag: "RowDef";
  elements: ScreenElementDefTagged[];
}

export type ColumnElementDef = {
  tag: "ColumnDef";
  elements: ScreenElementDefTagged[];
}
```

### PureScript counterpart

```purescript
newtype RowDef    = RowDef    (Array ScreenElementDef)
newtype ColumnDef = ColumnDef (Array ScreenElementDef)
```

Serialised as `{ tag: "RowDef", elements: [...] }` and `{ tag: "ColumnDef", elements: [...] }` respectively.

---

## 5. WhenElementDef

A `WhenElementDef` wraps a group of screen elements under a boolean condition. The condition is evaluated **server-side** by the PDR before the screen is sent; on the client the condition field contains a serialised `QueryFunctionDescription` (an opaque value that the client does not interpret).

### TypeScript

```typescript
export type WhenElementDef = {
  tag: "WhenDef";
  condition: unknown;
  elements: ScreenElementDefTagged[];
}
```

### PureScript counterpart

```purescript
newtype WhenDef = WhenDef
  { condition :: QueryFunctionDescription
  , elements  :: Array ScreenElementDef
  }
```

> **Important:** `WhenDef` elements reachable through `ScreenElementDef` are *structural* conditions on the free-form part of the screen. The analogous conditional wrapper for the Who/What/Where master-detail sections is `WhenTableFormDef` (see §10).

---

## 6. TableElementDef / FormElementDef

These are the core *widget* types. A **table** shows a list of role instances; a **form** shows the details of a single role instance. Both are backed by a `Perspective` (see §12).

### TypeScript

```typescript
export type TableElementDef = {
  tag: "TableDef";
  markdown: MarkDownElementDef[];
  widgetCommonFields: WidgetCommonFields;
}

export type FormElementDef = {
  tag: "FormDef";
  markdown: MarkDownElementDef[];
  widgetCommonFields: WidgetCommonFields;
}
```

### PureScript counterpart

```purescript
newtype TableDef = TableDef
  { markdown          :: Array MarkDownDef
  , widgetCommonFields :: WidgetCommonFieldsDef
  }

newtype FormDef = FormDef
  { markdown          :: Array MarkDownDef
  , widgetCommonFields :: WidgetCommonFieldsDef
  }
```

| Field | Purpose |
|---|---|
| `markdown` | Zero or more markdown elements shown above the widget. |
| `widgetCommonFields` | Shared widget configuration including the serialised `Perspective`. See §11. |

---

## 7. MarkDownElementDef

Markdown content can appear in three forms. All three share the same discriminated-union wrapper:

### TypeScript

```typescript
export type MarkDownElementDef =
  | MarkDownConstant
  | MarkDownPerspective
  | MarkDownExpression;

export type MarkDownConstant = {
  tag: "MarkDownConstantDef";
  element: { text: string; condition?: string; domain: string };
}

export type MarkDownPerspective = {
  tag: "MarkDownPerspectiveDef";
  element: { widgetFields: WidgetCommonFields; conditionProperty?: EnumeratedOrCalculatedProperty };
}

export type MarkDownExpression = {
  tag: "MarkDownExpressionDef";
  element: { textQuery: string; condition?: string; text?: string };
}
```

### PureScript counterpart

```purescript
data MarkDownDef
  = MarkDownConstantDef
      { text      :: String
      , condition :: Maybe QueryFunctionDescription
      , domain    :: String
      }
  | MarkDownPerspectiveDef
      { widgetFields       :: WidgetCommonFieldsDef
      , conditionProperty  :: Maybe PropertyType
      }
  | MarkDownExpressionDef
      { textQuery :: QueryFunctionDescription
      , condition :: Maybe QueryFunctionDescription
      , text      :: Maybe String
      }
```

| Variant | Purpose |
|---|---|
| `MarkDownConstantDef` | A static markdown string. `condition` (if present) is a pre-evaluated boolean string; `domain` is the model domain it belongs to. |
| `MarkDownPerspectiveDef` | Markdown whose text is drawn from a property of a perspective. The `conditionProperty` names the property that controls visibility. |
| `MarkDownExpressionDef` | Markdown whose text is computed by `textQuery` at runtime. `condition` gates visibility; `text` caches the last-computed value. |

---

## 8. ChatElementDef

A chat widget allows users to exchange messages through a designated role.

### TypeScript

```typescript
export type ChatElementDef = {
  tag: "ChatDef";
  fields: {
    chatRole: RoleType;
    title: string;
    chatInstance?: RoleInstanceT;
    messageProperty: PropertyType;
    mediaProperty: PropertyType;
  };
}
```

### PureScript counterpart

```purescript
newtype ChatDef = ChatDef (ChatDefFields (chatRole :: RoleType, title :: Maybe String))

type ChatDefFields f =
  { chatInstance    :: Maybe RoleInstance
  , messageProperty :: EnumeratedPropertyType
  , mediaProperty   :: EnumeratedPropertyType
  | f
  }
```

| Field | Purpose |
|---|---|
| `chatRole` | The role type that represents participants in the chat. |
| `title` | Human-readable label for the chat widget. |
| `chatInstance` | If a specific chat instance is already known, it is provided here. |
| `messageProperty` | The property on the chat role that holds text messages. |
| `mediaProperty` | The property on the chat role that holds media attachments. |

---

## 9. WhoWhatWhereScreenDef

The structured *Who / What / Where* layout is an alternative to the free-form tab/row/column layout. It divides the screen into three semantic regions:

### TypeScript

```typescript
export type WhoWhatWhereScreenDef = {
  who: Who;
  what: What;
  whereto: WhereTo;
};

export type Who = {
  markdown: MarkDownElementDef[];
  chats: ChatElementDef[];
  userRoles: TableFormDef[];
};

export type What =
    { tag: "TableForms"; elements: { markdown: MarkDownElementDef[]; tableForms: TableFormDef[] } }
  | { tag: "FreeFormScreen"; elements: MainScreenElements };

export type WhereTo = {
  markdown: MarkDownElementDef[];
  contextRoles: TableFormDef[];
};
```

### PureScript counterpart

```purescript
newtype WhoWhatWhereScreenDef = WhoWhatWhereScreenDef
  { who     :: Who
  , what    :: What
  , whereto :: WhereTo
  }

newtype Who = Who
  { markdown  :: Array MarkDownDef
  , chats     :: Array ChatDef
  , userRoles :: Array TableFormOrWhenDef
  }

data What
  = TableForms     { markdown :: Array MarkDownDef, tableForms :: Array TableFormOrWhenDef }
  | FreeFormScreen { | MainScreenElements }

newtype WhereTo = WhereTo
  { markdown    :: Array MarkDownDef
  , contextRoles :: Array TableFormOrWhenDef
  }
```

| Region | Typical content |
|---|---|
| `who` | User roles visible in the context, shown as master-detail table/form pairs, plus markdown headers and chat widgets. |
| `what` | The main content of the context. Either a set of master-detail pairs (TableForms) or a free-form sub-screen. |
| `whereto` | Context roles (links to related contexts), shown as master-detail pairs. |

---

## 10. TableFormDef and WhenTableFormDef

These types appear in the `userRoles` and `contextRoles` arrays of the Who/WhereTo sections and in the `tableForms` array of the What region.

### TypeScript

```typescript
export type TableFormDef = {
  markdown: MarkDownElementDef[];
  table: TableElementDef;
  form: FormElementDef;
};
```

(The conditional wrapper `WhenTableFormDef` is evaluated **server-side** by `contextualiseScreen`; the client always receives plain `TableFormDef` objects tagged `{ tag: "TableFormDef" }`.)

### PureScript counterpart

```purescript
newtype TableFormDef = TableFormDef
  { markdown :: Array MarkDownDef
  , table    :: TableDef
  , form     :: FormDef
  }

-- Conditional wrapper – evaluated before serialisation, never sent to client:
newtype WhenTableFormDef = WhenTableFormDef
  { condition  :: QueryFunctionDescription
  , tableForms :: Array TableFormOrWhenDef
  }

data TableFormOrWhenDef
  = PlainTableFormDef    TableFormDef
  | WhenTableFormItemDef WhenTableFormDef
```

> **Note:** `PlainTableFormDef` is serialised with tag `"TableFormDef"` (identical to a raw `TableFormDef`) so the client receives the same shape regardless of whether a `when` condition was involved in the model.

---

## 11. WidgetCommonFields

Every widget (table or form) carries a set of shared fields, the most important being the embedded `Perspective`.

### TypeScript

```typescript
export type WidgetCommonFields = {
  title: string;
  perspective: Perspective;
  fieldConstraints?: FieldDisplayConstraint[];
};

export type FieldDisplayConstraint = {
  propertyType: EnumeratedOrCalculatedProperty;
  minLines?: number;
  maxLines?: number;
};
```

### PureScript counterpart

```purescript
type WidgetCommonFieldsDef =
  WidgetCommonFieldsDefWithoutPerspective (perspective :: Maybe SerialisedPerspective')

type WidgetCommonFieldsDefWithoutPerspective f =
  { title               :: Maybe String
  , perspectiveId       :: PerspectiveId
  , fillFrom            :: Maybe QueryFunctionDescription
  , propertyRestrictions :: Maybe PropertyRestrictions
  , withoutProperties   :: Maybe (Array PropertyType)
  , roleVerbs           :: Maybe (Array RoleVerb)
  , userRole            :: RoleType
  , fieldConstraints    :: Maybe (Array FieldConstraintDef)
  | f
  }

-- | The keys are the string representations of PropertyTypes.
-- | The values are the PropertyVerbs that are EXCLUDED for that property.
type PropertyRestrictions = EncodableMap PropertyType (Array PropertyVerb)

type FieldConstraintDef =
  { propertyType :: PropertyType
  , minLines     :: Maybe Int
  , maxLines     :: Maybe Int
  }
```

| Field | Purpose |
|---|---|
| `title` | Widget heading shown above the table or form. |
| `perspective` | The full serialised perspective for this widget (see §12). Present at runtime after contextualisation. |
| `perspectiveId` | Internal identifier used to look up the perspective at runtime (not sent to client). |
| `fieldConstraints` | Per-property display hints, e.g. minimum/maximum lines for a textarea. |

> **Implementation note:** `propertyRestrictions`, `withoutProperties`, and `roleVerbs` are used server-side to derive the restricted serialised perspective. They are not forwarded to the client.

---

## 12. Perspective

A `Perspective` is the central data structure describing what a user role can **see** and **do** with a set of role instances inside a given context. It is embedded in every widget's `widgetCommonFields.perspective`.

### TypeScript

```typescript
export type Perspective = {
  id: string;
  displayName: string;
  isFunctional: boolean;
  isMandatory: boolean;
  isCalculated: boolean;
  userRoleType: RoleType;
  roleType: RoleType;
  roleKind: RoleKind;
  contextType: ContextType;
  contextIdToAddRoleInstanceTo: ContextInstanceT;
  contextTypesToCreate: Record<string, ContextType>;
  identifyingProperty: PropertyType;
  contextInstance: ContextInstanceT;
  roleInstances: Record<string, Roleinstancewithprops>;
  verbs: RoleVerb[];
  properties: Record<string, SerialisedProperty>;
  actions: Record<string, string>;
  possibleFillers: { readableName: string; instance: RoleInstanceT }[];
};
```

### PureScript counterpart

```purescript
-- Serialised form (module Perspectives.TypePersistence.PerspectiveSerialisation.Data)
type SerialisedPerspective' =
  { id                          :: String
  , displayName                 :: String
  , isFunctional                :: Boolean
  , isMandatory                 :: Boolean
  , isCalculated                :: Boolean
  , userRoleType                :: String
  , roleType                    :: Maybe String
  , roleKind                    :: Maybe RoleKind
  , contextType                 :: ContextType
  , contextTypesToCreate        :: Object String
  , identifyingProperty         :: String
  , contextInstance             :: ContextInstance
  , contextIdToAddRoleInstanceTo :: Maybe ContextInstance
  , roleInstances               :: Object RoleInstanceWithProperties
  , verbs                       :: Array String
  , properties                  :: Object SerialisedProperty
  , actions                     :: Object String
  , possibleFillers             :: Array { readableName :: String, instance :: RoleInstance }
  }
```

**Source:** `packages/perspectives-core/src/core/typePersistence/serialisedPerspective.purs`

### Field semantics

| Field | Type | Meaning |
|---|---|---|
| `id` | `string` | Opaque perspective identifier from the model. |
| `displayName` | `string` | Translated human-readable name of the perspective. |
| `isFunctional` | `boolean` | The role has at most one instance (functional). |
| `isMandatory` | `boolean` | The role must have at least one instance. |
| `isCalculated` | `boolean` | The role is computed, not stored directly. |
| `userRoleType` | `RoleType` | Qualified identifier of the user role that holds this perspective. |
| `roleType` | `RoleType` | Qualified identifier of the role type that is the *object* of this perspective. |
| `roleKind` | `RoleKind` | See §13. |
| `contextType` | `ContextType` | Qualified identifier of the context type. |
| `contextInstance` | `ContextInstanceT` | The context instance from which the object role instances are computed. |
| `contextIdToAddRoleInstanceTo` | `ContextInstanceT` | If the object is outside the user's context, the context where new instances should be added. |
| `contextTypesToCreate` | `Record<string, ContextType>` | Context types that can be created via this perspective (key = translated name, value = qualified type). |
| `identifyingProperty` | `PropertyType` | The property whose value is used as a human-readable label for role instances. |
| `roleInstances` | `Record<string, Roleinstancewithprops>` | The actual role instances, keyed by role instance identifier. |
| `verbs` | `RoleVerb[]` | The role operations the user may perform, given the current context/subject state. |
| `properties` | `Record<string, SerialisedProperty>` | All properties visible to this user, keyed by `PropertyType` string. |
| `actions` | `Record<string, string>` | Available actions (key = model action name, value = translated label). |
| `possibleFillers` | `{readableName, instance}[]` | Role instances that may be used to *fill* (bind to) this role. |

---

## 13. RoleKind

An enumeration describing the structural kind of a role type.

### TypeScript

```typescript
export type RoleKind =
  | "RoleInContext"
  | "ContextRole"
  | "ExternalRole"
  | "UserRole"
  | "Public"
  | "PublicProxy";
```

### PureScript counterpart

```purescript
-- module Perspectives.Representation.TypeIdentifiers
data RoleKind
  = RoleInContext  -- An ordinary role inside a context
  | ContextRole    -- A role that is the external role of a nested context
  | ExternalRole   -- The unique external role of a context
  | UserRole       -- A user role (may have perspectives)
  | Public         -- A publicly readable role
  | PublicProxy    -- A proxy for a public role
```

---

## 14. RoleVerb

The operations a user may perform on the role instances of a perspective.

### TypeScript

```typescript
export type RoleVerb =
  | "Remove"         // Remove a single instance
  | "RemoveContext"  // Remove a context-role instance together with its context
  | "Delete"         // Remove all instances
  | "DeleteContext"  // Delete all context-role instances together with their contexts
  | "Create"         // Create an instance
  | "CreateAndFill"  // Create and immediately fill with a role expression
  | "Fill"           // Fill a functional role with another role expression
  | "Unbind"         // Remove all binders of a given type
  | "RemoveFiller"   // Remove the filler of a functional role
  | "Move";          // Move an instance from one context to another
```

### PureScript counterpart

```purescript
-- module Perspectives.Representation.Verbs
data RoleVerb
  = Remove | RemoveContext | Delete | DeleteContext
  | Create | CreateAndFill | Fill | Unbind | RemoveFiller | Move
```

---

## 15. Roleinstancewithprops

Each entry in `Perspective.roleInstances` is a `Roleinstancewithprops`. It bundles the instance identifier with its current property values and the operations available on it given the *object's own state*.

### TypeScript

```typescript
export type Roleinstancewithprops = {
  roleId: RoleInstanceT;
  objectStateBasedRoleVerbs: RoleVerb[];
  propertyValues: Record<string, PropertyValues>;
  actions: Record<string, string>;
  objectStateBasedProperties: { type: string; value: PropertyType }[];
  publicUrl?: string;   // OBSOLETE
  filler?: RoleInstanceT;
  isMe: boolean;
  readableName: string;
  cancelled: boolean;
};
```

### PureScript counterpart

```purescript
type RoleInstanceWithProperties =
  { roleId                   :: String
  , isMe                     :: Boolean
  , publicUrl                :: Maybe String
  , objectStateBasedRoleVerbs :: Array String
  , propertyValues           :: Object ValuesWithVerbs
  , actions                  :: Object String
  , objectStateBasedProperties :: Array PropertyType
  , filler                   :: Maybe RoleInstance
  , readableName             :: String
  , cancelled                :: Boolean
  }
```

| Field | Meaning |
|---|---|
| `roleId` | The instance identifier of this role instance. |
| `isMe` | `true` when this instance represents the current user. |
| `readableName` | Value of the identifying property for display purposes. |
| `cancelled` | `true` when the peer this instance represents has disconnected. |
| `filler` | The role instance that fills (is bound to) this instance, if any. |
| `objectStateBasedRoleVerbs` | Additional role verbs that apply because of the *object's* current state (supplements the perspective-level `verbs`). |
| `objectStateBasedProperties` | Additional property types that become visible because of the object's state. |
| `propertyValues` | Current values of all properties, keyed by `PropertyType` string. Each entry is a `PropertyValues` record (see §16). |
| `actions` | Actions available specifically for this instance in its current state. |
| `publicUrl` | **Obsolete.** Was used for public role URLs; no longer populated. |

---

## 16. PropertyValues

The current values of a single property on a role instance, together with the verbs the user may apply to them.

### TypeScript

```typescript
export type PropertyValues = {
  values: ValueT[];
  propertyVerbs: string[];
};
```

### PureScript counterpart

```purescript
type ValuesWithVerbs =
  { values       :: Array String
  , propertyVerbs :: Array String
  }
```

| Field | Meaning |
|---|---|
| `values` | The actual property values as strings. The interpretation depends on the property's `range` (see `SerialisedProperty.range`). |
| `propertyVerbs` | Property operations available to the user (`"Consult"`, `"AddPropertyValue"`, `"SetPropertyValue"`, `"RemovePropertyValue"`, `"DeleteProperty"`). |

---

## 17. SerialisedProperty

Metadata about a property type, sent once per property in `Perspective.properties`.

### TypeScript

```typescript
export type SerialisedProperty = {
  id: PropertyType;
  displayName: string;
  isFunctional: boolean;
  isMandatory: boolean;
  isCalculated: boolean;
  range: PRange;
  constrainingFacets: {
    minLength?: number;
    maxLength?: number;
    pattern?: { regex: string; label: string };
    whiteSpace?: string;
    enumeration?: string[];
    maxInclusive?: string;
    maxExclusive?: string;
    minInclusive?: string;
    minExclusive?: string;
    totalDigits?: number;
    fractionDigits?: number;
  };
};
```

### PureScript counterpart

```purescript
type SerialisedProperty =
  { id                 :: String
  , displayName        :: String
  , isFunctional       :: Boolean
  , isMandatory        :: Boolean
  , isCalculated       :: Boolean
  , range              :: String
  , constrainingFacets :: PropertyFacets
  }

type PropertyFacets =
  { minLength           :: Maybe Int
  , maxLength           :: Maybe Int
  , pattern             :: Maybe { regex :: String, label :: String }
  , whiteSpace          :: Maybe String
  , enumeration         :: Maybe (Array String)
  , maxInclusive        :: Maybe String
  , maxExclusive        :: Maybe String
  , minInclusive        :: Maybe String
  , minExclusive        :: Maybe String
  , totalDigits         :: Maybe Int
  , fractionDigits      :: Maybe Int
  , isMessageProperty   :: Boolean
  , isMediaProperty     :: Boolean
  , isReadableNameProperty :: Boolean
  , isSettingProperty   :: Boolean
  }
```

`SerialisedProperty` is **state-independent**: it describes the type, not any particular instance value.

### PRange / range

```typescript
export type PRange =
  | "PString" | "PBool" | "PDateTime" | "PDate" | "PTime"
  | "PNumber" | "PEmail" | "PFile" | "PMarkDown";
```

The PureScript `Range` type also includes `PDuration Duration_` (year, month, week, day, hour, minute, second, millisecond), but this is serialised as its string representation and is not listed in the TypeScript union.

The `range` field governs how the client renders and validates the property value:

| Range | HTML input type / widget |
|---|---|
| `PString` | `text` |
| `PBool` | `checkbox` |
| `PDateTime` | `datetime-local` |
| `PDate` | `date` |
| `PTime` | `time` |
| `PNumber` | `number` |
| `PEmail` | `email` |
| `PFile` | `file` |
| `PMarkDown` | rich text (markdown editor) |

The extra flags in `PropertyFacets` (not exposed in the TypeScript `constrainingFacets`) have the following meanings:

| Flag | Meaning |
|---|---|
| `isMessageProperty` | This property holds a chat message text (used by `ChatDef`). |
| `isMediaProperty` | This property holds a chat media attachment (used by `ChatDef`). |
| `isReadableNameProperty` | The value of this property is used as the human-readable name (`readableName`) of the role instance. |
| `isSettingProperty` | This property is a user-configurable setting. |

---

## 18. Additional Types Reachable via WidgetCommonFields

### EnumeratedOrCalculatedProperty

```typescript
export type EnumeratedOrCalculatedProperty =
  { type: "ENP" | "CP"; value: PropertyType };
```

A tagged union that records whether a property type is Enumerated (`ENP`) or Calculated (`CP`). Used in `MarkDownPerspective.conditionProperty` and `FieldDisplayConstraint.propertyType`.

---

## 19. Types Not Reachable from ScreenDefinition

The following types appear in `perspectivesshape.d.ts` but are not part of the `ScreenDefinition` subtree. They are used for other PDR API calls or ancillary data.

### Primitive branded identity types

All identifiers are branded `string` types to prevent accidental confusion at compile time:

```typescript
export type RoleInstanceT   = string & { readonly brand: unique symbol };
export type ContextInstanceT = string & { readonly brand: unique symbol };
export type ValueT          = string & { readonly brand: unique symbol };
export type PropertyType    = string & { readonly brand: unique symbol };
export type ContextType     = string & { readonly brand: unique symbol };
export type RoleType        = string & { readonly brand: unique symbol };
export type UserRoleType    = RoleType;
```

These correspond to PureScript newtypes defined in `Perspectives.Representation.TypeIdentifiers` and `Perspectives.Representation.InstanceIdentifiers`:

```purescript
newtype RoleInstance    = RoleInstance    String
newtype ContextInstance = ContextInstance String
newtype PropertyType    = PropertyType    String   -- union of ENP / CP
newtype ContextType     = ContextType     String
newtype RoleType        = ENR EnumeratedRoleType | CR CalculatedRoleType | ...
```

### Callback receiver types

```typescript
export type RoleReceiver         = (roleInstance: RoleInstanceT[]) => void;
export type PropertyValueReceiver = (value: ValueT[]) => void;
export type RoleTypeReceiver     = (roleType: RoleType[]) => void;
export type PerspectivesReceiver = (perspectives: Perspective[]) => void;
export type ContextAndNameReceiver = (contextAndName: ContextAndName[]) => void;
export type ScreenReceiver       = (screen: ScreenDefinition[]) => void;
export type TableFormReceiver    = (tableForm: TableFormDef[]) => void;
```

These are the callback signatures used by `PerspectivesProxy` methods. The PDR pushes updates by calling these callbacks whenever the relevant data changes.

### ContextAndName

```typescript
export type ContextAndName =
  { externalRole: RoleInstanceT
  ; readableName: string
  };
```

Returned by queries that list context instances together with their human-readable names (e.g. for navigation menus).

### RuntimeOptions

```typescript
export type RuntimeOptions = {
  isFirstInstallation: boolean;
  useSystemVersion: string | null;
  privateKey?: CryptoKey;
  publicKey?: CryptoKey;
  myContextsVersion: string;
};
```

Passed to the PDR at startup to configure the runtime. `isFirstInstallation` controls whether a fresh system context is created; `useSystemVersion` allows an experimental system model to be used.

### PouchdbUser

```typescript
export type PouchdbUser = {
  systemIdentifier: string;
  perspectivesUser: string;
  userName: string;
  password?: string;
  couchdbUrl?: string;
};
```

Credentials used to authenticate with CouchDB (the underlying document store) and to identify the current user within the PDR.

### Unsubscriber

```typescript
export type Unsubscriber = { request: string; subject: string; corrId: number };
```

Returned by subscription calls. The client must send this back to the PDR to cancel the subscription.

### ContextActions

```typescript
export type ContextActions = Record<ModeledActionName, TranslatedActionName>;
```

A map from model action names to their translated labels, used for context-level actions (as opposed to perspective-level actions which appear in `Perspective.actions`).

### RoleOnClipboard

```typescript
export type RoleOnClipboard = {
  roleData: {
    rolinstance: RoleInstanceT;
    cardTitle: string;
    roleType: RoleType;
    contextType: ContextType;
  };
  addedBehaviour: string[];
  myroletype: RoleType;
};
```

The data structure held on the internal clipboard when a user copies a role instance for later binding.

### FillerType

```typescript
export type FillerType = { roleType: RoleType; readableName: string };
```

A pair of role type identifier and human-readable name, used when enumerating the types that may fill a given role.

### FileShareCredentials

```typescript
export type FileShareCredentials = {
  accountName: string;
  password: string;
  storageType: PStorageType;
  sharedStorageId: RoleInstanceT;
};

export type PStorageType = "mega" | "ppstorage";
```

Credentials for file-sharing storage backends (MEGA or a Perspectives-native storage).

### PerspectivesFile

```typescript
export type PerspectivesFile = {
  fileName: string;
  propertyType: PropertyType;
  mimeType: string;
  database?: string;
  roleFileName: string;
};
```

Metadata about a file attachment stored on a role instance. `database` is absent when the attachment lives in IndexedDB; otherwise it identifies the CouchDB database. `roleFileName` is the document name of the role instance that owns the attachment.

### PSharedFile

```typescript
export interface PSharedFile {
  name: string;
  size: number;
  type: string;
  sharedStorageId: string;
  storageType: string;
  url: string;
}
```

A file that has been shared via an external storage backend. `url` is the URL from which the file can be retrieved.

### RoleDataProper

```typescript
export type RoleDataProper = {
  rolinstance?: RoleInstanceT;
  cardTitle?: string;
  roleType?: string;
  contextType?: string;
};
```

A lightweight record used internally by React components to pass minimal role data around (e.g. for card display).

---

## 20. Developer-Tool Types: InspectableContext and InspectableRole

The `InspectableContext` and `InspectableRole` types are used by the **Perspectives Inspector** — a developer tool for examining the runtime state of context and role instances. They are not used in production screens generated from ARC models.

### InspectableContext

```typescript
export type InspectableContext = {
  id: ContextInstanceT;
  title: ReadableContextInstance;
  ctype: ReadableContextFQN;
  properties: Record<ReadablePropertyFQN, {
    translatedProperty: TranslatedPropertyTypeName;
    value: string;
  }>;
  roles: Record<ReadableRoleFQN, {
    translatedRole: TranslatedRoleTypeName;
    instances: Array<RoleInstance>;
  }>;
  unlinkedRoles: Record<ReadableRoleFQN, {
    translatedRole: TranslatedRoleTypeName;
    instances: Array<RoleInstance>;
  }>;
  me?: { _id: RoleInstanceT; title: ReadableRoleInstance; roleType: ReadableRoleFQN };
  types: Record<ReadableContextFQN, TranslatedContextTypeName>;
  externalRole: RoleInstanceT;
  states: Record<ReadableStateFQN, TranslatedStateTypeName>;
};
```

### PureScript counterpart

```purescript
-- module Perspectives.Inspector.InspectableResources
type InspectableContext =
  { id           :: ContextInstance
  , title        :: ReadableContextInstance
  , ctype        :: ReadableContextFQN
  , properties   :: SimpleMap ReadablePropertyFQN
                      { translatedProperty :: TranslatedPropertyTypeName, value :: String }
  , roles        :: SimpleMap ReadableRoleFQN
                      { translatedRole :: TranslatedRoleTypeName, instances :: Array RoleInstance }
  , unlinkedRoles :: SimpleMap ReadableRoleFQN
                       { translatedRole :: TranslatedRoleTypeName, instances :: Array RoleInstance }
  , me           :: Maybe { _id :: RoleInstance, title :: ReadableRoleInstance, roleType :: ReadableRoleFQN }
  , types        :: SimpleMap ReadableContextFQN TranslatedContextTypeName
  , externalRole :: RoleInstance
  , states       :: SimpleMap ReadableStateFQN TranslatedStateTypeName
  }
```

**Source:** `packages/perspectives-core/src/core/resourceinspector/inspectableResources.purs`

#### Field descriptions

| Field | Description |
|---|---|
| `id` | The unique identifier of the context instance. |
| `title` | Human-readable name derived from the readable property of the external role. |
| `ctype` | Fully qualified, human-readable context type identifier. Shown on hover in the inspector UI. |
| `properties` | Properties on the external role, keyed by readable FQN. Each entry provides the translated property name and current value. |
| `roles` | Roles in the context that are *linked* (i.e. their instances are connected via fillers), keyed by readable role FQN. |
| `unlinkedRoles` | Roles whose instances exist but are not connected to any filler. |
| `me` | The role instance in this context that represents the current user, if any. |
| `types` | The ARC type hierarchy ancestors of this context type (context type FQN → translated name). |
| `externalRole` | The identifier of the external role instance of this context. |
| `states` | The currently active states of this context (state FQN → translated name). |

---

### InspectableRole

```typescript
export type InspectableRole = {
  _id: RoleInstanceT;
  title: ReadableRoleInstance;
  rtype: ReadableRoleFQN;
  index: number;
  isMe: boolean;
  context: { _id: ContextInstance; title: ReadableContextInstance };
  properties: Record<ReadablePropertyFQN, {
    translatedProperty: TranslatedPropertyTypeName;
    value: string;
  }>;
  filler?: RoleInstance;
  filledRoles: Record<ReadableContextFQN, {
    contextTitle: TranslatedContextTypeName;
    roleInstances: Record<ReadableRoleFQN, {
      translatedRole: TranslatedRoleTypeName;
      instances: Array<RoleInstance>;
    }>;
  }>;
  types: Record<ReadableRoleFQN, TranslatedRoleTypeName>;
  states: Record<ReadableStateFQN, TranslatedStateTypeName>;
};
```

### PureScript counterpart

```purescript
type InspectableRole =
  { _id        :: RoleInstance
  , title      :: ReadableRoleInstance
  , rtype      :: ReadableRoleFQN
  , index      :: Int
  , isMe       :: Boolean
  , context    :: { _id :: ContextInstance, title :: ReadableContextInstance }
  , properties :: SimpleMap ReadablePropertyFQN
                    { translatedProperty :: TranslatedPropertyTypeName, value :: String }
  , filler     :: Maybe RoleInstance
  , filledRoles ::
      SimpleMap ReadableContextFQN
        { contextTitle :: TranslatedContextTypeName
        , roleInstances :: SimpleMap ReadableRoleFQN
            { translatedRole :: TranslatedRoleTypeName, instances :: Array RoleInstance }
        }
  , types      :: SimpleMap ReadableRoleFQN TranslatedRoleTypeName
  , states     :: SimpleMap ReadableStateFQN TranslatedStateTypeName
  }
```

#### Field descriptions

| Field | Description |
|---|---|
| `_id` | The unique identifier of this role instance. |
| `title` | Value of the readable property of this instance. |
| `rtype` | Fully qualified, human-readable role type identifier. |
| `index` | Position of this instance in the ordered list of instances of its type (0-based). |
| `isMe` | Whether this instance represents the current user. |
| `context` | The context this instance belongs to (identifier + readable name). |
| `properties` | Properties on this role instance, keyed by readable FQN. |
| `filler` | The role instance that fills this instance (if any). |
| `filledRoles` | The contexts and roles *filled by* this instance. Outer key = readable context FQN; inner key = readable role FQN. |
| `types` | The ARC type hierarchy ancestors of this role type (role type FQN → translated name). |
| `states` | The currently active states of this role instance (state FQN → translated name). |

---

### Readable-name helper types

Both `InspectableContext` and `InspectableRole` use a set of specialised `string` aliases for documentation clarity. In TypeScript these are branded types; in PureScript they are plain `String` type aliases:

| TypeScript type | PureScript alias | Meaning |
|---|---|---|
| `ReadableContextInstance` | `ReadableContextInstance = String` | Human-readable value of the external role's readable property. |
| `ReadableContextFQN` | `ReadableContextFQN = String` | Fully qualified context type identifier in readable form. |
| `ReadablePropertyFQN` | `ReadablePropertyFQN = String` | Fully qualified property type identifier in readable form. |
| `TranslatedPropertyTypeName` | `TranslatedPropertyTypeName = String` | Translated (localised) name of a property type. |
| `ReadableRoleFQN` | `ReadableRoleFQN = String` | Fully qualified role type identifier in readable form. |
| `TranslatedRoleTypeName` | `TranslatedRoleTypeName = String` | Translated (localised) name of a role type. |
| `ReadableRoleInstance` | `ReadableRoleInstance = String` | Human-readable label of a role instance (value of its identifying property). |
| `ReadableStateFQN` | `ReadableStateFQN = String` | Fully qualified state identifier in readable form. |
| `TranslatedContextTypeName` | `TranslatedContextTypeName = String` | Translated (localised) name of a context type. |
| `TranslatedStateTypeName` | `TranslatedStateTypeName = String` | Translated (localised) name of a state type. |

---

## Type Map Summary

The table below maps every TypeScript type in `perspectivesshape.d.ts` to its PureScript counterpart and source file.

| TypeScript type | PureScript type | PureScript source file |
|---|---|---|
| `ScreenDefinition` | `ScreenDefinition` | `src/core/typerepresentation/screenDefinition.purs` |
| `MainScreenElements` | `MainScreenElements` (row type) | `src/core/typerepresentation/screenDefinition.purs` |
| `TabDef` | `TabDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `ScreenElementDefTagged` | `ScreenElementDef` (with `WriteForeign` envelope) | `src/core/typerepresentation/screenDefinition.purs` |
| `ScreenElementDef` | `ScreenElementDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `RowElementDef` | `RowDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `ColumnElementDef` | `ColumnDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `WhenElementDef` | `WhenDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `TableElementDef` | `TableDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `FormElementDef` | `FormDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `MarkDownElementDef` | `MarkDownDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `MarkDownConstant` | `MarkDownConstantDef` (constructor) | `src/core/typerepresentation/screenDefinition.purs` |
| `MarkDownPerspective` | `MarkDownPerspectiveDef` (constructor) | `src/core/typerepresentation/screenDefinition.purs` |
| `MarkDownExpression` | `MarkDownExpressionDef` (constructor) | `src/core/typerepresentation/screenDefinition.purs` |
| `ChatElementDef` | `ChatDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `WidgetCommonFields` | `WidgetCommonFieldsDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `FieldDisplayConstraint` | `FieldConstraintDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `WhoWhatWhereScreenDef` | `WhoWhatWhereScreenDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `Who` | `Who` | `src/core/typerepresentation/screenDefinition.purs` |
| `What` | `What` | `src/core/typerepresentation/screenDefinition.purs` |
| `WhereTo` | `WhereTo` | `src/core/typerepresentation/screenDefinition.purs` |
| `TableFormDef` | `TableFormDef` | `src/core/typerepresentation/screenDefinition.purs` |
| `Perspective` | `SerialisedPerspective'` | `src/core/typePersistence/serialisedPerspective.purs` |
| `RoleVerb` | `RoleVerb` | `src/core/typerepresentation/verbs.purs` |
| `RoleKind` | `RoleKind` | `src/core/typerepresentation/typeIdentifiers.purs` |
| `Roleinstancewithprops` | `RoleInstanceWithProperties` | `src/core/typePersistence/serialisedPerspective.purs` |
| `PropertyValues` | `ValuesWithVerbs` | `src/core/typePersistence/serialisedPerspective.purs` |
| `SerialisedProperty` | `SerialisedProperty` | `src/core/typePersistence/serialisedPerspective.purs` |
| `PRange` | `Range` | `src/core/typerepresentation/range.purs` |
| `InspectableContext` | `InspectableContext` | `src/core/resourceinspector/inspectableResources.purs` |
| `InspectableRole` | `InspectableRole` | `src/core/resourceinspector/inspectableResources.purs` |
| `RoleInstanceT` | `RoleInstance` (newtype) | `src/core/typerepresentation/typeIdentifiers.purs` |
| `ContextInstanceT` | `ContextInstance` (newtype) | `src/core/typerepresentation/typeIdentifiers.purs` |
| `PropertyType` | `PropertyType` | `src/core/typerepresentation/typeIdentifiers.purs` |
| `ContextType` | `ContextType` | `src/core/typerepresentation/typeIdentifiers.purs` |
| `RoleType` | `RoleType` | `src/core/typerepresentation/typeIdentifiers.purs` |
