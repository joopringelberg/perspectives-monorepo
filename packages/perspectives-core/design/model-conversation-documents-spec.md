# Model Conversation Documents - YAML Specification

## 1. Introduction

This document specifies how the conversations belonging to a Perspectives model are organised above the level of an individual conversation body. The grammar and YAML representation of one conversation body are specified in [Conversational Structure - YAML Specification](conversational-yaml-spec.md).

A model may provide conversations at two locations:

- **Context level** - a conversation about the context as a whole, presented to one or more user role types in that context.
- **Perspective level** - a conversation about a target role, presented to one or more user role types that have a perspective on that role.

Several user role types may share a perspective and the corresponding conversation. Conversation bodies are therefore defined independently from their bindings. A body is stored once and one or more bindings refer to it.

Large models should be split into a small model manifest and one document per context. This keeps authoring and validation manageable. A small model may embed context documents directly in the manifest without changing their internal representation. These source-document boundaries do not prescribe the runtime artifact layout.

---

## 2. Concepts

| Concept | Purpose |
|---------|---------|
| Model manifest | Identifies the model and locates or embeds its context conversation documents. |
| Context document | Contains the conversations and bindings for one context type. |
| Conversation body | A reusable conversation as defined by the conversational YAML specification. |
| Binding | Associates a conversation body with a location and one or more audience user role types. |
| Context binding | Associates a conversation with the context as a whole. |
| Perspective binding | Associates a conversation with a target role in the context. |
| Library document | Optionally contains conversations shared by multiple context documents. |

An **audience** is a user role type declared in the context, not an individual role instance. A **target role** is the role type the perspective is about.

---

## 3. Abstract Document Structure

```bnf
<model-manifest>       ::= <schema> <model-id> <context-entry>+ [<library-entry>*]

<context-entry>        ::= <context-id> (<document-path> | <context-document>)
<library-entry>        ::= <document-path>

<context-document>     ::= <schema> <model-id> <context-id>
                           <conversation-definition>+
                           [<context-binding>*]
                           [<perspective-binding>*]

<library-document>     ::= <schema> <model-id> <conversation-definition>+

<conversation-definition> ::= <conversation-id> <conversation-body>

<context-binding>      ::= <audience>+ <conversation-reference>
<perspective-binding>  ::= <audience>+ <target-role-id>
                           [<perspective-id>] <conversation-reference>

<conversation-reference> ::= <local-conversation-id>
                           | <external-conversation-reference>

<external-conversation-reference> ::= <document-path> <conversation-id>

<conversation-body>    ::= the conversation document structure defined in
                           conversational-yaml-spec.md
```

The BNF describes containment and required fields rather than YAML punctuation. The mapping in the next section is normative for the serialized form.

---

## 4. Mapping to YAML

### 4.1 Model manifest

The manifest is the entry point for all conversation material belonging to one model.

```yaml
schema: perspectives-conversations/v1
model: "model://example.org#Sales@1.0"

contexts:
  - context: "model://example.org#Sales$Marketplace"
    document: "conversations/marketplace.yaml"

  - context: "model://example.org#Sales$Order"
    document: "conversations/order.yaml"

libraries:
  - document: "conversations/shared.yaml"
```

The source `model` value is the readable, fully qualified, versioned model URI.
Compilation validates its readable root through `stableIdMapping.json` and
validates its version against the active stable versioned model URI. The two
URIs identify the same model but must not be compared as literal strings.

`libraries` is optional. Context entries normally contain a relative `document` path. Paths are resolved relative to the manifest.

### 4.2 Embedded context documents

For small models, a context entry may embed its document instead of referring to another file.

```yaml
schema: perspectives-conversations/v1
model: "model://example.org#Sales@1.0"

contexts:
  - context: "model://example.org#Sales$Marketplace"
    embedded:
      schema: perspectives-context-conversations/v1
      model: "model://example.org#Sales@1.0"
      context: "model://example.org#Sales$Marketplace"
      conversations:
        marketplace-help:
          conversation:
            - statement: "This is the marketplace."
      bindings:
        context:
          - audiences:
              - "model://example.org#Sales$Marketplace$Visitor"
            conversation: marketplace-help
```

An entry must contain exactly one of `document` and `embedded`. An embedded context document has the same structure and validation rules as a context document stored in a separate file.

### 4.3 Context document

A context document contains reusable conversation bodies and their bindings.

```yaml
schema: perspectives-context-conversations/v1
model: "model://example.org#Sales@1.0"
context: "model://example.org#Sales$Order"

conversations:
  order-overview:
    conversation:
      - statement: "This is an order."
      - question: "What would you like to know?"
      - answer: "Show its current status"

  customer-details:
    conversation:
      - statement: "The customer placed this order."
      - question: "What would you like to inspect?"
      - answer: "Contact details"
      - answer: "Delivery address"

  fulfilment:
    elements:
      shipping-details:
        statement: "Shipping starts after payment has been received."
    conversation:
      - statement: "This conversation concerns order fulfilment."
      - ref: shipping-details

bindings:
  context:
    - audiences:
        - "model://example.org#Sales$Order$Buyer"
        - "model://example.org#Sales$Order$Seller"
      conversation: order-overview

    - audiences:
        - "model://example.org#Sales$Order$Carrier"
      conversation: fulfilment

  perspectives:
    - audiences:
        - "model://example.org#Sales$Order$Buyer"
        - "model://example.org#Sales$Order$Seller"
      targetRole: "model://example.org#Sales$Order$Customer"
      conversation: customer-details

    - audiences:
        - "model://example.org#Sales$Order$Seller"
        - "model://example.org#Sales$Order$Carrier"
      targetRole: "model://example.org#Sales$Order$Shipment"
      conversation: fulfilment
```

The keys under `conversations` are document-local identifiers. They use the identifier syntax from the conversational YAML specification. They are not ARC names and need only be unique within the context document.

### 4.4 External conversation references

A context document may refer to a conversation in a library document. External references use `conversationRef` so they cannot be confused with local conversation identifiers or element-level `ref` values.

```yaml
bindings:
  context:
    - audiences:
        - "model://example.org#Sales$Order$Buyer"
      conversationRef:
        document: "conversations/shared.yaml"
        conversation: generic-help
```

A binding must contain exactly one of `conversation` and `conversationRef`.

### 4.5 Library document

A library document contains conversation bodies shared across context documents. It has no bindings because bindings are owned by the contexts in which conversations are offered.

```yaml
schema: perspectives-conversation-library/v1
model: "model://example.org#Sales@1.0"

conversations:
  generic-help:
    conversation:
      - statement: "I can explain the information and actions available here."
      - question: "What do you need help with?"
      - answer: "Understanding the information"
      - answer: "Choosing an action"
```

Conversation identifiers are local to the library document. An external reference therefore identifies both the library document and the conversation.

---

## 5. Identifiers and Namespaces

Perspectives context and role types have unique, structured names. Context documents store their fully qualified identifiers as YAML scalar values:

```yaml
context: "model://example.org#Sales$Order"
audiences:
  - "model://example.org#Sales$Order$Buyer"
targetRole: "model://example.org#Sales$Order$Customer"
```

They are values rather than mapping keys because qualified names may contain `:`, `/`, `#`, `$`, and model versions. Quoting these values is recommended.

A perspective is normally addressed by the tuple:

```text
(context, audience user role, target role)
```

ARC perspectives are not generally named independently. If a model can contain more than one distinct perspective from the same user role to the same target role, the binding includes the stable compiled `Perspective.id` as `perspectiveId`:

```yaml
- audiences:
    - "model://example.org#Sales$Order$Seller"
  targetRole: "model://example.org#Sales$Order$Customer"
  perspectiveId: customer-administration
  conversation: customer-details
```

A textual perspective query must not be used as an identifier: query formatting and implementation details may change without changing the semantic perspective.

---

## 6. Sharing and Scope

Conversation sharing is represented through references:

- Several audiences in one binding share the referenced conversation.
- Several bindings may refer to the same local conversation.
- Several context documents may refer to a conversation in a library document.

Conversation bodies are immutable definitions for the purpose of resolution; a binding does not override or merge part of a body.

The `elements` and `ref` mechanism inside a conversation body remains local to that body. Element labels do not cross conversation boundaries. Consequently, two conversations may both define an element named `introduction` without collision.

Context documents are the default unit of source authoring and local validation. Library documents should be introduced only for material genuinely shared across contexts; local conversations keep ownership and dependencies easier to understand. Runtime loading and caching use the compiled artifact described below rather than the individual YAML source documents.

---

## 7. Source Resolution

During compilation, the source documents are resolved as follows:

1. Load the model manifest and verify that its `model` matches the active model.
2. Select the context entry whose `context` matches the active context type.
3. Load its external document or use its embedded context document.
4. For a context-level request, select a context binding whose `audiences` contains the active user role type.
5. For a perspective-level request, select a perspective binding whose `audiences` contains the active user role type and whose `targetRole` matches the requested target role. Match `perspectiveId` as well when it is present.
6. Resolve `conversation` in the local `conversations` map, or resolve `conversationRef` in the indicated library document.
7. Validate the resulting body according to the conversational YAML specification.

No matching binding means that no conversation is available at that location. More than one matching binding is a validation error; ordering must not be used to choose between ambiguous bindings.

---

## 8. Semantic Constraints

The following constraints are not fully expressed by the YAML shape:

- Every context named by the manifest must belong to `model`.
- The `model` and `context` fields of an external or embedded context document must match its manifest entry.
- Every audience must be a user role type in the context document's context.
- Every perspective binding's target role must be addressable by a perspective of every listed audience.
- A local `conversation` reference must resolve in the same context document.
- An external `conversationRef` must name a library document listed by the manifest and an existing conversation in that document.
- A library document's `model` must match the manifest's `model`.
- A conversation identifier must be unique within its document.
- At most one binding may match a context-level or perspective-level location for an audience.
- The conversation body in each definition must satisfy the conversational YAML specification.

These constraints should be checked against the compiled Perspectives model when the documents are built or loaded.

---

## 9. Authoring in CouchdbManagement

The YAML documents are modeller-facing source files. They are stored in Perspectives File properties in the `VersionedModelManifest` context, following the existing translation workflow.

A separate property for every possible context cannot be modelled statically. Instead, the versioned model manifest should contain a relational collection whose instances all use the same File property. Conceptually:

```arc
thing ConversationSources (relational)
  property DocumentName (mandatory, String)
  property DocumentKind (mandatory, String)
    enumeration = ("Context", "Library")
  property ContextType (String)
  property ConversationYaml (File)
    pattern = "text/yaml" "Only YAML conversation files are allowed."
  property LastYamlChangeDT (DateTime)
```

Each role instance represents one context or library source document. `ContextType` is mandatory for a context document and absent for a library document. `DocumentName` identifies the document within the authoring collection and is used to resolve paths from the source manifest.

The source manifest may be represented in one of two ways:

- generated from the `ConversationSources` collection; or
- uploaded through one additional manifest File property.

Generating the manifest is preferred because it avoids maintaining both role-instance metadata and a separate file-to-context mapping. Embedded context documents remain useful for importing or exporting a complete small conversation set, but repository authoring should normalize them into source-role instances.

The exact ARC declarations, states, actions, and screens belong to the CouchdbManagement implementation. The invariant required by this specification is that a versioned model can expose a variable-sized collection of YAML source documents to one compilation action.

---

## 10. Build Pipeline

Conversation YAML is an authoring format. Clients do not consume it directly. The complete source collection is compiled into a JSON runtime artifact whenever the modeller requests generation or a source File changes.

The translation pipeline provides the precedent:

```text
editable YAML in a Perspectives File property
  -> parse and validate
  -> resolve readable identifiers
  -> compile to runtime JSON
  -> attach JSON to the versioned DomeinFile
```

Conversation compilation processes the complete collection because bindings and references may cross source-document boundaries:

1. Read every conversation source File belonging to the versioned model.
2. Parse the YAML into typed source representations.
3. Validate document schemas, conversation grammar, and local element references.
4. Resolve context, audience, and target-role names against the compiled DomeinFile.
5. Verify that every audience user role has the declared perspective on the target role.
6. Load `stableIdMapping.json` for the versioned model.
7. Convert readable context and role identifiers to stable identifiers.
8. Resolve local and library conversation references.
9. Reject duplicate or ambiguous bindings.
10. Normalize and serialize the complete result as JSON.
11. Store the JSON as `conversations.json` on the repository DomeinFile.

Compilation must be atomic from the publisher's perspective: a validation error leaves the previously published attachment intact. Successful compilation replaces the complete attachment so bindings and bodies cannot come from different source revisions.

As with `translationtable.json`, uploading or recompiling the DomeinFile must preserve an existing `conversations.json` attachment unless the conversation build explicitly replaces it.

---

## 11. Runtime Artifact

The runtime artifact is one consolidated attachment named `conversations.json` with media type `application/json`. Per-context YAML documents remain authoring units; they do not become separate runtime attachments.

```json
{
  "schema": "perspectives-help/v1",
  "model": "model://example.org#sales-cuid@1.0",
  "bindings": {
    "context-type-cuid": {
      "context": {
        "buyer-role-cuid": "order-overview"
      },
      "perspectives": {
        "buyer-role-cuid": {
          "customer-role-cuid": "customer-details"
        }
      }
    }
  },
  "conversations": {
    "order-overview": {
      "conversation": [
        {
          "statement": {
            "message": "help.order-overview.statement.introduction",
            "fallback": "This is an order."
          }
        }
      ]
    },
    "customer-details": {
      "conversation": [
        {
          "statement": {
            "message": "help.customer-details.statement.introduction",
            "fallback": "The customer placed this order."
          }
        },
        {
          "question": {
            "message": "help.customer-details.question.inspect",
            "fallback": "What would you like to inspect?"
          }
        },
        {
          "answer": {
            "message": "help.customer-details.answer.contact-details",
            "fallback": "Contact details"
          }
        },
        {
          "answer": {
            "message": "help.customer-details.answer.delivery-address",
            "fallback": "Delivery address"
          }
        }
      ]
    }
  }
}
```

The example uses descriptive placeholders for stable CUID-based identifiers. The generated artifact contains the actual stable identifiers from the model's sidecar mapping.

The runtime representation differs intentionally from the authoring structure:

- source document boundaries are removed;
- local and library references are resolved;
- readable ARC identifiers are replaced by stable identifiers;
- bindings are indexed for direct lookup;
- user-facing strings are represented by stable message identifiers plus fallbacks;
- conversation bodies remain reusable and are not duplicated for shared audiences.

One attachment is preferred initially because it provides atomic publication and simple caching. Splitting it into an index and per-context attachments should only be considered when measured model sizes require it. Model installation currently retrieves and preserves all DomeinFile attachments, so multiple attachments would not by itself provide remote lazy loading.

---

## 12. Identifier Boundary

Modellers author YAML with readable, fully qualified ARC names. Runtime JSON uses stable context and role identifiers. Conversion occurs during compilation while both the DomeinFile and `stableIdMapping.json` are available.

The runtime must never use translated or human-readable labels as lookup keys. `HumanReadableType` and `translateTypeString` produce display text and may vary with the current language. They are suitable for presenting a type name when help is unavailable, but not for identifying a help binding.

The stable lookup location is:

```text
context-level:     (stable context type, stable audience user role type)
perspective-level: (stable context type, stable audience user role type,
                    stable target role type [, stable perspective id])
```

If distinct perspectives can share the same audience and target role, compilation must include the stable compiled `Perspective.id` as `perspectiveId`. A textual query or generated display name is not a stable substitute. The serialized perspective exposed to clients carries the same identifier.

---

## 13. Runtime Retrieval and Cache

The PDR owns attachment retrieval, JSON decoding, audience resolution, and caching. MyContexts must not fetch repository documents or resolve model identifiers itself.

Conversation loading follows the same broad pattern as translation loading:

1. Check an in-memory conversation cache by stable model URI.
2. Locate the installed DomeinFile in the local models database.
3. Fetch its `conversations.json` attachment.
4. Convert the Blob to text and decode the JSON into the runtime representation.
5. Validate the runtime schema version.
6. Cache the decoded index for subsequent help requests.

A missing attachment means that the model provides no help conversations. It is not a model-loading error. An attachment that exists but cannot be decoded or has an unsupported schema is a modeller warning and yields no conversation.

The cache lifetime follows the installed model version. Replacing or removing a model must invalidate its cached conversation index, just as a newly installed attachment must not be hidden by data cached for the previous model version.

---

## 14. Runtime Resolution API

When help mode is active, MyContexts identifies the clicked object but delegates semantic resolution to the PDR. A request contains the active context instance and one of these targets:

```text
ContextTarget
RoleTarget stableRoleType stablePerspectiveId
```

The PDR derives the stable context type and the active user's role type from the context instance. It then performs an indexed lookup in `conversations.json`:

1. Select the stable context-type entry.
2. Select the active audience user-role entry.
3. For a role target, select the entry keyed by the stable target role and serialized `Perspective.id`.
4. Resolve the resulting conversation identifier in `conversations`.
5. Return the conversation body to MyContexts.

No match is a normal result and allows the GUI to report that no help is available. Multiple matches cannot occur in a valid compiled artifact.

The public API should expose the semantic operation rather than the attachment. Conceptually:

```text
getHelpConversation(contextInstance, target) -> Maybe ConversationBody
```

This boundary keeps model-location rules, stable identifiers, active-user resolution, attachment persistence, and schema evolution inside the PDR. The movable MyContexts viewer and its interaction state are specified in [Conversation Viewer - Interaction and Integration Specification](conversation-viewer-spec.md).

---

## 15. Localization

Conversation content is user-facing model text and should participate in model translation. The compiled artifact should assign a stable message identifier to every statement, question, and answer while retaining the source text as a fallback:

```json
{
  "statement": {
    "message": "help.order-overview.statement.1",
    "fallback": "This is an order."
  }
}
```

The translation-generation pipeline should include these message identifiers in the modeller's translation YAML. At runtime, conversation rendering resolves the message identifier through the model's current-language translation table and uses `fallback` when no translation exists.

Message identifiers must remain stable when unrelated text or source documents are reordered. Explicit conversation and element identifiers should form their basis; positional suffixes may be used only within a stable named parent. Changing source wording alone should not silently change an identifier and discard existing translations.

This reuses the existing translation infrastructure while keeping identity separate from localized display text.

---

## 16. Repository Artifacts

A published versioned DomeinFile may have these sidecar attachments:

```text
storedQueries.json
stableIdMapping.json
translationtable.json
conversations.json
```

The editable YAML conversation sources remain File property values in CouchdbManagement. They are publication inputs, not runtime attachments. The generated JSON attachment is the contract consumed by the PDR and, through the PDR API, by MyContexts.

---

## 17. Recommended Source File Layout

```text
conversations/
  manifest.yaml
  shared.yaml
  marketplace.yaml
  order.yaml
```

The manifest is the only fixed entry point. Context and library filenames are deployment details declared by the manifest; their names do not identify contexts or conversations semantically.

---

## 18. Summary

| Layer or artifact | Contains | Default scope |
|-------------------|----------|---------------|
| Model manifest YAML | Model identity and context source locations | Model authoring |
| Context document YAML | Conversation bodies and bindings | Context authoring |
| Library document YAML | Cross-context conversation bodies | Model authoring |
| Conversation source role | One editable YAML File and its metadata | Versioned model manifest |
| `conversations.json` | Resolved bodies and stable-ID binding index | Published model version |
| PDR conversation cache | Decoded runtime index | Installed model version |
| Conversation viewer | Help mode, target selection, and conversation playback | MyContexts session |
| Binding | Audience and context-level or perspective-level location | One or more user role types |

This structure separates conversational content from where it is offered and separates modeller-facing sources from runtime data. It supports shared perspectives without duplication, preserves readable names for authors, compiles them to stable runtime identifiers, and publishes one validated JSON attachment that the PDR can resolve efficiently for MyContexts.
