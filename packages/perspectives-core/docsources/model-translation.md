# Model Translation

This chapter describes how the PDR translates the names and texts of a model (type names, action names, notifications, screen titles and markdown) into the language of the end user. It covers the data structures, the YAML file that a modeller edits, how that file is turned into a lookup table, and how the table is used at runtime.

Source modules:

- [`translateModel.purs`](../src/core/modelTranslation/translateModel.purs) (`Perspectives.ModelTranslation`): generation, YAML writing and parsing, qualification, the translation table and the runtime lookup functions.
- [`representation.purs`](../src/core/modelTranslation/representation.purs) (`Perspectives.ModelTranslation.Representation`): the `ModelTranslation` data structure.
- [`Normalize.purs`](../src/core/modelTranslation/Normalize.purs) (`Perspectives.ModelTranslation.Normalize`): conversion of keys between Readable and Stable identifiers.
- [`parsing.purs`](../src/core/computedValues/parsing.purs) (`Perspectives.Extern.Parsing`): the external functions in the `model://perspectives.domains#Parsing` library that the modelling workflow calls.
- [`couchdbManagement@12.4.arc`](../src/model/couchdbManagement@12.4.arc): the `VersionedModelManifest$Translation` role that drives the workflow.

---

## 1. Overview

By convention a model is written in English. Before a string from the model is shown in a screen, the PDR looks it up in the model's **translation table** (`TranslationTable`). The table is a flat JSON object:

```json
{
  "model://perspectives.domains#xyfxpg3lzq$bhyejgccf1": { "en": "Couchdb Server", "nl": "Couchdb Server" },
  "model://perspectives.domains#xyfxpg3lzq$bhyejgccf1$b4nl73qx9q": { "en": "Accounts", "nl": "Accounts" },
  "You now have an account with CouchdbServer $1": { "orig": "You now have an account with CouchdbServer $1", "en": "…", "nl": "U heeft nu een account bij CouchdbServer $1" }
}
```

There are two kinds of keys:

| Key | Used for |
|---|---|
| Fully qualified (Stable) type identifier | contexts, roles (enumerated and calculated), properties (enumerated and calculated), actions, indexed context names. The external role of a context is translated as the context itself. |
| The original text | notifications, screen titles, and constant markdown texts. These entries also hold the original text under the key `orig`. |

The table is stored as the attachment `translationtable.json` of the DomeinFile document of each model version (next to `storedQueries.json` and `stableIdMapping.json`).

Modellers do not edit the table directly. Instead they edit a **YAML translation file**. In that file, names are *local* names, and the nesting of blocks qualifies them implicitly. It looks like this:

```yaml
model://perspectives.domains#xyfxpg3lzq:
  version: "12.4"
  contexts:
    CouchdbServer:
      translations:
        en: "Couchdb Server"
        nl: "Couchdb Server"
      users:
        Accounts:
          translations:
            en: "Accounts"
            nl: "Accounts"
          properties:
            UserName:
              translations:
                en: "User name"
                nl: "Gebruikersnaam"
```

---

## 2. Data structures

### 2.1 `ModelTranslation`

`ModelTranslation` (in `representation.purs`) mirrors the lexical structure of a model:

```
ModelTranslation   { namespace, version, contexts :: Maybe ContextsTranslation, roles :: Maybe RolesTranslation }
ContextsTranslation  Object ContextTranslation            -- key: context name
ContextTranslation { translations, external :: RoleTranslation, users, things, contextroles :: RolesTranslation,
                     contexts :: ContextsTranslation, notifications, indexedName :: Maybe IndexedContextNameTranslation }
RolesTranslation     Object RoleTranslation               -- key: role name
RoleTranslation    { translations, properties, actions :: ActionsPerStateTranslation, notifications, markdowns, titles }
PropertiesTranslation      Object Translations            -- key: property name
ActionsPerStateTranslation Object ActionsTranslation      -- key: state identifier
ActionsTranslation         Object Translations            -- key: action name
NotificationsTranslation / MarkdownsTranslation / TitlesTranslation
                           Object Translations            -- key: the original text
Translations               Object String                  -- key: language code ("en", "nl", or "orig")
```

The same structure is used in three states. Only the keys differ:

| State | Keys of contexts, roles, properties, actions | Produced by |
|---|---|---|
| Qualified, Stable | Stable FQNs (CUID segments) | `generateFirstTranslation`, `parseTranslation_pass2` |
| Qualified, Readable | Readable FQNs | `toReadableModelTranslation`, `qualifyModelTranslation` |
| Local | Local names (the last segment) | `parseTranslation_pass1` |

The keys of `ActionsPerStateTranslation` (state identifiers) are always fully qualified. They are written to the YAML file in full, because the local name of a state would not be enough to recover its identifier.

The **namespace** of a `ModelTranslation` is the Stable model URI (`model://perspectives.domains#xyfxpg3lzq`). It stays Stable in the YAML file, so the parser can find the model in the repository.

### 2.2 How the YAML blocks qualify names

`qualify` (class `Translation`) rebuilds qualified names from the local names in the YAML file:

| YAML location | Qualified key |
|---|---|
| `contexts: C` (top level) | `<namespace>$C` |
| `contexts: … C: contexts: D` (nested context) | `<C>$D` |
| `users` / `things` / `contextroles: R` in context `C` | `<C>$R` |
| `roles: R` (top level: roles of the domain context) | `<namespace>$R` |
| `properties: P` of role `R` | `<R>$P` |
| `external: properties: P` of context `C` | `<C>$External$P` |
| `actions: <state>: A` of role `R` | `<R>$A` (the state key is kept in full) |
| `indexedName: name: N` | `N` (already fully qualified) |
| `notifications` / `titles` / `markdowns`, entry `n` | the `orig` value of the entry |

The numeric keys (`0`, `1`, …) under `notifications`, `titles` and `markdowns` carry no meaning. Each entry is identified by its `orig` text.

`generateTranslationTable` then flattens a qualified `ModelTranslation` into the `TranslationTable` (method `writeKeys`). It also inserts an entry `<C>$External` for every context.

---

## 3. The workflow

The workflow is modelled in `couchdbManagement@12.4.arc`, on the thing role `Translation` of a `VersionedModelManifest`. All functions are in the `Parsing` library (`p:`).

```
                      ┌────────────── repository DomeinFile (Stable) ──────────────┐
                      │                                                            │
Translation$ModelTranslation = GenerateFirstTranslation(VersionedModelURI)          │
                      │  (JSON, empty translations, Stable keys)                   │
state GenerateYaml:   ▼                                                            │
   AugmentModelTranslation(ModelTranslation, VersionedModelURI) ◄── translationtable.json
                      │  (previous translations filled in)                         ▲
   GetTranslationYaml(…)  → Stable keys → Readable keys → YAML (local names)       │
                      │                                                            │
   create file …yaml in TranslationYaml   ──► modeller downloads, edits, uploads   │
                      │                                                            │
state GenerateTranslationTable (or the action of the same name):                   │
   ParseYamlTranslation(TranslationYaml)                                           │
                      │  pass 1: YAML → ModelTranslation (local names)             │
                      │  pass 2: qualify with the Readable namespace → Stable keys │
   GenerateTranslationTable(modeltranslation, VersionedModelURI) ──────────────────┘
```

1. **Upload of a model version.** `UploadToRepository` stores the compiled DomeinFile in the repository (see 3.1 for the attachments). Afterwards the manifest sets `GenerateYaml = true` on its `Translation` role.
2. **Generating the YAML** (`state GenerateYaml`). `GenerateFirstTranslation` builds a `ModelTranslation` from the compiled model. `AugmentModelTranslation` fills it with the existing translations of the table on that model version. `GetTranslationYaml` writes it as YAML with readable names. The result is stored as the file property `TranslationYaml`.
3. **Editing.** The modeller downloads the file, replaces the `MISSING <lang>` placeholders, and uploads it again.
4. **Building the table** (`state GenerateTranslationTable`, at most once per 10 seconds, or the `GenerateTranslationTable` action of the `Author`). `ParseYamlTranslation` parses the file. `GenerateTranslationTable` flattens it and uploads it as `translationtable.json` to the repository DomeinFile.

### 3.1 Translation table on upload (`uploadToRepository_`)

Writing the DomeinFile document to the repository removes all its attachments, so `uploadToRepository_` decides which attachments to write back *before* it writes the document (`combineUploadAttachments`):

| Attachment | Re-upload of an existing version | First upload of a version |
|---|---|---|
| `storedQueries.json`, `stableIdMapping.json`, `modelDependencies.json` | regenerated by compilation | regenerated by compilation |
| `translationtable.json` | preserved | copied from `BasedOnVersion`, if given and available |
| other attachments (e.g. `conversations.json`) | preserved | — |

If the version still has no translation table with entries (no `BasedOnVersion`, a failure to read it, or a table that an earlier upload emptied), it is seeded from `BasedOnVersion` where possible, and gets an empty table otherwise. Copying a table between versions works because both versions use the same Stable identifiers: a version based on another one keeps its CUIDs.

As a result, the YAML regenerated after an upload (state `GenerateYaml`) contains all types of the new model. Types that existed before keep their translations. New types have `MISSING` placeholders. Types that were removed from the model disappear from the YAML and, once the table has been rebuilt, from the table.

### 3.2 Feedback to the modeller

When the YAML file cannot be turned into a table, the Author receives a `TranslationTableNotUpdated` warning. It uses the warnings mechanism described in [PDR → Frontend Messaging](pdr-messaging.md): it is delivered with the next API response and appears in MyContexts' user messaging panel. The warning names the file or the versioned model; its error detail gives the cause:

| Raised by | Cause |
|---|---|
| `ParseYamlTranslation` | The file cannot be read as text (e.g. an unexpected MIME type). |
| `ParseYamlTranslation` | The file is not valid YAML. The detail is the parser message, which includes the line and column and a snippet of the file. A typical cause is a missing closing double quote in a hand-edited value. |
| `ParseYamlTranslation` | The model named in the file (its first line, plus `version`) cannot be retrieved from the repository or processed. |
| `GenerateTranslationTable` | The model translation cannot be decoded, or uploading the table to the repository fails. |

When `ParseYamlTranslation` fails it returns nothing, and `GenerateTranslationTable` then does nothing. That second case is only logged, so the problem is reported once. A successful upload produces no message. Remember that installations only see a new table after they have reinstalled the model (see section 4.1).

### 3.3 Generating from a DomeinFile (`generateFirstTranslation`)

The model is traversed as follows:

- **Top-level contexts** (contexts whose namespace is the model namespace, excluding the domain context itself) are listed under `contexts`. A context's nested contexts are listed inside it (`translateContext` follows `nestedContexts`, restricted to contexts that are lexically embedded).
- **Roles of the domain context** are listed under `roles`. The external role of the domain is excluded.
- The roles of a context are listed under `users`, `things` and `contextroles`. Roles that a context merely acquires from an aspect are not listed, because they are translated in their own model.
- For each role: its properties, its actions, the notifications of its states, and the markdown texts and titles of the screen it defines.
  - **Actions** come from the role itself (context actions) and from each of its perspectives. They are grouped by state. The actions of all sources in the same state are merged (see the `Semigroup ActionsPerStateTranslation` instance).
  - **Notifications** come from the states of the role (`Orole`/`Srole`), or of the context (`Cnt`), both `notifyOnEntry` and `notifyOnExit`. A sentence that occurs more than once yields a single entry.
  - **Markdown and titles** are collected from the screen whose `ScreenKey` is the lexical context and the role (`collectMarkdowns`, `collectTitles`). Only constant markdown (`MarkDownConstantDef`) is collected.

Views and states are not translated. The runtime uses their `readableName` instead (see `HumanReadableType` below).

### 3.4 Writing YAML (`writeTranslationYaml`, `writeReadableTranslationYaml`)

- Every value under `translations` is written as a **double-quoted YAML scalar**, using JSON string syntax (`quoteForYaml`). This preserves texts that YAML would otherwise misread, for example `Note: …`, `Leaving # now`, `{Name} joined`, `- item`, `true`, `123`, or backslashes.
- **Markdown texts** are written as multi-line double-quoted scalars, so that the modeller can read them (`escapeMarkdownForYaml`):
  - every newline is written as the escape `\n` followed by a real line break and indentation;
  - backslashes and double quotes are escaped;
  - a space or tab at the start of a line is escaped as `\ ` or `\t`, because YAML strips leading white space on continuation lines.

  When the file is read, YAML folds every real line break into a space. `normaliseFoldedMarkdown` removes that space again, for the original text and for the translations alike. When writing a multi-line translation by hand, follow the same pattern: end each line with `\n` and, if a line must start with white space (as with a nested list), escape the first space as `\ `.
- Each language in `addDefaultLanguages` (`en`, `nl`) that has no translation yet is written as the placeholder `"MISSING <lang>"`.
- `writeReadableTranslationYaml` first converts the Stable keys to Readable ones (`toReadableModelTranslation`), so that the modeller sees the names used in the ARC source. The namespace stays Stable.
- Blocks without content (no properties, no actions, an external role with nothing to translate, …) are omitted.

### 3.5 Parsing YAML (`parseTranslation_pass1`, `parseTranslation_pass2`)

Pass 1 uses `js-yaml` (via `Purescript.YAML.load`) and converts the result into the "underscore" types (`ModelTranslation_`, `ContextTranslation_`, …). In those types, absent blocks are `Nullable`. `hydrate` (class `Rehydrate`) then converts them into a `ModelTranslation`. Because the file is edited by hand, parsing is lenient:

- A file that is not a mapping, or whose single top-level key does not hold a mapping, is reported as an error (`Left`).
- Unquoted numbers and booleans are converted to strings (`scalarToString`). Empty values (`null`) are ignored.
- Entries that are not mappings (for example an empty `Admin:`) are ignored (`objectEntries`).
- Notification, title and markdown entries without an `orig` text are skipped, because they cannot be matched against the model.
- An `indexedName` block without a `name` is ignored.

Pass 2 (`parseTranslation_pass2`) qualifies all names with the *readable* namespace of the model (`qualifyModelTranslation`). It then maps the keys to Stable identifiers with `fromReadableModelTranslation`. That conversion uses the Stable/Readable mapping of the model, which `withRepositoryModel` (in `parsing.purs`) provides by temporarily caching the repository version of the DomeinFile. Action keys are mapped by searching the role (and its perspectives) for an `Action` whose `readable` name equals the qualified readable action name.

### 3.6 Augmenting (`augmentModelTranslation`)

`augmentModelTranslation table mt` copies existing translations into a freshly generated `ModelTranslation`. Type names, properties, actions, titles and indexed names are matched exactly by key. Notifications and markdown texts are matched with `fuzzyLookup` (`fuzzysort`, score ≥ 0.9), so that a translation survives small edits of the original text. The `orig` text written to the YAML file is always the current text.

---

## 4. Runtime

### 4.1 Loading

`fetchTranslations` (in `domeinCache.purs`) reads `translationtable.json` from the local models database when a model is retrieved into the cache. It stores the table in `PerspectivesState.translations`, keyed by the unversioned Stable model URI. If the table cannot be parsed, a warning is logged (not shown to the user) and the model is shown untranslated. When a model is updated or removed, its table is removed from the state (`removeTranslationTable`) and fetched again on the next use.

### 4.2 Lookup

- `translationOf domain text` looks up `text` in the table of `domain` for the current language (`PerspectivesState.currentLanguage`). It returns `text` unchanged if there is no table, no entry, no translation for that language, or if the translation is a `MISSING <lang>` placeholder.
- `translateTypeString typeName` derives the domain from the type name. For an external role it translates the context type.
- The class `HumanReadableType` (`humanReadableType.purs`) translates types for the client. If there is no translation, it falls back to the readable local name.

Call sites:

| What | Where |
|---|---|
| Context, role, property, indexed context and action names | `HumanReadableType` (perspective serialisation, `runAction`, instance getters) |
| Screen titles and constant markdown | `screenContextualisation.purs`, `contextSerialization.purs` |
| Notifications (on entry and on exit) | `sentenceCompiler.purs` (`compileSentence`) |

---

## 5. Known limitations

- The domain context itself, its external role, and notifications on the states of the domain context have no place in the YAML file and are therefore not translated.
- Views and states are not translated (their `readableName` is shown).
- The same original text (a notification, title or markdown) has a single translation per model, wherever it occurs.
- `GenerateTranslationTable` uploads the table to the repository only. A running PDR uses the new table after the model has been updated (or the session restarted).
- Tables produced before markdown normalisation was introduced may contain translations with a single leading space on every continuation line. They still work. When such a translation is written to YAML again, the space is shown as an escaped `\ `.

---

## 6. Tests

[`test/modelTranslation.purs`](../test/modelTranslation.purs) (`Test.ModelTranslation`, part of Layer 1, no database needed) covers:

- the qualification of every YAML block into table keys (a hand-written fixture);
- lossless round trips (ModelTranslation → YAML → table) for texts with YAML-significant characters, for multi-line markdown, and for `MISSING` placeholders;
- augmentation preserving earlier translations;
- lenient parsing of hand-edited files (numbers and booleans, missing `orig`, empty or non-mapping files);
- merging of actions per state;
- which attachments are uploaded with a compiled model (preserved, seeded, regenerated or default);
- generation from a model compiled in memory (nested contexts, roles whose name ends in `External`, actions from several perspectives, notifications on entry and on exit, screen titles and markdown), including the invariant that a YAML round trip reproduces the keys of the table;
- the runtime lookup functions `translationOf` and `translateTypeString`.

Run them with:

```bash
pnpm run test:modelTranslation   # this suite only
pnpm run test:layer1             # all Layer 1 tests
```

The conversion between Readable and Stable keys (pass 2) and the repository reads and writes of `uploadToRepository_` need the repository and are not covered by these tests.
