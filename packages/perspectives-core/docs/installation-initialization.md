# Installation Initialization Process

This document describes what happens when a new Perspectives installation is created, covering both the Purescript (PDR) side and the Perspectives Language (PL) side.

> **Model version:** This document reflects `model://perspectives.domains#System@6.3` (source: `perspectivesSysteem@6.3.arc`) and the corresponding PDR code in `couchdbExternalFunctions.purs`.

## Background

Perspectives is a model-driven system. When a new installation is created, a set of shared contexts are created from scratch and stored locally:

- **TheWorld** – the same instance for every peer (identifier: `"TheWorld"`).
- **SocialEnvironment** – the same identifier for every peer (`"TheSocialEnvironment"`).
- **PerspectivesSystem** – a unique instance per installation, with an identifier derived from the user's CUID.

The base model is `model://perspectives.domains#System` (source: `perspectivesSysteem@6.3.arc`).

There are two scenarios:
1. **First installation** – the user sets up Perspectives for the first time.
2. **Subsequent installation** – the user adds Perspectives on a second device, using an identity document exported from the first installation.

---

## Division of Labour

### Purescript (PDR) is responsible for:
- Setting up the CouchDB databases and their views.
- Loading the System model (and other required models) into the local store.
- Creating the `PerspectivesSystem` context instance (authored by `PerspectivesSystem$Installer`).
- On a first installation: creating `TheWorld`, the user's `PerspectivesUsers` role (including the public key), `SocialEnvironment`, and `SocialEnvironment$Persons` — all before any PL state reactions fire.
- On a subsequent installation: executing the identity document (a serialised transaction from the first installation) to reconstruct `TheWorld`, `PerspectivesUsers`, `SocialEnvironment`, and `SocialEnvironment$Persons`.
- Creating the `PerspectivesSystem$User` role and binding it to `me` (the `SocialEnvironment$Persons` instance for the local user).
- Adding the `BaseRepository` role to `PerspectivesSystem`.
- Creating the `Installer` user role in the model root context of the System model.

### Perspectives Language (PL) is responsible for:
- Creating `StartContexts`, `RecoveryPoint`, `SystemDataUpgrade`, and `SystemCaches` role instances.
- Binding the `SocialEnvironment` external role into `PerspectivesSystem` (in state `FirstInstallation` for first installations, or state `NoSocialEnvironment` for subsequent ones).
- Performing data-model upgrades (via the `Upgrader` role).

---

## Chronological Overview

### Step 1 – Database Setup (PDR, `setupUser` in `setupUser.purs`)

Before any context or role is created, the local CouchDB databases are configured:

- Views are set on the entities database (roles, contexts, credentials, filler/filled relations, etc.).
- Views are set on the models database.
- Views are set on the inverted-query database.

### Step 2 – Load the System Model (PDR, `addModelToLocalStore`)

The System model (`model://perspectives.domains#System`) is loaded into the local store. Loading a model triggers `createInitialInstances`, which detects it is the System model and calls `initSystem`.

### Step 3 – Create `PerspectivesSystem` context (PDR, `initSystem → createSystem`)

`initSystem` first calls `createSystem` (with `PerspectivesSystem$Installer` as the authoring role):

1. Obtains the unique system identifier (a CUID-derived string).
2. Constructs the `PerspectivesSystem` context with that identifier, setting the `CurrentLanguage` and `PreviousLanguage` external properties.

> **Note:** At this point there is no `PerspectivesSystem$User` yet. The `User` role is created later (Step 5), after `me` is established. The `Installer` role is flagged `isMe` in `createInitialInstances` (Step 6 below), so the PL state reactions that require the `Installer` as the authoring role can fire correctly once it is created.

**Authoring role:** `PerspectivesSystem$Installer`.

### Step 4a – Create `TheWorld`, `SocialEnvironment`, and `Persons` (PDR, first installation only)

If `isFirstInstallation = true` (a runtime option set by the client) and a public key is available, `initSystem` calls `createTheWorld` with `TheWorld$Initializer` (= `me`) as the authoring role. Inside `createTheWorld`:

1. Constructs the `TheWorld` context with the fixed identifier `"TheWorld"`.
2. Creates the user's `PerspectivesUsers` role inside `TheWorld`:
   - Identifier: the user's CUID (includes the storage scheme).
   - Sets the `PublicKey` property to the user's public key.
   - Marks this role as `isMe = true` (via `roleIsMe`), which also sets `context_me` on `TheWorld`.
3. Constructs the `SocialEnvironment` context with the fixed identifier `"TheSocialEnvironment"`.
4. Switches authoring role to `SocialEnvironment$SystemUser` and creates one `SocialEnvironment$Persons` role:
   - Filled with the user's `PerspectivesUsers` CUID.
   - Marked as `isMe = true` (via the `isMe` flag on `createAndAddRoleInstance_`).
   - **At this point `me` is available**: `computeMe_` will find this `Persons` instance.
5. Creates the serialisation persona: a second `PerspectivesUsers` role with the fixed identifier `"def:#serializationuser"` and `LastName = "Serialisation persona"`. Authored by `TheWorld$Initializer`.

**Authoring role for steps 1–3 and 5:** `TheWorld$Initializer` (= `me`).  
**Authoring role for step 4:** `SocialEnvironment$SystemUser` (= `me`).

### Step 4b – Execute Identity Document (PDR, subsequent installation)

If `isFirstInstallation = false` and an identity document is available:

1. Executes the identity document as a Perspectives transaction.  
   This reconstructs `TheWorld`, the user's `PerspectivesUsers` role, `SocialEnvironment`, and `SocialEnvironment$Persons` from the first installation.
2. Marks the `PerspectivesUsers` role as `isMe = true` in the local cache.
3. After this step, `computeMe_` can find the `SocialEnvironment$Persons` instance (via the filler relationship to the user's CUID), so `me` is available for Step 5.

### Step 5 – Create `PerspectivesSystem$User` and bind to `me` (PDR, `initSystem → createSystemUser`)

After the first/subsequent installation block, `initSystem` calls `createSystemUser` with `PerspectivesSystem$User` (`DEP.sysUser`) as the authoring role:

1. Creates the `PerspectivesSystem$User` role with the identifier `<systemId>$User`.
2. Marks this role as `isMe = true` (via `roleIsMe`), which sets `context_me` on `PerspectivesSystem`.
3. Calls `computeMe_` to retrieve the local user's `SocialEnvironment$Persons` instance.
4. Calls `setFirstBinding sysUser me Nothing` to fill `PerspectivesSystem$User` with the `Persons` instance, connecting the two identity anchors.

**Authoring role:** `PerspectivesSystem$User`.

### Step 6 – Add `BaseRepository` (PDR, `initSystem`)

`initSystem` adds a `BaseRepository` role to `PerspectivesSystem`, filled with the public external role of the Perspectives repository at `pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains$External`.

**Authoring role:** `TheWorld$Initializer` (= `me`), inherited from the outer `runMonadPerspectivesTransaction` call in `setupUser` after all `withAuthoringRole` blocks have returned. `PerspectivesSystem$User` has `perspective on BaseRepository: props (Domain) verbs (Consult)` (Consult only), but `TheWorld$Initializer` has no explicit perspective on `BaseRepository`. Since `PerspectivesSystem` is local-only, no peer checks this delta.

### Step 7 – Create Model Root Context and `Installer` Role (PDR, `createInitialInstances`)

After `initSystem` returns, `createInitialInstances` continues:

1. Constructs the model root context for `model://perspectives.domains#System`.
2. Creates the `Installer` user role inside that model root context, filled by the current user.
3. Marks the `Installer` role as `isMe = true`.

**Authoring role:** `PerspectivesSystem$User`.

### Step 8 – PL State Reactions (System domain context, on entry)

After the System model has been installed, the PL state machine evaluates states. In the domain context of `model://perspectives.domains#System`:

```
on entry
  do for sys:PerspectivesSystem$Installer
    letA
      start <- create role StartContexts in sys:MySystem
    in
      bind_ sys:MySystem >> extern to start
      Name = "My System" for start
      IsSystemModel = true for start
  do for Upgrader
    create role SystemDataUpgrade
```

The `Installer` creates a `StartContexts` role in `PerspectivesSystem` and sets its name and system-model flag. The `Upgrader` (= `me`) creates the `SystemDataUpgrade` role.

Then, state `FirstInstallation` is evaluated (first installation only):

```
state FirstInstallation =
  (callExternal util:SystemParameter( "IsFirstInstallation" ) returns Boolean)
  and (exists sys:MySocialEnvironment)
  on entry
    do for sys:PerspectivesSystem$Installer
      -- SocialEnvironment was created by the PDR in initSystem; just bind it into PerspectivesSystem.
      bind sys:MySocialEnvironment >> extern to SocialEnvironment in sys:MySystem
```

The `Installer` binds the already-created `SocialEnvironment` (created by PDR in Step 4a) into `PerspectivesSystem`. The context itself was created in PDR; PL just registers the binding.

### Step 9 – PL State Reactions in `PerspectivesSystem`

On entry of the `PerspectivesSystem` context, `User` executes:

```
on entry
  do for User
    create role RecoveryPoint
    create role SystemDataUpgrade
    MaxHistoryLength = 7 for extern
```

State `NoCaches`:

```
state NoCaches = not exists SystemCaches
  on entry
    do for User
      create context Caches bound to SystemCaches
```

State `NoSocialEnvironment` (relevant for subsequent installations):

```
state NoSocialEnvironment = (not exists SocialEnvironment) and (exists sys:MySocialEnvironment)
  on entry
    do for User
      bind sys:MySocialEnvironment >> extern to SocialEnvironment
```

**Authoring role:** `PerspectivesSystem$User`.

> **Note:** State `InitMe` has been removed from `SocialEnvironment`. `SocialEnvironment$Persons` is now created in the PDR (Step 4a) for first installations, and via the identity document for subsequent installations. `SocialEnvironment$Me` is a calculated role (`= me`); no instance needs to be created. This ensures `me` is available before any PL state reactions execute.

---

## User Roles Involved

| Role | Description | Context |
|------|-------------|---------|
| `PerspectivesSystem$Installer` | Created by PDR and flagged `isMe`. Used as authoring role when creating `PerspectivesSystem` context and binding `SocialEnvironment` into it. Performs initial setup in PL states. | model root context (= System domain context) |
| `TheWorld$Initializer` | A calculated role equal to `me`. Used as the authoring role when creating `TheWorld`, `PerspectivesUsers`, `SocialEnvironment`, and the serialisation persona. | `TheWorld` |
| `SocialEnvironment$SystemUser` | A calculated role equal to `me`. Used as the authoring role when creating `SocialEnvironment$Persons`. | `SocialEnvironment` |
| `PerspectivesSystem$User` | The main user role of the installation. Indexed as `sys:Me`. Created after `me` is established; filled with the `SocialEnvironment$Persons` instance. Used to author `RecoveryPoint`, `SystemDataUpgrade`, `SystemCaches`, and other system resources. | `PerspectivesSystem` |

---

## Perspective Analysis

For each role that authors changes during initialization, we check whether its perspectives are sufficient to justify those changes.

### `PerspectivesSystem$Installer` as authoring role

**Changes made (PDR):** Creates `PerspectivesSystem` context (Step 3). The `PerspectivesSystem` context itself is a locally-unique installation artefact; no peer would complain about its creation.

**Changes made (PL):** Creates `StartContexts` (Step 8), binds `SocialEnvironment` into `PerspectivesSystem` (Step 8 `FirstInstallation` state), creates `IndexedContexts` and `IndexedRoles` entries.

**Relevant perspectives (from the arc source):**

```arc
user Installer
  perspective on StartContexts
    only (Create, CreateAndFill, Remove, RemoveContext, Fill)
    props (Name, IsSystemModel) verbs (SetPropertyValue)
  perspective on IndexedContexts
    only (Create, Fill, Remove, RemoveContext)
    props (IndexedContexts$Name) verbs (SetPropertyValue)
  perspective on IndexedRoles
    only (Create, Fill, Remove, RemoveContext)
    props (IndexedRoles$Name) verbs (SetPropertyValue)
  perspective on BaseRepository
    only (CreateAndFill)
  perspective on SocialEnvironment
    only (CreateAndFill, Fill)
```

**Assessment:**
- `PerspectivesSystem` creation (PDR): not synchronized; no peer perspective check required.
- `StartContexts`: `Create` and `SetPropertyValue` for `Name` and `IsSystemModel` – **sufficient**.
- `SocialEnvironment` binding: `Fill` – **sufficient**.
- `IndexedContexts`, `IndexedRoles`: `Create`, `Fill` – **sufficient**.

### `TheWorld$Initializer` as authoring role

**Changes made:** Creates `TheWorld` context, the user's `PerspectivesUsers` role (with `PublicKey`), `SocialEnvironment` context, and the serialisation persona.

**Relevant perspectives:**

```arc
user Initializer = me
  perspective on PerspectivesUsers
    only (Create)
    props (Identifiable$PublicKey, SharedFileServerKey, HasKey) verbs (SetPropertyValue, AddPropertyValue)
```

**Assessment:**
- `Create` on `PerspectivesUsers` – **sufficient** to create the role instances.
- `SetPropertyValue` on `PublicKey` (via `Identifiable$PublicKey`) – **sufficient** to store the public key.
- `TheWorld` context creation: not synchronized; no perspective check required.
- `SocialEnvironment` context creation: `SocialEnvironment` is a shared context, but its creation here is local. The binding into `PerspectivesSystem` (which determines synchronization) is done separately by `Installer` who has the `Fill` perspective on `SocialEnvironment`.
- `SetPropertyValue` on `LastName` (for the serialisation persona): the `Initializer`'s perspective covers `Identifiable$PublicKey`, `SharedFileServerKey`, and `HasKey`, but **not** `Identifiable$LastName`. The serialisation persona's `LastName` is set without an explicit perspective granting `SetPropertyValue` for `LastName` to `Initializer`. However, the serialisation persona (`def:#serializationuser`) is a local artifact — it is never synchronized to peers — so no peer perspective check is triggered for this delta.

### `SocialEnvironment$SystemUser` as authoring role

**Changes made:** Creates `SocialEnvironment$Persons` filled with the user's `PerspectivesUsers` instance.

**Relevant perspectives:**

```arc
user SystemUser = me
  perspective on Persons
    only (Create, Fill)
    props (FirstName, LastName) verbs (SetPropertyValue, Consult)
```

**Assessment:**
- `Create` and `Fill` on `Persons` – **sufficient** to create and fill the `Persons` entry.

### `PerspectivesSystem$User` as authoring role

**Changes made:** Creates `PerspectivesSystem$User` itself, binds it to `Persons` (`me`), creates `RecoveryPoint`, `SystemDataUpgrade`, `SystemCaches`, and (on subsequent installations) binds `SocialEnvironment`.

**Relevant perspectives:**

```arc
user User (mandatory) filledBy (Persons + PerspectivesUsers)
  perspective on User
    only (Create, Fill)
    props (LastName, FirstName, PublicKey) verbs (SetPropertyValue)
  perspective on BaseRepository
    props (Domain) verbs (Consult)
  perspective on SocialEnvironment
    only (CreateAndFill, Fill)
  perspective on SystemCaches
    defaults
  perspective on RecoveryPoint
    only (Create, Remove)
    props (InstancesLastSeq, ModelsLastSeq) verbs (Consult, SetPropertyValue)
  perspective on SystemDataUpgrade
    only (Create, Fill)
```

**Assessment:**
- `PerspectivesSystem$User` creation and binding to `Persons`: `only (Create, Fill)` – **sufficient**.
- `SocialEnvironment` binding (`NoSocialEnvironment` state, subsequent installation): `Fill` – **sufficient**.
- `SystemCaches`, `RecoveryPoint`, `SystemDataUpgrade`: `Create` verb – **sufficient**.

---

## Summary Diagram

```
PDR (Purescript)                                  PL (Perspectives Language)
─────────────────────────────────────────────     ─────────────────────────────────────────
1. setupUser: set up database views
2. addModelToLocalStore (System model)
3. createInitialInstances → initSystem:
   withAuthoringRole Installer:
   a. createSystem:
      - create PerspectivesSystem context
   ── FIRST INSTALLATION ──────────────────
   withAuthoringRole TheWorld$Initializer (= me):
   b. createTheWorld:
      - create TheWorld context
      - create PerspectivesUsers (user, PublicKey)
        isMe=true on PerspectivesUsers
      - create SocialEnvironment context
      withAuthoringRole SocialEnvironment$SystemUser (= me):
        - create SocialEnvironment$Persons
          filled with PerspectivesUsers, isMe=true
          *** me is now available ***
      - create serializationuser (PerspectivesUsers)
   ── SUBSEQUENT INSTALLATION ─────────────
   b'. execute identity document
       (reconstructs TheWorld + PerspectivesUsers
        + SocialEnvironment + Persons)
       mark PerspectivesUsers isMe=true
       *** me is now available ***
   ──────────────────────────────────────
   withAuthoringRole PerspectivesSystem$User:
   c. createSystemUser:
      - create PerspectivesSystem$User (<sysId>$User)
        isMe=true
      - computeMe_ → retrieve Persons instance
      - setFirstBinding User ← Persons
   d. add BaseRepository to PerspectivesSystem
4. create model root context
5. create Installer role (isMe = true)
                                                  6. Domain on entry (Installer = me):
                                                     - create StartContexts in MySystem
                                                       (set Name, IsSystemModel)
                                                  7. Domain on entry (Upgrader = me):
                                                     - create SystemDataUpgrade
                                                  8. State FirstInstallation (Installer = me):
                                                     [first installation only]
                                                     - bind MySocialEnvironment >> extern
                                                       to SocialEnvironment in MySystem
                                                  9. PerspectivesSystem on entry (User = me):
                                                     - create RecoveryPoint
                                                     - create SystemDataUpgrade
                                                     - MaxHistoryLength = 7 for extern
                                                  10. State NoCaches (User = me):
                                                      - create Caches context → SystemCaches
                                                  11. State NoSocialEnvironment (User = me):
                                                      [subsequent installation only]
                                                      - bind MySocialEnvironment >> extern
                                                        to SocialEnvironment
                                                  [State InitMe removed — Me is now
                                                   a calculated role (= me)]
```

---

## Source References

| Topic | File |
|-------|------|
| `initSystem`, `createInitialInstances` | `packages/perspectives-core/src/core/computedValues/couchdbExternalFunctions.purs` |
| `setupUser` | `packages/perspectives-core/src/core/setupUser.purs` |
| `computeMe_`, `computeMe` | `packages/perspectives-core/src/core/instances/me.purs` |
| System model (PL, current) | `packages/perspectives-core/src/model/perspectivesSysteem@6.3.arc` |
| Stable identifiers (CUID mapping) | `packages/perspectives-core/src/core/modelDependencies.purs` |
| RuntimeOptions (incl. `isFirstInstallation`) | `packages/perspectives-core/src/core/coreTypes.purs` |
| `createAccount` (entry point for new installation) | `packages/perspectives-core/src/core/Main.purs` |

---

## Notes on the Authoring Role in Transactions

When the PDR creates or modifies data (contexts, roles, properties), it records each change as a **delta**. Each delta is signed by, and carries the type of, the current **authoring role** — the role acting on behalf of the local user in the current transaction. Peers that receive these deltas check whether the sender, in their capacity as that authoring role, had a sufficient perspective to make the change.

The authoring role is set in two ways:

1. **Explicitly in PDR code** — using `withAuthoringRole authoringRole ...` from `Perspectives.Assignment.Update`, which temporarily sets `Transaction.authoringRole` for the duration of the monadic action.
2. **Implicitly by the PL automatic-action clause** — the role named after `do for <A>` becomes the authoring role for all statements in that block. This is the same mechanism: the state compiler calls `withAuthoringRole` under the hood when evaluating `do for A`.

In both cases, the authoring role is a `RoleType` (enumerated or calculated). The PDR stores the calculated role type as-is in the delta; peers receiving it look up the concrete instances when checking perspectives.

---

## The `me` Identity Anchor

### What `me` resolves to

The PL keyword `me` is a query step that resolves, at runtime, to the `SocialEnvironment$Persons` role instance that is filled with the local user's `PerspectivesUsers` role. Its implementation (in `Perspectives.Instances.Me`):

```purescript
computeMe :: MonadPerspectivesQuery RoleInstance
computeMe = do
  PerspectivesUser pUser <- lift $ lift getPerspectivesUser
  getFilledRoles (ContextType socialEnvironment)
                 (EnumeratedRoleType socialEnvironmentPersons)
                 (RoleInstance pUser)
```

For `me` to return a value, three things must exist: `SocialEnvironment`, `SocialEnvironment$Persons`, and the filler relationship from `SocialEnvironment$Persons` to `PerspectivesUsers`.

### Why `me` must be created before PL reactions execute

The System model defines all six "local user" calculated roles as `X = me`:

| Role | Context |
|------|---------|
| `Upgrader` | System domain context |
| `Initializer` | `TheWorld` |
| `SystemUser` | `SocialEnvironment` |
| `WWWUser` | `PerspectivesSystem` |
| `Manager` | `Caches` |
| `Visitor` | public context |

If `me` were not available when the PL state machine first fires (Steps 8 onwards), all of these calculated roles would be empty and their `do for <role>` blocks would be silently skipped.

`SocialEnvironment` and `SocialEnvironment$Persons` are therefore created in PDR (Steps 4a/4b), before any PL state reactions execute. This is the key constraint driving the initialization order.

### `SocialEnvironment$Me` is a calculated role

`SocialEnvironment$Me` is defined as `user Me = me` — a calculated (derived) role with no persistent instances. No instance of `SocialEnvironment$Me` is ever created; its value is always computed on demand from `computeMe`. This is why the previous attempt to call `createAndAddRoleInstance_` with `EnumeratedRoleType socialEnvironmentMe` failed: the type does not exist in the `enumeratedRoles` section of the deployed DomeinFile.

### `PerspectivesSystem$User` is filled with `me`

After `me` is available, the PDR creates `PerspectivesSystem$User` and immediately binds it to the `SocialEnvironment$Persons` instance (via `setFirstBinding`). This ensures:

- `sys:Me` (the indexed role) refers to the `User` instance.
- `PerspectivesSystem$User` is filled by a `Persons` instance (satisfying `filledBy (Persons + PerspectivesUsers)`).
- The two identity anchors (`sys:Me` and `me`) are linked at the instance level.

---

## On the Serialisation Persona

The serialisation persona (`def:#serializationuser`) is a fictive `PerspectivesUsers` instance created in `TheWorld` with a fixed identifier. It is used by `serialisedAsDeltasForUserType` as a stand-in when serialising context data for a user *type* (not a specific instance):

```purescript
serialisedAsDeltasFor_ cid (RoleInstance "def:#serializationuser") userType
```

The persona is needed because serialisation deltas require a concrete role instance identifier to determine self-only perspectives. The code comment explicitly notes that the authoring role is "in effect ignored" in this path — only the user identifier matters for the self-only check.

The persona is created in PDR (inside `createTheWorld`) with `TheWorld$Initializer` as the authoring role. The `LastName = "Serialisation persona"` property is set, even though `Initializer`'s perspective on `PerspectivesUsers` does not explicitly include `SetPropertyValue` for `LastName`. This is intentional: the serialisation persona is a local artifact — it is never synchronized to peers — so no peer will verify the perspective.
