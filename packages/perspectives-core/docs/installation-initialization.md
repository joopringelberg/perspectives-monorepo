# Installation Initialization Process

This document describes what happens when a new Perspectives installation is created, covering both the Purescript (PDR) side and the Perspectives Language (PL) side.

## Background

Perspectives is a model-driven system. When a new installation is created, a set of shared contexts are created from scratch and stored locally:

- **TheWorld** – the same instance for every peer (identifier: `"TheWorld"`).
- **SocialEnvironment** – the same identifier for every peer (`"TheSocialEnvironment"`).
- **PerspectivesSystem** – a unique instance per installation, with an identifier derived from the user's CUID.

The base model is `model://perspectives.domains#System` (source: `perspectivesSysteem.arc`).

There are two scenarios:
1. **First installation** – the user sets up Perspectives for the first time.
2. **Subsequent installation** – the user adds Perspectives on a second device, using an identity document exported from the first installation.

---

## Division of Labour

### Purescript (PDR) is responsible for:
- Setting up the CouchDB databases and their views.
- Loading the System model (and other required models) into the local store.
- Creating the `PerspectivesSystem` context instance and the `PerspectivesSystem$User` role.
- On a first installation: creating the `TheWorld` context and the user's `PerspectivesUsers` role (including storing the public key) inside it.
- On a subsequent installation: executing the identity document (a serialised transaction from the first installation) to reconstruct `TheWorld` and the user's `PerspectivesUsers` role.
- Adding the `BaseRepository` role to `PerspectivesSystem`.
- Creating the `Installer` user role in the model root context of the System model.

### Perspectives Language (PL) is responsible for:
- Creating the `SocialEnvironment` context (on the first installation, from a state in the System domain context).
- Binding the `SocialEnvironment` external role into `PerspectivesSystem`.
- Populating `SocialEnvironment$Me` and `SocialEnvironment$Persons` from `TheWorld$PerspectivesUsers`.
- Creating `StartContexts`, `RecoveryPoint`, `SystemDataUpgrade`, and `SystemCaches` role instances.

---

## Chronological Overview

### Step 1 – Database Setup (PDR, `setupUser` in `SetupUser.purs`)

Before any context or role is created, the local CouchDB databases are configured:

- Views are set on the entities database (roles, contexts, credentials, filler/filled relations, etc.).
- Views are set on the models database.
- Views are set on the inverted-query database.

The transaction is run with `PerspectivesSystem$User` as the authoring role (the role that signs the deltas).

### Step 2 – Load the System Model (PDR, `addModelToLocalStore`)

The System model (`model://perspectives.domains#System`) is loaded into the local store. Loading a model triggers `createInitialInstances`, which detects it is the System model and calls `initSystem`.

### Step 3 – Create `PerspectivesSystem` and `PerspectivesSystem$User` (PDR, `initSystem`)

`initSystem` (in `Perspectives.Extern.Couchdb`) does the following:

1. Constructs the `PerspectivesSystem` context with a unique identifier derived from the user's system identifier (a CUID).  
2. Creates the `PerspectivesSystem$User` role inside that context, with the identifier `<systemId>$User`.  
3. Marks this role as `isMe = true` in the cache so the PDR recognises it as the local user.

**Authoring role:** `PerspectivesSystem$User` (signs the creation deltas for these resources).

### Step 4a – Create `TheWorld` and `PerspectivesUsers` (PDR, first installation only)

If `isFirstInstallation = true` (a runtime option set by the client) and a public key is available:

1. Constructs the `TheWorld` context with the fixed identifier `"TheWorld"`.  
2. Switches authoring role to `TheWorld$Initializer` (a calculated role equal to `sys:Me`).  
3. Creates the user's `PerspectivesUsers` role inside `TheWorld`:  
   - Identifier: the user's CUID (a CUID, includes the storage scheme).  
   - Sets the `PublicKey` property to the user's public key.  
   - Marks this role as `isMe = true`.  
   - Binds it to `PerspectivesSystem$User` (fills `sys:Me`).  
4. Creates the serialisation persona: a second `PerspectivesUsers` role with the fixed identifier `"def:#serializationuser"` and `LastName = "Serialisation persona"`. This special persona is used when serialising data for peers.

**Authoring role for step 3 and 4:** `TheWorld$Initializer` (= `sys:Me`).

### Step 4b – Execute Identity Document (PDR, subsequent installation)

If `isFirstInstallation = false` and an identity document is available (passed in by the client from the previously exported file):

1. Executes the identity document as a Perspectives transaction.  
   This reconstructs `TheWorld` and the user's `PerspectivesUsers` role from the first installation, including the `PublicKey`.  
2. Marks the `PerspectivesUsers` role as `isMe = true`.

The identity document is a JSON file that was produced on the first device using the action `ExportForAnotherInstallation` in `SocialEnvironment$SystemUser` (PL).

### Step 5 – Add `BaseRepository` (PDR, `initSystem`)

After the conditional block, `initSystem` adds a `BaseRepository` role to `PerspectivesSystem`, filled with the public external role of the Perspectives repository at `pub:https://perspectives.domains/cw_servers_and_repositories/#perspectives_domains$External`.

**Authoring role:** `PerspectivesSystem$User`.

### Step 6 – Create Model Root Context and `Installer` Role (PDR, `createInitialInstances`)

After `initSystem` returns, `createInitialInstances` continues:

1. Constructs the model root context for `model://perspectives.domains#System` (identifier: `<manifestName>_modelRootContext`).  
2. Creates the `Installer` user role inside that model root context, filled by the current user.  
3. Marks the `Installer` role as `isMe = true`.

**Authoring role:** `PerspectivesSystem$User`.

### Step 7 – PL State Reactions (System domain context, first installation)

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
```

The `Installer` creates a `StartContexts` role in `PerspectivesSystem` and sets its name and system-model flag.

Then, state `FirstInstallation` is evaluated:

```
state FirstInstallation =
  (callExternal util:SystemParameter( "IsFirstInstallation" ) returns Boolean)
  and (exists sys:TheWorld >> PerspectivesUsers)
  on entry
    do for sys:PerspectivesSystem$Installer
      letA
        mysocialenvironment <- create context SocialEnvironment named "TheSocialEnvironment"
      in
        bind mysocialenvironment >> extern to SocialEnvironment in sys:MySystem
```

The `Installer` creates the `SocialEnvironment` context (with the fixed indexed name `"TheSocialEnvironment"`) and binds its external role into the `SocialEnvironment` slot of `PerspectivesSystem`.

### Step 8 – PL State Reactions in `PerspectivesSystem`

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

State `NoSocialEnvironment` (relevant for subsequent installations, where `SocialEnvironment` already exists as `sys:MySocialEnvironment` but is not yet linked):

```
state NoSocialEnvironment = (not exists SocialEnvironment) and (exists sys:MySocialEnvironment)
  on entry
    do for User
      bind sys:MySocialEnvironment >> extern to SocialEnvironment
```

**Authoring role:** `PerspectivesSystem$User`.

### Step 9 – PL State Reactions in `SocialEnvironment`

State `InitMe` is entered when this is the first installation and `SocialEnvironment$Me` does not yet exist:

```
state InitMe =
  (callExternal util:SystemParameter( "IsFirstInstallation" ) returns Boolean)
  and not exists Me
  on entry
    do for SystemUser
      bind sys:TheWorld >> PerspectivesUsers >>= first to Me
      bind sys:TheWorld >> PerspectivesUsers >>= first to Persons
```

The `SystemUser` (= `sys:Me`, i.e. `PerspectivesSystem$User`) binds the user's `PerspectivesUsers` role to `SocialEnvironment$Me` and creates a `Persons` entry for it.

**Authoring role:** `SocialEnvironment$SystemUser` (= `sys:Me`).

---

## User Roles Involved

| Role | Description | Context |
|------|-------------|---------|
| `PerspectivesSystem$User` | The main user role of the installation. Indexed as `sys:Me`. Used to author most setup transactions. | `PerspectivesSystem` |
| `PerspectivesSystem$Installer` | A non-calculated user role in the model root context. Created by the PDR and flagged as `isMe`. Performs initial setup in PL states. | model root context (= model domain context) |
| `TheWorld$Initializer` | A calculated role equal to `sys:Me`. Used as the authoring role when creating `PerspectivesUsers` inside `TheWorld`. | `TheWorld` |
| `SocialEnvironment$SystemUser` | A calculated role equal to `sys:Me`. Fills `SocialEnvironment$Me` and `SocialEnvironment$Persons` on first installation. Also used to export the identity document for a second device. | `SocialEnvironment` |

---

## Perspective Analysis

For each role that authors changes during initialization, we check whether its perspectives are sufficient to justify those changes. Perspectives Distributed Runtime (PDR) checks that the authoring role (the "subject") has the right to make a change when processing deltas.

### `PerspectivesSystem$User` as authoring role

**Changes made:** Creates `PerspectivesSystem`, `PerspectivesSystem$User`, the model root context, the `Installer` role, `BaseRepository`, `RecoveryPoint`, `SystemDataUpgrade`, `SystemCaches`, and (on subsequent installations) binds `SocialEnvironment`.

**Relevant perspectives (from the arc source):**

```arc
user User (mandatory) filledBy PerspectivesUsers
  perspective on User
    only (Create, Fill)
    props (LastName, FirstName, PublicKey) verbs (SetPropertyValue)
  perspective on StartContexts
    props (Name) verbs (Consult)
  perspective on BaseRepository
    only (CreateAndFill)
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
- `PerspectivesSystem` is created by the PDR directly, before the state machine is active; no delta is sent by the PDR for this creation in the same way as regular data changes.  
- `PerspectivesSystem$User` has the perspective `only (Create, Fill)` on itself – **sufficient** to create and fill itself.  
- `BaseRepository`: `only (CreateAndFill)` – **sufficient**.  
- `SocialEnvironment`: `only (CreateAndFill, Fill)` – **sufficient** for the `NoSocialEnvironment` state.  
- `SystemCaches`, `RecoveryPoint`, `SystemDataUpgrade`: perspectives with `Create` verb – **sufficient**.  
- `StartContexts`: The `User` has `perspective on StartContexts props (Name) verbs (Consult)` – this gives read access. The actual `StartContexts` creation is done by the `Installer`, not the `User`. The `User` perspective does not include `Create`; however, the `Installer` has the `Create` verb (see below).

### `PerspectivesSystem$Installer` as authoring role

**Changes made:** Creates `StartContexts` (incl. setting `Name` and `IsSystemModel`), creates `SocialEnvironment` (on first installation), and binds it into `PerspectivesSystem`.

**Relevant perspectives:**

```arc
user Installer
  perspective on StartContexts
    only (Create, CreateAndFill, Remove, RemoveContext, Fill)
    props (Name, IsSystemModel) verbs (SetPropertyValue)
  perspective on SocialEnvironment
    only (CreateAndFill, Fill)
```

**Assessment:**  
- `StartContexts`: `Create` and `SetPropertyValue` for `Name` and `IsSystemModel` – **sufficient**.  
- `SocialEnvironment`: `CreateAndFill` and `Fill` – **sufficient** to create and bind the `SocialEnvironment` context.

### `TheWorld$Initializer` as authoring role

**Changes made:** Creates the user's `PerspectivesUsers` role in `TheWorld` (with `PublicKey`), and creates the serialisation persona.

**Relevant perspectives:**

```arc
user Initializer = sys:Me
  perspective on PerspectivesUsers
    only (Create)
    props (Identifiable$PublicKey, SharedFileServerKey, HasKey) verbs (SetPropertyValue, AddPropertyValue)
```

**Assessment:**  
- `Create` on `PerspectivesUsers` – **sufficient** to create the role instances.  
- `SetPropertyValue` on `PublicKey` (via `Identifiable$PublicKey`) – **sufficient** to store the public key.  
- `SetPropertyValue` on `LastName` (used for the serialisation persona): `identifiableLastName` is the property used; however, this property is defined via the `sys:Identifiable` aspect. The `Initializer`'s perspective covers `Identifiable$PublicKey`, `SharedFileServerKey`, and `HasKey`, but **not** `Identifiable$LastName`. The serialisation persona's `LastName` is therefore set without an explicit perspective granting `SetPropertyValue` for `LastName` to `Initializer`.

> **Note:** The serialisation persona (`LastName = "Serialisation persona"`) is set in the PDR with `TheWorld$Initializer` as the authoring role, but `Initializer` does not have an explicit perspective granting `SetPropertyValue` for `LastName`. This may warrant a review to determine whether a perspective should be added, or whether this is an intentional shortcut because the serialisation persona is only relevant locally and is never shared.

### `SocialEnvironment$SystemUser` as authoring role

**Changes made:** Binds `TheWorld$PerspectivesUsers` to `SocialEnvironment$Me` and to `SocialEnvironment$Persons`.

**Relevant perspectives:**

```arc
user SystemUser = sys:Me
  perspective on Me
    only (Create, Fill)
    props (FirstName, LastName, PublicKey, MyIdentity) verbs (Consult)
    props (MyIdentity, Cancelled) verbs (SetPropertyValue, Consult)
  perspective on Persons
    only (Create, Fill)
    props (FirstName, LastName) verbs (SetPropertyValue, Consult)
```

**Assessment:**  
- `Me`: `only (Create, Fill)` – **sufficient** to bind a `PerspectivesUsers` instance to `Me`.  
- `Persons`: `only (Create, Fill)` – **sufficient** to create and fill a `Persons` entry.

---

## Summary Diagram

```
PDR (Purescript)                                  PL (Perspectives Language)
─────────────────────────────────────────────     ─────────────────────────────────────────
1. setupUser: set up database views
2. addModelToLocalStore (System model)
3. createInitialInstances → initSystem:
   a. create PerspectivesSystem context
   b. create PerspectivesSystem$User role
      (isMe = true)
   ── FIRST INSTALLATION ──────────────
   c. create TheWorld context
   d. withAuthoringRole TheWorld$Initializer:
      - create PerspectivesUsers (user, with PublicKey)
      - create PerspectivesUsers (serializationuser)
   ── SUBSEQUENT INSTALLATION ─────────
   c'. execute identity document
       (reconstructs TheWorld + PerspectivesUsers)
   ────────────────────────────────────
   e. add BaseRepository to PerspectivesSystem
4. create model root context
5. create Installer role (isMe = true)
                                                  6. Domain on entry (Installer):
                                                     - create StartContexts in MySystem
                                                     (set Name, IsSystemModel)
                                                  7. State FirstInstallation (Installer):
                                                     - create SocialEnvironment "TheSocialEnvironment"
                                                     - bind SocialEnvironment >> extern to MySystem
                                                  8. PerspectivesSystem on entry (User):
                                                     - create RecoveryPoint
                                                     - create SystemDataUpgrade
                                                     - set MaxHistoryLength = 7
                                                  9. State NoCaches (User):
                                                     - create Caches context → SystemCaches
                                                  10. State NoSocialEnvironment (User):
                                                      [second device only]
                                                      - bind MySocialEnvironment to SocialEnvironment
                                                  11. SocialEnvironment state InitMe (SystemUser):
                                                      - bind TheWorld>>PerspectivesUsers to Me
                                                      - bind TheWorld>>PerspectivesUsers to Persons
```

---

## Source References

| Topic | File |
|-------|------|
| `initSystem`, `createInitialInstances` | `packages/perspectives-core/src/core/computedValues/couchdbExternalFunctions.purs` |
| `setupUser` | `packages/perspectives-core/src/core/setupUser.purs` |
| System model (PL) | `packages/perspectives-core/src/model/perspectivesSysteem.arc` |
| RuntimeOptions (incl. `isFirstInstallation`) | `packages/perspectives-core/src/core/coreTypes.purs` |
| `createAccount` (entry point for new installation) | `packages/perspectives-core/src/core/Main.purs` |
