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

### Step 9 – ~~PL State Reactions in `SocialEnvironment`~~ *(removed)*

> **Note:** State `InitMe` in `SocialEnvironment` has been removed. `SocialEnvironment$Me` and
> `SocialEnvironment$Persons` are now created in the PDR (`initSystem`, Step 3) for first
> installations, and come from the identity document for subsequent installations. This makes `me`
> available before any PL state reactions execute.

---

## User Roles Involved

| Role | Description | Context |
|------|-------------|---------|
| `PerspectivesSystem$User` | The main user role of the installation. Indexed as `sys:Me`. Used to author most setup transactions. | `PerspectivesSystem` |
| `PerspectivesSystem$Installer` | A non-calculated user role in the model root context. Created by the PDR and flagged as `isMe`. Performs initial setup in PL states. | model root context (= model domain context) |
| `TheWorld$Initializer` | A calculated role equal to `me`. Used as the authoring role when creating `PerspectivesUsers` inside `TheWorld`. | `TheWorld` |
| `SocialEnvironment$SystemUser` | A calculated role equal to `me`. Used as the authoring role when creating `SocialEnvironment$Me` and `SocialEnvironment$Persons` in PDR, and to export the identity document for a second device. | `SocialEnvironment` |

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
   d. withAuthoringRole TheWorld$Initializer (= me):
      - create PerspectivesUsers (user, with PublicKey)
      - create PerspectivesUsers (serializationuser)
   e. withAuthoringRole SocialEnvironment$SystemUser (= me):
      - create SocialEnvironment "TheSocialEnvironment"
      - create SocialEnvironment$Me ← filled with PerspectivesUsers
      - create SocialEnvironment$Persons ← filled with PerspectivesUsers
      *** me is now available ***
   ── SUBSEQUENT INSTALLATION ─────────
   c'. execute identity document
       (reconstructs TheWorld + PerspectivesUsers
        + SocialEnvironment + Me + Persons)
       then: roleIsMe on SocialEnvironment$Me
       *** me is now available ***
   ────────────────────────────────────
   f. add BaseRepository to PerspectivesSystem
4. create model root context
5. create Installer role (isMe = true)
                                                  6. Domain on entry (Installer = me):
                                                     - create StartContexts in MySystem
                                                     (set Name, IsSystemModel)
                                                  7. Domain on entry (Upgrader = me):
                                                     - create SystemDataUpgrade
                                                  8. State FirstInstallation (Installer = me):
                                                     - bind MySocialEnvironment >> extern
                                                       to SocialEnvironment in MySystem
                                                  9. PerspectivesSystem on entry (User = me):
                                                     - create RecoveryPoint
                                                     - create SystemDataUpgrade
                                                     - set MaxHistoryLength = 7
                                                  10. State NoCaches (User = me):
                                                      - create Caches context → SystemCaches
                                                  11. State NoSocialEnvironment (User = me):
                                                      [second device only]
                                                      - bind MySocialEnvironment to SocialEnvironment
                                                  [State InitMe removed]
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

---

## Notes on the Authoring Role in Transactions

When the PDR creates or modifies data (contexts, roles, properties), it records each change as a **delta**. Each delta is signed by, and carries the type of, the current **authoring role** — the role acting on behalf of the local user in the current transaction. Peers that receive these deltas check whether the sender, in their capacity as that authoring role, had a sufficient perspective to make the change.

The authoring role is set in two ways:

1. **Explicitly in PDR code** — using `withAuthoringRole authoringRole ...` from `Perspectives.Assignment.Update`, which temporarily sets `Transaction.authoringRole` for the duration of the monadic action.  
2. **Implicitly by the PL automatic-action clause** — the role named after `do for <A>` becomes the authoring role for all statements in that block. This is the same mechanism: the state compiler calls `withAuthoringRole` under the hood when evaluating `do for A`.

In both cases, the authoring role is a `RoleType` (enumerated or calculated). The PDR always resolves calculated roles to their concrete enumerated type when recording deltas and when checking perspectives.

---

## Implemented: `me` as the Identity Anchor

### Background: the `me` keyword

The PL keyword `me` is a query step that resolves, at runtime, to the `SocialEnvironment$Persons` role instance that is filled with the local user's `PerspectivesUsers` role. Its implementation (in `Perspectives.Queries.UnsafeCompiler`) is:

```purescript
compileFunction (SQD _ (DataTypeGetter MeF) _ _ _) = pure $ \_ -> do
  PerspectivesUser pUser <- lift $ lift getPerspectivesUser
  unwrap <$> getFilledRoles
    (ContextType socialEnvironment)
    (EnumeratedRoleType socialEnvironmentPersons)
    (RoleInstance pUser)
```

That is, `me` looks up the `SocialEnvironment$Persons` roles filled by the user's CUID. For `me` to return a value, three things must exist: `SocialEnvironment`, `SocialEnvironment$Persons`, and the filler relationship from `SocialEnvironment$Persons` to `PerspectivesUsers`.

### The six `X = sys:Me` calculated roles

The System model currently defines six calculated user roles as `X = sys:Me`:

| Role | Context |
|------|---------|
| `Upgrader` | System domain context |
| `Initializer` | `TheWorld` |
| `SystemUser` | `SocialEnvironment` |
| `User` (via `FillWithPerspectivesUser` state) | `PerspectivesSystem` |
| `WWWUser` | `PerspectivesSystem` |
| `Manager` | `Caches` |
| `Visitor` | public context |

`sys:Me` resolves to the indexed instance of `PerspectivesSystem$User` — a locally-scoped indexed role whose value is `def:#<CUID>$User`. This is the earlier, PDR-centric identity anchor. The `me` keyword provides a more semantically rich anchor: the user as a social person (`SocialEnvironment$Persons`), backed by a `PerspectivesUsers` role in `TheWorld`. Changing all six definitions from `= sys:Me` to `= me` would make the social identity (`SocialEnvironment$Persons`) the canonical way the system recognises "the local user".

### The timing problem

Under the current initialization order, `me` only becomes available in **Step 9** (the `InitMe` state of `SocialEnvironment`), which is far too late: several automatic actions in Steps 6–8 already run using roles defined as `= sys:Me`. If those roles were instead defined as `= me`, their computed value would be empty during Steps 6–8 and the automatic actions would be skipped.

### Revised initialization sequence (implemented)

To allow all calculated roles to be defined as `X = me`, `SocialEnvironment$Persons` (and its fill chain) must exist **before** any PL state machine reactions execute — i.e., it must be created in PDR, not in PL.

The implemented change moved steps 4d and 5 (creation of `SocialEnvironment` and population of `Me`/`Persons`) entirely into the PDR, right after `PerspectivesUsers` is created:

```
PDR (Purescript)                                  PL (Perspectives Language)
─────────────────────────────────────────────     ─────────────────────────────────────────
1. setupUser: set up database views
2. addModelToLocalStore (System model)
3. createInitialInstances → initSystem:
   a. create PerspectivesSystem context
   b. create PerspectivesSystem$User (isMe=true)
   ── FIRST INSTALLATION ──────────────────
   c. create TheWorld context
   d. (withAuthoringRole TheWorld$Initializer):
      - create PerspectivesUsers (user, PublicKey)
   e. create SocialEnvironment "TheSocialEnvironment"
      - create SocialEnvironment$Me  ← filled with PerspectivesUsers
      - create SocialEnvironment$Persons ← filled with PerspectivesUsers
      *** me is now available ***
   ── SUBSEQUENT INSTALLATION ─────────────
   c'. execute identity document
       (reconstructs TheWorld + PerspectivesUsers
        + SocialEnvironment + Me + Persons)
      *** me is now available ***
   ─────────────────────────────────────────
   f. add BaseRepository to PerspectivesSystem
4. create model root context
5. create Installer role (isMe = true)
                                                  6. Domain on entry (Installer = me):
                                                     - create StartContexts in MySystem
                                                  7. State FirstInstallation (Installer = me):
                                                     - bind SocialEnvironment to MySystem
                                                     [SocialEnvironment already exists]
                                                  8. PerspectivesSystem on entry (User = me):
                                                     - create RecoveryPoint
                                                     - create SystemDataUpgrade
                                                  9. State NoCaches (User = me):
                                                     - create Caches → SystemCaches
                                                  10. State InitMe: now a no-op
                                                      [Me and Persons already filled by PDR]
```

Key consequences of this implemented change:
- `SocialEnvironment` is created in PDR for both first and subsequent installations (replacing the current PL state `FirstInstallation`).
- `SocialEnvironment$Me` and `SocialEnvironment$Persons` are created in PDR (replacing the current PL state `InitMe`).
- State `FirstInstallation` in the domain context simplifies to just binding an already-existing `SocialEnvironment` into `PerspectivesSystem`.
- State `InitMe` in `SocialEnvironment` becomes redundant and can be removed.
- All six `X = sys:Me` roles can be changed to `X = me`.
- `TheWorld$Initializer` changes from `= sys:Me` to `= me`; since `me` is set before `TheWorld$Initializer` is used in PDR (with `withAuthoringRole`), the PDR must resolve `me` from the Purescript layer (i.e., by looking up `SocialEnvironment$Persons` directly) rather than from PL, or the PDR must continue to use `TheWorld$Initializer` directly by its stable type identifier during setup.

### Perspective adjustments required

If `SocialEnvironment$Me` and `SocialEnvironment$Persons` are created by the PDR in a transaction authored by `PerspectivesSystem$User`, the existing perspective:

```arc
user SystemUser = sys:Me   -- will become: = me
  perspective on Me
    only (Create, Fill)
  perspective on Persons
    only (Create, Fill)
```

should remain unchanged in structure, but the authoring role in the PDR code must be set to `SocialEnvironment$SystemUser` (via `withAuthoringRole`) when creating those roles, just as `TheWorld$Initializer` is used now for `PerspectivesUsers`.

### On the serialisation persona

The serialisation persona (`def:#serializationuser`) is a fictive `PerspectivesUsers` instance created in `TheWorld` with a fixed identifier. It is used by `serialisedAsDeltasForUserType` as a stand-in when serialising context data for a user *type* (not a specific instance):

```purescript
serialisedAsDeltasFor_ cid (RoleInstance "def:#serializationuser") userType
```

The persona is needed because serialisation deltas require a concrete role instance identifier to determine self-only perspectives. The code comment explicitly notes that the authoring role is "in effect ignored" in this path — only the user identifier matters for the self-only check.

**Could it be replaced by the owner's `PerspectivesUsers`?**  
Technically yes, if the intent is to treat the serialised view as if it were the owner's own view. However, using the owner's identifier carries a risk: if the owner's `PerspectivesUsers` role is not yet available when serialisation runs (e.g., on a fresh install), serialisation would fail. The fictive persona with a fixed, pre-known identifier sidesteps this. If the serialisation persona is retained, the perspective gap noted above (no `SetPropertyValue` for `LastName` on `Initializer`) needs to be resolved either by:
1. Adding `props (Identifiable$LastName) verbs (SetPropertyValue)` to `TheWorld$Initializer`'s perspective on `PerspectivesUsers`, or
2. Moving the persona creation to a different authoring role that already has that permission.
