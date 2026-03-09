# Context Serialisation for a Peer

This document describes the process by which a Perspectives context instance is serialised as a set of deltas, so that a peer user can reconstruct it locally. This process is central to the Invitation mechanism and to any situation where one installation must hand a complete representation of a context to another installation that does not yet have it.

> **Source files:** `packages/perspectives-core/src/core/instancePersistance/serialiseAsJson.purs`, `packages/perspectives-core/src/core/assignment/serialiseAsDeltas.purs`

---

## Background

In the normal synchronisation flow, every modification to an instance (a context or role) produces a signed *delta* that is distributed to all peers who need to know about the change. Each peer maintains its own local copy of the instances it has access to, kept up to date by processing incoming deltas.

But when a user *creates a new connection* (for example via an Invitation), the recipient does not yet have any of the relevant instances. There is no shared history of deltas. The solution is to reconstruct, on demand, the full set of deltas that would have built the target context from scratch, and to send that set as a single package called a `TransactionForPeer`.

---

## Entry Point: `ser:SerialiseFor`

In a Perspectives model the serialisation is triggered by the external function call:

```arc
callExternal ser:SerialiseFor( <userRoleType> ) returns String
```

This function is defined in `module Perspectives.Instances.SerialiseAsJson` as `serialiseFor`:

```purescript
serialiseFor :: Array RoleType -> RoleInstance -> MPQ Value
serialiseFor userTypes externalRoleId =
  ArrayT $ case ARR.head userTypes of
    Nothing -> pure []
    Just u  -> ARR.singleton <$>
      serialisedAsDeltasForUserType
        (ContextInstance $ deconstructBuitenRol $ unwrap externalRoleId)
        u
```

The function receives:
- **`userTypes`** — a singleton array containing the *RoleType* of the user for whom to serialise (typically the Invitee role type).
- **`externalRoleId`** — the external role of the context to be serialised (e.g., the external role of the Invitation context instance).

It delegates immediately to `serialisedAsDeltasForUserType`.

---

## `serialisedAsDeltasForUserType`

```purescript
serialisedAsDeltasForUserType :: ContextInstance -> RoleType -> MonadPerspectives Value
```

This is the top-level serialisation function. Its responsibilities are:

1. **Create a fresh transaction environment** (using `execMonadPerspectivesTransaction`). The authoring role is set to `sysUser` as a placeholder; it is irrelevant because this function only *reads* pre-stored deltas rather than constructing new ones.

2. **Run the delta-collection pass** by calling `serialisedAsDeltasFor_` with:
   - the context instance,
   - a *fictive user identifier* `RoleInstance "def:#serializationuser"` — because the real user id is unknown when serialising for a type, and
   - the user's RoleType.

3. **Augment the transaction with public keys** (`addPublicKeysToTransaction`). After all deltas have been collected, the function scans every delta for its author (a `PerspectivesUser` identifier) and attaches the corresponding public-key information. The recipient needs these keys to verify the signatures.

4. **Wrap the result** in a `TransactionForPeer` record:

   ```purescript
   TransactionForPeer
     { author          :: PerspectivesUser       -- serialiser's identity
     , perspectivesSystem :: ContextInstance     -- serialiser's PerspectivesSystem id
     , timeStamp       :: SerializableDateTime
     , deltas          :: Array SignedDelta       -- all collected deltas
     , publicKeys      :: EncodableMap PerspectivesUser PublicKeyInfo
     }
   ```

5. **Serialise to JSON** with `unsafeStringify (write tfp)` and return it as a `Value`.

### The fictive serialisation user

During `initSystem` the PDR creates a special `PerspectivesUsers` instance:

```
id: "def:#serializationuser"
LastName: "serializationuser"
```

This instance exists solely so that `serialisedAsDeltasFor_` can be given a concrete `RoleInstance` to work with (it needs one to determine whether a perspective is a *self-perspective*). For the `selfOnly` calculation the actual head of the dependency path is used instead of the fictive user, so the privacy guarantee is not compromised (see below).

---

## `serialisedAsDeltasFor_`

```purescript
serialisedAsDeltasFor_ :: ContextInstance -> RoleInstance -> RoleType
                        -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor_ cid userId userType =
  liftToMPT (userType ###= perspectivesClosure_)
    >>= traverse_ (serialisePerspectiveForUser cid (NA.singleton userId) userType)
```

`perspectivesClosure_` returns every `Perspective` defined for the user's role type, *including perspectives inherited from role aspects*. The function then processes each perspective in turn.

---

## `serialisePerspectiveForUser`

```purescript
serialisePerspectiveForUser
  :: ContextInstance
  -> NA.NonEmptyArray RoleInstance   -- user role instances
  -> RoleType
  -> Perspective
  -> MonadPerspectivesTransaction Unit
serialisePerspectiveForUser cid users userRoleType
    p@(Perspective { object, propertyVerbs, selfOnly, authorOnly, isSelfPerspective }) =
  if authorOnly then pure unit
  else do
    visiblePropertyTypes <- liftToMPT $ propertiesInPerspective p
    serialiseRoleInstancesAndProperties cid users object
      (nub visiblePropertyTypes) selfOnly isSelfPerspective
```

### `authorOnly` on a perspective

If a perspective is marked **`authorOnly`** (`authoronly` in the ARC language), it means: *these data are private to the author and must never be shared with a peer*. The function returns immediately without emitting any deltas. No role instances and no properties from this perspective are included in the serialised package.

---

## `serialiseRoleInstancesAndProperties`

This is the core of the serialisation. It handles two special cases (`selfOnly` perspective and `selfOnly` properties) and the default case.

### Step 1 — Handle self-perspective augmentation

```purescript
properties' <-
  if isPerspectiveOnSelf
  then pure $ ARR.cons (ENP $ EnumeratedPropertyType perspectivesUsersPublicKey) properties
  else pure properties
```

When the perspective is a *self-perspective* (the user has a perspective on their own role), the function prepends the `PublicKey` property to the list of properties to be serialised. This is a safety measure: the receiving user obviously already knows their own public key, but the delta must be present so that the full role telescope (including the `PerspectivesUsers` filler with the key) is transmitted.

### Step 2 — Find role instances

```purescript
(rinstances :: Array DependencyPath) <-
  liftToMPT ((singletonPath (C cid)) ##= interpret object)
```

The perspective's `object` is a `QueryFunctionDescription`. Executing it from the context instance yields the role instances the user is entitled to see, each wrapped in a `DependencyPath` that records the full chain of resources traversed.

### Step 3a — `selfOnly` perspective

A perspective marked **`selfOnly`** (`selfonly` in ARC) means: *each user may only receive data about their own role instance, not about the instances belonging to other users.* The constraint is that the object of such a perspective must be a user role and the perspective must be *of* that same user role.

```purescript
if selfOnly then do
  for_ (join (allPaths <$> rinstances))
    \(dependencies :: NonEmptyList Dependency) -> do
      -- The head of each dependency path is one user's role instance.
      oneUserOnly <- unsafePartial case head dependencies of
        R r -> pure [ r ]
      serialiseDependencies oneUserOnly dependencies
      for_ properties' \pt -> do
        vals <- liftToMPT ((singletonPath (R $ fromJust $ ARR.head oneUserOnly)) ##= getPropertyValues pt)
        for_ (join (allPaths <$> vals)) (serialiseDependencies oneUserOnly)
```

For each role instance path the head of the path is the actual user role instance. The instance is used *both* as the resource to serialise *and* as the sole recipient of those deltas. No user ever receives another user's data from a `selfOnly` perspective.

Properties are computed starting from that single user's role instance, and the resulting deltas are likewise delivered only to that user.

### Step 3b — Normal perspective (possibly with `selfOnly` properties)

```purescript
else do
  for_ (join (allPaths <$> rinstances)) (serialiseDependencies (toArray users))
  for_ properties' \pt -> do
    isSelfOnlyProperty <- lift $ propertyTypeIsSelfOnly pt
    for_ (_.head <$> rinstances) \(dep :: Dependency) -> do
      vals <- liftToMPT ((singletonPath dep) ##= getPropertyValues pt)
      if isSelfOnlyProperty
      then for_ (join (allPaths <$> vals))
             (serialiseDependencies (unsafePartial case dep of R r -> [r]))
      else for_ (join (allPaths <$> vals)) (serialiseDependencies (toArray users))
```

For a normal perspective all role instances are serialised for all users with that perspective.

However, a property on an enumerated role can itself be marked **`selfOnly`** (`selfonly` on the property). When this is the case, the property delta is delivered only to the single user who owns the role instance on which the property lives — not to every user who has the containing perspective. This is a finer-grained privacy mechanism than the perspective-level `selfOnly`.

---

## `getPropertyValues` — `authorOnly` on a property

```purescript
getPropertyValues :: PropertyType -> DependencyPath ~~> DependencyPath
getPropertyValues pt dep = (lift $ lift $ propertyTypeIsAuthorOnly pt) >>=
  if _ then ArrayT $ pure []
  else do
    calc <- lift $ lift $ (PClass.getProperty >=> PClass.getCalculation) pt
    pathToRoleWithProperty <- unsafePartial computePathToFillerWithProperty dep
    interpret calc pathToRoleWithProperty
```

A property marked **`authorOnly`** (`authoronly` on the property) is never included in serialisation: the function returns an empty array so no delta is ever emitted for it.

For all other properties the function proceeds to compute the value, first locating the filler in the role's filler chain that actually *stores* the property.

---

## Recursive Filler Traversal

### `computePathToFillerWithProperty`

```purescript
computePathToFillerWithProperty :: Partial => DependencyPath ~~> DependencyPath
computePathToFillerWithProperty path@{ head } = ArrayT case head of
  R rid -> do
    localProps <- lift $ (roleType_ >=> allLocallyRepresentedProperties <<< ST) rid
    if isJust $ elemIndex pt localProps
    then pure [ path ]
    else do
      mfiller <- lift $ binding_ rid
      case mfiller of
        Nothing -> pure []
        Just b  -> runArrayT $ computePathToFillerWithProperty
                     (consOnMainPath (R b) path)
```

When a property is not locally stored on the role instance itself, the function walks up the *filler chain* (via `binding_`), accumulating each intermediate filler into the `DependencyPath`. The recursion stops when:

- a filler whose type locally represents the property is found, or
- there is no further filler.

**Why this matters for user roles:** User roles in Perspectives are typically filled by a `Persons` role, which in turn is filled by a `PerspectivesUsers` role (in `TheWorld`). The `PerspectivesUsers` role holds the `PublicKey` property. To ensure the receiving peer can verify signatures, the serialisation must include the full filler chain all the way up to `PerspectivesUsers`. This traversal does exactly that: every binding in the chain is represented as a `RoleBindingDelta` inside the `DependencyPath`.

---

## `serialiseDependencies` — Turning Paths into Deltas

A `DependencyPath` records a chain of `Dependency` values. `serialiseDependencies` iterates over that chain pairwise, producing the appropriate delta for each consecutive pair:

| Pair | Action |
|------|--------|
| `R roleId` (any position) | `addDeltasForRole roleId` |
| `R roleId1` → `R roleId2` | Add the `RoleBindingDelta` linking them |
| `V propertyType val` → `R roleId` | Add the `RolePropertyDelta` for that value |

Duplicate deltas are silently dropped by `addDelta`: before appending, it checks whether an equal `DeltaInTransaction` (same `SignedDelta` and same `users` array) is already present in the transaction. This is the primary deduplication mechanism when multiple dependency paths touch the same role instance.

### `addDeltasForRole`

Adding a role instance to the transaction requires five deltas in strict order:

1. **External role's `UniverseRoleDelta`** (`ConstructExternalRole`) — declares the existence of the external role in the universe.
2. **Context's `UniverseContextDelta`** (`ConstructEmptyContext`) — declares the existence of the context.
3. **External role's `ContextDelta`** (`AddExternalRole`) — links the external role into the context.
4. **Role instance's `UniverseRoleDelta`** (`ConstructEmptyRole`) — declares the existence of the role instance.
5. **Role instance's `ContextDelta`** (`AddRoleInstancesToContext`) — places the role instance in the context.

The order is essential: the receiving peer executes these deltas sequentially, and each step depends on the previous one.

These five deltas are *pre-computed* and stored directly on the `PerspectContext` (for the context's `UniverseContextDelta`) and on each `PerspectRol` (for its own `UniverseRoleDelta` and `ContextDelta`) at the time the instance was originally constructed. The serialisation function merely *reads* and re-uses these stored deltas; it does not sign anything itself.

Similarly, binding deltas are stored on `PerspectRol` as `bindingDelta`, and property deltas are indexed by `(propertyType, value)` in `propertyDeltas`.

---

## DependencyPath — Data Structure

```purescript
type DependencyPath =
  { head :: Dependency
  , mainPath :: Maybe (NonEmptyList Dependency)
  , supportingPaths :: Array (NonEmptyList Dependency)
  }

data Dependency
  = C ContextInstance
  | R RoleInstance
  | V String Value      -- String is the property type identifier
  | CT ContextType
  | RT RoleType
  | AnyRoleTypeDependency
```

- **`head`** is the result of a query step.
- **`mainPath`** is the ordered chain from `head` back to the query source. The leftmost element is the most recently computed result; the rightmost is the original starting point.
- **`supportingPaths`** are side-chains used during the computation (e.g., paths for filter conditions) that are not part of the main chain but still reference resources that must be serialised.

`allPaths` returns both `mainPath` and all `supportingPaths` as a flat array of `NonEmptyList Dependency`, which is then iterated by `serialiseDependencies`.

---

## Summary of Filtering Rules

| Modifier | Location | Effect on serialisation |
|----------|----------|------------------------|
| `authorOnly` | Perspective | Entire perspective is skipped; no deltas emitted. |
| `selfOnly` | Perspective | Each user receives only deltas for their *own* role instance; other users' instances are invisible to them. |
| `authorOnly` | Property | Property is never serialised; `getPropertyValues` returns empty. |
| `selfOnly` | Property | Property delta is sent only to the specific role instance owner, not to all users with the perspective. |

---

## Worked Example: Serialising an Invitation

### Model fragment (from `perspectivesSysteem@6.3.arc`)

```arc
case Invitation
  ...
  external
    state CreateInvitation = exists ConfirmationCode
      on entry
        do for Inviter
          letA
            transaction <- callExternal ser:SerialiseFor(
              ((filter origin >> context >> contextType >> roleTypes
                with specialisesRoleType model://perspectives.domains#System$Invitation$Invitee)
              orElse [role model://perspectives.domains#System$Invitation$Invitee])
            ) returns String
            ...

  user Inviter (mandatory) filledBy (Persons + PerspectivesUsers)
    perspective on Invitee
      props (FirstName, LastName, HasKey) verbs (Consult)
    perspective on External
      props (Message, ConfirmationCode, SerialisedInvitation) verbs (SetPropertyValue, Consult)
      ...

  user Invitee (mandatory) filledBy (Persons + PerspectivesUsers)
    perspective on Inviter
      props (FirstName, LastName, HasKey) verbs (Consult)
    perspective on External
      props (InviterLastName, Message, ConfirmationCode, Addressing) verbs (Consult)
      ...
```

### What happens when state `CreateInvitation` is entered

1. The ARC runtime evaluates the query:
   ```arc
   filter origin >> context >> contextType >> roleTypes
     with specialisesRoleType model://…#System$Invitation$Invitee
   ```
   to determine the concrete Invitee role type to use. If no specialisation exists, it falls back to the base `Invitee` type.

2. `ser:SerialiseFor` is called with that type and the external role of the current Invitation context instance.

3. `serialisedAsDeltasForUserType` opens a fresh transaction environment and runs `serialisedAsDeltasFor_` for the Invitee type.

4. `perspectivesClosure_` returns the two perspectives defined for `Invitee`: one on `Inviter` (properties: FirstName, LastName, HasKey) and one on `External` (properties: InviterLastName, Message, ConfirmationCode, Addressing).

5. For the **perspective on Inviter**:
   - There is at most one Inviter instance. The query for the object is executed; it yields the Inviter role instance plus the dependency path back to the Invitation context.
   - `serialiseDependencies` processes the dependency path. When it encounters the Inviter role dependency it calls `addDeltasForRole(inviterRoleInstance)`, which emits **5 deltas** in order:
     1. Invitation external role's `UniverseRoleDelta` (ConstructExternalRole)
     2. Invitation context's `UniverseContextDelta` (ConstructEmptyContext)
     3. Invitation external role's `ContextDelta` (AddExternalRole)
     4. Inviter's `UniverseRoleDelta` (ConstructEmptyRole)
     5. Inviter's `ContextDelta` (AddRoleInstancesToContext)
   - The filler chain of the Inviter is: Inviter → `SocialEnvironment$Persons` → `TheWorld$PerspectivesUsers`. The `Persons` role has no locally defined properties and no aspects; the `PerspectivesUsers` role has the `Identifiable` aspect which provides `FirstName`, `LastName`, and `PublicKey`, plus its own `HasKey`.
   - For property `FirstName` (and likewise `LastName`): `getPropertyValues` uses `computePathToFillerWithProperty` which walks the filler chain. Inviter does not locally represent `FirstName`; Persons does not either; `PerspectivesUsers` does (via the `Identifiable` aspect). The function stops there. Along the way the dependency path now includes the Persons filler and the PerspectivesUsers filler. `serialiseDependencies` processes this path, calling `addDeltasForRole` for Persons (5 more deltas for the Persons role and its containing SocialEnvironment context) and for PerspectivesUsers (5 more deltas for PerspectivesUsers and its containing TheWorld context). `RoleBindingDelta`s are added for Inviter→Persons and Persons→PerspectivesUsers. Finally, the `RolePropertyDelta` for `FirstName` is added.
   - For property `HasKey`: also stored on `PerspectivesUsers`. The same chain is traversed; the structural deltas and binding deltas for Persons and PerspectivesUsers are not emitted again because `addDelta` silently drops equal `DeltaInTransaction` entries that are already in the transaction. Only the `RolePropertyDelta` for `HasKey` is new.
   - As a result, the full filler chain up to `PerspectivesUsers` is included in the serialisation. Because `PerspectivesUsers` also carries `PublicKey` (via `Identifiable`), and because `computePathToFillerWithProperty` walks all the way up to find `FirstName` (which is also on `PerspectivesUsers`), the `PerspectivesUsers` instance — and therefore the public key — is always present in the serialised package.

6. For the **perspective on External**:
   - The query returns the external role instance (already serialised in step 5, so the "seen before" check skips the structural deltas).
   - Property deltas are emitted for `InviterLastName`, `Message`, `ConfirmationCode`, and `Addressing`.
   - `InviterLastName` is a *calculated* property (`context >> Inviter >> LastName`). Its delta was computed when the property was first set and is stored in `propertyDeltas` on the external role.

7. `addPublicKeysToTransaction` collects the public key info for every `PerspectivesUser` referenced as a delta author and adds it to `publicKeys`.

8. The resulting `TransactionForPeer` is serialised to JSON and stored in the `SerialisedInvitation` file property on the external role.

### What the recipient does with it

When the Invitee opens the invitation file, the system calls `executeTransactionForPeer` (in `handleTransaction.purs`). This function:

1. Verifies the public key information attached to the transaction.
2. Verifies the signature on every delta.
3. Expands the resource identifiers (adding the sender's storage URL).
4. Executes each delta in order:
   - `UniverseContextDelta` → creates the Invitation context locally.
   - `UniverseRoleDelta` / `ContextDelta` → creates and places each role instance.
   - `RoleBindingDelta` → links fillers.
   - `RolePropertyDelta` → sets property values.

After processing, the Invitee's local installation has a complete, verified replica of the Invitation context.

---

## Relation to `PendingInvitations`

`PerspectivesSystem` contains a calculated role:

```arc
context PendingInvitations = callExternal cdb:PendingInvitations() returns sys:Invitation$External
```

This role is filled by external roles of `Invitation` contexts that were created by other users and stored in the shared post database. The Invitee discovers such an invitation by polling `PendingInvitations`. When they open the invitation file stored in `SerialisedInvitation`, the above process is triggered to reconstruct the full Invitation context locally.
