# Context Serialisation for a Peer

This document describes the process by which a Perspectives context instance is serialised as a set of deltas, so that a peer user can reconstruct it locally. This process is central to the Invitation mechanism and to any situation where one installation must hand a complete representation of a context to another installation that does not yet have it.

> **Source files:** `packages/perspectives-core/src/core/instancePersistance/serialiseAsJson.purs`, `packages/perspectives-core/src/core/assignment/serialiseAsDeltas.purs`

---

## Background

In the normal synchronisation flow, every modification to an instance (a context or role) produces a signed *delta* that is distributed to all peers who need to know about the change. Each peer maintains its own local copy of the instances it has access to, kept up to date by processing incoming deltas.

But when a user *creates a new connection* (for example via an Invitation), the recipient does not yet have any of the relevant instances. There is no shared history of deltas. The solution is to reconstruct, on demand, the full set of deltas that would have built the target context from scratch, and to send that set as a single package called a `TransactionForPeer`.

---

## Communicated context of an elementary change

This section inventories the **current implementation**, not a proposed
minimal-disclosure algorithm. An elementary change's own deltas describe the
changed property, binding, role membership/existence, or context existence.
Its *communicated context* is the additional stored deltas selected to let a
recipient reconstruct the supporting structures and values. It need not be
confined to the context containing the changed instance.

### Common inclusion rules

The following vocabulary applies to the cases below:

| Rule | Included information |
|---|---|
| **Structural support for a role** | The deltas at the resource keys of the role, its containing context, and that context's external role. These normally establish existence and context membership. No unrelated roles or properties are included merely because they belong to that context. |
| **Query-path support** | Structural support for private roles on the query's main and supporting dependency paths; stored binding deltas for adjacent roles that are actually bound; stored property deltas where a value dependency is followed by its property-bearing role. Supporting paths can include filter criteria and calculated-property dependencies. |
| **Perspective-property support** | Query-path support for the permitted properties of the object roles, including the filler chain needed to reach the role that locally represents a property. Calculated properties communicate their underlying dependencies, not a newly authored delta for the calculated result. |
| **Perspective closure** | Query-path and property support for every non-`authorOnly` perspective of the receiving user type, including aspects, evaluated from the specified context. This is a perspective-selected representation, not a dump of every context member. |

These are selections from the **DeltaStore**, not synthetic snapshots.
`getDeltasForResource` returns all stored versions at an exact resource key.
In particular, selecting a property value path retrieves the whole stored
history for that role/property key: the `val` argument to `addPropertyDelta`
does not filter the returned deltas. Likewise, structural and binding support
can include historical changes, not just creation or the latest binding.
Sub-resource histories are not included by a structural lookup alone.

Normal inverted-query support checks perspective states: a context state
sanctions users in that context; a subject state sanctions the matching user
instances; an object state sanctions the matching object instances.
`computeProperties` separately considers the states granting each property.
Its undefined-state branch only serialises the existing object paths, however,
and does not evaluate the property calculation. Full perspective-closure
serialisation uses `propertiesInPerspective` and does **not** perform this
same per-instance state check; it must not be described as a state-filtered
minimal snapshot.

`authorOnly` perspectives are skipped by full serialisation (and are not
inverted for ordinary synchronisation); `authorOnly` properties are skipped
by `getPropertyValues`/`computeProperties`. A `selfOnly` perspective routes
each path and its properties to the user role at that path's head.
A `selfOnly` property similarly routes property support to its object role
alone. These restrictions apply to perspective/property support, not as a
blanket post-filter on every structural delta selected elsewhere.

Dependency serialisation skips `pub:` roles and binding/property pairs
involving them: public resources are fetched from their public endpoint.
The specialised assumption path has its own handling rather than that general
public-resource filter.

Sources: [dependency serialisation](../src/core/assignment/serialiseAsDeltas.purs)
(`serialiseDependency`, lines 337–422; `getPropertyValues`, 295–320);
[stored history](../src/core/persistence/deltaStore.purs)
(`getDeltasForResource`, 271–292);
[state-sensitive collection](../src/core/sync/collectAffectedContexts.purs)
(`runForwardsComputation`, 690–759; `computeProperties`, 765–828).

### Trigger catalogue

| Elementary change / circumstance | Conditions for extra communication | Declarative inclusion rule |
|---|---|---|
| **Create/add a role instance to a context** | An active perspective query uses that role, either as its object or along the path to its object. `addRoleInstanceToContext` enables forward collection. | Include support for the continuation from the new role to the perspective objects and their permitted properties. If the role itself is the object, include structural support even if its properties have no values. The role's `ConstructEmptyRole` and membership deltas describe the elementary creation itself, rather than extra context. |
| **First fill of a role: binding/binder navigation** | A perspective uses the binding or reverse-binding relation, also through roles recursively filled by the changed role and its filler chain. | Include support for the resulting perspective-query continuations and their properties. This can introduce roles in other contexts, not only the filled role's context. |
| **First fill: the filler must be constructible** | The selected actual filler is private; recipients are those found by the binding-query computation. No property value on the filler is required. | Include structural support for that filler and its containing context/external role, independently of property-path support. Public fillers are excluded from this additional lookup. |
| **First fill: an enumerated user becomes a peer** | The filled type has kind `UserRole`, the instance is not `me`, and it is not the fictive `def:#serializationuser`. | Include the perspective closure of that user type from the filled role's context. If the type has no modelled self-perspective, try to add a minimal self-perspective so the receiver obtains its own role. Self-perspectives additionally request `PublicKey` and `TransportPublicKey`, bringing the identity filler telescope into the dependency paths. |
| **First fill: properties acquired through a filler** | The filled role, or a role it recursively fills, is itself a perspective object. | Include permitted property support for those objects, even when the perspective ends at the filled role and therefore has no forward binding step. This accounts for properties already present on a newly attached filler. |
| **Binding change exposes a calculated user** | A stored calculated-user detection query has both directions compiled, yields context(s) and user instance(s), and the user is not `me`. Filter-based queries must yield a matching candidate. | Include the context/external-role resource histories and the perspective closure for each context/user combination, using the **calculated** user type. This call uses `noNewPeer`: it does not add the fallback minimal self-perspective. |
| **Replace a filler** | The resolved replacement differs from the existing filler. Replacement is deferred; its detection and later installation have distinct collection paths. | Binding-query detection can include continuation/property support; when the new filler is installed, `changeRoleBinding` applies enumerated-new-peer and perspective-object property support. Unlike first fill, it has no independent structural lookup for the replacement filler. Old-filler `ReplaceBinding` queries add recipients without forward support. |
| **Remove a filler** | The role has a filler. `removeBinding_` performs binding-query collection with forward collection enabled while the old binding still exists. | Include support for the paths and properties still found through that binding, before it is severed. This can resend the information being disconnected; removal does not imply that extra context is suppressed. There is no new-filler lookup or enumerated-new-peer serialisation for a pure unbinding. |
| **Set/add/remove/delete a property: ordinary perspective** | The actual property-bearing role or a role it recursively fills is a perspective object with access to the original or replacing property. | Include support for that property's calculation, subject to its property-state and privacy rules. This may include bindings, fillers, structural deltas and prior deltas for the changed property, but not all unrelated perspective properties. |
| **Property change makes a filtered perspective object visible** | An applicable object query starts with a filter, yields active perspective holders, and does not take the earlier `selfOnly` perspective/property branch. The changed property can be only the filter criterion, not a displayed property. | Include structural support for the matching object and support for **all** properties in that perspective, rather than only the changed property. |
| **Property change exposes a filter-based calculated user** | Property-query detection identifies a calculated-user query whose forward part starts with a filter and yields non-local users. | Include the same context/external-role histories and calculated-user perspective closure as for binding-based calculated-user detection. |
| **Create a context with a public user proxy** | Context construction creates public-role proxy instances for a non-public context. | In addition to the context's own creation deltas, include the perspective closure for each proxy, for publication. This uses `noNewPeer`, so no fallback self-perspective is added. Initial roles, bindings and properties also invoke their own mutation paths. |
| **Any elementary change causes entry into a visibility-granting state** | Transaction state evaluation enters a state with a compiled perspective-on-entry and eligible users; `authorOnly` entries are excluded. This is an indirect consequence, not necessarily immediate mutation-time collection. | Context entry supplies the newly granted object/property support to eligible users; subject-role entry supplies the perspective objects/properties to that subject instance only; object-role entry supplies support for that entering object instance only, to eligible perspective holders. |
| **Delete a role / remove all instances of a role** | Synchronised assignment removal schedules each selected role and computes recipients with forward collection disabled. The lower-level `SaveUserData.removeAllRoleInstances` instead directly removes each instance without collecting outgoing deltas. | No normal continuation/property expansion is requested. Nevertheless, specialised self-only collection and calculated-user binding detection in synchronised removal do not obey the forward flag and can still select extra support; see the qualification below. |
| **Delete a context** | Context removal sends `RemoveExternalRoleInstance` to its user roles and the supplied holders of perspectives on its embedding role. Its roles are subsequently removed. | No direct perspective-closure or structural snapshot is added by context-removal scheduling. Member-removal special paths and state/action consequences may independently add deltas. |

Sources for the catalogue:

- [Assignment.Update](../src/core/assignment/update.purs):
  `addRoleInstanceToContext` (130–205), `addProperty` (270–327),
  `removeProperty` (366–404), `deleteProperty` (415–456),
  `setProperty` (468–526).
- [SaveUserData](../src/core/instancePersistance/saveUserData.purs):
  `scheduleContextRemoval` (167–219),
  `statesAndPeersForRoleInstanceToRemove` (329–344),
  `replaceBindingWithMode` (469–488),
  `setFirstBindingWithMode` (524–628), `handleNewPeer` (644–651),
  `removeBinding_` (669–737), `changeRoleBinding` (750–785).
- [CollectAffectedContexts](../src/core/sync/collectAffectedContexts.purs):
  `usersWithPerspectiveOnRoleInstance` (85–200),
  `usersWithPerspectiveOnRoleBinding'` (420–568),
  `handleNewCalculatedUsersForBinding` (589–615),
  `addDeltasForPerspectiveObjects` (621–657),
  `aisInPropertyDelta` (928–976),
  `addDeltasForPropertyChange` (985–1070).
- [Instance builders](../src/core/instances/instanceBuilders.purs):
  context construction's proxy serialisation (134–155).
- [Assignment compiler](../src/core/assignment/compileAssignment.purs):
  selected-role removal scheduling (86–90);
  `SaveUserData.removeAllRoleInstances` (433–436) is the direct-removal helper.
- [ContextStateCompiler](../src/core/assignment/contextStateCompiler.purs)
  (218–235) and [RoleStateCompiler](../src/core/assignment/roleStateCompiler.purs)
  (236–272): perspective-on-entry serialisation.

### Special paths and timing qualifications

**Self-only query support.** Role-instance detection communicates only the
matching user instance's calculated paths and permitted properties. Binding
detection additionally communicates the assumptions used on its backward
path to that peer: selected role sets with structural support, fillers and
binding histories, reverse-bound role sets and their binding histories,
property-bearing roles and property histories, and context support.
`Me`, external-role assumptions, and context/role state assumptions add no
separate deltas. See `createDeltasFromAssumption` (838–903).
Consequently, the comment that role-removal recipient detection “adds no
deltas” is not an unconditional implementation guarantee: self-only handlers
and calculated-user detection bypass the `runForwards` flag.

**No before/after visibility proof.** “New peer” and “new calculated user”
name collection paths, not an established difference between old and new
visible sets. `handleNewPeer` does not check for another role of the same peer
in the context. Calculated-user detection serialises every non-local result
it finds, without subtracting prior results. Filtered-object expansion checks
the result at collection time, not whether that result was previously
invisible. Removal/replacement detection can also run these paths.

**Which values are visible at collection time?** Property set/add saves the
new values before computing peers; remove/delete computes peers and support
before deleting the old values. Therefore remove/delete can serialise old
support rather than the post-removal visible set. Calculated-property
inverted queries in `aisInPropertyDelta` add recipients but do not immediately
serialise the whole calculated-property perspective; its new change delta is
added after recipient computation. State transitions and automatic actions
can produce further support later in the transaction.

The first-fill binding delta is stored before enumerated-new-peer
serialisation, allowing that serialisation to retrieve it. Earlier
binding-query collection in the same function occurs before this storage;
the phases must not be treated as one atomic snapshot.

### What redundancy is actually avoided?

- Setting an unchanged property set, filling with the same resolved filler,
  and adding an already present linked role skip the mutation path.
- Dependency serialisation remembers roles within one `serialiseDependencies`
  invocation; the memory is not shared across all paths or recipients.
- `addDelta` filters out `me`, suppresses outgoing collection while executing
  incoming deltas, and rejects an equal `DeltaInTransaction` already in the
  transaction (same signed delta and same filtered recipient array). Different
  recipient arrays are not merged by that test.
- Distribution also deduplicates signed deltas within each destination's
  `TransactionForPeer`; this removes overlap in one outgoing transaction,
  not repeats across transactions.
- Role-removal deltas are suppressed when the containing context is being
  removed; binding-removal deltas are suppressed when the role is being
  removed. These guards avoid redundant **change** deltas, not a general
  communicated-context expansion.

There is **no general sender-side “the peer already knows this context”
test** in these collection rules. In the motivating example, an existing role
of the same peer in the newly attached context does not by itself suppress
enumerated-new-peer or calculated-user serialisation. Proving and subtracting
previous knowledge would be an additional optimisation, not current behavior.
Nor does history replay deduplication prove that only the minimum necessary
resource versions were sent.

Sources: [Deltas](../src/core/sync/deltas.purs), `addDelta` (295–320);
[TransactionForPeer](../src/core/sync/transactionForPeer.purs),
`addToTransactionForPeer`; [SaveUserData](../src/core/instancePersistance/saveUserData.purs),
`synchroniseRoleRemoval` (348–373), `removeBinding_` (705–735).

Local query correlation identifiers and state-evaluation records are not
extra peer deltas. Deletion cleanup and automatic actions are separate
elementary changes, whose deltas and support should be accounted for
separately. Public-key information attached to `TransactionForPeer` is
verification metadata, not itself communicated context in the delta sense.
Explicit `ser:SerialiseFor` and API serialisation requests also use the
serializer, but are not automatic support around an elementary mutation.

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

---

## Design Consideration: State-Dependent Perspectives and the Return-Transaction Risk

### Background

`serialiseRoleInstancesAndProperties` is not only called via `ser:SerialiseFor`. It is also called inside `enteringRoleState` (module `Perspectives.RoleStateCompiler`) when a role instance enters a state that is decorated with `perspectivesOnEntry` entries. The rationale is: if a peer user gains a new (or enriched) perspective because a role enters a state, the installation that drives the transition must ensure the peer receives the corresponding resources as deltas.

### The asymmetry in `enteringRoleState`

`enteringRoleState` contains two distinct loops over on-entry entries:

1. **Automatic actions** (`automaticOnEntry`): guarded by `whenRightUser`, which checks `isMe` on the set of actors that fill the `allowedUser` role. If the local end user does not fill that role, the automatic action is **silently skipped**.

2. **Perspective synchronisation** (`perspectivesOnEntry`): **not** guarded by `whenRightUser`. It iterates over all instances of the `allowedUser` role that are *not* the local user (`COMB.not' (filledBy …me…)`), and calls `serialiseRoleInstancesAndProperties` for each such peer. This step runs regardless of who authored the incoming transaction.

### What happens when the triggering transaction came from a peer

Incoming peer transactions are executed with `share = false` (see `transactionConsumer` in `Perspectives.AMQP.IncomingPost`):

```purescript
runMonadPerspectivesTransaction'
  false
  (ENR $ EnumeratedRoleType sysUser)
  (executeTransaction body)
```

However, state transitions triggered by the deltas in that transaction are evaluated inside `runSharing`, which spawns an embedded **sharing** transaction (`share = true`). Concretely:

- Phase-1 steps 1.3, 1.4, 1.5, 1.6 and phase-2 step 2.1 all call `runSharing` before executing state entry/exit handlers.
- Any deltas added to the transaction inside `runSharing` (including those produced by `serialiseRoleInstancesAndProperties` in `enteringRoleState`) are therefore in a **sharing** transaction and will be distributed to peers.

### The resulting "return transaction"

When the receiver's installation processes a peer transaction that causes a role to enter a state with `perspectivesOnEntry` entries, `serialiseRoleInstancesAndProperties` is called for those peers — which may include the **original sender**. The receiver thereby generates and sends back to the sender a `TransactionForPeer` containing deltas that represent the sender's perspective on the resources in question.

Whether this return transaction is informative depends on whether the receiver holds resources the sender does not yet know about. In typical cases the sender already has all those resources (it created them), so the return transaction is **redundant**. In edge cases it may carry genuinely new information (e.g. a filler created locally by the receiver that is part of the perspective's object query).

### What to remember

> **Important:** When an incoming peer transaction causes a state transition, **automatic actions** specified for that state are only executed if the local user fills the `allowedUser` role (guarded by `whenRightUser`). The sender's automatic actions are **never** executed on behalf of the sender. However, **perspective-synchronisation deltas** (`perspectivesOnEntry`) are generated unconditionally for every non-local peer, including potentially the sender, and these are distributed in an embedded sharing transaction.
>
> This means: processing an incoming transaction can silently produce a return transaction back to the original sender, even though no automatic actions were executed for that sender. This interaction is easy to overlook. Whenever the mechanism of state-dependent perspectives is changed or extended, care must be taken to re-examine whether the return transaction could carry stale, duplicate, or conflicting information, or whether it could trigger further state transitions on the sender's side that create an unintended loop.
