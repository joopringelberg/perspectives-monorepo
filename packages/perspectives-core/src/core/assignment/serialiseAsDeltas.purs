-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Assignment.SerialiseAsDeltas
  ( getPropertyValues
  , newPeer
  , noNewPeer
  , perspectivePositionText
  , serialiseDependencies
  , serialiseRoleInstancesAndProperties
  , serialisedAsDeltasFor
  , serialisedAsDeltasForUserType
  , serialisedAsDeltasFor_
  ) where

import Control.Monad.AvarMonadAsk (get) as AMA
import Control.Monad.Error.Class (try)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (StateT, gets, runStateT, modify)
import Control.Monad.Trans.Class (lift)
import Effect.Aff.Class (liftAff)
import Data.Array (cons, head) as ARR
import Data.Array (elemIndex, filter, length, nub, null, snoc)
import Data.Array.NonEmpty (NonEmptyArray, singleton) as NA
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Foldable (for_, traverse_)
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList, foldM, head)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (new)
import Effect.Class.Console (log)
import Foreign.Object (empty)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), LogLevel(..), LogTopic(..), MonadPerspectives, MonadPerspectivesTransaction, (###=), (##=))
import Perspectives.Data.EncodableMap (empty) as EM
import Perspectives.Deltas (addDelta, addPublicKeysToTransaction)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinFile (defaultDomeinFileRecord)
import Perspectives.Error.Boundaries (handlePerspectContextError, handlePerspectRolError, handlePerspectRolError')
import Perspectives.Error.Pretty (humanizePerspectivesWarning)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Instances.ObjectGetters (binding_, contextType_, roleType_)
import Perspectives.Logging (debugSync, errorDelta, logWhen)
import Perspectives.ModelDependencies (perspectivesUsersPublicKey, sysUser)
import Perspectives.Names (getMySystem, getUserIdentifier)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwoState, runPhaseTwo_')
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (MultiplePerspectivesErrors)
import Perspectives.Persistence.DeltaStore (getDeltasForResource)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.PerspectivesState (getPerspectivesUser, transactionLevel)
import Perspectives.Query.ExpressionCompiler (makeRoleGetter)
import Perspectives.Query.Interpreter (interpret)
import Perspectives.Query.Interpreter.Dependencies (Dependency(..), DependencyPath, allPaths, consOnMainPath, singletonPath)
import Perspectives.Query.QueryTypes (Domain(..), QueryFunctionDescription)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Property (getProperty, getCalculation) as PClass
import Perspectives.Representation.Class.Property (propertyTypeIsAuthorOnly, propertyTypeIsSelfOnly)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), propertytype2string, roletype2string)
import Perspectives.ResourceIdentifiers (isInPublicScheme)
import Perspectives.Sidecar.ToReadable (toReadable)
import Perspectives.Sync.DeltaInTransaction (DeltaInTransaction(..))
import Perspectives.Sync.Transaction (Transaction(..), createTransaction)
import Perspectives.Sync.TransactionForPeer (TransactionForPeer(..))
import Perspectives.Types.ObjectGetters (isEnumeratedRoleType, perspectivesClosure_, propertiesInPerspective)
import Perspectives.Warning (PerspectivesWarning(..))
import Prelude (Unit, bind, discard, join, pure, show, unit, void, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=), (||), (<), (*>), (&&))
import Simple.JSON (unsafeStringify, write)

noNewPeer :: Boolean
noNewPeer = false

newPeer :: Boolean
newPeer = true

serialisedAsDeltasFor :: ContextInstance -> RoleInstance -> Boolean -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor cid userId isNewPeer = do
  userType <- lift $ roleType_ userId
  serialisedAsDeltasFor_ cid userId (ENR userType) isNewPeer

-- | Construct a Transaction that represents a context for a particular user role.
-- | Serialise the Transaction as a string.
serialisedAsDeltasForUserType :: ContextInstance -> RoleType -> MonadPerspectives Value
serialisedAsDeltasForUserType cid userType = do
  me <- getUserIdentifier
  (Transaction { timeStamp, deltas, publicKeys }) <-
    ( execMonadPerspectivesTransaction
        -- The authoringRole is used on *constructing* deltas. However, here we merely *read* deltas from the
        -- context- and role representations. So this value is in effect ignored.
        (ENR $ EnumeratedRoleType sysUser)
        -- NOTE: we provide serialisedAsDeltasFor_ with the fictive PerspectivesUser we created for this purpose, as
        -- the user for whom we serialise. As we don't know the real
        -- user identifier (we serialise for a type!) we use it as a stand in.
        (serialisedAsDeltasFor_ cid (RoleInstance "def:#serializationuser") userType noNewPeer)
    ) >>= addPublicKeysToTransaction
  author <- getPerspectivesUser
  perspectivesSystem <- ContextInstance <$> getMySystem
  tfp <- pure $ TransactionForPeer
    { author
    , perspectivesSystem
    , timeStamp
    , deltas: _.delta <<< unwrap <$> deltas
    , publicKeys
    }
  pure $ Value $ unsafeStringify $ write tfp
  where
  -- | Execute a value in MonadPerspectivesTransaction, discard the result and return the transaction.
  execMonadPerspectivesTransaction
    :: forall o
     . RoleType
    -> MonadPerspectivesTransaction o
    -> MonadPerspectives Transaction
  execMonadPerspectivesTransaction authoringRole a =
    (liftAff $ createTransaction authoringRole false)
      >>= liftAff <<< new
      >>= runReaderT run
    where
    run :: MonadPerspectivesTransaction Transaction
    run = do
      void a
      AMA.get

liftToMPT :: forall a. MonadPerspectives a -> MonadPerspectivesTransaction a
liftToMPT = lift

-- | The `userId` must be an instance of the `userType`, otherwise we cannot establish whether
-- | a perspective is a self-perspective.
serialisedAsDeltasFor_ :: ContextInstance -> RoleInstance -> RoleType -> Boolean -> MonadPerspectivesTransaction Unit
serialisedAsDeltasFor_ cid userId userType isNewPeer = do
  -- All Roletypes the user may see in this context, expressed as Perspectives.
  perspectives <- liftToMPT (userType ###= perspectivesClosure_)
  if isNewPeer && (null $ filter isPerspectiveOnSelf perspectives) then do
    -- Always make sure that the user receives the deltas that describe his own role.
    selfPerspective <- lift $ minimalSelfPerspective
    case selfPerspective of
      Just sp -> serialisePerspectiveForUser cid (NA.singleton userId) userType sp
      Nothing -> pure unit
  else pure unit
  -- Now serialise all modelled perspectives for the user. This will include the self-perspective if it is modelled.
  traverse_ (serialisePerspectiveForUser cid (NA.singleton userId) userType) perspectives
  where
  isPerspectiveOnSelf :: Perspective -> Boolean
  isPerspectiveOnSelf (Perspective { roleTypes }) = isJust $ elemIndex userType roleTypes

  minimalSelfPerspective :: MonadPerspectives (Maybe Perspective)
  minimalSelfPerspective = do
    ctype <- contextType_ cid
    (Tuple result state :: Tuple (Either MultiplePerspectivesErrors QueryFunctionDescription) PhaseTwoState) <-
      runPhaseTwo_' (unsafePartial makeRoleGetter (CDOM $ ST ctype) userType) defaultDomeinFileRecord empty empty Nil
    case result of
      Left _ -> logWhen Warn SYNC (show <$> (humanizePerspectivesWarning $ CannotConstructMinimalSelfPerspective ctype userType)) *> pure Nothing
      Right object -> do
        logWhen Trace SYNC (show <$> (humanizePerspectivesWarning $ ConstructedMinimalSelfPerspective ctype userType))
        pure $ Just $ Perspective
          { id: "igored"
          , object
          , "displayName": "automatic self perspective"
          , roleTypes: [ userType ]
          , isEnumerated: isEnumeratedRoleType userType
          , roleVerbs: EM.empty
          -- Because we have isSelfPerspective = true, in the call tree below we add the property `perspectivesUsersPublicKey`/
          , propertyVerbs: EM.empty
          , actions: EM.empty
          , selfOnly: true
          , authorOnly: false
          , isSelfPerspective: true
          , automaticStates: []
          , perspectiveStartPosition: Nothing
          }

-- | Add Deltas to the transaction of the peers ultimately filling the given user roles, to provide
-- | them with a complete account of the perspective on the context instance.
serialisePerspectiveForUser
  :: ContextInstance
  -> NA.NonEmptyArray RoleInstance
  -> RoleType
  -> Perspective
  -> MonadPerspectivesTransaction Unit
serialisePerspectiveForUser cid users userRoleType p@(Perspective { object, propertyVerbs, selfOnly, authorOnly, isSelfPerspective, perspectiveStartPosition, roleTypes }) =
  if authorOnly then pure unit
  else do
    (visiblePropertyTypes :: Array PropertyType) <- liftToMPT (propertiesInPerspective p)
    readableVisiblePropertyTypes <- liftToMPT (traverse toReadable visiblePropertyTypes)
    readableUserRoleType <- liftToMPT (toReadable userRoleType)
    readableRoleTypes <- liftToMPT (traverse toReadable roleTypes)
    lift $ debugSync $
      "Serialising perspective for users "
        <> show (unwrap <$> toArray users)
        <> " of type "
        <> roletype2string readableUserRoleType
        <> "(based on perspective starting at "
        <> perspectivePositionText perspectiveStartPosition
        <> " on role types "
        <> show (roletype2string <$> readableRoleTypes)
        <> ") with properties "
        <> show (propertytype2string <$> readableVisiblePropertyTypes)
        <> "."
    serialiseRoleInstancesAndProperties cid users object (nub visiblePropertyTypes) selfOnly isSelfPerspective

-- | MODEL DEPENDENCY IN THIS FUNCTION. The correct operation of this function depends on
-- | model:System. The role model:System$PerspectivesSystem$User should have a property with
-- | local name "Id". Instances need not have a value for that property.
serialiseRoleInstancesAndProperties
  :: ContextInstance
  -> -- The context instance for which we serialise roles and properties.
  NA.NonEmptyArray RoleInstance
  -> -- User Role instances to serialise for. These have a single type.
  QueryFunctionDescription
  -> -- Find object role instances with this description.
  Array PropertyType
  -> -- PropertyTypes whose values on the role instances should be serialised.
  Boolean
  -> -- true iff the perspective is selfonly.
  Boolean
  -> -- true iff the object of the perspective equals its subject.
  MonadPerspectivesTransaction Unit
serialiseRoleInstancesAndProperties cid users object properties selfOnly isPerspectiveOnSelf = do
  -- TODO. Pas traceSync hier toe en gebruik:
  --   - Perspective.roleTypes (waar het perspectief op is)
  --   - userRoleType (wie het perspectief heeft)
  --   - positie van het perspectief in de brontekst.
  -- We know that object has a role range.
  properties' <-
    if isPerspectiveOnSelf
    -- To ensure that the receiving user of a self-perspective actually receives the full role telescope, we add
    -- the following property. In terms of communication it will be neutral as the receiver obviously has access to his own 
    -- PublicKey.
    then pure $ ARR.cons (ENP $ EnumeratedPropertyType perspectivesUsersPublicKey) properties
    else pure properties
  -- All instances of this RoleType (object) the user may see in this context.
  -- In general, these may be instances of several role types, as the perspective object is expressed as a query.
  (rinstances :: Array (DependencyPath)) <- liftToMPT ((singletonPath (C cid)) ##= interpret object)
  -- Serialise all the dependencies.
  -- If the perspective is selfOnly, then each of the users can only receive his own role and properties.
  -- This means that the users and (the role instances in) rinstances should be the same collection.
  -- in that case, call serialiseDependency on a pair consisting of the instance (one of rinstances, each represented by a Dependency) and a user that is that same instance.
  if selfOnly then do
    for_ (join (allPaths <$> rinstances))
      \(dependencies :: NonEmptyList Dependency) -> do
        -- The head will be a user role because of selfOnly.
        oneUserOnly <- unsafePartial case head dependencies of
          R r -> pure [ r ]
        serialiseDependencies oneUserOnly dependencies
        for_ properties'
          \pt -> do
            (vals :: Array DependencyPath) <- liftToMPT ((singletonPath (R $ unsafePartial fromJust $ ARR.head oneUserOnly)) ##= getPropertyValues pt)
            for_ (join (allPaths <$> vals)) (serialiseDependencies oneUserOnly)
  else do
    for_ (join (allPaths <$> rinstances)) (serialiseDependencies (toArray users))
    for_ properties'
      \pt -> do
        isSelfOnlyProperty <- lift $ propertyTypeIsSelfOnly pt
        for_ (_.head <$> rinstances)
          \(dep :: Dependency) -> do
            (vals :: Array DependencyPath) <- liftToMPT ((singletonPath dep) ##= getPropertyValues pt)
            if isSelfOnlyProperty then for_ (join (allPaths <$> vals))
              ( serialiseDependencies
                  ( unsafePartial case dep of
                      R oneUserOnly -> [ oneUserOnly ]
                  )
              )
            else for_ (join (allPaths <$> vals)) (serialiseDependencies (toArray users))

perspectivePositionText :: Maybe ArcPosition -> String
perspectivePositionText Nothing = ""
perspectivePositionText (Just position) = "Perspective source position: " <> show position

getPropertyValues :: PropertyType -> DependencyPath ~~> DependencyPath
getPropertyValues pt dep = (lift $ lift $ propertyTypeIsAuthorOnly pt) >>=
  if _ then ArrayT $ pure []
  else do
    calc <- lift $ lift $ (PClass.getProperty >=> PClass.getCalculation) pt
    -- Calculate the DependencyPath that leads to the filler whose type supports the property. Start from that.
    -- This is to accomodate overloaded fillers.
    pathToRoleWithProperty <- unsafePartial computePathToFillerWithProperty dep
    interpret calc pathToRoleWithProperty

  where
  computePathToFillerWithProperty :: Partial => DependencyPath ~~> DependencyPath
  computePathToFillerWithProperty path@{ head } = ArrayT case head of
    R rid -> do
      localProps <- lift $ (roleType_ >=> allLocallyRepresentedProperties <<< ST) rid
      if isJust $ elemIndex pt localProps
      -- The type of the role instance support the property. Return the path
      then pure [ path ]
      -- The type of the role instance doesn't support the property. Compute the path that leads to its filler
      else do
        mfiller <- lift $ binding_ rid
        case mfiller of
          -- We cannot find the filler that supports this property.
          Nothing -> pure []
          -- Contintue with the filler and add a step to the path.
          Just b -> runArrayT $ computePathToFillerWithProperty (consOnMainPath (R b) path)

-- | Adds deltas to the current transaction for the given users and for the dependencies in the List of Dependency-s.
serialiseDependencies :: Array RoleInstance -> NonEmptyList Dependency -> MonadPerspectivesTransaction Unit
serialiseDependencies users deps = void $ runStateT (serialiseDependencies_ users deps) []

serialiseDependencies_
  :: Array RoleInstance
  -> NonEmptyList Dependency
  -> StateT (Array Dependency) MonadPerspectivesTransaction Unit
serialiseDependencies_ users deps = void $ foldM
  (serialiseDependency users)
  Nothing
  deps

-- Always returns the second argument in Maybe.
-- | `users` will not always be model:System$PerspectivesSystem$User instances.
serialiseDependency
  :: Array RoleInstance
  -> Maybe Dependency
  -> Dependency
  -> StateT (Array Dependency) MonadPerspectivesTransaction (Maybe Dependency)
serialiseDependency users mpreviousDependency currentDependency = do
  -- We serialise a role dependency as soon as we see it, hence we analyse the currentDependency.
  -- Public resources are not serialised as deltas: the receiving peer will fetch them directly
  -- from the public endpoint.
  case currentDependency of
    (R roleId) -> do
      if isInPublicScheme (unwrap roleId) then pure unit
      else do
        seenBefore <- gets \depsSeenBefore -> isJust $ elemIndex currentDependency depsSeenBefore
        if seenBefore then pure unit
        else do
          void $ modify \depsSeenBefore -> snoc depsSeenBefore currentDependency
          lift $ addDeltasForRole roleId
    otherwise -> pure unit

  case mpreviousDependency, currentDependency of
    Just first@(R roleId1), (R roleId2) -> do
      if isInPublicScheme (unwrap roleId1) || isInPublicScheme (unwrap roleId2) then pure unit
      else
        lift $ addBindingDelta roleId1 roleId2 >>=
          if _ then pure unit
          else addBindingDelta roleId2 roleId1 >>=
            if _ then pure unit
            else do
              padding <- lift transactionLevel
              log (padding <> "serialiseDependency finds two role dependencies without binding: " <> show mpreviousDependency <> ", " <> show currentDependency)
    Just (V ptypeString (Value val)), (R roleId) ->
      if isInPublicScheme (unwrap roleId) then pure unit
      else lift $ addPropertyDelta roleId ptypeString val
    _, _ -> pure unit
  pure $ Just currentDependency

  where
  -- | Returns true iff the binding of the first argument equals the second argument.
  -- | If true, also adds the binding delta from the DeltaStore to the transaction.
  addBindingDelta :: RoleInstance -> RoleInstance -> MonadPerspectivesTransaction Boolean
  addBindingDelta roleId1 roleId2 = (liftToMPT $ try $ getPerspectRol roleId2) >>= handlePerspectRolError' "addBindingDelta" false
    \(PerspectRol { binding }) -> case binding of
      Just b ->
        if b == roleId1 then do
          -- Query DeltaStore for binding delta of this role instance.
          bindingDeltas <- lift $ getDeltasForResource (unwrap roleId2 <> "#binding")
          for_ bindingDeltas \(DeltaStoreRecord { signedDelta }) ->
            addDelta $ DeltaInTransaction { users, delta: signedDelta }
          pure true
        else pure false
      Nothing -> pure false

  -- | Query DeltaStore for property deltas of this role instance and property type.
  addPropertyDelta :: RoleInstance -> PropertyName -> String -> MonadPerspectivesTransaction Unit
  addPropertyDelta roleId ptypeString val = do
    propertyDeltas <- lift $ getDeltasForResource (unwrap roleId <> "#" <> ptypeString)
    for_ propertyDeltas \(DeltaStoreRecord { signedDelta }) ->
      addDelta $ DeltaInTransaction { users, delta: signedDelta }

  -- | Query DeltaStore for creation deltas of this role and its context.
  addDeltasForRole :: RoleInstance -> MonadPerspectivesTransaction Unit
  addDeltasForRole roleId = do
    (liftToMPT $ try $ getPerspectRol roleId) >>=
      handlePerspectRolError "addDeltasForRole"
        \(PerspectRol { context }) -> do
          (liftToMPT $ try $ getPerspectContext context) >>=
            handlePerspectContextError "addDeltasForRole"
              \(PerspectContext { buitenRol }) -> do
                -- Get creation deltas for external role.
                extRoleDeltas <- lift $ getDeltasForResource (unwrap buitenRol)
                -- We expect at least two deltas for the external role: the ConstructExternalRole delta and the ContextDelta that links it to the context. If there are less than two, something is wrong.
                if length extRoleDeltas < 2 then lift $ errorDelta ("No deltas found for external role " <> show buitenRol) else pure unit
                -- Get creation deltas for context.
                contextDeltas <- lift $ getDeltasForResource (unwrap context)
                -- We expect at least one delta for the context: the ConstructEmptyContext delta. If there are none, something is wrong.
                if null contextDeltas then lift $ errorDelta ("No deltas found for context " <> show context) else pure unit
                -- Get creation deltas for this role.
                roleDeltas <- lift $ getDeltasForResource (unwrap roleId)
                -- We expect at least one delta for the role: the ConstructEmptyRole delta. If there are none, something is wrong.
                if null roleDeltas then lift $ errorDelta ("No deltas found for role " <> show roleId) else pure unit
                for_ (extRoleDeltas <> contextDeltas <> roleDeltas) \(DeltaStoreRecord { signedDelta }) ->
                  addDelta $ DeltaInTransaction { users, delta: signedDelta }

  withContext :: Boolean
  withContext = true

  withoutContext :: Boolean
  withoutContext = false

type PropertyName = String
