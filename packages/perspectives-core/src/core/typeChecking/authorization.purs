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

module Perspectives.Checking.Authorization where

import Prelude

import Control.Monad.State (StateT, get)
import Data.Array (elem, filterA, foldMap, nub, uncons)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala, unwrap)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.Assignment.Update (RoleProp(..), getPropertyBearingRoleInstance)
import Perspectives.CoreTypes (MonadPerspectives, (##=), (##>>), (###=))
import Perspectives.Error.Pretty (humanizePerspectivesError, renderPerspectivesError)
import Perspectives.Instances.ObjectGetters (binding_, context_, contextType_, deltaAuthor2ResourceIdentifier, getAllFilledRoles_, roleType)
import Perspectives.Logging (warnAuth)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.PerspectivesState (addBinding, withFrame)
import Perspectives.Persistent (entityExists)
import Perspectives.Query.UnsafeCompiler (compileFunction, getRoleInstances, withQueryAuthor)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Role (adtOfRole, contextOfRoleType, getRole)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser(..), RoleInstance(..), perspectivesUser2RoleInstance, roleInstance2PerspectivesUser)
import Perspectives.Representation.Perspective (Perspective(..), perspectiveSupportsOneOfRoleVerbs)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..), RoleType(..))
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerb)
import Perspectives.Sidecar.ToReadable (toReadable)
import Perspectives.Sidecar.ToStable (toStable)
import Perspectives.Types.ObjectGetters (equalsOrGeneralisesContextADT, findPerspective, hasPerspectiveOnPropertyWithVerbByImplication, hasPerspectiveOnRoleWithVerbs, isPerspectiveOnADT, propertiesInPerspective, supportsPropertyForVerb)
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)

type Found a = StateT Boolean MonadPerspectives a

hasBeenFound :: Found Boolean
hasBeenFound = get

-- | True iff the user role represented by the subject argument has a perspective
-- | on an object implying the property's declaring role (in any state) with PropertyVerbs
-- |  * whose properties include the property,
-- |  * whose verbs include the given Verb.
roleHasPerspectiveOnPropertyWithVerb :: PerspectivesUser -> RoleType -> RoleInstance -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnPropertyWithVerb author subject roleInstance property verb' = do
  roleType' <- roleInstance ##>> roleType
  typePermission <- unsafePartial $ hasPerspectiveOnPropertyWithVerbByImplication subject property verb'
  if typePermission then pure $ Right true
  else hasInstancePropertyPermission author subject roleInstance property verb' >>= case _ of
    Right _ -> pure $ Right true
    Left instanceFailure -> do
      typeFailure <- renderPerspectivesError $
        UnauthorizedForProperty "Auteur" subject (ENR roleType') (ENP property) verb' Nothing Nothing
      pure $ Left $ Custom $ "Type-level property authorization failed: " <> typeFailure
        <> " "
        <> instanceFailure

-- | A disjunctive filler restriction cannot prove where a property is stored.
-- | Instead, witness the signed author's subject role, its selected object and
-- | the exact property storage reached by the same resolver used for writes.
hasInstancePropertyPermission :: PerspectivesUser -> RoleType -> RoleInstance -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives (Either String Unit)
hasInstancePropertyPermission author subject storage property verb = do
  let
    identities = nub $ perspectivesUser2RoleInstance <<< deltaAuthor2ResourceIdentifier <$>
      [ PerspectivesUser $ unwrap $ unschemePerspectivesUser author, author ]
  roots <- filterA entityExists identities
  let
    queryAuthor = case uncons roots of
      Nothing -> author
      Just { head } -> roleInstance2PerspectivesUser head
  -- All role instances transitively filled by the author are considered for property authorization.
  authorRoles <- filledRolesClosure [] roots
  -- The context instance of each author role is considered for property authorization.
  contexts <- nub <<< join <$> traverse context_ authorRoles
  -- The type of the context of the type of the subject.
  subjectContexts <- contextOfRoleType subject
  readableSubject <- toReadable subject
  permitted <- withQueryAuthor queryAuthor $ isJust <$> findPerspective readableSubject
    ( \perspective@(Perspective p) -> do
        properties <- propertiesInPerspective perspective
        writableProperties <- filterA (\prop -> toReadable prop >>= \readable -> supportsPropertyForVerb perspective readable verb) properties
        if writableProperties == [] then pure false
        else anyM
          ( \ctx -> do
              -- ctx is one of the context instances of the author roles.
              contextType <- contextType_ ctx
              -- Is this context compatible with the subject's context type?
              compatible <- equalsOrGeneralisesContextADT subjectContexts (ST contextType)
              if not compatible then pure false
              else do
                -- Get all role instances of the subject within this context.
                actors <- ctx ##= getRoleInstances subject
                anyM
                  ( \actor ->
                      if not $ elem actor authorRoles then pure false
                      -- If the actor is one of the author roles, we proceed with further checks.
                      else withFrame do
                        addBinding "currentactor" [ unwrap actor ]
                        addBinding "currentcontext" [ unwrap ctx ]
                        getter <- compileFunction p.object
                        -- Retrieve all perspective objects within this context.
                        objects <- unwrap ctx ##= getter
                        anyM
                          ( \object ->
                              -- A provision for selfonly perspectives.
                              if p.selfOnly && object /= unwrap actor then pure false
                              -- true if the object's filler chain contains the storage instance for the given property.
                              else anyM (resolvesToStorage $ RoleInstance object) writableProperties
                          )
                          objects
                  )
                  actors
          )
          contexts
    )
  if permitted then pure $ Right unit
  else do
    let
      message = "Instance property authorization failed for author " <> unwrap author
        <> " writing "
        <> unwrap property
        <> " on "
        <> unwrap storage
        <> ": no author-linked subject role and permitted perspective object could be witnessed resolving to this exact property-bearing instance and property. Author identity found: "
        <> show (unwrap <$> roots)
        <> "; roles (transitively) filled by the author: "
        <> show (unwrap <$> authorRoles)
        <> "; their contexts: "
        <> show (unwrap <$> contexts)
        <> "."
    warnAuth message
    pure $ Left message
  where
  anyM :: forall a. (a -> MonadPerspectives Boolean) -> Array a -> MonadPerspectives Boolean
  anyM predicate = foldM (\found value -> if found then pure true else predicate value) false

  -- Returns true iff the objects filler chain contains the storage instance for the given property.
  resolvesToStorage :: RoleInstance -> PropertyType -> MonadPerspectives Boolean
  resolvesToStorage object (ENP prop) = do
    stableProperty <- toStable prop
    resolved <- getPropertyBearingRoleInstance stableProperty object
    pure $ case resolved of
      Just (RoleProp target targetProperty) -> target == storage && targetProperty == property
      Nothing -> false
  resolvesToStorage _ (CP _) = pure false

  filledRolesClosure :: Array RoleInstance -> Array RoleInstance -> MonadPerspectives (Array RoleInstance)
  filledRolesClosure visited remaining = case uncons remaining of
    Nothing -> pure visited
    Just { head: role, tail } ->
      if elem role visited then filledRolesClosure visited tail
      else do
        candidates <- getAllFilledRoles_ role
        filled <- filterA (\candidate -> (_ == Just role) <$> binding_ candidate) candidates
        filledRolesClosure (visited <> [ role ]) (tail <> filled)

-- | True if the user role represented by the subject argument has a perspective
-- | on the RoleType that includes an Action with the given Verb,
-- | OR if the subject has the aspect "sys:RootContext$RootUser"
roleHasPerspectiveOnRoleWithVerb :: RoleType -> EnumeratedRoleType -> Array RoleVerb -> Maybe ArcPosition -> Maybe ArcPosition -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnRoleWithVerb subject roleType verbs mstart mend = do
  hasVerbs <- unsafePartial (roleType ###= hasPerspectiveOnRoleWithVerbs verbs subject)
  if ala Disj foldMap hasVerbs then pure $ Right true
  else Left <$> humanizePerspectivesError (UnauthorizedForRole "Auteur" subject (ENR roleType) verbs mstart mend)

-- | This function differs from `roleHasPerspectiveOnRoleWithVerb` in that it uses an
-- | `authorizedRole` (second parameter). This is the role that binds the external role that we scrutinize,
-- | or it is the Calculated role that results in external roles. It is this contextrole (or calculated role)
-- | that the subject is authorized for.
roleHasPerspectiveOnExternalRoleWithVerbs :: RoleType -> Maybe RoleType -> Array RoleVerb -> Maybe ArcPosition -> Maybe ArcPosition -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnExternalRoleWithVerbs subject mroleType verbs mstart mend = case mroleType of
  Nothing -> do
    warnAuth "roleHasPerspectiveOnExternalRoleWithVerb: no authorizedRole provided to construct external role"
    pure $ Left $ Custom "roleHasPerspectiveOnExternalRoleWithVerb: no authorizedRole provided to construct external role"
  Just rt -> do
    (hasPerspectiveWithVerb subject rt) >>=
      if _ then pure $ Right true
      else Left <$> humanizePerspectivesError (UnauthorizedForRole "Auteur" subject rt verbs mstart mend)
    where
    hasPerspectiveWithVerb :: RoleType -> RoleType -> MonadPerspectives Boolean
    hasPerspectiveWithVerb subjectType authorizedRoleType = do
      adtOfAuthorizedRoleType <- getRole authorizedRoleType >>= adtOfRole
      isJust <$> findPerspective subjectType
        \perspective -> do
          s <- pure $ perspectiveSupportsOneOfRoleVerbs perspective verbs
          p <- unsafePartial (perspective `isPerspectiveOnADT` adtOfAuthorizedRoleType)
          pure (s && p)
