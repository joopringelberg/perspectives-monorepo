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
import Perspectives.Error.Pretty (humanizePerspectivesError)
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
import Perspectives.Types.ObjectGetters (equalsOrGeneralisesContextADT, findPerspective, hasPerspectiveOnPropertyWithVerb, hasPerspectiveOnRoleWithVerbs, isPerspectiveOnADT, propertiesInPerspective, supportsPropertyForVerb)
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)

type Found a = StateT Boolean MonadPerspectives a

hasBeenFound :: Found Boolean
hasBeenFound = get

-- | True iff the user role represented by the subject argument has a perspective
-- | on the RoleType (in any state) with PropertyVerbs
-- |  * whose properties include the property,
-- |  * whose verbs include the given Verb.
roleHasPerspectiveOnPropertyWithVerb :: PerspectivesUser -> RoleType -> RoleInstance -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnPropertyWithVerb author subject roleInstance property verb' = do
  roleType' <- roleInstance ##>> roleType
  typePermission <- unsafePartial $ hasPerspectiveOnPropertyWithVerb subject roleType' property verb'
  permitted <-
    if typePermission then pure true
    else hasInstancePropertyPermission author subject roleInstance property verb'
  if permitted then pure $ Right true
  else pure $ Left $ UnauthorizedForProperty "Auteur" subject (ENR roleType') (ENP property) verb' Nothing Nothing

-- | A disjunctive filler restriction cannot prove where a property is stored.
-- | Instead, witness the signed author's subject role, its selected object and
-- | the exact property storage reached by the same resolver used for writes.
hasInstancePropertyPermission :: PerspectivesUser -> RoleType -> RoleInstance -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives Boolean
hasInstancePropertyPermission author subject storage property verb = do
  let
    identities = nub $ perspectivesUser2RoleInstance <<< deltaAuthor2ResourceIdentifier <$>
      [ PerspectivesUser $ unwrap $ unschemePerspectivesUser author, author ]
  roots <- filterA entityExists identities
  let
    queryAuthor = case uncons roots of
      Nothing -> author
      Just { head } -> roleInstance2PerspectivesUser head
  authorRoles <- filledRolesClosure [] roots
  contexts <- nub <<< join <$> traverse context_ authorRoles
  subjectContexts <- contextOfRoleType subject
  readableSubject <- toReadable subject
  withQueryAuthor queryAuthor $ isJust <$> findPerspective readableSubject
    ( \perspective@(Perspective p) -> do
        properties <- propertiesInPerspective perspective
        writableProperties <- filterA (\prop -> toReadable prop >>= \readable -> supportsPropertyForVerb perspective readable verb) properties
        if writableProperties == [] then pure false
        else anyM
          ( \ctx -> do
              contextType <- contextType_ ctx
              compatible <- equalsOrGeneralisesContextADT subjectContexts (ST contextType)
              if not compatible then pure false
              else do
                actors <- ctx ##= getRoleInstances subject
                anyM
                  ( \actor ->
                      if not $ elem actor authorRoles then pure false
                      else withFrame do
                        addBinding "currentactor" [ unwrap actor ]
                        addBinding "currentcontext" [ unwrap ctx ]
                        getter <- compileFunction p.object
                        objects <- unwrap ctx ##= getter
                        anyM
                          ( \object ->
                              if p.selfOnly && object /= unwrap actor then pure false
                              else anyM (resolvesToStorage $ RoleInstance object) writableProperties
                          )
                          objects
                  )
                  actors
          )
          contexts
    )
  where
  anyM :: forall a. (a -> MonadPerspectives Boolean) -> Array a -> MonadPerspectives Boolean
  anyM predicate = foldM (\found value -> if found then pure true else predicate value) false

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
