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
import Data.Array (foldMap)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (ala, unwrap)
import Partial.Unsafe (unsafePartial)
import Perspectives.Checking.PropertyProvenance (checkPropertyProvenance, findWitness, witnessSearchDiagnostics)
import Perspectives.CoreTypes (MonadPerspectives, (###=), (##>>))
import Perspectives.Error.Pretty (humanizePerspectivesError, renderPerspectivesError)
import Perspectives.Instances.ObjectGetters (roleType)
import Perspectives.Logging (warnAuth)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.Class.Role (adtOfRole, getRole)
import Perspectives.Representation.InstanceIdentifiers (PerspectivesUser, RoleInstance)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..), isElementOf)
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), perspectiveSupportsOneOfRoleVerbs)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..), RoleType(..))
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerb)
import Perspectives.Sidecar.ToReadable (toReadable)
import Perspectives.TypesForDeltas (PropertyProvenance)
import Perspectives.Types.ObjectGetters (findPerspective, hasPerspectiveOnPropertyWithVerbByImplication, hasPerspectiveOnRoleWithVerbs, isPerspectiveOnADT)

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

-- | True iff no perspective of the subject role type (or its aspects) lets it apply any of the
-- | given verbs to any property at all. Then neither the type-level grant nor an instance witness
-- | (hasInstancePropertyPermission) can ever authorize a property delta of this subject with one
-- | of these verbs: the lack of authority follows from the model alone and does not depend on
-- | instance evidence that may still be missing. See delta-ordering.md §6.4 (Stage 0).
-- | This is deliberately conservative: it does not try to prove that a granted property cannot
-- | resolve to the delta's property, as that depends on aliases and filler chains.
subjectCannotWriteAnyPropertyWith :: RoleType -> Array PropertyVerb -> MonadPerspectives Boolean
subjectCannotWriteAnyPropertyWith subject verbs = do
  readableSubject <- toReadable subject
  isNothing <$> findPerspective readableSubject
    ( \(Perspective { propertyVerbs }) -> pure $ any
        ( any \(PropertyVerbs properties pverbs) ->
            properties /= Empty && properties /= PSet [] && any (flip isElementOf pverbs) verbs
        )
        (unwrap propertyVerbs)
    )

-- | A disjunctive filler restriction cannot prove where a property is stored.
-- | Instead, witness the signed author's subject role, its selected object and
-- | the exact property storage reached by the same resolver used for writes.
hasInstancePropertyPermission :: PerspectivesUser -> RoleType -> RoleInstance -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives (Either String Unit)
hasInstancePropertyPermission author subject storage property verb = do
  findWitness author subject storage property verb >>= case _ of
    Just _ -> pure $ Right unit
    Nothing -> do
      diagnostics <- witnessSearchDiagnostics author
      let
        message = "Instance property authorization failed for author " <> unwrap author
          <> " writing "
          <> unwrap property
          <> " on "
          <> unwrap storage
          <> ": no author-linked subject role and permitted perspective object could be witnessed resolving to this exact property-bearing instance and property. "
          <> diagnostics
      warnAuth message
      pure $ Left message

-- | Authorize an incoming property delta (Stage 1, delta-ordering.md §6.5).
-- | With signed provenance, the signed witness is checked directly; if it cannot be established, a witness is
-- | searched in the author's filled-role closure. The self-declared subject type alone no longer suffices.
-- | Without provenance (the sender found no witness), the legacy check applies: type-level permission of the
-- | self-declared subject, then the instance witness search. This is logged, as it is a gap to close.
authorizePropertyDelta :: PerspectivesUser -> RoleType -> RoleInstance -> EnumeratedPropertyType -> Maybe PropertyProvenance -> PropertyVerb -> MonadPerspectives (Either PerspectivesError Boolean)
authorizePropertyDelta author subject storage property mprovenance verb = case mprovenance of
  Nothing -> do
    warnAuth $ "Property delta without provenance by " <> unwrap author <> " on " <> unwrap storage <> " for " <> unwrap property <> "; using the type-level check."
    roleHasPerspectiveOnPropertyWithVerb author subject storage property verb
  Just provenance -> checkPropertyProvenance author subject storage property verb provenance >>= case _ of
    Right _ -> pure $ Right true
    Left provenanceFailure -> hasInstancePropertyPermission author subject storage property verb >>= case _ of
      Right _ -> pure $ Right true
      Left instanceFailure -> pure $ Left $ Custom $ "Signed property provenance could not be established: " <> provenanceFailure <> ". " <> instanceFailure

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
