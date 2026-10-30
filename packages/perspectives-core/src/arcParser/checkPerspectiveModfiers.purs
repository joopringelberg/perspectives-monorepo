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

module Perspectives.Parsing.Arc.PhaseThree.CheckPerspectivesModifiers where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (gets) as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (execWriterT, tell)
import Data.Array (elemIndex, filter, head, length, null, nub)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Foreign.Object (values)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Error.Pretty (humanizePerspectivesWarning)
import Perspectives.ErrorLogging (warnModeller)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, withDomeinFile)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), QueryFunctionDescription(..), range, roleInContext2Context)
import Perspectives.Representation.ADT (allLeavesInADT)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (getCalculatedRole, getContext)
import Perspectives.Representation.Class.Role (Role(..), identifierOfRole, kindOfRole, perspectivesOfRole, pos, posOfRole, roleIsFunctional, typeOfRole)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Perspective (Perspective(..), perspectiveSupportsPropertyWithOneofVerbs)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(BinaryCombinator, DataTypeGetter, FilterF, RolGetter, WithFrame))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType, PropertyType(..), RoleKind(..), RoleType(..))
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.Types.ObjectGetters (propertiesInPerspective)
import Perspectives.Warning (PerspectivesWarning(..))

checkPerspectiveModifiers :: PhaseThree Unit
checkPerspectiveModifiers = do
  df@{ id } <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    do
      { enumeratedProperties } <- lift $ State.gets _.dfr
      for_ enumeratedProperties checkAuthorOnly
      for_ enumeratedProperties checkSelfOnly
      checkIndexedRootPerspectives

checkIndexedRootPerspectives :: PhaseThree Unit
checkIndexedRootPerspectives = do
  userRoles <- userRolesInModel
  for_ userRoles \userRole -> do
    let
      userType = case userRole of
        E role -> typeOfRole role
        C role -> typeOfRole role
    localUser <- lift $ lift $ isLocalUser userRole
    unless localUser $
      for_ (perspectivesOfRole userRole) \perspective@(Perspective { object }) -> do
        properties <- lift $ lift $ propertiesInPerspective perspective
        unless (null properties) case range object of
          RDOM roles ->
            for_ (nub $ roleInContext2Context <$> allLeavesInADT roles) \contextType -> do
              Context { indexedContext, context } <- lift $ lift $ getContext contextType
              when (isJust indexedContext && isNothing context)
                $ lift
                $ lift
                $
                  humanizePerspectivesWarning (PeerPerspectiveOnIndexedRootContext userType contextType)
                    >>= warnModeller
          _ -> pure unit
  where
  isLocalUser (C (CalculatedRole { calculation: Q query })) = calculatesMe query
  isLocalUser _ = pure false

-- Only exempt calculations whose result is provably the local user, not queries that merely contain `me`.
calculatesMe :: QueryFunctionDescription -> MonadPerspectives Boolean
calculatesMe = check []
  where
  check :: Array CalculatedRoleType -> QueryFunctionDescription -> MonadPerspectives Boolean
  check _ (SQD _ (DataTypeGetter MeF) _ _ _) = pure true
  check visited (SQD _ (RolGetter (CR roleType)) _ _ _)
    | not (isJust $ elemIndex roleType visited) = do
        CalculatedRole { calculation } <- getCalculatedRole roleType
        case calculation of
          Q query -> check (visited <> [ roleType ]) query
          _ -> pure false
  check visited (UQD _ WithFrame query _ _ _) = check visited query
  check visited (BQD _ (BinaryCombinator SequenceF) _ result _ _ _) = check visited result
  check visited (BQD _ (BinaryCombinator ComposeF) source (UQD _ FilterF _ _ _ _) _ _ _) = check visited source
  check visited (BQD _ (BinaryCombinator ComposeF) _ result _ _ _) = check visited result
  check visited (BQD _ (BinaryCombinator UnionF) left right _ _ _) = do
    localLeft <- check visited left
    if localLeft then check visited right else pure false
  check _ _ = pure false

findRolesWithProperty :: PropertyType -> PhaseThree (Array EnumeratedRole)
findRolesWithProperty pt = do
  { enumeratedRoles } <- lift $ State.gets _.dfr
  pure $ filter
    (\(EnumeratedRole { properties }) -> isJust $ elemIndex pt properties)
    (values enumeratedRoles)

isPerspectiveOnRoleAndProperty :: RoleType -> PropertyType -> Perspective -> MonadPerspectives Boolean
isPerspectiveOnRoleAndProperty role prop p@(Perspective { roleTypes }) = do
  props <- (propertiesInPerspective p)
  pure ((isJust $ elemIndex prop props) && (isJust $ elemIndex role roleTypes))

findPerspectivesOnProperty :: PropertyType -> PhaseThree (Array { userRole :: Role, objectRole :: EnumeratedRole, perspective :: Perspective })
findPerspectivesOnProperty prop = do
  objectRoles <- findRolesWithProperty prop
  userRoles <- userRolesInModel
  lift $ lift $ execWriterT
    ( for_ objectRoles
        ( \objectRole ->
            for_ userRoles
              \userRole -> for_ (perspectivesOfRole userRole)
                \perspective -> (lift $ isPerspectiveOnRoleAndProperty (ENR $ identifier objectRole) prop perspective) >>=
                  if _ then tell [ { userRole, objectRole, perspective } ]
                  else pure unit
        )
    )

checkAuthorOnly :: EnumeratedProperty -> PhaseThree Unit
checkAuthorOnly (EnumeratedProperty { id: prop, authoronly }) =
  if authoronly then do
    perspectivesAndRoles <- findPerspectivesOnProperty (ENP prop)
    case head perspectivesAndRoles of
      (Just { userRole, objectRole, perspective }) ->
        if ((length perspectivesAndRoles) == 1) then do
          isFunctional <- lift $ lift $ roleIsFunctional userRole
          if isFunctional then throwError [ PerspectiveCannotBeAuthorOnly (identifierOfRole userRole) (posOfRole userRole) (identifier objectRole) (pos objectRole) prop ]
          else pure unit
        else pure unit
      -- Give a warning here?
      Nothing -> pure unit
  else pure unit

userRolesInModel :: PhaseThree (Array Role)
userRolesInModel = do
  { enumeratedRoles, calculatedRoles } <- lift $ State.gets _.dfr
  pure $ (E <$> filter (eq UserRole <<< kindOfRole) (values enumeratedRoles)) <>
    (C <$> (filter (eq UserRole <<< kindOfRole) (values calculatedRoles)))

checkSelfOnly :: EnumeratedProperty -> PhaseThree Unit
checkSelfOnly (EnumeratedProperty { id: prop, selfonly }) =
  if selfonly then do
    perspectivesAndRoles <- findPerspectivesOnProperty (ENP prop)
    perspectivesThatWrite <- pure $ filter
      (\{ perspective } -> perspectiveSupportsPropertyWithOneofVerbs perspective (ENP prop) (PSet [ SetPropertyValue, AddPropertyValue, RemovePropertyValue ]))
      perspectivesAndRoles
    if ((length perspectivesAndRoles) == 1) then unsafePartial case head perspectivesAndRoles of
      (Just { userRole, objectRole, perspective }) -> throwError [ SelfOnlyNeedsTwoRoles (identifierOfRole userRole) (posOfRole userRole) prop ]
    else void $ for_ perspectivesAndRoles
      ( \{ userRole, objectRole, perspective } ->
          if (unwrap perspective).isSelfPerspective then do
            isFunctional <- lift $ lift $ roleIsFunctional userRole
            isWritingPerspective <- pure $ perspectiveSupportsPropertyWithOneofVerbs perspective (ENP prop) (PSet [ SetPropertyValue, AddPropertyValue, RemovePropertyValue ])
            anotherPerspectiveWrites <-
              if isWritingPerspective then pure $ length perspectivesThatWrite > 1
              else pure $ length perspectivesThatWrite > 0
            if isFunctional || not anotherPerspectiveWrites then throwError $ [ SelfOnlyShouldBeAuthorOnly (identifierOfRole userRole) (posOfRole userRole) prop ]
            else pure unit
          else pure unit
      )
  else pure unit
