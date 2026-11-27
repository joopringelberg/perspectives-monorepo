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

-- | Instance-level authority for property deltas (delta-ordering.md §6.5, Stage 1).
-- |
-- | A *witness* for a property write by `author` with subject role type `subject` on (`storage`, `property`)
-- | with `verb` is a triple (actor, context, object) such that:
-- |   * the actor is (transitively) filled by the author;
-- |   * evaluating the subject role from the context yields the actor;
-- |   * some perspective of the subject grants the verb on a property and evaluating its object query
-- |     from the context (with `currentactor` and `currentcontext` bound) yields the object
-- |     (for a selfonly perspective, the object is the actor);
-- |   * resolving that property from the object reaches exactly (`storage`, `property`).
-- |
-- | The sender finds a witness (`findPropertyProvenance`) and signs it into the delta. The receiver
-- | checks the signed witness directly (`checkPropertyProvenance`) instead of searching for one.
module Perspectives.Checking.PropertyProvenance
  ( Witness
  , authorRoots
  , checkPropertyProvenance
  , findPropertyProvenance
  , findWitness
  , refutedPropertyProvenance
  , verbsForPropertyDeltaType
  , witnessSearchDiagnostics
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Data.Array (elem, filterA, nub, uncons)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Perspectives.Assignment.PropertyBearingRole (RoleProp(..), getPropertyBearingRoleInstance)
import Perspectives.CoreTypes (MonadPerspectives, (##=))
import Perspectives.Instances.ObjectGetters (binding_, context_, contextType_, deltaAuthor2ResourceIdentifier, getAllFilledRoles_, roleType_)
import Perspectives.PerspectivesState (addBinding, withFrame)
import Perspectives.Persistent (entityExists)
import Perspectives.Query.UnsafeCompiler (compileFunction, getRoleInstances, withQueryAuthor)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Role (contextOfRepresentationOfRole)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, PerspectivesUser(..), RoleInstance(..), perspectivesUser2RoleInstance, roleInstance2PerspectivesUser)
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, PropertyType(..), RoleType)
import Perspectives.Representation.Verbs (PropertyVerb)
import Perspectives.Representation.Verbs as Verbs
import Perspectives.Sidecar.ToReadable (toReadable)
import Perspectives.Sidecar.ToStable (toStable)
import Perspectives.Types.ObjectGetters (equalsOrGeneralisesContextADT, findPerspective, propertiesInPerspective, supportsPropertyForVerb)
import Perspectives.TypesForDeltas (PropertyProvenance, RolePropertyDeltaType(..))
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)

type Witness = { actor :: RoleInstance, context :: ContextInstance, object :: RoleInstance }

-- | The verbs (in order of preference) that authorize a property delta of the given type.
-- | AddProperty accepts SetPropertyValue for historical reasons (see executeRolePropertyDelta_).
verbsForPropertyDeltaType :: RolePropertyDeltaType -> Array PropertyVerb
verbsForPropertyDeltaType = case _ of
  AddProperty -> [ Verbs.AddPropertyValue, Verbs.SetPropertyValue ]
  RemoveProperty -> [ Verbs.RemovePropertyValue ]
  DeleteProperty -> [ Verbs.DeleteProperty ]
  SetProperty -> [ Verbs.SetPropertyValue ]
  UploadFile -> [ Verbs.SetPropertyValue ]

-- | The locally stored identities of the author (the roots of every authority chain).
authorRoots :: PerspectivesUser -> MonadPerspectives (Array RoleInstance)
authorRoots author = filterA entityExists $ nub $ perspectivesUser2RoleInstance <<< deltaAuthor2ResourceIdentifier <$>
  [ PerspectivesUser $ unwrap $ unschemePerspectivesUser author, author ]

queryAuthorFor :: PerspectivesUser -> Array RoleInstance -> PerspectivesUser
queryAuthorFor author roots = case uncons roots of
  Nothing -> author
  Just { head } -> roleInstance2PerspectivesUser head

-- | True iff following the filler chain from the role reaches one of the roots.
filledFromRoots :: Array RoleInstance -> RoleInstance -> MonadPerspectives Boolean
filledFromRoots roots = go []
  where
  go visited r =
    if elem r roots then pure true
    else if elem r visited then pure false
    else binding_ r >>= case _ of
      Nothing -> pure false
      Just filler -> go (visited <> [ r ]) filler

-- | Search for a witness, considering only the given candidate contexts and only actors
-- | accepted by `isAuthorRole`.
searchWitness
  :: PerspectivesUser
  -> RoleType
  -> RoleInstance
  -> EnumeratedPropertyType
  -> PropertyVerb
  -> Array ContextInstance
  -> (RoleInstance -> MonadPerspectives Boolean)
  -> (Witness -> Boolean)
  -> MonadPerspectives (Maybe Witness)
searchWitness queryAuthor subject storage property verb contexts isAuthorRole acceptWitness = do
  subjectContexts <- contextOfRepresentationOfRole subject
  compatibleContexts <- filterA
    ( \ctx -> (try $ contextType_ ctx) >>= case _ of
        Left _ -> pure false
        Right contextType -> equalsOrGeneralisesContextADT subjectContexts (ST contextType)
    )
    contexts
  if compatibleContexts == [] then pure Nothing
  else do
    found <- liftEffect $ Ref.new Nothing
    readableSubject <- toReadable subject
    void $ withQueryAuthor queryAuthor $ findPerspective readableSubject
      ( \perspective@(Perspective p) -> do
          properties <- propertiesInPerspective perspective
          writableProperties <- filterA (\prop -> toReadable prop >>= \readable -> supportsPropertyForVerb perspective readable verb) properties
          if writableProperties == [] then pure false
          else do
            mwitness <- findM
              ( \ctx -> do
                  actors <- ctx ##= getRoleInstances subject
                  findM
                    ( \actor -> isAuthorRole actor >>=
                        if _ then withFrame do
                          addBinding "currentactor" [ unwrap actor ]
                          addBinding "currentcontext" [ unwrap ctx ]
                          getter <- compileFunction p.object
                          objects <- unwrap ctx ##= getter
                          findM
                            ( \object ->
                                let
                                  witness = { actor, context: ctx, object: RoleInstance object }
                                in
                                  if p.selfOnly && object /= unwrap actor then pure Nothing
                                  else if not (acceptWitness witness) then pure Nothing
                                  else anyM (resolvesToStorage storage property (RoleInstance object)) writableProperties >>=
                                    if _ then pure (Just witness) else pure Nothing
                            )
                            objects
                        else pure Nothing
                    )
                    actors
              )
              compatibleContexts
            case mwitness of
              Nothing -> pure false
              Just w -> liftEffect (Ref.write (Just w) found) *> pure true
      )
    liftEffect $ Ref.read found

-- | Find a witness for the author's write. First looks in the contexts of the storage instance and of the
-- | roles it (transitively) fills, checking authorship by walking the actor's filler chain. Then falls
-- | back to all contexts of the roles transitively filled by the author.
findWitness :: PerspectivesUser -> RoleType -> RoleInstance -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives (Maybe Witness)
findWitness author subject storage property verb = do
  roots <- authorRoots author
  let queryAuthor = queryAuthorFor author roots
  filledByStorage <- filledRolesClosure [] [ storage ]
  nearContexts <- nub <<< join <$> traverse context_ filledByStorage
  near <- searchWitness queryAuthor subject storage property verb nearContexts (filledFromRoots roots) (const true)
  case near of
    Just w -> pure $ Just w
    Nothing -> do
      authorRoles <- filledRolesClosure [] roots
      contexts <- nub <<< join <$> traverse context_ authorRoles
      searchWitness queryAuthor subject storage property verb contexts (\actor -> pure $ elem actor authorRoles) (const true)

-- | Diagnostic information for a failed witness search.
witnessSearchDiagnostics :: PerspectivesUser -> MonadPerspectives String
witnessSearchDiagnostics author = do
  roots <- authorRoots author
  authorRoles <- filledRolesClosure [] roots
  contexts <- nub <<< join <$> traverse context_ authorRoles
  pure $ "Author identity found: "
    <> show (unwrap <$> roots)
    <> "; roles (transitively) filled by the author: "
    <> show (unwrap <$> authorRoles)
    <> "; their contexts: "
    <> show (unwrap <$> contexts)
    <> "."

-- | The sender side: find a witness for the first verb that has one and describe it as signed provenance.
findPropertyProvenance :: PerspectivesUser -> RoleType -> RoleInstance -> EnumeratedPropertyType -> Array PropertyVerb -> MonadPerspectives (Maybe PropertyProvenance)
findPropertyProvenance author subject storage property verbs = do
  mwitness <- findM (findWitness author subject storage property) verbs
  case mwitness of
    Nothing -> pure Nothing
    Just { actor, context, object } -> do
      subjectInstanceType <- roleType_ actor
      evaluationContextType <- contextType_ context
      objectInstanceType <- roleType_ object
      pure $ Just
        { subjectInstance: actor
        , subjectInstanceType
        , evaluationContext: context
        , evaluationContextType
        , objectInstance: object
        , objectInstanceType
        }

-- | The receiver side: check the signed witness directly.
-- | Left means: the witness could not be established with the evidence available now (missing or not
-- | yet received resources, or state that does not support it). It never permits.
checkPropertyProvenance :: PerspectivesUser -> RoleType -> RoleInstance -> EnumeratedPropertyType -> PropertyVerb -> PropertyProvenance -> MonadPerspectives (Either String Unit)
checkPropertyProvenance author subject storage property verb prov = do
  roots <- authorRoots author
  subjectExists <- entityExists prov.subjectInstance
  objectExists <- entityExists prov.objectInstance
  contextExists <- entityExists prov.evaluationContext
  if roots == [] then pure $ Left $ "the author " <> unwrap author <> " is not known"
  else if not subjectExists then pure $ Left $ "the subject instance " <> unwrap prov.subjectInstance <> " is not known"
  else if not objectExists then pure $ Left $ "the object instance " <> unwrap prov.objectInstance <> " is not known"
  else if not contextExists then pure $ Left $ "the evaluation context " <> unwrap prov.evaluationContext <> " is not known"
  else filledFromRoots roots prov.subjectInstance >>=
    if _ then do
      mwitness <- searchWitness
        (queryAuthorFor author roots)
        subject
        storage
        property
        verb
        [ prov.evaluationContext ]
        (\actor -> pure $ actor == prov.subjectInstance)
        (\{ object } -> object == prov.objectInstance)
      pure case mwitness of
        Just _ -> Right unit
        Nothing -> Left $ "from evaluation context " <> unwrap prov.evaluationContext <> ", subject instance "
          <> unwrap prov.subjectInstance
          <> " has no perspective object "
          <> unwrap prov.objectInstance
          <> " that resolves to "
          <> unwrap property
          <> " on "
          <> unwrap storage
    else pure $ Left $ "the subject instance " <> unwrap prov.subjectInstance <> " is not (transitively) filled by the author " <> unwrap author

-- | True iff the signed provenance contradicts the model, irrespective of instance state: the claimed
-- | evaluation context type cannot hold instances of the subject role. Such a delta can never be authorized
-- | by its own provenance.
refutedPropertyProvenance :: RoleType -> PropertyProvenance -> MonadPerspectives Boolean
refutedPropertyProvenance subject prov = do
  subjectContexts <- contextOfRepresentationOfRole subject
  not <$> equalsOrGeneralisesContextADT subjectContexts (ST prov.evaluationContextType)

-- Returns true iff the object's filler chain contains the storage instance for the given property.
resolvesToStorage :: RoleInstance -> EnumeratedPropertyType -> RoleInstance -> PropertyType -> MonadPerspectives Boolean
resolvesToStorage storage property object (ENP prop) = do
  stableProperty <- toStable prop
  resolved <- getPropertyBearingRoleInstance stableProperty object
  pure $ case resolved of
    Just (RoleProp target targetProperty) -> target == storage && targetProperty == property
    Nothing -> false
resolvesToStorage _ _ _ (CP _) = pure false

-- | All roles transitively filled by the given roles (including these).
filledRolesClosure :: Array RoleInstance -> Array RoleInstance -> MonadPerspectives (Array RoleInstance)
filledRolesClosure visited remaining = case uncons remaining of
  Nothing -> pure visited
  Just { head: role, tail } ->
    if elem role visited then filledRolesClosure visited tail
    else do
      candidates <- getAllFilledRoles_ role
      filled <- filterA (\candidate -> (_ == Just role) <$> binding_ candidate) candidates
      filledRolesClosure (visited <> [ role ]) (tail <> filled)

anyM :: forall a. (a -> MonadPerspectives Boolean) -> Array a -> MonadPerspectives Boolean
anyM predicate = foldM (\found value -> if found then pure true else predicate value) false

findM :: forall a b. (a -> MonadPerspectives (Maybe b)) -> Array a -> MonadPerspectives (Maybe b)
findM f = foldM (\found value -> if isJust found then pure found else f value) Nothing
