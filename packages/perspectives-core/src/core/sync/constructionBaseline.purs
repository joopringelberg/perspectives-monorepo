-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Perspectives.Sync.ConstructionBaseline where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (any, catMaybes, filter, head, nub, nubByEq, null, sortBy)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Perspectives.Persistence.DeltaStore (safeKey)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType)
import Perspectives.Sync.Causal (CausalStamp, coversClock, deltaCausalClock, readCausalStamp)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.VersionedDelta (DeltaEnvelope(..), parseIncomingDelta)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), UniverseContextDelta(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)
import Simple.JSON (readJSON')

type ConstructionBaseline =
  { construction :: SignedDelta
  , membership :: SignedDelta
  , role :: UniverseRoleDelta
  , version :: Int
  , causal :: Maybe CausalStamp
  }

sameResource :: String -> String -> Boolean
sameResource left right = safeKey left == safeKey right

sameSignedContent :: SignedDelta -> SignedDelta -> Boolean
sameSignedContent (SignedDelta left) (SignedDelta right) =
  unschemePerspectivesUser left.author == unschemePerspectivesUser right.author
    && left.encryptedDelta == right.encryptedDelta

causallyBefore :: SignedDelta -> SignedDelta -> Either String Boolean
causallyBefore earlier later = do
  earlierStamp <- readCausalStamp (unwrap earlier).encryptedDelta
  laterStamp <- readCausalStamp (unwrap later).encryptedDelta
  summary <- deltaCausalClock earlier
  pure case earlierStamp, laterStamp of
    Just _, Just stamp -> coversClock stamp.context summary
    _, _ -> false

sameContextTypeReference :: SignedDelta -> SignedDelta -> Boolean
sameContextTypeReference left right =
  case
    runExcept $ readJSON' (unwrap left).encryptedDelta,
    runExcept $ readJSON' (unwrap right).encryptedDelta
    of
    Right (a :: { contextType :: String }), Right (b :: { contextType :: String }) -> a.contextType == b.contextType
    _, _ -> false

latestConstructionEvidence :: String -> Array SignedDelta -> Either String (Maybe SignedDelta)
latestConstructionEvidence description history = do
  let distinct = nubByEq sameSignedContent history
  stamped <- traverse (\signed -> { signed, causal: _ } <$> readCausalStamp (unwrap signed).encryptedDelta) distinct
  let
    causalRecords = filter (isJust <<< _.causal) stamped
    candidates = if null causalRecords then distinct else map _.signed causalRecords
  maximal <- traverse
    ( \candidate -> do
        superseded <- traverse (causallyBefore candidate) candidates
        pure if any identity superseded then Nothing else Just candidate
    )
    candidates
  case filter isJust maximal of
    [] -> Right Nothing
    [ selected ] -> Right selected
    _ -> Left $ "Ambiguous construction baseline for " <> description

selectRoleConstruction
  :: RoleInstance
  -> EnumeratedRoleType
  -> ContextInstance
  -> Array SignedDelta
  -> Either String (Maybe SignedDelta)
selectRoleConstruction role roleType context history =
  latestConstructionEvidence (unwrap role) $ filter matches history
  where
  matches signed = case parseIncomingDelta (unwrap signed).encryptedDelta of
    Right (UniverseRoleEnvelope (UniverseRoleDelta delta)) ->
      (delta.deltaType == ConstructEmptyRole || delta.deltaType == ConstructExternalRole)
        && sameResource (unwrap delta.roleInstance) (unwrap role)
        && sameResource (unwrap delta.id) (unwrap context)
        && delta.roleType == roleType
    _ -> false

selectRoleBaseline
  :: RoleInstance
  -> EnumeratedRoleType
  -> ContextInstance
  -> Array SignedDelta
  -> Either String (Maybe ConstructionBaseline)
selectRoleBaseline role roleType context history = do
  construction <- selectRoleConstruction role roleType context history
  case construction of
    Nothing -> Right Nothing
    Just signed -> case parseIncomingDelta (unwrap signed).encryptedDelta of
      Right (UniverseRoleEnvelope delta@(UniverseRoleDelta creation)) -> do
        causal <- readCausalStamp (unwrap signed).encryptedDelta
        let
          memberships = filter
            ( \candidate -> case parseIncomingDelta (unwrap candidate).encryptedDelta of
                Right (ContextEnvelope (ContextDelta membership)) ->
                  membership.deltaType ==
                    (if creation.deltaType == ConstructExternalRole then AddExternalRole else AddRoleInstancesToContext)
                    && sameResource (unwrap membership.roleInstance) (unwrap role)
                    && sameResource (unwrap membership.contextInstance) (unwrap context)
                    && membership.contextType == creation.contextType
                    && sameContextTypeReference signed candidate
                    && membership.roleType == roleType
                    &&
                      ( creation.deltaType /= ConstructExternalRole ||
                          ( membership.subject == creation.subject &&
                              unschemePerspectivesUser (unwrap candidate).author == unschemePerspectivesUser (unwrap signed).author
                          )
                      )
                _ -> false
            )
            history
        linked <- traverse
          ( \candidate -> do
              follows <- causallyBefore signed candidate
              pure if follows || not (isJust causal) then Just candidate else Nothing
          )
          memberships
        membership <- latestConstructionEvidence (unwrap role <> " membership") $ catMaybes linked
        case membership of
          Nothing -> Right Nothing
          Just member -> case parseIncomingDelta (unwrap member).encryptedDelta of
            Right (ContextEnvelope (ContextDelta link)) ->
              Right $ Just { construction: signed, membership: member, role: delta, version: link.resourceVersion, causal }
            _ -> Left "Invalid construction membership"
      _ -> Left "Invalid role construction"

selectContextConstruction :: ConstructionBaseline -> Array SignedDelta -> Either String (Maybe SignedDelta)
selectContextConstruction baseline history =
  Right $ head $ sortBy
    (\left right -> compare (unwrap left).encryptedDelta (unwrap right).encryptedDelta)
    (filter matches history)
  where
  UniverseRoleDelta creation = baseline.role
  matches signed = case parseIncomingDelta (unwrap signed).encryptedDelta of
    Right (UniverseContextEnvelope (UniverseContextDelta delta)) ->
      sameResource (unwrap delta.id) (unwrap creation.id)
        && delta.contextType == creation.contextType
        && delta.subject == creation.subject
        && sameContextTypeReference signed baseline.construction
        && unschemePerspectivesUser (unwrap signed).author == unschemePerspectivesUser (unwrap baseline.construction).author
    _ -> false

inferConstructionBaselines :: Array SignedDelta -> Either String (Array ConstructionBaseline)
inferConstructionBaselines history = do
  let
    constructions = filter
      ( \signed -> case parseIncomingDelta (unwrap signed).encryptedDelta of
          Right (UniverseRoleEnvelope (UniverseRoleDelta delta)) ->
            delta.deltaType == ConstructEmptyRole || delta.deltaType == ConstructExternalRole
          _ -> false
      )
      history
    keys = nub $ map
      ( \signed -> case parseIncomingDelta (unwrap signed).encryptedDelta of
          Right (UniverseRoleEnvelope (UniverseRoleDelta delta)) -> safeKey (unwrap delta.roleInstance)
          _ -> ""
      )
      constructions
  catMaybes <$> traverse
    ( \key -> do
        latest <- latestConstructionEvidence key $ filter
          ( \signed -> case parseIncomingDelta (unwrap signed).encryptedDelta of
              Right (UniverseRoleEnvelope (UniverseRoleDelta delta)) -> safeKey (unwrap delta.roleInstance) == key
              _ -> false
          )
          constructions
        case latest of
          Just signed -> case parseIncomingDelta (unwrap signed).encryptedDelta of
            Right (UniverseRoleEnvelope (UniverseRoleDelta delta)) ->
              selectRoleBaseline delta.roleInstance delta.roleType delta.id history
            _ -> Left "Invalid inferred construction"
          Nothing -> Right Nothing
    )
    keys
