-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Perspectives.Persistence.CausalStore where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Persistence.API (addDocument_, tryGetDocument_)
import Perspectives.Persistence.DeltaStore (extractDeltaInfo, safeKey)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.PerspectivesState (getPerspectivesUser)
import Perspectives.ResourceIdentifiers (createCuid)
import Perspectives.Sync.Causal (Clock, CausalStamp, causalRoleKey, covers, coversClock, currentCausalEpoch, deltaCausalClock, isRoleDeletion, mergeClock, readCausalStamp, stampClock)
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)
import Simple.JSON (class ReadForeign, class WriteForeign, writeJSON)

newtype CausalRoleDoc = CausalRoleDoc
  { _id :: String
  , _rev :: Maybe String
  , clock :: Clock
  , deletions :: Array CausalStamp
  }

derive newtype instance ReadForeign CausalRoleDoc
derive newtype instance WriteForeign CausalRoleDoc

newtype CausalInstallationDoc = CausalInstallationDoc
  { _id :: String
  , _rev :: Maybe String
  , epoch :: String
  , installation :: String
  }

derive newtype instance ReadForeign CausalInstallationDoc
derive newtype instance WriteForeign CausalInstallationDoc

causalDatabaseName :: MonadPerspectives String
causalDatabaseName = (_ <> "_causal") <$> getSystemIdentifier

ensureCausalEpoch :: MonadPerspectives String
ensureCausalEpoch = do
  db <- causalDatabaseName
  existing <- tryGetDocument_ db "installation"
  case existing of
    Just (CausalInstallationDoc doc) ->
      if doc.epoch == currentCausalEpoch then pure doc.installation
      else throwError $ error $ "Unsupported persisted causal epoch: " <> doc.epoch
    Nothing -> do
      installation <- createCuid
      void $ addDocument_ db
        (CausalInstallationDoc { _id: "installation", _rev: Nothing, epoch: currentCausalEpoch, installation })
        "installation"
      pure installation

getCausalRole :: String -> MonadPerspectives CausalRoleDoc
getCausalRole role = do
  db <- causalDatabaseName
  let id = "role:" <> safeKey role
  existing <- tryGetDocument_ db id
  pure case existing of
    Just doc -> doc
    Nothing -> CausalRoleDoc { _id: id, _rev: Nothing, clock: [], deletions: [] }

saveCausalRole :: CausalRoleDoc -> MonadPerspectives Unit
saveCausalRole doc@(CausalRoleDoc { _id }) = do
  db <- causalDatabaseName
  void $ addDocument_ db doc _id

reserveCausalStamp :: String -> MonadPerspectives CausalStamp
reserveCausalStamp role = do
  installation <- ensureCausalEpoch
  author <- unwrap <<< unschemePerspectivesUser <$> getPerspectivesUser
  let actor = writeJSON [ author, installation ]
  CausalRoleDoc doc <- getCausalRole role
  let
    previous = case Array.find (_.actor >>> (_ == actor)) doc.clock of
      Nothing -> 0
      Just known -> known.counter
  when (previous == top) $ throwError $ error "Causal counter exhausted"
  let stamp = { epoch: currentCausalEpoch, dot: { actor, counter: previous + 1 }, context: doc.clock }
  saveCausalRole $ CausalRoleDoc doc { clock = stampClock stamp }
  pure stamp

recordCausalDelta :: SignedDelta -> MonadPerspectives Unit
recordCausalDelta signed@(SignedDelta { encryptedDelta }) = do
  role <- eitherError $ causalRoleKey encryptedDelta
  case role of
    Nothing -> pure unit
    Just key -> do
      void ensureCausalEpoch
      summary <- eitherError $ deltaCausalClock signed
      stamp <- eitherError $ readCausalStamp encryptedDelta
      CausalRoleDoc doc <- getCausalRole key
      let
        clock = mergeClock doc.clock summary
        deletion = case extractDeltaInfo encryptedDelta, stamp of
          Just info, Just causal | isRoleDeletion info.deltaType -> Just causal
          _, _ -> Nothing
        deletions = case deletion of
          Nothing -> doc.deletions
          Just causal ->
            if Array.any (\old -> covers (stampClock old) causal.dot) doc.deletions then doc.deletions
            else Array.filter (\old -> not $ covers causal.context old.dot) doc.deletions <> [ causal ]
      when (clock /= doc.clock || deletions /= doc.deletions)
        $ saveCausalRole
        $ CausalRoleDoc doc { clock = clock, deletions = deletions }

observedByDeletion :: SignedDelta -> SignedDelta -> MonadPerspectives Boolean
observedByDeletion deletion modification = do
  stamp <- eitherError $ readCausalStamp (unwrap deletion).encryptedDelta
  summary <- eitherError $ deltaCausalClock modification
  pure case stamp of
    Nothing -> false
    Just causal -> coversClock causal.context summary

coveredByDeletion :: SignedDelta -> MonadPerspectives Boolean
coveredByDeletion signed@(SignedDelta { encryptedDelta }) = do
  role <- eitherError $ causalRoleKey encryptedDelta
  summary <- eitherError $ deltaCausalClock signed
  case role of
    Nothing -> pure false
    Just key -> do
      CausalRoleDoc doc <- getCausalRole key
      pure $ Array.any (\deletion -> coversClock deletion.context summary) doc.deletions

eitherError :: forall a. Either String a -> MonadPerspectives a
eitherError = case _ of
  Left message -> throwError $ error message
  Right value -> pure value
