-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Persistence.PropertyHistoryStore
  ( retainPropertyHistory
  , storeLocalDeltaWithHistory
  , storeIncomingDeltaWithHistory
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import Perspectives.Authenticate (signDeltaMP)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Persistence.DeltaStore (deltaStoreDocIdWithDeltaId, extractDeltaInfo, getDeltasForResource, pruneDeltasThrough, safeKey, storeDelta, storeDeltaFromSignedDelta)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Representation.Class.Property (enumeratedPropertyRetainsFullHistory)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..))
import Perspectives.Sync.CanonicalJson (computeDeltaId)
import Perspectives.Sync.PropertyHistory (compactPropertyHistory, isCheckpoint, isIdentityProperty, isPropertyOperation, propertyTypeName, readPropertyChange)
import Perspectives.Sync.SignedDelta (SignedDelta(..))

storeLocalDeltaWithHistory :: SignedDelta -> MonadPerspectives Unit
storeLocalDeltaWithHistory delta = do
  storeDeltaFromSignedDelta delta
  retainPropertyHistory delta

storeIncomingDeltaWithHistory :: DeltaStoreRecord -> MonadPerspectives Unit
storeIncomingDeltaWithHistory record@(DeltaStoreRecord r) = do
  bounded <- hasBoundedPropertyHistory r.signedDelta
  existing <- getDeltasForResource r.resourceKey
  let
    floor = foldl
      ( \v (DeltaStoreRecord old) ->
          if isCheckpoint (unwrap old.signedDelta).encryptedDelta then max v old.resourceVersion else v
      )
      0
      existing
  -- Keep the cutoff durable, including across restart and replayed old packets.
  unless (bounded && r.resourceVersion < floor && r.deltaType /= "UploadFile") do
    storeDelta $ DeltaStoreRecord r { resourceKey = safeKey r.resourceKey }
    retainPropertyHistory r.signedDelta

hasBoundedPropertyHistory :: SignedDelta -> MonadPerspectives Boolean
hasBoundedPropertyHistory signed@(SignedDelta { encryptedDelta }) = case extractDeltaInfo encryptedDelta of
  Just info | isPropertyOperation info.deltaType -> case readPropertyChange signed of
    Left message -> throwError $ error message
    Right change -> do
      if isIdentityProperty change.property then pure false
      else not <$> enumeratedPropertyRetainsFullHistory (EnumeratedPropertyType $ propertyTypeName change.property)
  _ -> pure false

retainPropertyHistory :: SignedDelta -> MonadPerspectives Unit
retainPropertyHistory signed@(SignedDelta { encryptedDelta }) =
  if isCheckpoint encryptedDelta then pure unit
  else case extractDeltaInfo encryptedDelta of
    Just info | isPropertyOperation info.deltaType -> case readPropertyChange signed of
      Left message -> throwError $ error message
      Right change -> do
        bounded <- hasBoundedPropertyHistory signed
        when bounded do
          records <- getDeltasForResource change.resourceKey
          case compactPropertyHistory records of
            Left message -> throwError $ error message
            Right Nothing -> do
              let
                baselines = Array.sortBy (\(DeltaStoreRecord a) (DeltaStoreRecord b) -> compare a.resourceVersion b.resourceVersion)
                  $ Array.filter (\(DeltaStoreRecord r) -> isCheckpoint (unwrap r.signedDelta).encryptedDelta) records
              case Array.last baselines of
                Just (DeltaStoreRecord baseline) ->
                  when (Array.any (\(DeltaStoreRecord r) -> r.resourceVersion <= baseline.resourceVersion && r._id /= baseline._id) records)
                    $ pruneDeltasThrough change.resourceKey baseline.resourceVersion baseline._id
                Nothing -> pure unit
            Right (Just baseline) -> do
              signedDelta <- signDeltaMP baseline.json
              let author = (unwrap signedDelta).author
              deltaId <- liftAff $ computeDeltaId (unwrap author) (unwrap signedDelta).encryptedDelta
              let
                id = deltaStoreDocIdWithDeltaId change.resourceKey baseline.version author deltaId
                record = DeltaStoreRecord
                  { _id: id
                  , _rev: Nothing
                  , resourceKey: safeKey change.resourceKey
                  , resourceVersion: baseline.version
                  , author
                  , deltaId: Just deltaId
                  , signedDelta
                  , deltaType: "PropertyCheckpoint"
                  , applied: true
                  , disposition: Just "Checkpoint"
                  , contextKey: Nothing
                  }
              storeDelta record
              pruneDeltasThrough change.resourceKey baseline.version id
    _ -> pure unit
