-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Perspectives.Sync.ConstructionHistory
  ( getContextConstructionHistory
  , getRoleBaselineHistory
  , getRoleConstructionHistory
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Persistence.DeltaStore (getDeltasForResource)
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.Persistent (getPerspectContext, getPerspectRol)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.ResourceIdentifiers (isInPublicScheme)
import Perspectives.Sync.ConstructionBaseline (ConstructionBaseline, selectContextConstruction, selectRoleBaseline)
import Perspectives.Sync.SignedDelta (SignedDelta)

getRoleBaselineHistory :: RoleInstance -> MonadPerspectives (Array DeltaStoreRecord)
getRoleBaselineHistory role = do
  records <- getDeltasForResource (unwrap role)
  baseline <- requireRoleBaseline role records
  pure $ filter
    (\(DeltaStoreRecord record) -> record.signedDelta == baseline.construction || record.signedDelta == baseline.membership)
    records

getContextConstructionHistory :: ContextInstance -> MonadPerspectives (Array DeltaStoreRecord)
getContextConstructionHistory context = do
  PerspectContext { buitenRol } <- getPerspectContext context
  externalRecords <- getDeltasForResource (unwrap buitenRol)
  baseline <- requireRoleBaseline buitenRol externalRecords
  contextRecords <- getDeltasForResource (unwrap context)
  constructor <- requireSelection $ selectContextConstruction baseline (signedRecords contextRecords)
  case constructor of
    Nothing -> throwError $ error $ "No compatible context construction for " <> show context
    Just signed -> pure $
      filter (\(DeltaStoreRecord record) -> record.signedDelta == signed) contextRecords <>
        filter
          (\(DeltaStoreRecord record) -> record.signedDelta == baseline.construction || record.signedDelta == baseline.membership)
          externalRecords

getRoleConstructionHistory :: RoleInstance -> MonadPerspectives (Array DeltaStoreRecord)
getRoleConstructionHistory role = do
  PerspectRol { context } <- getPerspectRol role
  if isInPublicScheme (unwrap context) then getRoleBaselineHistory role
  else do
    contextRecords <- getContextConstructionHistory context
    PerspectContext { buitenRol } <- getPerspectContext context
    if role == buitenRol then pure contextRecords
    else (contextRecords <> _) <$> getRoleBaselineHistory role

requireRoleBaseline :: RoleInstance -> Array DeltaStoreRecord -> MonadPerspectives ConstructionBaseline
requireRoleBaseline role records = do
  PerspectRol { context, pspType } <- getPerspectRol role
  selected <- requireSelection $ selectRoleBaseline role pspType context (signedRecords records)
  case selected of
    Nothing -> throwError $ error $ "No valid construction baseline for " <> show role
    Just baseline -> pure baseline

signedRecords :: Array DeltaStoreRecord -> Array SignedDelta
signedRecords = map (\(DeltaStoreRecord record) -> record.signedDelta)

requireSelection :: forall a. Either String a -> MonadPerspectives a
requireSelection = case _ of
  Left message -> throwError $ error message
  Right result -> pure result
