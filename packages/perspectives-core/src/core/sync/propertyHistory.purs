-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
-- END LICENSE

module Perspectives.Sync.PropertyHistory where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (foldM, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Traversable (traverse)
import Foreign (Foreign)
import Foreign.Object as Object
import Perspectives.Persistence.DeltaStoreTypes (DeltaStoreRecord(..))
import Perspectives.ModelDependencies (perspectivesUsersPublicKey, perspectivesUsersTransportPublicKey)
import Perspectives.Representation.InstanceIdentifiers (Value)
import Perspectives.Representation.TypeIdentifiers (RoleType(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.Sync.Causal (Clock, coversClock, deltaCausalClock, mergeClock, readCausalStamp, validateClock)
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)
import Simple.JSON (readJSON', write, writeJSON)

defaultPropertyHistoryDepth :: Int
defaultPropertyHistoryDepth = 10

type Checkpoint =
  { beforeValues :: Array Value
  , winner :: SignedDelta
  , causalContext :: Maybe Clock
  }

type PropertyChange =
  { resourceKey :: String
  , resourceVersion :: Int
  , id :: String
  , property :: String
  , roleType :: String
  , deltaType :: String
  , values :: Array Value
  }

type HistoryState =
  { beforeValues :: Array Value
  , values :: Array Value
  , changes :: Array SignedDelta
  }

readCheckpoint :: String -> Either String (Maybe Checkpoint)
readCheckpoint json = case runExcept $ readJSON' json of
  Left errors -> Left $ "Invalid property checkpoint: " <> show errors
  Right (r :: { historyCheckpoint :: Maybe Checkpoint }) -> Right r.historyCheckpoint

checkpoint :: String -> Maybe Checkpoint
checkpoint json = case readCheckpoint json of
  Right marker -> marker
  Left _ -> Nothing

isCheckpoint :: String -> Boolean
isCheckpoint json = case checkpoint json of
  Just _ -> true
  Nothing -> false

readPropertyChange :: SignedDelta -> Either String PropertyChange
readPropertyChange (SignedDelta { encryptedDelta }) = case runExcept $ readJSON' encryptedDelta of
  Left errors -> Left $ "Invalid stored property delta: " <> show errors
  Right r -> Right r

isPropertyOperation :: String -> Boolean
isPropertyOperation operation = Array.elem operation
  [ "SetProperty", "AddProperty", "RemoveProperty", "DeleteProperty", "UploadFile", "PropertyCheckpoint" ]

propertyTypeName :: String -> String
propertyTypeName property = case String.lastIndexOf (String.Pattern "@") property of
  Just at -> String.take at property
  Nothing -> property

isIdentityProperty :: String -> Boolean
isIdentityProperty property = Array.elem (propertyTypeName property)
  [ perspectivesUsersPublicKey, perspectivesUsersTransportPublicKey ]

applyPropertyChange :: Array Value -> PropertyChange -> Either String (Array Value)
applyPropertyChange values change = case change.deltaType of
  "SetProperty" -> Right change.values
  "AddProperty" -> Right $ Array.union values change.values
  "RemoveProperty" -> Right $ Array.difference values change.values
  "DeleteProperty" -> Right []
  "UploadFile" -> Right values
  other -> Left $ "Cannot reconstruct property operation " <> other

-- The original winner is retained inside the baseline, including its signature.
-- This also allows a concurrent winner at the checkpoint boundary to replace it.
propertyHistory :: Array DeltaStoreRecord -> Either String HistoryState
propertyHistory records = do
  let applied = Array.filter (\(DeltaStoreRecord r) -> r.applied) records
  entries <- traverse decode applied
  let
    baselines = Array.sortBy (\a b -> compare a.change.resourceVersion b.change.resourceVersion) $
      Array.filter (\r -> isCheckpoint (unwrap r.signed).encryptedDelta) entries
    baseline = Array.last baselines
    startingValues = case baseline of
      Just b -> case checkpoint (unwrap b.signed).encryptedDelta of
        Just marker -> marker.beforeValues
        Nothing -> []
      Nothing -> []
    startingVersion = case baseline of
      Just b -> b.change.resourceVersion
      Nothing -> 0
  seeded <- case baseline of
    Just b -> case checkpoint (unwrap b.signed).encryptedDelta of
      Just marker -> do
        change <- readPropertyChange marker.winner
        pure [ { signed: marker.winner, change } ]
      Nothing -> pure []
    Nothing -> pure []
  let
    ordinary = Array.filter
      (\r -> not (isCheckpoint (unwrap r.signed).encryptedDelta) && r.change.resourceVersion >= startingVersion)
      entries
    winners = Map.values $ foldl
      (\acc entry -> Map.insertWith chooseWinner entry.change.resourceVersion entry acc)
      Map.empty
      (seeded <> ordinary)
    ordered = Array.fromFoldable winners
  values <- foldM (\current entry -> applyPropertyChange current entry.change) startingValues ordered
  pure { beforeValues: startingValues, values, changes: _.signed <$> ordered }
  where
  decode (DeltaStoreRecord r) = do
    change <- readPropertyChange r.signedDelta
    pure { signed: r.signedDelta, change }
  chooseWinner left right =
    if unschemePerspectivesUser (unwrap left.signed).author >= unschemePerspectivesUser (unwrap right.signed).author then left
    else right

-- Return a baseline immediately before the ten retained logical versions.
-- Several conflicting records at one version count as one history position.
compactPropertyHistory :: Array DeltaStoreRecord -> Either String (Maybe { json :: String, version :: Int })
compactPropertyHistory records = do
  let
    floorVersion = foldl
      ( \v (DeltaStoreRecord r) ->
          if isCheckpoint (unwrap r.signedDelta).encryptedDelta then max v r.resourceVersion else v
      )
      0
      records
    ordinary = Array.filter
      ( \(DeltaStoreRecord r) ->
          r.resourceVersion > floorVersion && not (isCheckpoint (unwrap r.signedDelta).encryptedDelta)
      )
      records
    versions = Array.sort $ Array.nub $ map (\(DeltaStoreRecord r) -> r.resourceVersion) ordinary
  case Array.index versions (Array.length versions - defaultPropertyHistoryDepth - 1) of
    Nothing -> pure Nothing
    Just cutoff -> do
      let prefix = Array.filter (\(DeltaStoreRecord r) -> r.resourceVersion <= cutoff) records
      resolved <- propertyHistory prefix
      winner <- note "Cannot compact property history without an applied winner" (Array.last resolved.changes)
      winningChange <- readPropertyChange winner
      -- Suppressed/outdated records cannot move a checkpoint beyond applied state.
      if winningChange.resourceVersion /= cutoff then pure Nothing
      else do
        prior <- propertyHistory $ Array.filter (\(DeltaStoreRecord r) -> r.resourceVersion < cutoff) prefix
        source <- case runExcept $ readJSON' (unwrap winner).encryptedDelta of
          Left errors -> Left $ "Cannot encode property checkpoint: " <> show errors
          Right (object :: Object.Object Foreign) -> Right object
        normalised <- case Object.lookup "subjectKind" source of
          Just _ -> Right source
          Nothing -> case runExcept $ readJSON' (unwrap winner).encryptedDelta of
            Left errors -> Left $ "Cannot encode checkpoint subject: " <> show errors
            Right (legacy :: { subject :: RoleType }) ->
              let
                subject = case legacy.subject of
                  ENR role -> { reference: unwrap role, kind: "ENR" }
                  CR role -> { reference: unwrap role, kind: "CR" }
              in
                Right $ Object.insert "subject" (write subject.reference)
                  $ Object.insert "subjectKind" (write subject.kind) source
        summaries <- traverse (\(DeltaStoreRecord r) -> deltaCausalClock r.signedDelta)
          $ Array.filter (\(DeltaStoreRecord r) -> r.applied) prefix
        let
          marker = { beforeValues: prior.values, winner, causalContext: Just $ foldl mergeClock [] summaries }
          payload = Object.insert "deltaFormatVersion" (write 3)
            $ Object.insert "deltaType" (write "SetProperty")
            $ Object.insert "values" (write resolved.values)
            $ Object.insert "historyCheckpoint" (write marker) normalised
        pure $ Just { json: writeJSON payload, version: cutoff }

validateCheckpoint :: SignedDelta -> Either String Unit
validateCheckpoint signed@(SignedDelta { encryptedDelta }) = do
  marker <- readCheckpoint encryptedDelta >>= note "Missing property checkpoint marker"
  baseline <- readPropertyChange signed
  original <- readPropertyChange marker.winner
  originalStamp <- readCausalStamp (unwrap marker.winner).encryptedDelta
  originalClock <- deltaCausalClock marker.winner
  case marker.causalContext, originalStamp of
    Nothing, Just _ -> Left "Causal checkpoints require a causal summary"
    Just summary, _ -> do
      validateClock summary
      if coversClock summary originalClock then pure unit else Left "Checkpoint omits its winner's causal history"
    _, _ -> pure unit
  format <- case runExcept $ readJSON' encryptedDelta of
    Left errors -> Left $ "Invalid checkpoint format: " <> show errors
    Right (r :: { deltaFormatVersion :: Int }) -> Right r.deltaFormatVersion
  if format /= 3 then Left "Property checkpoints require delta format version 3"
  else if isIdentityProperty baseline.property then Left "Identity keys cannot be certified by a property checkpoint"
  else if isCheckpoint (unwrap marker.winner).encryptedDelta then Left "Nested property checkpoints are not allowed"
  else if
    baseline.resourceKey /= original.resourceKey
      || baseline.resourceVersion /= original.resourceVersion
      || baseline.id /= original.id
      || baseline.property /= original.property
      || baseline.roleType /= original.roleType
      || baseline.resourceVersion < 1
      || baseline.deltaType /= "SetProperty" then Left "Property checkpoint does not match its signed winner"
  else Right unit
