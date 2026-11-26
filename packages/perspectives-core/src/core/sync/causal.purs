-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Perspectives.Sync.Causal where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (all, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Perspectives.Sync.SignedDelta (SignedDelta(..))
import Perspectives.UnschemedIdentifiers (unschemePerspectivesUser)
import Simple.JSON (readJSON')

currentCausalEpoch :: String
currentCausalEpoch = "perspectives-causal-1"

type Dot = { actor :: String, counter :: Int }
type Clock = Array Dot
type CausalStamp = { epoch :: String, dot :: Dot, context :: Clock }

mergeClock :: Clock -> Clock -> Clock
mergeClock left right = map (\(Tuple actor counter) -> { actor, counter })
  (Map.toUnfoldable $ foldl (\m d -> Map.insertWith max d.actor d.counter m) Map.empty (left <> right) :: Array (Tuple String Int))

covers :: Clock -> Dot -> Boolean
covers clock dot = Array.any (\known -> known.actor == dot.actor && known.counter >= dot.counter) clock

coversClock :: Clock -> Clock -> Boolean
coversClock clock = all (covers clock)

stampClock :: CausalStamp -> Clock
stampClock stamp = mergeClock stamp.context [ stamp.dot ]

readCausalStamp :: String -> Either String (Maybe CausalStamp)
readCausalStamp json = case runExcept $ readJSON' json of
  Left errors -> Left $ "Invalid causal metadata: " <> show errors
  Right (r :: { causal :: Maybe CausalStamp }) -> Right r.causal

causalRoleKey :: String -> Either String (Maybe String)
causalRoleKey json = case runExcept $ readJSON' json of
  Left errors -> Left $ "Invalid causal role: " <> show errors
  Right (r :: { roleInstance :: Maybe String, filled :: Maybe String, property :: Maybe String, id :: Maybe String }) ->
    Right $ r.roleInstance <|> r.filled <|> (if isJust r.property then r.id else Nothing)

deltaCausalClock :: SignedDelta -> Either String Clock
deltaCausalClock (SignedDelta { encryptedDelta }) = do
  stamp <- readCausalStamp encryptedDelta
  case runExcept $ readJSON' encryptedDelta of
    Left errors -> Left $ "Invalid checkpoint causal summary: " <> show errors
    Right (r :: { historyCheckpoint :: Maybe { causalContext :: Maybe Clock } }) ->
      Right $ mergeClock
        (maybeClock stamp)
        ( case r.historyCheckpoint of
            Just marker -> fromMaybe [] marker.causalContext
            Nothing -> []
        )
  where
  maybeClock Nothing = []
  maybeClock (Just stamp) = stampClock stamp

validateClock :: Clock -> Either String Unit
validateClock clock =
  if
    Array.length (Array.nub $ map _.actor clock) /= Array.length clock
      || not (all (\d -> d.counter > 0 && validActor d.actor) clock) then
    Left "Invalid causal clock"
  else Right unit
  where
  validActor actor = case runExcept $ readJSON' actor of
    Right (parts :: Array String) -> Array.length parts == 2 && all (_ /= "") parts
    Left _ -> false

validateCausalDelta :: SignedDelta -> Either String Unit
validateCausalDelta signed@(SignedDelta { encryptedDelta, author }) = do
  stamp <- readCausalStamp encryptedDelta
  role <- causalRoleKey encryptedDelta
  marker <- case runExcept $ readJSON' encryptedDelta of
    Left errors -> Left $ "Invalid causal format marker: " <> show errors
    Right (r :: { deltaFormatVersion :: Maybe Int, historyCheckpoint :: Maybe { causalContext :: Maybe Clock } }) -> Right r
  case stamp of
    Nothing ->
      -- Every delta of the current format (5, see VersionedDelta.currentDeltaFormatVersion) that mutates a role is causally stamped.
      if marker.deltaFormatVersion == Just 5 && isJust role then Left "A causal role mutation requires a causal stamp"
      else pure unit
    Just causal -> do
      validateClock causal.context
      validateClock [ causal.dot ]
      if causal.epoch /= currentCausalEpoch || covers causal.context causal.dot then Left "Invalid causal mutation epoch or counter"
      else pure unit
      case marker.historyCheckpoint of
        Just _ -> pure unit
        Nothing -> case runExcept $ readJSON' causal.dot.actor of
          Right (parts :: Array String) | Array.head parts == Just (unwrap $ unschemePerspectivesUser author) -> pure unit
          _ -> Left "Causal actor does not belong to the signed author"
  summary <- deltaCausalClock signed
  validateClock summary

isRoleDeletion :: String -> Boolean
isRoleDeletion operation = Array.elem operation
  [ "RemoveRoleInstance", "RemoveExternalRoleInstance", "RemoveUnboundExternalRoleInstance", "RetractRoleInstance" ]
