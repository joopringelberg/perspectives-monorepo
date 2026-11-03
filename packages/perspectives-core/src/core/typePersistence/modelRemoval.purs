-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

-- | The final step of uninstalling a model: removing its DomeinFile from the local models database,
-- | together with everything the model contributed to other models and to the runtime caches.
-- | This is run at the very end of the transaction in which the model was scheduled for removal,
-- | so that the model's types (and its inverted queries) were available while its instances were removed.

module Perspectives.ModelRemoval (removeModelFromLocalDatabase) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.State (execState)
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Map (filter) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.Assignment.StateCache (clearModelStates)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (storeDomeinFileInCouchdbPreservingAttachments)
import Perspectives.DomeinFile (DomeinFile(..), removeDownStreamAutomaticEffect, removeDownStreamNotification)
import Perspectives.Error.Boundaries (handleDomeinFileError)
import Perspectives.Identifiers (unversionedModelUri)
import Perspectives.InvertedQuery.Storable (removeInvertedQueriesContributedByModel)
import Perspectives.Persistent (getDomeinFile, tryGetPerspectEntiteit, tryRemoveEntiteit)
import Perspectives.PerspectivesState (clearQueryCache, conversationCacheDelete, getModelUris, removeTranslationTable, setModelUris)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Stable)

-- | Removes the DomeinFile (and its attachments) from the local models database. Also removes:
-- |  * the downstream state notifications and automatic effects the model added to other models;
-- |  * the inverted queries the model contributed to the local inverted queries database;
-- | and clears the caches that may hold compiled queries, states or translations of the model.
-- | Does nothing if the model is not available locally.
-- | Finally removes the model from the map of installed Readable to Stable model URIs.
removeModelFromLocalDatabase :: ModelUri Stable -> MonadPerspectives Unit
removeModelFromLocalDatabase (ModelUri modelUri) = do
  let unversionedModel = ModelUri (unversionedModelUri modelUri) :: ModelUri Stable
  (mdf :: Maybe (DomeinFile Stable)) <- tryGetPerspectEntiteit unversionedModel
  case mdf of
    Nothing -> pure unit
    Just (DomeinFile { upstreamStateNotifications, upstreamAutomaticEffects }) -> do
      forWithIndex_ upstreamStateNotifications
        \domainName notifications ->
          (try $ getDomeinFile (ModelUri domainName)) >>=
            handleDomeinFileError "removeModelFromLocalDatabase"
              \(DomeinFile dfr) -> storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ notifications removeDownStreamNotification) dfr)
      forWithIndex_ upstreamAutomaticEffects
        \domainName automaticEffects ->
          (try $ getDomeinFile (ModelUri domainName)) >>=
            handleDomeinFileError "removeModelFromLocalDatabase"
              \(DomeinFile dfr) -> storeDomeinFileInCouchdbPreservingAttachments (DomeinFile $ execState (for_ automaticEffects removeDownStreamAutomaticEffect) dfr)
      removeInvertedQueriesContributedByModel unversionedModel
      -- Compiled inverted queries of the model may be cached under keys of types of other models.
      clearQueryCache
      void $ pure $ clearModelStates unversionedModel
      removeTranslationTable (unwrap unversionedModel)
      conversationCacheDelete (unwrap unversionedModel)
      tryRemoveEntiteit unversionedModel
      getModelUris >>= setModelUris <<< Map.filter (\(ModelUri mu) -> unversionedModelUri mu /= unwrap unversionedModel)
