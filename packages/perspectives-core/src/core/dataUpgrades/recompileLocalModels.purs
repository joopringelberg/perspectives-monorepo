-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

module Perspectives.DataUpgrade.RecompileLocalModels where

import Control.Alt (void)
import Control.Monad.Except (runExceptT)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Main.RecompileBasicModels (UninterpretedDomeinFile, executeInTopologicalOrder, recompileModel)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (lookupStableModelUri_)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.Identifiers (modelUri2ModelUrl)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistence.API (databaseInfo, deleteDatabase, documentsInDatabase, getDocument_, includeDocs)
import Perspectives.Persistent (invertedQueryDatabaseName, saveMarkedResources)
import Perspectives.PerspectivesState (modelsDatabaseName, pushMessage, removeMessage)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.SetupUser (setupInvertedQueryDatabase)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Readable)
import Prelude (Unit, bind, discard, pure, show, ($), (*>), (<$>), (<>))
import Simple.JSON (read) as JSON

recompileLocalModels :: MonadPerspectives Boolean
recompileLocalModels =
  do
    addAllExternalFunctions
    modelsDb <- modelsDatabaseName
    { rows: allModels } <- documentsInDatabase modelsDb includeDocs
    -- As doc is still uninterpreted, we can only rely on the rows.id member of the PouchdbAllDocs record. These, however, are DomeinFileIdentifiers.
    -- We do not have a useful test on the form of such identifiers.
    uninterpretedDomeinFiles <- for allModels \({ id, doc }) -> case JSON.read <$> doc of
      Just (Left errs) -> (logPerspectivesError (Custom ("Cannot interpret model document as UninterpretedDomeinFile: '" <> id <> "' " <> show errs))) *> pure Nothing
      Nothing -> logPerspectivesError (Custom ("No document retrieved for model '" <> id <> "'.")) *> pure Nothing
      Just (Right (df :: UninterpretedDomeinFile)) -> pure $ Just df
    clearInvertedQueriesDatabase
    pushMessage "Updating local models..."
    r <- runMonadPerspectivesTransaction'
      false
      (ENR $ EnumeratedRoleType sysUser)
      (runExceptT (executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) recompileModel))
    void $ removeMessage "Updating local models..."
    case r of
      Left errors -> logPerspectivesError (Custom ("recompileLocalModels: " <> show errors)) *> pure false
      Right success -> do
        saveMarkedResources
        pure success
  where

  clearInvertedQueriesDatabase :: MonadPerspectives Unit
  clearInvertedQueriesDatabase = do
    db <- invertedQueryDatabaseName
    deleteDatabase db
    void $ databaseInfo db
    setupInvertedQueryDatabase

recompileLocalModel :: ModelUri Readable -> MonadPerspectives Boolean
recompileLocalModel modelUri = do
  mstableModelUri <- lookupStableModelUri_ modelUri
  case mstableModelUri of
    Nothing -> logPerspectivesError (Custom ("Cannot find stable model URI for model URI: " <> show modelUri)) *> pure false
    Just (ModelUri stableModelUri) -> do
      { repositoryUrl, documentName } <- pure $ unsafePartial modelUri2ModelUrl stableModelUri
      modelsdb <- modelsDatabaseName
      udf :: UninterpretedDomeinFile <- getDocument_ modelsdb documentName
      r <- runMonadPerspectivesTransaction'
        false
        (ENR $ EnumeratedRoleType sysUser)
        (runExceptT (recompileModel udf))
      case r of
        Left errors -> logPerspectivesError (Custom ("recompileLocalModel: " <> show errors)) *> pure false
        Right success -> do
          saveMarkedResources
          pure true

