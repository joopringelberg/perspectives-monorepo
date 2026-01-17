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

module Perspectives.DataUpgrade.UpdateLocalModels where

import Control.Alt (void)
import Control.Monad.Cont (lift)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (for)
import Effect.Class.Console (log)
import Main.RecompileBasicModels (UninterpretedDomeinFile(..), executeInTopologicalOrder)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Extern.Couchdb (updateModel')
import Perspectives.External.CoreModules (addAllExternalFunctions)
import Perspectives.ModelDependencies (sysUser)
import Perspectives.Parsing.Messages (PerspectivesError(..), MultiplePerspectivesErrors)
import Perspectives.Persistence.API (databaseInfo, deleteDatabase, documentsInDatabase, includeDocs)
import Perspectives.Persistent (invertedQueryDatabaseName, saveMarkedResources)
import Perspectives.PerspectivesState (modelsDatabaseName, pushMessage, removeMessage)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction')
import Perspectives.SetupUser (setupInvertedQueryDatabase)
import Prelude (Unit, bind, discard, pure, show, ($), (*>), (<$>), (<>))
import Simple.JSON (read) as JSON

updateLocalModels :: MonadPerspectives Boolean
updateLocalModels =
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
      (runExceptT (executeInTopologicalOrder (catMaybes uninterpretedDomeinFiles) updateLocalModel))
    void $ removeMessage "Updating local models..."
    case r of
      Left errors -> logPerspectivesError (Custom ("updateLocalModels: " <> show errors)) *> pure false
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

  updateLocalModel :: UninterpretedDomeinFile -> ExceptT MultiplePerspectivesErrors MonadPerspectivesTransaction UninterpretedDomeinFile
  updateLocalModel df@(UninterpretedDomeinFile { id, namespace }) = do
    lift $ updateModel' id false false
    log ("Model updated: " <> show namespace)
    pure df
