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

module Perspectives.ModelGraph where

import Prelude

import Data.Array (concat, filter, null)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Effect.Aff (try)
import Foreign.Object (keys, values) as Object
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.HumanReadableType (translateType)
import Perspectives.Identifiers (isExternalRole, typeUri2ModelUri)
import Perspectives.Persistent (getDomeinFile)
import Perspectives.Query.QueryTypes (RoleInContext(..))
import Perspectives.Representation.ADT (allLeavesInADT)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), RoleKind(..)) as TI
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Simple.JSON (writeJSON)

-----------------------------------------------------------
-- MODEL CONTEXT GRAPH
-----------------------------------------------------------

type GraphNode = { id :: String, label :: String }

type GraphEdge = { from :: String, to :: String, roleId :: String, roleLabel :: String }

type SerializedModelGraph = { nodes :: Array GraphNode, edges :: Array GraphEdge }

emptyGraph :: SerializedModelGraph
emptyGraph = { nodes: [], edges: [] }

-- | Constructs the type-level context navigation DAG for the model that contains
-- | the given context type, and returns it as a JSON string.
-- |
-- | Nodes: all context types defined in the model.
-- | Edges: directed edges derived from ContextRole-kinded roles, pointing from the
-- | containing context type to the context type that the role binds to.
-- |
-- | Returns an empty graph JSON if the model URI cannot be derived or if the
-- | DomeinFile cannot be loaded.
constructModelGraph :: String -> MonadPerspectives String
constructModelGraph contextTypeStr = do
  let mModelUri = typeUri2ModelUri contextTypeStr
  case mModelUri of
    Nothing -> pure $ writeJSON emptyGraph
    Just modelUriStr -> do
      mDomeinFile <- try $ getDomeinFile (ModelUri modelUriStr)
      case mDomeinFile of
        Left _ -> pure $ writeJSON emptyGraph
        Right (DomeinFile df) -> do
          -- Build nodes from all context types in the DomeinFile, with translated labels.
          nodes <- for (Object.keys df.contexts) \ctKey -> do
            label <- translateType (TI.ContextType ctKey)
            pure { id: ctKey, label }
          -- Build edges from ContextRole-kinded enumerated roles.
          edgeGroups <- for (Object.values df.enumeratedRoles) \(EnumeratedRole er) ->
            if er.kindOfRole == TI.ContextRole
              then case er.binding of
                Nothing -> pure []
                Just adtBinding -> do
                  -- Collect all RoleInContext leaves from the ADT.
                  let leaves = allLeavesInADT adtBinding
                  -- Keep only external-role leaves; their .context field is the
                  -- target context type of the edge.
                  let externalRics = filter (\(RoleInContext { role }) -> isExternalRole (unwrap role)) leaves
                  if null externalRics
                    then pure []
                    else do
                      roleLabel <- translateType er.id
                      pure $ map
                        ( \(RoleInContext { context: toCtx }) ->
                            { from: unwrap er.context
                            , to: unwrap toCtx
                            , roleId: unwrap er.id
                            , roleLabel
                            }
                        )
                        externalRics
              else pure []
          let edges = concat edgeGroups
          pure $ writeJSON { nodes, edges }
