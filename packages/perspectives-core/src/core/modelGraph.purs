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

import Data.Array (any, concat, filter)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Effect.Aff (try)
import Foreign.Object (keys, values) as Object
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.HumanReadableType (translateType)
import Perspectives.Identifiers (isExternalRole, typeUri2ModelUri)
import Perspectives.Names (lookupIndexedContext)
import Perspectives.Persistent (getDomeinFile)
import Perspectives.Query.QueryTypes (RoleInContext(..))
import Perspectives.Representation.ADT (allLeavesInADT)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (ContextType(..), EnumeratedRoleType, RoleKind(..)) as TI
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))
import Perspectives.Types.ObjectGetters (indexedContextName)
import Simple.JSON (writeJSON)

-----------------------------------------------------------
-- MODEL CONTEXT GRAPH
-----------------------------------------------------------

type GraphNode = { id :: TI.ContextType, label :: String, indexedName :: Maybe ContextInstance }

type GraphEdge =
  { from :: TI.ContextType
  , to :: TI.ContextType
  , roleId :: TI.EnumeratedRoleType
  , roleLabel :: String
  , roleKind :: TI.RoleKind
  }

type SerializedModelGraph = { nodes :: Array GraphNode, edges :: Array GraphEdge }

emptyGraph :: SerializedModelGraph
emptyGraph = { nodes: [], edges: [] }

-- | Constructs the type-level context navigation DAG for the model that contains
-- | the given context type, and returns it as a JSON string.
-- |
-- | Nodes: all context types defined in the model.
-- | Edges: directed edges derived from ContextRole- and UserRole-kinded roles,
-- | pointing from the containing context type to the context type that the role
-- | binds to. Self-referential edges are omitted, and ContextRole edges take
-- | precedence between the same context types.
-- |
-- | Returns an empty graph JSON if the model URI cannot be derived or if the
-- | DomeinFile cannot be loaded.
constructModelGraph :: TI.ContextType -> MonadPerspectives String
constructModelGraph contextTypeStr = do
  let mModelUri = typeUri2ModelUri (unwrap contextTypeStr)
  case mModelUri of
    Nothing -> pure $ writeJSON emptyGraph
    Just modelUriStr -> do
      mDomeinFile <- try $ getDomeinFile (ModelUri modelUriStr)
      case mDomeinFile of
        Left _ -> pure $ writeJSON emptyGraph
        Right (DomeinFile df) -> do
          -- Build nodes from all context types in the DomeinFile, with translated labels.
          nodes <- for (filter (\k -> not (k == modelUriStr)) (Object.keys df.contexts)) \ctKey -> do
            label <- translateType (TI.ContextType ctKey)
            indexedName <- indexedContextName (TI.ContextType ctKey)
            case indexedName of
              Nothing -> pure { id: TI.ContextType ctKey, label, indexedName: Nothing }
              Just (ContextInstance c) -> do
                indexedIndividual <- lookupIndexedContext c
                pure { id: TI.ContextType ctKey, label, indexedName: indexedIndividual }
          -- Build edges from ContextRole- and UserRole-kinded enumerated roles.
          edgeGroups <- for (Object.values df.enumeratedRoles) \(EnumeratedRole er) ->
            if er.kindOfRole == TI.ContextRole || er.kindOfRole == TI.UserRole then case er.binding of
              Nothing -> pure []
              Just adtBinding -> do
                let leaves = allLeavesInADT adtBinding
                let
                  targetRoles =
                    if er.kindOfRole == TI.ContextRole then
                      filter (\(RoleInContext { role }) -> isExternalRole (unwrap role)) leaves
                    else
                      leaves
                roleLabel <- translateType er.id
                pure $ map
                  ( \(RoleInContext { context: toCtx }) ->
                      { from: er.context
                      , to: toCtx
                      , roleId: er.id
                      , roleLabel
                      , roleKind: er.kindOfRole
                      }
                  )
                  targetRoles
            else pure []
          let allEdges = filter (\edge -> edge.from /= edge.to) (concat edgeGroups)
          let contextRoleEdges = filter (\edge -> edge.roleKind == TI.ContextRole) allEdges
          let userRoleEdges = filter (\edge -> edge.roleKind == TI.UserRole) allEdges
          let
            contextRoleConnects userEdge = any
              ( \contextRoleEdge ->
                  (contextRoleEdge.from == userEdge.from && contextRoleEdge.to == userEdge.to)
                    || (contextRoleEdge.from == userEdge.to && contextRoleEdge.to == userEdge.from)
              )
              contextRoleEdges
          let edges = contextRoleEdges <> filter (not <<< contextRoleConnects) userRoleEdges
          pure $ writeJSON { nodes, edges }
