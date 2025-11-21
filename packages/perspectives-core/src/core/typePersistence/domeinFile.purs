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

module Perspectives.DomeinFile where

import Control.Monad.State (State, execState, modify)
import Data.Array (cons)
import Data.Eq.Generic (genericEq)
import Data.Foldable (for_)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Show.Generic (genericShow)
import Foreign.Object (Object, empty, insert, lookup)
import Partial.Unsafe (unsafePartial)
import Persistence.Attachment (class Attachment)
import Perspectives.Couchdb (AttachmentInfo)
import Perspectives.Couchdb.Revision (class Revision)
import Perspectives.Data.EncodableMap (EncodableMap, addAll, removeAll)
import Perspectives.Data.EncodableMap (empty) as EM
import Perspectives.Identifiers (typeUri2ModelUri)
import Perspectives.InvertedQuery (InvertedQuery)
import Perspectives.Persistence.Types (PouchbdDocumentFields)
import Perspectives.Representation.Action (AutomaticAction)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty)
import Perspectives.Representation.CalculatedRole (CalculatedRole)
import Perspectives.Representation.Class.Identifiable (class Identifiable)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..), InvertedQueryKey)
import Perspectives.Representation.ScreenDefinition (ScreenDefinition, ScreenKey)
import Perspectives.Representation.State (State(..), Notification) as PEState
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType, IndexedContext, RoleType, StateIdentifier(..))
import Perspectives.Representation.UserGraph (UserGraph(..))
import Perspectives.Representation.View (View)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Readable)
import Prelude (class Eq, class Show, Unit, bind, eq, pure, unit, void, ($), (<$>), (<<<))
import Simple.JSON (class ReadForeign, class WriteForeign, read', readJSON', writeImpl, writeJSON)

newtype DomeinFile f = DomeinFile (DomeinFileRecord f)

-- NOTE: the qualification of the identifiers is in terms of the scheme "model:", 
-- two forward slashes and an internet namespace, followed by a hash sign.
type DomeinFileRecord f = PouchbdDocumentFields
  ( id :: ModelUri f -- The qualified stable identifier in terms of a CUID (ModelUri Stable)
  -- _id :: String -- The name of the file in Couchdb, e.g. perspectives_domains-System.json. DomeinFileName.
  , namespace :: ModelUri Readable -- The qualified readable name of the model that may vary. Semantically (ModelUri Readable), but is a String here.
  , contexts :: Object Context
  , enumeratedRoles :: Object EnumeratedRole
  , calculatedRoles :: Object CalculatedRole
  , enumeratedProperties :: Object EnumeratedProperty
  , calculatedProperties :: Object CalculatedProperty
  , views :: Object View
  , states :: Object PEState.State
  , arc :: String
  , referredModels :: Array (ModelUri f)
  -- Keys are DomeinFileIds.
  , invertedQueriesInOtherDomains :: Object (Array SeparateInvertedQuery)
  , upstreamStateNotifications :: Object (Array UpstreamStateNotification)
  , upstreamAutomaticEffects :: Object (Array UpstreamAutomaticEffect)
  , userGraph :: UserGraph
  , screens :: EncodableMap ScreenKey ScreenDefinition
  , toStableContextType :: EncodableMap ContextType ContextType
  , toStableEnumeratedRoleType :: EncodableMap EnumeratedRoleType EnumeratedRoleType
  , toStableCalculatedRoleType :: EncodableMap EnumeratedRoleType EnumeratedRoleType
  , toStableEnumeratedPropertyType :: EncodableMap EnumeratedPropertyType EnumeratedPropertyType
  , toStableCalculatedPropertyType :: EncodableMap EnumeratedPropertyType EnumeratedPropertyType
  , toReadableContextIndividuals :: EncodableMap IndexedContext IndexedContext
  -- , toReadableEnumeratedRoleIndividuals :: Map IndexedRole IndexedRole
  , _attachments :: Maybe AttachmentInfo
  )

derive instance genericDomeinFile :: Generic (DomeinFile f) _

derive instance newtypeDomeinFile :: Newtype (DomeinFile f) _

derive newtype instance WriteForeign (DomeinFile f)
derive newtype instance ReadForeign (DomeinFile f)

instance showDomeinFile :: Show (DomeinFile f) where
  show = genericShow

instance eqDomeinFile :: Eq (DomeinFile f) where
  eq (DomeinFile { id: id1 }) (DomeinFile { id: id2 }) = eq id1 id2

instance identifiableDomeinFile :: Identifiable (DomeinFile f) (ModelUri f) where
  identifier (DomeinFile { id }) = id
  displayName (DomeinFile { namespace }) = unwrap namespace

instance revisionDomeinFile :: Revision (DomeinFile f) where
  rev = _._rev <<< unwrap
  changeRevision s = over DomeinFile (\vr -> vr { _rev = s })

instance Attachment (DomeinFile f) where
  setAttachment d _ = d
  -- TODO. Handle screen attachments here!
  getAttachments _ = Nothing

-------------------------------------------------------------------------------
---- UPSTREAMSTATENOTIFICATION
-------------------------------------------------------------------------------
newtype UpstreamStateNotification = UpstreamStateNotification
  { stateId :: StateIdentifier
  , isOnEntry :: Boolean
  , notification :: PEState.Notification
  , qualifiedUsers :: Array RoleType
  }

derive instance Generic UpstreamStateNotification _
instance Show UpstreamStateNotification where
  show = genericShow

instance Eq UpstreamStateNotification where
  eq = genericEq

derive newtype instance WriteForeign UpstreamStateNotification
derive newtype instance ReadForeign UpstreamStateNotification

-------------------------------------------------------------------------------
---- UPSTREAMAUTOMATICEFFECT
-------------------------------------------------------------------------------
newtype UpstreamAutomaticEffect = UpstreamAutomaticEffect
  { stateId :: StateIdentifier
  , isOnEntry :: Boolean
  , automaticAction :: AutomaticAction
  , qualifiedUsers :: Array RoleType
  }

derive instance Generic UpstreamAutomaticEffect _
instance Show UpstreamAutomaticEffect where
  show = genericShow

instance Eq UpstreamAutomaticEffect where
  eq = genericEq

derive newtype instance WriteForeign UpstreamAutomaticEffect
derive newtype instance ReadForeign UpstreamAutomaticEffect

-------------------------------------------------------------------------------
---- INVERTEDQUERYCOLLECTION
-------------------------------------------------------------------------------
data SeparateInvertedQuery
  =
    -- Type of the role instance; Type of the context instance to store on; InvertedQuery
    RoleInvertedQuery EnumeratedRoleType TypeName InvertedQuery
  | -- `role` step
    -- Type of the context of the role instance; Type of the role instance to store on; InvertedQuery
    ContextInvertedQuery ContextType TypeName InvertedQuery
  | -- `context` step
    -- Triple keys; Type of the role instance to store on; InvertedQuery
    FillerInvertedQuery (Array InvertedQueryKey) TypeName InvertedQuery
  | -- `filledBy` step
    FilledInvertedQuery (Array InvertedQueryKey) TypeName InvertedQuery
  | -- `fills` step
    -- EnumeratedRoleTypes to index with; EnumeratedProperty to store on; InvertedQuery
    OnPropertyDelta (Array EnumeratedRoleType) TypeName InvertedQuery -- `Value2Role` step.

type TypeName = String

derive instance genericSeparateInvertedQuery :: Generic SeparateInvertedQuery _

instance showSeparateInvertedQuery :: Show SeparateInvertedQuery where
  show = genericShow

derive instance eqSeparateInvertedQuery :: Eq SeparateInvertedQuery

instance WriteForeign SeparateInvertedQuery where
  writeImpl (RoleInvertedQuery key typeName invertedQuery) = writeImpl { constructor: "RoleInvertedQuery", key: writeJSON key, typeName, invertedQuery }
  writeImpl (ContextInvertedQuery key typeName invertedQuery) = writeImpl { constructor: "ContextInvertedQuery", key: writeJSON key, typeName, invertedQuery }
  writeImpl (FillerInvertedQuery key typeName invertedQuery) = writeImpl { constructor: "FillerInvertedQuery", key: writeJSON key, typeName, invertedQuery }
  writeImpl (FilledInvertedQuery key typeName invertedQuery) = writeImpl { constructor: "FilledInvertedQuery", key: writeJSON key, typeName, invertedQuery }
  writeImpl (OnPropertyDelta key typeName invertedQuery) = writeImpl { constructor: "OnPropertyDelta", key: writeJSON key, typeName, invertedQuery }

instance ReadForeign SeparateInvertedQuery where
  readImpl f = do
    { constructor, key, typeName, invertedQuery } :: { constructor :: String, key :: String, typeName :: TypeName, invertedQuery :: InvertedQuery } <- read' f
    unsafePartial case constructor of
      "RoleInvertedQuery" -> (\k -> RoleInvertedQuery k typeName invertedQuery) <$> readJSON' key
      "ContextInvertedQuery" -> (\k -> ContextInvertedQuery k typeName invertedQuery) <$> readJSON' key
      "FillerInvertedQuery" -> (\k -> FillerInvertedQuery k typeName invertedQuery) <$> readJSON' key
      "FilledInvertedQuery" -> (\k -> FilledInvertedQuery k typeName invertedQuery) <$> readJSON' key
      "OnPropertyDelta" -> (\k -> OnPropertyDelta k typeName invertedQuery) <$> readJSON' key

addInvertedQueryForDomain :: forall f. TypeName -> InvertedQuery -> (TypeName -> InvertedQuery -> SeparateInvertedQuery) -> DomeinFileRecord f -> DomeinFileRecord f
addInvertedQueryForDomain typeName iq collectionConstructor dfr@{ invertedQueriesInOtherDomains } = case typeUri2ModelUri typeName of
  Nothing -> dfr
  Just modelName ->
    let
      invertedQueriesInOtherDomains' = case lookup modelName invertedQueriesInOtherDomains of
        Nothing -> insert modelName [ collectionConstructor typeName iq ] invertedQueriesInOtherDomains
        Just separateQueries -> insert modelName (cons (collectionConstructor typeName iq) separateQueries) invertedQueriesInOtherDomains
    in
      dfr { invertedQueriesInOtherDomains = invertedQueriesInOtherDomains' }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

defaultDomeinFileRecord :: forall f. DomeinFileRecord f
defaultDomeinFileRecord =
  { _rev: Nothing
  , _id: ""
  , id: ModelUri ""
  , namespace: ModelUri ""
  , contexts: empty
  , enumeratedRoles: empty
  , calculatedRoles: empty
  , enumeratedProperties: empty
  , calculatedProperties: empty
  , views: empty
  , states: empty
  , arc: ""
  , referredModels: []
  , invertedQueriesInOtherDomains: empty
  , upstreamStateNotifications: empty
  , upstreamAutomaticEffects: empty
  , userGraph: UserGraph $ EM.empty
  , screens: EM.empty
  , toStableContextType: EM.empty
  , toStableEnumeratedRoleType: EM.empty
  , toStableCalculatedRoleType: EM.empty
  , toStableEnumeratedPropertyType: EM.empty
  , toStableCalculatedPropertyType: EM.empty
  , toReadableContextIndividuals: EM.empty
  , _attachments: Nothing
  }

defaultDomeinFile :: forall f. (DomeinFile f)
defaultDomeinFile = DomeinFile defaultDomeinFileRecord

type DomeinFileEnumeratedRoles = Object EnumeratedRole

setRevision :: forall f. String -> (DomeinFile f) -> (DomeinFile f)
setRevision vs (DomeinFile dff) = DomeinFile $ dff { _rev = Just vs }

-- | Returns a table with indexed names as key and ContextType as value.
indexedContexts :: forall f. (DomeinFile f) -> Object ContextType
indexedContexts (DomeinFile { contexts }) = execState indexedContexts_ empty
  where
  indexedContexts_ :: State (Object ContextType) Unit
  indexedContexts_ = for_ contexts \(Context { id, indexedContext }) -> case indexedContext of
    Nothing -> pure unit
    Just i -> void $ modify \table -> insert (unwrap i) id table

-- | Returns a table with indexed names as key and ContextType as value.
indexedRoles :: forall f. (DomeinFile f) -> Object EnumeratedRoleType
indexedRoles (DomeinFile { enumeratedRoles }) = execState indexedRoles_ empty
  where
  indexedRoles_ :: State (Object EnumeratedRoleType) Unit
  indexedRoles_ = for_ enumeratedRoles \(EnumeratedRole { id, indexedRole }) -> case indexedRole of
    Nothing -> pure unit
    Just i -> void $ modify \table -> insert (unwrap i) id table

addUpstreamNotification :: forall f. UpstreamStateNotification -> StateIdentifier -> DomeinFileRecord f -> DomeinFileRecord f
addUpstreamNotification notification (StateIdentifier s) dfr@{ upstreamStateNotifications } =
  let
    domeinName = unsafePartial fromJust $ typeUri2ModelUri s
  in
    dfr
      { upstreamStateNotifications = case lookup domeinName upstreamStateNotifications of
          Nothing -> insert domeinName [ notification ] upstreamStateNotifications
          Just ns -> insert domeinName (cons notification ns) upstreamStateNotifications
      }

addUpstreamAutomaticEffect :: forall f. UpstreamAutomaticEffect -> StateIdentifier -> DomeinFileRecord f -> DomeinFileRecord f
addUpstreamAutomaticEffect effect (StateIdentifier s) dfr@{ upstreamAutomaticEffects } =
  let
    domeinName = unsafePartial fromJust $ typeUri2ModelUri s
  in
    dfr
      { upstreamAutomaticEffects = case lookup domeinName upstreamAutomaticEffects of
          Nothing -> insert domeinName [ effect ] upstreamAutomaticEffects
          Just ns -> insert domeinName (cons effect ns) upstreamAutomaticEffects
      }

addDownStreamNotification :: forall f. UpstreamStateNotification -> State (DomeinFileRecord f) Unit
addDownStreamNotification = modifyDownstreamNotification true

removeDownStreamNotification :: forall f. UpstreamStateNotification -> State (DomeinFileRecord f) Unit
removeDownStreamNotification = modifyDownstreamNotification false

modifyDownstreamNotification :: forall f. Boolean -> UpstreamStateNotification -> State (DomeinFileRecord f) Unit
modifyDownstreamNotification add (UpstreamStateNotification { stateId, isOnEntry, notification, qualifiedUsers }) = void $ modify \dfr@{ states } ->
  case lookup (unwrap stateId) states of
    Nothing -> dfr
    Just s -> dfr { states = insert (unwrap stateId) (modifyState s) states }
  where
  modifyState :: PEState.State -> PEState.State
  modifyState (PEState.State sr@{ notifyOnEntry, notifyOnExit }) =
    if isOnEntry then
      if add then PEState.State sr { notifyOnEntry = addAll notification notifyOnEntry qualifiedUsers }
      else PEState.State sr { notifyOnEntry = removeAll notification notifyOnEntry qualifiedUsers }
    else if add then PEState.State sr { notifyOnExit = addAll notification notifyOnExit qualifiedUsers }
    else PEState.State sr { notifyOnExit = removeAll notification notifyOnExit qualifiedUsers }

addDownStreamAutomaticEffect :: forall f. UpstreamAutomaticEffect -> State (DomeinFileRecord f) Unit
addDownStreamAutomaticEffect = modifyDownstreamAutomaticEffect true

removeDownStreamAutomaticEffect :: forall f. UpstreamAutomaticEffect -> State (DomeinFileRecord f) Unit
removeDownStreamAutomaticEffect = modifyDownstreamAutomaticEffect false

modifyDownstreamAutomaticEffect :: forall f. Boolean -> UpstreamAutomaticEffect -> State (DomeinFileRecord f) Unit
modifyDownstreamAutomaticEffect add (UpstreamAutomaticEffect { stateId, isOnEntry, automaticAction, qualifiedUsers }) = void $ modify \dfr@{ states } ->
  case lookup (unwrap stateId) states of
    Nothing -> dfr
    Just s -> dfr { states = insert (unwrap stateId) (modifyState s) states }
  where
  modifyState :: PEState.State -> PEState.State
  modifyState (PEState.State sr@{ automaticOnEntry, automaticOnExit }) =
    if isOnEntry then
      if add then PEState.State sr { automaticOnEntry = addAll automaticAction automaticOnEntry qualifiedUsers }
      else PEState.State sr { automaticOnEntry = removeAll automaticAction automaticOnEntry qualifiedUsers }
    else if add then PEState.State sr { automaticOnExit = addAll automaticAction automaticOnExit qualifiedUsers }
    else PEState.State sr { automaticOnExit = removeAll automaticAction automaticOnExit qualifiedUsers }

