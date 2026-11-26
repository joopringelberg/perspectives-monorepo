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
--
-- END LICENSE

module Perspectives.Sync.VersionedDelta
  ( DeltaEnvelope(..)
  , class SignVersionedDelta
  , isPredefinedContext
  , isPredefinedContextScaffolding
  , parseIncomingDelta
  , currentDeltaFormatVersion
  , parseDeltaFormatVersion
  , signVersionedDelta
  ) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String as Str
import Data.Traversable (for, traverse)
import Perspectives.Authenticate (signDelta)
import Perspectives.CoreTypes (MonadPerspectivesTransaction)
import Perspectives.Persistence.CausalStore (reserveCausalStamp)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.Representation.Class.PersistentType (getCalculatedRole, getContext, getEnumeratedProperty, getEnumeratedRole)
import Perspectives.ResourceIdentifiers (stripNonPublicIdentifiers)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.Sync.PropertyHistory (isCheckpoint)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), RoleBindingDelta(..), RoleBindingDeltaType, RolePropertyDelta(..), RolePropertyDeltaType, UniverseContextDelta(..), UniverseContextDeltaType, UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Simple.JSON (class ReadForeign, readJSON', writeJSON)

defaultTypeVersion :: String
defaultTypeVersion = "0.0"

-- | Format 5 (Perspectives Universe cut-over): every delta carries an explicit `deltaFamily`, property deltas
-- | carry signed `provenance`. Only format 5 deltas and format-3 property checkpoints are accepted.
currentDeltaFormatVersion :: Int
currentDeltaFormatVersion = 5

data DeltaEnvelope
  = ContextEnvelope ContextDelta
  | RoleBindingEnvelope RoleBindingDelta
  | RolePropertyEnvelope RolePropertyDelta
  | UniverseContextEnvelope UniverseContextDelta
  | UniverseRoleEnvelope UniverseRoleDelta

class SignVersionedDelta a where
  signVersionedDelta :: a -> MonadPerspectivesTransaction SignedDelta

type DeltaFormatMarker = { deltaFormatVersion :: Maybe Int }

parseDeltaFormatVersion :: String -> Int
parseDeltaFormatVersion stringifiedDelta = case runExcept $ readJSON' stringifiedDelta of
  Right ({ deltaFormatVersion: Just version } :: DeltaFormatMarker) -> version
  _ -> 1

type WireBase f =
  { deltaFormatVersion :: Int
  , deltaFamily :: String
  , subject :: String
  , subjectKind :: String
  , resourceKey :: String
  , resourceVersion :: Int
  | f
  }

newtype WireUniverseContextDelta = WireUniverseContextDelta
  ( WireBase
      ( id :: ContextInstance
      , contextType :: String
      , deltaType :: UniverseContextDeltaType
      )
  )

derive newtype instance ReadForeign WireUniverseContextDelta

newtype WireUniverseRoleDelta = WireUniverseRoleDelta
  ( WireBase
      ( id :: ContextInstance
      , contextType :: String
      , roleType :: String
      , authorizedRole :: Maybe String
      , authorizedRoleKind :: Maybe String
      , roleInstance :: RoleInstance
      , deltaType :: UniverseRoleDeltaType
      )
  )

derive newtype instance ReadForeign WireUniverseRoleDelta

newtype WireContextDelta = WireContextDelta
  ( WireBase
      ( contextInstance :: ContextInstance
      , contextType :: String
      , roleType :: String
      , roleInstance :: RoleInstance
      , destinationContext :: Maybe ContextInstance
      , destinationContextType :: Maybe String
      , deltaType :: ContextDeltaType
      )
  )

derive newtype instance ReadForeign WireContextDelta

newtype WireRoleBindingDelta = WireRoleBindingDelta
  ( WireBase
      ( filled :: RoleInstance
      , filledType :: String
      , filler :: Maybe RoleInstance
      , fillerType :: Maybe String
      , oldFiller :: Maybe RoleInstance
      , oldFillerType :: Maybe String
      , deltaType :: RoleBindingDeltaType
      )
  )

derive newtype instance ReadForeign WireRoleBindingDelta

newtype WireRolePropertyDelta = WireRolePropertyDelta
  ( WireBase
      ( id :: RoleInstance
      , roleType :: String
      , property :: String
      , values :: Array Value
      , deltaType :: RolePropertyDeltaType
      , provenance :: Maybe WirePropertyProvenance
      )
  )

derive newtype instance ReadForeign WireRolePropertyDelta

type WirePropertyProvenance =
  { subjectInstance :: RoleInstance
  , subjectInstanceType :: String
  , evaluationContext :: ContextInstance
  , evaluationContextType :: String
  , objectInstance :: RoleInstance
  , objectInstanceType :: String
  }

-- | The values of the explicit `deltaFamily` discriminator on the wire.
rolePropertyFamily :: String
rolePropertyFamily = "RolePropertyDelta"

roleBindingFamily :: String
roleBindingFamily = "RoleBindingDelta"

contextFamily :: String
contextFamily = "ContextDelta"

universeRoleFamily :: String
universeRoleFamily = "UniverseRoleDelta"

universeContextFamily :: String
universeContextFamily = "UniverseContextDelta"

parseIncomingDelta :: String -> Either String DeltaEnvelope
parseIncomingDelta stringifiedDelta =
  let
    format = parseDeltaFormatVersion stringifiedDelta
  in
    if format == currentDeltaFormatVersion || (format == 3 && isCheckpoint stringifiedDelta) then
      parseCurrentFormat stringifiedDelta
    else Left $ "Unsupported delta format version " <> show format

-- | TheWorld and TheSocialEnvironment are constructed locally by every installation, with the same
-- | identifier. Their construction history is never shipped. Published copies are not predefined.
isPredefinedContext :: String -> Boolean
isPredefinedContext id =
  let
    localId = stripNonPublicIdentifiers id
  in
    localId == "TheWorld" || localId == "TheSocialEnvironment"

-- | Private bootstrap containers already exist at every receiver. Published copies do not.
isPredefinedContextScaffolding :: String -> Boolean
isPredefinedContextScaffolding stringifiedDelta = case parseIncomingDelta stringifiedDelta of
  Right (UniverseContextEnvelope (UniverseContextDelta { id })) ->
    isPredefined (unwrap id)
  Right (UniverseRoleEnvelope (UniverseRoleDelta { id, roleInstance, deltaType: ConstructExternalRole })) ->
    isExternal (unwrap id) (unwrap roleInstance)
  Right (ContextEnvelope (ContextDelta { contextInstance, roleInstance, deltaType: AddExternalRole })) ->
    isExternal (unwrap contextInstance) (unwrap roleInstance)
  _ -> false
  where
  isPredefined = isPredefinedContext

  isExternal id roleId =
    isPredefined id && stripNonPublicIdentifiers roleId == stripNonPublicIdentifiers id <> "$External"

parseCurrentFormat :: String -> Either String DeltaEnvelope
parseCurrentFormat stringifiedDelta = case runExcept $ readJSON' stringifiedDelta of
  Left err -> Left $ "Missing delta family: " <> show err
  Right ({ deltaFamily } :: { deltaFamily :: String })
    | deltaFamily == rolePropertyFamily -> readWire stringifiedDelta \(WireRolePropertyDelta r) ->
        RolePropertyEnvelope $ RolePropertyDelta
          { subject: mkRoleType r.subjectKind r.subject
          , resourceKey: r.resourceKey
          , resourceVersion: r.resourceVersion
          , id: r.id
          , roleType: EnumeratedRoleType $ stripRevisionedTypeReference r.roleType
          , property: EnumeratedPropertyType $ stripRevisionedTypeReference r.property
          , values: r.values
          , deltaType: r.deltaType
          , provenance: r.provenance <#> \p ->
              { subjectInstance: p.subjectInstance
              , subjectInstanceType: EnumeratedRoleType $ stripRevisionedTypeReference p.subjectInstanceType
              , evaluationContext: p.evaluationContext
              , evaluationContextType: ContextType $ stripRevisionedTypeReference p.evaluationContextType
              , objectInstance: p.objectInstance
              , objectInstanceType: EnumeratedRoleType $ stripRevisionedTypeReference p.objectInstanceType
              }
          }
    | deltaFamily == roleBindingFamily -> readWire stringifiedDelta \(WireRoleBindingDelta r) ->
        RoleBindingEnvelope $ RoleBindingDelta
          { subject: mkRoleType r.subjectKind r.subject
          , resourceKey: r.resourceKey
          , resourceVersion: r.resourceVersion
          , filled: r.filled
          , filledType: EnumeratedRoleType $ stripRevisionedTypeReference r.filledType
          , filler: r.filler
          , fillerType: map (EnumeratedRoleType <<< stripRevisionedTypeReference) r.fillerType
          , oldFiller: r.oldFiller
          , oldFillerType: map (EnumeratedRoleType <<< stripRevisionedTypeReference) r.oldFillerType
          , deltaType: r.deltaType
          }
    | deltaFamily == contextFamily -> readWire stringifiedDelta \(WireContextDelta r) ->
        ContextEnvelope $ ContextDelta
          { subject: mkRoleType r.subjectKind r.subject
          , resourceKey: r.resourceKey
          , resourceVersion: r.resourceVersion
          , contextInstance: r.contextInstance
          , contextType: ContextType $ stripRevisionedTypeReference r.contextType
          , roleType: EnumeratedRoleType $ stripRevisionedTypeReference r.roleType
          , roleInstance: r.roleInstance
          , destinationContext: r.destinationContext
          , destinationContextType: map (ContextType <<< stripRevisionedTypeReference) r.destinationContextType
          , deltaType: r.deltaType
          }
    | deltaFamily == universeRoleFamily -> readWire stringifiedDelta \(WireUniverseRoleDelta r) ->
        UniverseRoleEnvelope $ UniverseRoleDelta
          { subject: mkRoleType r.subjectKind r.subject
          , resourceKey: r.resourceKey
          , resourceVersion: r.resourceVersion
          , id: r.id
          , contextType: ContextType $ stripRevisionedTypeReference r.contextType
          , roleType: EnumeratedRoleType $ stripRevisionedTypeReference r.roleType
          , authorizedRole: case r.authorizedRole, r.authorizedRoleKind of
              Nothing, _ -> Nothing
              Just roleRef, Just roleKind -> Just $ mkRoleType roleKind roleRef
              Just roleRef, Nothing -> Just $ ENR $ EnumeratedRoleType $ stripRevisionedTypeReference roleRef
          , roleInstance: r.roleInstance
          , deltaType: r.deltaType
          }
    | deltaFamily == universeContextFamily -> readWire stringifiedDelta \(WireUniverseContextDelta r) ->
        UniverseContextEnvelope $ UniverseContextDelta
          { subject: mkRoleType r.subjectKind r.subject
          , resourceKey: r.resourceKey
          , resourceVersion: r.resourceVersion
          , id: r.id
          , contextType: ContextType $ stripRevisionedTypeReference r.contextType
          , deltaType: r.deltaType
          }
    | otherwise -> Left $ "Unknown delta family " <> deltaFamily
  where
  readWire :: forall w. ReadForeign w => String -> (w -> DeltaEnvelope) -> Either String DeltaEnvelope
  readWire json f = case runExcept $ readJSON' json of
    Right w -> Right $ f w
    Left err -> Left $ show err

instance signVersionedUniverseContextDelta :: SignVersionedDelta UniverseContextDelta where
  signVersionedDelta (UniverseContextDelta delta) = do
    subject <- versionedRoleParts delta.subject
    contextType <- versionedContextType delta.contextType
    signDelta $ writeJSON
      { deltaFormatVersion: currentDeltaFormatVersion
      , deltaFamily: universeContextFamily
      , subject: subject.reference
      , subjectKind: subject.kind
      , resourceKey: delta.resourceKey
      , resourceVersion: delta.resourceVersion
      , id: delta.id
      , contextType
      , deltaType: delta.deltaType
      }

instance signVersionedUniverseRoleDelta :: SignVersionedDelta UniverseRoleDelta where
  signVersionedDelta (UniverseRoleDelta delta) = do
    causal <- lift $ reserveCausalStamp (unwrap delta.roleInstance)
    subject <- versionedRoleParts delta.subject
    contextType <- versionedContextType delta.contextType
    roleType <- versionedEnumeratedRoleType delta.roleType
    authorizedRole <- traverse versionedRoleParts delta.authorizedRole
    signDelta $ writeJSON
      { deltaFormatVersion: currentDeltaFormatVersion
      , deltaFamily: universeRoleFamily
      , causal
      , subject: subject.reference
      , subjectKind: subject.kind
      , resourceKey: delta.resourceKey
      , resourceVersion: delta.resourceVersion
      , id: delta.id
      , contextType
      , roleType
      , authorizedRole: map _.reference authorizedRole
      , authorizedRoleKind: map _.kind authorizedRole
      , roleInstance: delta.roleInstance
      , deltaType: delta.deltaType
      }

instance signVersionedContextDelta :: SignVersionedDelta ContextDelta where
  signVersionedDelta (ContextDelta delta) = do
    causal <- lift $ reserveCausalStamp (unwrap delta.roleInstance)
    subject <- versionedRoleParts delta.subject
    contextType <- versionedContextType delta.contextType
    roleType <- versionedEnumeratedRoleType delta.roleType
    destinationContextType <- traverse versionedContextType delta.destinationContextType
    signDelta $ writeJSON
      { deltaFormatVersion: currentDeltaFormatVersion
      , deltaFamily: contextFamily
      , causal
      , subject: subject.reference
      , subjectKind: subject.kind
      , resourceKey: delta.resourceKey
      , resourceVersion: delta.resourceVersion
      , contextInstance: delta.contextInstance
      , contextType
      , roleType
      , roleInstance: delta.roleInstance
      , destinationContext: delta.destinationContext
      , destinationContextType
      , deltaType: delta.deltaType
      }

instance signVersionedRoleBindingDelta :: SignVersionedDelta RoleBindingDelta where
  signVersionedDelta (RoleBindingDelta delta) = do
    causal <- lift $ reserveCausalStamp (unwrap delta.filled)
    subject <- versionedRoleParts delta.subject
    filledType <- versionedEnumeratedRoleType delta.filledType
    fillerType <- traverse versionedEnumeratedRoleType delta.fillerType
    oldFillerType <- traverse versionedEnumeratedRoleType delta.oldFillerType
    signDelta $ writeJSON
      { deltaFormatVersion: currentDeltaFormatVersion
      , deltaFamily: roleBindingFamily
      , causal
      , subject: subject.reference
      , subjectKind: subject.kind
      , resourceKey: delta.resourceKey
      , resourceVersion: delta.resourceVersion
      , filled: delta.filled
      , filledType
      , filler: delta.filler
      , fillerType
      , oldFiller: delta.oldFiller
      , oldFillerType
      , deltaType: delta.deltaType
      }

instance signVersionedRolePropertyDelta :: SignVersionedDelta RolePropertyDelta where
  signVersionedDelta (RolePropertyDelta delta) = do
    causal <- lift $ reserveCausalStamp (unwrap delta.id)
    subject <- versionedRoleParts delta.subject
    roleType <- versionedEnumeratedRoleType delta.roleType
    property <- versionedEnumeratedPropertyType delta.property
    provenance <- for delta.provenance \p -> do
      subjectInstanceType <- versionedEnumeratedRoleType p.subjectInstanceType
      evaluationContextType <- versionedContextType p.evaluationContextType
      objectInstanceType <- versionedEnumeratedRoleType p.objectInstanceType
      pure
        { subjectInstance: p.subjectInstance
        , subjectInstanceType
        , evaluationContext: p.evaluationContext
        , evaluationContextType
        , objectInstance: p.objectInstance
        , objectInstanceType
        }
    signDelta $ writeJSON
      { deltaFormatVersion: currentDeltaFormatVersion
      , deltaFamily: rolePropertyFamily
      , causal
      , subject: subject.reference
      , subjectKind: subject.kind
      , resourceKey: delta.resourceKey
      , resourceVersion: delta.resourceVersion
      , id: delta.id
      , roleType
      , property
      , values: delta.values
      , deltaType: delta.deltaType
      , provenance
      }

versionedContextType :: ContextType -> MonadPerspectivesTransaction String
versionedContextType contextType@(ContextType identifier) = do
  Context { typeVersion } <- lift $ getContext contextType
  pure $ appendTypeVersion identifier typeVersion

versionedEnumeratedRoleType :: EnumeratedRoleType -> MonadPerspectivesTransaction String
versionedEnumeratedRoleType roleType@(EnumeratedRoleType identifier) = do
  EnumeratedRole { typeVersion } <- lift $ getEnumeratedRole roleType
  pure $ appendTypeVersion identifier typeVersion

versionedEnumeratedPropertyType :: EnumeratedPropertyType -> MonadPerspectivesTransaction String
versionedEnumeratedPropertyType propertyType@(EnumeratedPropertyType identifier) = do
  EnumeratedProperty { typeVersion } <- lift $ getEnumeratedProperty propertyType
  pure $ appendTypeVersion identifier typeVersion

versionedRoleParts :: RoleType -> MonadPerspectivesTransaction { reference :: String, kind :: String }
versionedRoleParts (ENR roleType@(EnumeratedRoleType identifier)) = do
  EnumeratedRole { typeVersion } <- lift $ getEnumeratedRole roleType
  pure { reference: appendTypeVersion identifier typeVersion, kind: "ENR" }
versionedRoleParts (CR roleType@(CalculatedRoleType identifier)) = do
  CalculatedRole { typeVersion } <- lift $ getCalculatedRole roleType
  pure { reference: appendTypeVersion identifier typeVersion, kind: "CR" }

appendTypeVersion :: String -> Maybe String -> String
appendTypeVersion identifier typeVersion = identifier <> "@" <> fromMaybe defaultTypeVersion typeVersion

mkRoleType :: String -> String -> RoleType
mkRoleType kind reference = case kind of
  "CR" -> CR $ CalculatedRoleType $ stripRevisionedTypeReference reference
  _ -> ENR $ EnumeratedRoleType $ stripRevisionedTypeReference reference

stripRevisionedTypeReference :: String -> String
stripRevisionedTypeReference reference = case Str.lastIndexOf (Str.Pattern "@") reference of
  Nothing -> reference
  Just ix -> Str.take ix reference
