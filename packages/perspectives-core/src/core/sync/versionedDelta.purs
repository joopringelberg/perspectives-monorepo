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
  , parseIncomingDelta
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
import Data.Traversable (traverse)
import Perspectives.Authenticate (signDelta)
import Perspectives.CoreTypes (MonadPerspectivesTransaction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), RoleType(..))
import Perspectives.Representation.Class.PersistentType (getCalculatedRole, getContext, getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Sync.LegacyDeltas (LegacyContextDelta, LegacyRoleBindingDelta, LegacyRolePropertyDelta, LegacyUniverseContextDelta, LegacyUniverseRoleDelta, toContextDelta, toRoleBindingDelta, toRolePropertyDelta, toUniverseContextDelta, toUniverseRoleDelta)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType, RoleBindingDelta(..), RoleBindingDeltaType, RolePropertyDelta(..), RolePropertyDeltaType, UniverseContextDelta(..), UniverseContextDeltaType, UniverseRoleDelta(..), UniverseRoleDeltaType)
import Simple.JSON (class ReadForeign, readJSON', writeJSON)

defaultTypeVersion :: String
defaultTypeVersion = "0.0"

currentDeltaFormatVersion :: Int
currentDeltaFormatVersion = 2

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
      )
  )

derive newtype instance ReadForeign WireRolePropertyDelta

parseIncomingDelta :: String -> Either String DeltaEnvelope
parseIncomingDelta stringifiedDelta =
  if parseDeltaFormatVersion stringifiedDelta == currentDeltaFormatVersion then
    parseVersion2 stringifiedDelta
  else
    parseLegacyCompatible stringifiedDelta

parseVersion2 :: String -> Either String DeltaEnvelope
parseVersion2 stringifiedDelta =
  case runExcept $ readJSON' stringifiedDelta of
    Right (WireRolePropertyDelta r) ->
      Right $ RolePropertyEnvelope $ RolePropertyDelta
        { subject: mkRoleType r.subjectKind r.subject
        , resourceKey: r.resourceKey
        , resourceVersion: r.resourceVersion
        , id: r.id
        , roleType: EnumeratedRoleType $ stripRevisionedTypeReference r.roleType
        , property: EnumeratedPropertyType $ stripRevisionedTypeReference r.property
        , values: r.values
        , deltaType: r.deltaType
        }
    Left _ -> case runExcept $ readJSON' stringifiedDelta of
      Right (WireRoleBindingDelta r) ->
        Right $ RoleBindingEnvelope $ RoleBindingDelta
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
      Left _ -> case runExcept $ readJSON' stringifiedDelta of
        Right (WireContextDelta r) ->
          Right $ ContextEnvelope $ ContextDelta
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
        Left _ -> case runExcept $ readJSON' stringifiedDelta of
          Right (WireUniverseRoleDelta r) ->
            Right $ UniverseRoleEnvelope $ UniverseRoleDelta
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
          Left _ -> case runExcept $ readJSON' stringifiedDelta of
            Right (WireUniverseContextDelta r) ->
              Right $ UniverseContextEnvelope $ UniverseContextDelta
                { subject: mkRoleType r.subjectKind r.subject
                , resourceKey: r.resourceKey
                , resourceVersion: r.resourceVersion
                , id: r.id
                , contextType: ContextType $ stripRevisionedTypeReference r.contextType
                , deltaType: r.deltaType
                }
            Left err -> Left $ show err

parseLegacyCompatible :: String -> Either String DeltaEnvelope
parseLegacyCompatible stringifiedDelta =
  case runExcept $ readJSON' stringifiedDelta of
    Right delta -> Right $ RolePropertyEnvelope delta
    Left _ -> case runExcept $ readJSON' stringifiedDelta of
      Right delta -> Right $ RoleBindingEnvelope delta
      Left _ -> case runExcept $ readJSON' stringifiedDelta of
        Right delta -> Right $ ContextEnvelope delta
        Left _ -> case runExcept $ readJSON' stringifiedDelta of
          Right delta -> Right $ UniverseRoleEnvelope delta
          Left _ -> case runExcept $ readJSON' stringifiedDelta of
            Right delta -> Right $ UniverseContextEnvelope delta
            Left _ -> case runExcept $ readJSON' stringifiedDelta of
              Right (delta :: LegacyRolePropertyDelta) -> Right $ RolePropertyEnvelope $ toRolePropertyDelta delta
              Left _ -> case runExcept $ readJSON' stringifiedDelta of
                Right (delta :: LegacyRoleBindingDelta) -> Right $ RoleBindingEnvelope $ toRoleBindingDelta delta
                Left _ -> case runExcept $ readJSON' stringifiedDelta of
                  Right (delta :: LegacyContextDelta) -> Right $ ContextEnvelope $ toContextDelta delta
                  Left _ -> case runExcept $ readJSON' stringifiedDelta of
                    Right (delta :: LegacyUniverseRoleDelta) -> Right $ UniverseRoleEnvelope $ toUniverseRoleDelta delta
                    Left _ -> case runExcept $ readJSON' stringifiedDelta of
                      Right (delta :: LegacyUniverseContextDelta) -> Right $ UniverseContextEnvelope $ toUniverseContextDelta delta
                      Left err -> Left $ show err

instance signVersionedUniverseContextDelta :: SignVersionedDelta UniverseContextDelta where
  signVersionedDelta (UniverseContextDelta delta) = do
    subject <- versionedRoleParts delta.subject
    contextType <- versionedContextType delta.contextType
    signDelta $ writeJSON
      { deltaFormatVersion: currentDeltaFormatVersion
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
    subject <- versionedRoleParts delta.subject
    contextType <- versionedContextType delta.contextType
    roleType <- versionedEnumeratedRoleType delta.roleType
    authorizedRole <- traverse versionedRoleParts delta.authorizedRole
    signDelta $ writeJSON
      { deltaFormatVersion: currentDeltaFormatVersion
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
    subject <- versionedRoleParts delta.subject
    contextType <- versionedContextType delta.contextType
    roleType <- versionedEnumeratedRoleType delta.roleType
    destinationContextType <- traverse versionedContextType delta.destinationContextType
    signDelta $ writeJSON
      { deltaFormatVersion: currentDeltaFormatVersion
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
    subject <- versionedRoleParts delta.subject
    filledType <- versionedEnumeratedRoleType delta.filledType
    fillerType <- traverse versionedEnumeratedRoleType delta.fillerType
    oldFillerType <- traverse versionedEnumeratedRoleType delta.oldFillerType
    signDelta $ writeJSON
      { deltaFormatVersion: currentDeltaFormatVersion
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
    subject <- versionedRoleParts delta.subject
    roleType <- versionedEnumeratedRoleType delta.roleType
    property <- versionedEnumeratedPropertyType delta.property
    signDelta $ writeJSON
      { deltaFormatVersion: currentDeltaFormatVersion
      , subject: subject.reference
      , subjectKind: subject.kind
      , resourceKey: delta.resourceKey
      , resourceVersion: delta.resourceVersion
      , id: delta.id
      , roleType
      , property
      , values: delta.values
      , deltaType: delta.deltaType
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
