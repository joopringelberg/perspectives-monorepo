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

-- | Generates a TCP SQL configuration (schema + name map) from a compiled DomeinFile.
-- |
-- | The generator:
-- |   1. Finds all Onlooker roles: EnumeratedRoles whose filledBy restriction chain
-- |      includes the sys:TheWorld$Onlookers type (directly or via specialisation).
-- |   2. Collects every Perspective held by those Onlooker roles.
-- |   3. Resolves EnumeratedRoleTypes visible in each perspective, expanding CalculatedRoles
-- |      and dropping FilterF steps (§4.6 design decision).
-- |   4. Emits one SQL TableConfig per visible EnumeratedRoleType (plus a single universal
-- |      context table).
-- |   5. Produces a nameMap mapping stable identifiers to readable model source names for use
-- |      as SQL table / column names.
-- |
-- | Design reference: packages/perspectives-tcp/docs/pl-query-to-sql-design.md

module Perspectives.TCP.Configuration where

import Prelude

import Data.Array (any, catMaybes, concatMap, last, mapMaybe, nub)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable, lookup, values) as OBJ
import Perspectives.CoreTypes (MP)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ModelDependencies (onlookers) as MD
import Perspectives.Query.QueryTypes (Calculation(..), QueryFunctionDescription(..), RoleInContext(..))
import Perspectives.Query.QueryTypes (RoleInContext) as QT
import Perspectives.Representation.ADT (ADT, allLeavesInADT, equalsOrSpecialises_)
import Perspectives.Representation.CNF (CNF)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (completeDeclaredFillerRestriction, roleADT, toConjunctiveNormalForm_)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType, EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.Sidecar.StableIdMapping (Stable) as Sidecar
import Simple.JSON (class WriteForeign, write)

-------------------------------------------------------------------------------
---- TCP CONFIGURATION TYPES
---- (These mirror perspectives-tcp/src/config.ts TableConfig / ColumnConfig)
-------------------------------------------------------------------------------

type TCPColumnConfig =
  { name :: String
  -- "type" is a reserved word in PureScript; we use "sqlType" internally and
  -- rename to "type" in the WriteForeign instance below.
  , sqlType :: String
  , nullable :: Boolean
  , propertyType :: Maybe String
  }

-- | Write TCPColumnConfig as JSON, renaming `sqlType` back to the `type` key
-- | expected by the TCP.
newtype TCPColumnConfigJ = TCPColumnConfigJ TCPColumnConfig

instance WriteForeign TCPColumnConfigJ where
  writeImpl (TCPColumnConfigJ c) = write
    { name: c.name
    , type: c.sqlType
    , nullable: c.nullable
    , propertyType: c.propertyType
    }

type TCPTableConfig =
  { name :: String
  , roleType :: Maybe String
  , contextType :: Maybe String
  -- | When true, this is the single universal context table that all role
  -- | tables' context_id columns reference.  The TCP schema generator uses this
  -- | flag to create `id` (PK) and `context_type` columns automatically.
  , isUniversalContextTable :: Boolean
  , columns :: Array TCPColumnConfigJ
  }

-- | The full configuration emitted by the PDR for a TCP subscriber.
-- | The `tables` array is a drop-in replacement for `TCPConfig.schema.tables`
-- | in perspectives-tcp.  The operator must merge it with the broker and
-- | database settings before starting the TCP process.
type TCPConfiguration =
  { modelUri :: String
  , nameMap :: OBJ.Object String
  , tables :: Array TCPTableConfig
  }

-------------------------------------------------------------------------------
---- ENTRY POINT
-------------------------------------------------------------------------------

-- | Build a `TCPConfiguration` from a compiled (stable) DomeinFile.
-- | `modelUri` is the versioned model URI used as a reference key.
buildTCPConfiguration :: DomeinFile Sidecar.Stable -> String -> MP TCPConfiguration
buildTCPConfiguration (DomeinFile dfr) modelUri =
  do
    -- 1. Onlooker user roles: any EnumeratedRole whose filledBy restriction chain
    --    includes the sys:TheWorld$Onlookers type (directly or via specialisation)
    onlookerRoles <- catMaybes <$> traverse (toOnlooker dfr.enumeratedRoles) (OBJ.values dfr.enumeratedRoles)

    -- 2. Collect all perspectives from Onlookers
    allPerspectives <- pure $ concatMap (\(EnumeratedRole r) -> r.perspectives) onlookerRoles

    -- 3. For each perspective resolve to leaf EnumeratedRoleTypes
    --    (FilterF steps dropped per §4.6)
    enumeratedRoleTypes <- pure $ nub $ concatMap (perspectiveToENRs dfr.calculatedRoles) allPerspectives

    -- 4. Build one role table per ENR
    roleTables <- pure $ mapMaybe (buildRoleTable dfr.enumeratedRoles dfr.enumeratedProperties) enumeratedRoleTypes

    -- 5. Universal context table (single table, referenced by all role tables)
    ctxTable <- pure $ buildUniversalContextTable

    -- 6. Name map: stable → readable for every ENR (and its properties) encountered
    nameMap <- pure $ buildNameMap dfr.enumeratedRoles dfr.enumeratedProperties enumeratedRoleTypes
    pure
      { modelUri
      , nameMap
      , tables: [ ctxTable ] <> roleTables
      }

-------------------------------------------------------------------------------
---- ONLOOKER DETECTION
-------------------------------------------------------------------------------

-- | An Onlooker role is an EnumeratedRole whose filledBy restriction chain
-- | includes the sys:TheWorld$Onlookers type (directly or via specialisation).
-- | The filler restriction is walked up to 6 hops deep (depths 0–5), covering
-- | the §Q1 design decision (max 5, extendable to 6).
toOnlooker :: OBJ.Object EnumeratedRole -> EnumeratedRole -> MP (Maybe EnumeratedRole)
toOnlooker erMap er@(EnumeratedRole r) = bindingEqualsOrSpecialisesOnlookers er >>= case _ of
  true -> pure $ Just er
  false -> pure Nothing

-- | Returns true if the binding ADT (the filledBy restriction) contains the
-- | Onlookers type anywhere in the recursive filler chain.
-- | `depth` is defensive: the PDR runtime prevents actual filler cycles at
-- | data-entry time, but this guard protects against unexpected model states
-- | during compilation (e.g. draft models not yet validated by the runtime).
bindingIncludesOnlooker :: OBJ.Object EnumeratedRole -> Int -> Maybe (ADT RoleInContext) -> Boolean
bindingIncludesOnlooker _ _ Nothing = false
bindingIncludesOnlooker _ depth _ | depth > 5 = false
bindingIncludesOnlooker erMap depth (Just binding) =
  any
    ( \(RoleInContext { role }) ->
        unwrap role == MD.onlookers
          || case OBJ.lookup (unwrap role) erMap of
            Nothing -> false
            Just (EnumeratedRole r) -> bindingIncludesOnlooker erMap (depth + 1) r.binding
    )
    (allLeavesInADT binding)

-- | Alternative approach: check if the binding restriction equals or specialises the Onlookers type.
-- | `bindingIncludesOnlooker` is incomplete as it ignores Aspects. `bindingEqualsOrSpecialisesOnlookers` is more complete but also more expensive to compute, as it requires retrieving and CNF-transforming the full binding restriction of each role.
-- | Furthermore, it would require computing `buildTCPConfiguration` in the MP monad, as it needs to retrieve models from the repository. For these reasons, we currently use `bindingIncludesOnlooker`.
bindingEqualsOrSpecialisesOnlookers :: EnumeratedRole -> MP Boolean
bindingEqualsOrSpecialisesOnlookers er = do
  (mrestriction :: Maybe (CNF QT.RoleInContext)) <- completeDeclaredFillerRestriction er >>= traverse toConjunctiveNormalForm_
  onlookersCNF <- getEnumeratedRole (EnumeratedRoleType MD.onlookers) >>= roleADT >>= toConjunctiveNormalForm_
  case mrestriction of
    Nothing -> pure false
    Just restriction -> pure (restriction `equalsOrSpecialises_` onlookersCNF)

-------------------------------------------------------------------------------
---- PERSPECTIVE → ENR RESOLUTION
-------------------------------------------------------------------------------

-- | Extract all leaf EnumeratedRoleTypes from a Perspective.
-- | Uses `roleTypes` from the Perspective record as the starting set and
-- | expands any CalculatedRoleType to its underlying ENRs.
perspectiveToENRs :: OBJ.Object CalculatedRole -> Perspective -> Array EnumeratedRoleType
perspectiveToENRs crMap (Perspective p) =
  nub $ concatMap (roleTypeToENRs crMap) p.roleTypes

roleTypeToENRs :: OBJ.Object CalculatedRole -> RoleType -> Array EnumeratedRoleType
roleTypeToENRs _ (ENR ert) = [ ert ]
roleTypeToENRs crMap (CR crt) = extractENRsFromCR crMap crt

-- | Walk the QueryFunctionDescription of a CalculatedRole, collecting ENRs
-- | and dropping FilterF nodes (per §4.6).
extractENRsFromCR :: OBJ.Object CalculatedRole -> CalculatedRoleType -> Array EnumeratedRoleType
extractENRsFromCR crMap crt =
  case OBJ.lookup (unwrap crt) crMap of
    Nothing -> []
    Just (CalculatedRole cr) ->
      case cr.calculation of
        Q qfd -> extractENRsFromQFD crMap qfd
        _ -> []

-- | Recursively extract EnumeratedRoleTypes from a QueryFunctionDescription.
-- | Key rules (per design doc):
-- |   * FilterF (UQD _ FilterF inner …)  → drop filter, recurse into `inner`
-- |   * RolGetter (ENR ert)               → [ert]
-- |   * RolGetter (CR crt)                → expand CalculatedRole recursively
-- |   * BinaryCombinator ComposeF         → result type is from right operand
-- |   * BinaryCombinator UnionF / IntersectionF → both operands
-- |   * UnaryCombinator _                 → pass through to inner
extractENRsFromQFD :: OBJ.Object CalculatedRole -> QueryFunctionDescription -> Array EnumeratedRoleType
extractENRsFromQFD crMap = go
  where
  go (SQD _ (RolGetter (ENR ert)) _ _ _) = [ ert ]
  go (SQD _ (RolGetter (CR crt)) _ _ _) = extractENRsFromCR crMap crt
  go (UQD _ FilterF inner _ _ _) = go inner -- §4.6: peers pre-filter before forwarding to TCP; re-applying filters would yield empty results under the Closed World Assumption
  go (UQD _ (UnaryCombinator _) inner _ _ _) = go inner
  go (BQD _ (BinaryCombinator ComposeF) _ right _ _ _) = go right
  go (BQD _ (BinaryCombinator UnionF) left right _ _ _) = go left <> go right
  go (BQD _ (BinaryCombinator IntersectionF) left right _ _ _) = go left <> go right
  go _ = []

-------------------------------------------------------------------------------
---- TABLE BUILDERS
-------------------------------------------------------------------------------

-- | The single universal context table referenced by all role tables.
buildUniversalContextTable :: TCPTableConfig
buildUniversalContextTable =
  { name: "context"
  , roleType: Nothing
  , contextType: Nothing
  , isUniversalContextTable: true
  -- Standard columns (id PK, context_type) are added by the TCP schema
  -- generator when isUniversalContextTable == true.
  , columns: []
  }

-- | Build a role table for a single EnumeratedRoleType.
-- | Columns are derived from the role's enumerated properties only (calculated
-- | properties are not materialised in SQL).
buildRoleTable
  :: OBJ.Object EnumeratedRole
  -> OBJ.Object EnumeratedProperty
  -> EnumeratedRoleType
  -> Maybe TCPTableConfig
buildRoleTable erMap epMap ert =
  case OBJ.lookup (unwrap ert) erMap of
    Nothing -> Nothing
    Just (EnumeratedRole r) ->
      let
        columns = mapMaybe (buildPropertyColumn epMap) r.properties
        tableName = localName (unwrap r.readableName)
      in
        Just
          { name: tableName
          , roleType: Just (unwrap ert)
          , contextType: Nothing
          , isUniversalContextTable: false
          , columns
          }

-- | Build a column config for a single PropertyType.
-- | CalculatedPropertyTypes are skipped (they are not materialised).
buildPropertyColumn :: OBJ.Object EnumeratedProperty -> PropertyType -> Maybe TCPColumnConfigJ
buildPropertyColumn epMap (ENP ept) =
  case OBJ.lookup (unwrap ept) epMap of
    Nothing -> Nothing
    Just (EnumeratedProperty p) ->
      Just $ TCPColumnConfigJ
        { name: localName (unwrap p.readableName)
        , sqlType: rangeToSQLType p.range
        , nullable: true
        , propertyType: Just (unwrap ept)
        }
buildPropertyColumn _ (CP _) = Nothing

-------------------------------------------------------------------------------
---- NAME MAP
-------------------------------------------------------------------------------

-- | Build a map from stable identifiers to readable (model-source) names for
-- | every EnumeratedRoleType and EnumeratedPropertyType encountered.
buildNameMap
  :: OBJ.Object EnumeratedRole
  -> OBJ.Object EnumeratedProperty
  -> Array EnumeratedRoleType
  -> OBJ.Object String
buildNameMap erMap epMap erts =
  OBJ.fromFoldable $ roleEntries <> propEntries
  where
  roleEntries :: Array (Tuple String String)
  roleEntries = catMaybes $ map
    ( \ert -> case OBJ.lookup (unwrap ert) erMap of
        Nothing -> Nothing
        Just (EnumeratedRole r) -> Just (Tuple (unwrap ert) (unwrap r.readableName))
    )
    erts

  allPropertyTypes :: Array PropertyType
  allPropertyTypes = nub $ concatMap
    ( \ert -> case OBJ.lookup (unwrap ert) erMap of
        Nothing -> []
        Just (EnumeratedRole r) -> r.properties
    )
    erts

  propEntries :: Array (Tuple String String)
  propEntries = catMaybes $ map
    ( \pt -> case pt of
        ENP ept -> case OBJ.lookup (unwrap ept) epMap of
          Nothing -> Nothing
          Just (EnumeratedProperty p) -> Just (Tuple (unwrap ept) (unwrap p.readableName))
        CP _ -> Nothing
    )
    allPropertyTypes

-------------------------------------------------------------------------------
---- HELPERS
-------------------------------------------------------------------------------

-- | Convert a Perspectives `Range` to a TCP SQL column type string.
rangeToSQLType :: Range -> String
rangeToSQLType PString = "text"
rangeToSQLType PBool = "boolean"
rangeToSQLType PNumber = "real"
rangeToSQLType PDate = "datetime"
rangeToSQLType PDateTime = "datetime"
rangeToSQLType PTime = "datetime"
rangeToSQLType PEmail = "text"
rangeToSQLType PFile = "text"
rangeToSQLType PMarkDown = "text"
rangeToSQLType (PDuration _) = "text"

-- | Extract the local part of a qualified Perspectives name.
-- | E.g. "model://perspectives.domains#System$User" → "User"
-- |      "model://perspectives.domains#System"       → "System"
localName :: String -> String
localName qualifiedName =
  fromMaybe qualifiedName $ last $ split (Pattern "$") qualifiedName
