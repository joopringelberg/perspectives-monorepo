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

import Data.Array (catMaybes, concat, concatMap, elem, filter, foldl, last, mapMaybe, nub)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty, fromFoldable, insert, lookup, values) as OBJ
import Perspectives.CoreTypes (MP)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ModelDependencies (onlookers) as MD
import Perspectives.Query.QueryTypes (RoleInContext) as QT
import Perspectives.Representation.ADT (allLeavesInADT, equalsOrSpecialises_)
import Perspectives.Representation.CNF (CNF)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (completeDeclaredFillerRestriction, rangeOfRoleCalculation, roleADT, toConjunctiveNormalForm_)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), PropertyType(..), RoleKind(..))
import Perspectives.Sidecar.StableIdMapping (Stable) as Sidecar
import Perspectives.Types.ObjectGetters (propertiesInPerspective)
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
    -- 1a. Onlooker enumerated user roles: any EnumeratedRole whose filledBy restriction
    --     chain includes the sys:TheWorld$Onlookers type (directly or via specialisation).
    onlookerERoles <- catMaybes <$> traverse (toOnlooker dfr.enumeratedRoles) (OBJ.values dfr.enumeratedRoles)

    -- 1b. Onlooker calculated user roles: any CalculatedRole (UserRole) whose type
    --     expansion (roleADT → toConjunctiveNormalForm_) specialises sys:TheWorld$Onlookers.
    onlookerCRoles <- catMaybes <$> traverse toOnlookerCR (OBJ.values dfr.calculatedRoles)

    -- 2. Collect all perspectives from Onlookers (both enumerated and calculated)
    allPerspectives <- pure $
      concatMap (\(EnumeratedRole r) -> r.perspectives) onlookerERoles
        <> concatMap (\(CalculatedRole r) -> r.perspectives) onlookerCRoles

    -- 3. For each perspective collect (ENR, perspective-visible-properties) pairs.
    --    perspectiveToENRsWithProps uses rangeOfRoleCalculation to resolve calculated roles
    --    (e.g. `filter X with Y` → X's ENR), and calls propertiesInPerspective (which
    --    resolves Universal to actual role properties via allProperties).
    enrWithPropsNested <- traverse perspectiveToENRsWithProps allPerspectives
    let
      allEnrWithProps = concat enrWithPropsNested

      -- Merge: for the same ENR appearing in multiple perspectives, union the property sets.
      enrPropMap :: OBJ.Object (Array PropertyType)
      enrPropMap = foldl
        ( \acc (Tuple ert props) ->
            let
              key = unwrap ert
            in
              case OBJ.lookup key acc of
                Nothing -> OBJ.insert key props acc
                Just existing -> OBJ.insert key (nub $ existing <> props) acc
        )
        OBJ.empty
        allEnrWithProps
      enumeratedRoleTypes = nub $ map (\(Tuple ert _) -> ert) allEnrWithProps

    -- 4. Build one role table per ENR, filtering columns to perspective-visible properties only.
    roleTables <- pure $ mapMaybe
      ( \ert -> buildRoleTable dfr.enumeratedRoles dfr.enumeratedProperties
          (fromMaybe [] $ OBJ.lookup (unwrap ert) enrPropMap)
          ert
      )
      enumeratedRoleTypes

    -- 5. Universal context table (single table, referenced by all role tables)
    ctxTable <- pure buildUniversalContextTable

    -- 6. Name map: stable → readable for every ENR and its perspective-visible properties
    nameMap <- pure $ buildNameMap dfr.enumeratedRoles dfr.enumeratedProperties enrPropMap enumeratedRoleTypes
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
toOnlooker _ er = bindingEqualsOrSpecialisesOnlookers er >>= case _ of
  true -> pure $ Just er
  false -> pure Nothing

-- | Returns true if the filler restriction of an EnumeratedRole equals or specialises the
-- | sys:TheWorld$Onlookers type (using full CNF expansion of the declared filler restriction).
bindingEqualsOrSpecialisesOnlookers :: EnumeratedRole -> MP Boolean
bindingEqualsOrSpecialisesOnlookers er@(EnumeratedRole { kindOfRole }) =
  if kindOfRole == UserRole then do
    (mrestriction :: Maybe (CNF QT.RoleInContext)) <- completeDeclaredFillerRestriction er >>= traverse toConjunctiveNormalForm_
    onlookersCNF <- getEnumeratedRole (EnumeratedRoleType MD.onlookers) >>= roleADT >>= toConjunctiveNormalForm_
    case mrestriction of
      Nothing -> pure false
      Just restriction -> pure (restriction `equalsOrSpecialises_` onlookersCNF)
  else pure false

-- | Returns Just the CalculatedRole if it is an Onlooker: a UserRole whose type expansion
-- | (the range of its calculation, fully expanded via toConjunctiveNormalForm_) specialises
-- | or equals sys:TheWorld$Onlookers.
-- |
-- | The expansion uses `roleADT` (range of the calculation) followed by `toConjunctiveNormalForm_`,
-- | which resolves each leaf EnumeratedRoleType to its pre-computed `completeType` CNF field.
-- | That `completeType` includes the role's own type, its aspects, and its filler restrictions —
-- | so if the yielded role type has `filledBy Onlookers`, this check returns true.
toOnlookerCR :: CalculatedRole -> MP (Maybe CalculatedRole)
toOnlookerCR cr@(CalculatedRole { kindOfRole }) =
  if kindOfRole == UserRole then do
    crCNF <- roleADT cr >>= toConjunctiveNormalForm_
    onlookersCNF <- getEnumeratedRole (EnumeratedRoleType MD.onlookers) >>= roleADT >>= toConjunctiveNormalForm_
    if crCNF `equalsOrSpecialises_` onlookersCNF then pure $ Just cr
    else pure Nothing
  else pure Nothing

-------------------------------------------------------------------------------
---- PERSPECTIVE → ENR RESOLUTION
-------------------------------------------------------------------------------

-- | Extract all leaf EnumeratedRoleTypes from a Perspective.
-- |
-- | For each RoleType in the perspective, `rangeOfRoleCalculation` fetches the
-- | compiled range type from the PDR cache (available because we run inside
-- | `withStableDomeinFile`).  The range is an ADT of EnumeratedRoleType;
-- | `allLeavesInADT` collects all leaf ENRTs, handling SUM / PROD structures.
-- |
-- | Calculated roles such as `filter Aanwezigen with Aanwezig` compile to a
-- | composition whose range type is still `Aanwezigen` (FilterF preserves
-- | the output type), so this approach subsumes the old manual QFD-walking
-- | and is correct per §4.6 design decision.
perspectiveToENRs :: Perspective -> MP (Array EnumeratedRoleType)
perspectiveToENRs (Perspective p) = do
  adts <- traverse rangeOfRoleCalculation p.roleTypes
  pure $ nub $ concatMap allLeavesInADT adts

-- | For each leaf ENR in a perspective, pair it with the perspective's visible
-- | property set, resolved via `propertiesInPerspective` from
-- | Perspectives.Types.ObjectGetters.  For `Universal` property sets this
-- | correctly expands to all actual role properties (including aspect-inherited
-- | ones) by calling `allProperties`.
perspectiveToENRsWithProps :: Perspective -> MP (Array (Tuple EnumeratedRoleType (Array PropertyType)))
perspectiveToENRsWithProps p = do
  enrts <- perspectiveToENRs p
  props <- propertiesInPerspective p
  pure $ map (\ert -> Tuple ert props) enrts

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
-- | `visibleProps` restricts the columns to perspective-visible properties only
-- | (as resolved by `propertiesInPerspective`).  Only the role's own properties
-- | that also appear in `visibleProps` are emitted as columns.
-- | Calculated properties are always skipped (they are not materialised in SQL).
buildRoleTable
  :: OBJ.Object EnumeratedRole
  -> OBJ.Object EnumeratedProperty
  -> Array PropertyType
  -> EnumeratedRoleType
  -> Maybe TCPTableConfig
buildRoleTable erMap epMap visibleProps ert =
  case OBJ.lookup (unwrap ert) erMap of
    Nothing -> Nothing
    Just (EnumeratedRole r) ->
      let
        perspectiveProps = filter (\pt -> elem pt visibleProps) r.properties
        columns = mapMaybe (buildPropertyColumn epMap) perspectiveProps
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
-- | every EnumeratedRoleType and its perspective-visible EnumeratedPropertyTypes.
-- | `enrPropMap` carries the visible property set per ENR as a plain array
-- | (resolved by `propertiesInPerspective`).
buildNameMap
  :: OBJ.Object EnumeratedRole
  -> OBJ.Object EnumeratedProperty
  -> OBJ.Object (Array PropertyType)
  -> Array EnumeratedRoleType
  -> OBJ.Object String
buildNameMap erMap epMap enrPropMap erts =
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
        Just (EnumeratedRole r) ->
          let
            props = fromMaybe [] $ OBJ.lookup (unwrap ert) enrPropMap
          in
            filter (\pt -> elem pt props) r.properties
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
