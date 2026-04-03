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

import Control.Monad.Error.Class (catchError)
import Data.Array (catMaybes, concat, concatMap, elem, filter, foldl, head, last, mapMaybe, nub)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty, fromFoldable, insert, lookup, values) as OBJ
import Perspectives.CoreTypes (MP)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ModelDependencies (onlookers) as MD
import Perspectives.Query.QueryTypes (RoleInContext, roleInContext2Role) as QT
import Perspectives.Representation.ADT (ADT, allLeavesInADT, equalsOrSpecialises_)
import Perspectives.Representation.CNF (CNF)
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.PersistentType (getEnumeratedProperty, getEnumeratedRole)
import Perspectives.Representation.Class.Role (allLocallyOnRoleRepresentedProperties, completeDeclaredFillerRestriction, rangeOfRoleCalculation, roleADT, toConjunctiveNormalForm_)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Perspective (Perspective(..))
import Perspectives.Representation.Range (Range(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleKind(..))
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
  -- | When present, the property is not stored directly on this role's table
  -- | but must be fetched via N filler hops.  Each element is the stable
  -- | EnumeratedRoleType ID of the role table at that hop; the property is on
  -- | the LAST role in the chain.  TCP uses this to construct JOIN queries and
  -- | CREATE VIEW statements.
  , fillerChain :: Maybe (Array String)
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
    , fillerChain: c.fillerChain
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
                Nothing -> OBJ.insert key (nub props) acc
                Just existing -> OBJ.insert key (nub $ existing <> props) acc
        )
        OBJ.empty
        allEnrWithProps
      enumeratedRoleTypes = nub $ map (\(Tuple ert _) -> ert) allEnrWithProps

    -- 4. Build one role table per ENR.
    --    buildRoleTable is now monadic: it calls allLocallyOnRoleRepresentedProperties to
    --    distinguish direct columns from filler-chain columns (§4.5 / §4.7 design).
    roleTables <- catMaybes <$> traverse
      ( \ert -> buildRoleTable dfr.enumeratedRoles dfr.enumeratedProperties
          (fromMaybe [] $ OBJ.lookup (unwrap ert) enrPropMap)
          ert
      )
      enumeratedRoleTypes

    -- 5. Universal context table (single table, referenced by all role tables)
    let ctxTable = buildUniversalContextTable

    -- 6. Collect all filler-chain role type IDs from generated columns so they can
    --    be added to the nameMap for TCP's JOIN / view queries.
    let fillerChainRoleIds = nub $ concatMap collectFillerChainRoleIds roleTables

    -- 7. Name map: stable → readable for every ENR, its perspective-visible properties,
    --    and any role types appearing in filler chains (may be from other models).
    nameMap <- buildNameMap dfr.enumeratedRoles dfr.enumeratedProperties enrPropMap enumeratedRoleTypes fillerChainRoleIds
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
  -- Apply nub: propertiesInPerspective accumulates over all states and may
  -- return the same PropertyType multiple times when a property is declared
  -- in more than one state of the perspective.
  props <- nub <$> propertiesInPerspective p
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
-- |
-- | `visibleProps` restricts the columns to perspective-visible properties only
-- | (as resolved by `propertiesInPerspective`).
-- |
-- | Properties are split into two groups:
-- |   1. **Direct columns** — properties in `r.properties` that are also in
-- |      `visibleProps`.  These are stored on this role's own table row.
-- |   2. **Filler-chain columns** — properties in `visibleProps` that are NOT
-- |      locally accessible on the role (not in `allLocallyOnRoleRepresentedProperties`,
-- |      which covers the role itself and its aspects but excludes the filler chain).
-- |      These need N JOIN hops through filler tables; `computeFillerChain` locates
-- |      the destination role and emits the hop list in `fillerChain`.
buildRoleTable
  :: OBJ.Object EnumeratedRole
  -> OBJ.Object EnumeratedProperty
  -> Array PropertyType
  -> EnumeratedRoleType
  -> MP (Maybe TCPTableConfig)
buildRoleTable erMap epMap visibleProps ert =
  case OBJ.lookup (unwrap ert) erMap of
    Nothing -> pure Nothing
    Just er@(EnumeratedRole r) -> do
      -- Properties locally accessible on the role or its aspects (no filler hops).
      localProps <- allLocallyOnRoleRepresentedProperties er

      -- Direct columns: in r.properties (directly declared) AND perspective-visible.
      let directProps = filter (\pt -> elem pt visibleProps) r.properties
      let directColumns = mapMaybe (buildPropertyColumn epMap) directProps

      -- Filler-chain columns: perspective-visible but NOT locally accessible.
      let fillerProps = filter (\pt -> not (elem pt localProps)) visibleProps
      fillerColumns <- catMaybes <$> traverse (\pt -> buildFillerChainColumn pt er) fillerProps

      let tableName = localName (unwrap r.readableName)
      pure $ Just
        { name: tableName
        , roleType: Just (unwrap ert)
        , contextType: Nothing
        , isUniversalContextTable: false
        , columns: directColumns <> fillerColumns
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
        , fillerChain: Nothing
        }
buildPropertyColumn _ (CP _) = Nothing

-- | Build a filler-chain column for a property that is NOT locally on the role.
-- |
-- | Calls `computeFillerChain` to discover the sequence of role type IDs that
-- | must be joined to reach the property, then looks up the property details via
-- | `getEnumeratedProperty` (which searches the full PDR cache, not just the
-- | current DomeinFile, as the property may belong to another model).
buildFillerChainColumn :: PropertyType -> EnumeratedRole -> MP (Maybe TCPColumnConfigJ)
buildFillerChainColumn (ENP ept) startRole = do
  mChain <- computeFillerChain (ENP ept) startRole
  case mChain of
    Nothing -> pure Nothing
    Just chain -> do
      -- `getEnumeratedProperty` looks in the full PDR cache (cross-model safe).
      -- If the property is not found, it means the PDR cache is incomplete —
      -- this should not happen for properties referenced by a valid perspective,
      -- but we fall back to Nothing to avoid aborting the entire config generation.
      mprop <- catchError (Just <$> getEnumeratedProperty ept) (const $ pure Nothing)
      pure $ case mprop of
        Nothing -> Nothing
        Just (EnumeratedProperty p) ->
          Just $ TCPColumnConfigJ
            { name: localName (unwrap p.readableName)
            , sqlType: rangeToSQLType p.range
            , nullable: true
            , propertyType: Just (unwrap ept)
            , fillerChain: Just chain
            }
buildFillerChainColumn (CP _) _ = pure Nothing

-- | Walk the filler (binding) chain of `startRole` to find which role type carries
-- | `pt` locally (on the role or one of its aspects, but NOT via a further filler).
-- |
-- | Returns `Nothing` when:
-- |   * The property is already locally on `startRole` (no filler hop needed).
-- |   * No role within `maxFillerDepth` hops carries the property locally.
-- |   * The binding chain runs out before the property is found.
-- |
-- | Returns `Just chain` where `chain` is the ordered list of
-- | `EnumeratedRoleType` stable IDs from hop 1 to (and including) the role that
-- | carries the property.  TCP uses this list to build LEFT JOIN clauses.
-- |
-- | When the binding ADT at a hop is a SUM (union), the first leaf is chosen
-- | (any path works per the §Q1 design decision: the PDR guarantees uniform
-- | hop count across all union branches).
computeFillerChain :: PropertyType -> EnumeratedRole -> MP (Maybe (Array String))
computeFillerChain pt (EnumeratedRole r) = do
  localProps <- allLocallyOnRoleRepresentedProperties (EnumeratedRole r)
  if elem pt localProps
    then pure Nothing
    else walkBinding maxFillerDepth [] r.binding
  where
  walkBinding :: Int -> Array String -> Maybe (ADT QT.RoleInContext) -> MP (Maybe (Array String))
  walkBinding 0 _ _ = pure Nothing
  walkBinding _ _ Nothing = pure Nothing
  walkBinding remaining acc (Just bindingADT) =
    case head (allLeavesInADT bindingADT) of
      Nothing -> pure Nothing
      Just leaf -> do
        let ert = QT.roleInContext2Role leaf
        leafRole <- getEnumeratedRole ert
        localProps <- allLocallyOnRoleRepresentedProperties leafRole
        let chainWithLeaf = acc <> [ unwrap ert ]
        if elem pt localProps
          then pure $ Just chainWithLeaf
          else do
            let (EnumeratedRole lr) = leafRole
            walkBinding (remaining - 1) chainWithLeaf lr.binding

-- | Maximum filler-chain depth for SQL JOIN unrolling.
-- | Matches the §Q1 design decision (max 5, extendable to 6).
maxFillerDepth :: Int
maxFillerDepth = 5

-- | Extract all filler-chain role type IDs from a table's columns.
-- | Used to collect the complete set of filler-chain role types so their
-- | readable names can be added to the nameMap.
collectFillerChainRoleIds :: TCPTableConfig -> Array String
collectFillerChainRoleIds table =
  concatMap (\(TCPColumnConfigJ col) -> fromMaybe [] col.fillerChain) table.columns

-------------------------------------------------------------------------------
---- NAME MAP
-------------------------------------------------------------------------------

-- | Build a map from stable identifiers to readable (model-source) names for
-- | every EnumeratedRoleType, its perspective-visible EnumeratedPropertyTypes,
-- | and any role types that appear in filler chains (which may be from other
-- | models and are needed by TCP to resolve JOIN table names).
-- |
-- | `enrPropMap` carries the visible property set per ENR as a plain array
-- | (resolved by `propertiesInPerspective`).
-- | `fillerChainRoleIds` is the union of all `fillerChain` arrays from all
-- | generated columns; these role types are looked up via `getEnumeratedRole`
-- | (full PDR cache) as they may belong to other models.
buildNameMap
  :: OBJ.Object EnumeratedRole
  -> OBJ.Object EnumeratedProperty
  -> OBJ.Object (Array PropertyType)
  -> Array EnumeratedRoleType
  -> Array String
  -> MP (OBJ.Object String)
buildNameMap erMap epMap enrPropMap erts fillerChainRoleIds = do
  fillerEntries <- catMaybes <$> traverse lookupFillerRoleName fillerChainRoleIds
  pure $ OBJ.fromFoldable $ staticEntries <> fillerEntries
  where
  -- Static entries: current-model ENRs and their perspective-visible ENPs.
  staticEntries :: Array (Tuple String String)
  staticEntries = roleEntries <> propEntries

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

  -- Look up a filler-chain role type by its stable ID.  First try the
  -- current model's erMap (fast path), then fall back to the PDR cache.
  -- Silent failure is acceptable: roles from other models may not be in the
  -- cache if they have not been loaded, but in a running PDR the sys model
  -- is always present and other dependent models are loaded on demand.
  lookupFillerRoleName :: String -> MP (Maybe (Tuple String String))
  lookupFillerRoleName ertId =
    case OBJ.lookup ertId erMap of
      Just (EnumeratedRole r) -> pure $ Just (Tuple ertId (unwrap r.readableName))
      Nothing -> do
        mRole <- catchError (Just <$> getEnumeratedRole (EnumeratedRoleType ertId)) (const $ pure Nothing)
        pure $ case mRole of
          Just (EnumeratedRole r) -> Just (Tuple ertId (unwrap r.readableName))
          Nothing -> Nothing

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
