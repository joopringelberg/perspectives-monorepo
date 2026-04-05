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
import Data.Array (any, catMaybes, concat, concatMap, elem, filter, foldl, last, length, mapMaybe, mapWithIndex, null, nub)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, empty, fromFoldable, insert, keys, lookup, member, values) as OBJ
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

    -- 4b. Build tables for intermediate and endpoint filler-chain hop roles that are
    --     not already represented in roleTables.  The TCP schema generator creates
    --     SQL views that LEFT JOIN through these tables; they must exist in the database.
    hopTables <- buildHopRoleTables roleTables

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
      , tables: [ ctxTable ] <> roleTables <> hopTables
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
-- |      These need N JOIN hops through filler tables; `computeFillerChains` locates
-- |      all destination roles (one per union branch) and emits the hop lists in `fillerChain`.
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
      fillerColumnsNested <- traverse (\pt -> buildFillerChainColumn pt er) fillerProps
      let fillerColumns = concat fillerColumnsNested

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

-- | Build filler-chain column entries for a property that is NOT locally on the role.
-- |
-- | Calls `computeFillerChains` to discover ALL possible chains of role type IDs
-- | (one per union branch in the binding ADT) that must be joined to reach the
-- | property.  Returns one `TCPColumnConfigJ` per chain.
-- |
-- | When the binding ADT has a union at the endpoint hop (e.g. `Persons` can be
-- | filled by both `PerspectivesUsers` and `NonPerspectivesUsers`), this function
-- | returns a column entry for EACH branch.  The SQL generator uses COALESCE to
-- | merge them when building the view.
buildFillerChainColumn :: PropertyType -> EnumeratedRole -> MP (Array TCPColumnConfigJ)
buildFillerChainColumn (ENP ept) startRole = do
  chains <- computeFillerChains (ENP ept) startRole
  if null chains then pure []
  else do
    -- `getEnumeratedProperty` looks in the full PDR cache (cross-model safe).
    -- If the property is not found, it means the PDR cache is incomplete —
    -- this should not happen for properties referenced by a valid perspective,
    -- but we fall back to [] to avoid aborting the entire config generation.
    mprop <- catchError (Just <$> getEnumeratedProperty ept) (const $ pure Nothing)
    pure $ case mprop of
      Nothing -> []
      Just (EnumeratedProperty p) ->
        map
          ( \chain -> TCPColumnConfigJ
              { name: localName (unwrap p.readableName)
              , sqlType: rangeToSQLType p.range
              , nullable: true
              , propertyType: Just (unwrap ept)
              , fillerChain: Just chain
              }
          )
          chains
buildFillerChainColumn (CP _) _ = pure []

-- | Walk the filler (binding) chain of `startRole` to find ALL role types that
-- | carry `pt` locally (on the role or one of its aspects, but NOT via a further
-- | filler).
-- |
-- | Returns an empty array when:
-- |   * The property is already locally on `startRole` (no filler hop needed).
-- |   * No role within `maxFillerDepth` hops carries the property locally.
-- |   * The binding chain runs out before the property is found.
-- |
-- | Returns one element per union branch in the binding ADT at the endpoint hop.
-- | For example, if `Persons.binding = SUM [PerspectivesUsers, NonPerspectivesUsers]`
-- | and both carry `FirstName`, this returns two chains:
-- |   [ ["Deelnemer", "Persons", "PerspectivesUsers"]
-- |   , ["Deelnemer", "Persons", "NonPerspectivesUsers"] ]
-- |
-- | All returned chains have the same length (guaranteed by the runtime's
-- | uniform-hop-count invariant, §Q1 design decision).  TCP uses these chains
-- | to build COALESCE LEFT JOIN clauses in the view SQL.
computeFillerChains :: PropertyType -> EnumeratedRole -> MP (Array (Array String))
computeFillerChains pt (EnumeratedRole r) = do
  localProps <- allLocallyOnRoleRepresentedProperties (EnumeratedRole r)
  if elem pt localProps then pure []
  else walkBinding maxFillerDepth [] r.binding
  where
  walkBinding :: Int -> Array String -> Maybe (ADT QT.RoleInContext) -> MP (Array (Array String))
  walkBinding 0 _ _ = pure []
  walkBinding _ _ Nothing = pure []
  walkBinding remaining acc (Just bindingADT) = do
    let leaves = allLeavesInADT bindingADT
    -- Walk ALL leaves (handles SUM / union: each branch may be a different table).
    chainsFromAllLeaves <- traverse (processLeaf remaining acc) leaves
    pure $ concat chainsFromAllLeaves

  processLeaf :: Int -> Array String -> QT.RoleInContext -> MP (Array (Array String))
  processLeaf remaining acc leaf = do
    let ert = QT.roleInContext2Role leaf
    leafRole <- getEnumeratedRole ert
    localProps <- allLocallyOnRoleRepresentedProperties leafRole
    let chainWithLeaf = acc <> [ unwrap ert ]
    if elem pt localProps
    -- This leaf carries the property: return this complete chain.
    then pure [ chainWithLeaf ]
    -- Otherwise recurse deeper (while depth budget remains).
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
---- HOP ROLE TABLE BUILDERS
-------------------------------------------------------------------------------

-- | Build TCPTableConfig entries for all intermediate and endpoint filler-chain
-- | hop roles that are not already represented in the directly-visible role tables.
-- |
-- | For each unique role ID appearing in any `fillerChain` array:
-- |   - If already represented in `existingTables`, skip it.
-- |   - Otherwise, build a new TableConfig via `buildHopRoleTable`.
-- |
-- | Pass-through hops (roles that are neither the direct ENR target nor the
-- | property bearer for this chain) receive an empty `columns` array; the TCP
-- | schema generator still adds the required `id`, `context_id`, and `filler_id`
-- | columns automatically because `roleType` is set.
-- |
-- | Endpoint hops (last element in a `fillerChain`) also receive a direct column
-- | entry for the property stored there.  When the same role is the endpoint for
-- | more than one property (across different perspectives / chains), all endpoint
-- | properties are merged and deduplicated.
buildHopRoleTables :: Array TCPTableConfig -> MP (Array TCPTableConfig)
buildHopRoleTables existingTables = do
  let
    existingRoleIds = catMaybes $ map _.roleType existingTables

    -- Collect all (roleId, Maybe endpointColInfo) from every filler-chain column
    -- in every existing role table.
    allHopEntries
      :: Array
           { roleId :: String
           , endpointCol :: Maybe { name :: String, sqlType :: String, nullable :: Boolean, propertyType :: Maybe String }
           }
    allHopEntries =
      concatMap (concatMap extractColHopRoles <<< _.columns) existingTables

    -- Group by roleId: accumulate endpoint column infos per role, deduplicating
    -- by propertyType so the same property is not listed twice.
    hopRoleMap
      :: OBJ.Object
           (Array { name :: String, sqlType :: String, nullable :: Boolean, propertyType :: Maybe String })
    hopRoleMap = foldl
      ( \acc { roleId, endpointCol } ->
          let
            existing = fromMaybe [] $ OBJ.lookup roleId acc
          in
            case endpointCol of
              Nothing ->
                -- Pass-through: ensure the role has an entry even if empty.
                if OBJ.member roleId acc then acc
                else OBJ.insert roleId [] acc
              Just col ->
                -- Endpoint: add the column if not already present for this prop.
                if any (\c -> c.propertyType == col.propertyType) existing then acc
                else OBJ.insert roleId (existing <> [ col ]) acc
      )
      OBJ.empty
      allHopEntries

    -- Only build tables for roles that do not already have a table.
    newHopRoleIds = filter (\id -> not (elem id existingRoleIds)) (OBJ.keys hopRoleMap)

  catMaybes <$> traverse
    ( \roleId ->
        buildHopRoleTable roleId (fromMaybe [] $ OBJ.lookup roleId hopRoleMap)
    )
    newHopRoleIds

-- | Extract (roleId, endpointCol) pairs from a single column config.
-- |
-- | For a filler-chain column with chain [r0, r1, r2]:
-- |   r0 → { roleId: r0, endpointCol: Nothing }   (pass-through)
-- |   r1 → { roleId: r1, endpointCol: Nothing }   (pass-through)
-- |   r2 → { roleId: r2, endpointCol: Just col }  (endpoint – property stored here)
extractColHopRoles
  :: TCPColumnConfigJ
  -> Array
       { roleId :: String
       , endpointCol :: Maybe { name :: String, sqlType :: String, nullable :: Boolean, propertyType :: Maybe String }
       }
extractColHopRoles (TCPColumnConfigJ col) = case col.fillerChain of
  Nothing -> []
  Just chain ->
    let
      len = length chain
      endpointInfo =
        { name: col.name
        , sqlType: col.sqlType
        , nullable: col.nullable
        , propertyType: col.propertyType
        }
    in
      mapWithIndex
        ( \i roleId ->
            { roleId
            , endpointCol: if i == len - 1 then Just endpointInfo else Nothing
            }
        )
        chain

-- | Build a single TableConfig for a hop role, looked up via the PDR cache.
-- |
-- | `endpointCols` contains the direct-property columns for properties stored
-- | on this role as the endpoint of one or more filler chains.  Pass-through
-- | roles have an empty `endpointCols`; the TCP schema generator still creates
-- | `id`, `context_id`, and `filler_id` columns automatically because `roleType`
-- | is set to a non-Nothing value.
-- |
-- | Silent failure: if the role is not found in the PDR cache (e.g. because the
-- | model has not been loaded), `Nothing` is returned and the caller silently
-- | skips the table.  This should not happen for perspectives that compiled
-- | successfully, but is safe to handle here.
buildHopRoleTable
  :: String
  -> Array { name :: String, sqlType :: String, nullable :: Boolean, propertyType :: Maybe String }
  -> MP (Maybe TCPTableConfig)
buildHopRoleTable roleId endpointCols = do
  mRole <- catchError (Just <$> getEnumeratedRole (EnumeratedRoleType roleId)) (const $ pure Nothing)
  pure $ case mRole of
    Nothing -> Nothing
    Just (EnumeratedRole r) ->
      let
        tableName = localName (unwrap r.readableName)
        columns = map
          ( \c -> TCPColumnConfigJ
              { name: c.name
              , sqlType: c.sqlType
              , nullable: c.nullable
              , propertyType: c.propertyType
              , fillerChain: Nothing
              }
          )
          endpointCols
      in
        Just
          { name: tableName
          , roleType: Just roleId
          , contextType: Nothing
          , isUniversalContextTable: false
          , columns
          }

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
