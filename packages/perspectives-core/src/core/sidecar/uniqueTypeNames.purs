-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2025 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
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

module Perspectives.Sidecar.UniqueTypeNames
  ( -- Key data extracted for heuristic matching (sidecar and fresh parse)
    ContextKey(..)
  , RoleKey(..)
  , PropertyKey(..)
  , ViewKey(..)
  , HeuristicKey(..)
  -- Weights and scoring
  , ContextWeights
  , RoleWeights
  , PropertyWeights
  , ViewWeights
  , class HeuristicMapping
  , defaultWeights
  , score
  , defaultContextWeights
  , defaultRoleWeights
  , defaultPropertyWeights
  , defaultViewWeights
  , Score
  -- Ranking helpers
  , rank
  , rankBest
  , applyStableIdMappingWith
  , extractKeysFromDfr
  , planCuidAssignments
  , finalizeCuidAssignments
  -- Refactor target: end-to-end mapping update with CUID minting for a model
  , updateStableMappingForModel
  ) where

import Prelude

import Data.Array (filter, find, findLastIndex, head, length, mapMaybe, nub, singleton, sortBy)
import Data.Either (Either(..))
import Data.Foldable (elem, foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.String.CodeUnits (drop, toCharArray)
import Data.String.CodeUnits as SCU
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.DomeinFile (DomeinFileRecord)
import Perspectives.ExecuteInTopologicalOrder (sortTopologicallyEither)
import Perspectives.Identifiers (isModelUri, typeUri2LocalName_, typeUri2typeNameSpace_, modelUri2SchemeAndAuthority)
import Perspectives.Query.QueryTypes (Calculation(..), Domain(..), range)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Context (enumeratedRoles)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.Action (Action)
import Perspectives.Representation.State (State(..)) as ST
import Perspectives.Representation.TypeIdentifiers (propertytype2string, roletype2string)
import Perspectives.Representation.View (View(..)) as VW
import Perspectives.Sidecar.HashQFD (qfdSignature)
import Perspectives.Sidecar.StableIdMapping (ActionKeySnapshot, ContextIndividualKeySnapshot, ContextKeySnapshot, PropertyKeySnapshot, RoleIndividualKeySnapshot, RoleKeySnapshot, Stable, StableIdMapping, StateKeySnapshot, ViewKeySnapshot, emptyStableIdMapping, idUriForContext, idUriForRole, ContextUri(..), RoleUri(..), ModelUri(..))
import Perspectives.Cuid2 (cuid2)
import Effect.Class (class MonadEffect, liftEffect)
import Perspectives.Representation.Perspective (Perspective(..), StateSpec, stateSpec2StateIdentifier)
import Perspectives.Data.EncodableMap (toUnfoldable) as EM

-- | We match three kinds for now (omit state types): Context, Role, Property
newtype ContextKey = ContextKey
  { fqn :: String -- fully qualified name (if known)
  , declaringContextFqn :: String -- fully qualified name of the declaring context (if known)
  , roles :: Array String -- child role type FQNs
  , properties :: Array String -- property type FQNs directly on the context role(s) if relevant
  , aspects :: Array String -- aspect type FQNs applied to this context
  }

newtype RoleKey = RoleKey
  { fqn :: String
  , declaringContextFqn :: String
  , properties :: Array String
  , bindingTypes :: Array String
  , aspects :: Array String
  }

newtype PropertyKey = PropertyKey
  { fqn :: String
  , valueType :: String -- value domain/type
  , facets :: Array String -- facets/flags (e.g., Mandatory, Calculated, etc.)
  , aspects :: Array String
  , declaringRoleFqn :: String
  }

data HeuristicKey
  = HContext ContextKey
  | HRole RoleKey
  | HProperty PropertyKey
  | HView ViewKey

-- | Scoring is a weighted sum of feature similarities.
type Score = Number

type ContextWeights =
  { fqnExact :: Number
  , rolesCommon :: Number
  , propertiesCommon :: Number
  , aspectsCommon :: Number
  , nameSimilarity :: Number
  }

type RoleWeights =
  { fqnExact :: Number
  , declaringContextExact :: Number
  , nameSimilarity :: Number
  , propertiesCommon :: Number
  , bindingTypesCommon :: Number
  , aspectsCommon :: Number
  }

type PropertyWeights =
  { fqnExact :: Number
  , valueTypeExact :: Number
  , facetsCommon :: Number
  , aspectsCommon :: Number
  , declaringRoleExact :: Number
  , nameSimilarity :: Number
  }

type ViewWeights =
  { fqnExact :: Number
  , declaringRoleExact :: Number
  , propertiesCommon :: Number
  , nameSimilarity :: Number
  }

-- | Multi-parameter class so each key kind can carry its own weight record.
class HeuristicMapping a w | a -> w where
  defaultWeights :: w
  score :: w -> a -> a -> Score

instance heuristicContext :: HeuristicMapping ContextKey ContextWeights where
  defaultWeights = defaultContextWeights
  score w (ContextKey a) (ContextKey b) =
    let
      -- typeUri2LocalName geeft een fout voor de domein naam.
      nameScore = compareContextIds a.fqn b.fqn
    in
      (if a.fqn == b.fqn then w.fqnExact else 0.0)
        + w.rolesCommon * commonFrac a.roles b.roles
        + w.propertiesCommon * commonFrac a.properties b.properties
        + w.aspectsCommon * commonFrac a.aspects b.aspects
        + w.nameSimilarity * nameScore
    where
    compareContextIds :: String -> String -> Number
    compareContextIds x y =
      if isModelUri x || isModelUri y then
        if x == y then 1.0
        else 0.0
      else nameSim (typeUri2LocalName_ x) (typeUri2LocalName_ y)

instance heuristicRole :: HeuristicMapping RoleKey RoleWeights where
  defaultWeights = defaultRoleWeights
  score w (RoleKey a) (RoleKey b) =
    let
      nameScore = nameSim (typeUri2LocalName_ a.fqn) (typeUri2LocalName_ b.fqn)
      declScore = if a.declaringContextFqn == b.declaringContextFqn then 1.0 else 0.0
    in
      (if a.fqn == b.fqn then w.fqnExact else 0.0)
        + w.declaringContextExact * declScore
        + w.nameSimilarity * nameScore
        + w.propertiesCommon * commonFrac a.properties b.properties
        + w.bindingTypesCommon * commonFrac a.bindingTypes b.bindingTypes
        + w.aspectsCommon * commonFrac a.aspects b.aspects

instance heuristicProperty :: HeuristicMapping PropertyKey PropertyWeights where
  defaultWeights = defaultPropertyWeights
  score w (PropertyKey a) (PropertyKey b) =
    let
      nameScore = nameSim (typeUri2LocalName_ a.fqn) (typeUri2LocalName_ b.fqn)
      valueTypeScore = if a.valueType == b.valueType then 1.0 else 0.0
      roleScore = if a.declaringRoleFqn == b.declaringRoleFqn then 1.0 else 0.0
    in
      (if a.fqn == b.fqn then w.fqnExact else 0.0)
        + w.valueTypeExact * valueTypeScore
        + w.facetsCommon * commonFrac a.facets b.facets
        + w.aspectsCommon * commonFrac a.aspects b.aspects
        + w.declaringRoleExact * roleScore
        + w.nameSimilarity * nameScore

newtype ViewKey = ViewKey
  { fqn :: String
  , declaringRoleFqn :: String
  , properties :: Array String
  }

instance heuristicView :: HeuristicMapping ViewKey ViewWeights where
  defaultWeights = defaultViewWeights
  score w (ViewKey a) (ViewKey b) =
    let
      nameScore = nameSim (typeUri2LocalName_ a.fqn) (typeUri2LocalName_ b.fqn)
      roleScore = if a.declaringRoleFqn == b.declaringRoleFqn then 1.0 else 0.0
    in
      (if a.fqn == b.fqn then w.fqnExact else 0.0)
        + w.declaringRoleExact * roleScore
        + w.propertiesCommon * commonFrac a.properties b.properties
        + w.nameSimilarity * nameScore

defaultContextWeights :: ContextWeights
defaultContextWeights =
  { fqnExact: 1000.0
  , rolesCommon: 5.0
  , propertiesCommon: 2.0
  , aspectsCommon: 1.0
  , nameSimilarity: 10.0
  }

defaultRoleWeights :: RoleWeights
defaultRoleWeights =
  { fqnExact: 1000.0
  , declaringContextExact: 50.0
  , nameSimilarity: 10.0
  , propertiesCommon: 2.0
  , bindingTypesCommon: 3.0
  , aspectsCommon: 1.0
  }

defaultPropertyWeights :: PropertyWeights
defaultPropertyWeights =
  { fqnExact: 1000.0
  , valueTypeExact: 6.0
  , facetsCommon: 2.0
  , aspectsCommon: 1.0
  , declaringRoleExact: 50.0
  , nameSimilarity: 10.0
  }

defaultViewWeights :: ViewWeights
defaultViewWeights =
  { fqnExact: 1000.0
  , declaringRoleExact: 50.0
  , propertiesCommon: 2.0
  , nameSimilarity: 10.0
  }

-- | Rank candidates (CUID × key) for a given probe key using the provided weights.
rank
  :: forall a w
   . HeuristicMapping a w
  => w
  -> a
  -> Array (Tuple String a) -- candidates: (cuid, key)
  -> Array (Tuple String Score)
rank w probe candidates =
  let
    withScores = candidates <#> \(Tuple cuid k) -> Tuple cuid (score w probe k)
  in
    sortBy (\(Tuple _ s1) (Tuple _ s2) -> compare s2 s1) withScores

-- | Return the best match above a minimal score (if any).
rankBest
  :: forall a w
   . HeuristicMapping a w
  => w
  -> Number -- minimal score to accept
  -> a
  -> Array (Tuple String a)
  -> Maybe (Tuple String Score)
rankBest w minScore probe candidates =
  case head (rank w probe candidates) of
    Just (Tuple cuid s) | s >= minScore -> Just (Tuple cuid s)
    _ -> Nothing

-- Helpers --------------------------------------------------------------------

-- | Fraction of common unique elements: |A ∩ B| / max(1, max(|A|, |B|))
commonFrac :: Array String -> Array String -> Number
commonFrac as bs =
  let
    as' = nub as
    bs' = nub bs
    interCount = length $ filter (\x -> elem x bs') as'
    denom = max (length as') (length bs')
  in
    if denom == 0 then 0.0 else toNumber interCount / toNumber denom

-- Jaccard similarity on unique character sets of two local names.
nameSim :: String -> String -> Number
nameSim a b =
  let
    as = nub (toCharArray a)
    bs = nub (toCharArray b)
    interCount = length $ filter (\c -> elem c bs) as
    denom = length as + length bs - interCount
  in
    if denom == 0 then if a == b then 1.0 else 0.0 else toNumber interCount / toNumber denom

-- Mapping hook ---------------------------------------------------------------

-- | Pure hook: apply a provided mapping and also compute the updated mapping snapshot.
-- | Returns the alias-augmented DomeinFileRecord and the updated mapping.
applyStableIdMappingWith :: forall f. StableIdMapping -> DomeinFileRecord f -> Tuple (DomeinFileRecord f) StableIdMapping
applyStableIdMappingWith mapping0 df =
  let
    cur = extractKeysFromDfr df
    mapping' = mergeStableIdMapping cur mapping0
  in
    Tuple (applyAliases mapping' df) mapping'

-- | Add alias keys (from the mapping file) that point to the canonical entries
-- | in the DomeinFileRecord. We NEVER rename the canonical identifiers here;
-- | we only add lookups under the alias keys for backwards/forwards compatibility.
applyAliases :: forall f. StableIdMapping -> DomeinFileRecord f -> DomeinFileRecord f
applyAliases mapping dfr@{ contexts, enumeratedRoles, enumeratedProperties, states, views } =
  dfr
    { contexts = aliasTable contexts mapping.contexts
    , enumeratedRoles = aliasTable enumeratedRoles mapping.roles
    , enumeratedProperties = aliasTable enumeratedProperties mapping.properties
    , views = aliasTable views mapping.views
    , states = aliasTable states mapping.states
    }
  where
  -- | Given a table of entities (by canonical key) and a mapping (alias -> canonicalKey),
  -- | insert entries under the alias that reference the same entity as the canonical key.
  aliasTable
    :: forall a
     . OBJ.Object a
    -> OBJ.Object String
    -> OBJ.Object a
  aliasTable table m =
    let
      ks = OBJ.keys m
    in
      foldl
        ( \acc aliasKey ->
            case OBJ.lookup aliasKey m of
              Nothing -> acc
              Just canonicalKey -> case OBJ.lookup canonicalKey table of
                Nothing -> acc
                Just entity -> OBJ.insert aliasKey entity acc
        )
        table
        ks

-- Extract lightweight feature keys from the current DomeinFileRecord for heuristics.
-- IMPORTANT: only snapshot canonical entries (skip alias keys). We consider an entry canonical
-- if the table key equals the entity’s internal canonical FQN (unwrap *.id).
extractKeysFromDfr
  :: forall f
   . DomeinFileRecord f
  -> { contexts :: OBJ.Object ContextKeySnapshot
     , roles :: OBJ.Object RoleKeySnapshot
     , properties :: OBJ.Object PropertyKeySnapshot
     , views :: OBJ.Object ViewKeySnapshot
     , states :: OBJ.Object StateKeySnapshot
     , actions :: OBJ.Object ActionKeySnapshot
     -- Indexed individuals present in the model text (readable identifiers)
     , contextIndividuals :: Array String
     , roleIndividuals :: Array String
     -- Indexed individual key snapshots (parent scope + name). Keyed by parent FQN.
     , contextIndividualKeys :: OBJ.Object ContextIndividualKeySnapshot
     , roleIndividualKeys :: OBJ.Object RoleIndividualKeySnapshot
     }
extractKeysFromDfr dfr@{ contexts, enumeratedRoles: eroles, calculatedRoles: croles, enumeratedProperties, calculatedProperties, views, states } =
  { contexts: mapContext contexts
  , roles: mapErole eroles <> mapCrole croles
  , properties: mapEProp enumeratedProperties <> mapCProp calculatedProperties
  , views: mapView views
  , states: mapState states
  , actions: mapActions eroles croles
  -- Lift indexed individuals from canonical Context and EnumeratedRole entries
  , contextIndividuals: collectIndexedContexts contexts
  , roleIndividuals: collectIndexedRoles eroles
  , contextIndividualKeys: collectIndexedContextKeys contexts
  , roleIndividualKeys: collectIndexedRoleKeys eroles
  }
  where
  -- Gather readable identifiers for indexed contexts from canonical Context entries
  collectIndexedContexts :: Object Context -> Array String
  collectIndexedContexts tbl = do
    k <- OBJ.keys tbl
    case OBJ.lookup k tbl of
      Nothing -> []
      Just (Context c) ->
        let
          canon = unwrap c.id
        in
          if canon /= k then []
          else case c.indexedContext of
            Nothing -> []
            Just ci -> [ unwrap ci ]

  -- Gather readable identifiers for indexed roles from canonical EnumeratedRole entries
  collectIndexedRoles :: Object EnumeratedRole -> Array String
  collectIndexedRoles tbl = do
    k <- OBJ.keys tbl
    case OBJ.lookup k tbl of
      Nothing -> []
      Just (EnumeratedRole r) ->
        let
          canon = unwrap r.id
        in
          if canon /= k then []
          else case r.indexedRole of
            Nothing -> []
            Just ri -> [ unwrap ri ]

  -- Individual key snapshots keyed by parent FQN
  collectIndexedContextKeys :: Object Context -> OBJ.Object ContextIndividualKeySnapshot
  collectIndexedContextKeys tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just (Context c) ->
          let
            canon = unwrap c.id
          in
            if canon /= k then []
            else case c.indexedContext of
              Nothing -> []
              Just ci -> [ Tuple k { contextFqn: k, name: unwrap ci } ]

  collectIndexedRoleKeys :: Object EnumeratedRole -> OBJ.Object RoleIndividualKeySnapshot
  collectIndexedRoleKeys tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just (EnumeratedRole r) ->
          let
            canon = unwrap r.id
          in
            if canon /= k then []
            else case r.indexedRole of
              Nothing -> []
              Just ri -> [ Tuple k { roleFqn: k, name: unwrap ri } ]

  mapContext :: Object Context -> Object ContextKeySnapshot
  mapContext tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just ctxt@(Context c) ->
          let
            canon = unwrap c.id
          in
            if canon /= k then []
            else
              pure $ Tuple k
                { fqn: k
                , declaringContextFqn: typeUri2typeNameSpace_ k
                , roles: (enumeratedRoles ctxt) <#> unwrap
                , properties: []
                , aspects: c.contextAspects <#> unwrap
                }

  mapErole :: Object EnumeratedRole -> Object RoleKeySnapshot
  mapErole tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just (EnumeratedRole r) ->
          let
            canon = unwrap r.id
          in
            if canon /= k then []
            else
              pure $ Tuple k
                { fqn: k
                , declaringContextFqn: unwrap r.context
                , properties: r.properties <#> propertytype2string
                , bindingTypes: []
                , aspects: []
                }

  mapCrole :: Object CalculatedRole -> Object RoleKeySnapshot
  mapCrole tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just (CalculatedRole r) ->
          let
            canon = unwrap r.id
          in
            if canon /= k then []
            else
              pure $ Tuple k
                { fqn: k
                , declaringContextFqn: unwrap r.context
                , properties: []
                , bindingTypes: []
                , aspects: []
                }

  mapEProp :: Object EnumeratedProperty -> Object PropertyKeySnapshot
  mapEProp tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just (EnumeratedProperty p) ->
          let
            canon = unwrap p.id
          in
            if canon /= k then []
            else
              let
                declaring = unwrap p.role
              in
                pure $ Tuple k
                  { fqn: k
                  , valueType: show p.range
                  , facets: []
                  , aspects: []
                  , declaringRoleFqn: declaring
                  }

  mapCProp :: Object CalculatedProperty -> Object PropertyKeySnapshot
  mapCProp tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just (CalculatedProperty p) ->
          let
            canon = unwrap p.id
          in
            if canon /= k then []
            else
              let
                declaring = unwrap p.role
              in
                pure $ Tuple k
                  { fqn: k
                  , valueType: unsafePartial case p.calculation of
                      Q calc -> case range calc of
                        (VDOM rn _) -> show rn
                  , facets: []
                  , aspects: []
                  , declaringRoleFqn: declaring
                  }

  mapView :: Object VW.View -> Object ViewKeySnapshot
  mapView tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just (VW.View v) ->
          let
            canon = unwrap v.id
          in
            if canon /= k then []
            else
              pure $ Tuple k
                { fqn: k
                , declaringRoleFqn: roletype2string v.role
                , properties: v.propertyReferences <#> propertytype2string
                }

  -- States: snapshot canonical entries by FQN and hash of their query
  mapState :: Object ST.State -> Object StateKeySnapshot
  mapState tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just (ST.State s) ->
          let
            canon = unwrap s.id
          in
            if canon /= k then []
            else
              let
                qh = unsafePartial case s.query of
                  Q qfd -> qfdSignature qfd
              in
                pure $ Tuple k { fqn: k, queryHash: qh }

  -- Actions: collect from role-level action maps and perspectives; snapshot canonical entries only
  mapActions :: Object EnumeratedRole -> Object CalculatedRole -> OBJ.Object ActionKeySnapshot
  mapActions erolesTbl crolesTbl =
    let
      fromRoleActions = foldl
        ( \acc k ->
            case OBJ.lookup k erolesTbl of
              Nothing -> acc
              Just (EnumeratedRole r) ->
                let
                  roleFqn = unwrap r.id
                  addFromEm acc' =
                    let
                      pairs = EM.toUnfoldable r.actions :: Array (Tuple StateSpec (OBJ.Object Action))
                    in
                      foldl accumulate acc' pairs
                  addFromPerspectives acc' =
                    foldl
                      ( \a p ->
                          case p of
                            Perspective pr ->
                              let
                                pairs = EM.toUnfoldable pr.actions :: Array (Tuple StateSpec (OBJ.Object Action))
                              in
                                foldl accumulate a pairs
                      )
                      acc'
                      r.perspectives
                  accumulate a (Tuple ss objActs) =
                    let
                      stFqn = unwrap (stateSpec2StateIdentifier ss)
                      ks = OBJ.keys objActs
                      inserts = ks <#>
                        ( \nm -> Tuple (stFqn <> "$" <> nm)
                            { fqn: stFqn <> "$" <> nm
                            , declaringRoleFqn: roleFqn
                            , declaringStateFqn: stFqn
                            }
                        )
                    in
                      OBJ.union a (OBJ.fromFoldable inserts)
                in
                  addFromPerspectives (addFromEm acc)
        )
        OBJ.empty
        (OBJ.keys erolesTbl)
      fromCalcRoleActions = foldl
        ( \acc k ->
            case OBJ.lookup k crolesTbl of
              Nothing -> acc
              Just (CalculatedRole r) ->
                let
                  roleFqn = unwrap r.id
                  pairs = EM.toUnfoldable r.actions :: Array (Tuple StateSpec (OBJ.Object Action))
                  acc' = foldl accumulate OBJ.empty pairs
                  acc'' = foldl
                    ( \a p ->
                        case p of
                          Perspective pr ->
                            let
                              pairsP = EM.toUnfoldable pr.actions :: Array (Tuple StateSpec (OBJ.Object Action))
                            in
                              foldl accumulate a pairsP
                    )
                    acc'
                    r.perspectives
                  accumulate a (Tuple ss objActs) =
                    let
                      stFqn = unwrap (stateSpec2StateIdentifier ss)
                      ks = OBJ.keys objActs
                      inserts = ks <#>
                        ( \nm -> Tuple (stFqn <> "$" <> nm)
                            { fqn: stFqn <> "$" <> nm
                            , declaringRoleFqn: roleFqn
                            , declaringStateFqn: stFqn
                            }
                        )
                    in
                      OBJ.union a (OBJ.fromFoldable inserts)
                in
                  OBJ.union acc acc''
        )
        OBJ.empty
        (OBJ.keys crolesTbl)
    in
      OBJ.union fromRoleActions fromCalcRoleActions

-- | Refactored from loadArc.purs: plan, mint and finalize stable IDs for a model
-- | and return the updated StableIdMapping including indexed individuals.
-- | There is at most a single indexed individual per parent in ARC, which this assumes.
updateStableMappingForModel
  :: forall m f
   . MonadEffect m
  => ModelUri Stable
  -> String -- modelCuid
  -> DomeinFileRecord f
  -> Maybe StableIdMapping
  -> m StableIdMapping
updateStableMappingForModel (ModelUri stableModelUri) modelCuid correctedDFR mMapping = do
  -- Base mapping from caller or initialize a fresh one for this model
  let
    mapping0 = case mMapping of
      Nothing -> emptyStableIdMapping
        { contextCuids = OBJ.singleton stableModelUri modelCuid
        , modelIdentifier = ModelUri $ (unsafePartial $ modelUri2SchemeAndAuthority stableModelUri) <> "#" <> modelCuid
        }
      Just m0 -> m0

  -- Extend aliases and compute current key snapshots
  let
    cur = extractKeysFromDfr correctedDFR
    planned = planCuidAssignments cur mapping0

  -- Mint new CUIDs for canonicals that need one (author-local, effectful)
  ctxPairs <- for planned.needCuids.contexts \fqn -> do
    v <- liftEffect (cuid2 (stableModelUri <> ":ctx"))
    pure (Tuple fqn v)

  -- Special handling for synthetic external roles (<context-fqn>$External):
  -- Skip minting separate CUIDs; derive <context-cuid>$External so code can rely on structure.
  let
    isExternalRole fqn =
      let
        suf = "$External"
        lf = SCU.length fqn
        ls = SCU.length suf
      in
        lf >= ls && SCU.drop (lf - ls) fqn == suf
    regularRoleFqns = filter (not <<< isExternalRole) planned.needCuids.roles
  rolPairsRegular <- for regularRoleFqns \fqn -> do
    v <- liftEffect (cuid2 (stableModelUri <> ":rol"))
    pure (Tuple fqn v)
  let
    externalRolePairs = do
      Tuple ctxFqn ctxCuid <- ctxPairs
      let extRoleFqn = ctxFqn <> "$External"
      case OBJ.lookup extRoleFqn cur.roles of
        Nothing -> []
        Just _ -> [ Tuple extRoleFqn "External" ]
    rolPairs = rolPairsRegular <> externalRolePairs

  propPairs <- for planned.needCuids.properties \fqn -> do
    v <- liftEffect (cuid2 (stableModelUri <> ":prop"))
    pure (Tuple fqn v)

  statePairs <- for planned.needCuids.states \fqn -> do
    v <- liftEffect (cuid2 (stableModelUri <> ":state"))
    pure (Tuple fqn v)

  viewPairs <- for planned.needCuids.views \fqn -> do
    v <- liftEffect (cuid2 (stableModelUri <> ":view"))
    pure (Tuple fqn v)

  actionPairs <- for planned.needCuids.actions \fqn -> do
    v <- liftEffect (cuid2 (stableModelUri <> ":action"))
    pure (Tuple fqn v)

  -- Mint instance CUIDs for indexed individuals (names)
  ctxIndPairs <- for planned.needCuids.contextIndividuals \nm -> do
    v <- liftEffect (cuid2 (stableModelUri <> ":ctx-ind"))
    pure (Tuple nm v)
  rolIndPairs <- for planned.needCuids.roleIndividuals \nm -> do
    v <- liftEffect (cuid2 (stableModelUri <> ":rol-ind"))
    pure (Tuple nm v)

  let
    newCuids =
      { contexts: OBJ.fromFoldable ctxPairs
      , roles: OBJ.fromFoldable rolPairs
      , properties: OBJ.fromFoldable propPairs
      , states: OBJ.fromFoldable statePairs
      , views: OBJ.fromFoldable viewPairs
      , actions: OBJ.fromFoldable actionPairs
      }

  let mapping1 = finalizeCuidAssignments planned.mappingWithAliases newCuids

  -- Build alias maps for individuals (oldName -> newName) based on parent scope snapshots
  let
    buildCtxIndAliases olds news =
      foldl
        ( \acc parentFqn -> case Tuple (OBJ.lookup parentFqn olds) (OBJ.lookup parentFqn news) of
            Tuple (Just o) (Just n) -> if not (o.name == n.name) then OBJ.insert o.name n.name acc else acc
            _ -> acc
        )
        OBJ.empty
        (OBJ.keys olds)
    buildRolIndAliases olds news =
      foldl
        ( \acc parentFqn -> case Tuple (OBJ.lookup parentFqn olds) (OBJ.lookup parentFqn news) of
            Tuple (Just o) (Just n) -> if not (o.name == n.name) then OBJ.insert o.name n.name acc else acc
            _ -> acc
        )
        OBJ.empty
        (OBJ.keys olds)
    ctxIndAliases = buildCtxIndAliases mapping0.contextIndividualKeys cur.contextIndividualKeys
    rolIndAliases = buildRolIndAliases mapping0.roleIndividualKeys cur.roleIndividualKeys

  -- Carry over IDs for renamed individuals (alias reuse): add newName -> oldId if present
  let
    carryAliases prev aliases =
      foldl
        ( \acc oldNm -> case Tuple (OBJ.lookup oldNm aliases) (OBJ.lookup oldNm prev) of
            Tuple (Just newNm) (Just oldId) ->
              case OBJ.lookup newNm acc of
                Nothing -> OBJ.insert newNm oldId acc
                Just _ -> acc
            _ -> acc
        )
        prev
        (OBJ.keys aliases)

    -- Start from previous maps in mapping1
    carriedCtxInd = carryAliases mapping1.contextIndividuals ctxIndAliases
    carriedRolInd = carryAliases mapping1.roleIndividuals rolIndAliases

  -- Compose stable instance IDs for newly minted individuals: parent type stable id + "$" + cuid
  let
    -- Build name -> parentFqn indices from current snapshots
    ctxName2Parent = OBJ.fromFoldable do
      parent <- OBJ.keys cur.contextIndividualKeys
      case OBJ.lookup parent cur.contextIndividualKeys of
        Nothing -> []
        Just snap -> [ Tuple snap.name parent ]
    rolName2Parent = OBJ.fromFoldable do
      parent <- OBJ.keys cur.roleIndividualKeys
      case OBJ.lookup parent cur.roleIndividualKeys of
        Nothing -> []
        Just snap -> [ Tuple snap.name parent ]

    composeCtx nm cuid = do
      parentFqn <- OBJ.lookup nm ctxName2Parent
      parentTid <- idUriForContext mapping1 (ContextUri parentFqn)
      pure (Tuple nm (parentTid <> "$" <> cuid))
    composeRol nm cuid = do
      parentFqn <- OBJ.lookup nm rolName2Parent
      parentTid <- idUriForRole mapping1 (RoleUri parentFqn)
      pure (Tuple nm (parentTid <> "$" <> cuid))

    newCtxIndMap = OBJ.fromFoldable $ mapMaybe (\(Tuple nm cuid) -> composeCtx nm cuid) ctxIndPairs
    newRolIndMap = OBJ.fromFoldable $ mapMaybe (\(Tuple nm cuid) -> composeRol nm cuid) rolIndPairs

    finalCtxInd = OBJ.union carriedCtxInd newCtxIndMap
    finalRolInd = OBJ.union carriedRolInd newRolIndMap

  let mapping2 = mapping1 { contextIndividuals = finalCtxInd, roleIndividuals = finalRolInd }

  pure mapping2

-- Merge the current key snapshots with the previous sidecar to generate/update alias maps.
mergeStableIdMapping
  :: forall r
   . { contexts :: OBJ.Object ContextKeySnapshot
     , roles :: OBJ.Object RoleKeySnapshot
     , properties :: OBJ.Object PropertyKeySnapshot
     , states :: OBJ.Object StateKeySnapshot
     , views :: OBJ.Object ViewKeySnapshot
     , actions :: OBJ.Object ActionKeySnapshot
     | r
     }
  -> StableIdMapping
  -> StableIdMapping
mergeStableIdMapping cur mapping0 =
  let
    candidatesC :: Array (Tuple String ContextKey)
    candidatesC = objToArrayWith snapshotToContextKeyCurrent cur.contexts

    candidatesR :: Array (Tuple String RoleKey)
    candidatesR = objToArrayWith snapshotToRoleKeyCurrent cur.roles

    candidatesP :: Array (Tuple String PropertyKey)
    candidatesP = objToArrayWith snapshotToPropertyKeyCurrent cur.properties

    candidatesV :: Array (Tuple String ViewKey)
    candidatesV = objToArrayWith snapshotToViewKeyCurrent cur.views

    -- For states, we don't use a graded heuristic yet: we require exact (fqn, queryHash).
    -- Build an index of current states by (fqn#hash)
    candStatesIndex :: OBJ.Object String
    candStatesIndex = OBJ.fromFoldable $ do
      k <- OBJ.keys cur.states
      case OBJ.lookup k cur.states of
        Nothing -> []
        Just s -> pure (Tuple (s.fqn <> "#" <> s.queryHash) s.fqn)

    contextKeys = OBJ.values mapping0.contextKeys

    -- Sort contextKeys topologically first!
    toposortedContexts = _.fqn <$> case sortTopologicallyEither _.fqn (singleton <<< typeUri2typeNameSpace_ <<< _.fqn) contextKeys of
      Left err -> contextKeys
      Right sorted -> sorted

    -- Build context aliases in topological order, threading inferred aliases
    ctxAliases' = buildContextAliasesTopologically
      defaultContextWeights
      0.5
      mapping0.contextKeys
      candidatesC
      mapping0.contexts
    rolAliases' = buildAliases defaultRoleWeights 0.5 mapping0.roleKeys (snapshotToRoleKeyOld ctxAliases') candidatesR mapping0.roles
    propAliases' = buildAliases defaultPropertyWeights 0.7 mapping0.propertyKeys (snapshotToPropertyKeyOld rolAliases') candidatesP mapping0.properties
    viewAliases' = buildAliases defaultViewWeights 0.7 mapping0.viewKeys (snapshotToViewKeyOld rolAliases') candidatesV mapping0.views
    ctxCuids' = assignCuids ctxAliases' mapping0.contextCuids (OBJ.keys cur.contexts)
    rolCuids' = assignCuids rolAliases' mapping0.roleCuids (OBJ.keys cur.roles)
    propCuids' = assignCuids propAliases' mapping0.propertyCuids (OBJ.keys cur.properties)
    viewCuids' = assignCuids viewAliases' mapping0.viewCuids (OBJ.keys cur.views)

    -- State aliasing: process old snapshots topologically by namespace and normalize parent namespaces through known context/state aliases.
    stateAliases' = buildStateAliasesTopologically mapping0.stateKeys ctxAliases' mapping0.states candStatesIndex

    -- Action aliases: normalize through state alias map only; preserve existing aliases otherwise
    actionAliases' = buildActionAliases mapping0.actionKeys mapping0.actions (OBJ.fromFoldable (OBJ.keys cur.actions <#> (\k -> Tuple k true))) stateAliases'
    actionCuids' = assignCuids actionAliases' mapping0.actionCuids (OBJ.keys cur.actions)
  in
    mapping0
      { contexts = ctxAliases'
      , roles = rolAliases'
      , properties = propAliases'
      , views = viewAliases'
      , states = stateAliases'
      , actions = actionAliases'
      , contextKeys = cur.contexts
      , roleKeys = cur.roles
      , propertyKeys = cur.properties
      , viewKeys = cur.views
      , stateKeys = cur.states
      , actionKeys = cur.actions
      , contextCuids = ctxCuids'
      , roleCuids = rolCuids'
      , propertyCuids = propCuids'
      , viewCuids = viewCuids'
      , actionCuids = actionCuids'
      }
  where
  objToArrayWith :: forall s a. (s -> a) -> OBJ.Object s -> Array (Tuple String a)
  objToArrayWith f o = mapMaybe
    ( \k -> case OBJ.lookup k o of
        Nothing -> Nothing
        Just s -> Just (Tuple k (f s))
    )
    (OBJ.keys o)

  buildAliases
    :: forall s a w
     . HeuristicMapping a w
    => w
    -> Number
    -> OBJ.Object s
    -> (s -> a)
    -> Array (Tuple String a)
    -> OBJ.Object String
    -> OBJ.Object String
  buildAliases w minScore oldSnap toKey candidates acc =
    let
      candIndex = OBJ.fromFoldable (candidates <#> \(Tuple k _) -> Tuple k true)
    in
      foldl
        ( \acc' oldFqn -> case OBJ.lookup oldFqn oldSnap of
            Nothing -> acc'
            Just sOld ->
              if OBJ.lookup oldFqn candIndex /= Nothing then acc'
              else case rankBest w minScore (toKey sOld) candidates of
                Just (Tuple newFqn _) -> OBJ.insert oldFqn newFqn acc'
                Nothing -> acc'
        )
        acc
        (OBJ.keys oldSnap)

  buildContextAliasesTopologically
    :: ContextWeights
    -> Number
    -> OBJ.Object ContextKeySnapshot -- old snapshots
    -> Array (Tuple String ContextKey) -- current candidates (canonical only)
    -> OBJ.Object String -- initial alias map (typically mapping0.contexts)
    -> OBJ.Object String -- result alias map
  buildContextAliasesTopologically w minScore oldSnap candidates acc0 =
    let
      candIndex = OBJ.fromFoldable (candidates <#> \(Tuple k _) -> Tuple k true)
      olds = OBJ.values oldSnap
      -- Topologically sort old snapshots by parent namespace
      sorted = case sortTopologicallyEither _.fqn (singleton <<< typeUri2typeNameSpace_ <<< _.fqn) olds of
        Right xs -> xs
        Left _ -> olds
    in
      foldl
        ( \acc sOld ->
            let
              oldFqn = sOld.fqn
            in
              if OBJ.lookup oldFqn candIndex /= Nothing then acc
              else
                case rankBest w minScore (snapshotToContextKeyOld acc sOld) candidates of
                  Just (Tuple newFqn _) -> OBJ.insert oldFqn newFqn acc
                  Nothing -> acc
        )
        acc0
        sorted

  -- Compute new FQN->CUID map from previous map and alias relations.
  assignCuids
    :: OBJ.Object String -- alias map (aliasFqn -> canonicalFqn)
    -> OBJ.Object String -- previous fqn -> cuid
    -> Array String -- current canonical FQNs
    -> OBJ.Object String
  assignCuids aliasMap prevCuids currentCanon =
    let
      reuseFromAlias :: String -> Maybe String
      reuseFromAlias newFqn = findFirstJust (OBJ.keys aliasMap) \oldFqn ->
        case OBJ.lookup oldFqn aliasMap of
          Just tgt | tgt == newFqn -> OBJ.lookup oldFqn prevCuids
          _ -> Nothing

      go acc fqn = case OBJ.lookup fqn prevCuids of
        Just cuid -> OBJ.insert fqn cuid acc
        Nothing -> case reuseFromAlias fqn of
          Just cuid -> OBJ.insert fqn cuid acc
          Nothing -> acc
    in
      foldl go prevCuids currentCanon

-- Build state aliases in a parent-first order, normalizing namespaces via context/state alias maps.
buildStateAliasesTopologically
  :: OBJ.Object StateKeySnapshot -- old state snapshots
  -> OBJ.Object String -- context alias map
  -> OBJ.Object String -- initial state alias map
  -> OBJ.Object String -- current index (fqn#hash -> fqn)
  -> OBJ.Object String
buildStateAliasesTopologically oldStates ctxAliases acc0 curIndex =
  let
    olds = OBJ.values oldStates
    sorted = case sortTopologicallyEither _.fqn (singleton <<< typeUri2typeNameSpace_ <<< _.fqn) olds of
      Right xs -> xs
      Left _ -> olds
    normalizeNs acc ns =
      case Tuple (OBJ.lookup ns ctxAliases) (OBJ.lookup ns acc) of
        Tuple (Just newNs) _ -> newNs
        Tuple _ (Just newNs) -> newNs
        _ -> ns
    step acc sOld =
      let
        keySame = sOld.fqn <> "#" <> sOld.queryHash
      in
        if OBJ.lookup keySame curIndex /= Nothing then acc
        else
          let
            ns0 = typeUri2typeNameSpace_ sOld.fqn
            ln = typeUri2LocalName_ sOld.fqn
            ns1 = normalizeNs acc ns0
            normFqn = if ns1 == ns0 then sOld.fqn else ns1 <> "$" <> ln
            k2 = normFqn <> "#" <> sOld.queryHash
          in
            case OBJ.lookup k2 curIndex of
              Just newFqn -> OBJ.insert sOld.fqn newFqn acc
              Nothing -> acc
  in
    foldl step acc0 sorted

-- Build action aliases by normalizing old state namespaces via state alias map, keep same local action name.
buildActionAliases
  :: OBJ.Object ActionKeySnapshot -- old action snapshots
  -> OBJ.Object String -- existing alias map (oldFqn -> newFqn)
  -> OBJ.Object Boolean -- current index of canonical action FQNs
  -> OBJ.Object String -- state alias map (oldStateFqn -> newStateFqn)
  -> OBJ.Object String
buildActionAliases oldActions acc0 curIndex stateAliases =
  foldl
    ( \acc oldFqn -> case OBJ.lookup oldFqn oldActions of
        Nothing -> acc
        Just sOld ->
          if OBJ.lookup oldFqn curIndex /= Nothing then acc
          else
            let
              st0 = sOld.declaringStateFqn
              stN = case OBJ.lookup st0 stateAliases of
                Just ns -> ns
                Nothing -> st0
              -- take local action name as suffix after last '$'
              local = lastSegment oldFqn
              cand = stN <> "$" <> local
            in
              if OBJ.lookup cand curIndex /= Nothing then OBJ.insert oldFqn cand acc else acc
    )
    acc0
    (OBJ.keys oldActions)

lastSegment :: String -> String
lastSegment s =
  case findLastIndex (_ == '$') (toCharArray s) of
    Nothing -> s
    Just i -> drop (i + 1) s

-- uses top-level findFirstJust
snapshotToContextKeyCurrent :: ContextKeySnapshot -> ContextKey
snapshotToContextKeyCurrent s = ContextKey { fqn: s.fqn, declaringContextFqn: s.declaringContextFqn, roles: s.roles, properties: s.properties, aspects: s.aspects }

snapshotToContextKeyOld :: OBJ.Object String -> ContextKeySnapshot -> ContextKey
snapshotToContextKeyOld ctxAliases s =
  let
    decl0 = s.declaringContextFqn
    declN = case OBJ.lookup decl0 ctxAliases of
      Just new -> new
      Nothing -> decl0
  in
    ContextKey
      { fqn: s.fqn
      , declaringContextFqn: declN
      , roles: s.roles
      , properties: s.properties
      , aspects: s.aspects
      }

-- Current role snapshot uses canonical declaring context
snapshotToRoleKeyCurrent :: RoleKeySnapshot -> RoleKey
snapshotToRoleKeyCurrent s =
  RoleKey
    { fqn: s.fqn
    , declaringContextFqn: s.declaringContextFqn
    , properties: s.properties
    , bindingTypes: s.bindingTypes
    , aspects: s.aspects
    }

-- Old role snapshot normalized through context alias map
snapshotToRoleKeyOld :: OBJ.Object String -> RoleKeySnapshot -> RoleKey
snapshotToRoleKeyOld ctxAliases s =
  let
    decl0 = s.declaringContextFqn
    declN = case OBJ.lookup decl0 ctxAliases of
      Just new -> new
      Nothing -> decl0
  in
    RoleKey
      { fqn: s.fqn
      , declaringContextFqn: declN
      , properties: s.properties
      , bindingTypes: s.bindingTypes
      , aspects: s.aspects
      }

snapshotToPropertyKeyCurrent :: PropertyKeySnapshot -> PropertyKey
snapshotToPropertyKeyCurrent s =
  PropertyKey
    { fqn: s.fqn
    , valueType: s.valueType
    , facets: s.facets
    , aspects: s.aspects
    , declaringRoleFqn: s.declaringRoleFqn
    }

snapshotToPropertyKeyOld :: OBJ.Object String -> PropertyKeySnapshot -> PropertyKey
snapshotToPropertyKeyOld roleAliases s =
  let
    decl0 = s.declaringRoleFqn
    declN = case OBJ.lookup decl0 roleAliases of
      Just new -> new
      Nothing -> decl0
  in
    PropertyKey
      { fqn: s.fqn
      , valueType: s.valueType
      , facets: s.facets
      , aspects: s.aspects
      , declaringRoleFqn: declN
      }

snapshotToViewKeyCurrent :: ViewKeySnapshot -> ViewKey
snapshotToViewKeyCurrent s =
  ViewKey
    { fqn: s.fqn
    , declaringRoleFqn: s.declaringRoleFqn
    , properties: s.properties
    }

snapshotToViewKeyOld :: OBJ.Object String -> ViewKeySnapshot -> ViewKey
snapshotToViewKeyOld roleAliases s =
  let
    decl0 = s.declaringRoleFqn
    declN = case OBJ.lookup decl0 roleAliases of
      Just new -> new
      Nothing -> decl0
  in
    ViewKey
      { fqn: s.fqn
      , declaringRoleFqn: declN
      , properties: s.properties
      }

-- Re-exported helper locally for broader use in this module
findFirstJust
  :: forall a
   . Array String
  -> (String -> Maybe a)
  -> Maybe a
findFirstJust ks f =
  case find (\k -> isJust (f k)) ks of
    Nothing -> Nothing
    Just k -> f k

-- Plan CUID assignments: same stronger property threshold
planCuidAssignments
  :: forall r
   . { contexts :: OBJ.Object ContextKeySnapshot
     , roles :: OBJ.Object RoleKeySnapshot
     , properties :: OBJ.Object PropertyKeySnapshot
     , views :: OBJ.Object ViewKeySnapshot
     , states :: OBJ.Object StateKeySnapshot
     , actions :: OBJ.Object ActionKeySnapshot
     -- Indexed individuals present in the model text (readable identifiers)
     , contextIndividuals :: Array String
     , roleIndividuals :: Array String
     -- Indexed individual key snapshots (parent scope + name)
     , contextIndividualKeys :: OBJ.Object ContextIndividualKeySnapshot
     , roleIndividualKeys :: OBJ.Object RoleIndividualKeySnapshot
     | r
     }
  -> StableIdMapping
  -> { mappingWithAliases :: StableIdMapping
     , needCuids :: { contexts :: Array String, roles :: Array String, properties :: Array String, views :: Array String, states :: Array String, actions :: Array String, contextIndividuals :: Array String, roleIndividuals :: Array String }
     }
planCuidAssignments cur mapping0 =
  let
    candidatesC = objToArrayWith snapshotToContextKeyCurrent cur.contexts
    candidatesR = objToArrayWith snapshotToRoleKeyCurrent cur.roles
    candidatesP = objToArrayWith snapshotToPropertyKeyCurrent cur.properties
    candidatesV = objToArrayWith snapshotToViewKeyCurrent cur.views

    contextKeys = OBJ.values mapping0.contextKeys

    ctxAliases' = buildContextAliasesTopologically
      defaultContextWeights
      0.5
      mapping0.contextKeys
      candidatesC
      mapping0.contexts

    rolAliases' = buildAliases defaultRoleWeights 0.5 mapping0.roleKeys (snapshotToRoleKeyOld ctxAliases') candidatesR mapping0.roles (OBJ.keys mapping0.roleKeys)
    propAliases' = buildAliases defaultPropertyWeights 0.7 mapping0.propertyKeys (snapshotToPropertyKeyOld rolAliases') candidatesP mapping0.properties (OBJ.keys mapping0.propertyKeys)
    viewAliases' = buildAliases defaultViewWeights 0.7 mapping0.viewKeys (snapshotToViewKeyOld rolAliases') candidatesV mapping0.views (OBJ.keys mapping0.viewKeys)

    -- state aliases (exact match fqn+hash with normalized namespace)
    candStatesIndex :: OBJ.Object String
    candStatesIndex = OBJ.fromFoldable $ do
      k <- OBJ.keys cur.states
      case OBJ.lookup k cur.states of
        Nothing -> []
        Just s -> pure (Tuple (s.fqn <> "#" <> s.queryHash) s.fqn)

    stateAliases' = buildStateAliasesTopologically mapping0.stateKeys ctxAliases' mapping0.states candStatesIndex

    mappingWithAliases = mapping0
      { contexts = ctxAliases'
      , roles = rolAliases'
      , properties = propAliases'
      , views = viewAliases'
      , states = stateAliases'
      , contextKeys = cur.contexts
      , roleKeys = cur.roles
      , propertyKeys = cur.properties
      , viewKeys = cur.views
      , stateKeys = cur.states
      , contextIndividualKeys = cur.contextIndividualKeys
      , roleIndividualKeys = cur.roleIndividualKeys
      }

    needs :: forall s. OBJ.Object s -> OBJ.Object String -> OBJ.Object String -> Array String
    needs current prevCuids aliases =
      let
        hasPrev fqn = OBJ.lookup fqn prevCuids /= Nothing
        reusedFromAlias fqn = findFirstJust (OBJ.keys aliases) \oldFqn ->
          case OBJ.lookup oldFqn aliases of
            Just tgt | tgt == fqn -> OBJ.lookup oldFqn prevCuids
            _ -> Nothing
      in
        filter (\fqn -> not (hasPrev fqn) && reusedFromAlias fqn == Nothing) (OBJ.keys current)

    needCtx = needs cur.contexts mapping0.contextCuids ctxAliases'
    needRol = needs cur.roles mapping0.roleCuids rolAliases'
    needProp = needs cur.properties mapping0.propertyCuids propAliases'
    needView = needs cur.views mapping0.viewCuids viewAliases'
    -- For states, plan CUIDs for current canonical states with no cuid and not reusable from alias
    needState =
      let
        hasPrev fqn = OBJ.lookup fqn mapping0.stateCuids /= Nothing
        reusedFromAlias fqn = findFirstJust (OBJ.keys stateAliases') \oldFqn ->
          case OBJ.lookup oldFqn stateAliases' of
            Just tgt | tgt == fqn -> OBJ.lookup oldFqn mapping0.stateCuids
            _ -> Nothing
      in
        filter (\fqn -> not (hasPrev fqn) && reusedFromAlias fqn == Nothing) (OBJ.keys cur.states)
    needAction =
      let
        hasPrev fqn = OBJ.lookup fqn mapping0.actionCuids /= Nothing
        reusedFromAlias fqn = findFirstJust (OBJ.keys mapping0.actions) \oldFqn ->
          case OBJ.lookup oldFqn mapping0.actions of
            Just tgt | tgt == fqn -> OBJ.lookup oldFqn mapping0.actionCuids
            _ -> Nothing
      in
        filter (\fqn -> not (hasPrev fqn) && reusedFromAlias fqn == Nothing) (OBJ.keys cur.actions)

    -- Build simple alias maps for individuals based on parent scope: oldName -> newName
    contextIndividualAliases :: OBJ.Object String
    contextIndividualAliases =
      let
        olds = mapping0.contextIndividualKeys
        news = cur.contextIndividualKeys
      in
        foldl
          ( \acc parentFqn -> case Tuple (OBJ.lookup parentFqn olds) (OBJ.lookup parentFqn news) of
              Tuple (Just o) (Just n) ->
                if o.name /= n.name then OBJ.insert o.name n.name acc else acc
              _ -> acc
          )
          OBJ.empty
          (OBJ.keys olds)

    roleIndividualAliases :: OBJ.Object String
    roleIndividualAliases =
      let
        olds = mapping0.roleIndividualKeys
        news = cur.roleIndividualKeys
      in
        foldl
          ( \acc parentFqn -> case Tuple (OBJ.lookup parentFqn olds) (OBJ.lookup parentFqn news) of
              Tuple (Just o) (Just n) ->
                if o.name /= n.name then OBJ.insert o.name n.name acc else acc
              _ -> acc
          )
          OBJ.empty
          (OBJ.keys olds)

    needsIndividuals :: Array String -> OBJ.Object String -> OBJ.Object String -> Array String
    needsIndividuals current prevMap aliases =
      let
        hasPrev nm = OBJ.lookup nm prevMap /= Nothing
        reusedFromAlias nm = findFirstJust (OBJ.keys aliases) \oldNm ->
          case OBJ.lookup oldNm aliases of
            Just tgt | tgt == nm -> OBJ.lookup oldNm prevMap
            _ -> Nothing
      in
        filter (\nm -> not (hasPrev nm) && reusedFromAlias nm == Nothing) current

    needContextIndividuals = needsIndividuals cur.contextIndividuals mapping0.contextIndividuals contextIndividualAliases
    needRoleIndividuals = needsIndividuals cur.roleIndividuals mapping0.roleIndividuals roleIndividualAliases
  in
    { mappingWithAliases
    , needCuids: { contexts: needCtx, roles: needRol, properties: needProp, views: needView, states: needState, actions: needAction, contextIndividuals: needContextIndividuals, roleIndividuals: needRoleIndividuals }
    }
  where
  objToArrayWith :: forall s a. (s -> a) -> OBJ.Object s -> Array (Tuple String a)
  objToArrayWith f o = mapMaybe
    ( \k -> case OBJ.lookup k o of
        Nothing -> Nothing
        Just s -> Just (Tuple k (f s))
    )
    (OBJ.keys o)

  buildAliases
    :: forall s a w
     . HeuristicMapping a w
    => w
    -> Number
    -> OBJ.Object s
    -> (s -> a)
    -> Array (Tuple String a)
    -> OBJ.Object String
    -> Array String
    -> OBJ.Object String
  buildAliases w minScore oldSnap toKey candidates acc keys =
    let
      candIndex = OBJ.fromFoldable (candidates <#> \(Tuple k _) -> Tuple k true)
    in
      foldl
        ( \acc' oldFqn -> case OBJ.lookup oldFqn oldSnap of
            Nothing -> acc'
            Just sOld ->
              if OBJ.lookup oldFqn candIndex /= Nothing then acc'
              else case rankBest w minScore (toKey sOld) candidates of
                Just (Tuple newFqn _) -> OBJ.insert oldFqn newFqn acc'
                Nothing -> acc'
        )
        acc
        keys

  buildContextAliasesTopologically
    :: ContextWeights
    -> Number
    -> OBJ.Object ContextKeySnapshot -- old snapshots
    -> Array (Tuple String ContextKey) -- current candidates (canonical only)
    -> OBJ.Object String -- initial alias map (typically mapping0.contexts)
    -> OBJ.Object String -- result alias map
  buildContextAliasesTopologically w minScore oldSnap candidates acc0 =
    let
      candIndex = OBJ.fromFoldable (candidates <#> \(Tuple k _) -> Tuple k true)
      olds = OBJ.values oldSnap
      sorted = case sortTopologicallyEither _.fqn (singleton <<< typeUri2typeNameSpace_ <<< _.fqn) olds of
        Right xs -> xs
        Left _ -> olds
    in
      foldl
        ( \acc sOld ->
            let
              oldFqn = sOld.fqn
            in
              if OBJ.lookup oldFqn candIndex /= Nothing then acc
              else case rankBest w minScore (snapshotToContextKeyOld acc sOld) candidates of
                Just (Tuple newFqn _) -> OBJ.insert oldFqn newFqn acc
                Nothing -> acc
        )
        acc0
        sorted

-- Finalize CUID assignments: copy existing cuids from oldFqn to newFqn wherever alias old->new exists and new lacks a cuid
finalizeCuidAssignments
  :: StableIdMapping
  -> { contexts :: OBJ.Object String, roles :: OBJ.Object String, properties :: OBJ.Object String, views :: OBJ.Object String, states :: OBJ.Object String, actions :: OBJ.Object String }
  -> StableIdMapping
finalizeCuidAssignments mappingWithAliases newCuids =
  let
    -- Copy existing cuids from oldFqn to newFqn wherever alias old->new exists and new lacks a cuid
    propagate
      :: OBJ.Object String -- alias map (aliasFqn -> canonicalFqn)
      -> OBJ.Object String -- prev cuids (fqn -> cuid)
      -> OBJ.Object String -- result cuids (after propagation)
    propagate aliases prev =
      foldl
        ( \acc oldFqn ->
            case Tuple (OBJ.lookup oldFqn aliases) (OBJ.lookup oldFqn prev) of
              Tuple (Just newFqn) (Just cuid) ->
                case OBJ.lookup newFqn acc of
                  Nothing -> OBJ.insert newFqn cuid acc
                  Just _ -> acc
              _ -> acc
        )
        prev
        (OBJ.keys aliases)

    -- 1) propagate cuids through aliases
    ctxCuidsProp = propagate mappingWithAliases.contexts mappingWithAliases.contextCuids
    rolCuidsProp = propagate mappingWithAliases.roles mappingWithAliases.roleCuids
    propCuidsProp = propagate mappingWithAliases.properties mappingWithAliases.propertyCuids
    viewCuidsProp = propagate mappingWithAliases.views mappingWithAliases.viewCuids
    stateCuidsProp = propagate mappingWithAliases.states mappingWithAliases.stateCuids
    actionCuidsProp = propagate mappingWithAliases.actions mappingWithAliases.actionCuids

    -- 2) add freshly minted cuids (if any)
    ctxCuids' = OBJ.union ctxCuidsProp newCuids.contexts
    rolCuids' = OBJ.union rolCuidsProp newCuids.roles
    propCuids' = OBJ.union propCuidsProp newCuids.properties
    viewCuids' = OBJ.union viewCuidsProp newCuids.views
    stateCuids' = OBJ.union stateCuidsProp newCuids.states
    actionCuids' = OBJ.union actionCuidsProp newCuids.actions
  in
    mappingWithAliases
      { contextCuids = ctxCuids'
      , roleCuids = rolCuids'
      , propertyCuids = propCuids'
      , viewCuids = viewCuids'
      , stateCuids = stateCuids'
      , actionCuids = actionCuids'
      }

