-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2025 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.ArcParser.UniqueTypeNames
  ( -- Key data extracted for heuristic matching (sidecar and fresh parse)
    ContextKey(..)
  , RoleKey(..)
  , PropertyKey(..)
  , HeuristicKey(..)
    -- Weights and scoring
  , ContextWeights
  , RoleWeights
  , PropertyWeights
  , class HeuristicMapping
  , defaultWeights
  , score
  , defaultContextWeights
  , defaultRoleWeights
  , defaultPropertyWeights
  , Score
    -- Ranking helpers
  , rank
  , rankBest
  , applyStableIdMappingWith
  , extractKeysFromDfr
  , planCuidAssignments
  , finalizeCuidAssignments
  ) where

import Prelude

import Data.Array (filter, find, head, length, mapMaybe, nub, singleton, sortBy)
import Data.Either (Either(..))
import Data.Foldable (elem, foldl)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object)
import Foreign.Object as OBJ
import Perspectives.DomeinFile (DomeinFileRecord)
import Perspectives.ExecuteInTopologicalOrder (sortTopologicallyEither)
import Perspectives.Identifiers (isModelUri, typeUri2LocalName_, typeUri2typeNameSpace_)
import Perspectives.Representation.Class.Context (enumeratedRoles)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.TypeIdentifiers (propertytype2string)
import Perspectives.Sidecar.StableIdMapping (PropertyKeySnapshot, RoleKeySnapshot, StableIdMapping, ContextKeySnapshot)

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
    in (if a.fqn == b.fqn then w.fqnExact else 0.0)
       + w.rolesCommon * commonFrac a.roles b.roles
       + w.propertiesCommon * commonFrac a.properties b.properties
       + w.aspectsCommon * commonFrac a.aspects b.aspects
       + w.nameSimilarity * nameScore
    where
      compareContextIds :: String -> String -> Number
      compareContextIds x y = if isModelUri x || isModelUri y
        then if x == y
          then 1.0
          else 0.0
        else nameSim (typeUri2LocalName_ x) (typeUri2LocalName_ y)

instance heuristicRole :: HeuristicMapping RoleKey RoleWeights where
  defaultWeights = defaultRoleWeights
  score w (RoleKey a) (RoleKey b) =
    let
      nameScore = nameSim (typeUri2LocalName_ a.fqn) (typeUri2LocalName_ b.fqn)
      declScore = if a.declaringContextFqn == b.declaringContextFqn then 1.0 else 0.0
    in (if a.fqn == b.fqn then w.fqnExact else 0.0)
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
    in (if a.fqn == b.fqn then w.fqnExact else 0.0)
       + w.valueTypeExact * valueTypeScore
       + w.facetsCommon * commonFrac a.facets b.facets
       + w.aspectsCommon * commonFrac a.aspects b.aspects
       + w.declaringRoleExact * roleScore
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

-- | Rank candidates (CUID × key) for a given probe key using the provided weights.
rank
  :: forall a w
   . HeuristicMapping a w
  => w
  -> a
  -> Array (Tuple String a) -- candidates: (cuid, key)
  -> Array (Tuple String Score)
rank w probe candidates =
  let withScores = candidates <#> \(Tuple cuid k) -> Tuple cuid (score w probe k)
  in sortBy (\(Tuple _ s1) (Tuple _ s2) -> compare s2 s1) withScores

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
  let as' = nub as
      bs' = nub bs
      interCount = length $ filter (\x -> elem x bs') as'
      denom = max (length as') (length bs')
  in if denom == 0 then 0.0 else toNumber interCount / toNumber denom

-- Jaccard similarity on unique character sets of two local names.
nameSim :: String -> String -> Number
nameSim a b =
  let as = nub (toCharArray a)
      bs = nub (toCharArray b)
      interCount = length $ filter (\c -> elem c bs) as
      denom = length as + length bs - interCount
  in if denom == 0 then if a == b then 1.0 else 0.0 else toNumber interCount / toNumber denom

-- Mapping hook ---------------------------------------------------------------

-- | Pure hook: apply a provided mapping and also compute the updated mapping snapshot.
-- | Returns the alias-augmented DomeinFileRecord and the updated mapping.
applyStableIdMappingWith :: StableIdMapping -> DomeinFileRecord -> Tuple DomeinFileRecord StableIdMapping
applyStableIdMappingWith mapping0 df =
  let cur = extractKeysFromDfr df
      mapping' = mergeStableIdMapping cur mapping0
  in Tuple (applyAliases mapping' df) mapping'

-- | Add alias keys (from the mapping file) that point to the canonical entries
-- | in the DomeinFileRecord. We NEVER rename the canonical identifiers here;
-- | we only add lookups under the alias keys for backwards/forwards compatibility.
applyAliases :: StableIdMapping -> DomeinFileRecord -> DomeinFileRecord
applyAliases mapping dfr@{contexts, enumeratedRoles, enumeratedProperties} =
  dfr
    { contexts = aliasTable contexts mapping.contexts
    , enumeratedRoles = aliasTable enumeratedRoles mapping.roles
    , enumeratedProperties = aliasTable enumeratedProperties mapping.properties
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
    let ks = OBJ.keys m in
    foldl
      (\acc aliasKey ->
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
  :: DomeinFileRecord
  -> { contexts :: OBJ.Object ContextKeySnapshot
     , roles :: OBJ.Object RoleKeySnapshot
     , properties :: OBJ.Object PropertyKeySnapshot
     }
extractKeysFromDfr dfr@{contexts, enumeratedRoles:eroles, enumeratedProperties} =
  { contexts: mapContext contexts
  , roles: mapRole eroles
  , properties: mapProp enumeratedProperties
  }
  where
  mapContext :: Object Context -> Object ContextKeySnapshot
  mapContext tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just ctxt@(Context c) ->
          let canon = unwrap c.id
          in if canon /= k then [] else
              pure $ Tuple k
                { fqn: k
                , declaringContextFqn: typeUri2typeNameSpace_ k
                , roles: (enumeratedRoles ctxt) <#> unwrap
                , properties: []
                , aspects: c.contextAspects <#> unwrap
                }

  mapRole :: Object EnumeratedRole -> Object RoleKeySnapshot
  mapRole tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just (EnumeratedRole r) ->
          let canon = unwrap r.id
          in if canon /= k then [] else
              pure $ Tuple k
                { fqn: k
                , declaringContextFqn: unwrap r.context
                , properties: r.properties <#> propertytype2string
                , bindingTypes: []
                , aspects: []
                }

  mapProp :: Object EnumeratedProperty -> Object PropertyKeySnapshot
  mapProp tbl =
    OBJ.fromFoldable do
      k <- OBJ.keys tbl
      case OBJ.lookup k tbl of
        Nothing -> []
        Just (EnumeratedProperty p) ->
          let canon = unwrap p.id
          in if canon /= k then [] else
              let declaring = unwrap p.role
              in pure $ Tuple k
                    { fqn: k
                    , valueType: show p.range
                    , facets: []
                    , aspects: []
                    , declaringRoleFqn: declaring
                    }

-- Merge the current key snapshots with the previous sidecar to generate/update alias maps.
mergeStableIdMapping
  :: { contexts :: OBJ.Object ContextKeySnapshot
     , roles :: OBJ.Object RoleKeySnapshot
     , properties :: OBJ.Object PropertyKeySnapshot
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

    contextKeys = OBJ.values mapping0.contextKeys

    -- Sort contextKeys topologically first!
    toposortedContexts = _.fqn <$> case sortTopologicallyEither _.fqn (singleton <<< typeUri2typeNameSpace_ <<< _.fqn) contextKeys of
      Left err -> contextKeys
      Right sorted -> sorted

    -- Build context aliases in topological order, threading inferred aliases
    ctxAliases'  = buildContextAliasesTopologically
      defaultContextWeights
      0.5
      mapping0.contextKeys
      candidatesC
      mapping0.contexts
    rolAliases'  = buildAliases defaultRoleWeights      0.5 mapping0.roleKeys     (snapshotToRoleKeyOld ctxAliases')      candidatesR mapping0.roles
    propAliases' = buildAliases defaultPropertyWeights  0.7 mapping0.propertyKeys (snapshotToPropertyKeyOld rolAliases')  candidatesP mapping0.properties

    ctxCuids'  = assignCuids ctxAliases'  mapping0.contextCuids  (OBJ.keys cur.contexts)
    rolCuids'  = assignCuids rolAliases'  mapping0.roleCuids     (OBJ.keys cur.roles)
    propCuids' = assignCuids propAliases' mapping0.propertyCuids (OBJ.keys cur.properties)
  in
    mapping0
      { contexts = ctxAliases'
      , roles = rolAliases'
      , properties = propAliases'
      , contextKeys = cur.contexts
      , roleKeys = cur.roles
      , propertyKeys = cur.properties
      , contextCuids = ctxCuids'
      , roleCuids = rolCuids'
      , propertyCuids = propCuids'
      }
  where
  objToArrayWith :: forall s a. (s -> a) -> OBJ.Object s -> Array (Tuple String a)
  objToArrayWith f o = mapMaybe (\k -> case OBJ.lookup k o of
    Nothing -> Nothing
    Just s -> Just (Tuple k (f s))) (OBJ.keys o)

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
    let candIndex = OBJ.fromFoldable (candidates <#> \(Tuple k _) -> Tuple k true)
    in foldl
      (\acc' oldFqn -> case OBJ.lookup oldFqn oldSnap of
        Nothing -> acc'
        Just sOld ->
          if OBJ.lookup oldFqn candIndex /= Nothing
            then acc'
            else case rankBest w minScore (toKey sOld) candidates of
              Just (Tuple newFqn _) -> OBJ.insert oldFqn newFqn acc'
              Nothing -> acc')
      acc
      (OBJ.keys oldSnap)

  buildContextAliasesTopologically
    :: ContextWeights
    -> Number
    -> OBJ.Object ContextKeySnapshot   -- old snapshots
    -> Array (Tuple String ContextKey) -- current candidates (canonical only)
    -> OBJ.Object String               -- initial alias map (typically mapping0.contexts)
    -> OBJ.Object String               -- result alias map
  buildContextAliasesTopologically w minScore oldSnap candidates acc0 =
    let
      candIndex = OBJ.fromFoldable (candidates <#> \(Tuple k _) -> Tuple k true)
      olds = OBJ.values oldSnap
      -- Topologically sort old snapshots by parent namespace
      sorted = case sortTopologicallyEither _.fqn (singleton <<< typeUri2typeNameSpace_ <<< _.fqn) olds of
        Right xs -> xs
        Left _   -> olds
    in
      foldl
        (\acc sOld ->
           let oldFqn = sOld.fqn
           in if OBJ.lookup oldFqn candIndex /= Nothing
                then acc
                else
                  case rankBest w minScore (snapshotToContextKeyOld acc sOld) candidates of
                    Just (Tuple newFqn _) -> OBJ.insert oldFqn newFqn acc
                    Nothing               -> acc
        )
        acc0
        sorted

  -- Compute new FQN->CUID map from previous map and alias relations.
  assignCuids
    :: OBJ.Object String        -- alias map (aliasFqn -> canonicalFqn)
    -> OBJ.Object String        -- previous fqn -> cuid
    -> Array String             -- current canonical FQNs
    -> OBJ.Object String
  assignCuids aliasMap prevCuids currentCanon =
    let reuseFromAlias :: String -> Maybe String
        reuseFromAlias newFqn = findFirstJust (OBJ.keys aliasMap) \oldFqn ->
          case OBJ.lookup oldFqn aliasMap of
            Just tgt | tgt == newFqn -> OBJ.lookup oldFqn prevCuids
            _ -> Nothing

        go acc fqn = case OBJ.lookup fqn prevCuids of
          Just cuid -> OBJ.insert fqn cuid acc
          Nothing -> case reuseFromAlias fqn of
            Just cuid -> OBJ.insert fqn cuid acc
            Nothing   -> acc
    in foldl go prevCuids currentCanon

  -- uses top-level findFirstJust
snapshotToContextKeyCurrent :: ContextKeySnapshot -> ContextKey
snapshotToContextKeyCurrent s = ContextKey { fqn: s.fqn, declaringContextFqn: s.declaringContextFqn, roles: s.roles, properties: s.properties, aspects: s.aspects }

snapshotToContextKeyOld :: OBJ.Object String -> ContextKeySnapshot -> ContextKey
snapshotToContextKeyOld ctxAliases s =
  let decl0 = s.declaringContextFqn
      declN = case OBJ.lookup decl0 ctxAliases of
        Just new -> new
        Nothing  -> decl0
  in ContextKey 
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
  let decl0 = s.declaringContextFqn
      declN = case OBJ.lookup decl0 ctxAliases of
        Just new -> new
        Nothing  -> decl0
  in RoleKey
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
  let decl0 = s.declaringRoleFqn
      declN = case OBJ.lookup decl0 roleAliases of
        Just new -> new
        Nothing  -> decl0
  in PropertyKey 
    { fqn: s.fqn
    , valueType: s.valueType
    , facets: s.facets
    , aspects: s.aspects
    , declaringRoleFqn: declN
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
  :: { contexts :: OBJ.Object ContextKeySnapshot
     , roles :: OBJ.Object RoleKeySnapshot
     , properties :: OBJ.Object PropertyKeySnapshot
     }
  -> StableIdMapping
  -> { mappingWithAliases :: StableIdMapping
     , needCuids :: { contexts :: Array String, roles :: Array String, properties :: Array String }
     }
planCuidAssignments cur mapping0 =
  let
    candidatesC = objToArrayWith snapshotToContextKeyCurrent cur.contexts
    candidatesR = objToArrayWith snapshotToRoleKeyCurrent cur.roles
    candidatesP = objToArrayWith snapshotToPropertyKeyCurrent cur.properties

    contextKeys = OBJ.values mapping0.contextKeys

    ctxAliases'  = buildContextAliasesTopologically
      defaultContextWeights
      0.5
      mapping0.contextKeys
      candidatesC
      mapping0.contexts

    rolAliases'  = buildAliases defaultRoleWeights      0.5 mapping0.roleKeys     (snapshotToRoleKeyOld ctxAliases')            candidatesR mapping0.roles (OBJ.keys mapping0.roleKeys)
    propAliases' = buildAliases defaultPropertyWeights  0.7 mapping0.propertyKeys (snapshotToPropertyKeyOld rolAliases')        candidatesP mapping0.properties (OBJ.keys mapping0.propertyKeys)

    mappingWithAliases = mapping0
      { contexts = ctxAliases'
      , roles = rolAliases'
      , properties = propAliases'
      , contextKeys = cur.contexts
      , roleKeys = cur.roles
      , propertyKeys = cur.properties
      }

    needs :: forall s. OBJ.Object s -> OBJ.Object String -> OBJ.Object String -> Array String
    needs current prevCuids aliases =
      let hasPrev fqn = OBJ.lookup fqn prevCuids /= Nothing
          reusedFromAlias fqn = findFirstJust (OBJ.keys aliases) \oldFqn ->
            case OBJ.lookup oldFqn aliases of
              Just tgt | tgt == fqn -> OBJ.lookup oldFqn prevCuids
              _ -> Nothing
      in filter (\fqn -> not (hasPrev fqn) && reusedFromAlias fqn == Nothing) (OBJ.keys current)

    needCtx  = needs cur.contexts  mapping0.contextCuids  ctxAliases'
    needRol  = needs cur.roles     mapping0.roleCuids     rolAliases'
    needProp = needs cur.properties mapping0.propertyCuids propAliases'
  in
    { mappingWithAliases
    , needCuids: { contexts: needCtx, roles: needRol, properties: needProp }
    }
  where
  objToArrayWith :: forall s a. (s -> a) -> OBJ.Object s -> Array (Tuple String a)
  objToArrayWith f o = mapMaybe (\k -> case OBJ.lookup k o of
    Nothing -> Nothing
    Just s -> Just (Tuple k (f s))) (OBJ.keys o)

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
    let candIndex = OBJ.fromFoldable (candidates <#> \(Tuple k _) -> Tuple k true)
    in foldl
      (\acc' oldFqn -> case OBJ.lookup oldFqn oldSnap of
        Nothing -> acc'
        Just sOld ->
          if OBJ.lookup oldFqn candIndex /= Nothing
            then acc'
            else case rankBest w minScore (toKey sOld) candidates of
              Just (Tuple newFqn _) -> OBJ.insert oldFqn newFqn acc'
              Nothing -> acc')
      acc
      keys
  buildContextAliasesTopologically
    :: ContextWeights
    -> Number
    -> OBJ.Object ContextKeySnapshot   -- old snapshots
    -> Array (Tuple String ContextKey) -- current candidates (canonical only)
    -> OBJ.Object String               -- initial alias map (typically mapping0.contexts)
    -> OBJ.Object String               -- result alias map
  buildContextAliasesTopologically w minScore oldSnap candidates acc0 =
    let
      candIndex = OBJ.fromFoldable (candidates <#> \(Tuple k _) -> Tuple k true)
      olds = OBJ.values oldSnap
      sorted = case sortTopologicallyEither _.fqn (singleton <<< typeUri2typeNameSpace_ <<< _.fqn) olds of
        Right xs -> xs
        Left _   -> olds
    in
      foldl
        (\acc sOld ->
           let oldFqn = sOld.fqn
           in if OBJ.lookup oldFqn candIndex /= Nothing
                then acc
                else case rankBest w minScore (snapshotToContextKeyOld acc sOld) candidates of
                  Just (Tuple newFqn _) -> OBJ.insert oldFqn newFqn acc
                  Nothing               -> acc
        )
        acc0
        sorted
-- Finalize CUID assignments: copy existing cuids from oldFqn to newFqn wherever alias old->new exists and new lacks a cuid
finalizeCuidAssignments
  :: StableIdMapping
  -> { contexts :: OBJ.Object String, roles :: OBJ.Object String, properties :: OBJ.Object String }
  -> StableIdMapping
finalizeCuidAssignments mappingWithAliases newCuids =
  let
    -- Copy existing cuids from oldFqn to newFqn wherever alias old->new exists and new lacks a cuid
    propagate
      :: OBJ.Object String  -- alias map (aliasFqn -> canonicalFqn)
      -> OBJ.Object String  -- prev cuids (fqn -> cuid)
      -> OBJ.Object String  -- result cuids (after propagation)
    propagate aliases prev =
      foldl
        (\acc oldFqn ->
          case Tuple (OBJ.lookup oldFqn aliases) (OBJ.lookup oldFqn prev) of
            Tuple (Just newFqn) (Just cuid) ->
              case OBJ.lookup newFqn acc of
                Nothing -> OBJ.insert newFqn cuid acc
                Just _  -> acc
            _ -> acc
        )
        prev
        (OBJ.keys aliases)

    -- 1) propagate cuids through aliases
    ctxCuidsProp  = propagate mappingWithAliases.contexts  mappingWithAliases.contextCuids
    rolCuidsProp  = propagate mappingWithAliases.roles     mappingWithAliases.roleCuids
    propCuidsProp = propagate mappingWithAliases.properties mappingWithAliases.propertyCuids

    -- 2) add freshly minted cuids (if any)
    ctxCuids'  = OBJ.union ctxCuidsProp  newCuids.contexts
    rolCuids'  = OBJ.union rolCuidsProp  newCuids.roles
    propCuids' = OBJ.union propCuidsProp newCuids.properties
  in
    mappingWithAliases
      { contextCuids = ctxCuids'
      , roleCuids = rolCuids'
      , propertyCuids = propCuids'
      }

