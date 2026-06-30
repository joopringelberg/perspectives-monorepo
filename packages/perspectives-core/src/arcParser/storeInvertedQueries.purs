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

module Perspectives.Parsing.Arc.PhaseThree.StoreInvertedQueries where

import Control.Monad.Except (lift)
import Control.Monad.Reader (ReaderT, ask)
import Data.Array (concat, fromFoldable, head, union)
import Data.Foldable (for_)
import Data.Map (Map, empty, singleton, toUnfoldable, values) as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Perspective.InvertedQuery.Indices (typeLevelKeyForContextQueries, typeLevelKeyForFilledQueries, typeLevelKeyForFillerQueries, typeLevelKeyForPropertyQueries, typeLevelKeyForRoleQueries)
import Perspectives.ArrayUnions (ArrayUnions(..))
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap (EncodableMap(..))
import Perspectives.InvertedQuery (InvertedQuery(..), QueryWithAKink(..), backwards, forwards)
import Perspectives.InvertedQueryKey (serializeInvertedQueryKey)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwo', addStorableInvertedQuery, getsDF, lift2, throwError)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Query.Kinked (invert)
import Perspectives.Query.QueryTypes (Domain, QueryFunctionDescription(..), Range, RoleInContext(..), addTermOnRight, domain, domain2roleInContext, makeComposition, mandatory, range)
import Perspectives.Representation.ADT (allLeavesInADT)
import Perspectives.Representation.Class.PersistentType (getCalculatedProperty)
import Perspectives.Representation.Class.Property (calculation)
import Perspectives.Representation.Perspective (ModificationSummary)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..)) as QF
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleType(..), StateIdentifier)
import Perspectives.Utilities (prettyPrint)
import Prelude (Unit, bind, discard, flip, pure, show, unit, ($), (<$>), (<>), (==), (>=>), (<*>))

type WithModificationSummary = ReaderT ModificationSummary (PhaseTwo' MonadPerspectives)

storeInvertedQuery
  :: QueryWithAKink
  -> Array RoleType
  -> Array StateIdentifier
  -> Map.Map PropertyType (Array StateIdentifier)
  -> Boolean
  -> Boolean
  -> Maybe ArcPosition
  -> WithModificationSummary Unit
storeInvertedQuery qwk users roleStates statesPerProperty selfOnly authorOnly perspectiveStartPosition = storeInvertedQuery' qwk users roleStates statesPerProperty selfOnly authorOnly perspectiveStartPosition Nothing Nothing

-- | Modifies the DomeinFile in PhaseTwoState.
storeInvertedQuery'
  :: QueryWithAKink
  -> Array RoleType
  -> Array StateIdentifier
  -> Map.Map PropertyType (Array StateIdentifier)
  -> Boolean
  -> Boolean
  -> Maybe ArcPosition
  -> Maybe QueryFunctionDescription
  -> Maybe RoleType
  -> WithModificationSummary Unit
storeInvertedQuery' qwk@(ZQ backward forward) users roleStates statesPerProperty selfOnly authorOnly perspectiveStartPosition mfilter mCalcUserRoleType = do
  -- What is confusing about what follows is that it just seems to handle the first step of an inverted query.
  -- What about the steps that follow?
  -- Reflect that we have generated *separate inverted queries* for all these steps, each 'kinking' the original query
  -- at a different position.
  -- So if the original query was:
  --    s1 >> s2 >> s3
  -- we generate:
  --    backwards (forwards)
  --    ^s1 (s2 >> s3)          and for this we store an InvertedQuery with s1
  --    ^s1 << ^s2 (s3)         and for this we store an InvertedQuery with s2
  --    ^s1 << ^s2 << ^s3 ()    and for this we store an InvertedQuery with s3
  -- where x << y equals y >> x.

  case backward of
    -- Think of this as: {first source step} << filter criterium << {last criterium step} (NOTICE the inverse composition step <<)
    Just (BQD _ (BinaryCombinator ComposeF) qfd1 qfd2 _ _ _) -> case qfd2 of
      -- qfd1 is the last step of the INVERTED criterium; 'criterium' in FilterF is the ORIGINAL criterium. 
      (BQD _ (BinaryCombinator ComposeF) filter@(UQD _ FilterF criterium _ _ _) source _ _ _) -> do
        case mCalcUserRoleType of
          -- For Calculated User role detection queries: produce a filter-based description so that
          -- `handleNewCalculatedUsersForBinding` can apply both backward (filter >> source) and
          -- forward (filter) to the same role instance, checking the filter before recognising
          -- the instance as a new Calculated User.
          -- The second storeInvertedQuery' call (which would store an RTPropertyKey or similar with
          -- a property-getter forward) is intentionally omitted: it would incorrectly return
          -- property values instead of role instances at runtime.
          Just _ ->
            unsafePartial $ setPathForStep
              qfd1
              (ZQ (Just $ makeComposition filter source) (Just filter))
              users
              roleStates
              statesPerProperty
              selfOnly
              authorOnly
              perspectiveStartPosition
              Nothing
              mCalcUserRoleType
          -- Regular case: drop the filter to detect changes even when the filter evaluates to
          -- false (the user may have just *lost* visibility of an item). Also store a separate
          -- query with the filter prepended, for the case where the filter condition now becomes
          -- satisfied.
          Nothing -> do
            -- Drop the filter. Store  {first source step} << {last criterium step}
            unsafePartial $ setPathForStep
              qfd1
              (ZQ (Just $ makeComposition qfd1 source) $ forwards qwk)
              users
              roleStates
              statesPerProperty
              selfOnly
              authorOnly
              perspectiveStartPosition
              Nothing
              mCalcUserRoleType
            -- prepend the filter to the source. Store {first source step} << filter criterium.
            storeInvertedQuery'
              (ZQ (Just source) $ forwards qwk)
              users
              (roleStates `union` (concat $ fromFoldable $ Map.values statesPerProperty))
              statesPerProperty
              selfOnly
              authorOnly
              perspectiveStartPosition
              (Just filter)
              mCalcUserRoleType
      -- unsafePartial $ setPathForStep 
      --   source
      --   (ZQ (Just source) $ forwards qwk)
      --   users 
      --   (roleStates `union` (concat $ fromFoldable $ Map.values statesPerProperty)) 
      --   statesPerProperty 
      --   selfOnly 
      --   (Just filter)
      _ -> unsafePartial $ setPathForStep qfd1 qwk users roleStates statesPerProperty selfOnly authorOnly perspectiveStartPosition mfilter mCalcUserRoleType
    (Just b@(SQD _ _ _ _ _)) -> unsafePartial $ setPathForStep b qwk users roleStates statesPerProperty selfOnly authorOnly perspectiveStartPosition mfilter mCalcUserRoleType
    (Just b@(MQD _ _ _ _ _ _)) -> unsafePartial $ setPathForStep b qwk users roleStates statesPerProperty selfOnly authorOnly perspectiveStartPosition mfilter mCalcUserRoleType
    otherwise -> lift $ throwError (Custom $ "impossible case in setInvertedQueries:\n" <> prettyPrint otherwise)

-- | The function is partial, because we just handle the SQD and MQD cases.
-- | The first argument is the first step of the backward path of the second argument (but, in the case the backwards part holds a filter, it may have been changed).
-- | This is not a recursive function! It merely adds the QueryWithAKink to a Context, Role or Property type.
-- | Modifies the DomeinFile in PhaseTwoState.
setPathForStep
  :: Partial
  => QueryFunctionDescription
  -> -- First step of the backward part of the next argument.
  QueryWithAKink
  -> Array RoleType
  -> Array StateIdentifier
  -> Map.Map PropertyType (Array StateIdentifier)
  -> Boolean
  -> Boolean
  -> Maybe ArcPosition
  -> Maybe QueryFunctionDescription
  -> Maybe RoleType
  -> WithModificationSummary Unit
setPathForStep qfd@(SQD dom qf ran fun man) qWithAK users states statesPerProperty selfOnly authorOnly perspectiveStartPosition mfilter mCalcUserRoleType = do
  model <- getsDF _.id
  { modifiesRoleInstancesOf, modifiesRoleBindingOf, modifiesPropertiesOf } <- ask
  case qf of
    -- The original property expression can never be the source in `filter source with criterium`, so we ignore mfilter.
    QF.Value2Role pt ->
      if dom == ran
      -- This handles cases like `step >>= sum`, where we construct a Value2Role
      -- but really must ignore it.
      then pure unit
      else case pt of
        ENP p -> do
          keys <- lift $ lift $ lift $ typeLevelKeyForPropertyQueries p qfd
          lift $ addStorableInvertedQuery
            { keys: serializeInvertedQueryKey <$> keys
            , queryType: "RTPropertyKey"
            , query:
                ( InvertedQuery
                    { description: qWithAK
                    , backwardsCompiled: Nothing
                    , forwardsCompiled: Nothing
                    , users
                    -- Default value. Should be computed per case.
                    , modifies: false
                    , statesPerProperty: EncodableMap statesPerProperty
                    -- The inverted query is the inversion of the computation of the Perspective object. This object should be available
                    -- to the user in all states that the perspective is valid in. These states are not listed explicitly in the Perspective,
                    -- but can be computed from the union of the StateSpecs of the roleVerbs, propertyVerbs and actions.
                    , states
                    , selfOnly
                    , authorOnly
                    , perspectiveStartPosition
                    , calculatedUserRoleType: mCalcUserRoleType
                    }
                )
            , model
            }
        CP _ -> pure unit

    -- FILLED STEP
    -- NOTE: qfd here is a `FilledF enr ctxt` step, which is the INVERSE of the original
    -- `FillerF` step at the kink point.  Its domain is RDOM filler, its range is RDOM filled.
    -- We remove this first step so that at runtime the remaining backward is applied to the
    -- *filled* role instance (range of FilledF) rather than the filler.  The key is stored
    -- under the filled role type so that it fires whenever that filled role acquires a new filler.
    -- No compensating step is added to the forward part because `usersWithPerspectiveOnRoleBinding`
    -- applies the forward to the filler directly.
    -- After step removal: domain(new backward) = range(FilledF) = RDOM filled.
    -- The query is indexed under the **FILLED** role for two reasons:
    --  * the filler may well be in another model, leading to an InvertedQuery for another domain;
    --  * if at runtime a filler is used that is a specialisation of the required type, it will
    --    still trigger the inverted query.
    QF.FilledF enr ctxt ->
      let
        oneStepLess = removeFirstBackwardsStep qWithAK (\_ _ _ -> Nothing)
        description = case mfilter of
          Nothing -> case oneStepLess of
            -- If backwards of oneStepLess is Nothing, the backwards step of qWithAK (== qfd) consisted of just
            -- a single step and that was FilledF.
            -- Consequently, the role instance that we are going to apply the backwards part of the inverted query to,
            -- is already the end result we want to obtain. Hence we put the Identity function in the place of backwards,
            -- where the domain and range are the range of the backward step.
            ZQ Nothing fwd -> ZQ (Just (SQD ran (QF.DataTypeGetter IdentityF) ran True True)) fwd
            x -> x
          -- Add the FillerF step to the criterium of the FilterF step;
          -- Prepend the modified filter to the backwards part.
          Just filter -> preprendToCriterium
            oneStepLess
            (\ran' dom' man' -> SQD ran' (QF.DataTypeGetter FillerF) dom' True man')
            filter
      in
        do
          (ArrayUnions keys) <- lift $ lift $ lift $ typeLevelKeyForFilledQueries qfd -- The first step of the backwards part of the original inverted query, being FilledF.
          lift $ addStorableInvertedQuery
            { keys: serializeInvertedQueryKey <$> keys
            , queryType: "RTFilledKey"
            , query:
                ( InvertedQuery
                    { description
                    , backwardsCompiled: Nothing
                    , forwardsCompiled: Nothing
                    , users
                    -- Default value. Should be computed per case.
                    , modifies: false
                    , statesPerProperty: EncodableMap statesPerProperty
                    , states
                    , selfOnly
                    , authorOnly
                    , perspectiveStartPosition
                    , calculatedUserRoleType: mCalcUserRoleType
                    }
                )
            , model
            }

    -- FILLER STEP
    -- NOTE: qfd here is a `FillerF` step, which is the INVERSE of the original `FilledF` step
    -- at the kink point.  Its domain is RDOM filled, its range is RDOM filler.
    -- We remove this first step so that at runtime the remaining backward is applied to the
    -- *filler* role instance (range of FillerF) rather than the filled role.
    -- The key is stored under the filled role type (domain of FillerF) so that it fires whenever
    -- that filled role acquires a new filler.  No compensating step is added to the forward part.
    -- After step removal: domain(new backward) = range(FillerF) = RDOM filler.
    QF.DataTypeGetter QF.FillerF -> do
      (ArrayUnions keys) <- lift $ lift $ lift $ typeLevelKeyForFillerQueries qfd
      oneStepLess <- pure $ removeFirstBackwardsStep qWithAK (\_ _ _ -> Nothing)
      description <- pure $ case mfilter of
        Nothing -> case oneStepLess of
          -- If backwards of oneStepLess is Nothing, the backwards step of qWithAK (== qfd) consisted of just
          -- a single step and that was FillerF.
          -- Consequently, the role instance that we are going to apply the backwards part of the inverted query to,
          -- is already the end result we want to obtain. Hence we put the Identity function in the place of backwards,
          -- where the domain and range are the range of the backward step.
          ZQ Nothing fwd -> ZQ (Just (SQD ran (QF.DataTypeGetter IdentityF) ran True True)) fwd
          x -> x
        -- Add the FilledF step to the criterium of the FilterF step;
        -- Prepend the modified filter to the backwards part.
        Just filter -> preprendToCriterium
          oneStepLess
          -- NOTICE. The domain, being an ADT, may have multiple RoleInContext combinations. But we can only use one of these
          -- as parameters of FilledF. We arbitrarily choose the first.
          ( \ran' dom' man' ->
              let
                RoleInContext { context, role } = unsafePartial fromJust $ head $ allLeavesInADT (domain2roleInContext dom')
              in
                SQD ran' (FilledF role context) dom' True man'
          )
          filter
      case dom of
        _ -> lift $ addStorableInvertedQuery
          { keys: serializeInvertedQueryKey <$> keys
          , queryType: "RTFillerKey"
          , query:
              ( InvertedQuery
                  { description: description
                  , backwardsCompiled: Nothing
                  , forwardsCompiled: Nothing
                  , users
                  -- Default value. Should be computed per case.
                  , modifies: false
                  , statesPerProperty: EncodableMap statesPerProperty
                  -- pas states aan als een stap is weggehaald. D.w.z. Verwijder de states van het oorspronkelijke domein van backwards, voeg als state toe de ground state van het nieuwe domein van backwards.
                  , states
                  , selfOnly
                  , authorOnly
                  , perspectiveStartPosition
                  , calculatedUserRoleType: mCalcUserRoleType
                  }
              )
          , model
          }

    -- Treat the variant with a context restriction in exactly the same way as without that restriction.
    QF.DataTypeGetterWithParameter QF.FillerF _ -> setPathForStep (SQD dom (QF.DataTypeGetter QF.FillerF) ran fun man) qWithAK users states statesPerProperty selfOnly authorOnly perspectiveStartPosition mfilter mCalcUserRoleType

    -- NOTE: qfd here is a `RolGetter (ENR role)` step, which is the INVERSE of the original
    -- `ContextF` step at the kink point.  Its domain is CDOM ctx, its range is RDOM roleType.
    -- At runtime the RTRoleKey query is applied starting from a role *instance*, not a context.
    -- Therefore we drop the RolGetter step (domain CDOM).  After removal the remaining backward
    -- has domain = range(RolGetter) = RDOM roleType — correct for a role-instance argument.
    -- Because the forward part was designed to start from the context (range of ContextF), we
    -- compensate by prepending a ContextF step to the forward part.
    -- When the full backward consisted of only this single RolGetter step (ZQ Nothing _), there
    -- is nothing meaningful left to store, so we silently discard the description.
    QF.RolGetter roleType -> case roleType of
      ENR role ->
        let
          oneStepLess = removeFirstBackwardsStep
            qWithAK
            (\ran' dom' man' -> Just $ SQD ran' (QF.DataTypeGetter ContextF) dom' True man')
          description = case mfilter of
            Nothing -> oneStepLess
            -- Add the context step to the criterium of the FilterF step;
            -- Prepend the modified filter to the backwards part.
            Just filter -> preprendToCriterium
              oneStepLess
              (\ran' dom' man' -> SQD ran' (QF.DataTypeGetter ContextF) dom' True man')
              filter
        in
          case description of
            ZQ Nothing _ -> pure unit
            left -> do
              (ArrayUnions keys) <- lift $ typeLevelKeyForRoleQueries qfd
              lift $ addStorableInvertedQuery
                { keys: serializeInvertedQueryKey <$> keys
                , queryType: "RTRoleKey"
                , query:
                    ( InvertedQuery
                        { description
                        , backwardsCompiled: Nothing
                        , forwardsCompiled: Nothing
                        , users
                        -- Default value. But is not used while adding a model to an installation.
                        , modifies: false
                        , statesPerProperty: EncodableMap statesPerProperty
                        , states
                        , selfOnly
                        , authorOnly
                        , perspectiveStartPosition
                        , calculatedUserRoleType: mCalcUserRoleType
                        }
                    )
                , model
                }
      CR _ -> lift $ throwError $ Custom "Implement the handling of Calculated Roles in setPathForStep."

    -- NOTE: qfd here is a `ContextF` step, which is the INVERSE of the original `RolGetter
    -- (ENR role)` step at the kink point.  Its domain is RDOM roleType, its range is CDOM ctx.
    -- At runtime the RTContextKey backward is applied starting from a role *instance*.  Because
    -- the ContextF step's domain is already RDOM, we do NOT need to remove it; `handleBackwardQuery`
    -- passes the role instance directly as the starting point.
    QF.DataTypeGetter QF.ContextF -> do
      description <- case mfilter of
        Nothing -> pure qWithAK
        -- If there is a filter, prepend it to the backwards part.
        Just filter -> pure $ ZQ ((makeComposition filter) <$> backwards qWithAK) (forwards qWithAK)
      (ArrayUnions keys) <- lift $ typeLevelKeyForContextQueries qfd
      lift $ addStorableInvertedQuery
        { keys: serializeInvertedQueryKey <$> keys
        , queryType: "RTContextKey"
        , query:
            ( InvertedQuery
                { description
                , backwardsCompiled: Nothing
                , forwardsCompiled: Nothing
                , users
                -- Default value. But is not used while adding a model to an installation.
                , modifies: false
                , statesPerProperty: EncodableMap statesPerProperty
                , states
                , selfOnly
                , authorOnly
                , perspectiveStartPosition
                , calculatedUserRoleType: mCalcUserRoleType
                }
            )
        , model
        }
      -- When the perspective object has been fully inverted (no remaining forward steps), also
      -- handle calculated properties: invert each property's calculation and combine with the
      -- backwards path so that peers are notified when the underlying enumerated properties change.
      case forwards qWithAK of
        Nothing ->
          for_ (Map.toUnfoldable statesPerProperty :: Array (Tuple PropertyType (Array StateIdentifier)))
            \(Tuple propType propStates) ->
              case propType of
                CP calcPropType -> do
                  propCalc <- lift $ lift2 $ (getCalculatedProperty >=> calculation) calcPropType
                  propInversions <- lift $ invert propCalc
                  for_ propInversions \(ZQ bwProp fwdProp) ->
                    -- Only handle complete inversions to avoid infinite recursion and
                    -- because the primary use-case is: underlying enumerated property value changes.
                    case fwdProp of
                      Nothing ->
                        storeInvertedQuery
                          (ZQ (addTermOnRight <$> bwProp <*> (backwards qWithAK)) Nothing)
                          users
                          propStates
                          (Map.singleton propType propStates)
                          selfOnly
                          authorOnly
                          perspectiveStartPosition
                      Just _ -> pure unit
                ENP _ -> pure unit
        Just _ -> pure unit

    -- The query would be added to roleInvertedQueries of the context. Such inverse queries are run when a new
    -- instance of the role type is added to the context (or when it is removed). But the external role never changes,
    -- so this is superfluous.
    QF.DataTypeGetter QF.ExternalRoleF -> pure unit

    -- Ignore ExternalRoleF and IdentityF in an inverse path. We do not
    -- establish queries to gather affected contexts on them. For IdentityF this is
    -- because we will establish a query on the next step (or have done so at the
    -- previous step). For ExternalRoleF this is because there cannot be ContextDeltas
    -- for the External role.
    QF.DataTypeGetter QF.IdentityF -> pure unit

    -- Exactly the same treatment as case RolGetter for ENR.
    QF.DataTypeGetterWithParameter GetRoleInstancesForContextFromDatabaseF et ->
      let
        -- We remove the first step of the backwards path, because we apply it runtime not to the context, but to
        -- the new role instance. We skip the RolGetter step because its cardinality is larger than one.
        -- Because the forward part will be applied to that same role (instead of the context), we have to compensate
        -- for that by prepending it with the inversal of the first backward step - which is, by construction, a
        -- `context` step.
        oneStepLess = removeFirstBackwardsStep
          qWithAK
          (\ran' dom' man' -> Just $ SQD ran' (QF.DataTypeGetter ContextF) dom' True man')
        description = case mfilter of
          Nothing -> oneStepLess
          -- Add the context step to the criterium of the FilterF step;
          -- Prepend the modified filter to the backwards part.
          Just filter -> preprendToCriterium
            oneStepLess
            (\ran' dom' man' -> SQD ran' (QF.DataTypeGetter ContextF) dom' True man')
            filter
      in
        case description of
          ZQ Nothing _ -> pure unit
          left -> do
            (ArrayUnions keys) <- lift $ typeLevelKeyForRoleQueries qfd
            lift $ addStorableInvertedQuery
              { keys: serializeInvertedQueryKey <$> keys
              , queryType: "RTRoleKey"
              , query:
                  ( InvertedQuery
                      { description
                      , backwardsCompiled: Nothing
                      , forwardsCompiled: Nothing
                      , users
                      -- Default value. But is not used while adding a model to an installation.
                      , modifies: false
                      , statesPerProperty: EncodableMap statesPerProperty
                      , states
                      , selfOnly
                      , authorOnly
                      , perspectiveStartPosition
                      , calculatedUserRoleType: mCalcUserRoleType
                      }
                  )
              , model
              }

    QF.RoleIndividual _ -> pure unit

    QF.ContextIndividual _ -> pure unit

    _ -> lift $ throwError $ Custom "setPathForStep: there should be no other cases. This is a system programming error."

setPathForStep (MQD _ qf _ _ _ _) qWithAK users states statesPerProperty selfOnly authorOnly perspectiveStartPosition mfilter mCalcUserRoleType =
  case qf of
    -- ExternalCoreRoleGetter is the inversion of a role individual step, such as sys:Me,
    -- and there are no other query steps that invert to it.
    -- This inversion should be stored with the type of the role individual. 
    -- However, by definition, no instances of such a context individual will be created beyond the original
    -- one that is created at setup of a model. In other words, we have nothing to gain by setting up a new-instance-detection
    -- system at that type!
    -- Therefore we simply ignore this step.
    -- NOTICE: the inversion is useful, as it will be part of backwards queries - we just don't store the inverted query of such
    -- a query that is kinked at this step.
    QF.ExternalCoreRoleGetter f -> pure unit

    -- ExternalCoreContextGetter is the inversion of a context individual step, such as sys:TheWorld,
    -- and there are no other query steps that invert to it.
    -- This inversion should be stored with the type of the context individual. 
    -- However, by definition, no instances of such a context individual will be created beyond the original
    -- one that is created at setup of a model. In other words, we have nothing to gain by setting up a new-instance-detection
    -- system at that type!
    -- Therefore we simply ignore this step.
    -- NOTICE: the inversion is useful, as it will be part of backwards queries - we just don't store the inverted query of such
    -- a query that is kinked at this step.
    QF.ExternalCoreContextGetter f -> pure unit
    _ -> lift $ throwError $ Custom $ "setPathForStep: uncovered case:" <> show qf <> " in MQD. This is a system programming error."

------------------------------------------------------------------------------------------
---- REMOVE FIRST BACKWARDS STEP
------------------------------------------------------------------------------------------
removeFirstBackwardsStep
  :: QueryWithAKink
  ->
  -- A function from domain and range of the step to prepend, and whether it is mandatory.
  -- For RoleBindersF (fills step) this function always returns Nothing, and thus does not affect the
  -- forwards part returned from `removeFirstBackwardsStep`.
  (Domain -> Range -> ThreeValuedLogic -> Maybe QueryFunctionDescription)
  -> QueryWithAKink
removeFirstBackwardsStep q@(ZQ backward forward) originalStepF = case backward, forward of
  (Just (BQD _ (BinaryCombinator ComposeF) firstBackwardStep qfd2 _ _ _)), (Just forward') -> ZQ
    (Just qfd2)
    -- The domain and range of the new forward step are those of the first backward step switched.
    -- We keep its mandatory status.
    (Just $ maybe forward' (flip makeComposition forward') (originalStepF (range firstBackwardStep) (domain firstBackwardStep) (mandatory firstBackwardStep)))
  (Just (BQD _ (BinaryCombinator ComposeF) firstBackwardStep qfd2 _ _ _)), Nothing -> ZQ
    (Just qfd2)
    -- The domain and range of the new forward step are those of the first backward step switched.
    -- We keep its mandatory status.
    (originalStepF (range firstBackwardStep) (domain firstBackwardStep) (mandatory firstBackwardStep))
  (Just i@(SQD dom _ ran fun man)), (Just forward') ->
    ( ZQ
        Nothing
        -- The domain and range of the new forward step are those of the first backward step switched.
        -- We keep its mandatory status.
        (Just $ maybe forward' (flip makeComposition forward') (originalStepF ran dom man))
    )
  (Just i@(SQD dom _ ran fun man)), Nothing ->
    ( ZQ
        Nothing
        -- The domain and range of the new forward step are those of the first backward step switched.
        -- We keep its mandatory status.
        (originalStepF ran dom man)
    )
  _, _ -> q

-- Add an extra step to the criterium of the FilterF step;
-- Prepend the modified filter to the backwards part.
preprendToCriterium
  :: QueryWithAKink
  ->
  -- A function from domain and range of the step to prepend to the criterium of the filter, and whether it is mandatory.
  (Domain -> Range -> ThreeValuedLogic -> QueryFunctionDescription)
  -> QueryFunctionDescription
  -> QueryWithAKink
preprendToCriterium q@(ZQ backward forward) createExtraStep filter = case filter of
  (UQD dom FilterF criterium ran fun man) -> case backward of
    -- backward is just the filter with its criterium augmented by an initial extra step.
    Nothing -> ZQ (Just $ (UQD dom FilterF (makeComposition (createExtraStep dom ran man) criterium) ran fun man)) forward
    -- prepend to backward the filter with its criterium augmented by an initial extra step.
    Just bw -> ZQ (Just $ makeComposition (UQD dom FilterF (makeComposition (createExtraStep dom ran man) criterium) ran fun man) bw) forward
  _ -> q

------------------------------------------------------------------------------------------
---- STORE CALCULATED USER INVERTED QUERY
------------------------------------------------------------------------------------------
-- | Store an InvertedQuery generated from the calculation of a Calculated User Role, for the purpose of
-- | detecting new Calculated User role instances that arise when a role binding changes.
-- | Uses an empty modification summary (these queries don't represent modifications).
storeCalculatedUserInvertedQuery
  :: RoleType
  -> QueryWithAKink
  -> WithModificationSummary Unit
storeCalculatedUserInvertedQuery calcUserRoleType qwk =
  storeInvertedQuery' qwk [] [] Map.empty false false Nothing Nothing (Just calcUserRoleType)