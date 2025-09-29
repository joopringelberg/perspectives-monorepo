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

module Perspectives.TypePersistence.LoadArc where

import Control.Alt (void)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Array (delete, null, filter)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String.CodeUnits as SCU
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import Effect.Class (liftEffect)
import Foreign.Object (empty, singleton)
import Foreign.Object as OBJ
import Parsing (ParseError(..))
import Partial.Unsafe (unsafePartial)
import Perspectives.Checking.PerspectivesTypeChecker (checkDomeinFile)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.Cuid2 (cuid2)
import Perspectives.DomeinCache (retrieveDomeinFile, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.Identifiers (modelUri2SchemeAndAuthority)
import Perspectives.InvertedQuery.Storable (StoredQueries)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (position2ArcPosition, runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwoState, runPhaseTwo_', toStableModelUri)
import Perspectives.Parsing.Messages (MultiplePerspectivesErrors, PerspectivesError(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.SideCar.PhantomTypedNewtypes (Readable)
import Perspectives.Sidecar.NormalizeTypeNames (StableIdMappingForModel, getinstalledModelCuids, normalizeTypes)
import Perspectives.Sidecar.StableIdMapping (ContextUri(..), ModelUri(..), Stable, StableIdMapping, emptyStableIdMapping, idUriForContext, loadStableMapping)
import Perspectives.Sidecar.UniqueTypeNames (extractKeysFromDfr)
import Perspectives.Sidecar.UniqueTypeNames as UTN
import Prelude (bind, discard, pure, show, ($), (<<<), (==), (<>), (>=), (&&), (-), not, (>=>))

-- | The functions in this module load Arc files and parse and compile them to DomeinFiles.
-- | Some functions expect a CRL file with the same name and add the instances found in them
-- | to the DomeinFile.
-- | Notice that these functions are more about creating DomeinFiles than about using them.
-- | A function to start using a particular model, by
-- |  * downloading the DomeinFile
-- |  * installing it in the local Couchdb installation
-- |  * and adding the Domain instances to the local Couchdb installation,
-- |  * can be found in the module Perspectives.Extern.Couchdb.

type Source = String

-- | Parses and compiles the ARC file to a DomeinFile. 
-- | Parameter `saveInCache` determines whether to cache the DomeinFIle. Does not store the DomeinFile.
-- | However, will load, cache and store dependencies of the model.
loadAndCompileArcFile_ :: ModelUri Stable -> Source -> Boolean -> String -> MonadPerspectivesTransaction (Either (Array PerspectivesError) (Tuple (DomeinFile Stable) StoredQueries))
loadAndCompileArcFile_ dfid text saveInCache modelCuid = do
  -- Retrieve existing sidecar (if any) from repository. domeinFilename should be Stable.
  mMapping <- lift $ loadStableMapping dfid
  x <- loadAndCompileArcFileWithSidecar_ dfid text saveInCache mMapping modelCuid
  pure case x of
    Left errs -> Left errs
    Right (Tuple df (Tuple iqs _m)) -> Right (Tuple df iqs)

-- New: sidecar-aware API that returns the updated mapping with results.
loadAndCompileArcFileWithSidecar_ :: ModelUri Stable -> Source -> Boolean -> Maybe StableIdMapping -> String -> MonadPerspectivesTransaction (Either (Array PerspectivesError) (Tuple (DomeinFile Stable) (Tuple StoredQueries StableIdMapping)))
loadAndCompileArcFileWithSidecar_ dfid@(ModelUri stableModelUri) text saveInCache mMapping modelCuid =
  catchError
    ( do
        (r :: Either ParseError ContextE) <- lift $ lift $ runIndentParser text domain
        case r of
          Left e -> pure $ Left [ parseError2PerspectivesError e ]
          Right ctxt@(ContextE { id: sourceIdReadable, pos }) ->
            -- If we have a mapping, it is sure to have the enclosing Domein context, so then we can map ModelUri Readable to ModelUri Stable.
            if testModelName sourceIdReadable then do
              (Tuple result state :: Tuple (Either MultiplePerspectivesErrors (DomeinFile Readable)) PhaseTwoState) <-
                lift $ lift $ runPhaseTwo_' (traverseDomain ctxt) defaultDomeinFileRecord empty empty Nil
              case result of
                Left e -> pure $ Left e
                Right (DomeinFile dr'@{ id }) -> do
                  dr''@{ referredModels } <- pure dr' { referredModels = state.referredModels }
                  -- We should load referred models if they are missing (but not the model we're compiling!).
                  -- Throw an error if a referred model is not installed. It will show up in the arc feedback.
                  -- NOTICE: referredModels is in termen van Readable, dus dat moet nog omgezet worden naar Stable.
                  installedModelCuids <- lift $ getinstalledModelCuids false -- unversioned.
                  for_ (delete id state.referredModels) (lift <<< (toStable installedModelCuids >=> retrieveDomeinFile))

                  (x' :: Either MultiplePerspectivesErrors (Tuple (DomeinFileRecord Readable) StoredQueries)) <-
                    lift $ phaseThree dr'' state.postponedStateQualifiedParts state.screens
                  case x' of
                    Left e -> pure $ Left e
                    Right (Tuple correctedDFR@{ referredModels: refModels } invertedQueries) -> do
                      -- Base mapping from caller
                      let
                        mapping0 = case mMapping of
                          Nothing -> emptyStableIdMapping { contextCuids = singleton stableModelUri modelCuid, modelIdentifier = ModelUri $ (unsafePartial $ modelUri2SchemeAndAuthority stableModelUri) <> "#" <> modelCuid }
                          Just m0 -> m0

                      -- Extend aliases and compute current key snapshots
                      let cur = extractKeysFromDfr correctedDFR
                      let planned = UTN.planCuidAssignments cur mapping0

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

                      let
                        newCuids =
                          { contexts: OBJ.fromFoldable ctxPairs
                          , roles: OBJ.fromFoldable rolPairs
                          , properties: OBJ.fromFoldable propPairs
                          , states: OBJ.fromFoldable statePairs
                          , views: OBJ.fromFoldable viewPairs
                          , actions: OBJ.fromFoldable actionPairs
                          }

                      let mapping1 = UTN.finalizeCuidAssignments planned.mappingWithAliases newCuids

                      -- Run the type checker (NOTE: but a stub, right now).
                      typeCheckErrors <- lift $ checkDomeinFile (DomeinFile correctedDFR)
                      if null typeCheckErrors then do
                        -- Remove the self-referral and add the source.
                        df <- pure $ DomeinFile correctedDFR
                          { referredModels = delete id refModels
                          , arc = text
                          -- Notice that this is the UNVERSIONED id. It will be overwritten with the versioned id when uploading to the repository.
                          , _id = takeGuid $ unwrap id
                          }
                        -- Now replace the readable name given by the modeller with a cuid, in FQNs:
                        normalizedDf <- lift $ normalizeTypes df mapping1

                        if saveInCache then void $ lift $ storeDomeinFileInCache (toStableModelUri id) normalizedDf else pure unit

                        pure $ Right $ Tuple normalizedDf (Tuple invertedQueries mapping1)
                      else
                        pure $ Left typeCheckErrors
            else
              pure $ Left [ (DomeinFileIdIncompatible stableModelUri (ModelUri sourceIdReadable) pos) ]
    )
    (\e -> pure $ Left [ Custom (show e) ])

  where
  -- The model under construction itself is not remapped, only its dependencies.
  -- With this function we cover both cases: models in terms of Stable and in terms of Readable.
  toStable :: StableIdMappingForModel -> ModelUri Readable -> MonadPerspectives (ModelUri Stable)
  toStable m idfid@(ModelUri mUri) = case lookup (ModelUri mUri) m of
    Nothing -> pure $ toStableModelUri idfid
    Just (ModelUri s) -> pure $ (ModelUri s)

  testModelName :: String -> Boolean
  testModelName sourceIdReadable = case mMapping of
    Nothing -> true
    -- look up the context CUID
    Just m -> case idUriForContext m (ContextUri sourceIdReadable) of
      Nothing -> false
      Just s -> s == stableModelUri

type ArcSource = String
type CrlSource = String

parseError2PerspectivesError :: ParseError -> PerspectivesError
parseError2PerspectivesError (ParseError message pos) = ParserError message (position2ArcPosition pos)
