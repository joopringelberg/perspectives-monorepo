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
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (delete, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import Effect.Exception (error)
import Foreign.Object (empty)
import Parsing (ParseError(..))
import Perspectives.Checking.PerspectivesTypeChecker (checkDomeinFile)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.DomeinCache (retrieveDomeinFile, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.Identifiers (modelUriVersion, unversionedModelUri)
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
import Perspectives.Sidecar.NormalizeTypeNames (StableIdMappingForModel, getinstalledModelCuids, normalizeInvertedQueries, normalizeTypes)
import Perspectives.Sidecar.StableIdMapping (ContextUri(..), ModelUri(..), Stable, StableIdMapping, fromLocalModels, fromRepository, idUriForContext, loadStableMapping)
import Perspectives.Sidecar.UniqueTypeNames as UTN
import Prelude (bind, discard, pure, show, ($), (/=), (<<<), (<>), (==), (>=>))

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
-- | ModelUri should be Stable and versioned.
-- | modelUriReadable should be the Readable ModelUri with version for error reporting.
loadAndCompileArcFile_ :: ModelUri Stable -> Source -> Boolean -> String -> String -> Maybe String -> MonadPerspectivesTransaction (Either (Array PerspectivesError) (Tuple (DomeinFile Stable) (Tuple StoredQueries StableIdMapping)))
loadAndCompileArcFile_ dfid text saveInCache modelCuid modelUriReadable mbasedOnVersion = do
  -- Retrieve existing sidecar (if any) from repository. domeinFilename should be Stable.
  mMapping <- lift $ loadStableMapping dfid fromRepository
  version <- case modelUriVersion (unwrap dfid) of
    Nothing -> throwError $ error ("ModelUri " <> show dfid <> " is expected to be versioned.")
    Just v -> pure v
  case mMapping of
    -- The case below is when we compile a version that hasn't been compiled before.
    Nothing -> do
      mmapping <- case mbasedOnVersion of
        -- In this case, we take the mapping from the indicated version, from the Repository.
        Just basedOnVersion -> lift $ loadStableMapping (ModelUri basedOnVersion) fromRepository
        -- In this case, we take the mapping from the local models.
        Nothing -> lift $ loadStableMapping dfid fromLocalModels
      -- In this case, we generate new CUIDs. Most likely this is the first version ever for this model.
      -- Nothing -> pure Nothing
      loadAndCompileArcFileWithSidecar_ (over ModelUri unversionedModelUri dfid) text saveInCache mmapping modelCuid modelUriReadable version
    -- The case below is when we've compiled this version before.
    Just _ -> loadAndCompileArcFileWithSidecar_ (over ModelUri unversionedModelUri dfid) text saveInCache mMapping modelCuid modelUriReadable version

-- New: sidecar-aware API that returns the updated mapping with results.
-- | ModelUri should be Stable and unversioned.
-- | Version should equal the version of the domain declaration in the ARC file.
-- | modelUriReadable should be the Readable ModelUri with version for error reporting.
loadAndCompileArcFileWithSidecar_ :: ModelUri Stable -> Source -> Boolean -> Maybe StableIdMapping -> String -> String -> String -> MonadPerspectivesTransaction (Either (Array PerspectivesError) (Tuple (DomeinFile Stable) (Tuple StoredQueries StableIdMapping)))
loadAndCompileArcFileWithSidecar_ dfid@(ModelUri stableModelUri) text saveInCache mMapping modelCuid modelUriReadable version =
  catchError
    ( do
        (r :: Either ParseError ContextE) <- lift $ lift $ runIndentParser text domain
        case r of
          Left e -> pure $ Left [ parseError2PerspectivesError e ]
          Right (ContextE rec@{ id: sourceIdReadable, pos }) -> do
            -- sourceIdReadable should be versioned.
            -- If we have a mapping, it is sure to have the enclosing Domein context, so then we can map ModelUri Readable to ModelUri Stable.
            mversionOfSourceIdReadable <- pure $ modelUriVersion sourceIdReadable
            case mversionOfSourceIdReadable of
              Nothing -> throwError $ error ("The domain declaration in the ARC file should be versioned. Found: " <> show sourceIdReadable)
              Just versionInArc ->
                if versionInArc /= version then throwError $ error ("The version in the domain declaration in the ARC file should match the version in the function argument. Found version " <> show versionInArc <> " but expected " <> show version)
                else if testModelName (unversionedModelUri sourceIdReadable) then do
                  unversionedCtxt <- pure $ ContextE rec { id = unversionedModelUri sourceIdReadable }
                  (Tuple result state :: Tuple (Either MultiplePerspectivesErrors (DomeinFile Readable)) PhaseTwoState) <-
                    lift $ lift $ runPhaseTwo_' (traverseDomain unversionedCtxt) defaultDomeinFileRecord empty empty Nil
                  case result of
                    Left e -> pure $ Left e
                    Right (DomeinFile dr'@{ id }) -> do
                      dr''@{ referredModels } <- pure dr' { referredModels = (delete id state.referredModels) }
                      -- We should load referred models if they are missing (but not the model we're compiling!).
                      -- Throw an error if a referred model is not installed. It will show up in the arc feedback.
                      installedModelCuids <- lift $ getinstalledModelCuids fromLocalModels
                      for_ referredModels (lift <<< (toStable installedModelCuids >=> retrieveDomeinFile))

                      (x' :: Either MultiplePerspectivesErrors (Tuple (DomeinFileRecord Readable) StoredQueries)) <-
                        lift $ phaseThree dr'' state.postponedStateQualifiedParts state.screens
                      case x' of
                        Left e -> pure $ Left e
                        Right (Tuple correctedDFR invertedQueries) -> do
                          -- Compute updated StableIdMapping (with cuids and individuals) in one call
                          mapping2 <- UTN.updateStableMappingForModel dfid modelCuid correctedDFR mMapping

                          -- Run the type checker (NOTE: but a stub, right now).
                          typeCheckErrors <- lift $ checkDomeinFile (DomeinFile correctedDFR)
                          if null typeCheckErrors then do
                            -- Add the source and _id.
                            df <- pure $ DomeinFile correctedDFR
                              { arc = text
                              -- Notice that this is the UNVERSIONED id. It will be overwritten with the versioned id when uploading to the repository.
                              , _id = takeGuid $ unwrap id
                              }
                            -- Now replace the readable name given by the modeller with a cuid, in FQNs:
                            normalizedDf <- lift $ normalizeTypes df mapping2

                            if saveInCache then void $ lift $ storeDomeinFileInCache (toStableModelUri id) normalizedDf else pure unit

                            normalizedInvertedQueries <- lift $ normalizeInvertedQueries df mapping2 invertedQueries

                            pure $ Right $ Tuple normalizedDf (Tuple normalizedInvertedQueries mapping2)
                          else
                            pure $ Left typeCheckErrors
                else
                  pure $ Left [ (DomeinFileIdIncompatible modelUriReadable sourceIdReadable pos) ]
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
