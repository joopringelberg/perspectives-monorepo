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
import Data.Array (delete, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import Foreign.Object (empty)
import Parsing (ParseError(..))
import Perspectives.Checking.PerspectivesTypeChecker (checkDomeinFile)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesTransaction)
import Perspectives.DomeinCache (retrieveDomeinFile, storeDomeinFileInCache)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, defaultDomeinFileRecord)
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
import Perspectives.Sidecar.StableIdMapping (ContextUri(..), ModelUri(..), Stable, StableIdMapping, fromRepository, idUriForContext, loadStableMapping)
import Perspectives.Sidecar.UniqueTypeNames as UTN
import Prelude (bind, discard, pure, show, ($), (<<<), (==), (>=>))

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
  mMapping <- lift $ loadStableMapping dfid fromRepository
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
                  dr''@{ referredModels } <- pure dr' { referredModels = (delete id state.referredModels) }
                  -- We should load referred models if they are missing (but not the model we're compiling!).
                  -- Throw an error if a referred model is not installed. It will show up in the arc feedback.
                  installedModelCuids <- lift $ getinstalledModelCuids false -- unversioned.
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

                        pure $ Right $ Tuple normalizedDf (Tuple invertedQueries mapping2)
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
