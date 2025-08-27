-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
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
import Perspectives.Parsing.Arc.PhaseThree (phaseThreeWithMapping)
import Perspectives.Sidecar.UniqueTypeNames (applyStableIdMappingWith, extractKeysFromDfr) 
import Perspectives.Sidecar.UniqueTypeNames as UTN
import Perspectives.Sidecar.StableIdMapping (StableIdMapping, emptyStableIdMapping)
import Perspectives.Cuid2 (cuid2)
import Foreign.Object as OBJ
import Effect.Class (liftEffect)
import Data.Traversable (for)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseTwoState, runPhaseTwo_')
import Perspectives.Parsing.Messages (MultiplePerspectivesErrors, PerspectivesError(..))
import Perspectives.Representation.TypeIdentifiers (DomeinFileId(..))
import Perspectives.ResourceIdentifiers (takeGuid)
import Prelude (bind, discard, pure, show, ($), (<<<), (==), (<>))

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
loadAndCompileArcFile_ :: DomeinFileId -> Source -> Boolean -> MonadPerspectivesTransaction (Either (Array PerspectivesError) (Tuple DomeinFile StoredQueries))
loadAndCompileArcFile_ dfid text saveInCache = do
  x <- loadAndCompileArcFileWithSidecar_ dfid text saveInCache Nothing
  pure case x of
    Left errs -> Left errs
    Right (Tuple df (Tuple iqs _m)) -> Right (Tuple df iqs)

-- New: sidecar-aware API that returns the updated mapping with results.
loadAndCompileArcFileWithSidecar_ :: DomeinFileId -> Source -> Boolean -> Maybe StableIdMapping -> MonadPerspectivesTransaction (Either (Array PerspectivesError) (Tuple DomeinFile (Tuple StoredQueries StableIdMapping)))
loadAndCompileArcFileWithSidecar_ dfid@(DomeinFileId dfName) text saveInCache mMapping =
  catchError
    (do
      (r :: Either ParseError ContextE) <- lift $ lift $ runIndentParser text domain
      case r of
        Left e -> pure $ Left [parseError2PerspectivesError e]
        Right ctxt@(ContextE { id: sourceDfid, pos }) ->
          if sourceDfid == dfName then do
            (Tuple result state :: Tuple (Either MultiplePerspectivesErrors DomeinFile) PhaseTwoState) <-
              lift $ lift $ runPhaseTwo_' (traverseDomain ctxt) defaultDomeinFileRecord empty empty Nil
            case result of
              Left e -> pure $ Left e
              Right (DomeinFile dr'@{ id }) -> do
                dr''@{ referredModels } <- pure dr' { referredModels = state.referredModels }
                -- We should load referred models if they are missing (but not the model we're compiling!).
                for_ (delete id state.referredModels) (lift <<< retrieveDomeinFile)

                (x' :: Either MultiplePerspectivesErrors (Tuple DomeinFileRecord StoredQueries)) <-
                  lift $ phaseThreeWithMapping dr'' state.postponedStateQualifiedParts state.screens mMapping
                case x' of
                  Left e -> pure $ Left e
                  Right (Tuple correctedDFR@{ referredModels: refModels } invertedQueries) -> do
                    -- Base mapping from caller
                    let mapping0 = case mMapping of
                          Nothing -> emptyStableIdMapping
                          Just m0 -> m0

                    -- Extend aliases and compute current key snapshots
                    let cur = extractKeysFromDfr correctedDFR
                    let planned = UTN.planCuidAssignments cur mapping0

                    -- Mint new CUIDs for canonicals that need one (author-local, effectful)
                    ctxPairs <- for planned.needCuids.contexts \fqn -> do
                      v <- liftEffect (cuid2 (dfName <> ":ctx"))
                      pure (Tuple fqn v)
                    rolPairs <- for planned.needCuids.roles \fqn -> do
                      v <- liftEffect (cuid2 (dfName <> ":rol"))
                      pure (Tuple fqn v)
                    propPairs <- for planned.needCuids.properties \fqn -> do
                      v <- liftEffect (cuid2 (dfName <> ":prop"))
                      pure (Tuple fqn v)

                    let newCuids =
                          { contexts: OBJ.fromFoldable ctxPairs
                          , roles: OBJ.fromFoldable rolPairs
                          , properties: OBJ.fromFoldable propPairs
                          }

                    let mapping1 = UTN.finalizeCuidAssignments planned.mappingWithAliases newCuids

                    -- Apply aliases (no-op for canonical keys) to ensure semantic consistency
                    let _ = applyStableIdMappingWith mapping1 correctedDFR

                    -- Run the type checker
                    typeCheckErrors <- lift $ checkDomeinFile (DomeinFile correctedDFR)
                    if null typeCheckErrors then do
                      -- Remove the self-referral and add the source.
                      df <- pure $ DomeinFile correctedDFR
                        { referredModels = delete id refModels
                        , arc = text
                        , _id = takeGuid $ unwrap id
                        }
                      if saveInCache then void $ lift $ storeDomeinFileInCache id df else pure unit
                      pure $ Right $ Tuple df (Tuple invertedQueries mapping1)
                    else
                      pure $ Left typeCheckErrors
          else
            pure $ Left [ (DomeinFileIdIncompatible dfid (DomeinFileId sourceDfid) pos) ]
    )
    (\e -> pure $ Left [ Custom (show e) ])

type Persister = String -> DomeinFile -> MonadPerspectives (Array PerspectivesError)

type ArcSource = String
type CrlSource = String

parseError2PerspectivesError :: ParseError -> PerspectivesError
parseError2PerspectivesError (ParseError message pos) = ParserError message (position2ArcPosition pos)
