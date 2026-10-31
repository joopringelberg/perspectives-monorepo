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

-- | The PDR doubles as a Language Server (LSP) for ARC.
-- | Messages arrive through the API (request type `Lsp`, the JSON-RPC message as `subject`)
-- | and every call returns the (possibly empty) list of JSON-RPC messages the server emits in reaction:
-- | a response for requests, and notifications such as `textDocument/publishDiagnostics`.
-- |
-- | Analysis is sterile: the source is parsed and compiled (phases one to three plus the type checker),
-- | but nothing is normalised to stable identifiers, cached or stored.
-- | Synchronisation is full-document only (no incremental parsing yet). Results of an analysis
-- | of a document version that has been superseded in the meantime are discarded.
module Perspectives.Lsp.Server
  ( handleLspMessage
  , modelSourceScheme
  ) where

import Prelude

import Control.Monad.Error.Class (catchError)
import Data.Array (catMaybes, concat, delete, filter, last, null, (!!))
import Data.Either (Either(..), hush)
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (over, unwrap)
import Data.Nullable (Nullable, notNull, null) as N
import Data.String (Pattern(..), split, stripPrefix)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, modify_, new)
import Effect.Ref (read) as Ref
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Foreign.Object (Object, empty, insert, toUnfoldable)
import Foreign.Object (delete, lookup) as OBJ
import Perspectives.Checking.PerspectivesTypeChecker (checkDomeinFile)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap (EncodableMap)
import Perspectives.Data.EncodableMap (lookup) as EM
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..), DomeinFileRecord, defaultDomeinFileRecord)
import Perspectives.Identifiers (modelUriVersion, typeUri2ModelUri, unversionedModelUri)
import Perspectives.Lsp.Diagnostics (Diagnostic, LspRange, error2Diagnostic, warning2Diagnostic)
import Perspectives.Lsp.Resolve (Definition, Resolution(..), enclosingScope, nameAt, resolve, selectionRange)
import Perspectives.Parsing.Arc (domain)
import Perspectives.Parsing.Arc.AST (ContextE(..))
import Perspectives.Parsing.Arc.IndentParser (runIndentParser)
import Perspectives.Parsing.Arc.PhaseThree (phaseThree)
import Perspectives.Parsing.Arc.PhaseTwo (traverseDomain)
import Perspectives.Parsing.Arc.PhaseTwoDefs (runPhaseTwo_', toStableModelUri)
import Perspectives.Parsing.Arc.Position (ArcPosition(..))
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.PerspectivesState (getWarnings, resetWarnings, setWarnings)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), ViewType(..))
import Perspectives.SideCar.PhantomTypedNewtypes (Readable)
import Perspectives.Sidecar.NormalizeTypeNames (StableIdMappingForModel, getinstalledModelCuids)
import Perspectives.Sidecar.StableIdMapping (ModelUri(..), Stable, fromLocalModels)
import Perspectives.TypePersistence.LoadArc (normalizeTrailingWhitespace, parseError2PerspectivesError)
import Simple.JSON (class ReadForeign, class WriteForeign, read, readJSON_, writeJSON)

-----------------------------------------------------------
-- DOCUMENT STORE
-----------------------------------------------------------
type Analysis = { definitions :: Array Definition, namespaces :: Object String }

type Document = { text :: String, version :: Int, analysis :: Maybe Analysis }

-- | Open documents, by URI. There is one PDR per worker, so a module-level store suffices.
documents :: Ref (Object Document)
documents = unsafePerformEffect (new empty)

-- | Uri scheme for the (read-only) sources of installed models, e.g. `perspectives-model:model://perspectives.domains#System`.
modelSourceScheme :: String
modelSourceScheme = "perspectives-model:"

-----------------------------------------------------------
-- JSON-RPC
-----------------------------------------------------------
type Incoming = { method :: Maybe String, id :: Maybe Foreign, params :: Maybe Foreign }

type Position = { line :: Int, character :: Int }
type Location = { uri :: String, range :: LspRange }

respond :: forall a. WriteForeign a => Foreign -> a -> String
respond id result = writeJSON { jsonrpc: "2.0", id, result }

respondError :: Foreign -> Int -> String -> String
respondError id code message = writeJSON { jsonrpc: "2.0", id, error: { code, message } }

publishDiagnostics :: String -> Int -> Array Diagnostic -> String
publishDiagnostics uri version diagnostics = writeJSON
  { jsonrpc: "2.0", method: "textDocument/publishDiagnostics", params: { uri, version, diagnostics } }

params' :: forall a. ReadForeign a => Maybe Foreign -> Maybe a
params' mp = mp >>= hush <<< read

-- | Handle a single JSON-RPC message. Returns the messages to send back to the client.
handleLspMessage :: String -> MonadPerspectives (Array String)
handleLspMessage message = case readJSON_ message :: Maybe Incoming of
  Nothing -> pure []
  Just { method, id: mid, params } -> case method, mid of
    Just "initialize", Just id -> pure
      [ respond id
          { capabilities: { textDocumentSync: 1, definitionProvider: true }
          , serverInfo: { name: "perspectives-pdr" }
          }
      ]
    Just "shutdown", Just id -> pure [ respond id (N.null :: N.Nullable Int) ]
    Just "textDocument/didOpen", _ -> case params' params of
      Just ({ textDocument: { uri, text, version } } :: { textDocument :: { uri :: String, text :: String, version :: Int } }) ->
        updateAndAnalyse uri text version
      Nothing -> pure []
    Just "textDocument/didChange", _ -> case params' params of
      Just ({ textDocument: { uri, version }, contentChanges } :: { textDocument :: { uri :: String, version :: Int }, contentChanges :: Array { text :: String } }) ->
        case last contentChanges of
          Just { text } -> updateAndAnalyse uri text version
          Nothing -> pure []
      Nothing -> pure []
    Just "textDocument/didClose", _ -> case params' params of
      Just ({ textDocument: { uri } } :: { textDocument :: { uri :: String } }) -> do
        liftEffect $ modify_ (OBJ.delete uri) documents
        pure [ writeJSON { jsonrpc: "2.0", method: "textDocument/publishDiagnostics", params: { uri, diagnostics: ([] :: Array Diagnostic) } } ]
      Nothing -> pure []
    Just "textDocument/definition", Just id -> case params' params of
      Just ({ textDocument: { uri }, position } :: { textDocument :: { uri :: String }, position :: Position }) -> do
        locations <- catchError (definition uri position) (\_ -> pure [])
        pure [ respond id (if null locations then N.null else N.notNull locations) ]
      Nothing -> pure [ respondError id (-32602) "Invalid params" ]
    Just "perspectives/modelSource", Just id -> case params' params of
      Just ({ uri } :: { uri :: String }) -> do
        msrc <- catchError (modelSource uri) (\_ -> pure Nothing)
        pure [ respond id (maybe N.null (\text -> N.notNull { text }) msrc) ]
      Nothing -> pure [ respondError id (-32602) "Invalid params" ]
    -- Other requests are not supported.
    Just m, Just id -> pure [ respondError id (-32601) ("Method not found: " <> m) ]
    -- Other notifications (initialized, exit, $/cancelRequest, ...) are ignored.
    _, _ -> pure []

-----------------------------------------------------------
-- DIAGNOSTICS
-----------------------------------------------------------
-- | Store the new text, analyse it and publish diagnostics,
-- | unless a newer version of the document arrived while we were analysing.
updateAndAnalyse :: String -> String -> Int -> MonadPerspectives (Array String)
updateAndAnalyse uri text version = do
  liftEffect $ modify_ (\docs -> insert uri { text, version, analysis: _.analysis =<< OBJ.lookup uri docs } docs) documents
  { errors, warnings, analysis } <- analyseWithWarnings text
  docs <- liftEffect $ Ref.read documents
  case OBJ.lookup uri docs of
    Just doc | doc.version == version -> do
      liftEffect $ modify_ (insert uri doc { analysis = maybe doc.analysis Just analysis }) documents
      pure [ publishDiagnostics uri version ((error2Diagnostic <$> errors) <> (warning2Diagnostic <$> warnings)) ]
    _ -> pure []

-- | Compiler warnings are collected in PerspectivesState. We take the ones produced by this analysis
-- | and restore the previous warnings, so analysing does not pollute the state.
analyseWithWarnings :: String -> MonadPerspectives { errors :: Array PerspectivesError, warnings :: Array String, analysis :: Maybe Analysis }
analyseWithWarnings text = do
  previous <- getWarnings
  resetWarnings
  { errors, analysis } <- analyse text
  warnings <- getWarnings
  setWarnings previous
  pure { errors, warnings: _.message <$> warnings, analysis }

-- | A sterile version of the compilation pipeline in Perspectives.TypePersistence.LoadArc:
-- | no stable identifiers, no caching, no storage.
analyse :: String -> MonadPerspectives { errors :: Array PerspectivesError, analysis :: Maybe Analysis }
analyse rawText = catchError analyse_ (\e -> pure { errors: [ Custom (show e) ], analysis: Nothing })
  where
  text :: String
  text = normalizeTrailingWhitespace rawText

  analyse_ :: MonadPerspectives { errors :: Array PerspectivesError, analysis :: Maybe Analysis }
  analyse_ = do
    r <- liftAff $ runIndentParser text domain
    case r of
      Left e -> pure { errors: [ parseError2PerspectivesError e ], analysis: Nothing }
      Right (ContextE rec@{ id: sourceIdReadable }) -> do
        let
          versionErrors = case modelUriVersion sourceIdReadable of
            Nothing -> [ Custom ("The domain declaration should be versioned (e.g. `domain model://example.org#Model@1.0`). Found: " <> sourceIdReadable) ]
            Just _ -> []
        Tuple result state <- liftAff $ runPhaseTwo_' (traverseDomain (ContextE rec { id = unversionedModelUri sourceIdReadable })) defaultDomeinFileRecord empty empty Nil
        case result of
          Left errs -> pure { errors: versionErrors <> errs, analysis: Just (mkAnalysis state.dfr state.namespaces) }
          Right (DomeinFile dr') -> do
            let
              dr'' = dr' { referredModels = delete dr'.id state.referredModels }
              phaseTwoAnalysis = Just (mkAnalysis dr'' state.namespaces)
            loadErrors <- catchError
              ( do
                  installedModelCuids <- getinstalledModelCuids fromLocalModels
                  for_ (over ModelUri unversionedModelUri <$> dr''.referredModels) (toStable installedModelCuids >=> retrieveDomeinFile)
                  pure []
              )
              (\e -> pure [ Custom ("A model this model depends on could not be loaded: " <> show e) ])
            if not (null loadErrors) then pure { errors: versionErrors <> loadErrors, analysis: phaseTwoAnalysis }
            else do
              x <- phaseThree dr'' state.postponedStateQualifiedParts state.screens
              case x of
                Left errs -> pure { errors: versionErrors <> errs, analysis: phaseTwoAnalysis }
                Right (Tuple correctedDFR _) -> do
                  typeCheckErrors <- checkDomeinFile (DomeinFile correctedDFR)
                  pure { errors: versionErrors <> typeCheckErrors, analysis: Just (mkAnalysis correctedDFR state.namespaces) }

-- | The model under analysis itself is not remapped, only its dependencies.
toStable :: StableIdMappingForModel -> ModelUri Readable -> MonadPerspectives (ModelUri Stable)
toStable m idfid@(ModelUri mUri) = case Map.lookup (ModelUri mUri) m of
  Nothing -> pure $ toStableModelUri idfid
  Just (ModelUri s) -> pure $ ModelUri s

-----------------------------------------------------------
-- DEFINITIONS
-----------------------------------------------------------
-- | All type definitions that have a source position. States have no position and are not included.
mkAnalysis :: forall f. DomeinFileRecord f -> Object String -> Analysis
mkAnalysis dfr namespaces =
  { definitions: filter (\d -> d.line >= 1) $ concat
      [ defs "context" (_.pos <<< unwrap) dfr.contexts
      , defs "enumeratedRole" (_.pos <<< unwrap) dfr.enumeratedRoles
      , defs "calculatedRole" (_.pos <<< unwrap) dfr.calculatedRoles
      , defs "enumeratedProperty" (_.pos <<< unwrap) dfr.enumeratedProperties
      , defs "calculatedProperty" (_.pos <<< unwrap) dfr.calculatedProperties
      , defs "view" (_.pos <<< unwrap) dfr.views
      ]
  , namespaces
  }
  where
  defs :: forall a. String -> (a -> ArcPosition) -> Object a -> Array Definition
  defs kind getPos obj = (\(Tuple name a) -> let ArcPosition { line, column } = getPos a in { name, kind, line, column }) <$> toUnfoldable obj

definition :: String -> Position -> MonadPerspectives (Array Location)
definition uri position = do
  docs <- liftEffect $ Ref.read documents
  case OBJ.lookup uri docs of
    Just { text, analysis: Just { definitions, namespaces } } -> do
      let
        lines = split (Pattern "\n") text
        lineText = fromMaybe "" (lines !! position.line)
      case nameAt lineText position.character of
        Nothing -> pure []
        Just name -> case resolve definitions namespaces (enclosingScope definitions lines (position.line + 1)) name of
          Local ds -> pure $ (\d -> { uri, range: selectionRange lines d }) <$> ds
          External qualifiedName -> externalDefinition qualifiedName
          Unresolved -> pure []
    _ -> pure []

-- | Look up a (readable) qualified type name in the installed model that defines it.
externalDefinition :: String -> MonadPerspectives (Array Location)
externalDefinition qualifiedName = case typeUri2ModelUri qualifiedName of
  Nothing -> pure []
  Just modelUri -> do
    DomeinFile df <- retrieveInstalledModel modelUri
    let
      stableName :: forall t. Ord t => String -> EncodableMap t t -> (String -> t) -> (t -> String) -> String
      stableName name toStableMap wrap unwrap' = maybe name unwrap' (EM.lookup (wrap name) toStableMap)

      find :: forall a. Object a -> (a -> ArcPosition) -> String -> Maybe ArcPosition
      find obj getPos s = getPos <$> OBJ.lookup s obj
      candidates = catMaybes
        [ find df.contexts (_.pos <<< unwrap) (stableName qualifiedName df.toStableContextType ContextType unwrap)
        , find df.enumeratedRoles (_.pos <<< unwrap) (stableName qualifiedName df.toStableEnumeratedRoleType EnumeratedRoleType unwrap)
        , find df.calculatedRoles (_.pos <<< unwrap) (stableName qualifiedName df.toStableCalculatedRoleType CalculatedRoleType unwrap)
        , find df.enumeratedProperties (_.pos <<< unwrap) (stableName qualifiedName df.toStableEnumeratedPropertyType EnumeratedPropertyType unwrap)
        , find df.calculatedProperties (_.pos <<< unwrap) (stableName qualifiedName df.toStableCalculatedPropertyType CalculatedPropertyType unwrap)
        , find df.views (_.pos <<< unwrap) (stableName qualifiedName df.toStableViewType ViewType unwrap)
        ]
      lines = split (Pattern "\n") df.arc
    pure $ (\(ArcPosition { line, column }) -> { uri: modelSourceScheme <> unversionedModelUri modelUri, range: selectionRange lines { name: qualifiedName, kind: "", line, column } }) <$> candidates

retrieveInstalledModel :: String -> MonadPerspectives (DomeinFile Stable)
retrieveInstalledModel modelUri = do
  installedModelCuids <- getinstalledModelCuids fromLocalModels
  toStable installedModelCuids (ModelUri (unversionedModelUri modelUri)) >>= retrieveDomeinFile

-- | The ARC source of an installed model, given a `perspectives-model:` uri.
modelSource :: String -> MonadPerspectives (Maybe String)
modelSource uri = case stripPrefix (Pattern modelSourceScheme) uri of
  Nothing -> pure Nothing
  Just modelUri -> do
    DomeinFile { arc } <- retrieveInstalledModel modelUri
    pure if arc == "" then Nothing else Just arc
