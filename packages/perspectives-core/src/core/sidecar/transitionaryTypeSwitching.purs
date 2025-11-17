module Perspectives.Sidecar.TransitionaryTypeSwitching where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State (StateT, get, runStateT)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect.AVar (AVar)
import Effect.Aff (Aff, error, forkAff, try)
import Effect.Aff.AVar (empty, put, take, read)
import Effect.Class.Console (log)
import Foreign.Object (lookup)
import Foreign.Object as Object
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesState, TypeFix(..), TypeKind(..), TypeToBeMapped(..))
import Perspectives.Extern.Couchdb (computeVersionedAndUnversiondName)
import Perspectives.Identifiers (modelUri2LocalName, modelUri2ModelUrl, typeUri2ModelUri)
import Perspectives.ModelDependencies.ReadableStableMappings (modelReadableToCuid, modelReadableToStable)
import Perspectives.Representation.Class.Cacheable (EnumeratedRoleType(..))
import Perspectives.Representation.Class.PersistentType (toTypeToBeMapped)
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), StateIdentifier(..), ViewType(..))
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Readable)
import Perspectives.Sidecar.NormalizeTypeNames (Env, fqn2tid)
import Perspectives.Sidecar.StableIdMapping (fromLocalModels, loadStableMapping, loadStableMapping_)

-- This module contains functions to switch types on the fly.
-- We may only need it during the transition from Readable to Stable types,
-- probably just to be able to make a first installation with Stable types.
-- But maybe it is neccesary during model compilation as well.

mapReadableToStable :: TypeToBeMapped -> StateT Env MonadPerspectives TypeToBeMapped
mapReadableToStable ttbm@(TypeToBeMapped { kind, fqn }) =
  case typeUri2ModelUri fqn of
    -- Onderzoek of deze test naar getPerspectType kan.
    Just modelUri -> case lookup (unsafePartial modelUri2LocalName modelUri) modelReadableToCuid of
      Just _ -> do
        ensureMapping (ModelUri modelUri)
        env <- get
        case kind of
          KContext -> do
            -- All kinds share the same mapping logic
            tid <- pure $ unwrap $ runReaderT (fqn2tid (ContextType fqn)) env
            pure (toTypeToBeMapped tid)
          KEnumeratedRole -> do
            tid <- pure $ unwrap $ runReaderT (fqn2tid (EnumeratedRoleType fqn)) env
            pure (toTypeToBeMapped tid)
          KCalculatedRole -> do
            tid <- pure $ unwrap $ runReaderT (fqn2tid (CalculatedRoleType fqn)) env
            pure (toTypeToBeMapped tid)
          KEnumeratedProperty -> do
            tid <- pure $ unwrap $ runReaderT (fqn2tid (EnumeratedPropertyType fqn)) env
            pure (toTypeToBeMapped tid)
          KCalculatedProperty -> do
            tid <- pure $ unwrap $ runReaderT (fqn2tid (CalculatedPropertyType fqn)) env
            pure (toTypeToBeMapped tid)
          KView -> do
            tid <- pure $ unwrap $ runReaderT (fqn2tid (ViewType fqn)) env
            pure (toTypeToBeMapped tid)
          KState -> do
            tid <- pure $ unwrap $ runReaderT (fqn2tid (StateIdentifier fqn)) env
            pure (toTypeToBeMapped tid)

      Nothing -> pure ttbm
    Nothing -> pure ttbm
  where
  ensureMapping :: ModelUri Readable -> StateT Env MonadPerspectives Unit
  ensureMapping mu = do
    sidecars <- gets _.sidecars
    case Map.lookup mu sidecars of
      Just _ -> pure unit
      Nothing -> do
        case lookup (unwrap mu) modelReadableToStable of
          Just stableFqn -> do
            { versionedModelName } <- lift $ computeVersionedAndUnversiondName (ModelUri stableFqn)
            { repositoryUrl, documentName } <- pure $ unsafePartial modelUri2ModelUrl versionedModelName
            mmapping <- lift $ loadStableMapping_ repositoryUrl documentName
            case mmapping of
              Just mapping -> do
                log ("Loaded StableIdMapping for model " <> unwrap mu <> " from repository.")
                void $ modify \env -> env { sidecars = Map.insert mu mapping sidecars }
              Nothing -> do
                mlocalMapping <- lift $ loadStableMapping (ModelUri $ stableFqn) fromLocalModels
                case mlocalMapping of
                  Just mapping -> do
                    log ("Loaded StableIdMapping for model " <> unwrap mu <> " from local models.")
                    void $ modify \env -> env { sidecars = Map.insert mu mapping sidecars }
                  Nothing -> throwError (error ("Could not load StableIdMapping for model " <> unwrap mu))
          Nothing -> throwError (error ("No stable mapping found for model " <> unwrap mu))

forkedTypeFixer :: AVar (TypeFix) -> AVar PerspectivesState -> Aff Unit
forkedTypeFixer typeToBeFixed state = do
  -- Create a local AVar to accumulate and share Env between concurrent fixing fibers.
  envVar <- empty
  put { sidecars: Map.empty, perspMap: Object.empty } envVar
  let
    mergeEnv :: Env -> Env -> Env
    mergeEnv cur new =
      { sidecars: Map.union cur.sidecars new.sidecars
      , perspMap: Object.union cur.perspMap new.perspMap
      }

    loop :: Aff Unit
    loop = do
      -- Take the next hotline out of the coordination AVar,
      -- unblocking it for the next requester.
      requestWithHotline <- take typeToBeFixed
      -- Take the fixing request out of the hotline, so the requester can wait for the result.
      case requestWithHotline of
        (TypeFixingHotline hotline) -> do
          fixingRequest <- take hotline
          case fixingRequest of
            -- This is the only expected case.
            Fix tb@(TypeToBeMapped { fqn }) -> void $ forkAff do
              -- Fork a fiber to do the actual fixing without blocking the coordinator loop.
              -- Read a snapshot of the current env for heavy work.
              envSnap <- read envVar
              -- log ("fixer: mapping start for fqn=" <> fqn)
              result <- try (runPerspectivesWithState (runStateT (mapReadableToStable tb) envSnap) state)
              case result of
                Left e -> do
                  -- Notify the requester of the error via the hotline.
                  log ("fixer: mapping error for fqn=" <> fqn <> ": " <> show e)
                  put (TypeFixingError (show e)) hotline
                Right (Tuple mapped env') -> do
                  -- Return the Fixed value to the requester.
                  -- log ("On the fly type mapping completed for fqn=" <> fqn)
                  put (Fixed mapped) hotline
                  -- Merge new env into shared envVar atomically.
                  cur <- take envVar
                  put (mergeEnv cur env') envVar
            -- log "fixer: env merged"
            _ -> put (TypeFixingError "Invalid state in coordination AVar.") hotline
        _ -> throwError (error "Invalid request in coordination AVar.")
      loop
  loop