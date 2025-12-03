module Perspectives.Sidecar.ToStable where

import Prelude

import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap as EM
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (typeUri2ModelUri_)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.PerspectivesState (getModelUris)
import Perspectives.Representation.Class.Cacheable (ViewType(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType(..), CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), StateIdentifier(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..), Stable, Readable)

-- Also see module Perspectives.HumanReadableType, where we convert from Stable to Readable based on the displayName property of the type.

lookupStableModelUri :: ModelUri Readable -> MonadPerspectives (ModelUri Stable)
lookupStableModelUri muReadable = do
  muMap <- getModelUris
  case lookup muReadable muMap of
    Just muStable -> pure muStable
    Nothing -> pure (ModelUri $ unwrap muReadable)

class ToStable i where
  toStable :: i -> MonadPerspectives i

instance ToStable ContextType where
  toStable ct@(ContextType stableId) = lookupStableModelUri (ModelUri $ unsafePartial typeUri2ModelUri_ stableId) >>= retrieveDomeinFile >>= \(DomeinFile { toStableContextType }) -> case EM.lookup ct toStableContextType of
    Just stableCt -> pure stableCt
    Nothing -> do
      logPerspectivesError (Custom $ "Failed to convert ContextType from readable to stable: no mapping found for readable id " <> stableId)
      pure ct

instance ToStable EnumeratedRoleType where
  toStable er@(EnumeratedRoleType stableId) = lookupStableModelUri (ModelUri $ unsafePartial typeUri2ModelUri_ stableId) >>= retrieveDomeinFile >>= \(DomeinFile { toStableEnumeratedRoleType }) -> case EM.lookup er toStableEnumeratedRoleType of
    Just stableEr -> pure stableEr
    Nothing -> do
      logPerspectivesError (Custom $ "Failed to convert EnumeratedRoleType from readable to stable: no mapping found for readable id " <> stableId)
      pure er

instance ToStable CalculatedRoleType where
  toStable er@(CalculatedRoleType stableId) = lookupStableModelUri (ModelUri $ unsafePartial typeUri2ModelUri_ stableId) >>= retrieveDomeinFile >>= \(DomeinFile { toStableCalculatedRoleType }) -> case EM.lookup er toStableCalculatedRoleType of
    Just stableEr -> pure stableEr
    Nothing -> do
      logPerspectivesError (Custom $ "Failed to convert CalculatedRoleType from readable to stable: no mapping found for readable id " <> stableId)
      pure er

instance ToStable EnumeratedPropertyType where
  toStable er@(EnumeratedPropertyType stableId) = lookupStableModelUri (ModelUri $ unsafePartial typeUri2ModelUri_ stableId) >>= retrieveDomeinFile >>= \(DomeinFile { toStableEnumeratedPropertyType }) -> case EM.lookup er toStableEnumeratedPropertyType of
    Just stableEr -> pure stableEr
    Nothing -> do
      logPerspectivesError (Custom $ "Failed to convert EnumeratedPropertyType from readable to stable: no mapping found for readable id " <> stableId)
      pure er

instance ToStable CalculatedPropertyType where
  toStable er@(CalculatedPropertyType stableId) = lookupStableModelUri (ModelUri $ unsafePartial typeUri2ModelUri_ stableId) >>= retrieveDomeinFile >>= \(DomeinFile { toStableCalculatedPropertyType }) -> case EM.lookup er toStableCalculatedPropertyType of
    Just stableEr -> pure stableEr
    Nothing -> do
      logPerspectivesError (Custom $ "Failed to convert CalculatedPropertyType from readable to stable: no mapping found for readable id " <> stableId)
      pure er

instance ToStable StateIdentifier where
  toStable st@(StateIdentifier stableId) = lookupStableModelUri (ModelUri $ unsafePartial typeUri2ModelUri_ stableId) >>= retrieveDomeinFile >>= \(DomeinFile { toStableStateIdentifier }) -> case EM.lookup st toStableStateIdentifier of
    Just stableSt -> pure stableSt
    Nothing -> do
      logPerspectivesError (Custom $ "Failed to convert StateIdentifier from readable to stable: no mapping found for readable id " <> stableId)
      pure st

instance ToStable ViewType where
  toStable vt@(ViewType stableId) = lookupStableModelUri (ModelUri $ unsafePartial typeUri2ModelUri_ stableId) >>= retrieveDomeinFile >>= \(DomeinFile { toStableViewType }) -> case EM.lookup vt toStableViewType of
    Just stableVt -> pure stableVt
    Nothing -> do
      logPerspectivesError (Custom $ "Failed to convert ViewType from readable to stable: no mapping found for readable id " <> stableId)
      pure vt