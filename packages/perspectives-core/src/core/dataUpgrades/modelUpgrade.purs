module Perspectives.ModelUpgrade where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (liftAff)
import Foreign (unsafeToForeign)
import IDBKeyVal (idbGet, idbSet)
import Perspectives.CoreTypes (MonadPerspectives, (##=))
import Perspectives.Extern.Couchdb (roleInstancesFromCouchdb)
import Perspectives.Extern.Utilities (isLowerVersion)
import Perspectives.ModelDependencies (modelDataUpgrade, sysUser)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), RoleType(..))
import Perspectives.RoleStateCompiler (evaluateRootRoleState)
import Perspectives.RunMonadPerspectivesTransaction (runMonadPerspectivesTransaction)
import Unsafe.Coerce (unsafeCoerce)

runModelUpgrades :: MonadPerspectives Unit
runModelUpgrades = do
  -- Get the current PDR version
  mcurrentVersion <- liftAff $ idbGet "CurrentPDRVersion"
  (installedVersion :: String) <- case mcurrentVersion of
    Just installedVersion -> pure (unsafeCoerce installedVersion)
    Nothing -> do
      -- This mechanism was introduced during development of version 0.25.
      -- Installations existing prior to 0.25 will be brought to heel with these instructions.
      liftAff $ idbSet "CurrentPDRVersion" (unsafeToForeign "0.24")
      pure "0.24"
  -- Get the last PDR version we did a model upgrade for
  mlastModelUpgradeVersion <- liftAff $ idbGet "LastModelUpgradeVersion"
  lastModelUpgradeVersion <- case mlastModelUpgradeVersion of
    Just version -> pure (unsafeCoerce version)
    Nothing -> do
      -- This mechanism was introduced during development of version 3.0.68.
      -- Installations existing prior to 3.0.68 will be brought to heel with these instructions.
      liftAff $ idbSet "LastModelUpgradeVersion" (unsafeToForeign "3.0.68")
      pure "3.0.68"
  if isLowerVersion lastModelUpgradeVersion installedVersion then do
    -- Find all roles with type sys:Upgrade.
    -- Evaluate their state in a Transaction.
    modelUpgradeRoles <- (ContextInstance "ignored") ##= roleInstancesFromCouchdb [ modelDataUpgrade ]
    runMonadPerspectivesTransaction (ENR $ EnumeratedRoleType sysUser) do
      for_ modelUpgradeRoles evaluateRootRoleState
    -- After running the necessary upgrades, we update the LastModelUpgradeVersion
    -- to the current installedVersion.
    liftAff $ idbSet "LastModelUpgradeVersion" (unsafeToForeign installedVersion)
  else
    pure unit