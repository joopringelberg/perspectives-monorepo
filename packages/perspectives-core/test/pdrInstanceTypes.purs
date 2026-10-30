module Test.PDRInstance.Types where

import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Perspectives.CoreTypes (MonadPerspectives, PerspectivesState)
import Perspectives.Logging (traceTest)
import Perspectives.RunPerspectives (runPerspectivesWithState)
import Prelude

-----------------------------------------------------------
-- PDR INSTANCE
-----------------------------------------------------------

-- | A running PDR installation with its state AVar and a shutdown action.
-- | Use `runInPDR` / `runTransactionInPDR` to interact with the PDR, and
-- | call `shutdown` (or use `withPDR` / `withTwoPDRs`) to clean up.
type PDRInstance =
  { stateAVar :: AVar PerspectivesState
  -- | Kill all background fibers for this instance.
  , shutdown :: Aff Unit
  , name :: String
  }

-- | Run a `MonadPerspectives` action against a PDR instance.
runInPDR :: forall a. PDRInstance -> MonadPerspectives a -> Aff a
runInPDR pdr mp = do
  runPerspectivesWithState
    ( do
        traceTest $ "Running in PDR instance: " <> pdr.name
        mp 
    )
    pdr.stateAVar
