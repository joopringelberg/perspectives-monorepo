module Perspectives.Sidecar.ToReadable where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Data.EncodableMap as EM
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Identifiers (typeUri2ModelUri_)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Persistent (getDomeinFile)
import Perspectives.Query.QueryTypes (Domain(..), RoleInContext(..))
import Perspectives.Representation.ADT (ADT)
import Perspectives.Representation.CalculatedProperty (CalculatedProperty(..))
import Perspectives.Representation.CalculatedRole (CalculatedRole(..))
import Perspectives.Representation.Class.Cacheable (ViewType)
import Perspectives.Representation.Class.PersistentType (tryGetPerspectType)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.State (State(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedPropertyType, CalculatedRoleType, ContextType, EnumeratedPropertyType, EnumeratedRoleType, IndexedContext(..), PropertyType(..), RoleType(..), StateIdentifier)
import Perspectives.Representation.View (View(..))
import Perspectives.SideCar.PhantomTypedNewtypes (ModelUri(..))

-- Also see module Perspectives.HumanReadableType, where we convert from Stable to Readable based on the displayName property of the type.

class ToReadable a where
  toReadable :: a -> MonadPerspectives a

instance ToReadable IndexedContext where
  toReadable ic@(IndexedContext stableId) = do
    getDomeinFile (ModelUri $ unsafePartial typeUri2ModelUri_ stableId) >>= \(DomeinFile { toReadableContextIndividuals }) -> case EM.lookup ic toReadableContextIndividuals of
      Just readableId -> pure readableId
      Nothing -> do
        logPerspectivesError (Custom $ "Failed to convert IndexedContext from stable to readable: no mapping found for stable id " <> stableId)
        pure (IndexedContext stableId)

instance ToReadable ContextType where
  toReadable ct = tryGetPerspectType ct >>= maybe (pure ct) \(Context { readableName }) -> pure readableName

instance ToReadable EnumeratedRoleType where
  toReadable er = tryGetPerspectType er >>= maybe (pure er) \(EnumeratedRole { readableName }) -> pure readableName

instance ToReadable CalculatedRoleType where
  toReadable er = tryGetPerspectType er >>= maybe (pure er) \(CalculatedRole { readableName }) -> pure readableName

instance ToReadable RoleType where
  toReadable (ENR r) = ENR <$> toReadable r
  toReadable (CR r) = CR <$> toReadable r

-- Convert a PropertyType that carries a stable identifier into its readable FQN variant
instance ToReadable PropertyType where
  toReadable (ENP p) = ENP <$> toReadable p
  toReadable (CP p) = CP <$> toReadable p

instance ToReadable EnumeratedPropertyType where
  toReadable er = tryGetPerspectType er >>= maybe (pure er) \(EnumeratedProperty { readableName }) -> pure readableName

instance ToReadable CalculatedPropertyType where
  toReadable er = tryGetPerspectType er >>= maybe (pure er) \(CalculatedProperty { readableName }) -> pure readableName

instance ToReadable StateIdentifier where
  toReadable sid = tryGetPerspectType sid >>= maybe (pure sid) \(State { readableName }) -> pure readableName

instance ToReadable RoleInContext where
  toReadable (RoleInContext { role, context }) = do
    role' <- toReadable role
    context' <- toReadable context
    pure $ RoleInContext { role: role', context: context' }

instance ToReadable (ADT RoleInContext) where
  toReadable adtRoleInContext = traverse toReadable adtRoleInContext

instance ToReadable (ADT ContextType) where
  toReadable adtContextType = traverse toReadable adtContextType

instance ToReadable Domain where
  toReadable (RDOM adt) = RDOM <$> toReadable adt
  toReadable (CDOM adt) = CDOM <$> toReadable adt
  toReadable (VDOM r mproptype) = do
    mproptype' <- case mproptype of
      Just proptype -> Just <$> toReadable proptype
      Nothing -> pure Nothing
    pure $ VDOM r mproptype'
  toReadable x = pure x

instance ToReadable ViewType where
  toReadable vt = tryGetPerspectType vt >>= maybe (pure vt) \(View { readableName }) -> pure readableName