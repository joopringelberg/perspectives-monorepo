module Perspectives.Instances.CreateContext where

import Control.Monad.Except (ExceptT)
import Control.Monad.Writer (lift)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Foreign.Object (isEmpty)
import Perspectives.ApiTypes (PropertySerialization(..))
import Perspectives.Assignment.Update (getSubject, setProperty)
import Perspectives.Authenticate (signDelta)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectivesTransaction, (###=))
import Perspectives.Deltas (addCorrelationIdentifiersToTransactie, addCreatedRoleToTransaction)
import Perspectives.DependencyTracking.Dependency (findRoleRequests)
import Perspectives.Identifiers (buitenRol)
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Names (expandDefaultNamespaces)
import Perspectives.Parsing.Messages (PerspectivesError)
import Perspectives.Persistent (saveEntiteit)
import Perspectives.Representation.Class.Cacheable (ContextType(..), EnumeratedPropertyType(..), cacheEntity)
import Perspectives.Representation.Class.PersistentType (StateIdentifier(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (RoleType, externalRoleType)
import Perspectives.ResourceIdentifiers (takeGuid)
import Perspectives.StrippedDelta (stripResourceSchemes)
import Perspectives.Sync.SignedDelta (SignedDelta)
import Perspectives.Types.ObjectGetters (contextAspectsClosure, roleAspectsClosure)
import Perspectives.TypesForDeltas (ContextDelta(..), ContextDeltaType(..), UniverseContextDelta(..), UniverseContextDeltaType(..), UniverseRoleDelta(..), UniverseRoleDeltaType(..))
import Prelude (bind, discard, pure, unit, void, ($), (<$>), (<<<), (>>=))
import Simple.JSON (writeJSON)

-- | Constructs an empty context, caches it.
-- | The context contains a UniverseContextDelta, the external role contains a UniverseRoleDelta and ContextDelta.
-- | However, they have not yet been added to the Transaction. This is because we need to know the users
-- | we should sent these deltas to, and these are computed on constructing the roles of the context.
-- | So each caller of constructEmptyContext should add these three deltas to the Transaction.
-- | to the Transaction (and also a UniverseRoleDelta for the external role).
-- | QUERY UPDATES
-- | PERSISTENCE of the external role, but not of the context itself.
-- |
-- | For properties:
-- | SYNCHRONISATION by RolePropertyDelta.
-- | RULE TRIGGERING
-- | QUERY UPDATES
-- | Result record for constructEmptyContext, carrying the deltas that callers need for the transaction.
type ContextCreationResult =
  { context :: PerspectContext
  , universeContextDelta :: SignedDelta
  , externalUniverseRoleDelta :: SignedDelta
  , externalContextDelta :: SignedDelta
  }

constructEmptyContext :: ContextInstance -> String -> String -> PropertySerialization -> Maybe RoleType -> ExceptT PerspectivesError MonadPerspectivesTransaction ContextCreationResult
constructEmptyContext contextInstanceId ctype localName externeProperties authorizedRole = do
  externalRole <- pure $ RoleInstance $ buitenRol $ unwrap contextInstanceId
  pspType <- ContextType <$> (lift $ lift $ expandDefaultNamespaces ctype)
  allExternalRoleTypes <- lift $ lift (externalRoleType pspType ###= roleAspectsClosure)
  allContextTypes <- lift $ lift (pspType ###= contextAspectsClosure)
  subject <- lift $ getSubject
  delta <- lift $ signDelta
    ( writeJSON $ stripResourceSchemes $ UniverseContextDelta
        { id: contextInstanceId
        , contextType: pspType
        , deltaType: ConstructEmptyContext
        , subject
        , resourceKey: unwrap contextInstanceId
        , resourceVersion: 0
        }
    )
  contextInstance <- pure
    ( PerspectContext defaultContextRecord
        { _id = takeGuid $ unwrap contextInstanceId
        , id = contextInstanceId
        , displayName = localName
        , pspType = pspType
        , allTypes = allContextTypes
        , buitenRol = externalRole
        , states = [ StateIdentifier $ unwrap pspType ]
        }
    )
  lift $ lift $ void $ cacheEntity contextInstanceId contextInstance
  delta' <- lift $ signDelta
    ( writeJSON $ stripResourceSchemes $ UniverseRoleDelta
        { id: contextInstanceId
        , contextType: pspType
        , roleInstance: externalRole
        , roleType: externalRoleType pspType
        , authorizedRole
        , deltaType: ConstructExternalRole
        , subject
        , resourceKey: unwrap externalRole
        , resourceVersion: 0
        }
    )
  contextDelta <- lift $ signDelta
    ( writeJSON $ stripResourceSchemes $ ContextDelta
        { contextInstance: contextInstanceId
        , contextType: pspType
        , roleType: externalRoleType pspType
        , roleInstance: externalRole
        , destinationContext: Nothing
        , destinationContextType: Nothing
        , deltaType: AddExternalRole
        , subject
        , resourceKey: unwrap externalRole
        , resourceVersion: 0
        }
    )
  _ <- lift $ lift $ cacheEntity externalRole
    ( PerspectRol defaultRolRecord
        { _id = takeGuid $ unwrap externalRole
        , id = externalRole
        , pspType = externalRoleType pspType
        , allTypes = allExternalRoleTypes
        , context = contextInstanceId
        , binding = Nothing
        , states = [ StateIdentifier $ unwrap (externalRoleType pspType) ]
        }
    )
  lift $ addCreatedRoleToTransaction externalRole
  -- QUERY UPDATES
  (lift $ lift $ findRoleRequests (ContextInstance "def:AnyContext") (externalRoleType pspType)) >>= lift <<< addCorrelationIdentifiersToTransactie
  -- TODO. Op dit moment van constructie aangekomen is nog niet vastgelegd wie 'me' is in de context.
  case externeProperties of
    (PropertySerialization props) -> lift do
      forWithIndex_ props \propertyTypeId values ->
        -- PERSISTENCE of the role instance.
        -- CURRENTUSER: there can be no change to the current user.
        setProperty [ externalRole ] (EnumeratedPropertyType propertyTypeId) Nothing (Value <$> values)
      -- If there were no props, we have to save the external role now.
      if isEmpty props then lift $ void $ saveEntiteit externalRole else pure unit
  pure { context: contextInstance, universeContextDelta: delta, externalUniverseRoleDelta: delta', externalContextDelta: contextDelta }
