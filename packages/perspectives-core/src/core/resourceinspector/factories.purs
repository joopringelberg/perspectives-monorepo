module Perspectives.Inspector.Factories where

import Prelude

import Control.Monad.Writer (runWriterT)
import Data.Array (cons, foldM, head)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object (empty, fromFoldable, insert, lookup)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.HumanReadableType (translateType)
import Perspectives.Inspector.InspectableResources as IC
import Perspectives.InstanceRepresentation (PerspectContext(..), PerspectRol(..))
import Perspectives.Parsing.Arc.AST (PropertyFacet(..))
import Perspectives.Persistent (getPerspectRol)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Property (hasFacet)
import Perspectives.Representation.InstanceIdentifiers (Value(..), externalRole)
import Perspectives.Representation.TypeIdentifiers (ContextType(..), PropertyType(..), StateIdentifier(..), EnumeratedPropertyType(..), EnumeratedRoleType(..))
import Perspectives.Sidecar.ToReadable (toReadable)
import Perspectives.TypePersistence.PerspectiveSerialisation (getReadableNameFromTelescope)

makeInspectableContext :: PerspectContext -> MonadPerspectives IC.InspectableContext
makeInspectableContext (PerspectContext ctxt) = do
  ctitle :: String <- getPerspectRol ctxt.buitenRol >>= readablePropertyValue
  ContextType ctype <- toReadable ctxt.pspType
  properties <- getPerspectRol ctxt.buitenRol >>= makeInspectableProperties
  roles <- foldWithIndexM
    ( \roleTypeName acc roleInstances -> do
        EnumeratedRoleType readableRoleFQN <- toReadable (EnumeratedRoleType roleTypeName)
        translatedRoleTypeName <- translateType (EnumeratedRoleType roleTypeName)
        instances <- foldM
          ( \arr roleInstanceId -> do
              roleInstance <- getPerspectRol roleInstanceId
              roleInst <- makeRoleInstance roleInstance
              pure $ cons roleInst arr
          )
          []
          roleInstances
        pure $ insert readableRoleFQN { translatedRole: translatedRoleTypeName, instances } acc
    )
    empty
    ctxt.rolInContext
  me <- case ctxt.me of
    Nothing -> pure Nothing
    Just meRoleInstanceId -> do
      meRoleInstance@(PerspectRol { pspType }) <- getPerspectRol meRoleInstanceId
      title <- readablePropertyValue meRoleInstance
      EnumeratedRoleType readableRoleFQN <- toReadable pspType
      pure $ Just
        { _id: meRoleInstanceId
        , title
        , roleType: readableRoleFQN
        }
  types <- fromFoldable <$> for ctxt.allTypes \cty -> do
    ContextType readableCtyFQN <- toReadable cty
    translatedContextTypeName <- translateType cty
    pure $ Tuple readableCtyFQN translatedContextTypeName
  states <- fromFoldable <$> for ctxt.states \stId -> do
    StateIdentifier readableStateFQN <- toReadable stId
    translatedStateTypeName <- translateType stId
    pure $ Tuple readableStateFQN translatedStateTypeName
  pure
    { id: ctxt.id
    , title: ctitle
    , ctype: ctype
    , properties
    , roles
    , me
    , types
    , externalRole: ctxt.buitenRol
    , states
    }

readablePropertyValue :: PerspectRol -> MonadPerspectives String
readablePropertyValue (PerspectRol { id, pspType, properties }) = do
  mlocalTitle <- foldWithIndexM
    ( \propName found prop ->
        case found of
          Just val -> pure (Just val)
          Nothing -> do
            isReadable <- hasFacet (ENP $ EnumeratedPropertyType propName) ReadableNameProperty
            if isReadable then
              pure $ case head <$> lookup propName properties of
                Just val -> val
                Nothing -> Nothing
            else pure Nothing
    )
    Nothing
    properties
  case mlocalTitle of
    Nothing -> do
      Tuple mNonLocalTitle _ <- runWriterT $ getReadableNameFromTelescope (flip hasFacet ReadableNameProperty) (ST pspType) id
      pure mNonLocalTitle
    Just (Value t) -> pure t

makeInspectableProperties
  :: PerspectRol
  -> MonadPerspectives
       (IC.SimpleMap IC.ReadablePropertyFQN { translatedProperty :: IC.TranslatedPropertyTypeName, value :: String })
makeInspectableProperties (PerspectRol { properties }) = do
  foldWithIndexM
    ( \propName acc values -> case head values of
        Nothing -> pure acc
        Just (Value value) -> do
          EnumeratedPropertyType readablePropFQN <- toReadable (EnumeratedPropertyType propName)
          translatedPropertyTypeName <- translateType (EnumeratedPropertyType propName)
          pure $ insert readablePropFQN { translatedProperty: translatedPropertyTypeName, value } acc
    )
    empty
    properties

makeRoleInstance :: PerspectRol -> MonadPerspectives IC.RoleInstance
makeRoleInstance r@(PerspectRol rol) = do
  title <- readablePropertyValue r
  pure
    { _id: rol.id
    -- title is the value of the readable property of the instance.
    , title
    }

makeInspectableRole :: PerspectRol -> MonadPerspectives IC.InspectableRole
makeInspectableRole r@(PerspectRol rol) = do
  title <- readablePropertyValue r
  EnumeratedRoleType rtype <- toReadable rol.pspType
  context <- do
    ctitle <- getPerspectRol (externalRole rol.context) >>= readablePropertyValue
    pure
      { _id: rol.context
      -- title is the value of the readable property of the external role instance.
      , title: ctitle
      }
  properties <- makeInspectableProperties r
  filler <- traverse (getPerspectRol >=> makeRoleInstance) rol.binding
  filledRoles <- foldWithIndexM
    ( \ctxTypeId acc roleInstances -> do
        ContextType readableContextFQN <- toReadable (ContextType ctxTypeId)
        translatedContextTypeName <- translateType (ContextType ctxTypeId)
        rolesMap <- foldWithIndexM
          ( \roleTypeName roles roleInstancesInContext -> do
              EnumeratedRoleType readableRoleFQN <- toReadable (EnumeratedRoleType roleTypeName)
              translatedRoleTypeName <- translateType (EnumeratedRoleType roleTypeName)
              instances <- foldM
                ( \arr roleInstanceId -> do
                    roleInstance <- getPerspectRol roleInstanceId
                    roleInst <- makeRoleInstance roleInstance
                    pure $ cons roleInst arr
                )
                []
                roleInstancesInContext
              pure $ insert readableRoleFQN { translatedRole: translatedRoleTypeName, instances } roles
          )
          empty
          roleInstances
        pure $ insert readableContextFQN { contextTitle: translatedContextTypeName, roleInstances: rolesMap } acc
    )
    empty
    rol.filledRoles
  types <- fromFoldable <$> for rol.allTypes \rty -> do
    EnumeratedRoleType readableRtyFQN <- toReadable rty
    translatedRoleTypeName <- translateType rty
    pure $ Tuple readableRtyFQN translatedRoleTypeName
  states <- fromFoldable <$> for rol.states \stId -> do
    StateIdentifier readableStateFQN <- toReadable stId
    translatedStateTypeName <- translateType stId
    pure $ Tuple readableStateFQN translatedStateTypeName

  pure
    { _id: rol.id
    -- title is the value of the readable property of the instance.
    , title
    -- rtype is the readable fully qualified role type identifier.
    , rtype
    -- occurrence is the occurrence number of the role instance in the context.
    , index: rol.occurrence
    , isMe: rol.isMe
    , context
    -- keys are readable property identifiers, fully qualified.
    -- translatedProperty is the translated name of the property.
    -- We will display translatedProperty as label and value as value.
    -- Property Readable will be available in hover.
    , properties
    , filler
    -- keys of the outer object are the identifiers of contexts.
    -- contextTitle is the readable external role property value of the context.
    -- Keys of the nested object are readable role type identifiers, fully qualified.
    -- translatedRole is the translated name of the role type.
    , filledRoles
    -- keys are fully qualified role type identifiers.
    -- values are the translated names of the role types.
    , types
    -- values are the translated names of the states.
    , states
    }
