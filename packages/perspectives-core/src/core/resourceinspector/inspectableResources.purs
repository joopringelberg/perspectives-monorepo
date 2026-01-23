module Perspectives.Inspector.InspectableResources where

import Data.Maybe (Maybe)
import Foreign.Object (Object)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance) as II

type RoleInstance =
  { _id :: II.RoleInstance
  -- title is the value of the readable property of the instance.
  , title :: ReadableRoleInstance
  }

type ContextInstance = String

type SimpleMap :: forall k. k -> Type -> Type
type SimpleMap k v = Object v

type InspectableContext =
  { id :: II.ContextInstance
  , title :: ReadableContextInstance
  , ctype :: ReadableContextFQN
  -- keys are readable property identifiers, fully qualified.
  -- translatedProperty is the translated name of the property.
  -- We will display translatedProperty as label and value as value.
  -- Property Readable will be available in hover.
  , properties :: SimpleMap ReadablePropertyFQN { translatedProperty :: TranslatedPropertyTypeName, value :: String }
  -- keys are readable role identifiers, fully qualified.
  -- translatedRole is the translated name of the role.
  , roles :: SimpleMap ReadableRoleFQN { translatedRole :: TranslatedRoleTypeName, instances :: Array RoleInstance }
  -- keys are readable role identifiers, fully qualified.
  -- translatedRole is the translated name of the role.
  , unlinkedRoles :: SimpleMap ReadableRoleFQN { translatedRole :: TranslatedRoleTypeName, instances :: Array RoleInstance }
  -- title is the value of the readable property of the instance.
  -- roleType is the readable fully qualified role type identifier.
  , me :: Maybe { _id :: II.RoleInstance, title :: ReadableRoleInstance, roleType :: ReadableRoleFQN }
  -- keys are fully qualified context type identifiers.
  -- values are the translated names of the context types.
  , types :: SimpleMap ReadableContextFQN TranslatedContextTypeName
  -- The identifier of the external role.
  , externalRole :: II.RoleInstance
  -- keys are fully qualified state identifiers.
  -- values are the translated names of the states.
  , states :: SimpleMap ReadableStateFQN TranslatedStateTypeName
  }

type InspectableRole =
  { _id :: II.RoleInstance
  -- title is the value of the readable property of the instance.
  , title :: ReadableRoleInstance
  -- rtype is the readable fully qualified role type identifier.
  , rtype :: ReadableRoleFQN
  -- occurrence is the occurrence number of the role instance in the context.
  , index :: Int
  , isMe :: Boolean
  , context :: { _id :: II.ContextInstance, title :: ReadableContextInstance }
  -- keys are readable property identifiers, fully qualified.
  -- translatedProperty is the translated name of the property.
  -- We will display translatedProperty as label and value as value.
  -- Property Readable will be available in hover.
  , properties :: SimpleMap ReadablePropertyFQN { translatedProperty :: TranslatedPropertyTypeName, value :: String }
  , filler :: Maybe RoleInstance
  -- keys of the outer object are the readable identifiers of context types.
  -- contextTitle is the translated context type name.
  -- Keys of the nested object are readable role type identifiers, fully qualified.
  -- translatedRole is the translated name of the role type.
  , filledRoles ::
      SimpleMap ReadableContextFQN
        { contextTitle :: TranslatedContextTypeName
        , roleInstances :: (SimpleMap ReadableRoleFQN { translatedRole :: TranslatedRoleTypeName, instances :: Array RoleInstance })
        }
  -- keys are fully qualified role type identifiers.
  -- values are the translated names of the role types.
  , types :: SimpleMap ReadableRoleFQN TranslatedRoleTypeName
  -- values are the translated names of the states.
  , states :: SimpleMap ReadableStateFQN TranslatedStateTypeName
  }

-- TranslatedContextTypeName is the translated name of the role type.
type TranslatedRoleTypeName = String
-- TranslatedContextName is the translated name of the context type.
type TranslatedContextTypeName = String
-- TranslatedPropertyTypeName is the translated name of the property type.
type TranslatedPropertyTypeName = String
-- TranslatedStateTypeName is the translated name of the state type.
type TranslatedStateTypeName = String

-- ReadableRoleInstance is the value of the readable property of the role instance.
type ReadableRoleInstance = String
-- ReadableContextInstance is the value of the readable property of the external role of the context instance.
type ReadableContextInstance = String

-- The fully qualified readable identifiers.
type ReadableRoleFQN = String
type ReadableContextFQN = String
type ReadablePropertyFQN = String
type ReadableStateFQN = String