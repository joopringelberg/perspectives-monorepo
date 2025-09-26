module Perspectives.SideCar.PhantomTypedNewtypes where

import Prelude

import Data.Newtype (class Newtype)
import Perspectives.Utilities (class PrettyPrint)
import Simple.JSON (class ReadForeign, class WriteForeign)

-- Phantom tag to distinguish shapes at the type level
foreign import data Readable :: Type
foreign import data Stable :: Type

newtype ModelUri :: Type -> Type
-- URIs tagged with their flavor (phantom parameter)
newtype ModelUri f = ModelUri String

derive instance newtypeModelUri :: Newtype (ModelUri f) _
derive newtype instance eqModelUri :: Eq (ModelUri f)
derive newtype instance ordModelUri :: Ord (ModelUri f)
derive newtype instance showModelUri :: Show (ModelUri f)
derive newtype instance ReadForeign (ModelUri f)
derive newtype instance WriteForeign (ModelUri f)

instance PrettyPrint (ModelUri f) where
  prettyPrint' t (ModelUri n) = n

newtype ContextUri :: forall k. k -> Type
newtype ContextUri f = ContextUri String

derive instance newtypeContextUri :: Newtype (ContextUri f) _
derive newtype instance eqContextUri :: Eq (ContextUri f)
derive newtype instance ordContextUri :: Ord (ContextUri f)
derive newtype instance showContextUri :: Show (ContextUri f)
derive newtype instance ReadForeign (ContextUri f)
derive newtype instance WriteForeign (ContextUri f)

newtype RoleUri :: forall k. k -> Type
newtype RoleUri f = RoleUri String

derive instance newtypeRoleUri :: Newtype (RoleUri f) _
derive newtype instance eqRoleUri :: Eq (RoleUri f)
derive newtype instance ordRoleUri :: Ord (RoleUri f)
derive newtype instance showRoleUri :: Show (RoleUri f)
derive newtype instance ReadForeign (RoleUri f)
derive newtype instance WriteForeign (RoleUri f)

newtype PropertyUri :: forall k. k -> Type
newtype PropertyUri f = PropertyUri String

derive instance newtypePropertyUri :: Newtype (PropertyUri f) _
derive newtype instance eqPropertyUri :: Eq (PropertyUri f)
derive newtype instance ordPropertyUri :: Ord (PropertyUri f)
derive newtype instance showPropertyUri :: Show (PropertyUri f)
derive newtype instance ReadForeign (PropertyUri f)
derive newtype instance WriteForeign (PropertyUri f)

newtype ViewUri :: forall k. k -> Type
newtype ViewUri f = ViewUri String

derive instance newtypeViewUri :: Newtype (ViewUri f) _
derive newtype instance eqViewUri :: Eq (ViewUri f)
derive newtype instance ordViewUri :: Ord (ViewUri f)
derive newtype instance showViewUri :: Show (ViewUri f)
derive newtype instance ReadForeign (ViewUri f)
derive newtype instance WriteForeign (ViewUri f)

newtype StateUri :: forall k. k -> Type
newtype StateUri f = StateUri String

derive instance newtypeStateUri :: Newtype (StateUri f) _
derive newtype instance eqStateUri :: Eq (StateUri f)
derive newtype instance ordStateUri :: Ord (StateUri f)
derive newtype instance showStateUri :: Show (StateUri f)
derive newtype instance ReadForeign (StateUri f)
derive newtype instance WriteForeign (StateUri f)
