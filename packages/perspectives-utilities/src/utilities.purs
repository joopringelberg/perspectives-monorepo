-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.Utilities where 

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (cons, elemIndex, uncons)
import Data.Foldable (intercalate)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, foldMap)
import Prim.RowList as RL
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))

onNothing :: forall m a e. MonadThrow e m => e -> m (Maybe a) -> m a
onNothing s ma = ma >>= (maybe (throwError s) pure)

onNothing' :: forall m e. MonadThrow e m => e -> Maybe ~> m
onNothing' s = maybe (throwError s) pure

maybeM :: forall a b m. Monad m => m a -> (b -> m a) -> m (Maybe b) -> m a
maybeM default fromJust monadicValue = do
  mv <- monadicValue
  case mv of
    Nothing -> default
    (Just v) -> fromJust v

ifNothing :: forall a b m. Monad m => m (Maybe b) -> m a -> (b -> m a) -> m a
ifNothing monadicValue default fromJust = maybeM default fromJust monadicValue

----------------------------------------------------------------------------------------
---- FINDM
----------------------------------------------------------------------------------------
-- | Find a value in an Array using a monadic criterium.
findM :: forall a f. Monad f => (a -> f Boolean) -> Array a -> f (Maybe a)
findM criterium arr = case uncons arr of
  Just {head, tail} -> do
    allowed <- criterium head
    if allowed
      then pure (Just head)
      else findM criterium tail
  Nothing -> pure Nothing

----------------------------------------------------------------------------------------
---- ADDUNIQUE
----------------------------------------------------------------------------------------
-- | Add an array element only if it is not yet in the array.
addUnique :: forall a. Eq a => a -> Array a -> Array a
addUnique a arr = if isJust $ elemIndex a arr
  then arr
  else cons a arr

----------------------------------------------------------------------------------------
---- PRETTYPRINT
----------------------------------------------------------------------------------------
prettyPrint :: forall a. PrettyPrint a => a -> String
prettyPrint a = prettyPrint' "  " a

class PrettyPrint d where
  prettyPrint' :: String -> d -> String

-- | No indentation for an Int.
instance intPrettyPrint :: PrettyPrint Int where
  prettyPrint' indent i = show i

-- | No indentation for a String.
instance stringPrettyPrint :: PrettyPrint String where
  prettyPrint' indent s = s

-- | No indentation for a Boolean.
instance boolPrettyPrint :: PrettyPrint Boolean where
  prettyPrint' indent b = show b

instance objectPrettyPrint :: PrettyPrint v => PrettyPrint (Object v) where
  prettyPrint' tab o = "{ " <> (foldMap (\(s :: String) (v :: v) -> newline <> tab <> s <> ": " <> (prettyPrint' (tab <> "  ") v)) o) <> " }"

instance arrayPrettyPrint :: (Show v, PrettyPrint v) => PrettyPrint (Array v) where
  -- prettyPrint' tab a = "[" <> fold (((<>) newline <<< (prettyPrint' (tab <> "  "))) <$> a) <> newline <> tab <> "]"
  prettyPrint' tab a = "[" <> intercalate ", " (map (prettyPrint' tab) a) <> "]"

instance maybePrettyPrint :: (PrettyPrint v) => PrettyPrint (Maybe v) where
  prettyPrint' tab Nothing = "Nothing"
  prettyPrint' tab (Just v) = "Just " <> prettyPrint' tab v

instance tuplePrettyPrint :: (PrettyPrint f, PrettyPrint s) => PrettyPrint (Tuple f s) where
  prettyPrint' tab (Tuple f s) = "Tuple " <> prettyPrint' tab f <> prettyPrint' tab s

newline :: String
newline = "\n"

-- | PrettyPrint a record by
-- |  * starting on a new line, with the given indent and an opening bracket and the first key;
-- |  * printing every other key on a new line between , and :
-- |  * providing every value with an extra indent.
-- |  * ending with a closing bracket on a new line.
instance prettyPrintRecord :: (RL.RowToList rs rl, PrettyPrintRecordFields rl rs) => PrettyPrint (Record rs) where
  prettyPrint' tab record = case prettyPrintRecordFields tab (Proxy :: Proxy rl) record  of
    [] -> "{}"
    fields -> "\n" <> tab <> "{ " <> intercalate ("\n" <> tab <> ", " ) fields <> "\n" <> tab <> "}"


class PrettyPrintRecordFields :: forall k. k -> Row Type -> Constraint
class PrettyPrintRecordFields rowlist row where
  prettyPrintRecordFields ::  String -> Proxy rowlist -> Record row -> Array String

instance prettyPrintRecordFieldsNil :: PrettyPrintRecordFields RL.Nil row where
  prettyPrintRecordFields _ _ _ = []

instance prettyPrintRecordFieldsCons
    ::  ( PrettyPrintRecordFields rowlistTail row
        , PrettyPrint valueType
        , Reflectable key String
        , IsSymbol key
        )
    => PrettyPrintRecordFields (RL.Cons key valueType rowlistTail) row where
  prettyPrintRecordFields tab _ record = cons (keyString <> ": " <> (prettyPrint' (tab <> "  ") value)) tail
    where
      keyString = reflectType (Proxy :: Proxy key)
      -- See test2.purs for another attempt to make this work.
      -- value = ((get (Proxy :: Proxy key) record) :: valueType)
      value = unsafeGet keyString record :: valueType
      tail = prettyPrintRecordFields tab (Proxy :: Proxy rowlistTail) record

instance PrettyPrint (Map k v) where
  prettyPrint' tab o = "<implement prettyprint for maps!>"