module RecordLabels where

import Data.Symbol (reflectSymbol, class IsSymbol)
import Data.Reflectable (class Reflectable, reflectType)
import Type.Proxy (Proxy(..))
import Data.Array (cons)
import Prim.RowList as RL

-- Define the type class for enumerating labels
class RecordLabels :: forall k. k -> Constraint
class RecordLabels row where
  recordLabels :: Proxy row -> Array String

-- Instance for empty record
instance recordLabelsNil :: RecordLabels RL.Nil where
  recordLabels _ = []

-- Instance for non-empty records
instance recordLabelsCons
  :: ( RecordLabels rowlistTail
     , Reflectable keySymbol String
     ) => RecordLabels (RL.Cons keySymbol valueType rowlistTail) where
  recordLabels _ = cons (reflectType (Proxy :: Proxy keySymbol)) (recordLabels (Proxy :: Proxy rowlistTail))

-- Function to collect labels from a record
getRecordLabels :: forall row. RecordLabels row => Record row -> Array String
getRecordLabels record = recordLabels (Proxy :: Proxy row)

