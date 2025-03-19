module Test2  where


import Data.List (List(..))
import Data.Reflectable (class Reflectable, reflectType)
import Data.Show (class Show, show)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Row as Row
import Prim.RowList as RL
import Record (get)
import Record.Unsafe (unsafeGet)
import Type.Proxy (Proxy(..))

class ShowKeysInRowList :: RL.RowList Type -> Row Type -> Constraint
class ShowKeysInRowList rowList row where
  buildKeyList :: forall r. Record r -> List (Tuple String String)

instance ShowKeysInRowList RL.Nil emptyRow where
  buildKeyList :: forall r. Record r -> List (Tuple String String)
  buildKeyList _ = Nil

instance 
  ( Reflectable sym String
  , ShowKeysInRowList rest restOfRows
  , IsSymbol sym
  , Show k
  , Row.Cons sym k restOfRows total
  ) => ShowKeysInRowList (RL.Cons sym k rest) total where
  buildKeyList record = Cons (Tuple key (show value)) remainingKeys
    where
      -- this would be `reflectType @sym` but the type class
      -- hasn't been updated yet as of this writing.
      key = reflectType (Proxy :: Proxy sym)
      {-
        De type checker meldt over: 
          
          value = get (Proxy :: Proxy sym) record

        No type class instance was found for

          Prim.Row.Cons sym4
                        t5
                        t2
                        r6

      while applying a function get
        of type IsSymbol t0 => Cons @Type t0 t1 t2 t3 => Proxy @Symbol t0 -> Record t3 -> t1
        to argument Proxy
      while inferring the type of get Proxy

      where sym4 is a rigid type variable
              bound at (line 0, column 0 - line 0, column 0)
            r6 is a rigid type variable
              bound at (line 0, column 0 - line 0, column 0)
            t3 is an unknown type
            t2 is an unknown type
            t0 is an unknown type
            t1 is an unknown type
            t5 is an unknown type
      -}
      value = (unsafeGet key record :: k)
      remainingKeys = buildKeyList @rest @restOfRows record 
