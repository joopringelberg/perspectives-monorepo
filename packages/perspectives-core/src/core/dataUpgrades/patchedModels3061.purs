module Perspectives.DataUpgrade.PatchModels.PDR3061 where

import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable)

foreign import italie :: String

replacements :: Object String
replacements = fromFoldable
  [ Tuple "model://corbaars.nl#Simple" italie
  ]