-- PDR VERSION 3.3.6
module Perspectives.DataUpgrade.PatchModels.PDR030306 where

import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable)

foreign import perspectivesSysteem :: String

replacements :: Object String
replacements = fromFoldable
  [ Tuple "model://perspectives.domains#System" perspectivesSysteem
  ]