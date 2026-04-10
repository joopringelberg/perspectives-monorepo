module Perspectives.Logging.DefaultLevels where

import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (LogConfig, LogLevel(..), LogTopic(..))

defaultLogLevels :: LogConfig
defaultLogLevels =
  { defaultLevel: Warn
  , topicLevels: fromFoldable
      [ (Tuple SYNC Info)
      , (Tuple BROKER Info)
      , (Tuple QUERY Info)
      , (Tuple PARSER Warn)
      , (Tuple MODEL Error)
      , (Tuple INSTALL Warn)
      ]
  }
