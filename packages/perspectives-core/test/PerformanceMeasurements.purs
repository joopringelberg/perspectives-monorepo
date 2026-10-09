-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Test.PerformanceMeasurements where

import Prelude

import Effect (Effect)

type MeasurementHooks =
  { beginTrial :: String -> Effect Unit
  , beginAction :: Effect Unit
  , endAction :: Effect Unit
  , endCompletion :: Effect Unit
  , endTrial :: String -> Boolean -> Effect Unit
  }

foreign import beginTrial :: String -> Effect Unit
foreign import beginAction :: Effect Unit
foreign import endAction :: Effect Unit
foreign import endCompletion :: Effect Unit
foreign import endTrial :: String -> Boolean -> Effect Unit
foreign import amqpMode :: Effect Boolean
foreign import publishResults :: Boolean -> Effect Unit

measurementHooks :: MeasurementHooks
measurementHooks = { beginTrial, beginAction, endAction, endCompletion, endTrial }
