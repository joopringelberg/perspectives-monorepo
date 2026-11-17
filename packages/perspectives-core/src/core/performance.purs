-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Perspectives.Performance where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn2, runEffectFn2)

foreign import data TimingToken :: Type
foreign import timingEnabled :: Effect Boolean
foreign import traceTimingImpl :: EffectFn2 String String Unit
foreign import startTiming :: String -> String -> Effect TimingToken
foreign import finishTiming :: TimingToken -> Boolean -> Effect Unit

traceTiming :: String -> String -> Effect Unit
traceTiming = runEffectFn2 traceTimingImpl

timed :: forall m a. MonadEffect m => MonadError Error m => String -> String -> m a -> m a
timed stage identifier action = do
  enabled <- liftEffect timingEnabled
  if not enabled then action
  else do
    token <- liftEffect $ startTiming stage identifier
    catchError
      ( do
          result <- action
          liftEffect $ finishTiming token true
          pure result
      )
      ( \err -> do
          liftEffect $ finishTiming token false
          throwError err
      )

foreign import data NormalisationToken :: Type
foreign import normalisationEnabled :: Effect Boolean
foreign import resetNormalisationMeasurements :: Effect Unit
foreign import setNormalisationPhase :: String -> Effect Unit
foreign import startNormalisation :: String -> String -> Boolean -> Effect NormalisationToken
foreign import finishNormalisation :: NormalisationToken -> Boolean -> Effect Unit
foreign import reportNormalisationMeasurements :: Effect Unit

measureNormalisation :: forall m a. MonadEffect m => MonadError Error m => String -> String -> Boolean -> m a -> m a
measureNormalisation pdr role compiling action = do
  token <- liftEffect $ startNormalisation pdr role compiling
  catchError
    ( do
        result <- action
        liftEffect $ finishNormalisation token true
        pure result
    )
    ( \err -> do
        liftEffect $ finishNormalisation token false
        throwError err
    )

foreign import data ProfileToken :: Type
foreign import data ProfileSession :: Type
foreign import captureSession :: Effect ProfileSession
foreign import sessionEnabled :: ProfileSession -> Effect Boolean
foreign import profilingEnabled :: Effect Boolean
foreign import startProfile :: String -> Effect ProfileToken
foreign import startProfileInSession :: ProfileSession -> String -> Effect ProfileToken
foreign import finishProfile :: ProfileToken -> Boolean -> Effect Unit
foreign import countEncrypted :: ProfileSession -> String -> Int -> Effect Unit
foreign import countDecrypted :: ProfileSession -> String -> Int -> Int -> Effect Unit

-- Disabled unless an opt-in measurement collector installs an active session.
profile :: forall m a. MonadEffect m => MonadError Error m => ProfileSession -> String -> m a -> m a
profile session label action = do
  enabled <- liftEffect $ sessionEnabled session
  if not enabled then action
  else do
    token <- liftEffect $ startProfileInSession session label
    catchError
      ( do
          result <- action
          liftEffect $ finishProfile token true
          pure result
      )
      ( \err -> do
          liftEffect $ finishProfile token false
          throwError err
      )
