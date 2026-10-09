-- SPDX-FileCopyrightText: 2026 Joop Ringelberg (joopringelberg@gmail.com), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later

module Perspectives.Performance where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)

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
