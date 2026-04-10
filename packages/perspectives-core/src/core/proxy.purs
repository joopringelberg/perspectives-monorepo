module Perspectives.Proxy
  ( createRequestEmitter
  , handleClientRequest
  , pdrStatusMessageChannel
  , receivePDRStatusMessageChannel
  , registerPutUserIntegrityChoice
  , retrieveRequestEmitter
  ) where

import Control.Coroutine.Aff (Emitter, Step(..))
import Control.Promise (Promise)
import Data.Function.Uncurried (Fn2)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Foreign (Foreign)
import Prelude (Unit)

-----------------------------------------------------------
-- PASSING ON handleClientRequest
-----------------------------------------------------------
foreign import handleClientRequest :: Foreign

foreign import receivePDRStatusMessageChannel :: Foreign
-----------------------------------------------------------
-- REQUEST, RESPONSE AND CHANNEL
-----------------------------------------------------------
foreign import createRequestEmitterImpl
  :: EffectFn3
       (Foreign -> Step Foreign Unit)
       (Unit -> Step Foreign Unit)
       (Emitter Effect Foreign Unit)
       Unit

-- createRequestEmitter :: Emitter Foreign Unit
createRequestEmitter :: Emitter Effect Foreign Unit -> Effect Unit
createRequestEmitter = runEffectFn3 createRequestEmitterImpl Emit Finish

foreign import retrieveRequestEmitterImpl
  :: EffectFn1
       (Emitter Effect Foreign Unit)
       Unit

-- createRequestEmitter :: Emitter Foreign Unit
retrieveRequestEmitter :: Emitter Effect Foreign Unit -> Effect Unit
retrieveRequestEmitter = runEffectFn1 retrieveRequestEmitterImpl

foreign import pdrStatusMessageChannel :: Promise (Fn2 String String Unit)

-- | Called by the PDR during startup to register the function that delivers
-- | the end-user's integrity choice (Boolean) into the userIntegrityChoice AVar.
-- | The registered function is later called by handleClientRequest when the
-- | "resolveUserIntegrityChoice" proxy request arrives from the frontend.
foreign import registerPutUserIntegrityChoice_
  :: EffectFn1
       (Boolean -> Effect Unit)
       Unit

registerPutUserIntegrityChoice :: (Boolean -> Effect Unit) -> Effect Unit
registerPutUserIntegrityChoice = runEffectFn1 registerPutUserIntegrityChoice_