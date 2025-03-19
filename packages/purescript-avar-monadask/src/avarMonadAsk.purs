module Control.Monad.AvarMonadAsk (get, gets, put, modify) where

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar (AVar, put, read, take) as AV
import Effect.Aff.Class (class MonadAff, liftAff)
import Prelude (Unit, bind, discard, pure, ($), (<<<), (>=>), (>>=))

state :: forall a s m.
  MonadAff m =>
  MonadAsk (AV.AVar s) m =>
  (s -> (Tuple a s)) -> m a
state f = do
  st <- get
  (Tuple a s) <- pure (f st)
  put s
  pure a

-- newtype AvarMonadAsk m = AvarMonadAsk m

-- instance functorAvarMonadAsk :: Functor m => Functor AvarMonadAsk where
--   map f (AvarMonadAsk m) = AvarMonadAsk (f <$> m)
--
-- instance monadStateAvarMonadAsk :: (MonadAff m, MonadAsk (AV.AVar s) m) => MonadState s AvarMonadAsk where
--   state f = do
--     st <- get
--     (Tuple a s) <- pure (f st)
--     put s
--     pure a
--
-- | Returns the AVar holding the state.
getAVarWithState :: forall s m. MonadAsk (AV.AVar s) m => m (AV.AVar s)
getAVarWithState = ask

-- | Returns the state. Blocks as long as the AVar is empty. Does not block a filled AVar: it is not left empty.
-- | Compares with StateT get.
get :: forall s m.
  MonadAff m =>
  MonadAsk (AV.AVar s) m =>
  m s
get = do
  (as :: AV.AVar s) <- getAVarWithState
  liftAff $ AV.read as

take :: forall s m.
  MonadAff m =>
  MonadAsk (AV.AVar s) m =>
  m s
take = do
  (as :: AV.AVar s) <- getAVarWithState
  liftAff $ AV.take as


-- | Returns the result of applying a function to the state.
-- | Compares with StateT gets.
gets :: forall s m a.
  MonadAff m =>
  MonadAsk (AV.AVar s) m =>
  (s -> a) -> m a
gets f = get >>= pure <<< f

-- | Puts a new state in the AVar holding the state. Uses replaceAVarContent: so blocks as long as the AVar
-- | passed in is empty.
-- | Compares with StateT put.
put :: forall s m. MonadAff m => MonadAsk (AV.AVar s) m => s -> m Unit
put st = getAVarWithState >>= (liftAff <<< (replaceAVarContent st))

-- | Make an AVar empty. Blocks as long as the AVar passed in is empty.
clearVar :: forall a m. MonadAff m => AV.AVar a -> m (AV.AVar a)
clearVar v = do
  _ <- liftAff $ AV.take v
  pure v

-- | Replace the content of an AVar. Blocks as long as the AVar passed in is empty (waits till it is filled).
-- | Then, if it is filled, blocks while the content is being replaced.
replaceAVarContent :: forall a m.
  MonadAff m =>
  a -> AV.AVar a -> m Unit
replaceAVarContent value = liftAff <<< clearVar >=> liftAff <<< AV.put value

-- | Notice that we block the AVar by taking its value.
-- | I would have liked to put the modified value back in the Effect,
-- | rather than in Aff, to make sure that no other asynchronous operation
-- | could intervene. This happend when I defined this operation in terms of 
-- | the take and put of this module, leading to a catch-22 where two or more 
-- | were waiting for each other.
-- | With the current solution that problem hasn't been observed.
modify  :: forall s m.
  MonadAff m =>
  MonadAsk (AV.AVar s) m =>
  (s -> s) -> m Unit
modify f = do
  (as :: AV.AVar s) <- getAVarWithState
  liftAff do 
    v <- AV.take as
    AV.put (f v) as