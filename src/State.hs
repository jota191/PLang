{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances#-}
-- | State Monad
-- | Juan García Garland (Nov. 2016)


module State where

data State s a = State { runState :: s -> (a,s)}

instance Monad (State s) where
  return a = State $ \s -> (a,s)
  m >>= f  = State $ \s -> let (a,s') = runState m s
                           in runState (f a) s'



class (Monad m) => MonadState s m | m -> s where
  put :: s -> m ()
  get :: m s


instance MonadState s (State s) where
  put s = State $ \_ -> ((),s)
  get   = State $ \s -> (s,s)
