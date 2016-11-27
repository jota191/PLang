
-- | Exception module for P
-- | Juan García Garland (Nov. 2016)

module Exception where

data Exc a = Return a
           | Error String
           deriving (Read,Show)


instance Monad Exc where
  return  = Return
  m >>= f = case m of
              Error s  -> Error s
              Return a -> f a 
  fail    = Error
