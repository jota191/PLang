
-- | Parser combinators
-- | Juan García Garland (Nov. 2016)

module ParserCombinators where

import Lexer
import Language
import Exception
import Control.Monad

-- | Datatype for parsers
data Parser s a = Parser {runP :: s -> [(a,s)]}


-- | We will use monadic parsers
instance Monad (Parser s) where
  return a = Parser $ \s -> [(a,s)]
  p >>= q  = Parser $ \s -> concat [runP (q a) s' | (a,s') <- runP p s]


-- | Parser Combinators

pFail :: Parser s a
pFail = Parser $ \s -> []


pSucceed :: a -> Parser s a
pSucceed a = Parser $ \s -> [(a,s)]

(<|>) :: Parser s a -> Parser s a -> Parser s a
p <|> q = Parser $ \s -> runP p s ++ runP q s

(<$>) :: (a -> b) -> Parser s a -> Parser s b
(<$>) = liftM

(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
(<*>) = ap


pList :: Parser s a -> Parser s [a]
pList p = (:) <$> p <*> pList p
       <|> pSucceed [] 
