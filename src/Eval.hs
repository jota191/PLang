
-- | Evaluator for P
-- | Juan García Garland (Nov. 2016)

module Eval where

import State
import Language
import Memory
import State

-- | eval evaluates P

 

eval :: Program -> Integer -> Integer
eval p@(Program vin sec vout) inp
  = let initialMemory = singletonMemory vin inp
    in evalState (evalProg p inp) initialMemory 

evalProg :: Program -> Integer -> State Memory Integer
evalProg (Program varin sec varout) inp = put (singletonMemory varin inp) >>
                                          evalSec sec >>
                                          get >>= \mem ->
                                          return $ lookUp varout mem

evalSent :: Sent -> State Memory ()
evalSent (Assign var expr)
  = get >>= \mem ->
    case expr of
      Zero   -> put (update var 0 mem) 
      Succ x -> put (update var (n+1) mem)
        where
          n = lookUp x mem
      Pred x -> put (update var (pre n) mem)
        where
          n     = lookUp x mem
          pre n = if n==0 then 0 else n-1

evalSent w@(While (Nonzero varcond) sec)
  = get >>= \mem ->
    case lookUp varcond mem of
      0 -> put mem
      n -> evalSec sec >> 
           evalSent w

evalSec (Sec [])     = return ()
evalSec (Sec (s:ss)) = evalSent s >>
                       evalSec (Sec ss)
