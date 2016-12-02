
-- | Main module for P interpreter
-- | Juan García Garland, Dec. 2016

module Main where

import System.Environment
import Lexer
import Eval
import Parser
import ParserCombinators
import Exception

-- | This is the main program, only for testing, It's neither controlling
-- | argument format correctness nor has opt parsing I'll reimplement this soon

main = getArgs >>= \[path,param] ->
       -- we assume that the first param is the name of source file
       -- and the second is a number (input for the program)
       
       readFile path >>= \program ->
       let inp    = (read param)::Integer
           source = (concat.lines) program
       in print $ eval ((fst.head.runP pProgram.extract.lexer) source)inp
