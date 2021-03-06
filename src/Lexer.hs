
-- | Lexer for P
-- | Juan García Garland (Nov. 2016)

module Lexer where

import Exception
import Data.Char

-- | Some tokens have attributes
type Lexeme = String



-- | DataType for tokens
data Token = TProgram | TResult
           | TLParen | TRParen
           | TVar Lexeme | TZero
           | TAssignSym | TSuc | TPred
           | TWhile | TDo | TEnd | TSemicol
           | TNeq0
           deriving (Eq,Show)



-- | Scanning one token
scan :: String -> Exc (Token,String)
-- returns the parsed token and the modified input strean, or an error
scan ('P':'R':'O':'G':'R':'A':'M':xs) = return (TProgram,xs)
scan ('R':'E':'S':'U':'L':'T':' ':xs) = return (TResult,xs)
scan ('W':'H':'I':'L':'E':' ':xs) = return (TWhile,xs)
scan ('D':'O':' ':xs) = return (TDo,xs)
scan ('E':'N':'D':xs) = return (TEnd,xs)
scan ('S':'U':'C':xs) = return (TSuc,xs)
scan ('P':'R':'E':'D':xs) = return (TPred,xs)
scan (';':xs) = return (TSemicol,xs)
scan ('/':'=':' ':'0':xs) = return (TNeq0,xs)
scan (':':'=':xs) = return (TAssignSym,xs)
scan ('(':xs) = return (TLParen,xs)
scan (')':xs) = return (TRParen,xs)
scan ('X':xs) = return (TVar ("X"++idf) ,xs')
  where idf   = takeWhile isDigit xs
        xs'   = dropWhile isDigit xs 
scan ('0':xs) = return (TZero,xs)
scan (' ':xs) = scan xs
--scan(d:xs)    = if isDigit d
--                then return (TDigit [d],xs)
--                else fail "Error: No Parse"

-- | Scanning function.

lexer :: String -> Exc [Token]
lexer [] = return []
lexer inp = scan inp >>= \(tok,tail) ->
            lexer tail >>= \toks ->
            return (tok:toks)


-- | Testing
t1 = (extract . lexer) $ "PROGRAM (X12) WHILE DO () END"
t2 = (extract . lexer) $ "X23 PROGRAM (X12) WHILE DO () END"
