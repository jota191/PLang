
-- | Parsers for P
-- | Juan García Garland (Nov. 2016)

module Parser where

import Lexer
import Language
import Exception
import ParserCombinators


-- | The type of P Parser
type PParser = Parser [Token]


pToken :: Token -> PParser Token
pToken t = Parser $ \s -> case s of
                            []     -> []
                            (t':ts) -> if t==t'
                                       then [(t,ts)]
                                       else []


-- | Parsers for P tokens

-- | values of type  :: PParser Token

pTProgram = pToken TProgram

pTResult  = pToken TResult

pTLParen  = pToken TLParen

pTRParen  = pToken TRParen

pTWhile   = pToken TWhile

pTDo      = pToken TDo

pTEnd     = pToken TEnd

pTSemicol = pToken TSemicol

pTSuc     = pToken TSuc

pTPred    = pToken TPred

pTNeq0    = pToken TNeq0

pTAssignSym
          = pToken TAssignSym

pTVar :: PParser String
pTVar = Parser $ \ts -> case ts of
                          (TVar s):ts' -> [(s,ts')]
                          _            -> []



-- | generating AST
pSec :: PParser Sec
pSec = Sec <$> pSent <*> pSent

pCond :: PParser Cond
pCond = (\var _ -> Nonzero var) <$> pTVar <*> pTNeq0

pWhile :: PParser While
pWhile = While <$> pCond <*> pSent 

pWhile :: PParser While
pWhile = While <$> pCond <*> pSent 
