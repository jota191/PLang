
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

pTZero    = pToken TZero

pTNeq0    = pToken TNeq0

pTAssignSym
          = pToken TAssignSym

pTVar :: PParser Int
pTVar = Parser $ \ts -> case ts of
                          (TVar s):ts' -> [((read.tail) s,ts')]
                          _            -> []



-- | generating AST

pSec = Sec <$> pList pSent

pCond = (\var _ -> Nonzero var) <$> pTVar <*> pTNeq0

pWhile
  = (\_ c _ s _-> While c s ) <$> pTWhile <*> pCond <*> pTDo <*> pSec <*> pTEnd
 

pExpr =     const Zero <$> pTZero
     <|> (\_ _ v _ -> Succ v) <$> pTSuc  <*> pTLParen <*> pTVar <*> pTRParen  
     <|> (\_ _ v _ -> Succ v) <$> pTPred <*> pTLParen <*> pTVar <*> pTRParen  
        

pAssign =  (\v _ e -> Assign v e) <$> pTVar <*> pTAssignSym <*> pExpr

pSent =  pAssign
     <|> pWhile
    
pProgram = (\_ _ vi _ s _ _ vo _ -> Program vi s vo) <$> pTProgram
                                                     <*> pTLParen
                                                     <*> pTVar
                                                     <*> pTRParen
                                                     <*> pSec
                                                     <*> pTResult
                                                     <*> pTLParen
                                                     <*> pTVar
                                                     <*> pTRParen
