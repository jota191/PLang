


module Test where
import Memory
import Lexer
import Parser
import Exception
import ParserCombinators
import Eval

te1 = (extract . lexer) $ "0 X23 PROGRAM (X12) WHILE DO () END"
p = runP pExpr te1

te2 = (extract . lexer) $ "SUC(X0) X23 PROGRAM (X12) WHILE DO () END"
p2 = runP pExpr te2


code = "PROGRAM (X0)" ++
       "X1 := SUC(X0)" ++
       "RESULT (X1)"
       
lexed = (extract.lexer) $ code


code2 = "PROGRAM (X0)" ++
       "X1 := SUC(X0)" ++
       "X2 := 0 "  ++
       "RESULT (X1)"
       
lexed2 = (extract.lexer) $ code2
ast2 = fst.head $ runP pProgram lexed2


code3 = "PROGRAM (X0)" ++
       "X1 := SUC(X0)" ++
       "X2 := PRED(X1) "  ++
       "RESULT (X2)"
       
lexed3 = (extract.lexer) $ code3
ast3 = fst.head $ runP pProgram lexed3
